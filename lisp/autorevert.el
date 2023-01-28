;;; autorevert.el --- revert buffers when files on disk change  -*- lexical-binding:t -*-

;; Copyright (C) 1997-1999, 2001-2023 Free Software Foundation, Inc.

;; Author: Anders Lindgren
;; Keywords: convenience
;; Created: 1997-06-01
;; Date: 1999-11-30

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; File reverting is forcibly synchronizing a file buffer to its state
;; on disk.  The interactive command `revert-buffer' does this
;; manually.  Automatic file reverting, or "auto-revert", does this
;; silently for unmodified buffers whose underlying files changed from
;; out beneath them.  This is a "dwim" behavior for most users.
;;
;; Under `auto-revert-use-notify' (default enabled), Emacs detects
;; disk changes via the system's file notification mechanism, e.g.,
;; inotify.
;;
;; In the absence of system-level file notifications, Emacs naively
;; polls for file timestamp changes every `auto-revert-interval'
;; seconds.
;;
;; A buffer whose file no longer exists is not reverted.  Auto-revert
;; behavior recommences should the deleted file reappear.
;;
;; If the file is only ever appended to, prefer
;; `auto-revert-tail-mode' to `auto-revert-mode', as the former
;; performs a parsimonious "tail" without reverting the entire file.
;;
;; Most users expect their text editor to auto-revert as a matter of
;; course.  As is its wont, Emacs offers always-active auto-revert as
;; an opt-in defcustom `global-auto-revert-mode'.  We do this partly
;; to avoid surprising the fusty (and fussy) userbase who went without
;; auto-revert for decades, and partly because we've not utmost
;; confidence in the code.
;;
;; Why are certain variables marked permanent-local?
;;
;; Under global-auto-revert-mode, auto-revert-mode activates through
;; after-change-major-mode-hook, which is run liberally, for example
;; in `normal-mode' and `run-mode-hooks'.  The natural means of
;; avoiding repeated activation is checking if auto-revert-mode is
;; already true.  However, that flag is liable to go poof because
;; `kill-all-local-variables' is also called liberally, for example in
;; `normal-mode' and `fundamental-mode'.  We thus rely on marking
;; certain buffer-local state as permanent-local, a longtime crutch
;; prone to errors of omission.

;;; Code:

(require 'filenotify)

(defgroup auto-revert nil
  "Revert unmodified buffer when file on disk changes."
  :group 'files
  :group 'convenience)

(defvar auto-revert-timer nil "Obsolesced")
(make-obsolete-variable 'auto-revert-timer nil "30.1")

(defsubst auto-revert-unset-timer ()
  (mapc (lambda (timer)
          (when (eq (timer--function timer) #'auto-revert--cycle)
            (cancel-timer timer)))
        timer-list))

(defalias 'auto-revert-reset-timer #'auto-revert-set-timer)
(defun auto-revert-set-timer ()
  "Cancel existing revert timers, and restart."
  (interactive) ; historical
  (auto-revert-unset-timer)
  (defvar auto-revert-interval)
  (setq auto-revert-timer (run-with-timer auto-revert-interval
                                          auto-revert-interval
                                          #'auto-revert--cycle)))

(defcustom auto-revert-interval 5
  "Time in seconds between auto-revert checks."
  :group 'auto-revert
  :type 'number
  :set (lambda (variable value)
	 (set-default variable value)
	 (auto-revert-set-timer)))

(defcustom auto-revert-stop-on-user-input t
  "Obsolesced.  auto-revert always defers to user input."
  :group 'auto-revert
  :type 'boolean)
(make-obsolete-variable 'auto-revert-stop-on-use-input nil "30.1")

(defcustom auto-revert-verbose t
  "Obsolesced."
  :group 'auto-revert
  :type 'boolean)
(make-obsolete-variable 'auto-revert-verbose nil "30.1")

(defcustom auto-revert-mode-text " ARev"
  "Obsolesced."
  :tag "auto-revert-mode Text"
  :group 'auto-revert
  :type 'string)
(make-obsolete-variable 'auto-revert-mode-text nil "30.1")

(defcustom auto-revert-tail-mode-text " Tail"
  "Obsolesced."
  :group 'auto-revert
  :type 'string
  :version "22.1")
(make-obsolete-variable 'auto-revert-tail-mode-text nil "30.1")

(defcustom global-auto-revert-mode-text ""
  "Obsolesced."
  :group 'auto-revert
  :type 'string)
(make-obsolete-variable 'global-auto-revert-mode-text nil "30.1")

(defvaralias 'global-auto-revert-non-file-buffers 'auto-revert-non-file-buffers)
(defcustom auto-revert-non-file-buffers nil
  "Activate experimental revert semantics for functional buffers.
Currently `buffer-menu-mode' and `dired-mode' define the
`revert-buffer-function' and `buffer-stale-function' methods
necessary for non-file reverting."
  :group 'auto-revert
  :type 'boolean
  :link '(info-link "(emacs)Auto Revert"))

(defcustom auto-revert-remote-files nil
  "Activate experimental revert of remote files (TRAMP)."
  :group 'auto-revert
  :type 'boolean
  :version "24.4")

(defcustom auto-revert-load-hook nil
  "Functions to run when this file is loaded."
  :tag "Load Hook"
  :group 'auto-revert
  :type 'hook)
(make-obsolete-variable 'auto-revert-load-hook
                        "use `with-eval-after-load' instead." "28.1")

(defcustom auto-revert-check-vc-info nil
  "Obsolesced."
  :group 'auto-revert
  :type 'boolean
  :version "22.1")
(make-obsolete-variable 'auto-revert-check-vc-info nil "30.1")

(defcustom global-auto-revert-ignore-modes nil
  "Obsolesced."
  :group 'auto-revert
  :type '(repeat sexp))
(make-obsolete-variable 'global-auto-revert-ignore-modes nil "30.1")

(defvar-local global-auto-revert-ignore-buffer nil "Obsolesced")
(make-obsolete-variable 'global-auto-revert-ignore-buffer nil "30.1")

(defcustom auto-revert-use-notify t
  "Use system provided file notification."
  :group 'auto-revert
  :type 'boolean
  :set (lambda (variable value)
	 (set-default variable value)
	 (unless (symbol-value variable)
	   (dolist (buf (buffer-list))
	     (with-current-buffer buf
	       (auto-revert-rm-watch)))))
  :initialize 'custom-initialize-default
  :version "24.4")

(defvaralias 'auto-revert-notify-exclude-dir-regexp
  'auto-revert-exclude-dir-regexp)
(defcustom auto-revert-exclude-dir-regexp
  mounted-file-systems
  "Regular expression of directories to be excluded from file notifications."
  :group 'auto-revert
  :type 'regexp
  :version "24.4")

(defcustom auto-revert-avoid-polling nil
  "Obsolesced."
  :group 'auto-revert
  :type 'boolean
  :set (lambda (variable value)
         (set-default variable value)
	 (auto-revert-set-timer))
  :version "27.1")
(make-obsolete-variable 'auto-revert-avoid-polling nil "30.1")

(defvar auto-revert--proximal-start 0)

(defvar auto-revert-buffer-list nil  "Obsolesced.")
(make-obsolete-variable 'auto-revert-buffer-list nil "30.1")

(defvar auto-revert-remaining-buffers ()
  "Buffers not checked when user input stopped execution.")
(make-obsolete-variable 'auto-revert-remaining-buffers nil "30.1")

(defvar-local auto-revert-tail-pos nil
  "Position of last known end of file.")
(put 'auto-revert-tail-pos 'permanent-local t) ; See Commentary

(defvaralias 'auto-revert-notify-watch-descriptor
  'auto-revert-watch-descriptor)
(defvar-local auto-revert-watch-descriptor nil
  "Each buffer references its watch descriptor.")
(put 'auto-revert-watch-descriptor 'permanent-local t) ; See Commentary

(defvar auto-revert--lookup-buffer nil
  "Alist of watch descriptor to buffer.")

(defvar auto-revert-debug nil "Obsolesced.")
(make-obsolete-variable 'auto-revert-debug nil "30.1")

(defun auto-revert-toggle-hooks (toggle)
  "TOGGLE is nil or non-nil."
  (dolist (entry
           ;; [HOOKS HOOK]
           `([after-set-visited-file-name-hook
              ,(lambda ()
                 (auto-revert-mode 0)
                 (auto-revert-mode))]
             [kill-buffer-hook
              ,(lambda ()
                 (auto-revert-mode 0))]))
    (cl-destructuring-bind (hooks hook)
        (append entry nil)
      (if toggle
          (add-hook hooks hook nil :local)
        (remove-hook hooks hook :local)))))

;;;###autoload
(define-minor-mode auto-revert-mode
  "Reverts unmodified buffer when file on disk changes.
Customize `global-auto-revert-mode' to enable universally.
Use `auto-revert-tail-mode' to effect tailing a buffer."
  :group 'auto-revert
  :lighter nil
  (auto-revert-toggle-hooks nil)
  (auto-revert-rm-watch)
  (auto-revert-unset-timer)
  (mapc #'kill-local-variable '())
  (when auto-revert-mode
    (auto-revert-tail-mode 0)
    (auto-revert-toggle-hooks t)
    (auto-revert-add-watch)
    (auto-revert-set-timer)))
(put 'auto-revert-mode 'permanent-local t) ; see Commentary

(defun turn-on-auto-revert-mode ()
  (when (and (or buffer-file-name
                 (and auto-revert-non-file-buffers
                      (not (string-prefix-p " " (buffer-name)))
                      (local-variable-p 'buffer-stale-function)
                      (local-variable-p 'revert-buffer-function)))
             (not auto-revert-mode))
    (auto-revert-mode)))

(defun auto-revert-tail-toggle-hooks (toggle)
  "TOGGLE is nil or non-nil."
  (dolist (entry
           ;; [HOOKS HOOK]
           `([before-save-hook ; prevent eating one's own poop
              ,(apply-partially #'auto-revert-tail-mode 0)]
             [kill-buffer-hook
              ,(apply-partially #'auto-revert-tail-mode 0)]))
    (cl-destructuring-bind (hooks hook)
        (append entry nil)
      (if toggle
          (add-hook hooks hook nil :local)
        (remove-hook hooks hook :local)))))

;;;###autoload
(define-minor-mode auto-revert-tail-mode
  "Follow as with the shell command `tail -f`."
  :group 'find-file :lighter " Tail"
  (auto-revert-tail-toggle-hooks nil)
  (when auto-revert-tail-mode
    (condition-case err
        (progn
          (auto-revert-mode 0)
          (unless buffer-file-name
            (error "This buffer is not visiting a file"))
          (when (buffer-modified-p)
            (error "Buffer modified"))
          (auto-revert-tail-toggle-hooks t)
          ;; 1- for `point-max' being one past final char.
          (setq auto-revert-tail-pos (1- (position-bytes (point-max))))
          (auto-revert-set-timer))
      (error (auto-revert-tail-mode 0)
             (error (error-message-string err))))))

;;;###autoload
(defun turn-on-auto-revert-tail-mode ()
  (auto-revert-tail-mode))
(make-obsolete 'turn-on-auto-revert-tail-mode nil "30.1")

;;;###autoload
(define-globalized-minor-mode global-auto-revert-mode
  auto-revert-mode turn-on-auto-revert-mode
  :group 'auto-revert
  :version "30.1")

(define-obsolete-function-alias 'auto-revert-notify-rm-watch
  #'auto-revert-rm-watch "30.1")
(defun auto-revert-rm-watch ()
  "Disable file notification for current buffer's associated file."
  (when-let ((desc auto-revert-watch-descriptor))
    (setq auto-revert--lookup-buffer
          (assoc-delete-all desc auto-revert--lookup-buffer))
    (file-notify-rm-watch desc)
    (setq auto-revert-watch-descriptor nil)))

(define-obsolete-function-alias 'auto-revert-notify-add-watch
  #'auto-revert-add-watch "30.1")
(defun auto-revert-add-watch ()
  (auto-revert-rm-watch)
  (when (and auto-revert-use-notify
             (or buffer-file-name
                 buffer-auto-revert-by-notification)
             (not (string-match-p auto-revert-exclude-dir-regexp
			          (expand-file-name default-directory)))
             (not (file-symlink-p (or buffer-file-name default-directory))))
    (setq auto-revert-watch-descriptor
	  (file-notify-add-watch
	   (expand-file-name (or buffer-file-name "") default-directory)
           (if buffer-file-name '(change attribute-change) '(change))
           #'auto-revert-handle-notification))
    (push (cons auto-revert-watch-descriptor (current-buffer))
          auto-revert--lookup-buffer)))

(defun auto-revert-handle-notification (event)
  "Handle an EVENT returned from file notification."
  (with-demoted-errors "Error while auto-reverting: %S"
    (cl-destructuring-bind (descriptor action file
                            &optional file1
                            &aux (buffer (assoc-default
                                          descriptor
                                          auto-revert--lookup-buffer)))
        event
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (eq action 'stopped)
             (auto-revert-rm-watch)
            (when (string-equal
                   (file-name-nondirectory buffer-file-name)
                   (file-name-nondirectory
                    (cond ((memq action '(renamed)) file1)
                          ((memq action '(attribute-changed changed created)) file)
                          (t ""))))
              (auto-revert--doit buffer))))))))

(defun auto-revert--doit (buffer)
  (with-current-buffer buffer
    (let ((remote-file-name-inhibit-cache t) ; force Tramp to reread file attrs
          eob-p eob-windows)
      (when (save-excursion
              ;; Revert if...
              (if (not buffer-file-name)
                  ;; non-file self-determines stale, or...
                  (funcall (or buffer-stale-function #'ignore) :noconfirm)
                ;; file is...
                (and
                 ;; local, or we enabled remote and it's connected, and...
                 (or (not (file-remote-p buffer-file-name))
                     (and auto-revert-remote-files
                          (file-remote-p buffer-file-name nil :connected)))
                 ;; either tailed or stale.
                 (or auto-revert-tail-mode
                     (funcall buffer-stale-function :noconfirm)))))
        ;; Historically, if before the revert, the point (or the
        ;; window point of any window) is at the end of buffer, it
        ;; moves to the new end of buffer afterwards.
        (when buffer-file-name
          (setq eob-p (eobp))
          (walk-windows
           (apply-partially (lambda (buffer* max* window)
                              (when (and (eq (window-buffer window) buffer*)
                                         (= (window-point window) max*))
                                (push window eob-windows)))
                            (current-buffer) (point-max))
           'no-mini t))
        (if auto-revert-tail-mode
            (auto-revert-tail--doit buffer)
          (ignore-errors    ; insulate against deleted file (Bug#23276)
            (revert-buffer 'ignore-auto 'dont-ask 'preserve-modes)))
        (when buffer-file-name
          (when eob-p (goto-char (point-max)))
          (dolist (window eob-windows)
            (set-window-point window (point-max))))
        (let ((revert-buffer-in-progress-p t))
          (vc-refresh-state))))))

(defun auto-revert-tail--doit (buffer)
  (with-current-buffer buffer
    (let ((size (file-attribute-size (file-attributes buffer-file-name))))
      (when (/= auto-revert-tail-pos size)
       (with-silent-modifications
         (let ((inhibit-read-only t))
           (run-hooks 'before-revert-hook)
           (undo-boundary)
           (save-restriction
	     (widen)
	     (save-excursion
	       (goto-char (point-max))
	       (insert-file-contents buffer-file-name nil
				     (if (< auto-revert-tail-pos size)
				         auto-revert-tail-pos
                                       ;; Else SIZE < TAIL-POS, i.e.,
                                       ;; file shrunk.  Append the whole
                                       ;; file.  SIZE not equal to
                                       ;; TAIL-POS because above
                                       ;; conditional.
                                       (ignore))
				     size)))
           (run-hooks 'after-revert-hook)
           (undo-boundary)
           (setq auto-revert-tail-pos size))))
      (set-visited-file-modtime))))

(define-obsolete-function-alias 'auto-revert-buffers
  #'auto-revert--cycle "30.1")
(defun auto-revert--cycle ()
  (save-match-data ; historical
    (while-no-input
      (prog1 nil
        (let ((bufs (cl-remove-if-not
                     (lambda (b)
                       (with-current-buffer b
                         (and (buffer-live-p b)
                              (or auto-revert-mode auto-revert-tail-mode))))
                     (buffer-list))))
          (dotimes (_i (length bufs))
            (let ((index (% auto-revert--proximal-start (length bufs))))
              (auto-revert--doit (nth index bufs))
              (setq auto-revert--proximal-start (1+ index)))))))))

(provide 'autorevert)

(run-hooks 'auto-revert-load-hook)

;;; autorevert.el ends here
