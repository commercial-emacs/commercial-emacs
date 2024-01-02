;;; autorevert.el --- revert buffers when files on disk change  -*- lexical-binding:t -*-

;; Copyright (C) 1997-1999, 2001-2024 Free Software Foundation, Inc.

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
;; Why are certain variables marked permanent-local?
;;
;; Liberal, uncalculated invocations of after-change-major-mode-hook
;; in `normal-mode' and `run-mode-hooks' threaten undesirable repeated
;; activations of auto-revert-mode, which we avoid by checking if
;; `auto-revert-mode' is already true.  Liberal, uncalculated
;; invocations of `kill-all-local-variables' in `normal-mode' and
;; `fundamental-mode' threaten to clear `auto-revert-mode'.  Marking a
;; variable permanent-local insulates it against being so cleared.

;; Rather than (correctly) sequencing calls to maintain state, Emacs
;; has long wallowed in the strictly accretive ball-of-mud paradigm
;; that eschews rearranging decades-old code when adding crutches like
;; permanent-local is so much safer (and prone to errors of omission)!

;;; Code:

(require 'filenotify)

(defgroup auto-revert nil
  "Revert unmodified buffer when file on disk changes."
  :group 'files
  :group 'convenience)

(defvar auto-revert-timer nil "Obsolesced")
(make-obsolete-variable 'auto-revert-timer nil "30.1")

(defun auto-revert-buffer-needs-timer-p (buffer)
  "Return t if BUFFER's revert is polling-based not event-based."
  (defvar auto-revert-mode)
  (defvar auto-revert-watch-descriptor)
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and auto-revert-mode
              (not auto-revert-watch-descriptor)))))

(defsubst auto-revert-timer-p (timer)
  (eq (timer--function timer) #'auto-revert--cycle))

(define-obsolete-function-alias 'auto-revert-set-timer
  #'auto-revert-ensure-timer "30.1")
(defun auto-revert-ensure-timer ()
  (interactive) ; historical from auto-revert-set-timer
  (let ((needs-timer-p (cl-some #'auto-revert-buffer-needs-timer-p (buffer-list)))
        (timer-extant-p (cl-some #'auto-revert-timer-p timer-list)))
    ;; Only take action if needs-timer-p != timer-extant-p
    (cond ((and (not needs-timer-p) timer-extant-p)
           (mapc (lambda (timer)
                   (when (auto-revert-timer-p timer)
                     (cancel-timer timer)))
                 timer-list))
          ((and needs-timer-p (not timer-extant-p))
           (defvar auto-revert-interval)
           (setq auto-revert-timer (run-with-timer auto-revert-interval
                                                   auto-revert-interval
                                                   #'auto-revert--cycle))))))

(defcustom auto-revert-interval 5
  "Time in seconds between auto-revert checks."
  :group 'auto-revert
  :type 'number
  :set (lambda (variable value)
	 (set-default variable value)
         (when (featurep 'autorevert) ; custom subsystem inception
	   (auto-revert-ensure-timer))))

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
         (when (featurep 'autorevert) ; custom subsystem inception
           (dolist (buf (cl-remove-if-not
                         (lambda (b)
                           (defvar auto-revert-mode)
                           (with-current-buffer b auto-revert-mode))
                         (buffer-list)))
             (with-current-buffer buf
               (auto-revert-mode)))))
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
  :version "27.1")
(make-obsolete-variable 'auto-revert-avoid-polling nil "30.1")

(defvar auto-revert--proximal-start 0
  "A last-we-left-off index ensuring no buffers get
short-changed in auto-revert--cycle")

(defvar auto-revert-buffer-list nil "Obsolesced.")
(make-obsolete-variable 'auto-revert-buffer-list nil "30.1")

(defvar auto-revert-remaining-buffers nil
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
  (mapc #'kill-local-variable '())
  (when auto-revert-mode
    (auto-revert-toggle-hooks t)
    (auto-revert-add-watch))
  (auto-revert-ensure-timer))
(put 'auto-revert-mode 'permanent-local t) ; see Commentary

;;;###autoload
(defun turn-on-auto-revert-mode ()
  (when (and (not auto-revert-mode)
             ;; weak substitute for non-nil inhibit_buffer_hooks
             (not (eq (aref (buffer-name) 0) ? ))
             (or buffer-file-name
                 (and auto-revert-non-file-buffers
                      (not (string-prefix-p " " (buffer-name)))
                      (local-variable-p 'buffer-stale-function)
                      (local-variable-p 'revert-buffer-function))))
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
  "Follow as with the shell command `tail -f`.
This mode essentially derives from `auto-revert-mode'
(but `define-derived-mode' only applies to major modes)."
  :group 'auto-revert :lighter " Tail"
  (auto-revert-tail-toggle-hooks nil)
  (auto-revert-mode 0)
  (when auto-revert-tail-mode
    (condition-case err
        (progn
          (unless buffer-file-name
            (error "This buffer is not visiting a file"))
          (when (buffer-modified-p)
            (error "Buffer modified"))
          (auto-revert-mode)
          (auto-revert-tail-toggle-hooks t)
          ;; 1- for `point-max' being one past final char.
          (setq auto-revert-tail-pos (1- (position-bytes (point-max)))))
      (error (auto-revert-tail-mode 0)
             (error (error-message-string err))))))

(define-obsolete-function-alias 'turn-on-auto-revert-tail-mode
  #'auto-revert-tail-mode "30.1")

;;;###autoload
(define-globalized-minor-mode global-auto-revert-mode
  auto-revert-mode turn-on-auto-revert-mode
  :group 'auto-revert
  :version "30.1")

(define-obsolete-function-alias 'auto-revert-notify-rm-watch
  #'auto-revert-rm-watch "30.1")
(defun auto-revert-rm-watch ()
  (when-let ((desc auto-revert-watch-descriptor))
    (setq auto-revert--lookup-buffer
          (assoc-delete-all desc auto-revert--lookup-buffer))
    (ignore-errors (file-notify-rm-watch desc))
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
	  (ignore-errors
            (file-notify-add-watch
	     (expand-file-name (or buffer-file-name "") default-directory)
             (if buffer-file-name '(change attribute-change) '(change))
             #'auto-revert-handle-notification)))
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
              (apply #'auto-revert-mode
                     ;; obfuscatory retoggling of minor mode
                     (unless auto-revert-mode (list 0)))
            (when (if (null buffer-file-name)
                      (memq action '(created renamed deleted))
                    (string-equal
                     (file-name-nondirectory buffer-file-name)
                     (file-name-nondirectory
                      (cond ((memq action '(renamed)) file1)
                            ((memq action '(attribute-changed changed created)) file)
                            (t "")))))
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
        ;; window point of any window) is at eob, it moves to the new
        ;; eob afterwards.
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
          ;; ignore errors for deleted files (Bug#23276)
          (ignore-errors (revert-buffer 'ignore-auto 'dont-ask)))
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
  (while-no-input
    (let ((bufs (cl-remove-if-not #'auto-revert-buffer-needs-timer-p
                                  (buffer-list))))
      (dotimes (_i (length bufs))
        ;; auto-revert--proximal-start ensures no buffers get
        ;; short-changed due to while-no-input interruption.
        (let ((index (% auto-revert--proximal-start (length bufs))))
          (auto-revert--doit (nth index bufs))
          (setq auto-revert--proximal-start (1+ index)))))))

(provide 'autorevert)

(run-hooks 'auto-revert-load-hook)

;;; autorevert.el ends here
