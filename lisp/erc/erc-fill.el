;;; erc-fill.el --- Filling IRC messages in various ways  -*- lexical-binding: t; -*-

;; Copyright (C) 2001-2004, 2006-2023 Free Software Foundation, Inc.

;; Author: Andreas Fuchs <asf@void.at>
;;         Mario Lang <mlang@delysid.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; URL: https://www.emacswiki.org/emacs/ErcFilling

;; This file is part of GNU Emacs.

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

;; This package implements filling of messages sent and received.  Use
;; `erc-fill-mode' to switch it on.  Customize `erc-fill-function' to
;; change the style.

;;; Code:

(require 'erc)
(require 'erc-stamp); for the timestamp stuff

(defgroup erc-fill nil
  "Filling means to reformat long lines in different ways."
  :group 'erc)

;;;###autoload(autoload 'erc-fill-mode "erc-fill" nil t)
(define-erc-module fill nil
  "Manage filling in ERC buffers.
ERC fill mode is a global minor mode.  When enabled, messages in
the channel buffers are filled."
  ;; FIXME ensure a consistent ordering relative to hook members from
  ;; other modules.  Ideally, this module's processing should happen
  ;; after "morphological" modifications to a message's text but
  ;; before superficial decorations.
  ((add-hook 'erc-insert-modify-hook #'erc-fill)
   (add-hook 'erc-send-modify-hook #'erc-fill))
  ((remove-hook 'erc-insert-modify-hook #'erc-fill)
   (remove-hook 'erc-send-modify-hook #'erc-fill)))

(defcustom erc-fill-prefix nil
  "Values used as `fill-prefix' for `erc-fill-variable'.
nil means fill with space, a string means fill with this string."
  :type '(choice (const nil) string))

(defcustom erc-fill-function 'erc-fill-variable
  "Function to use for filling messages.

Variable Filling with an `erc-fill-prefix' of nil:

<shortnick> this is a very very very long message with no
	    meaning at all

Variable Filling with an `erc-fill-prefix' of four spaces:

<shortnick> this is a very very very long message with no
    meaning at all

Static Filling with `erc-fill-static-center' of 27:

		<shortnick> foo bar baz
	 <a-very-long-nick> foo bar baz quuuuux
		<shortnick> this is a very very very long message with no
			    meaning at all

These two styles are implemented using `erc-fill-variable' and
`erc-fill-static'.  You can, of course, define your own filling
function.  Narrowing to the region in question is in effect while your
function is called.

A third style resembles static filling but \"wraps\" instead of
fills, courtesy of `visual-line-mode' mode, which ERC
automatically enables when this option is `erc-fill-wrap' or
`erc-fill-wrap-mode' is active.  Set `erc-fill-static-center' to
your preferred initial \"prefix\" width.  For adjusting the width
during a session, see the command `erc-fill-wrap-nudge'."
  :type '(choice (const :tag "Variable Filling" erc-fill-variable)
                 (const :tag "Static Filling" erc-fill-static)
                 (const :tag "Dynamic word-wrap" erc-fill-wrap)
                 function))

(defcustom erc-fill-static-center 27
  "Column around which all statically filled messages will be centered.
This column denotes the point where the ` ' character between
<nickname> and the entered text will be put, thus aligning nick
names right and text left.

Also used by the `erc-fill-function' variant `erc-fill-wrap' for
its initial leading \"prefix\" width."
  :type 'integer)

(defcustom erc-fill-variable-maximum-indentation 17
  "Don't indent a line after a long nick more than this many characters.
Set to nil to disable."
  :type 'integer)

(defcustom erc-fill-column 78
  "The column at which a filled paragraph is broken."
  :type 'integer)

;;;###autoload
(defun erc-fill ()
  "Fill a region using the function referenced in `erc-fill-function'.
You can put this on `erc-insert-modify-hook' and/or `erc-send-modify-hook'."
  (unless (erc-string-invisible-p (buffer-substring (point-min) (point-max)))
    (when erc-fill-function
      ;; skip initial empty lines
      (goto-char (point-min))
      (save-match-data
        (while (and (looking-at "[ \t\n]*$")
                    (= (forward-line 1) 0))))
      (unless (eobp)
        (save-restriction
          (narrow-to-region (point) (point-max))
          (funcall erc-fill-function))))))

(defun erc-fill-static ()
  "Fills a text such that messages start at column `erc-fill-static-center'."
  (save-restriction
    (goto-char (point-min))
    (looking-at "^\\(\\S-+\\)")
    (let ((nick (match-string 1)))
        (let ((fill-column (- erc-fill-column (erc-timestamp-offset)))
              (fill-prefix (make-string erc-fill-static-center 32)))
          (insert (make-string (max 0 (- erc-fill-static-center
                                         (length nick) 1))
                               32))
          (erc-fill-regarding-timestamp))
        (erc-restore-text-properties))))

(defun erc-fill-variable ()
  "Fill from `point-min' to `point-max'."
  (let ((fill-prefix erc-fill-prefix)
        (fill-column (or erc-fill-column fill-column)))
    (goto-char (point-min))
    (if fill-prefix
        (let ((first-line-offset (make-string (erc-timestamp-offset) 32)))
          (insert first-line-offset)
          (fill-region (point-min) (point-max) t t)
          (goto-char (point-min))
          (delete-char (length first-line-offset)))
      (save-match-data
        (let* ((nickp (looking-at "^\\(\\S-+\\)"))
               (nick (if nickp
                         (match-string 1)
                       ""))
               (fill-column (- erc-fill-column (erc-timestamp-offset)))
               (fill-prefix (make-string (min (+ 1 (length nick))
                                              (- fill-column 1)
                                              (or erc-fill-variable-maximum-indentation
                                                  fill-column))
                                         32)))
          (erc-fill-regarding-timestamp))))
    (erc-restore-text-properties)))

(defvar-local erc-fill--wrap-prefix nil)
(defvar-local erc-fill--wrap-value nil)

(define-erc-module fill-wrap nil
  "Fill style leveraging `visual-line-mode'.
This local module depends on the global `fill' module.  To use
it, either include `fill-wrap' in `erc-modules' or set
`erc-fill-function' to `erc-fill-wrap'.  You can also manually
invoke one of the minor-mode toggles."
  ((let (msg)
     (unless erc-fill-mode
       (unless (memq 'fill erc-modules)
         (setq msg
               (concat "WARNING: enabling default global module `fill' needed "
                       " by local module `fill-wrap'.  This will impact all"
                       " ERC sessions.  Add `fill' to `erc-modules' to avoid "
                       " this warning. See Info:\"(erc) Modules\" for more.")))
       (erc-fill-mode +1))
     (unless (eq erc-fill-function #'erc-fill-wrap)
       (setq-local erc-fill-function #'erc-fill-wrap))
     (when-let* ((vars (or erc--server-reconnecting erc--target-priors))
                 ((alist-get 'erc-fill-wrap-mode vars)))
       (setq erc-fill--wrap-value (alist-get 'erc-fill--wrap-value vars)
             erc-fill--wrap-prefix (alist-get 'erc-fill--wrap-prefix vars)))
     (when (eq erc-timestamp-use-align-to 'margin)
       (erc-timestamp--display-margin-mode +1))
     (setq erc-fill--wrap-value
           (or erc-fill--wrap-value erc-fill-static-center)
           ;;
           erc-fill--wrap-prefix
           (or erc-fill--wrap-prefix
               (list 'space :width erc-fill--wrap-value)))
     (visual-line-mode +1)
     (when msg
       (erc-display-error-notice nil msg))))
  ((when erc-timestamp--display-margin-mode
     (erc-timestamp--display-margin-mode -1))
   (kill-local-variable 'erc-button--add-nickname-face-function)
   (kill-local-variable 'erc-fill--wrap-prefix)
   (kill-local-variable 'erc-fill--wrap-value)
   (kill-local-variable 'erc-fill-function)
   (visual-line-mode -1))
  'local)

(defvar-local erc-fill--wrap-length-function nil
  "Function to determine length of perceived nickname.
It should return an integer representing the length of the
nickname, including any enclosing brackets, or nil, to fall back
to the default behavior of taking the length from the first word.")

(defun erc-fill-wrap ()
  "Use text props to mimic the effect of `erc-fill-static'.
See `erc-fill-wrap-mode' for details."
  (unless erc-fill-wrap-mode
    (erc-fill-wrap-mode +1))
  (save-excursion
    (goto-char (point-min))
    (let ((len (or (and erc-fill--wrap-length-function
                        (funcall erc-fill--wrap-length-function))
                   (progn (skip-syntax-forward "^-")
                          (- (point) (point-min))))))
      (erc-put-text-properties (point-min) (point-max)
                               '(line-prefix wrap-prefix) nil
                               `((space :width ,(- erc-fill--wrap-value 1 len))
                                 ,erc-fill--wrap-prefix)))))

;; This is an experimental helper for third-party modules.  You could,
;; for example, use this to automatically resize the prefix to a
;; fraction of the window's width on some event change.

(defun erc-fill--wrap-fix (&optional value)
  "Re-wrap from `point-min' to `point-max'.
Reset prefix to VALUE, when given."
  (save-excursion
    (when value
      (setq erc-fill--wrap-value value
            erc-fill--wrap-prefix (list 'space :width value)))
    (let ((inhibit-field-text-motion t)
          (inhibit-read-only t))
      (goto-char (point-min))
      (while (and (zerop (forward-line))
                  (< (point) (min (point-max) erc-insert-marker)))
        (save-restriction
          (narrow-to-region (pos-bol) (pos-eol))
          (erc-fill-wrap))))))

(defun erc-fill--wrap-nudge (arg)
  (save-excursion
    (save-restriction
      (widen)
      (let ((inhibit-field-text-motion t)
            (inhibit-read-only t) ; necessary?
            (p (goto-char (point-min))))
        (when (zerop arg)
          (setq arg (- erc-fill-static-center erc-fill--wrap-value)))
        (cl-incf (caddr erc-fill--wrap-prefix) arg)
        (cl-incf erc-fill--wrap-value arg)
        (while (setq p (next-single-property-change p 'line-prefix))
          (when-let ((v (get-text-property p 'line-prefix)))
            (cl-incf (caddr v) arg)
            (when-let
                ((e (text-property-not-all p (point-max) 'line-prefix v)))
              (goto-char e)))))))
  arg)

(defun erc-fill-wrap-nudge (arg)
  "Adjust `erc-fill-wrap' by ARG columns.
Offer to repeat command in a manner similar to
`text-scale-adjust'.  Note that misalignment may occur when
messages contain decorations applied by third-party modules.
See `erc-fill--wrap-fix' for a workaround."
  (interactive "p")
  (unless erc-fill--wrap-value
    (cl-assert (not erc-fill-wrap-mode))
    (user-error "Minor mode `erc-fill-wrap-mode' disabled"))
  (let ((total (erc-fill--wrap-nudge arg))
        (start (window-start))
        (marker (set-marker (make-marker) (point))))
    (when (zerop arg)
      (setq arg 1))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (dolist (key '(?+ ?= ?- ?0))
         (let ((a (pcase key
                    (?0 0)
                    (?- (- (abs arg)))
                    (_ (abs arg)))))
           (define-key map (vector (list key))
                       (lambda ()
                         (interactive)
                         (cl-incf total (erc-fill--wrap-nudge a))
                         (set-window-start (selected-window) start)
                         (goto-char marker)))))
       map)
     t
     (lambda ()
       (set-marker marker nil)
       (message "Fill prefix: %d (%+d col%s)"
                erc-fill--wrap-value total (if (> (abs total) 1) "s" "")))
     "Use %k for further adjustment"
     1)
    (goto-char marker)
    (set-window-start (selected-window) start)))

(defun erc-fill-regarding-timestamp ()
  "Fills a text such that messages start at column `erc-fill-static-center'."
  (fill-region (point-min) (point-max) t t)
  (goto-char (point-min))
  (forward-line)
  (indent-rigidly (point) (point-max) (erc-timestamp-offset)))

(defun erc-timestamp-offset ()
  "Get length of timestamp if inserted left."
  (if (and (boundp 'erc-timestamp-format)
           erc-timestamp-format
           (eq erc-insert-timestamp-function 'erc-insert-timestamp-left)
           (not erc-hide-timestamps))
      (length (format-time-string erc-timestamp-format))
    0))

(provide 'erc-fill)

;;; erc-fill.el ends here
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
