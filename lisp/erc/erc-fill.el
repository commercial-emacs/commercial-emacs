;;; erc-fill.el --- Filling IRC messages in various ways  -*- lexical-binding: t; -*-

;; Copyright (C) 2001-2004, 2006-2021 Free Software Foundation, Inc.

;; Author: Andreas Fuchs <asf@void.at>
;;         Mario Lang <mlang@delysid.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>
;; URL: https://www.emacswiki.org/emacs/ErcFilling

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
(define-minor-mode erc-fill-mode
  "Toggle ERC fill mode.
With a prefix argument ARG, enable ERC fill mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

ERC fill mode is a global minor mode.  When enabled, messages in
the channel buffers are filled."
  :global t
  (if erc-fill-mode
      (erc-fill-enable)
    (erc-fill-disable)))

(defun erc-fill-enable ()
  "Setup hooks for `erc-fill-mode'."
  (interactive)
  (add-hook 'erc-insert-modify-hook #'erc-fill)
  (add-hook 'erc-send-modify-hook #'erc-fill))

(defun erc-fill-disable ()
  "Cleanup hooks, disable `erc-fill-mode'."
  (interactive)
  (remove-hook 'erc-insert-modify-hook #'erc-fill)
  (remove-hook 'erc-send-modify-hook #'erc-fill))

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
function is called."
  :type '(choice (const :tag "Variable Filling" erc-fill-variable)
                 (const :tag "Static Filling" erc-fill-static)
                 function))

(defcustom erc-fill-static-center 27
  "Column around which all statically filled messages will be centered.
This column denotes the point where the ` ' character between
<nickname> and the entered text will be put, thus aligning nick
names right and text left."
  :type 'integer)

(defcustom erc-fill-variable-maximum-indentation 17
  "Don't indent a line after a long nick more than this many characters.
Set to nil to disable."
  :type 'integer)

(defcustom erc-fill-column 78
  "The column at which a filled paragraph is broken."
  :type 'integer)

(defun erc-fill--remove-stamp-right ()
  (goto-char (point-min))
  (let (changed)
    (while
        (when-let* ((nextf (next-single-property-change (point) 'field)))
          (goto-char (field-end nextf t))
          ;; Sweep up residual phantom field remants
          (delete-region nextf (field-end nextf t))
          (setq changed t)))
    changed))

(defun erc-fill--remove-stamp-left ()
  "Remove at most one LEFT or one right timestamp, if any."
  (goto-char (point-min))
  ;; FIXME actually, it may be a mistake to blow past white space
  ;; without checking for intervening intervals that need cleaning up.
  (when-let* ((beg (save-excursion (skip-syntax-forward ">-") (point)))
              (nextf (when (eq 'erc-timestamp (field-at-pos beg))
                       (field-beginning beg t)))
              ((eq 'erc-timestamp (get-text-property nextf 'field))))
    (goto-char (field-end nextf t))
    (skip-syntax-forward "-")
    (delete-region nextf (point))
    t))

(defun erc-fill--hack-csf (f)
  ;; HACK until necessary additions to erc-stamp.el arrive (possibly
  ;; with erc-v3 in #49860), there's no civilized way of detecting the
  ;; bounds of a displayed message after initial insertion.
  ;;
  ;; These callback closures are used for that purpose, but they also
  ;; contain the timestamp we need.  An unforeseen benefit of this
  ;; awkwardness is that it plays well with `text-property-not-all',
  ;; which needs unique values to match against.  That wouldn't be the
  ;; case were we to use lisp time objects instead because successive
  ;; messages might contain the exact same one.
  (if (byte-code-function-p f) (aref (aref f 2) 0) (alist-get 'ct (cadr f))))

;; Enabling `erc-fill-mode' is ultimately destructive to preformatted
;; text (like ASCII art and figlets), which degenerate immediately
;; upon display.  This is permanent because we don't store original
;; messages (though with IRCv3, it may be possible to request a
;; replacement from the server).
(defun erc-fill--refill ()
  (let ((m (make-marker))
        (reporter (unless noninteractive
                    (make-progress-reporter "filling" 0 (point-max))))
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        ;;
        left-changed right-changed ct) ; cached current time
    (cl-letf (((symbol-function #'erc-restore-text-properties) #'ignore)
              ((symbol-function #'current-time) (lambda () ct)))
      (while
          (save-excursion
            (goto-char (or (marker-position m) (set-marker m (point-min))))
            (when-let*
                ((beg (if (get-text-property (point) 'cursor-sensor-functions)
                          (point)
                        (when-let*
                            ((max (min (point-max) (+ 512 (point))))
                             (res (next-single-property-change
                                   (point) 'cursor-sensor-functions nil max))
                             ((/= res max))) ; otherwise, we're done.
                          res)))
                 (val (get-text-property beg 'cursor-sensor-functions))
                 (beg (progn ; remove left padding, if any.
                        (goto-char beg)
                        (skip-syntax-forward "-")
                        (delete-region (min (line-beginning-position) beg)
                                       (point))
                        (point)))
                 ;; Don't expect output limited to IRC message length.
                 (end (text-property-not-all beg (point-max)
                                             'cursor-sensor-functions val)))
              (save-restriction
                (narrow-to-region beg end)
                (setq left-changed (erc-fill--remove-stamp-left))
                ;; If NOSQUEEZE seems warranted, see note above.
                (let ((fill-column (- (point-max) (point-min))))
                  (fill-region (point-min) (point-max)))
                (setq right-changed (erc-fill--remove-stamp-right))
                (erc-fill)
                (when (setq ct (when (or left-changed right-changed)
                                 (erc-fill--hack-csf (car val))))
                  (when left-changed
                    (setq erc-timestamp-last-inserted-left nil))
                  (when right-changed
                    (setq erc-timestamp-last-inserted-right nil))
                  (erc-add-timestamp))
                (when reporter
                  (cl-incf (aref (cdr reporter) 2) ; max += d_new - d_old
                           (- (point-max) (point-min) end (- beg))))
                (set-marker m (goto-char (point-max))))))
        (when reporter
          (progress-reporter-update reporter (point)))
        (thread-yield)))))

(defvar-local erc-fill--refill-thread nil
  "A thread running a buffer-refill job.")

(define-error 'erc-fill-canceled "ERC refill canceled" 'error)

(defun erc-fill-buffer (force)
  "Refill an ERC buffer.
With FORCE, cancel an active refill job if one exists."
  (interactive "P")
  (when (and erc-fill--refill-thread
             (thread-live-p erc-fill--refill-thread))
    (if force
        (thread-signal erc-fill--refill-thread
                       'erc-fill-canceled (list (buffer-name)))
      (user-error "Already refilling.")))
  (setq erc-fill--refill-thread (make-thread #'erc-fill--refill "erc-fill")))

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
  (save-match-data
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
