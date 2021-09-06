;;; mb-depth.el --- Indicate minibuffer-depth in prompt -*- lexical-binding: t -*-
;;
;; Copyright (C) 2006-2021 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: convenience

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
;;
;; Defines the minor mode `minibuffer-depth-indicate-mode'.
;;
;; When active, any recursive use of the minibuffer will show
;; the recursion depth in the minibuffer prompt.  This is only
;; useful if `enable-recursive-minibuffers' is non-nil.

;;; Code:

(defcustom minibuffer-depth-indicator-function nil
  "If non-nil, a function to produce the minibuffer depth indicator.
The function will be called with one argument, the minibuffer depth,
and must return a string to display as indication of the minibuffer
depth.
If nil, display the depth as a number inside brackets, [NN], with
the `minibuffer-depth-indicator' face."
  :version "28.1"
  :type '(choice (const :tag "Default indicator display, [NN]" nil)
                 (function))
  :group 'minibuffer)

(defface minibuffer-depth-indicator '((t :inherit highlight))
  "Face to use for minibuffer depth indicator."
  :group 'minibuffer
  :version "28.1")

(defface minibuffer-depth-nonselected
  '((t (:background "yellow" :foreground "dark red" :weight bold)))
  "Face for non-selected minibuffer prompts.
It's used after leaving the minibuffer window while the minibuffer remains active."
  :group 'minibuffer
  :version "28.1")

(defcustom minibuffer-depth-indicate-nonselected t
  "If non-nil, indicate the non-selected minibuffer.
Use the face `minibuffer-depth-nonselected'."
  :type 'boolean
  :group 'minibuffer
  :version "28.1")

;; An overlay covering the prompt.  This is a buffer-local variable in
;; each affected minibuffer.
;;
(defvar-local minibuffer-depth-overlay nil)
(defvar-local minibuffer-depth-nonselected-overlay nil)

(defun minibuffer-depth-select (w)
  (if (eq (window-buffer w) (current-buffer))
      (when (overlayp minibuffer-depth-nonselected-overlay)
        (delete-overlay minibuffer-depth-nonselected-overlay))
    (unless (eq major-mode 'completion-list-mode)
      (with-current-buffer (window-buffer w)
        (let ((ov (make-overlay (point-min) (point-max))))
          (overlay-put ov 'face 'minibuffer-depth-nonselected)
          (overlay-put ov 'evaporate t)
          (setq minibuffer-depth-nonselected-overlay ov))))))

;; This function goes on minibuffer-setup-hook
(defun minibuffer-depth-setup ()
  "Set up a minibuffer for `minibuffer-depth-indicate-mode'.
The prompt should already have been inserted."
  (let ((depth (minibuffer-depth)))
    (when (> depth 1)
      (let ((pos (point-min)))
        (setq minibuffer-depth-overlay (make-overlay pos (1+ pos))))
      (overlay-put minibuffer-depth-overlay 'before-string
                   (if minibuffer-depth-indicator-function
                       (funcall minibuffer-depth-indicator-function depth)
                     (concat (propertize (format "[%d]" depth)
                                         'face
                                         'minibuffer-depth-indicator)
                             " ")))
      (overlay-put minibuffer-depth-overlay 'evaporate t)))
  (when minibuffer-depth-indicate-nonselected
    (add-hook 'window-state-change-functions 'minibuffer-depth-select nil t)))

;;;###autoload
(define-minor-mode minibuffer-depth-indicate-mode
  "Toggle Minibuffer Depth Indication mode.

Minibuffer Depth Indication mode is a global minor mode.  When
enabled, any recursive use of the minibuffer will show the
recursion depth in the minibuffer prompt.  This is only useful if
`enable-recursive-minibuffers' is non-nil."
  :global t
  :group 'minibuffer
  (if minibuffer-depth-indicate-mode
      ;; Enable the mode
      (add-hook 'minibuffer-setup-hook 'minibuffer-depth-setup)
    ;; Disable the mode
    (remove-hook 'minibuffer-setup-hook 'minibuffer-depth-setup)))

(provide 'mb-depth)

;;; mb-depth.el ends here
