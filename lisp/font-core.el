;;; font-core.el --- Core interface to font-lock  -*- lexical-binding: t; -*-

;; Copyright (C) 1992-2023 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: languages, faces
;; Package: emacs

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

;;; Code:

;; This variable is used by mode packages that support Font Lock mode by
;; defining their own keywords to use for `font-lock-keywords'.  (The mode
;; command should make it buffer-local and set it to provide the set up.)
(defvar-local font-lock-defaults nil
  "Defaults for Font Lock mode specified by the major mode.
Defaults should be of the form:

 (KEYWORDS [KEYWORDS-ONLY [CASE-FOLD [SYNTAX-ALIST ...]]])

KEYWORDS may be a symbol (a variable or function whose value is the keywords
to use for fontification) or a list of symbols (specifying different levels
of fontification).

If KEYWORDS-ONLY is non-nil, syntactic fontification (strings and
comments) is not performed.

If CASE-FOLD is non-nil, the case of the keywords is ignored when fontifying.

If SYNTAX-ALIST is non-nil, it should be a list of cons pairs of the form
\(CHAR-OR-STRING . STRING) used to set the local Font Lock syntax table, for
keyword and syntactic fontification (see `modify-syntax-entry').

These item elements are used by Font Lock mode to set the variables
`font-lock-keywords', `font-lock-keywords-only',
`font-lock-keywords-case-fold-search', `font-lock-syntax-table'.

Further item elements are alists of the form (VARIABLE . VALUE) and are in no
particular order.  Each VARIABLE is made buffer-local before set to VALUE.

Currently, appropriate variables include `font-lock-mark-block-function'.
If this is non-nil, it should be a function with no args used to mark any
enclosing block of text, for fontification via \\[font-lock-fontify-block].
Typical values are `mark-defun' for programming modes or `mark-paragraph' for
textual modes (i.e., the mode-dependent function is known to put point and mark
around a text block relevant to that mode).

Other variables include that for syntactic keyword fontification,
`font-lock-syntactic-keywords' and those for buffer-specialized fontification
functions, `font-lock-fontify-buffer-function',
`font-lock-unfontify-buffer-function', `font-lock-fontify-region-function',
`font-lock-unfontify-region-function'.")
;; Autoload if this file no longer dumped.
;;;###autoload
(put 'font-lock-defaults 'risky-local-variable t)

(defvar font-lock-function 'font-lock-default-function
  "A function which is called when `font-lock-mode' is toggled.
It will be passed one argument, which is the current value of
`font-lock-mode'.")

(define-minor-mode font-lock-mode
  "Toggle syntax highlighting in this buffer (Font Lock mode).

When Font Lock mode is enabled, text is fontified as you type it:

 - Comments are displayed in `font-lock-comment-face';
 - Strings are displayed in `font-lock-string-face';
 - Certain other expressions are displayed in other faces
   according to the value of the variable `font-lock-keywords'.

To customize the faces (colors, fonts, etc.) used by Font Lock for
fontifying different parts of buffer text, use \\[customize-face].

Global Font Lock mode is active by default, and turns on Font
Lock mode for major modes amongst `font-lock-global-modes'.

While not recommended, Font Lock mode can be individually applied
by turning off Global Font Lock mode and adding `font-lock-mode'
to the major mode's hook, e.g.,

 (add-hook \\='c-mode-hook #\\='font-lock-mode)

For those major modes which support it, the variable
`font-lock-maximum-decoration' allows greater control over the
degree of highlighting.

Highlighting can be further customized with
`font-lock-add-keywords' and `font-lock-defaults'.

Expert users can specify their own `font-lock-function' to
supplant all of the above."
  :lighter nil
  (when (or noninteractive (eq (aref (buffer-name) 0) ?\s))
    ;; batch mode or buffer is hidden (name starts space).
    (setq font-lock-mode nil))

  ;; Add `font-lock-face' as an alias for the face property.
  (if font-lock-mode
      (progn
        (setq-local char-property-alias-alist
                    (copy-tree char-property-alias-alist))
        (let ((elt (assq 'face char-property-alias-alist)))
          (if elt
	      (unless (memq 'font-lock-face (cdr elt))
	        (setcdr elt (nconc (cdr elt) (list 'font-lock-face))))
	    (push (list 'face 'font-lock-face) char-property-alias-alist))))
    (setq-local char-property-alias-alist
                (copy-tree char-property-alias-alist))
    (when-let ((elt (assq 'face char-property-alias-alist)))
      (setcdr elt (remq 'font-lock-face (cdr elt)))
      (unless (cdr elt)
	(setq char-property-alias-alist
	      (delq elt char-property-alias-alist)))))

  (funcall font-lock-function font-lock-mode)

  (if font-lock-mode
      (add-hook 'change-major-mode-hook 'font-lock-change-mode nil t)
    (remove-hook 'change-major-mode-hook 'font-lock-change-mode t)))

(defun font-lock-change-mode ()
  "Douse fontification on major mode change."
  (font-lock-mode -1))

(defun font-lock-defontify ()
  "Clear out all `font-lock-face' properties in current buffer.
A major mode that uses `font-lock-face' properties might want to put
this function onto `change-major-mode-hook'."
  (let ((modp (buffer-modified-p))
	(inhibit-read-only t))
    (save-restriction
      (widen)
      (remove-list-of-text-properties (point-min) (point-max)
				      '(font-lock-face)))
    (restore-buffer-modified-p modp)))

(defun font-lock-registrable-p ()
  (or (derived-mode-p 'tree-sitter-prog-mode)
      font-lock-keywords
      font-lock-defaults))

(defun font-lock-default-function (activate)
  (when (font-lock-registrable-p)
    (if activate
        (font-lock-register)
      (font-lock-deregister))))

;; Cannot `define-obsolete-function-alias' because
;; `font-lock-mode' is interactive and `turn-on-font-lock' wasn't.
(make-obsolete 'turn-on-font-lock 'font-lock-mode "29.1")
(make-obsolete 'turn-on-font-lock-if-desired 'font-lock-mode "29.1")
(defalias 'turn-on-font-lock-if-desired 'turn-on-font-lock)
(defun turn-on-font-lock ()
  (font-lock-mode))

(defcustom font-lock-global-modes t
  "Modes for which Font Lock mode is automagically turned on.
Global Font Lock mode is controlled by the command `global-font-lock-mode'.
If nil, means no modes have Font Lock mode automatically turned on.
If t, all modes that support Font Lock mode have it automatically turned on.
If a list, it should be a list of `major-mode' symbol names for which Font Lock
mode should be automatically turned on.  The sense of the list is negated if it
begins with `not'.  For example:
 (c-mode c++-mode)
means that Font Lock mode is turned on for buffers in C and C++ modes only."
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (set :menu-tag "mode specific" :tag "modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t (symbol :tag "mode"))))
  :group 'font-lock)

(defun turn-on-font-lock-mode ()
  (unless (minibufferp)
    (when (cond ((eq font-lock-global-modes t)
	         t)
	        ((eq (car-safe font-lock-global-modes) 'not)
	         (not (memq major-mode (cdr font-lock-global-modes))))
	        (t
                 (memq major-mode font-lock-global-modes)))
      (let (inhibit-quit)
        (font-lock-mode)))))

(define-globalized-minor-mode global-font-lock-mode
  font-lock-mode
  turn-on-font-lock-mode
  :initialize 'custom-initialize-delay
  :init-value (not (or noninteractive emacs-basic-display))
  :group 'font-lock
  :version "22.1")

;;; End of Global Font Lock mode.

(provide 'font-core)

;;; font-core.el ends here
