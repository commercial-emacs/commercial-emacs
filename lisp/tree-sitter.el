;;; tree-sitter.el --- tree-sitter utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

(defgroup tree-sitter
  nil
  "Tree-sitter is an incremental parser."
  :group 'tools)

(defcustom tree-sitter-mode-alist
  '((c++-mode . "cpp")
    (rust-mode . "rust")
    (sh-mode . "bash")
    (c-mode . "c")
    (go-mode . "go")
    (html-mode . "html")
    (java-mode . "java")
    (js-mode . "javascript")
    (python-mode . "python")
    (ruby-mode . "ruby"))
  "Map prog-mode to tree-sitter grammar."
  :group 'tree-sitter
  :type '(alist :key-type (symbol :tag "Prog mode")
                :value-type (string :tag "Tree-sitter symbol"))
  :risky t
  :version "28.1")

(defcustom tree-sitter-highlight-alist
  '(("constant" . font-lock-constant-face)
    ("type.builtin" . font-lock-type-face)
    ("operator" . font-lock-builtin-face)
    ("variable.parameter" . font-lock-variable-name-face)
    ("function.builtin" . font-lock-function-name-face)
    ;; ("punctuation.delimiter" . font-lock-constant-face)
    ("attribute" . font-lock-variable-name-face)
    ;; ("punctuation.bracket" . font-lock-constant-face)
    ("string" . font-lock-string-face)
    ("variable.builtin" . font-lock-builtin-face)
    ("comment" . font-lock-comment-face)
    ("number" . font-lock-constant-face)
    ("type" . font-lock-type-face)
    ("embedded" . font-lock-builtin-face)
    ("function" . font-lock-function-name-face)
    ("keyword" . font-lock-keyword-face)
    ("constructor" . font-lock-function-name-face)
    ("property" . font-lock-variable-name-face)
    ("tag" . font-lock-type-face)
    ("string.special" . font-lock-string-face)
    ("constant.builtin" . font-lock-constant-face))
  "Map tree-sitter highlight name to font lock face."
  :group 'tree-sitter
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (mapc (lambda (x)
                 (when (listp (cdr x))
                   (setcdr x (cadr x))))
               value)
         (if-let ((problem (seq-find (lambda (x)
                                       (or (not (stringp (car x)))
                                           (not (facep (cdr x)))))
                                     value)))
             (error "Bad setting %S" problem)
           (set-default symbol value)))
  :type '(alist :key-type (string :tag "Tree-sitter highlight")
                :value-type (symbol :tag "Font lock face"))
  :risky t
  :version "28.1")

;;; Node API supplement

;;; Query API suuplement

;;; Language API supplement

;;; Range API supplement

;;; Indent

;;; Debugging

(define-minor-mode tree-sitter-mode
  "Tree-sitter minor mode."
  :lighter " TS"
  (setq tree-sitter-mode
        (and tree-sitter-mode
             (not (eq (aref (buffer-name) 0) ?\s)))))

(defcustom tree-sitter-global-modes t
  "Modes for which tree-sitter mode is automagically turned on.
If nil, means no modes have tree-sitter mode automatically turned
on.  If t, all modes that support tree-sitter mode have it
automatically turned on.  If a list, it should be a list of
`major-mode' symbol names for which tree-sitter mode should be
automatically turned on.  The sense of the list is negated if it
begins with `not'.  For example:
 (c-mode c++-mode)
means that tree-sitter mode is turned on for buffers in C and C++ modes only."
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (set :menu-tag "mode specific" :tag "modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t (symbol :tag "mode"))))
  :group 'tree-sitter)

(defun turn-on-tree-sitter ()
  (when (cond ((eq tree-sitter-global-modes t)
	       t)
	      ((eq (car-safe tree-sitter-global-modes) 'not)
	       (not (memq major-mode (cdr tree-sitter-global-modes))))
	      (t
               (memq major-mode tree-sitter-global-modes)))
    (let (inhibit-quit)
      (tree-sitter-mode))))

(define-globalized-minor-mode global-tree-sitter-mode
  tree-sitter-mode turn-on-tree-sitter
  :initialize 'custom-initialize-delay
  :init-value (and (not noninteractive) (not emacs-basic-display))
  :group 'tree-sitter
  :version "28.1")

(provide 'tree-sitter)

;;; tree-sitter.el ends here
