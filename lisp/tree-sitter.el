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

(require 'font-lock)

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

(defun tree-sitter-fontify-region (beg end _loudly)
  "Presumably widened in `font-lock-fontify-region'."
  (with-silent-modifications
    (prog1 `(jit-lock-bounds ,beg . ,end)
      (let ((inhibit-point-motion-hooks t)
            (highlights (tree-sitter-highlights beg end))
            prevailing-face)
        (font-lock-unfontify-region beg end)
        (dolist (highlight highlights)
          (pcase highlight
            ('nil
             (setq prevailing-face nil))
            ((and (pred symbolp) face)
             (setq prevailing-face face))
	    (`(,pcase-beg . ,pcase-end)
	     (when prevailing-face
               (save-mark-and-excursion
                 (let ((mark-beg (make-marker))
                       (mark-end (make-marker))
                       (highlight (list 0 prevailing-face)))
                   (set-marker mark-beg (byte-to-position pcase-beg))
                   (set-marker mark-end (byte-to-position pcase-end))
                   (save-match-data
                     (set-match-data (list mark-beg mark-end))
                     (font-lock-apply-highlight highlight))))))))))))

(provide 'tree-sitter)

;;; tree-sitter.el ends here
