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

(defun tree-sitter-do-fontify (pos)
  "Analog to `jit-lock-fontify-now' without all the indirection."
  (let ((inhibit-point-motion-hooks t)
        (end (or (text-property-any pos (point-max) 'fontified t)
                 (point-max))))
    (with-silent-modifications
      (save-excursion
        (save-match-data
          (font-lock-fontify-region pos end))))))

(define-minor-mode tree-sitter-lock-mode
  "Tree-sitter font-lock minor mode."
  :lighter ""
  (if tree-sitter-lock-mode
      (progn
        (setq-local font-lock-fontify-region-function #'tree-sitter-fontify-region)
        (add-hook 'fontification-functions #'tree-sitter-do-fontify nil t))
    (kill-local-variable 'font-lock-fontify-region-function)
    (remove-hook 'fontification-functions #'tree-sitter-do-fontify t)))

(defun tree-sitter-fontify-region (beg end _loudly)
  "Presumably widened in `font-lock-fontify-region'."
  (let* ((changed-range (tree-sitter-changed-range))
         (left (if changed-range
                       (min beg (cl-first changed-range))
                     beg))
         (right (if changed-range
                        (max end (cl-second changed-range))
                      end))
         (highlights (tree-sitter-highlights left right))
         (leftmost left)
         (rightmost right)
         prevailing-face)
    (dolist (highlight highlights)
      (pcase highlight
        ('nil
         (setq prevailing-face nil))
        ((and (pred symbolp) face)
         (setq prevailing-face face))
	(`(,byte-beg . ,byte-end)
         (let ((pcase-beg (byte-to-position byte-beg))
               (pcase-end (byte-to-position byte-end)))
           (setq leftmost (min leftmost pcase-beg))
           (setq rightmost (max rightmost pcase-end))
	   (when prevailing-face
             (save-excursion
               (let ((mark-beg (make-marker))
                     (mark-end (make-marker))
                     (highlight (list 0 prevailing-face)))
                 (set-marker mark-beg pcase-beg)
                 (set-marker mark-end pcase-end)
                 (save-match-data
                   (set-match-data (list mark-beg mark-end))
                   (font-lock-apply-highlight highlight)))))))))
    (princ (format "hummina changed [%s %s], initial [%d %d], final [%d %d]\n"
                   (cl-first (tree-sitter-changed-range))
                   (cl-second (tree-sitter-changed-range))
                   beg end leftmost rightmost)
           #'external-debugging-output)
    (put-text-property leftmost rightmost 'fontified nil)
    (put-text-property left right 'fontified t)))

(provide 'tree-sitter)

;;; tree-sitter.el ends here
