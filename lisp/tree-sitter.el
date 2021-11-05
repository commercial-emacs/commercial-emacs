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
(declare-function tree-sitter-changed-range "tree-sitter.c")
(declare-function tree-sitter-highlight-region "tree-sitter.c")

(defgroup tree-sitter
  nil
  "Tree-sitter is an incremental parser."
  :group 'tools)

(defcustom tree-sitter-resources-dir
  (if-let ((interactive (not noninteractive))
           (proper-dir
            (ignore-errors
              (file-name-directory
               (directory-file-name
                (with-temp-buffer
                  (let ((proc (start-process "tree-sitter-resources-dir"
                                             (current-buffer) "tree-sitter" "dump-libpath")))
                    (cl-loop repeat 10
                          while (process-live-p proc)
                          do (sleep-for 0 100)
                          finally (when (process-live-p proc) (kill-process proc)))
                    (car (split-string (buffer-substring-no-properties
                                        (point-min) (point-max)))))))))))
      proper-dir
    (or (getenv "XDG_CACHE_HOME")
        (concat (expand-file-name ".cache/tree-sitter" "~"))))
  "Follows dirs::cache_dir in the Rust dirs crate.
On Linux systems this is $XDG_CACHE_HOME/tree-sitter."
  :group 'tree-sitter
  :type 'directory
  :risky t
  :version "28.1")

(defcustom tree-sitter-mode-alist
  '((c++-mode . "cpp")
    (rust-mode . "rust")
    (rustic-mode . "rust")
    (emacs-lisp-mode . "elisp")
    (csharp-mode . "sharp")
    (haskell-mode . "haskell")
    (go-mode . "go")
    (sh-mode . "bash")
    (c-mode . "c")
    (html-mode . "html")
    (makefile-gmake-mode . "make")
    (makefile-mode . "make")
    (php-mode . "php")
    (ess-mode . "r")
    (sql-mode . "sql")
    (conf-toml-mode . "toml")
    (vhdl-mode . "vhdl")
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
  (font-lock-fontify-region pos nil font-lock-verbose))

(define-minor-mode tree-sitter-lock-mode
  "Tree-sitter font-lock minor mode."
  :lighter ""
  (if tree-sitter-lock-mode
      (progn
        (setq-local font-lock-fontify-region-function #'tree-sitter-fontify-region)
        (add-hook 'after-change-functions #'tree-sitter-fontify-refresh nil t))
    (kill-local-variable 'font-lock-fontify-region-function)
    (remove-hook 'after-change-functions #'tree-sitter-fontify-refresh t)))

(defun tree-sitter-fontify-region (beg _end loudly)
  "Presumably widened in `font-lock-fontify-region'."
  (ignore loudly)
  (let ((inhibit-point-motion-hooks t))
    (with-silent-modifications
      (save-excursion
        (save-match-data
          (let* ((changed-range (tree-sitter-changed-range))
                 (beg* (if changed-range
                           (min beg (cl-first changed-range))
                         beg))
                 (end* (if changed-range
                           (min (point-max)
                                (min (+ beg jit-lock-chunk-size)
                                     (cl-second changed-range)))
                         (point-max)))
                 (bounds (tree-sitter-highlight-region beg* end*))
                 (leftmost (if bounds (min beg* (car bounds)) beg*))
                 (rightmost (if bounds (max end* (cdr bounds)) end*)))
            (ignore leftmost rightmost)
            ;; (when loudly
            ;;   (princ (format "changed [%s %s], initial [%d %d], final [%d %d]\n"
            ;;                  (cl-first (tree-sitter-changed-range))
            ;;                  (cl-second (tree-sitter-changed-range))
            ;;                  beg end leftmost rightmost)
            ;;          #'external-debugging-output))
            ))))))

(defsubst tree-sitter-fontify-refresh (start &rest _args)
  (tree-sitter-do-fontify start))

(provide 'tree-sitter)

;;; tree-sitter.el ends here
