;;; tree-sitter.el --- tree-sitter utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

;;; Code:

(eval-when-compile (require 'cl-lib))

;;; Activating tree-sitter

(defgroup tree-sitter
  nil
  "Tree-sitter is an incremental parser."
  :group 'tools)

(defcustom tree-sitter-disabled-modes nil
  "A list of major-modes for which tree-sitter support is disabled."
  :type '(list symbol))

(defcustom tree-sitter-maximum-size (* 4 1000 1000)
  "Maximum buffer size for enabling tree-sitter parsing."
  :type 'integer)

(defun tree-sitter-should-enable-p (&optional mode)
  "Return non-nil if MODE should activate tree-sitter support.
MODE defaults to the value of `major-mode'."
  (let* ((mode (or mode major-mode))
         (disabled (cl-loop
                    for disabled-mode in tree-sitter-disabled-modes
                    if (provided-mode-derived-p mode disabled-mode)
                    return t
                    finally return nil)))
    (and (not disabled)
         (< (buffer-size) tree-sitter-maximum-size))))

;;; Parser API supplement

(defun tree-sitter-get-parser (name)
  "Find the first parser with NAME in `tree-sitter-parser-list'.
Return nil if we can't find any."
  (unless (stringp name)
    (signal 'wrong-type-argument `(stringp ,name)))
  (catch 'found
    (dolist (parser tree-sitter-parser-list)
      (when (equal name (tree-sitter-parser-name parser))
        (throw 'found parser)))))

(defun tree-sitter-get-parser-create (language-symbol &optional name)
  "Find the first parser with name NAME.

First look for the parser in `tree-sitter-parser-list', if none
exists, create one using LANGUAGE-SYMBOL as the language, NAME
as the name in the current buffer, and return it.

NAME defaults to (symbol-name LANGUAGE-SYMBOL)."
  (or (tree-sitter-get-parser (or name (symbol-name language-symbol)))
      (tree-sitter-create-parser
       (current-buffer)
       language-symbol
       (or name (symbol-name language-symbol)))))

;;; Node API supplement

(defun tree-sitter-node-buffer (node)
  "Return the buffer in where NODE belongs."
  (tree-sitter-parser-buffer
   (tree-sitter-node-parser node)))

(defun tree-sitter-node-at (beg &optional end parser-or-name named)
  "Return the smallest node covering BEG to END.

If omitted, END equals to BEG.  Find node in current buffer.
Return nil if none find.  If NAMED non-nil, only look for named
node.  NAMED defaults to nil.

By default, use the first parser in `tree-sitter-parser-list';
but if PARSER-OR-NAME is non-nil, use the parser it represents:
if it is a parser, use that parser, if it is a name, look for the
parser with that name."
  (when-let ((root (tree-sitter-buffer-root-node parser-name)))
    (tree-sitter-node-descendant-for-range root beg end named)))

(defun tree-sitter-buffer-root-node (&optional parser-or-name)
  "Return the root node of the current buffer.

If PARSER-OR-NAME is nil, return the root node of the first
parser in `tree-sitter-parser-list'; otherwise find the parser
represented by PARSER-OR-NAME and return the root node of that
parser.  Return nil if couldn't find any.  PARSER-OR-NAME can be
either a parser object, or the name of a parser."
  (tree-sitter-parser-root-node
   (cond ((tree-sitter-parser-p parser-or-name) parser-or-name)
         ((null parser-or-name (car tree-sitter-parser-list)))
         (t (tree-sitter-get-parser parser-or-name)))))

(defun tree-sitter-filter-child (node pred &optional named)
  "Return children of NODE that satisfies PRED.

PRED is a function that takes one argument, the child node.  If
NAMED non-nil, only search for named node.  NAMED defaults to nil."
  (let ((child (tree-sitter-node-child node 0 named))
        result)
    (while child
      (when (funcall pred child)
        (push child result))
      (setq child (tree-sitter-node-next-sibling child named)))
    result))

(defun tree-sitter-node-content (node)
  "Return the buffer content corresponding to NODE."
  (with-current-buffer (tree-sitter-node-buffer node)
    (buffer-substring-no-properties
     (tree-sitter-node-start node)
     (tree-sitter-node-end node))))

(defun tree-sitter-parent-until (node pred)
  "Return the closest parent of NODE that satisfies PRED.

Return nil if none found.  PRED should be a function that takes
one argument, the parent node."
  (catch 'found
    (while node
      (setq node (tree-sitter-node-parent node))
      (when (funcall pred node)
        (throw 'found node)))))

;;; Lab

(define-derived-mode json-mode js-mode "JSON"
  "Major mode for JSON documents."
  (setq-local font-lock-tree-sitter-defaults
              '((json-tree-sitter-settings-1))))

(defvar json-tree-sitter-settings-1
  '(tree-sitter-json
    "(string) @font-lock-string-face
(true) @font-lock-constant-face
(false) @font-lock-constant-face
(null) @font-lock-constant-face"))

(defun ts-c-fontify-system-lib (beg end _)
  (put-text-property beg (1+ beg) 'face 'font-lock-preprocessor-face)
  (put-text-property (1- end) end 'face 'font-lock-preprocessor-face)
  (put-text-property (1+ beg) (1- end)
                     'face 'font-lock-string-face))

(define-derived-mode ts-c-mode prog-mode "TS C"
  "C mode with tree-sitter support."
  (if (tree-sitter-should-enable-p)
      (progn (setq-local font-lock-tree-sitter-defaults
                         '((ts-c-tree-sitter-settings-1)))
             (setq-local font-lock-defaults
                         (ignore t nil nil nil)))
    (setq-local font-lock-defaults
                '((c-font-lock-keywords
                   c-font-lock-keywords-1
                   c-font-lock-keywords-2
                   c-font-lock-keywords-3)
                  nil nil
                  ((95 . "w")
                   (36 . "w"))
                  c-beginning-of-syntax
                  (font-lock-mark-block-function . c-mark-function)))))

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.tsc\\'" . ts-c-mode))

(defvar ts-c-tree-sitter-settings-1
  '(tree-sitter-c "(null) @font-lock-constant-face
(true) @font-lock-constant-face
(false) @font-lock-constant-face

(comment) @font-lock-comment-face

(system_lib_string) @ts-c-fontify-system-lib

(unary_expression
  operator: _ @font-lock-negation-char-face)

(string_literal) @font-lock-string-face
(char_literal) @font-lock-string-face



(function_definition
  declarator: (identifier) @font-lock-function-name-face)

(declaration
  declarator: (identifier) @font-lock-function-name-face)

(function_declarator
 declarator: (identifier) @font-lock-function-name-face)



(init_declarator
 declarator: (identifier) @font-lock-variable-name-face)

(parameter_declaration
 declarator: (identifier) @font-lock-variable-name-face)

(preproc_def
 name: (identifier) @font-lock-variable-name-face)

(enumerator
 name: (identifier) @font-lock-variable-name-face)

(field_identifier) @font-lock-variable-name-face

(parameter_list
 (parameter_declaration
  (identifier) @font-lock-variable-name-face))

(pointer_declarator
 declarator: (identifier) @font-lock-variable-name-face)

(array_declarator
 declarator: (identifier) @font-lock-variable-name-face)

(preproc_function_def
 name: (identifier) @font-lock-variable-name-face
 parameters: (preproc_params
              (identifier) @font-lock-variable-name-face))



(type_identifier) @font-lock-type-face
(primitive_type) @font-lock-type-face

\"auto\" @font-lock-keyword-face
\"break\" @font-lock-keyword-face
\"case\" @font-lock-keyword-face
\"const\" @font-lock-keyword-face
\"continue\" @font-lock-keyword-face
\"default\" @font-lock-keyword-face
\"do\" @font-lock-keyword-face
\"else\" @font-lock-keyword-face
\"enum\" @font-lock-keyword-face
\"extern\" @font-lock-keyword-face
\"for\" @font-lock-keyword-face
\"goto\" @font-lock-keyword-face
\"if\" @font-lock-keyword-face
\"register\" @font-lock-keyword-face
\"return\" @font-lock-keyword-face
\"sizeof\" @font-lock-keyword-face
\"static\" @font-lock-keyword-face
\"struct\" @font-lock-keyword-face
\"switch\" @font-lock-keyword-face
\"typedef\" @font-lock-keyword-face
\"union\" @font-lock-keyword-face
\"volatile\" @font-lock-keyword-face
\"while\" @font-lock-keyword-face

\"long\" @font-lock-type-face
\"short\" @font-lock-type-face
\"signed\" @font-lock-type-face
\"unsigned\" @font-lock-type-face

\"#include\" @font-lock-preprocessor-face
\"#define\" @font-lock-preprocessor-face
\"#ifdef\" @font-lock-preprocessor-face
\"#ifndef\" @font-lock-preprocessor-face
\"#endif\" @font-lock-preprocessor-face
\"#else\" @font-lock-preprocessor-face
\"#elif\" @font-lock-preprocessor-face
"))

;;; Debug

(defun tree-sitter-inspect-node-at-point ()
  (interactive)
  (tooltip-show
   (tree-sitter-node-string
    (tree-sitter-node-at (point)))))

(define-minor-mode tree-sitter-inspect-mode
  "Shows the node at point."
  :lighter nil
  (if tree-sitter-inspect-mode
      (add-hook 'post-command-hook #'tree-sitter-inspect-node-at-point
                nil t)
    (remove-hook 'post-command-hook #'tree-sitter-inspect-node-at-point
                 t)))

(provide 'tree-sitter)

;;; tree-sitter.el ends here
