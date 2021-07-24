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

;;; Node & parser accessors

(defun tree-sitter-node-buffer (node)
  "Return the buffer in where NODE belongs."
  (tree-sitter-parser-buffer
   (tree-sitter-node-parser node)))

;;; Parser API supplement

(defun tree-sitter-get-parser (name)
  "Find the first parser with name NAME in `tree-sitter-parser-list'.
Return nil if we can't find any."
  (catch 'found
    (dolist (parser tree-sitter-parser-list)
      (when (equal name (tree-sitter-parser-name parser))
        (throw 'found parser)))))

(defun tree-sitter-get-parser-create (name language)
  "Find the first parser with name NAME in `tree-sitter-parser-list'.
If none exists, create one and return it.  LANGUAGE is passed to
`tree-sitter-create-parser' when creating the parser."
  (or (tree-sitter-get-parser name)
      (tree-sitter-create-parser (current-buffer) language name)))

;;; Node API supplement

(defun tree-sitter-node-beginning (node)
  "Return the start position of NODE."
  (byte-to-position (tree-sitter-node-start-byte node)))

(defun tree-sitter-node-end (node)
  "Return the end position of NODE."
  (byte-to-position (tree-sitter-node-end-byte node)))

(defun tree-sitter-node-in-range (beg end &optional parser-name named)
  "Return the smallest node covering BEG to END.
Find node in current buffer.  Return nil if none find.  If NAMED
non-nil, only look for named node.  NAMED defaults to nil.  By
default, use the first parser in `tree-sitter-parser-list'; but
if PARSER-NAME is non-nil, it specifies the name of the parser that
should be used."
  (when-let ((root (tree-sitter-parser-root-node
                    (if parser-name
                        (tree-sitter-get-parser parser-name)
                      (car tree-sitter-parser-list)))))
    (tree-sitter-node-descendant-for-byte-range
     root (position-bytes beg) (position-bytes end) named)))

(defun tree-sitter-filter-child (node pred &optional named)
  "Return children of NODE that satisfies PRED.
PRED is a function that takes one argument, the child node.  If
NAMED non-nil, only search named node.  NAMED defaults to nil."
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
     (tree-sitter-node-beginning node)
     (tree-sitter-node-end node))))

;;; Font-lock

(defvar-local tree-sitter-font-lock-settings nil
  "A list of settings for tree-sitter-based font-locking.

Each setting controls one parser (often of different language).
A settings is a list of form (NAME LANGUAGE PATTERN).  NAME is
the name given to the parser, by convention it is
\"font-lock-<language>\", where <language> is the language that
the parser uses.  LANGUAGE is the language object returned by
tree-sitter language dynamic modules.

PATTERN is a tree-sitter query pattern. (See manual for how to
write query patterns.)  This pattern should capture nodes with
either face names or function names.  If captured with a face
name, the node's corresponding text in the buffer is fontified
with that face; if captured with a function name, the function is
called with three arguments, BEG END NODE, where BEG and END
marks the span of the corresponding text, and NODE is the node
itself.")

(defun tree-sitter-fontify-region-function (beg end &optional verbose)
  "Fontify the region between BEG and END.
If VERBOSE is non-nil, print status messages.
\(See `font-lock-fontify-region-function'.)"
  (dolist (elm tree-sitter-font-lock-settings)
    (let ((parser-name (car elm))
          (language (nth 1 elm))
          (match-pattern (nth 2 elm)))
      (tree-sitter-get-parser-create parser-name language)
      (when-let ((node (tree-sitter-node-in-range beg end parser-name)))
        (let ((captures (tree-sitter-query-capture
                         node match-pattern
                         ;; specifying the range is important. More
                         ;; often than not, NODE will be the root
                         ;; node, and if we don't specify the range,
                         ;; we are basically querying the whole file.
                         (position-bytes beg) (position-bytes end))))
          (with-silent-modifications
            (while captures
              (let* ((face (caar captures))
                     (node (cdar captures))
                     (beg (tree-sitter-node-beginning node))
                     (end (tree-sitter-node-end node)))
                (cond ((facep face)
                       (put-text-property beg end 'face face))
                      ((functionp face)
                       (funcall face beg end node)))

                (if verbose
                    (message "Fontifying text from %d to %d with %s"
                             beg end face)))
              (setq captures (cdr captures))))
          `(jit-lock-bounds ,(tree-sitter-node-beginning node)
                            . ,(tree-sitter-node-end node)))))))


(define-derived-mode json-mode js-mode "JSON"
  "Major mode for JSON documents."
  (setq-local font-lock-fontify-region-function
              #'tree-sitter-fontify-region-function)
  (setq-local tree-sitter-font-lock-settings
              `(("font-lock-json"
                 ,(tree-sitter-json)
                 "(string) @font-lock-string-face
(true) @font-lock-constant-face
(false) @font-lock-constant-face
(null) @font-lock-constant-face"))))

(defun ts-c-fontify-system-lib (beg end _)
  (put-text-property beg (1+ beg) 'face 'font-lock-preprocessor-face)
  (put-text-property (1- end) end 'face 'font-lock-preprocessor-face)
  (put-text-property (1+ beg) (1- end)
                     'face 'font-lock-string-face))

(define-derived-mode ts-c-mode prog-mode "TS C"
  "C mode with tree-sitter support."
  (setq-local font-lock-fontify-region-function
              #'tree-sitter-fontify-region-function)
  (setq-local tree-sitter-font-lock-settings
              `(("font-lock-c"
                 ,(tree-sitter-c)
                 "(null) @font-lock-constant-face
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
\"#elif\" @font-lock-preprocessor-face"))))

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.tsc\\'" . ts-c-mode))

(provide 'tree-sitter)

;;; tree-sitter.el ends here
