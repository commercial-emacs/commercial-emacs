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

(defcustom tree-sitter-maximum-size (* 4 1024 1024)
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

(defun tree-sitter-get-parser (language)
  "Find the first parser using LANGUAGE in `tree-sitter-parser-list'."
  (catch 'found
    (dolist (parser tree-sitter-parser-list)
      (when (eq language (tree-sitter-parser-language parser))
        (throw 'found parser)))))

(defun tree-sitter-get-parser-create (language)
  "Find the first parser using LANGUAGE in `tree-sitter-parser-list'.
If none exists, create one and return it."
  (or (tree-sitter-get-parser language)
      (tree-sitter-create-parser
       (current-buffer) language)))

(defun tree-sitter-query-string (pattern string language)
  "Query STRING with PATTERN in LANGUAGE."
  (with-temp-buffer
    (insert string)
    (let ((parser (tree-sitter-create-parser (current-buffer) language)))
      (tree-sitter-query-capture
       (tree-sitter-parser-root-node parser)
       pattern))))

(defun tree-sitter-parse-string (string language)
  "Parse STRING using a parser for LANGUAGE.
Return the root node of the syntax tree."
  (with-temp-buffer
    (insert string)
    (tree-sitter-parser-root-node
     (tree-sitter-create-parser (current-buffer) language))))

(defun tree-sitter-language-at (point)
  "Return the language used at POINT."
  (cl-loop for parser in tree-sitter-parser-list
           if (tree-sitter-node-at point nil parser)
           return (tree-sitter-parser-language parser)))

;;; Node API supplement

(defun tree-sitter-node-buffer (node)
  "Return the buffer in where NODE belongs."
  (tree-sitter-parser-buffer
   (tree-sitter-node-parser node)))

(defun tree-sitter-node-at (beg &optional end parser-or-lang named)
  "Return the smallest node covering BEG to END.

If omitted, END defaults to BEG.  Return nil if none find.  If
NAMED non-nil, only look for named node.  NAMED defaults to nil.

If PARSER-OR-LANG is nil, use the first parser in
`tree-sitter-parser-list'; if PARSER-OR-LANG is a parser, use
that parser; if PARSER-OR-LANG is a language, find a parser using
that language (symbol) and use that."
  (when-let ((root (if (tree-sitter-parser-p parser-or-lang)
                       (tree-sitter-parser-root-node parser-or-lang)
                     (tree-sitter-buffer-root-node parser-or-lang))))
    (tree-sitter-node-descendant-for-range root beg (or end beg) named)))

(defun tree-sitter-buffer-root-node (&optional language)
  "Return the root node of the current buffer.
PARSER-OR-LANG is like in `tree-sitter-node-at'."
  (tree-sitter-parser-root-node
   (if language (tree-sitter-get-parser language)
     (car tree-sitter-parser-list))))

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

(defun tree-sitter-node-content (node &optional object)
  "Return the buffer content corresponding to NODE.
If NODE is generated from parsing a string instead of a buffer,
pass that string to OBJECT."
  (if object
      (substring object
                 (tree-sitter-node-start node)
                 (tree-sitter-node-end node))
    (with-current-buffer (tree-sitter-node-buffer node)
      (buffer-substring-no-properties
       (tree-sitter-node-start node)
       (tree-sitter-node-end node)))))

(defun tree-sitter-parent-until (node pred)
  "Return the closest parent of NODE that satisfies PRED.

Return nil if none found.  PRED should be a function that takes
one argument, the parent node."
  (let ((node (tree-sitter-node-parent node)))
    (while (and node (not (funcall pred node)))
      (setq node (tree-sitter-node-parent node)))
    node))

(defun tree-sitter-parent-while (node pred)
  "Return the furthest parent of NODE that satisfies PRED.

Return nil if none found.  PRED should be a function that takes
one argument, the parent node."
  (let ((last nil))
    (while (and node (funcall pred node))
      (setq last node
            node (tree-sitter-node-parent node)))
    last))

(defun tree-sitter-node-children (node &optional named)
  "Return a list of NODE's children.
If NAMED is non-nil, count named child only."
  (mapcar (lambda (idx)
            (tree-sitter-node-child idx named))
          (number-sequence
           0 (1- (tree-sitter-node-child-count node named)))))

(defun tree-sitter-node-index (node &optional named)
  "Return the index of NODE in its parent.
If NAMED is non-nil, count named child only."
  (let ((count 0))
    (while (setq node (tree-sitter-node-prev-sibling node named))
      (cl-incf count))
    count))

(defun tree-sitter-node-field-name (node)
  "Return the field name of NODE as a child of its parent."
  (when-let ((parent (tree-sitter-node-parent node))
             (idx (tree-sitter-node-index node)))
    (tree-sitter-node-field-name-for-child parent idx)))

;;; Indent

(defvar tree-sitter--indent-verbose t
  "If non-nil, log progress when indenting.")

(defvar tree-sitter-simple-indent-rules nil
  "A list of indent rule settings.
Each indent rule setting should be (LANGUAGE . RULES),
where LANGUAGE is a language symbol, and RULES is a list of
(MATCHER ANCHOR OFFSET).

MATCHER determines whether this rule applies, ANCHOR and OFFSET
together determines which column to indent to.

A MATCHER is a function that takes three arguments (NODE PARENT
BOL).  NODE is the largest (highest-in-tree) node starting at
point.  PARENT is the parent of NODE.  BOL is the point where we
are indenting: the beginning of line content, the position of the
first non-whitespace character.

If MATCHER returns non-nil, meaning the rule matches, Emacs then
uses ANCHOR to find an anchor, it should be a function that takes
the same argument (NODE PARENT BOL) and returns a point.

Finally Emacs computes the column of that point returned by ANCHOR
and adds OFFSET to it, and indent the line to that column.

For MATCHER and ANCHOR, Emacs provides some convenient presets.
See `tree-sitter-simple-indent-presets'.

TODO: examples in manual")

(defvar tree-sitter-simple-indent-presets
  '((match . (lambda
               (&optional node-type parent-type node-field
                          node-index-min node-index-max)
               `(lambda (node parent bol &rest _)
                  (and (or (null ,node-type)
                           (equal (tree-sitter-node-type node)
                                  ,node-type))
                       (or (null ,parent-type)
                           (equal (tree-sitter-node-type parent)
                                  ,parent-type))
                       (or (null ,node-field)
                           (equal (tree-sitter-node-field-name node)
                                  ,node-field))
                       (or (null ,node-index-min)
                           (>= (tree-sitter-node-index node t)
                               ,node-index-min))
                       (or (null ,node-index-max)
                           (<= (tree-sitter-node-index node t)
                               ,node-index-max))))))
    (no-node . (lambda (node parent bol &rest _) (null node)))
    (node-at-point . (lambda (type named)
                       `(lambda (node parent bol &rest _)
                          (equal ,type (tree-sitter-node-type
                                        (tree-sitter-node-at
                                         bol nil nil ,named))))))
    (parent-is . (lambda (type)
                   `(lambda (node parent bol &rest _)
                      (equal ,type (tree-sitter-node-type parent)))))

    (node-is . (lambda (type)
                 `(lambda (node parent bol &rest _)
                    (equal ,type (tree-sitter-node-type node)))))

    (parent-match . (lambda (pattern)
                      `(lambda (node parent bol &rest _)
                         (cl-loop for capture
                                  in (tree-sitter-query-capture
                                      parent ,pattern)
                                  if (tree-sitter-node-eq
                                      (cdr capture) node)
                                  return t
                                  finally return nil))))
    (node-match . (lambda (pattern)
                    `(lambda (node parent bol &rest _)
                       (tree-sitter-query-capture node ,pattern))))
    (first-child . (lambda (node parent bol &rest _)
                     (tree-sitter-node-start
                      (tree-sitter-node-child parent 0 t))))

    (parent . (lambda (node parent bol &rest _)
                (tree-sitter-node-start
                 (tree-sitter-node-parent node))))
    (prev-sibling . (lambda (node parent bol &rest _)
                      (tree-sitter-node-start
                       (tree-sitter-node-prev-sibling node))))
    (no-indent . (lambda (node parent bol &rest _) bol))
    (prev-line . (lambda (node parent bol &rest _)
                   (save-excursion
                     (goto-char bol)
                     (forward-line -1)
                     (skip-chars-forward " \t")
                     (tree-sitter-node-start
                      (tree-sitter-node-at (point) nil nil t))))))
  "A list of presets.
These presets that can be used as MATHER and ANCHOR in
`tree-sitter-simple-indent-rules'.

MATCHER:

(match NODE-TYPE PARENT-TYPE NODE-FIELD NODE-INDEX-MIN NODE-INDEX-MAX)

    NODE-TYPE checks for node's type, PARENT-TYPE check for
    parent's type, NODE-FIELD checks for the filed name of node
    in the parent, NODE-INDEX-MIN and NODE-INDEX-MAX checks for
    the node's index in the parent.  Therefore, to match the
    first child where parent is \"argument_list\", use (match nil
    \"argument_list\" nil nil 0 0).

no-node

    Matches the case where node is nil, i.e., there is no node
    that starts at point.  This is the case when indenting an
    empty line.

(node-at-point TYPE NAMED)

    Check that the node at point -- not the largest node at
    point, has type TYPE.  If NAMED non-nil, check the named node
    at point.

(parent-is TYPE)

    Check that the parent has type TYPE.

(node-is TYPE)

    Checks that the node has type TYPE.

(parent-match PATTERN)

    Checks that the parent matches PATTERN, a query pattern.

(node-match PATTERN)

    Checks that the node matches PATTERN, a query pattern.

ANCHOR:

first-child

    Find the first child of the parent.

parent

    Find the parent.

prev-sibling

    Find node's previous sibling.

no-indent

    Do nothing.

prev-line

    Find the named node on previous line.  This can be used when
    indenting an empty line: just indent like the previous node.

TODO: manual?")

(defun tree-sitter--simple-apply (fn args)
  "Apply ARGS to FN.

If FN is a key in `tree-sitter-simple-indent-presets', use the
corresponding value as the function."
  (cond ((consp fn)
         (apply (tree-sitter--simple-apply (car fn) (cdr fn))
                args))
        ((and (symbolp fn)
              (alist-get fn tree-sitter-simple-indent-presets))
         (apply (alist-get fn tree-sitter-simple-indent-presets)
                args))
        ((functionp fn) (apply fn args))
        (t (error "Couldn't find appropriate function for FN"))))

(defun tree-sitter-simple-indent-function ()
  "Indent according to `tree-sitter-simple-indent-rules'."
  (let* ((orig-pos (point))
         (bol (save-excursion
                (beginning-of-line)
                (skip-chars-forward " \t")
                (point)))
         (node (tree-sitter-parent-while
                (cl-loop for parser in tree-sitter-parser-list
                         for node = (tree-sitter-node-at
                                     bol nil parser)
                         if node return node)
                (lambda (node)
                  (eq bol (tree-sitter-node-start node)))))
         (parent (tree-sitter-node-parent node))
         (language (tree-sitter-language-at (point)))
         (rules (alist-get language tree-sitter-simple-indent-rules)))
    (cl-loop for rule in rules
             for pred = (nth 0 rule)
             for anchor = (nth 1 rule)
             for offset = (nth 2 rule)
             if (tree-sitter--simple-apply pred (list node parent bol))
             do (let ((col (+ (save-excursion
                                (goto-char
                                 (tree-sitter--simple-apply
                                  anchor (list node parent bol)))
                                (current-column))
                              offset)))
                  (if (< bol orig-pos)
                      (save-excursion
                        (indent-line-to col))
                    (indent-line-to col))
                  (when tree-sitter--indent-verbose
                    (message "matched %S\nindent to %s"
                             pred col)))
             and return nil)))

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

;; Please compiler.
(defvar ts-c-tree-sitter-indent-rules)
(define-derived-mode ts-c-mode prog-mode "TS C"
  "C mode with tree-sitter support."
  (if (tree-sitter-should-enable-p)
      (setq-local font-lock-tree-sitter-defaults
                  '((ts-c-tree-sitter-settings-1))

                  font-lock-defaults
                  (ignore t nil nil nil)

                  indent-line-function
                  #'tree-sitter-simple-indent-function

                  tree-sitter-simple-indent-rules
                  ts-c-tree-sitter-indent-rules)
    ;; Copied from cc-mode.
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

(defvar ts-c-tree-sitter-indent-rules
  `((tree-sitter-c
     ;; Empty line.
     (no-node prev-line 0)

     ;; Function/struct definition body {}.
     ((match nil "function_definition" "body") parent 0)
     ((node-is "field_declaration_list") parent 0)

     ;; Call expression.
     ((parent-is "call_expression") parent 2)

     ;; If-else.
     ((match nil "if_statement" "condition") parent 2)
     ((match nil "if_statement" "consequence") parent 2)
     ((match nil "if_statement" "alternative") parent 2)
     ((match nil "switch_statement" "condition")  parent 2)
     ((node-is "else") parent 0)

     ;; Switch case.
     ((node-is "case_statement") parent 0)
     ((parent-is "case_statement") parent 2)

     ;; { and }.
     ((node-is "compound_statement") parent 2)
     ((node-is "}") parent 0)

     ;; Multi-line string.
     ((node-at-point "string_literal" t) no-indent 0)

     ;; List.
     ,@(cl-loop for type in '("compound_statement" "initializer_list"
                              "argument_list" "parameter_list"
                              "field_declaration_list")
                collect `((match nil ,type nil 0 0) parent 2)
                collect `((match nil ,type nil 1) first-child 0)))))

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

(defvar-local tree-sitter--inspect-name nil
  "Tree-sitter-inspect-mode uses this to show node name in mode-line.")

(defun tree-sitter-inspect-node-at-point (&optional arg)
  "Show information of the node at point.
If called interactively, show in echo area, otherwise set
`tree-sitter--inspect-name' (which will appear in the mode-line
if `tree-sitter-inspect-mode' is enabled)."
  (interactive "p")
  (let* ((node (tree-sitter-node-at (point)))
         (pos (point))
         (largest-node
          (tree-sitter-parent-while
           node (lambda (node)
                  (eq pos (tree-sitter-node-start node))))))
    (setq tree-sitter--inspect-name
          (format "(%s %s(%s)) (%s)"
                  (or (tree-sitter-node-type
                       (tree-sitter-node-parent largest-node))
                      "N/A")
                  (if (tree-sitter-node-field-name largest-node)
                      (format "%s: "
                              (tree-sitter-node-field-name
                               largest-node))
                    "")
                  (or (tree-sitter-node-type largest-node) "N/A")
                  (or (tree-sitter-node-type node) "N/A")))
    (force-mode-line-update))
  (when arg
    (if tree-sitter--inspect-name
        (message "%s" tree-sitter--inspect-name)
      (message "No node at point"))))

(define-minor-mode tree-sitter-inspect-mode
  "Shows the node at point."
  :lighter nil
  (if tree-sitter-inspect-mode
      (progn
        (add-hook 'post-command-hook
                  #'tree-sitter-inspect-node-at-point 0 t)
        (add-to-list 'mode-line-misc-info
                     '(:eval tree-sitter--inspect-name)))
    (remove-hook 'post-command-hook
                 #'tree-sitter-inspect-node-at-point t)
    (setq mode-line-misc-info
          (remove '(:eval tree-sitter--inspect-name)
                  mode-line-misc-info))))

(provide 'tree-sitter)

;;; tree-sitter.el ends here
