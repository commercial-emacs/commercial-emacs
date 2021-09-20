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
MODE defaults to the value of `major-mode'.  The result depends
on the value of `tree-sitter-disabled-modes' and
`tree-sitter-maximum-size'."
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
      (tree-sitter-parser-create
       (current-buffer) language)))

(defun tree-sitter-parse-string (string language)
  "Parse STRING using a parser for LANGUAGE.
Return the root node of the syntax tree."
  (with-temp-buffer
    (insert string)
    (tree-sitter-parser-root-node
     (tree-sitter-parser-create (current-buffer) language))))

(defun tree-sitter-language-at (point)
  "Return the language used at POINT."
  (cl-loop for parser in tree-sitter-parser-list
           if (tree-sitter-node-at point nil parser)
           return (tree-sitter-parser-language parser)))

(defun tree-sitter-set-ranges (parser-or-lang ranges)
  "Set the ranges of PARSER-OR-LANG to RANGES."
  (tree-sitter-parser-set-included-ranges
   (cond ((symbolp parser-or-lang)
          (tree-sitter-get-parser parser-or-lang))
         ((tree-sitter-parser-p parser-or-lang)
          parser-or-lang))
   ranges))

(defun tree-sitter-get-ranges (parser-or-lang)
  "Get the ranges of PARSER-OR-LANG."
  (tree-sitter-parser-included-ranges
   (cond ((symbolp parser-or-lang)
          (tree-sitter-get-parser parser-or-lang))
         ((tree-sitter-parser-p parser-or-lang)
          parser-or-lang))))

;;; Node API supplement

(defun tree-sitter-node-buffer (node)
  "Return the buffer in where NODE belongs."
  (tree-sitter-parser-buffer
   (tree-sitter-node-parser node)))

(defun tree-sitter-node-language (node)
  "Return the language symbol that NODE's parser uses."
  (tree-sitter-parser-language
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
  (if-let ((parser (if language (tree-sitter-get-parser language)
                     (car tree-sitter-parser-list))))
      (tree-sitter-parser-root-node parser)))

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

(defun tree-sitter-node-text (node)
  "Return the buffer (or string) content corresponding to NODE."
  (with-current-buffer (tree-sitter-node-buffer node)
    (buffer-substring
     (tree-sitter-node-start node)
     (tree-sitter-node-end node))))

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

;;; Query API suuplement

(defun tree-sitter-query-buffer (source pattern &optional beg end)
  "Query the current buffer with PATTERN.

SOURCE can be a language symbol, a parser, or a node.  If a
language symbol, use the root node of the first parser using that
language; if a parser, use the root node of that parser; if a
node, use that node.

PATTERN is either a string containing one or more matching patterns,
or a list containing one or more s-expression matching patterns.  See
Info node `(elisp)Parsing' for how to write a query pattern in either
string or s-expression form.

BEG and END, if _both_ non-nil, specifies the range in which the query
is executed.

Raise an tree-sitter-query-error if PATTERN is malformed.  See
Info node `(elisp)Pattern Matching' for how to read the error message."
  (tree-sitter-query-capture
   (cond ((symbolp source) (tree-sitter-buffer-root-node source))
         ((tree-sitter-parser-p source)
          (tree-sitter-parser-root-node source))
         ((tree-sitter-node-p source) source))
   pattern
   beg end))

(defun tree-sitter-query-string (string pattern language)
  "Query STRING with PATTERN in LANGUAGE.
See `tree-sitter-query-capture' for PATTERN."
  (with-temp-buffer
    (insert string)
    (let ((parser (tree-sitter-parser-create (current-buffer) language)))
      (tree-sitter-query-capture
       (tree-sitter-parser-root-node parser)
       pattern))))

(defun tree-sitter-query-range (source pattern &optional beg end)
  "Query the current buffer and return ranges of captured nodes.

PATTERN, SOURCE, BEG, END are the same as in
`tree-sitter-query-capture'.  This function returns a list
of (START . END), where START and END specifics the range of each
captured node.  Capture names don't matter."
  (cl-loop for capture
           in (tree-sitter-query-buffer source pattern beg end)
           for node = (cdr capture)
           collect (cons (tree-sitter-node-start node)
                         (tree-sitter-node-end node))))

(defun tree-sitter-expand-pattern (pattern-list)
  "Expand PATTERN-LIST to its string form.
Each PATTERN in PATTERN-LIST can be

    :anchor
    :?
    :*
    :+
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"

Consult Info node `(elisp)Pattern Matching' form detailed
explanation."
  (string-join (mapcar
                #'tree-sitter-expand-pattern-1
                pattern-list)
               " "))

(defun tree-sitter-expand-pattern-1 (pattern)
  "Expand PATTERN to its string form.

PATTERN can be

    :anchor
    :?
    :*
    :+
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"

Consult Info node `(elisp)Pattern Matching' form detailed
explanation."
  (pcase pattern
    (:anchor ".")
    (:? "?")
    (:* "*")
    (:+ "+")
    ((pred vectorp) (concat
                     "["
                     (mapconcat #'tree-sitter-expand-pattern-1
                                pattern " ")
                     "]"))
    ((pred listp) (concat
                   "("
                   (mapconcat #'tree-sitter-expand-pattern-1
                              pattern " ")
                   ")"))
    (_ (format "%S" pattern))))

;;; Language API supplement

(defun tree-sitter-display-grammar (language)
  "Show the language grammar definition of LANGUAGE."
  (interactive (list (intern (read-string "Language: "
                                          "tree-sitter-"))))
  (pop-to-buffer
   (with-current-buffer
       (get-buffer-create (format "*Grammar: %s*" language))
     (let ((inhibit-read-only t))
       (erase-buffer)
       (insert (funcall (intern (format "%s-grammar" language))))
       (javascript-mode))
     (goto-char (point-min))
     (current-buffer))))

;;; Range API supplement

(defvar tree-sitter-range-functions nil
  "An alist of (LANGUAGE . FUNCTION) that sets ranges.
font-locking and indenting code uses functions in this alist to
set correct ranges for a language parser before using it.


LANGUAGE is a language symbol, FUNCTION is a function that sets
ranges for the default parser for LANGUAGE.  It's signature
should be

    (start end &rest _)

where START and END marks the region that is about to be used.
FUNCTION only need to (but not limited to) update ranges in that
region.

The default parser is the one returned by

    (tree-sitter-get-parser-create LANGUAGE)")

(defun tree-sitter-update-ranges (&optional start end)
  "Update the ranges for each language in the current buffer.
Calls the range function in `tree-sitter-range-functions' from
the front of the alist.

If only need to update the ranges in a region, pass the START and
END of that region."
  (pcase-dolist (`(,_lang . ,function) tree-sitter-range-functions)
    (funcall function (or start (point-min)) (or end (point-max)))))

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
    (parent-is . (lambda (type)
                   `(lambda (node parent bol &rest _)
                      (equal ,type (tree-sitter-node-type parent)))))

    (node-is . (lambda (type)
                 `(lambda (node parent bol &rest _)
                    (equal ,type (tree-sitter-node-type node)))))

    (query . (lambda (pattern)
               `(lambda (node parent bol &rest _)
                  (cl-loop for capture
                           in (tree-sitter-query-capture
                               parent ,pattern)
                           if (tree-sitter-node-eq node (cdr capture))
                           return t
                           finally return nil))))
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

\(match NODE-TYPE PARENT-TYPE NODE-FIELD NODE-INDEX-MIN NODE-INDEX-MAX)

    NODE-TYPE checks for node's type, PARENT-TYPE check for
    parent's type, NODE-FIELD checks for the filed name of node
    in the parent, NODE-INDEX-MIN and NODE-INDEX-MAX checks for
    the node's index in the parent.  Therefore, to match the
    first child where parent is \"argument_list\", use

        (match nil \"argument_list\" nil nil 0 0).

no-node

    Matches the case where node is nil, i.e., there is no node
    that starts at point.  This is the case when indenting an
    empty line.

\(parent-is TYPE)

    Check that the parent has type TYPE.

\(node-is TYPE)

    Checks that the node has type TYPE.

\(query PATTERN)

    Queries the parent node with PATTERN, and checks if the node
    is captured.

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
  ;; We don't want to match uncompiled lambdas, so make sure this cons
  ;; is not a function.  We could move the condition functionp
  ;; forward, but better be explicit.
  (cond ((and (consp fn) (not (functionp fn)))
         (apply (tree-sitter--simple-apply (car fn) (cdr fn))
                ;; We don't evaluate ARGS with `simple-apply', i.e.,
                ;; no composing, better keep it simple.
                args))
        ((and (symbolp fn)
              (alist-get fn tree-sitter-simple-indent-presets))
         (apply (alist-get fn tree-sitter-simple-indent-presets)
                args))
        ((functionp fn) (apply fn args))
        (t (error "Couldn't find appropriate function for FN"))))

(defvar tree-sitter-indent-function #'tree-sitter-simple-indent
  "Function used by `tree-sitter-indent' to do some of the work.

This function is called with

    (NODE PARENT BOL &rest _)

and returns

    (ANCHOR . OFFSET).

BOL is the position of the beginning of the line; NODE is the
\"largest\" node that starts at BOL; PARENT is its parent; ANCHOR
is a node, and OFFSET is a number.  Emacs finds the column of
ANCHOR's start and adds OFFSET to it, as the final indentation of
the current line.")

(defun tree-sitter-indent ()
  "Indent according to `tree-sitter-simple-indent-rules'."
  (tree-sitter-update-ranges)
  (pcase-let*
      ((orig-pos (point))
       (bol (save-excursion
              (forward-line 0)
              (skip-chars-forward " \t")
              (point)))
       (node (tree-sitter-parent-while
              (cl-loop for parser in tree-sitter-parser-list
                       for node = (tree-sitter-node-at
                                   bol nil parser)
                       if node return node)
              (lambda (node)
                (eq bol (tree-sitter-node-start node)))))
       (parser (tree-sitter-node-parser node))
       ;; NODE would be nil if BOL is on a whitespace.  In that case
       ;; we set PARENT to the "node at point", which would encompass
       ;; the whitespace.
       (parent (if (null node)
                   (tree-sitter-node-at bol nil parser)
                 (tree-sitter-node-parent node)))
       (`(,anchor . ,offset)
        (funcall tree-sitter-indent-function node parent bol)))
    (if (null anchor)
        (when tree-sitter--indent-verbose
          (message "Failed to find the anchor"))
      (let ((col (+ (save-excursion
                      (goto-char anchor)
                      (current-column))
                    offset)))
        (if (< bol orig-pos)
            (save-excursion
              (indent-line-to col))
          (indent-line-to col))))))

(defun tree-sitter-simple-indent (node parent bol)
  "Indent according to `tree-sitter-simple-indent-rules'.

For NODE, PARENT and BOL see `tree-sitter-indent'."
  (let* ((language (tree-sitter-node-language node))
         (rules (alist-get language tree-sitter-simple-indent-rules)))
    (cl-loop for rule in rules
             for pred = (nth 0 rule)
             for anchor = (nth 1 rule)
             for offset = (nth 2 rule)
             if (tree-sitter--simple-apply pred (list node parent bol))
             do (when tree-sitter--indent-verbose
                  (message "Matched rule: %S" rule))
             and
             return (cons (tree-sitter--simple-apply
                           anchor (list node parent bol))
                          offset))))

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
  "Fortify a #include <lib>.
Fortify the angled brackets in preprocessor-face,
and the lib name in string-face."
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
                  #'tree-sitter-indent

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
     ((parent-is "string_literal") no-indent 0)

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


;;; Debugging

(defvar-local tree-sitter--inspect-name nil
  "Tree-sitter-inspect-mode uses this to show node name in mode-line.")

(defun tree-sitter-inspect-node-at-point (&optional arg)
  "Show information of the node at point.
If called interactively, show in echo area, otherwise set
`tree-sitter--inspect-name' (which will appear in the mode-line
if `tree-sitter-inspect-mode' is enabled)."
  (interactive "p")
  ;; NODE-LIST contains all the node that starts at point.
  (let* ((node-list
          (cl-loop for node = (tree-sitter-node-at (point))
                   then (tree-sitter-node-parent node)
                   while node
                   if (eq (tree-sitter-node-start node)
                          (point))
                   collect node))
         (largest-node (car (last node-list)))
         (parent (tree-sitter-node-parent largest-node))
         ;; node-list-acending contains all the node bottom-up, then
         ;; the parent.
         (node-list-acending
          (if (null largest-node)
              ;; If there are no nodes that start at point, just show
              ;; the node at point and its parent.
              (list (tree-sitter-node-at (point))
                    (tree-sitter-node-parent
                     (tree-sitter-node-at (point))))
            (append node-list (list parent))))
         (name ""))
    ;; We draw nodes like (parent field-name: (node)) recursively,
    ;; so it could be (node1 field-name: (node2 field-name: (node3))).
    (dolist (node node-list-acending)
      (setq
       name
       (concat
        "("
        (or (tree-sitter-node-type node)
            "N/A")
        (if (tree-sitter-node-field-name node)
            (format " %s: " (tree-sitter-node-field-name node))
          " ")
        name
        ")")))
    (setq tree-sitter--inspect-name name)
    (force-mode-line-update)
    (when arg
      (if node-list
          (message "%s" tree-sitter--inspect-name)
        (message "No node at point")))))

(define-minor-mode tree-sitter-inspect-mode
  "Shows the node that _starts_ at point in the mode-line.

The mode-line displays some thing like:

    (parent field-name: (child (grand-child (...))))

CHILD, GRAND-CHILD, and GRAND-GRAND-CHILD, etc, are nodes that
have their beginning at point.  And parent is the parent of the
largest among them, CHILD.

If no node starts at point, i.e., point is in the middle of a
node, then we just display the node at point and its parent."
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
