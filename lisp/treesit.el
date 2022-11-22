;;; treesit.el --- tree-sitter utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

;; This file is the Lisp counterpart of treesit.c.  Together they
;; provide tree-sitter integration for Emacs.  This file contains
;; convenient functions that are more idiomatic and flexible than the
;; exposed C API of tree-sitter.  It also contains frameworks for
;; integrating tree-sitter with font-lock, indentation, activating and
;; deactivating tree-sitter, debugging tree-sitter, etc.

;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x)) ; For `string-join'.
(require 'cl-seq)
(require 'font-lock)

;;; Function declarations

(declare-function treesit-language-available-p "treesit.c")
(declare-function treesit-language-version "treesit.c")

(declare-function treesit-parser-p "treesit.c")
(declare-function treesit-node-p "treesit.c")
(declare-function treesit-compiled-query-p "treesit.c")
(declare-function treesit-query-p "treesit.c")
(declare-function treesit-query-language "treesit.c")

(declare-function treesit-node-parser "treesit.c")

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-parser-delete "treesit.c")
(declare-function treesit-parser-list "treesit.c")
(declare-function treesit-parser-buffer "treesit.c")
(declare-function treesit-parser-language "treesit.c")

(declare-function treesit-parser-root-node "treesit.c")

(declare-function treesit-parser-set-included-ranges "treesit.c")
(declare-function treesit-parser-included-ranges "treesit.c")
(declare-function treesit-parser-add-notifier "treesit.c")

(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-string "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-check "treesit.c")
(declare-function treesit-node-field-name-for-child "treesit.c")
(declare-function treesit-node-child-count "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-first-child-for-pos "treesit.c")
(declare-function treesit-node-descendant-for-range "treesit.c")
(declare-function treesit-node-eq "treesit.c")

(declare-function treesit-pattern-expand "treesit.c")
(declare-function treesit-query-expand "treesit.c")
(declare-function treesit-query-compile "treesit.c")
(declare-function treesit-query-capture "treesit.c")

(declare-function treesit-search-subtree "treesit.c")
(declare-function treesit-search-forward "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")

(declare-function treesit-available-p "treesit.c")

;;; Custom options

;; Tree-sitter always appear as treesit in symbols.
(defgroup treesit nil
  "Incremental parser.
It is used to enhance major mode features like font-lock,
indent, imenu, etc."
  :group 'tools
  :version "29.1")

(defcustom treesit-max-buffer-size (* 4 1024 1024)
  "Maximum buffer size for enabling tree-sitter parsing (in bytes)."
  :type 'integer
  :version "29.1")

;;; Parser API supplement

(defun treesit-parse-string (string language)
  "Parse STRING using a parser for LANGUAGE.
Return the root node of the syntax tree."
  (with-temp-buffer
    (insert string)
    (treesit-parser-root-node
     (treesit-parser-create language))))

(defvar-local treesit-language-at-point-function nil
  "A function that returns the language at point.
This is used by `treesit-language-at', which is used by various
functions to determine which parser to use at point.

The function is called with one argument, the position of point.")

(defun treesit-language-at (position)
  "Return the language at POSITION.
This function assumes that parser ranges are up-to-date.  It
returns the return value of `treesit-language-at-point-function'
if it's non-nil, otherwise it returns the language of the first
parser in `treesit-parser-list', or nil if there is no parser."
  (if treesit-language-at-point-function
      (funcall treesit-language-at-point-function position)
    (when-let ((parser (car (treesit-parser-list))))
      (treesit-parser-language parser))))

;;; Node API supplement

(defun treesit-node-buffer (node)
  "Return the buffer in which NODE belongs."
  (treesit-parser-buffer
   (treesit-node-parser node)))

(defun treesit-node-language (node)
  "Return the language symbol that NODE's parser uses."
  (treesit-parser-language
   (treesit-node-parser node)))

(defun treesit-node-at (pos &optional parser-or-lang named)
  "Return the leaf node at position POS.

A leaf node is a node that doesn't have any child nodes.

The returned node's span covers POS: the node's beginning is before
or at POS, and the node's end is at or after POS.

If no leaf node's span covers POS (e.g., POS is on whitespace
between two leaf nodes), return the first leaf node after POS.

If there is no leaf node after POS, return the first leaf node
before POS.

Return nil if no leaf node can be returned.  If NAMED is non-nil,
only look for named nodes.

If PARSER-OR-LANG is nil, use the first parser in
`treesit-parser-list'; if PARSER-OR-LANG is a parser, use
that parser; if PARSER-OR-LANG is a language, find a parser using
that language in the current buffer, and use that."
  (let* ((root (if (treesit-parser-p parser-or-lang)
                   (treesit-parser-root-node parser-or-lang)
                 (treesit-buffer-root-node parser-or-lang)))
         (node root)
         (node-before root)
         (pos-1 (max (1- pos) (point-min)))
         next)
    (when node
      ;; This is very fast so no need for C implementation.
      (while (setq next (treesit-node-first-child-for-pos
                         node pos named))
        (setq node next))
      ;; If POS is at the end of buffer, after all the text, we will
      ;; end up with NODE = root node.  Instead of returning nil,
      ;; return the last leaf node in the tree for convenience.
      (if (treesit-node-eq node root)
          (progn
            (while (setq next (treesit-node-child node -1 named))
              (setq node next))
            node)
        ;; Normal case, where we found a node.
        (if (<= (treesit-node-start node) pos)
            node
          ;; So the node we found is completely after POS, try to find
          ;; a node whose end equals to POS.
          (while (setq next (treesit-node-first-child-for-pos
                             node-before pos-1 named))
            (setq node-before next))
          (if (eq (treesit-node-end node-before) pos)
              node-before
            node))))))

(defun treesit-node-on (beg end &optional parser-or-lang named)
  "Return the smallest node covering BEG to END.

BEWARE!  Calling this function on an empty line that is not
inside any top-level construct (function definition, etc.) most
probably will give you the root node, because the root node is
the smallest node that covers that empty line.  You probably want
to use `treesit-node-at' instead.

Return nil if none was found.  If NAMED is non-nil, only look for
named node.

If PARSER-OR-LANG is nil, use the first parser in
`treesit-parser-list'; if PARSER-OR-LANG is a parser, use
that parser; if PARSER-OR-LANG is a language, find a parser using
that language in the current buffer, and use that."
  (let ((root (if (treesit-parser-p parser-or-lang)
                  (treesit-parser-root-node parser-or-lang)
                (treesit-buffer-root-node parser-or-lang))))
    (treesit-node-descendant-for-range root beg (or end beg) named)))

(defun treesit-node-top-level (node &optional type)
  "Return the top-level equivalent of NODE.
Specifically, return the highest parent of NODE that has the same
type as it.  If no such parent exists, return nil.

If TYPE is non-nil, match each parent's type with TYPE as a
regexp, rather than using NODE's type."
  (let ((type (or type (treesit-node-type node)))
        (result nil))
    (cl-loop for cursor = (treesit-node-parent node)
             then (treesit-node-parent cursor)
             while cursor
             if (string-match-p type (treesit-node-type cursor))
             do (setq result cursor))
    result))

(defun treesit-buffer-root-node (&optional language)
  "Return the root node of the current buffer.
Use the first parser in `treesit-parser-list'.

If optional argument LANGUAGE is non-nil, use the first parser
for LANGUAGE."
  (if-let ((parser
            (or (if language
                    (treesit-parser-create language)
                  (or (car (treesit-parser-list))
                      (signal 'treesit-error
                              '("Buffer has no parser")))))))
      (treesit-parser-root-node parser)))

(defun treesit-filter-child (node pred &optional named)
  "Return children of NODE that satisfies predicate PRED.
PRED is a function that takes one argument, the child node.
If optional argument NAMED is non-nil, only search for named
node."
  (let ((child (treesit-node-child node 0 named))
        result)
    (while child
      (when (funcall pred child)
        (push child result))
      (setq child (treesit-node-next-sibling child named)))
    (reverse result)))

(defun treesit-node-text (node &optional no-property)
  "Return the buffer (or string) content corresponding to NODE.
If optional argument NO-PROPERTY is non-nil, remove text
properties."
  (when node
    (with-current-buffer (treesit-node-buffer node)
      (if no-property
          (buffer-substring-no-properties
           (treesit-node-start node)
           (treesit-node-end node))
        (buffer-substring
         (treesit-node-start node)
         (treesit-node-end node))))))

(defun treesit-parent-until (node pred)
  "Return the closest parent of NODE that satisfies PRED.
Return nil if none was found.  PRED should be a function that
takes one argument, the parent node."
  (let ((node (treesit-node-parent node)))
    (while (and node (not (funcall pred node)))
      (setq node (treesit-node-parent node)))
    node))

(defun treesit-parent-while (node pred)
  "Return the furthest parent of NODE that satisfies PRED.
Return nil if none was found.  PRED should be a function that
takes one argument, the parent node."
  (let ((last nil))
    (while (and node (funcall pred node))
      (setq last node
            node (treesit-node-parent node)))
    last))

(defalias 'treesit-traverse-parent #'treesit-parent-until)

(defun treesit-node-children (node &optional named)
  "Return a list of NODE's children.
If NAMED is non-nil, collect named child only."
  (mapcar (lambda (idx)
            (treesit-node-child node idx named))
          (number-sequence
           0 (1- (treesit-node-child-count node named)))))

(defun treesit-node-index (node &optional named)
  "Return the index of NODE in its parent.
If NAMED is non-nil, count named child only."
  (let ((count 0))
    (while (setq node (treesit-node-prev-sibling node named))
      (cl-incf count))
    count))

(defun treesit-node-field-name (node)
  "Return the field name of NODE as a child of its parent."
  (when-let ((parent (treesit-node-parent node))
             (idx (treesit-node-index node)))
    (treesit-node-field-name-for-child parent idx)))

;;; Query API supplement

(defun treesit-query-string (string query language)
  "Query STRING with QUERY in LANGUAGE.
See `treesit-query-capture' for QUERY."
  (with-temp-buffer
    (insert string)
    (let ((parser (treesit-parser-create language)))
      (treesit-query-capture
       (treesit-parser-root-node parser)
       query))))

(defun treesit-query-range (node query &optional beg end)
  "Query the current buffer and return ranges of captured nodes.

QUERY, NODE, BEG, END are the same as in
`treesit-query-capture'.  This function returns a list
of (START . END), where START and END specifics the range of each
captured node.  Capture names don't matter."
  (cl-loop for capture
           in (treesit-query-capture node query beg end)
           for node = (cdr capture)
           collect (cons (treesit-node-start node)
                         (treesit-node-end node))))

;;; Range API supplement

(defvar-local treesit-range-settings nil
  "A list of range settings.

Each element of the list is of the form (QUERY LANGUAGE).
When updating the range of each parser in the buffer,
`treesit-update-ranges' queries each QUERY, and sets LANGUAGE's
range to the range spanned by captured nodes.  QUERY must be a
compiled query.

QUERY can also be a function, in which case it is called with 2
arguments, START and END.  It should ensure parsers' ranges are
correct in the region between START and END.

The exact form of each setting is considered internal and subject
to change.  Use `treesit-range-rules' to set this variable.")

(defun treesit-range-rules (&rest query-specs)
  "Produce settings for `treesit-range-settings'.

QUERY-SPECS are a series of QUERY-SPECs, where each QUERY-SPEC is
a QUERY preceeded by zero or more pairs of :KEYWORD and VALUE,
like this:

    :KEYWORD VALUE... QUERY

Each QUERY is a tree-sitter query in either the string,
s-expression or compiled form.

For each QUERY, :KEYWORD and VALUE pairs add meta information to
it.  For example,

    (treesit-range-rules
     :embed \\='javascript
     :host \\='html
     \\='((script_element (raw_text) @cap)))

The `:embed' keyword specifies the embedded language, and the
`:host' keyword specifies the host language.  They are used in
this way: Emacs queries QUERY in the host language's parser,
computes the ranges spanned by the captured nodes, and applies
these ranges to parsers for the embedded language.

QUERY can also be a function that takes two arguments, START and
END.  If QUERY is a function, it doesn't need the :KEYWORD VALUE
pair preceding it.  This function should set the ranges for
parsers in the current buffer in the region between START and
END.  It is OK for this function to set ranges in a larger region
that encompasses the region between START and END."
  (let (host embed result)
    (while query-specs
      (pcase (pop query-specs)
        (:host (let ((host-lang (pop query-specs)))
                 (unless (symbolp host-lang)
                   (signal 'treesit-error (list "Value of :host option should be a symbol" host-lang)))
                 (setq host host-lang)))
        (:embed (let ((embed-lang (pop query-specs)))
                  (unless (symbolp embed-lang)
                    (signal 'treesit-error (list "Value of :embed option should be a symbol" embed-lang)))
                  (setq embed embed-lang)))
        (query (if (functionp query)
                   (push (list query nil nil) result)
                 (when (null embed)
                   (signal 'treesit-error (list "Value of :embed option cannot be omitted")))
                 (when (null host)
                   (signal 'treesit-error (list "Value of :host option cannot be omitted")))
                 (push (list (treesit-query-compile host query)
                             embed host)
                       result))
               (setq host nil embed nil))))
    (nreverse result)))

(defun treesit--merge-ranges (old-ranges new-ranges start end)
  "Merge OLD-RANGES and NEW-RANGES, discarding ranges between START and END.
OLD-RANGES and NEW-RANGES are lists of cons of the form (BEG . END).  When
merging the two ranges, if a range in OLD-RANGES intersects with
another range in NEW-RANGES, discard the one in OLD-RANGES and
keep the one in NEW-RANGES.  Also discard any range in OLD-RANGES
that intersects the region marked by START and END.

Return the merged list of ranges."
  (let ((result nil))
    (while (and old-ranges new-ranges)
      (let ((new-beg (caar new-ranges))
            (new-end (cdar new-ranges))
            (old-beg (caar old-ranges))
            (old-end (cdar old-ranges)))
        (cond
         ;; Old range intersects with START-END, discard.
         ((and (< start old-end)
               (< old-beg end))
          (setq old-ranges (cdr old-ranges)))
         ;; New range and old range don't intersect, new comes
         ;; before, push new.
         ((<= new-end old-beg)
          (push (car new-ranges) result)
          (setq new-ranges (cdr new-ranges)))
         ;; New range and old range don't intersect, old comes
         ;; before, push old.
         ((<= old-end new-beg)
          (push (car old-ranges) result)
          (setq old-ranges (cdr old-ranges)))
         (t ;; New and old range intersect, discard old.
          (setq old-ranges (cdr old-ranges))))))
    (let ((left-over (or new-ranges old-ranges)))
      (dolist (range left-over)
        (push range result)))
    (nreverse result)))

(defun treesit-update-ranges (&optional beg end)
  "Update the ranges for each language in the current buffer.
If BEG and END are non-nil, only update parser ranges in that
region."
  ;; When updating ranges, we want to avoid querying the whole buffer
  ;; which could be slow in very large buffers.  Instead, we only
  ;; query for nodes that intersect with the region between BEG and
  ;; END.  Also, we only update the ranges intersecting BEG and END;
  ;; outside of that region we inherit old ranges.
  (dolist (setting treesit-range-settings)
    (let ((query (nth 0 setting))
          (language (nth 1 setting))
          (beg (or beg (point-min)))
          (end (or end (point-max))))
      (if (functionp query) (funcall query beg end)
        (let* ((host-lang (treesit-query-language query))
               (parser (treesit-parser-create language))
               (old-ranges (treesit-parser-included-ranges parser))
               (new-ranges (treesit-query-range
                            host-lang query beg end))
               (set-ranges (treesit--merge-ranges
                            old-ranges new-ranges beg end)))
          (dolist (parser (treesit-parser-list))
            (when (eq (treesit-parser-language parser)
                      language)
              (treesit-parser-set-included-ranges
               parser set-ranges))))))))

(defun treesit-parser-range-on (parser beg &optional end)
  "Check if PARSER's range covers the portion between BEG and END.

If it does, return the range covering that portion in the form
of (RANGE-BEG . RANGE-END), if not, return nil.  If nil or
omitted, default END to BEG."
  (let ((ranges (treesit-parser-included-ranges parser))
        (end (or end beg)))
    (if (null ranges)
        (cons (point-min) (point-max))
      (cl-loop for rng in ranges
               if (<= (car rng) beg end (cdr rng))
               return rng
               finally return nil))))

;;; Fontification

(define-error 'treesit-font-lock-error
              "Generic tree-sitter font-lock error"
              'treesit-error)

(defvar-local treesit--font-lock-query-expand-range (cons 0 0)
  "The amount to expand the start and end of the region when fontifying.
This should be a cons cell (START . END).  When fontifying a
buffer, Emacs will move the start of the query range backward by
START amount, and the end of the query range by END amount.  Both
START and END should be positive integers or 0.  This doesn't
affect the fontified range.

Sometimes, querying on some parser with a restricted range
returns nodes not in that range but before it, which breaks
fontification.  Major modes can adjust this variable as a
temporarily fix.")

(defvar-local treesit-font-lock-feature-list nil
  "A list of lists of feature symbols.

Use `treesit-font-lock-recompute-features' and
`font-lock-maximum-decoration' to configure enabled features.

Each sublist represents a decoration level.
`font-lock-maximum-decoration' controls which levels are
activated.

Inside each sublist are feature symbols, which corresponds to the
:feature value of a query defined in `treesit-font-lock-rules'.
Removing a feature symbol from this list disables the
corresponding query during font-lock.

Common feature names (for general programming languages) include
definition, type, assignment, builtin, constant, keyword,
string-interpolation, comment, doc, string, operator, property,
preprocessor, escape-sequence, key (in key-value pairs).  Major
modes are free to subdivide or extend on these common features.
See the manual for more explanations on some of the feature.

For changes to this variable to take effect, run
`treesit-font-lock-recompute-features'.")

(defvar-local treesit-font-lock-settings nil
  "A list of SETTINGs for treesit-based fontification.

The exact format of each SETTING is considered internal.  Use
`treesit-font-lock-rules' to set this variable.

Each SETTING has the form:

    (QUERY ENABLE FEATURE OVERRIDE)

QUERY must be a compiled query.  See Info node `(elisp)Pattern
Matching' for how to write a query and compile it.

For SETTING to be activated for font-lock, ENABLE must be t.  To
disable this SETTING, set ENABLE to nil.

FEATURE is the \"feature name\" of the query.  Users can control
which features are enabled with `font-lock-maximum-decoration'
and `treesit-font-lock-feature-list'.

OVERRIDE is the override flag for this query.  Its value can be
t, nil, append, prepend, keep.  See more in
`treesit-font-lock-rules'.")

(defun treesit-font-lock-rules (&rest query-specs)
  "Return a value suitable for `treesit-font-lock-settings'.

QUERY-SPECS is a series of QUERY-SPECs.  Each QUERY-SPEC is a
QUERY preceded by multiple pairs of :KEYWORD and VALUE:

   :KEYWORD VALUE... QUERY

QUERY is a tree-sitter query in either the string, s-expression
or compiled form.  For each query, captured nodes are highlighted
with the capture name as its face.

:KEYWORD and VALUE pairs preceeding a QUERY add meta information
to QUERY.  For example,

    (treesit-font-lock-rules
     :language \\='javascript
     :override t
     :feature\\='constant
     \\='((true) @font-lock-constant-face
       (false) @font-lock-constant-face)
     :language \\='html
     :feature \\='script
     \"(script_element) @font-lock-builtin-face\")

For each QUERY, a :language keyword and a :feature keyword is
required.  Each query's :feature is a symbol summarizing what the
query fontifies.  It is used to allow users to enable/disable
certain features.  See `treesit-font-lock-kind-list' for more.
Other keywords include:

  KEYWORD    VALUE      DESCRIPTION
  :override  nil        If the region already has a face,
                        discard the new face.
             t          Always apply the new face.
             `append'   Append the new face to existing ones.
             `prepend'  Prepend the new face to existing ones.
             `keep'     Fill-in regions without an existing face.

Capture names in QUERY should be face names like
`font-lock-keyword-face'.  The captured node will be fontified
with that face.

Capture names can also be function names, in which case the
function will be called with the following argument list:

    (NODE OVERRIDE START END &rest _)

where NODE is the tree-sitter node object, OVERRIDE is the
override option of that rule, and START and END specify the region
to be fontified.  This function should accept more arguments as
optional arguments for future extensibility, and it shouldn't
fontify text outside the region given by START and END.

If a capture name is both a face and a function, the face takes
priority.  If a capture name is not a face name nor a function
name, it is ignored."
  ;; Other tree-sitter function don't tend to be called unless
  ;; tree-sitter is enabled, which means tree-sitter must be compiled.
  ;; But this function is usually call in `defvar' which runs
  ;; regardless whether tree-sitter is enabled.  So we need this
  ;; guard.
  (when (treesit-available-p)
    (let (;; Tracks the current :language/:override/:toggle/:level value
          ;; that following queries will apply to.
          current-language current-override
          current-feature
          ;; The list this function returns.
          (result nil))
      (while query-specs
        (let ((token (pop query-specs)))
          (pcase token
            ;; (1) Process keywords.
            (:language
             (let ((lang (pop query-specs)))
               (when (or (not (symbolp lang)) (null lang))
                 (signal 'treesit-font-lock-error
                         `("Value of :language should be a symbol"
                           ,lang)))
               (setq current-language lang)))
            (:override
             (let ((flag (pop query-specs)))
               (when (not (memq flag '(t nil append prepend keep)))
                 (signal 'treesit-font-lock-error
                         `("Value of :override should be one of t, nil, append, prepend, keep"
                           ,flag))
                 (signal 'wrong-type-argument
                         `((or t nil append prepend keep)
                           ,flag)))
               (setq current-override flag)))
            (:feature
             (let ((var (pop query-specs)))
               (when (or (not (symbolp var))
                         (memq var '(t nil)))
                 (signal 'treesit-font-lock-error
                         `("Value of :feature should be a symbol"
                           ,var)))
               (setq current-feature var)))
            ;; (2) Process query.
            ((pred treesit-query-p)
             (when (null current-language)
               (signal 'treesit-font-lock-error
                       `("Language unspecified, use :language keyword to specify a language for this query" ,token)))
             (when (null current-feature)
               (signal 'treesit-font-lock-error
                       `("Feature unspecified, use :feature keyword to specify the feature name for this query" ,token)))
             (if (treesit-compiled-query-p token)
                 (push `(,current-language token) result)
               (push `(,(treesit-query-compile current-language token)
                       t
                       ,current-feature
                       ,current-override)
                     result))
             ;; Clears any configurations set for this query.
             (setq current-language nil
                   current-override nil
                   current-feature nil))
            (_ (signal 'treesit-font-lock-error
                       `("Unexpected value" ,token))))))
      (nreverse result))))

;; `font-lock-fontify-region-function' has the LOUDLY argument, but
;; `jit-lock-functions' doesn't pass that argument.  So even if we set
;; `font-lock-verbose' to t, if jit-lock is enabled (and it's almost
;; always is), we don't get debug messages.  So we add our own.
(defvar treesit--font-lock-verbose nil
  "If non-nil, print debug messages when fontifying.")

(defun treesit-font-lock-recompute-features (&optional add-list remove-list)
  "Enable/disable font-lock settings according to decoration level.

First compute the enabled features according to
`treesit-font-lock-feature-list' and `font-lock-maximum-decoration',
then, if ADD-LIST or REMOVE-LIST are not omitted, further add and
remove features accordingly.

ADD-LIST and REMOVE-LIST are each a list of feature symbols.  The
same feature symbol cannot appear in both lists; the function
signals the `treesit-font-lock-error' error if so."
  (when-let ((intersection (cl-intersection add-list remove-list)))
    (signal 'treesit-font-lock-error
            (list "ADD-LIST and REMOVE-LIST contain the same feature"
                  intersection)))
  (let* ((level (font-lock-value-in-major-mode
                 font-lock-maximum-decoration))
         (base-features (cl-loop
                         for idx = 0 then (1+ idx)
                         for features in treesit-font-lock-feature-list
                         if (or (eq level t)
                                (>= level (1+ idx)))
                         append features))
         (features (cl-set-difference (cl-union base-features add-list)
                                      remove-list)))
    (cl-loop for idx = 0 then (1+ idx)
             for setting in treesit-font-lock-settings
             for feature = (nth 2 setting)
             ;; Set the ENABLE flag for the setting.
             do (setf (nth 1 (nth idx treesit-font-lock-settings))
                      (if (memq feature features) t nil)))))

(defun treesit-fontify-with-override (start end face override)
  "Apply FACE to the region between START and END.
OVERRIDE can be nil, t, `append', `prepend', or `keep'.
See `treesit-font-lock-rules' for their semantic."
  (pcase override
    ('nil (unless (text-property-not-all
                   start end 'face nil)
            (put-text-property start end 'face face)))
    ('t (put-text-property start end 'face face))
    ('append (font-lock-append-text-property
              start end 'face face))
    ('prepend (font-lock-prepend-text-property
               start end 'face face))
    ('keep (font-lock-fillin-text-property
            start end 'face face))
    (_ (signal 'treesit-font-lock-error
               (list
                "Unrecognized value of :override option"
                override)))))

(defun treesit--set-nonsticky (start end sym &optional remove)
  "Set `rear-nonsticky' property between START and END.
Set the proeprty to a list containing SYM.  If there is already a
list, add SYM to that list.  If REMOVE is non-nil, remove SYM
instead."
  (let* ((prop (get-text-property start 'rear-nonsticky))
         (new-prop
          (pcase prop
            ((pred listp) ; PROP is a list or nil.
             (if remove
                 (remove sym prop)
               ;; We should make sure PORP doesn't contain SYM, but
               ;; whatever.
               (cons sym prop)))
            ;; PROP is t.
            (_ (if remove
                   nil
                 (list sym))))))
    (if (null new-prop)
        (remove-text-properties start end '(rear-nonsticky nil))
      (put-text-property start end 'rear-nonsticky new-prop))))

(defun treesit--children-covering-range (node start end)
  "Return a list of children of NODE covering a range.
The range is between START and END."
  (let* ((child (treesit-node-first-child-for-pos node start))
         (result (list child)))
    (while (and (< (treesit-node-end child) end)
                (setq child (treesit-node-next-sibling child)))
      (push child result))
    (nreverse result)))

(defun treesit--children-covering-range-recurse (node start end threshold)
  "Return a list of children of NODE covering a range.
Recursively go down the parse tree and collect children, until
all nodes in the returned list are smaller than THRESHOLD.  The
range is between START and END."
  (let* ((child (treesit-node-first-child-for-pos node start))
         result)
    (while (and child (<= (treesit-node-start child) end))
      ;; If child still too large, recurse down.  Otherwise collect
      ;; child.
      (if (> (- (treesit-node-end child)
                (treesit-node-start child))
             threshold)
          (dolist (r (treesit--children-covering-range-recurse
                      child start end threshold))
            (push r result))
        (push child result))
      (setq child (treesit-node-next-sibling child)))
    ;; If NODE has no child, keep NODE.
    (or result node)))

(defsubst treesit--node-length (node)
  "Return the length of the text of NODE."
  (- (treesit-node-end node) (treesit-node-start node)))

(defvar-local treesit--font-lock-fast-mode nil
  "If this variable is t, change the way we query so its faster.
This is not a general optimization and should be RARELY needed!
See comments in `treesit-font-lock-fontify-region' for more
detail.")

;; Some details worth explaining:
;;
;; 1. When we apply face to a node, we clip the face into the
;; currently fontifying region, this way we don't overwrite faces
;; applied by regexp-based font-lock.  The clipped part will be
;; fontified fine when Emacs fontifies the region containing it.
;;
(defun treesit-font-lock-fontify-region
    (start end &optional loudly)
  "Fontify the region between START and END.
If LOUDLY is non-nil, display some debugging information."
  (when (or loudly treesit--font-lock-verbose)
    (message "Fontifying region: %s-%s" start end))
  (treesit-update-ranges start end)
  (font-lock-unfontify-region start end)
  (dolist (setting treesit-font-lock-settings)
    (let* ((query (nth 0 setting))
           (enable (nth 1 setting))
           (override (nth 3 setting))
           (language (treesit-query-language query)))
      ;; If you insert an ending quote into a buffer, jit-lock only
      ;; wants to fontify that single quote, and (treesit-node-on
      ;; start end) will give you that quote node.  We want to capture
      ;; the string and apply string face to it, but querying on the
      ;; quote node will not give us the string node.  So we don't use
      ;; treesit-node-on: using the root node with a restricted range
      ;; is very fast anyway (even in large files of size ~10MB).
      ;; Plus, querying the result of `treesit-node-on' could still
      ;; miss patterns even if we use some heuristic to enlarge the
      ;; node (how much to enlarge? to which extent?), its much safer
      ;; to just use the root node.
      ;;
      ;; Sometimes the source file has some errors that causes
      ;; tree-sitter to parse it into a enormously tall tree (10k
      ;; levels tall).  In that case querying the root node is very
      ;; slow.  So we try to get top-level nodes and query them.  This
      ;; ensures that querying is fast everywhere else, except for the
      ;; problematic region.
      (when-let ((nodes (list (treesit-buffer-root-node language)))
                 ;; Only activate if ENABLE flag is t.
                 (activate (eq t enable)))
        (ignore activate)

        ;; If we run into problematic files, use the "fast mode" to
        ;; try to recover.
        (when treesit--font-lock-fast-mode
          (setq nodes (treesit--children-covering-range
                       (car nodes) start end)))

        (dolist (sub-node nodes)
          (let* ((delta-start (car treesit--font-lock-query-expand-range))
                 (delta-end (cdr treesit--font-lock-query-expand-range))
                 (start-time (current-time))
                 (captures (treesit-query-capture
                            sub-node query
                            (max (- start delta-start) (point-min))
                            (min (+ end delta-end) (point-max))))
                 (end-time (current-time))
                 (inhibit-point-motion-hooks t))
            ;; If for any query the query time is strangely long,
            ;; switch to fast mode (see comments above).
            (when (> (time-to-seconds (time-subtract end-time start-time))
                     0.01)
              (setq-local treesit--font-lock-fast-mode t))
            (with-silent-modifications
              (dolist (capture captures)
                (let* ((face (car capture))
                       (node (cdr capture))
                       (node-start (treesit-node-start node))
                       (node-end (treesit-node-end node)))
                  ;; Turns out it is possible to capture a node that's
                  ;; completely outside the region between START and
                  ;; END.  If the node is outside of that region, (max
                  ;; node-start start) and friends return bad values.
                  (when (and (< start node-end)
                             (< node-start end))
                    (cond
                     ((facep face)
                      (treesit-fontify-with-override
                       (max node-start start) (min node-end end)
                       face override))
                     ((functionp face)
                      (funcall face node override start end)))
                    ;; Don't raise an error if FACE is neither a face nor
                    ;; a function.  This is to allow intermediate capture
                    ;; names used for #match and #eq.
                    (when (or loudly treesit--font-lock-verbose)
                      (message "Fontifying text from %d to %d, Face: %s, Node: %s"
                               (max node-start start) (min node-end end)
                               face (treesit-node-type node))))))))))))
  `(jit-lock-bounds ,start . ,end))

(defun treesit--font-lock-notifier (ranges parser)
  "Ensures updated parts of the parse-tree is refontified.
RANGES is a list of (BEG . END) ranges, PARSER is the tree-sitter
parser notifying of the change."
  (with-current-buffer (treesit-parser-buffer parser)
    (dolist (range ranges)
      (when treesit--font-lock-verbose
        (message "Notifier recieved range: %s-%s"
                 (car range) (cdr range)))
      (put-text-property (car range) (cdr range) 'fontified nil))))

;;; Indent

;; `comment-start' and `comment-end' assume there is only one type of
;; comment, and that the comment spans only one line.  So they are not
;; sufficient for our purpose.

(defvar-local treesit-comment-start nil
  "Regular expression matching an opening comment token.")

(defvar-local treesit-comment-end nil
  "Regular expression matching a closing comment token.")

(define-error 'treesit-indent-error
              "Generic tree-sitter indentation error"
              'treesit-error)

(defvar treesit--indent-verbose nil
  "If non-nil, log progress when indenting.")

(defvar-local treesit-simple-indent-rules nil
  "A list of indent rule settings.
Each indent rule setting should be (LANGUAGE . RULES),
where LANGUAGE is a language symbol, and RULES is a list of

    (MATCHER ANCHOR OFFSET).

MATCHER determines whether this rule applies, ANCHOR and OFFSET
together determines which column to indent to.

A MATCHER is a function that takes three arguments (NODE PARENT
BOL).  BOL is the point where we are indenting: the beginning of
line content, the position of the first non-whitespace character.
NODE is the largest (highest-in-tree) node starting at that
point.  PARENT is the parent of NODE.

If MATCHER returns non-nil, meaning the rule matches, Emacs then
uses ANCHOR to find an anchor, it should be a function that takes
the same argument (NODE PARENT BOL) and returns a point.

Finally Emacs computes the column of that point returned by
ANCHOR and adds OFFSET to it, and indents to that column.  OFFSET
can be an integer or a variable whose value is an integer.

For MATCHER and ANCHOR, Emacs provides some convenient presets.
See `treesit-simple-indent-presets'.")

(defvar treesit-simple-indent-presets
  (list (cons 'match
              (lambda
                (&optional node-type parent-type node-field
                           node-index-min node-index-max)
                (lambda (node parent &rest _)
                  (and (or (null node-type)
                           (string-match-p
                            node-type (or (treesit-node-type node) "")))
                       (or (null parent-type)
                           (string-match-p
                            parent-type (treesit-node-type parent)))
                       (or (null node-field)
                           (string-match-p
                            node-field
                            (or (treesit-node-field-name node) "")))
                       (or (null node-index-min)
                           (>= (treesit-node-index node)
                               node-index-min))
                       (or (null node-index-max)
                           (<= (treesit-node-index node)
                               node-index-max))))))
        ;; TODO: Document if genuinely useful.
        (cons 'n-p-gp
              (lambda (node-t parent-t grand-parent-t)
                (lambda (node parent &rest _)
                  (and (or (null node-t)
                           (string-match-p
                            node-t (or (treesit-node-type node) "")))
                       (or (null parent-t)
                           (string-match-p
                            parent-t (treesit-node-type parent)))
                       (or (null grand-parent-t)
                           (string-match-p
                            grand-parent-t
                            (treesit-node-type
                             (treesit-node-parent parent))))))))
        (cons 'no-node (lambda (node &rest _) (null node)))
        (cons 'parent-is (lambda (type)
                           (lambda (_n parent &rest _)
                             (string-match-p
                              type (treesit-node-type parent)))))

        (cons 'node-is (lambda (type)
                         (lambda (node &rest _)
                           (string-match-p
                            type (or (treesit-node-type node) "")))))
        (cons 'field-is (lambda (name)
                          (lambda (node &rest _)
                            (string-match-p
                             name (or (treesit-node-field-name node) "")))))
        (cons 'comment-end (lambda (&rest _)
                             (looking-at-p treesit-comment-end)))
        ;; TODO: Document.
        (cons 'catch-all (lambda (&rest _) t))

        (cons 'query (lambda (pattern)
                       (lambda (node parent &rest _)
                         (cl-loop for capture
                                  in (treesit-query-capture
                                      parent pattern)
                                  if (treesit-node-eq node (cdr capture))
                                  return t
                                  finally return nil))))
        (cons 'first-sibling (lambda (_n parent &rest _)
                               (treesit-node-start
                                (treesit-node-child parent 0))))
        ;; TODO: Document.
        (cons 'nth-sibling (lambda (n &optional named)
                             (lambda (_n parent &rest _)
                               (treesit-node-start
                                (treesit-node-child parent n named)))))
        (cons 'parent (lambda (_n parent &rest _)
                        (treesit-node-start parent)))
        (cons 'comment-start
              (lambda (_n parent &rest _)
                (save-excursion
                  (goto-char (treesit-node-start parent))
                  (re-search-forward treesit-comment-start)
                  (point))))
        (cons 'comment-start-skip
              (lambda (_n parent &rest _)
                (save-excursion
                  (goto-char (treesit-node-start parent))
                  (re-search-forward treesit-comment-start)
                  (skip-syntax-forward "-")
                  (point))))
        ;; TODO: Document.
        (cons 'grand-parent
              (lambda (_n parent &rest _)
                (treesit-node-start (treesit-node-parent parent))))
        (cons 'parent-bol (lambda (_n parent &rest _)
                            (save-excursion
                              (goto-char (treesit-node-start parent))
                              (back-to-indentation)
                              (point))))
        (cons 'prev-sibling (lambda (node &rest _)
                              (treesit-node-start
                               (treesit-node-prev-sibling node))))
        (cons 'no-indent (lambda (_n _p bol &rest _) bol))
        (cons 'prev-line (lambda (_n _p bol &rest _)
                           (save-excursion
                             (goto-char bol)
                             (forward-line -1)
                             (skip-chars-forward " \t"))))
        (cons 'point-min (lambda (&rest _) (point-min)))
        ;; TODO: Document.
        (cons 'and (lambda (&rest fns)
                     (lambda (node parent bol &rest _)
                       (cl-reduce (lambda (a b) (and a b))
                                  (mapcar (lambda (fn)
                                            (funcall fn node parent bol))
                                          fns)))))
        (cons 'or (lambda (&rest fns)
                    (lambda (node parent bol &rest _)
                      (cl-reduce (lambda (a b) (or a b))
                                 (mapcar (lambda (fn)
                                           (funcall fn node parent bol))
                                         fns)))))
        (cons 'not (lambda (fn)
                     (lambda (node parent bol &rest _)
                       (debug)
                       (not (funcall fn node parent bol)))))
        (cons 'list (lambda (&rest fns)
                      (lambda (node parent bol &rest _)
                        (mapcar (lambda (fn)
                                  (funcall fn node parent bol))
                                fns)))))
  "A list of presets.
These presets that can be used as MATHER and ANCHOR in
`treesit-simple-indent-rules'.  MACHTERs and ANCHORs are
functions that takes 3 arguments: NODE, PARENT and BOL.

MATCHER:

\(match NODE-TYPE PARENT-TYPE NODE-FIELD NODE-INDEX-MIN NODE-INDEX-MAX)

    NODE-TYPE checks for NODE's type, PARENT-TYPE checks for
    PARENT's type, NODE-FIELD checks for the filed name of NODE
    in PARENT, NODE-INDEX-MIN and NODE-INDEX-MAX checks for
    NODE's index in PARENT.  Therefore, to match the first child
    where PARENT is \"argument_list\", use

        (match nil \"argument_list\" nil nil 0 0).

    NODE-TYPE, PARENT-TYPE, and NODE-FIELD are regexps.

no-node

    Matches the case where NODE is nil, i.e., there is no node
    that starts at point.  This is the case when indenting an
    empty line.

\(parent-is TYPE)

    Check that PARENT's type matches regexp TYPE.

\(node-is TYPE)

    Checks that NODE's type matches regexp TYPE.

\(query QUERY)

    Queries PARENT with QUERY, and checks if NODE is
    captured (by any capture name).

comment-end

    Matches if text after point matches `treesit-comment-end'.

ANCHOR:

first-sibling

    Returns the start of the first child of PARENT.

parent

    Returns the start of PARENT.

parent-bol

    Returns the beginning of non-space characters on the line where
    PARENT is on.

prev-sibling

    Returns the start of NODE's previous sibling.

no-indent

    Returns the start of NODE.

prev-line

    Returns the  first non-whitespace character on the previous line.

point-min

    Returns the beginning of buffer, which is always at column 0.

comment-start

    Returns the position after a match for `treesit-comment-start'.
    Assumes PARENT is a comment node.

comment-start-skip

    Goes to the position that comment-start would return, skips
    whitespace after that, and returns the resulting position.
    Assumes PARENT is a comment node.")

(defun treesit--simple-indent-eval (exp)
  "Evaluate EXP.

If EXP is an application and the function is a key in
`treesit-simple-indent-presets', use the corresponding value as
the function."
  ;; We don't want to match uncompiled lambdas, so make sure this cons
  ;; is not a function.  We could move the condition functionp
  ;; forward, but better be explicit.
  (cond ((and (consp exp) (not (functionp exp)))
         (apply (treesit--simple-indent-eval (car exp))
                (mapcar #'treesit--simple-indent-eval
                        (cdr exp))))
        ;; Presets override functions, so this condition comes before
        ;; `functionp'.
        ((alist-get exp treesit-simple-indent-presets)
         (alist-get exp treesit-simple-indent-presets))
        ((functionp exp) exp)
        ((symbolp exp)
         (if (null exp)
             exp
           ;; Matchers only return lambdas, anchors only return
           ;; integer, so we should never see a variable.
           (signal 'treesit-indent-error
                   (list "Couldn't find the preset corresponding to expression"
                         exp))))
        (t exp)))

;; This variable might seem unnecessary: why split
;; `treesit-indent' and `treesit-simple-indent' into two
;; functions?  We add this variable in between because later we might
;; add more powerful indentation engines, and that new engine can
;; probably share `treesit-indent'.  It is also useful, suggested
;; by Stefan M, to have a function that figures out how much to indent
;; but doesn't actually performs the indentation, because we might
;; want to know where will a node indent to if we put it at some other
;; location, and use that information to calculate the actual
;; indentation.  And `treesit-simple-indent' is that function.  I
;; forgot the example Stefan gave, but it makes a lot of sense.
(defvar treesit-indent-function #'treesit-simple-indent
  "Function used by `treesit-indent' to do some of the work.

This function is called with

    (NODE PARENT BOL &rest _)

and returns

    (ANCHOR . OFFSET).

BOL is the position of the beginning of the line; NODE is the
\"largest\" node that starts at BOL; PARENT is its parent; ANCHOR
is a point (not a node), and OFFSET is a number.  Emacs finds the
column of ANCHOR and adds OFFSET to it as the final indentation
of the current line.")

(defun treesit--indent-1 ()
  "Indent the current line.
Return (ANCHOR . OFFSET).  This function is used by
`treesit-indent' and `treesit-indent-region'."
  ;; Basically holds the common part between the two indent function.
  (let* ((bol (save-excursion
                (forward-line 0)
                (skip-chars-forward " \t")
                (point)))
         (smallest-node
          (cond ((null (treesit-parser-list)) nil)
                ((eq 1 (length (treesit-parser-list)))
                 (treesit-node-at bol))
                ((treesit-language-at (point))
                 (treesit-node-at bol (treesit-language-at (point))))
                (t (treesit-node-at bol))))
         (node (treesit-parent-while
                smallest-node
                (lambda (node)
                  (eq bol (treesit-node-start node))))))
    (let*
        ((parser (if smallest-node
                     (treesit-node-parser smallest-node)
                   nil))
         ;; NODE would be nil if BOL is on a whitespace.  In that case
         ;; we set PARENT to the "node at point", which would
         ;; encompass the whitespace.
         (parent (cond ((and node parser)
                        (treesit-node-parent node))
                       (t (treesit-node-on bol bol)))))
      (funcall treesit-indent-function node parent bol))))

(defun treesit-indent ()
  "Indent according to the result of `treesit-indent-function'."
  (treesit-update-ranges (line-beginning-position)
                         (line-end-position))
  ;; We don't return 'noindent even if no rules match, because
  ;; `indent-for-tab-command' tries to indent itself when we return
  ;; 'noindent, which leads to wrong indentation at times.
  (pcase-let* ((`(,anchor . ,offset) (treesit--indent-1)))
    (when (and anchor offset)
      (let ((col (+ (save-excursion
                      (goto-char anchor)
                      (current-column))
                    offset))
            (delta (- (point-max) (point))))
        (indent-line-to col)
        ;; Now point is at the end of indentation.  If we started
        ;; from within the line, go back to where we started.
        (when (> (- (point-max) delta) (point))
          (goto-char (- (point-max) delta)))))))

;; Batch size can't be too large, because we put markers on each
;; ANCHOR, so a batch size of 400 lines means 400 markers.
(defvar treesit--indent-region-batch-size 400
  "How many lines of indent value do we precompute.
In `treesit-indent-region' we indent in batches: precompute
indent for each line, apply them in one go, let parser reparse,
and do it again.  This way the parser don't need to unnecessarily
reparse after indenting every single line.")

(defun treesit-indent-region (beg end)
  "Indent the region between BEG and END.
Similar to `treesit-indent', but indent a region instead."
  (treesit-update-ranges beg end)
  ;; We indent `treesit--indent-region-batch-size' lines at a time, to
  ;; reduce the number of times the parser needs to re-parse.  In each
  ;; batch, we go through each line and calculate the anchor and
  ;; offset as usual, but instead of modifying the buffer, we save
  ;; these information in a vector.  Once we've collected ANCHOR and
  ;; OFFSET for each line in the batch, we go through each line again
  ;; and apply the changes.  Now that buffer is modified, we need to
  ;; reparse the buffer before continuing to indent the next batch.
  (let* ((meta-len 2)
         (vector-len (* meta-len treesit--indent-region-batch-size))
         ;; This vector saves the indent meta for each line in the
         ;; batch.  It is a vector [ANCHOR OFFSET ANCHOR OFFSET...].
         ;; ANCHOR is a marker on the anchor position, and OFFSET is
         ;; an integer.  ANCHOR and OFFSET are either both nil, or
         ;; both valid.
         (meta-vec (make-vector vector-len 0))
         (lines-left-to-move 0)
         (end (copy-marker end t))
         (idx 0)
         (starting-pos 0)
         (announce-progress (> (- end beg) 80000)))
    (save-excursion
      (goto-char beg)
      ;; First pass.  Go through each line and compute the
      ;; indentation.
      (while (and (eq lines-left-to-move 0) (< (point) end))
        (setq idx 0
              starting-pos (point))
        (while (and (eq lines-left-to-move 0)
                    (< idx treesit--indent-region-batch-size)
                    (< (point) end))
          (if (looking-at (rx (* whitespace) eol) t)
              ;; Unlike in `indent-line' where we sometimes pre-indent
              ;; an empty line, We don't indent empty lines in
              ;; `indent-region'.  Set ANCHOR and OFFSET to nil.
              (setf (aref meta-vec (* idx meta-len)) nil
                    (aref meta-vec (+ 1 (* idx meta-len))) nil)
            (pcase-let* ((`(,anchor . ,offset) (treesit--indent-1))
                         (marker (aref meta-vec (* idx meta-len))))
              ;; Set ANCHOR.
              (when anchor
                (if (markerp marker)
                    (move-marker marker anchor)
                  (setf (aref meta-vec (* idx meta-len))
                        (copy-marker anchor t))))
              ;; SET OFFSET.
              (setf (aref meta-vec (+ 1 (* idx meta-len))) offset)))
          (cl-incf idx)
          (setq lines-left-to-move (forward-line 1)))
        ;; Now IDX = last valid IDX + 1.
        (goto-char starting-pos)
        ;; Second pass, go to each line and apply the indentation.
        (dotimes (jdx idx)
          (let ((anchor (aref meta-vec (* jdx meta-len)))
                (offset (aref meta-vec (+ 1 (* jdx meta-len)))))
            (when offset
              (let ((col (save-excursion
                           (goto-char anchor)
                           (+ offset (current-column)))))
                (indent-line-to col))))
          (forward-line 1))
        (when announce-progress
          (message "Indenting region...%s%%"
                   (/ (* (- (point) beg) 100) (- end beg)))))
      ;; Delete markers.
      (dotimes (idx treesit--indent-region-batch-size)
        (let ((marker (aref meta-vec (* idx meta-len))))
          (when (markerp marker)
            (move-marker marker nil))))
      (move-marker end nil))))

(defun treesit-simple-indent (node parent bol)
  "Calculate indentation according to `treesit-simple-indent-rules'.

BOL is the position of the first non-whitespace character on the
current line.  NODE is the largest node that starts at BOL,
PARENT is NODE's parent.

Return (ANCHOR . OFFSET) where ANCHOR is a node, OFFSET is the
indentation offset, meaning indent to align with ANCHOR and add
OFFSET."
  (if (null parent)
      (progn (when treesit--indent-verbose
               (message "PARENT is nil, not indenting"))
             (cons nil nil))
    (let* ((language (treesit-node-language parent))
           (rules (alist-get language
                             treesit-simple-indent-rules)))
      (cl-loop for rule in rules
               for pred = (nth 0 rule)
               for anchor = (nth 1 rule)
               for offset = (nth 2 rule)
               if (treesit--simple-indent-eval
                   (list pred node parent bol))
               do (when treesit--indent-verbose
                    (message "Matched rule: %S" rule))
               and
               return
               (let ((anchor-pos
                      (treesit--simple-indent-eval
                       (list anchor node parent bol))))
                 (cons anchor-pos (if (symbolp offset)
                                      (symbol-value offset)
                                    offset)))
               finally return
               (progn (when treesit--indent-verbose
                        (message "No matched rule"))
                      (cons nil nil))))))

(defun treesit-check-indent (mode)
  "Check current buffer's indentation against a major mode MODE.

Pop up a diff buffer showing the difference.  Correct
indentation (target) is in green, current indentation is in red."
  (interactive "CTarget major mode: ")
  (let ((source-buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring source-buf)
      (funcall mode)
      (indent-region (point-min) (point-max))
      (diff-buffers source-buf (current-buffer)))))

(defun treesit--indent-rules-optimize (rules)
  "Optimize simple indent RULES.
RULES should be a value suitable for
`treesit-simple-indent-rules'.  Return the optimized version of
RULES."
  ;; Right now this function just compiles queries.  it doesn't
  ;; byte-compile matchers and anchors because it doesn't make much
  ;; difference.
  (cl-loop for setting in rules
           for lang = (car setting)
           for indent-rules = (cdr setting)
           collect
           (cl-labels
               ;; Optimize a matcher or anchor.
               ((optimize-func (func)
                  (pcase func
                    (`(query ,qry)
                     (list 'query (treesit-query-compile lang qry)))
                    (_ func)))
                ;; Optimize a rule (MATCHER ANCHOR OFFSET).
                (optimize-rule (rule)
                  (let ((matcher (nth 0 rule))
                        (anchor (nth 1 rule))
                        (offset (nth 2 rule)))
                    (list (optimize-func matcher)
                          (optimize-func anchor)
                          offset))))
             (cons lang (mapcar #'optimize-rule indent-rules)))))

;;; Search

(defun treesit-search-forward-goto
    (node predicate &optional start backward all)
  "Search forward for a node and move to its end position.

Stop at the first node after NODE that matches PREDICATE.
PREDICATE can be either a regexp that matches against each node's
type case-insensitively, or a function that takes a node and
returns nil/non-nil for match/no match.

If a node matches, move to that node and return the node,
otherwise return nil.  If START is non-nil, stop at the
beginning rather than the end of a node.

This function guarantees that the matched node it returns makes
progress in terms of buffer position: the start/end position of
the returned node is always greater than that of NODE.

BACKWARD and ALL are the same as in `treesit-search-forward'."
  (when-let* ((start-pos (if start
                             (treesit-node-start node)
                           (treesit-node-end node)))
              (current-pos start-pos))
    ;; When searching forward and stopping at beginnings, or search
    ;; backward stopping at ends, it is possible to "roll back" in
    ;; position.  Take three nodes N1, N2, N3 as an example, if we
    ;; start at N3, search for forward for beginning, and N1 matches,
    ;; we would stop at beg of N1, which is backwards!  So we skip N1
    ;; and keep going.
    ;;
    ;;   |<--------N1------->|
    ;;   |<--N2-->| |<--N3-->|
    (while (and node (if backward
                         (>= current-pos start-pos)
                       (<= current-pos start-pos)))
      (setq node (treesit-search-forward
                  node predicate backward all))
      (setq current-pos (if start
                            (treesit-node-start node)
                          (treesit-node-end node))))
    (cond
     ;; When there is a match and match made progress, go to the
     ;; result position.
     ((and node
           (if backward
               (< current-pos (point))
             (> current-pos (point))))
      (goto-char current-pos)))
    node))

;;; Navigation

(defvar-local treesit-defun-type-regexp nil
  "A regexp that matches the node type of defun nodes.
For example, \"(function|class)_definition\".

This is used by `treesit-beginning-of-defun' and friends.")

(defun treesit-beginning-of-defun (&optional arg)
  "Tree-sitter `beginning-of-defun' function.
ARG is the same as in `beginning-of-defun'."
  (let ((arg (or arg 1))
        (node (treesit-node-at (point))))
    (if (> arg 0)
        ;; Go backward.
        (while (and (> arg 0)
                    (setq node (treesit-search-forward-goto
                                node treesit-defun-type-regexp t t)))
          (setq node (or (treesit-node-top-level
                          node treesit-defun-type-regexp)
                         node))
          (setq arg (1- arg)))
      ;; Go forward.
      (while (and (< arg 0)
                  (setq node (treesit-search-forward-goto
                              node treesit-defun-type-regexp)))
        (setq node (or (treesit-node-top-level
                        node treesit-defun-type-regexp)
                       node))
        (setq arg (1+ arg))))
    (when node
      (goto-char (treesit-node-start node))
      t)))

(defun treesit-end-of-defun ()
  "Tree-sitter `end-of-defun' function."
  ;; Why not simply get the largest node at point: when point is at
  ;; (point-min), that gives us the root node.
  (let* ((node (treesit-node-at (point)))
         (top (or (treesit-node-top-level
                   node
                   treesit-defun-type-regexp)
                  node)))
    (goto-char (treesit-node-end top))))

;;; Imenu

(defvar-local treesit-imenu-function nil
  "Tree-sitter version of `imenu-create-index-function'.

Set this variable to a function and `treesit-mode' will bind it
to `imenu-create-index-function'.")

;;; Activating tree-sitter

(defun treesit-ready-p (language &optional quiet)
  "Check whether tree-sitter is ready to be used for MODE and LANGUAGE.

LANGUAGE is the language symbol to check for availability.
It can also be a list of language symbols.

If tree-sitter is not ready, emit a warning and return nil.  If
the user has chosen to activate tree-sitter for LANGUAGE and
tree-sitter is ready, return non-nil.  If QUIET is t, don't emit
warning in either case; if quiet is `message', display a message
instead of emitting warning."
  (let ((language-list (if (consp language)
                           language
                         (list language)))
        msg)
    ;; Check for each condition and set MSG.
    (catch 'term
      (when (not (treesit-available-p))
        (setq msg "tree-sitter library is not compiled with Emacs")
        (throw 'term nil))
      (when (> (buffer-size) treesit-max-buffer-size)
        (setq msg "buffer larger than `treesit-max-buffer-size'")
        (throw 'term nil))
      (dolist (lang language-list)
        (pcase-let ((`(,available . ,err)
                     (treesit-language-available-p lang t)))
          (when (not available)
            (setq msg (format "language definition for %s is unavailable (%s): %s"
                              lang (nth 0 err)
                              (string-join
                               (mapcar (lambda (x) (format "%s" x))
                                       (cdr err))
                               " ")))
            (throw 'term nil)))))
    ;; Decide if all conditions met and whether emit a warning.
    (if (not msg)
        t
      (setq msg (concat "Cannot activate tree-sitter, because " msg))
      (pcase quiet
        ('nil (display-warning 'treesit msg))
        ('message (message "%s" msg)))
      nil)))

(defun treesit-major-mode-setup ()
  "Activate tree-sitter to power major-mode features.

If `treesit-font-lock-settings' is non-nil, setup fontification and
enable `font-lock-mode'.

If `treesit-simple-indent-rules' is non-nil, setup indentation.

If `treesit-defun-type-regexp' is non-nil, setup
`beginning/end-of-defun' functions.

Make sure necessary parsers are created for the current buffer
before calling this function."
  ;; Font-lock.
  (when treesit-font-lock-settings
    ;; `font-lock-mode' wouldn't setup properly if
    ;; `font-lock-defaults' is nil, see `font-lock-specified-p'.
    (setq-local font-lock-defaults
                '( nil nil nil nil
                   (font-lock-fontify-syntactically-function
                    . treesit-font-lock-fontify-region)))
    (font-lock-mode 1)
    (treesit-font-lock-recompute-features)
    (dolist (parser (treesit-parser-list))
      (treesit-parser-add-notifier
       parser #'treesit--font-lock-notifier)))
  ;; Indent.
  (when treesit-simple-indent-rules
    (setq-local treesit-simple-indent-rules
                (treesit--indent-rules-optimize
                 treesit-simple-indent-rules))
    (setq-local indent-line-function #'treesit-indent)
    (setq-local indent-region-function #'treesit-indent-region))
  ;; Navigation.
  (when treesit-defun-type-regexp
    (setq-local beginning-of-defun-function #'treesit-beginning-of-defun)
    (setq-local end-of-defun-function #'treesit-end-of-defun)))

;;; Debugging

(defvar-local treesit--inspect-name nil
  "Used by `treesit-inspect-mode' to show node name in mode-line.")

(defun treesit-inspect-node-at-point (&optional arg)
  "Show information of the node at point.
If called interactively, show in echo area, otherwise set
`treesit--inspect-name' (which will appear in the mode-line
if `treesit-inspect-mode' is enabled).  Uses the first parser
in `treesit-parser-list'."
  (interactive "p")
  ;; NODE-LIST contains all the node that starts at point.
  (let* ((node-list
          (cl-loop for node = (treesit-node-at (point))
                   then (treesit-node-parent node)
                   while node
                   if (eq (treesit-node-start node)
                          (point))
                   collect node))
         (largest-node (car (last node-list)))
         (parent (treesit-node-parent largest-node))
         ;; node-list-acending contains all the node bottom-up, then
         ;; the parent.
         (node-list-acending
          (if (null largest-node)
              ;; If there are no nodes that start at point, just show
              ;; the node at point and its parent.
              (list (treesit-node-at (point))
                    (treesit-node-parent
                     (treesit-node-at (point))))
            (append node-list (list parent))))
         (name ""))
    ;; We draw nodes like (parent field-name: (node)) recursively,
    ;; so it could be (node1 field-name: (node2 field-name: (node3))).
    (dolist (node node-list-acending)
      (setq
       name
       (concat
        (if (treesit-node-field-name node)
            (format " %s: " (treesit-node-field-name node))
          " ")
        (if (treesit-node-check node 'named) "(" "\"")
        (propertize (or (treesit-node-type node) "N/A")
                    'face
                    (if (treesit-node-eq node largest-node)
                        'bold nil))
        name
        (if (treesit-node-check node 'named) ")" "\""))))
    (setq treesit--inspect-name name)
    (force-mode-line-update)
    (when arg
      (if node-list
          (message "%s" treesit--inspect-name)
        (message "No node at point")))))

(define-minor-mode treesit-inspect-mode
  "Minor mode that displays in the mode-line the node which starts at point.

When this mode is enabled, the mode-line displays

    PARENT FIELD-NAME: (NODE FIELD-NAME: (CHILD (...)))

where NODE, CHILD, etc, are nodes which begin at point.  PARENT
is the parent of NODE.  NODE is displayed in bold typeface.
FIELD-NAMEs are field names of NODE and CHILD, etc (see Info
node `(elisp)Language Definitions', heading \"Field names\").

If no node starts at point, i.e., point is in the middle of a
node, then the mode line displays the earliest node that spans point,
and its immediate parent.

This minor mode doesn't create parsers on its own.  It uses the first
parser in `treesit-parser-list'."
  :lighter nil
  (if treesit-inspect-mode
      (progn
        (add-hook 'post-command-hook
                  #'treesit-inspect-node-at-point 0 t)
        (add-to-list 'mode-line-misc-info
                     '(:eval treesit--inspect-name)))
    (remove-hook 'post-command-hook
                 #'treesit-inspect-node-at-point t)
    (setq mode-line-misc-info
          (remove '(:eval treesit--inspect-name)
                  mode-line-misc-info))))

(defun treesit-query-validate (language query)
  "Check if QUERY is valid for LANGUAGE.
If QUERY is invalid, display the query in a popup buffer, jump
to the offending pattern and highlight the pattern."
  (cl-assert (or (consp query) (stringp query)))
  (let ((buf (get-buffer-create "*tree-sitter check query*")))
    (with-temp-buffer
      (treesit-parser-create language)
      (condition-case err
          (progn (treesit-query-capture language query)
                 (message "QUERY is valid"))
        (treesit-query-error
         (with-current-buffer buf
           (let* ((data (cdr err))
                  (message (nth 0 data))
                  (start (nth 1 data)))
             (erase-buffer)
             (insert (treesit-query-expand query))
             (goto-char start)
             (search-forward " " nil t)
             (put-text-property start (point) 'face 'error)
             (message "%s" (buffer-substring start (point)))
             (goto-char (point-min))
             (insert (format "%s: %d\n" message start))
             (forward-char start)))
         (pop-to-buffer buf))))))

;;; Explorer

(defvar-local treesit--explorer-buffer nil
  "Buffer used to display the syntax tree.")

(defvar-local treesit--explorer-source-buffer nil
  "Source buffer corresponding to the playground buffer.")

(defvar-local treesit--explorer-language nil
  "The language used in the playground.")

(defvar-local treesit--explorer-refresh-timer nil
  "Timer for refreshing the syntax tree buffer.")

(defvar-local treesit--explorer-highlight-overlay nil
  "Overlay used to highlight in syntax tree and source buffer.")

(defvar-local treesit--explorer-last-node nil
  "Last top-level node used to generate syntax tree.")

(defvar treesit-explore-mode)

(defun treesit--explorer--nodes-to-highlight (language)
  "Return nodes for LANGUAGE covered in region.
This function tries to return the largest node possible.  So it
will return a single large node rather than a bunch of small
nodes.  If it end up returning multiple small nodes, it only
returns the first and last node, and omits the ones in between."
  (let* ((beg (region-beginning))
         (end (region-end))
         (node (treesit-node-on beg end language))
         (node (or (treesit-parent-while
                    node
                    (lambda (n)
                      (<= beg (treesit-node-start n)
                          (treesit-node-end n) end)))
                   node)))
    ;; If NODE is completely contained in the region, return NODE,
    ;; otherwise return its children that are in the region.
    (if (<= beg (treesit-node-start node)
            (treesit-node-end node) end)
        (list node)
      (list (treesit-node-at beg)
            (treesit-search-forward
             (treesit-node-at end)
             (lambda (n)
               (<= (treesit-node-end n) end))
             t t)))))

(defun treesit--explorer-refresh ()
  "Update the syntax tree buffer."
  (when (and treesit-explore-mode
             (buffer-live-p treesit--explorer-buffer))
    (let* ((root (treesit-node-on
                  (window-start) (window-end) treesit--explorer-language))
           ;; Only highlight the current top-level construct.
           ;; Highlighting the whole buffer is slow and unnecessary.
           (top-level (treesit-node-first-child-for-pos
                       root (if (eolp)
                                (max (point-min) (1- (point)))
                              (point))
                       t))
           ;; Only highlight node when region is active, if we
           ;; highlight node at point the syntax tree is too jumpy.
           (nodes-hl
            (when (region-active-p)
              (treesit--explorer--nodes-to-highlight
               treesit--explorer-language)))
           ;; If we didn't edit the buffer nor change the top-level
           ;; node, don't redraw the whole syntax tree.
           (highlight-only (treesit-node-eq
                            top-level treesit--explorer-last-node))
           (source-buffer (current-buffer)))
      (setq-local treesit--explorer-last-node top-level)
      (with-current-buffer treesit--explorer-buffer
        (let ((inhibit-read-only t))
          (setq-local treesit--explorer-source-buffer source-buffer)
          ;; Redraw the syntax tree or just rehighlight the focused
          ;; node.
          (when (and top-level (not highlight-only))
            (erase-buffer)
            (treesit--explorer-draw-node top-level))
          (when-let ((pos (treesit--explorer-highlight-node nodes-hl))
                     (window (get-buffer-window
                              treesit--explorer-buffer)))
            (if highlight-only
                (goto-char pos)
              ;; If HIGHLIGHT-ONLY is nil, we erased the buffer and
              ;; re-inserted text, scroll down from the very top until
              ;; we can see the highlighted node.
              (goto-char (point-min))
              (while (and (null (pos-visible-in-window-p pos window))
                          (= (forward-line 4) 0))
                (set-window-start window (point))))
            (set-window-point window pos)))))))

(defun treesit--explorer-post-command (&rest _)
  "Post-command function that runs in the source buffer."
  (when treesit-explore-mode
    (when treesit--explorer-highlight-overlay
      (delete-overlay treesit--explorer-highlight-overlay))
    (when treesit--explorer-refresh-timer
      (cancel-timer treesit--explorer-refresh-timer))
    (setq-local treesit--explorer-refresh-timer
                (run-with-timer 0.1 nil #'treesit--explorer-refresh))))

(defun treesit--explorer-jump (button)
  "Mark the original text corresponding to BUTTON."
  (interactive)
  (when (and (derived-mode-p 'treesit--explorer-tree-mode)
             (buffer-live-p treesit--explorer-source-buffer))
    (with-current-buffer treesit--explorer-source-buffer
      (let ((start (button-get button 'node-start))
            (end (button-get button 'node-end)))
        (when treesit--explorer-highlight-overlay
          (delete-overlay treesit--explorer-highlight-overlay))
        (setq-local treesit--explorer-highlight-overlay
                    (make-overlay start end nil t nil))
        (overlay-put treesit--explorer-highlight-overlay
                     'face 'highlight)))))

(defun treesit--explorer-highlight-node (nodes)
  "Highlight nodes in NODES in the syntax tree buffer.
Return the start of the syntax tree text corresponding to NODE."
  (when treesit--explorer-highlight-overlay
    (delete-overlay treesit--explorer-highlight-overlay))
  (let ((start-node (car nodes))
        (end-node (car (last nodes)))
        start end)
    (when (and start-node end-node)
      (cl-loop for ov in (overlays-in (point-min) (point-max))
               while (or (null start) (null end))
               if (treesit-node-eq start-node
                                   (overlay-get ov 'treesit-node))
               do (setq start (overlay-start ov))
               if (treesit-node-eq end-node (overlay-get ov 'treesit-node))
               do (setq end (overlay-end ov)))
      (when (and start end)
        (setq-local treesit--explorer-highlight-overlay
                    (make-overlay start end))
        (overlay-put treesit--explorer-highlight-overlay
                     'face 'highlight)
        start))))

(defun treesit--explorer-draw-node (node)
  "Draw the syntax tree of NODE.
If NODE and NODE-HIGHLIGHT are the same node, highlight it.

When this function is called, point should be at an empty line,
when appropriate indent in front of point.  When this function
returns, it leaves point at the end of the last line of NODE.

Return the start position of NODE-HIGHLIGHT in the buffer, if any."
  (let* ((type (treesit-node-type node))
         (field-name (treesit-node-field-name node))
         (children (treesit-node-children node))
         (named (treesit-node-check node 'named))
         ;; Column number of the start of the field-name, aka start of
         ;; the whole node.
         (before-field-column (current-column))
         ;; Column number after the field-name.
         after-field-column
         ;; Column number after the type.
         after-type-column
         ;; Are all children suitable for inline?
         (all-children-inline
          (eq 0 (apply #'+ (mapcar #'treesit-node-child-count children))))
         ;; If the child is the first child, we can inline, if the
         ;; previous child is suitable for inline, this child can
         ;; inline, if the previous child is not suitable for inline,
         ;; this child cannot inline.
         (can-inline t)
         ;; The beg and end of this node.
         beg end)
    (when treesit--explorer-highlight-overlay
      (delete-overlay treesit--explorer-highlight-overlay))

    (setq beg (point))
    ;; Draw field name.  If all children are suitable for inline, we
    ;; draw everything in one line, other wise draw field name and the
    ;; rest of the node in two lines.
    (when field-name
      (insert field-name ": ")
      (when (and children (not all-children-inline))
        (insert "\n")
        (indent-to-column (1+ before-field-column))))
    (setq after-field-column (current-column))

    ;; Draw type.
    (if named
        (progn
          (insert "(")
          (insert-text-button
           type 'action #'treesit--explorer-jump
           'follow-link t
           'node-start (treesit-node-start node)
           'node-end (treesit-node-end node)))
      (pcase type
        ("\n" (insert "\\n"))
        ("\t" (insert "\\t"))
        (" " (insert "SPC"))
        (_ (insert type))))
    (setq after-type-column (current-column))

    ;; Draw children.
    (dolist (child children)
      ;; If a child doesn't have children, it is suitable for inline.
      (let ((draw-inline (eq 0 (treesit-node-child-count child)))
            (children-indent (1+ after-field-column)))
        (while
            ;; This form returns t if it wants to run another
            ;; iteration, returns nil if it wants to stop.
            (if (and draw-inline can-inline)
                ;; Draw children on the same line.
                (let ((inline-beg (point)))
                  (insert " ")
                  (treesit--explorer-draw-node child)
                  ;; If we exceeds window width, draw on the next line.
                  (if (< (current-column) (window-width))
                      nil
                    (delete-region inline-beg (point))
                    (setq draw-inline nil
                          children-indent (1+ after-type-column))
                    t))
              ;; Draw children on the new line.
              (insert "\n")
              (indent-to-column children-indent)
              (treesit--explorer-draw-node child)
              nil))
        (setq can-inline draw-inline)))

    ;; Done drawing children, draw the ending paren.
    (when named (insert ")"))
    (setq end (point))

    ;; Associate the text with NODE, so we can later find a piece of
    ;; text by a node.
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'treesit-node node)
      (overlay-put ov 'evaporate t)
      (when (not named)
        (overlay-put ov 'face 'shadow)))))

(define-derived-mode treesit--explorer-tree-mode special-mode
  "TS Explorer"
  "Mode for displaying syntax trees for `treesit-explore-mode'."
  nil)

(define-minor-mode treesit-explore-mode
  "Enable exploring the current buffer's syntax tree.
Pops up a window showing the syntax tree of the source in the
current buffer in real time.  The corresponding node enclosing
the text in the active region is highlighted in the explorer
window."
  :lighter " TSplay"
  (if treesit-explore-mode
      (progn
        (unless (buffer-live-p treesit--explorer-buffer)
          (setq-local treesit--explorer-buffer
                      (get-buffer-create
                       (format "*tree-sitter playground for %s*"
                               (buffer-name))))
          (setq-local treesit--explorer-language
                      (intern (completing-read
                               "Language: "
                               (mapcar #'treesit-parser-language
                                       (treesit-parser-list)))))
          (with-current-buffer treesit--explorer-buffer
            (treesit--explorer-tree-mode)))
        (display-buffer treesit--explorer-buffer
                        (cons nil '((inhibit-same-window . t))))
        (treesit--explorer-refresh)
        (add-hook 'post-command-hook
                  #'treesit--explorer-post-command 0 t)
        (setq-local treesit--explorer-last-node nil))
    (remove-hook 'post-command-hook
                 #'treesit--explorer-post-command t)
    (kill-buffer treesit--explorer-buffer)))

;;; Etc

(declare-function find-library-name "find-func.el")
(defun treesit--check-manual-coverage ()
  "Print tree-sitter functions missing from the manual in message buffer."
  (interactive)
  (require 'find-func)
  (let ((functions-in-source
         (with-temp-buffer
           (insert-file-contents (find-library-name "treesit"))
           (cl-remove-if
            (lambda (name) (string-match "treesit--" name))
            (cl-sort
             (save-excursion
               (goto-char (point-min))
               (cl-loop while (re-search-forward
                               "^(defun \\([^ ]+\\)" nil t)
                        collect (match-string-no-properties 1)))
             #'string<))))
        (functions-in-manual
         (with-temp-buffer
           (insert-file-contents (expand-file-name
                                  "doc/lispref/parsing.texi"
                                  source-directory))
           (insert-file-contents (expand-file-name
                                  "doc/lispref/modes.texi"
                                  source-directory))
           (cl-sort
            (save-excursion
              (goto-char (point-min))
              (cl-loop while (re-search-forward
                              "^@defun \\([^ ]+\\)" nil t)
                       collect (match-string-no-properties 1)))
            #'string<))))
    (message "Missing: %s"
             (string-join
              (cl-remove-if
               (lambda (name) (member name functions-in-manual))
               functions-in-source)
              "\n"))))

;;; Shortdocs

(defun treesit--generate-shortdoc-examples ()
  "Generate examples for shortdoc."
  (with-temp-buffer
    (let (node parent)
      (insert "int c = 0;")
      (print (treesit-parser-create 'c))
      (print (treesit-parser-list))
      (goto-char (point-min))
      (print (setq node (treesit-node-at (point))))
      (print (setq parent (treesit-node-parent node)))
      (print (treesit-node-children parent))
      (print (treesit-node-next-sibling node))
      (print (treesit-node-child-by-field-name parent "declarator"))
      nil)))

(define-short-documentation-group treesit


  "Parsers"
  (treesit-parser-create
   :no-eval (treesit-parser-create)
   :eg-result-string "#<treesit-parser for c>")
  (treesit-parser-delete
   :no-value (treesit-parser-delete parser))
  (treesit-parser-list
   :no-eval (treesit-parser-list)
   :eg-result-string "(#<treesit-parser for c>)")
  (treesit-parser-buffer
   :no-eval (treesit-parser-buffer parser)
   :eg-result-string "#<buffer xdisp.c>")
  (treesit-parser-language
   :no-eval (treesit-parser-language parser)
   :eg-result c)
  (treesit-parser-add-notifier)
  (treesit-parser-remove-notifier)
  (treesit-parser-notifiers
   :no-eval (treesit-parser-notifiers parser)
   :eg-result (function1 function2 function3))


  "Parser ranges"
  (treesit-parser-set-included-ranges
   :no-value (treesit-parser-set-included-ranges parser '((1 . 4) (5 . 8))))
  (treesit-parser-included-ranges
   :no-eval (treesit-parser-included-ranges parser)
   :eg-result '((1 . 4) (5 . 8)))
  (treesit-query-range
   :no-eval (treesit-query-range node '((script_element) @cap))
   :eg-result-string '((1 . 4) (5 . 8)))


  "Retrieving a node"
  (treesit-node-at
   :no-eval (treesit-node-at (point))
   :eg-result-string "#<treesit-node (identifier) in 179-180>")
  (treesit-node-on
   :no-eval (treesit-node-on 18 28)
   :eg-result-string "#<treesit-node (compound_statement) in 143-290>")
  (treesit-buffer-root-node
   :no-eval (treesit-buffer-root-node)
   :eg-result-string "#<treesit-node (translation_unit) in 1-4830>")
  (treesit-parser-root-node
   :no-eval (treesit-parser-root-node parser)
   :eg-result-string "#<treesit-node (translation_unit) in 1-4830>")


  "Retrieving a node from another node"
  (treesit-node-parent
   :no-eval (treesit-node-parent node)
   :eg-result-string "#<treesit-node (declaration) in 1-11>")
  (treesit-node-child
   :no-eval (treesit-node-child node 0)
   :eg-result-string "#<treesit-node (primitive_type) in 1-4>")
  (treesit-node-children
   :no-eval (treesit-node-children node)
   :eg-result-string "(#<treesit-node (primitive_type) in 1-4> #<treesit-node (init_declarator) in 5-10> #<treesit-node \";\" in 10-11>)")
  (treesit-node-next-sibling
   :no-eval (treesit-node-next-sibling node)
   :eg-result-string "#<treesit-node (init_declarator) in 5-10>")
  (treesit-node-prev-sibling
   :no-eval (treesit-node-prev-sibling node)
   :eg-result-string "#<treesit-node (primitive_type) in 1-4>")
  (treesit-node-child-by-field-name
   :no-eval (treesit-node-child-by-field-name node "declarator")
   :eg-result-string "#<treesit-node (init_declarator) in 5-10>")


  (treesit-first-child-for-pos
   :no-eval (treesit-first-child-for-pos node 1)
   :eg-result-string "#<treesit-node (primitive_type) in 1-4>")
  (treesit-node-descendant-for-range
   :no-eval (treesit-node-descendant-for-range node 2 3)
   :eg-result-string "#<treesit-node (primitive_type) in 1-4>")


  "Searching for node"
  (treesit-search-subtree
   :no-eval (treesit-search-subtree node "function_definition")
   :eg-result-string "#<treesit-node (function_definition) in 57-146>")
  (treesit-search-forward
   :no-eval (treesit-search-forward node "function_definition")
   :eg-result-string "#<treesit-node (function_definition) in 57-146>")
  (treesit-search-forward-goto
   :no-eval (treesit-search-forward-goto node "function_definition")
   :eg-result-string "#<treesit-node (function_definition) in 57-146>")
  (treesit-induce-sparse-tree
   :no-eval (treesit-induce-sparse-tree node "function_definition")
   :eg-result-string "(nil (#<treesit-node (function_definition) in 57-146>) (#<treesit-node (function_definition) in 259-296>) (#<treesit-node (function_definition) in 303-659>))")
  (treesit-filter-child
   :no-eval (treesit-filter-child node (lambda (n) (equal (treesit-node-type) "identifier")))
   :eg-result-string "(#<treesit-node (identifier) in 195-196>)")
  (treesit-parent-until
   :no-eval (treesit-parent-until node (lambda (p) (eq (treesit-node-start p) (point))))
   :eg-result-string "#<treesit-node (declaration) in 1-11>")
  (treesit-parent-while
   :no-eval (treesit-parent-while node (lambda (p) (eq (treesit-node-start p) (point))))
   :eg-result-string "#<treesit-node (declaration) in 1-11>")
  (treesit-node-top-level
   :no-eval (treesit-node-top-level node)
   :eg-result-string "#<treesit-node (declaration) in 1-11>")


  "Retrieving node information"
  (treesit-node-text
   :no-eval (treesit-node-text node)
   :eg-result "int")
  (treesit-node-start
   :no-eval (treesit-node-start node)
   :eg-result 1)
  (treesit-node-end
   :no-eval (treesit-node-end node)
   :eg-result 10)
  (treesit-node-type
   :no-eval (treesit-node-type node)
   :eg-result "function_definition")
  (treesit-node-field-name
   :no-eval (treesit-node-field-name node)
   :eg-result "body")


  (treesit-node-parser
   :no-eval (treesit-node-parser node)
   :eg-result-string "#<treesit-parser for c>")
  (treesit-node-language
   :no-eval (treesit-node-language node)
   :eg-result c)
  (treesit-node-buffer
   :no-eval (treesit-node-buffer node)
   :eg-result-string "#<buffer xdisp.c>")


  (treesit-node-index
   :no-eval (treesit-node-index node)
   :eg-result 0)
  (treesit-node-string
   :no-eval (treesit-node-string node)
   :eg-result-string "(init_declarator declarator: (identifier) value: (number_literal))")
  (treesit-node-check
   :no-eval (treesit-node-check node 'named)
   :eg-result t)


  (treesit-field-name-for-child
   :no-eval (treesit-field-name-for-child node)
   :eg-result "body")
  (treesit-child-count
   :no-eval (treesit-child-count node)
   :eg-result 3)


  "Pattern matching"
  (treesit-query-capture
   :no-eval (treesit-query-capture node '((identifier) @id "return" @ret))
   :eg-result-string "((id . #<treesit-node (identifier) in 195-196>) (ret . #<treesit-node "return" in 338-344>))")
  (treesit-query-compile
   :no-eval (treesit-query-compile 'c '((identifier) @id "return" @ret))
   :eg-result-string "#<treesit-compiled-query>")
  (treesit-query-language
   :no-eval (treesit-query-language compiled-query)
   :eg-result c)
  (treesit-query-expand
   :eval (treesit-query-expand '((identifier) @id "return" @ret)))
  (treesit-pattern-expand
   :eval (treesit-pattern-expand :anchor)
   :eval (treesit-pattern-expand '(identifier))
   :eval (treesit-pattern-expand :equal))


  "Parsing a string"
  (treesit-parse-string
   :no-eval (treesit-parse-string "int c = 0;" 'c)
   :eg-result-string "#<treesit-node (translation_unit) in 1-11>")
  (treesit-query-string
   :no-eval (treesit-query-string "int c = 0;" '((identifier) @id) 'c)
   :eg-result-string "((id . #<treesit-node (identifier) in 5-6>))"))

(provide 'treesit)

;;; treesit.el ends here
