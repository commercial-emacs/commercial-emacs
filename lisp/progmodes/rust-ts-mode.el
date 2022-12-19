;;; rust-ts-mode.el --- tree-sitter support for Rust  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author     : Randy Taylor <dev@rjt.dev>
;; Maintainer : Randy Taylor <dev@rjt.dev>
;; Created    : December 2022
;; Keywords   : rust languages tree-sitter

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
;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defcustom rust-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `rust-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'rust)

(defvar rust-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)
    (modify-syntax-entry ?^   "."      table)
    (modify-syntax-entry ?!   "."      table)
    (modify-syntax-entry ?@   "."      table)
    (modify-syntax-entry ?~   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    (modify-syntax-entry ?\n  "> b"    table)
    (modify-syntax-entry ?\^m "> b"    table)
    table)
  "Syntax table for `rust-ts-mode'.")

(defvar rust-ts-mode--indent-rules
  `((rust
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((parent-is "arguments") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "await_expression") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "array_expression") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "binary_expression") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "block") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "declaration_list") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "enum_variant_list") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "field_declaration_list") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "field_expression") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "field_initializer_list") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "let_declaration") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "macro_definition") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "parameters") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "token_tree") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "use_list") parent-bol rust-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `rust-ts-mode'.")

(defvar rust-ts-mode--builtin-macros
  '("concat_bytes" "concat_idents" "const_format_args"
    "format_args_nl" "log_syntax" "trace_macros" "assert" "assert_eq"
    "assert_ne" "cfg" "column" "compile_error" "concat" "dbg"
    "debug_assert" "debug_assert_eq" "debug_assert_ne" "env" "eprint"
    "eprintln" "file" "format" "format_args" "include" "include_bytes"
    "include_str" "is_x86_feature_detected" "line" "matches"
    "module_path" "option_env" "panic" "print" "println" "stringify"
    "thread_local" "todo" "try" "unimplemented" "unreachable" "vec"
    "write" "writeln")
  "Rust built-in macros for tree-sitter font-locking.")

(defvar rust-ts-mode--keywords
  '("as" "async" "await" "break" "const" "continue" "dyn" "else"
    "enum" "extern" "fn" "for" "if" "impl" "in" "let" "loop" "match"
    "mod" "move" "pub" "ref" "return" "static" "struct" "trait" "type"
    "union" "unsafe" "use" "where" "while" (crate) (self) (super)
    (mutable_specifier))
  "Rust keywords for tree-sitter font-locking.")

(defvar rust-ts-mode--operators
  '("!"  "!=" "%" "%=" "&" "&=" "&&" "*" "*=" "+" "+=" "," "-" "-="
    "->" "."  ".."  "..=" "..."  "/" "/=" ":" ";" "<<" "<<=" "<" "<="
    "=" "==" "=>" ">" ">=" ">>" ">>=" "@" "^" "^=" "|" "|=" "||" "?")
  "Rust operators for tree-sitter font-locking.")

(defvar rust-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'rust
   :feature 'attribute
   '((attribute_item) @font-lock-constant-face
     (inner_attribute_item) @font-lock-constant-face)

   :language 'rust
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'rust
   :feature 'builtin
   `((macro_invocation
      macro: ((identifier) @font-lock-builtin-face
              (:match ,(rx-to-string
                        `(seq bol
                              (or ,@rust-ts-mode--builtin-macros)
                              eol))
                      @font-lock-builtin-face)))
     ((identifier) @font-lock-type-face
      (:match "^\\(:?Err\\|Ok\\|None\\|Some\\)$" @font-lock-type-face)))

   :language 'rust
   :feature 'comment
   '(([(block_comment) (line_comment)]) @font-lock-comment-face)

   :language 'rust
   :feature 'constant
   `((boolean_literal) @font-lock-constant-face
     ((identifier) @font-lock-constant-face
      (:match "^[A-Z][A-Z\\d_]*$" @font-lock-constant-face)))

   :language 'rust
   :feature 'delimiter
   '((["," "." ";" ":" "::"]) @font-lock-delimiter-face)

   :language 'rust
   :feature 'function
   '((call_expression
      function:
      [(identifier) @font-lock-function-name-face
       (field_expression
        field: (field_identifier) @font-lock-function-name-face)
       (scoped_identifier
        name: (identifier) @font-lock-function-name-face)])
     (function_item (identifier) @font-lock-function-name-face)
     (generic_function
      function: [(identifier) @font-lock-function-name-face
                 (field_expression
                  field: (field_identifier) @font-lock-function-name-face)
                 (scoped_identifier
                  name: (identifier) @font-lock-function-name-face)])
     (macro_definition "macro_rules!" @font-lock-constant-face)
     (macro_definition (identifier) @font-lock-preprocessor-face)
     (macro_invocation macro: (identifier) @font-lock-preprocessor-face))

   :language 'rust
   :feature 'keyword
   `([,@rust-ts-mode--keywords] @font-lock-keyword-face)

   :language 'rust
   :feature 'number
   '([(float_literal) (integer_literal)] @font-lock-number-face)

   :language 'rust
   :feature 'operator
   `([,@rust-ts-mode--operators] @font-lock-operator-face)

   :language 'rust
   :feature 'string
   '([(char_literal)
      (raw_string_literal)
      (string_literal)] @font-lock-string-face)

   :language 'rust
   :feature 'type
   `((call_expression
      function: (scoped_identifier
                 path: (identifier) @font-lock-type-face))
     (enum_variant name: (identifier) @font-lock-type-face)
     (match_arm
      pattern: (match_pattern (_ type: (identifier) @font-lock-type-face)))
     (match_arm
      pattern: (match_pattern
                (_ type: (scoped_identifier
                          path: (identifier) @font-lock-type-face))))
     (mod_item name: (identifier) @font-lock-constant-face)
     (primitive_type) @font-lock-type-face
     (type_identifier) @font-lock-type-face
     (scoped_identifier name: (identifier) @font-lock-type-face)
     (scoped_identifier path: (identifier) @font-lock-constant-face)
     (scoped_identifier
      (scoped_identifier
       path: (identifier) @font-lock-constant-face))
     ((scoped_identifier
       path: [(identifier) @font-lock-type-face
              (scoped_identifier
               name: (identifier) @font-lock-type-face)])
      (:match "^[A-Z]" @font-lock-type-face))
     (scoped_type_identifier path: (identifier) @font-lock-constant-face)
     (scoped_use_list
      path: [(identifier) @font-lock-constant-face
             (scoped_identifier (identifier) @font-lock-constant-face)])
     (type_identifier) @font-lock-type-face
     (use_as_clause alias: (identifier) @font-lock-type-face)
     (use_list (identifier) @font-lock-type-face))

   :language 'rust
   :feature 'variable
   '((identifier) @font-lock-variable-name-face
     ;; Everything in a token_tree is an identifier.
     (token_tree (identifier) @default))

   :language 'rust
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'rust
   :feature 'property
   :override t
   '((field_identifier) @font-lock-property-face
     (shorthand_field_initializer (identifier) @font-lock-property-face))

   :language 'rust
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `rust-ts-mode'.")

(defun rust-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (enum-tree (treesit-induce-sparse-tree
                     node "enum_item" nil))
         (enum-index (rust-ts-mode--imenu-1 enum-tree))
         (func-tree (treesit-induce-sparse-tree
                     node "function_item" nil))
         (func-index (rust-ts-mode--imenu-1 func-tree))
         (impl-tree (treesit-induce-sparse-tree
                     node "impl_item" nil))
         (impl-index (rust-ts-mode--imenu-1 impl-tree))
         (mod-tree (treesit-induce-sparse-tree
                    node "mod_item" nil))
         (mod-index (rust-ts-mode--imenu-1 mod-tree))
         (struct-tree (treesit-induce-sparse-tree
                       node "struct_item" nil))
         (struct-index (rust-ts-mode--imenu-1 struct-tree))
         (type-tree (treesit-induce-sparse-tree
                     node "type_item" nil))
         (type-index (rust-ts-mode--imenu-1 type-tree)))
    (append
     (when mod-index `(("Module" . ,mod-index)))
     (when enum-index `(("Enum" . ,enum-index)))
     (when impl-index `(("Impl" . ,impl-index)))
     (when type-index `(("Type" . ,type-index)))
     (when struct-index `(("Struct" . ,struct-index)))
     (when func-index `(("Fn" . ,func-index))))))

(defun rust-ts-mode--imenu-1 (node)
  "Helper for `rust-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'rust-ts-mode--imenu-1
                           children))
         (name (when ts-node
                 (pcase (treesit-node-type ts-node)
                   ("enum_item"
                    (treesit-node-text
                     (treesit-node-child-by-field-name ts-node "name") t))
                   ("function_item"
                    (treesit-node-text
                     (treesit-node-child-by-field-name ts-node "name") t))
                   ("impl_item"
                    (let ((trait-node (treesit-node-child-by-field-name ts-node "trait")))
                      (concat
                       (treesit-node-text
                        trait-node t)
                       (when trait-node
                         " for ")
                       (treesit-node-text
                        (treesit-node-child-by-field-name ts-node "type") t))))
                   ("mod_item"
                    (treesit-node-text
                     (treesit-node-child-by-field-name ts-node "name") t))
                   ("struct_item"
                    (treesit-node-text
                     (treesit-node-child-by-field-name ts-node "name") t))
                   ("type_item"
                    (treesit-node-text
                     (treesit-node-child-by-field-name ts-node "name") t)))))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((or (null ts-node) (null name)) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;;;###autoload
(define-derived-mode rust-ts-mode prog-mode "Rust"
  "Major mode for editing Rust, powered by tree-sitter."
  :group 'rust
  :syntax-table rust-ts-mode--syntax-table

  (when (treesit-ready-p 'rust)
    (treesit-parser-create 'rust)

    ;; Comments.
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx (or (seq "/" (+ "/"))
                                           (seq "/" (+ "*")))
                                       (* (syntax whitespace))))
    (setq-local comment-end-skip
                (rx (* (syntax whitespace))
                    (group (or (syntax comment-end)
                               (seq (+ "*") "/")))))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings rust-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment)
                  ( keyword string)
                  ( attribute builtin constant escape-sequence
                    function number property type variable)
                  ( bracket delimiter error operator)))

    ;; Imenu.
    (setq-local imenu-create-index-function #'rust-ts-mode--imenu)
    (setq-local which-func-functions nil)

    ;; Indent.
    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules rust-ts-mode--indent-rules)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("enum_item"
                              "function_item"
                              "impl_item"
                              "struct_item")))

    (treesit-major-mode-setup)))

(provide 'rust-ts-mode)

;;; rust-ts-mode.el ends here
