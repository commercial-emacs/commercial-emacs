;;; json-ts-mode.el --- tree-sitter support for JSON  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : November 2022
;; Keywords   : json languages tree-sitter

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'treesit)
(require 'rx)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")


(defcustom json-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `json-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'json)

(defvar json-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the cc-langs version
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?$ "_"      table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?` "\""     table)
    (modify-syntax-entry ?\240 "."   table)
    table)
  "Syntax table for `json-ts-mode'.")


(defvar json-ts--indent-rules
  `((json
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is "object") parent-bol json-ts-mode-indent-offset))))

(defvar json-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'json
   :feature 'comment
   :override t
   '((comment) @font-lock-comment-face)
   :language 'json
   :feature 'string
   :override t
   '((escape_sequence) @font-lock-constant-face
     (string) @font-lock-string-face)
   :language 'json
   :feature 'number
   :override t
   '((number) @font-lock-constant-face)
   :language 'json
   :feature 'constant
   :override t
   '([(null) (true) (false)] @font-lock-constant-face)
   :language 'json
   :feature 'pair
   :override t
   `((pair key: (_) @font-lock-variable-name-face)))
  "Font-lock settings for JSON.")

(defun json-ts-mode--imenu-1 (node)
  "Helper for `json-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (subtrees (mapcan #'json-ts-mode--imenu-1 (cdr node)))
         (name (when ts-node
                 (treesit-node-text
                  (treesit-node-child-by-field-name
                   ts-node "key")
                  t)))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((null ts-node) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

(defun json-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (tree (treesit-induce-sparse-tree
                node "pair" nil 1000)))
    (json-ts-mode--imenu-1 tree)))

;;;###autoload
(define-derived-mode json-ts-mode prog-mode "JSON"
  "Major mode for editing JSON, powered by tree-sitter."
  :group 'json
  :syntax-table json-ts-mode--syntax-table

  (unless (treesit-ready-p 'json)
    (error "Tree-sitter for JSON isn't available"))

  (treesit-parser-create 'json)

  ;; Comments.
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Indent.
  (setq-local treesit-simple-indent-rules json-ts--indent-rules)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (rx (or "pair" "object")))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings json-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment string number) (constant pair) ()))

  ;; Imenu.
  (setq-local imenu-create-index-function #'json-ts-mode--imenu)
  (setq-local which-func-functions nil) ;; Piggyback on imenu

  (treesit-major-mode-setup))

(provide 'json-ts-mode)

;;; json-ts-mode.el ends here
