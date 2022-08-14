;;;tree-sitter-prog-mode.el --- Tree Sitter enabled prog-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2022 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

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

;; This major mode is mostly intended as a parent of other programming
;; modes.  All major modes for programming languages should derive from this
;; mode so that users can put generic customization on prog-mode-hook.

;;; Code:

(declare-function tree-sitter "tree-sitter.c")

(define-derived-mode tree-sitter-prog-mode prog-mode "TreeSitter"
  "Tree-sitter enabled major mode."
  :group 'prog-mode
  :interactive nil ;; this precludes autoloadability
  (if (not (fboundp 'tree-sitter))
      (error "Executable not built with tree sitter support.")
    ;; Base `fundamental-mode' will `kill-all-local-variables',
    ;; so rest assured these won't linger after major mode change.
    (setq-local font-lock-support-mode 'tree-sitter-lock-mode
                forward-sexp-function #'tree-sitter-forward-sexp
                beginning-of-defun-function #'tree-sitter-beginning-of-defun
                end-of-defun-function #'tree-sitter-end-of-defun)
    (tree-sitter)))

(provide 'tree-sitter-prog-mode)

;;; tree-sitter-prog-mode.el ends here
