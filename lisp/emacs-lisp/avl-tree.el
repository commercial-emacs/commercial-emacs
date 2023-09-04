;;; avl-tree.el --- balanced binary trees, AVL-trees  -*- lexical-binding:t -*-

;; Copyright (C) 1995, 2007-2023 Free Software Foundation, Inc.

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;;         Inge Wallin <inge@lysator.liu.se>
;;         Thomas Bellman <bellman@lysator.liu.se>
;;         Toby Cubitt <toby-predictive@dr-qubit.org>
;; Maintainer: emacs-devel@gnu.org
;; Created: 10 May 1991
;; Keywords: extensions, data structures, AVL, tree

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

;;  This provides avl-tree-core plus an adaptor for the iterator
;;  interface.  Requiring the generator library also requires cl-lib,
;;  which is not allowed in loadup.el

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'avl-tree-core)
(require 'generator)



(iter-defun avl-tree-iter (tree &optional reverse)
  "Return an AVL tree iterator object.

Calling `iter-next' on this object will retrieve the next element
from TREE.  If REVERSE is non-nil, elements are returned in
reverse order.

Note that any modification to TREE *immediately* invalidates all
iterators created from TREE before the modification (in
particular, calling `iter-next' will give unpredictable results)."
  (let ((stack (avl-tree-stack tree reverse)))
    (while (not (avl-tree-stack-empty-p stack))
      (iter-yield (avl-tree-stack-pop stack)))))


(provide 'avl-tree)

;;; avl-tree.el ends here
