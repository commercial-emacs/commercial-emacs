;;; gnus-tests.el --- Wrapper for the Gnus tests  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2021 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>

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

;; This file should contain nothing but requires for all the Gnus
;; tests that are not standalone.

;;; Code:
;;

(require 'cl-macs)
(defmacro gnus-tests-let-customs (bindings &rest forms)
  (declare (indent defun))
  `(let (,@(mapcar #'car bindings))
     (funcall #'custom-set-variables
              ,@(mapcar (apply-partially #'list 'quote) bindings))
     ,@forms))

(cl-defmacro gnus-tests-doit (&rest body &key select-methods &allow-other-keys)
  (declare (indent defun))
  `(unwind-protect
       (let* ((load-file-name "/home/dick/emacs")
	      (default-directory elpaso-defs-toplevel-dir)
	      (user-emacs-directory default-directory)
	      (package-user-dir (locate-user-emacs-file "elpa"))
	      use-package-ensure-function
	      elpaso-admin--cookbooks-alist
	      elpaso-defs-install-dir
	      elpaso-admin-cookbooks
	      package-alist
	      package-activated-list
	      package-archives
	      package-archive-contents
	      (package-directory-list
	       (eval (car (get 'package-directory-list 'standard-value))))
	      (package-load-list
	       (eval (car (get 'package-load-list 'standard-value))))
	      (package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)))
         (gnus-tests-let-customs
           ((gnus-verbose 8)
            (gnus-save-newsrc-file nil)
            (gnus-home-directory (file-name-directory load-file-name))
            (gnus-use-dribble-file nil)
            (network-security-level (quote low))
            (gnus-interactive-exit (quote quiet))
            (gnus-select-methods (quote ((nnfolder "")))))
           gnus-home-directory)

	 (customize-set-variable 'elpaso-defs-install-dir (locate-user-emacs-file "elpaso"))
	 (customize-set-variable 'use-package-ensure-function
				 'elpaso-use-package-ensure-function)
	 (delete-directory package-user-dir t)
	 (make-directory package-user-dir t)
	 (delete-directory elpaso-admin--build-dir t)
	 (delete-directory elpaso-admin--recipes-dir t)
	 (customize-set-variable 'elpaso-admin--cookbooks-alist
				 '((user :file "recipes")
				   (rtest :url "mockhub.com/recipes.git" :file "recipes")))
	 (customize-set-variable 'elpaso-admin-cookbooks '(user rtest))
	 (elpaso-refresh)
	 (should (file-readable-p
		  (elpaso-admin--sling elpaso-admin--recipes-dir "rtest/recipes")))
	 (dolist (spec ,specs)
	   (elpaso-admin-add-recipe (intern (car spec)) (cdr spec))
	   (should (member spec (elpaso-admin--get-specs))))
	 (progn ,@body))
     t))

(ert-deftest gnus-test-stub () (should t))

(provide 'gnus-tests)
;;; gnus-tests.el ends here
