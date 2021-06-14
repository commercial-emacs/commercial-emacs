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
       (let* ((default-directory (if-let ((lib (locate-library "gnus-tests")))
                                     (file-name-directory lib)
                                   default-directory))
	      (user-emacs-directory default-directory))
         (gnus-tests-let-customs
           ((gnus-verbose 8)
            (gnus-save-newsrc-file nil)
            (gnus-home-directory default-directory)
            (gnus-use-dribble-file nil)
            (network-security-level (quote low))
            (gnus-interactive-exit (quote quiet))
            (gnus-select-methods (quote ((nnfolder "")))))
	   (progn ,@body)))
     t))

(ert-deftest gnus-test-stub () (should t))

(provide 'gnus-tests)
;;; gnus-tests.el ends here
