;;; gnus-test-select-methods.el    -*- lexical-binding:t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'gnus)
(require 'gnus-int)
(require 'gnus-start)

(eval-when-compile
  (put 'gnus-secondary-select-methods 'byte-obsolete-variable nil)
  (put 'gnus-select-method 'byte-obsolete-variable nil)
  (put 'gnus-nntp-server 'byte-obsolete-variable nil))

(ert-deftest gnus-test-select-methods-basic ()
  "Customizing `gnus-select-method' and `gnus-secondary-select-methods'
also modifies `gnus-select-methods'."
  (let (gnus-select-method
        gnus-secondary-select-methods
        gnus-select-methods
        (test-methods '((nnnil) (nntp "flab.flab.edu"))))
    (custom-set-variables `(gnus-select-method (quote ,(car test-methods)))
                          `(gnus-secondary-select-methods (quote ,(cdr test-methods))))
    (should (cl-every #'identity
                      (cl-mapcar #'gnus-methods-equal-p gnus-select-methods test-methods)))
    (should (gnus-method-equal gnus-select-method (car gnus-select-methods)))
    (should (cl-every #'identity
                      (cl-mapcar #'gnus-methods-equal-p gnus-secondary-select-methods
                                 (cdr gnus-select-methods))))))

(ert-deftest gnus-test-select-methods-override ()
  "Customizing `gnus-select-methods' overrides earlier customizations
of `gnus-select-method' and `gnus-secondary-select-methods'."
  (let (gnus-select-method
        gnus-secondary-select-methods
        gnus-select-methods
        (test-methods '((nnnil) (nntp "flab.flab.edu")))
        (override-methods '((nntp "override") (nnnil))))
    (custom-set-variables `(gnus-select-method (quote ,(car test-methods)))
                          `(gnus-secondary-select-methods (quote ,(cdr test-methods)))
                          `(gnus-select-methods (quote ,override-methods)))
    (should (cl-every #'identity
                      (cl-mapcar #'gnus-methods-equal-p gnus-select-methods override-methods)))
    (should (gnus-method-equal gnus-select-method (car gnus-select-methods)))
    (should (cl-every #'identity
                      (cl-mapcar #'gnus-methods-equal-p gnus-secondary-select-methods
                                 (cdr gnus-select-methods))))))

(ert-deftest gnus-test-gnus-start-news-server ()
  "Test an archaic method of initiating gnus."
  (let (gnus-current-select-method
        (gnus-nntp-server "::"))
    (cl-letf (((symbol-function 'gnus-y-or-n-p) #'ignore))
      (gnus-start-news-server)
      (should (gnus-method-equal gnus-select-method `(nnspool ,(system-name)))))))

(ert-deftest gnus-test-gnus-method-rank ()
  "Ensure unification does right by `gnus-method-rank'."
  (let (gnus-select-method
        gnus-secondary-select-methods
        gnus-select-methods
        type-cache
        (test-methods '((nnnil) (nntp "flab.flab.edu")))
        (sort-f (lambda (c1 c2)
                  (< (gnus-method-rank (cadr c1) (car c1))
                     (gnus-method-rank (cadr c2) (car c2))))))
    (custom-set-variables `(gnus-select-methods (quote ,test-methods)))
    (dolist (method test-methods)
      (push `(,method
              ,(cond
                ((gnus-secondary-method-p method) 'secondary)
                ((gnus-method-equal gnus-select-method method) 'primary)
                (t 'foreign)))
            type-cache))
    (equal '(nnnil nntp)
           (mapcar (lambda (x) (car (car x))) (sort type-cache sort-f)))))

(ert-deftest gnus-test-gnus-read-active-file ()
  "Ensure unification does right by `gnus-read-active-file'."
  (let (gnus-select-method
        gnus-secondary-select-methods
        gnus-select-methods
        (test-methods '((nnnil) (nntp "flab.flab.edu"))))
    (custom-set-variables `(gnus-select-methods (quote ,test-methods)))
    (should (equal
             (cl-remove-if (lambda (method)
                             (gnus-method-equal method gnus-select-method))
                           gnus-select-methods)
             gnus-secondary-select-methods))))

;;; gnus-test-select-methods.el ends here
