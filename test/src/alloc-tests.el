;;; alloc-tests.el --- alloc tests -*- lexical-binding: t -*-

;; Copyright (C) 2015-2023 Free Software Foundation, Inc.

;; Author: Daniel Colascione <dancol@dancol.org>
;; Keywords:

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

;;

;;; Code:

(require 'ert)
(require 'cl-lib)

(ert-deftest finalizer-object-type ()
  (should (equal (type-of (make-finalizer #'ignore)) 'finalizer)))

(ert-deftest record-1 ()
  (let ((x (record 'foo 1 2 3)))
    (should (recordp x))
    (should (eq (type-of x) 'foo))
    (should (eq (aref x 0) 'foo))
    (should (eql (aref x 3) 3))
    (should (eql (length x) 4))))

(ert-deftest record-2 ()
  (let ((x (make-record 'bar 1 0)))
    (should (eql (length x) 2))
    (should (eql (aref x 1) 0))))

(ert-deftest record-3 ()
  (let* ((x (record 'foo 1 2 3))
         (y (copy-sequence x)))
    (should-not (eq x y))
    (dotimes (i 4)
      (should (eql (aref x i) (aref y i))))))

;; Bug#39207
(ert-deftest aset-nbytes-change ()
  (let ((s (make-string 1 ?a)))
    (dolist (c (list 10003 ?b 128 ?c ?d (max-char) ?e))
      (aset s 0 c)
      (should (equal s (make-string 1 c))))))

(ert-deftest flood-mgc-strings ()
  "Should error but not crash."
  (should-error
   (mgc-make-string 100 "not-a-numeric")))

(ert-deftest flip-one-vector ()
  (let* ((gc-cons-threshold most-positive-fixnum)
         (ocount (alist-get 'vectors (mgc-counts)))
         (bar (mgc-vector 1)))
    (ignore bar)
    (should (= (1+ ocount) (alist-get 'vectors (mgc-counts))))
    (garbage-collect)
    (should (= (1+ ocount) (alist-get 'vectors (mgc-counts))))))

(ert-deftest flip-one-cons ()
  (let* ((gc-cons-threshold most-positive-fixnum)
         (ocount (alist-get 'conses (mgc-counts)))
         (bar (mgc-cons 1 2)))
    (ignore bar)
    (should (= (1+ ocount) (alist-get 'conses (mgc-counts))))
    (garbage-collect)
    (should (= (1+ ocount) (alist-get 'conses (mgc-counts))))))

(ert-deftest flip-one-symbol ()
  (let* ((gc-cons-threshold most-positive-fixnum)
         (ocount (alist-get 'symbols (mgc-counts)))
         (bar (mgc-make-symbol "hisfooness")))
    (ignore bar)
    (should (= (1+ ocount) (alist-get 'symbols (mgc-counts))))
    (garbage-collect)
    (should (= (1+ ocount) (alist-get 'symbols (mgc-counts))))))

(ert-deftest flip-one-float ()
  (let* ((gc-cons-threshold most-positive-fixnum)
         (ocount (alist-get 'floats (mgc-counts)))
         (bar (mgc-float -0.618)))
    (ignore bar)
    (should (= (1+ ocount) (alist-get 'floats (mgc-counts))))
    (garbage-collect)
    (should (= (1+ ocount) (alist-get 'floats (mgc-counts))))))

;;; alloc-tests.el ends here
