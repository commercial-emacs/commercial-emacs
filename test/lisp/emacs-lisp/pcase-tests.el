;;; pcase-tests.el --- Test suite for pcase macro.  -*- lexical-binding:t -*-

;; Copyright (C) 2012-2023 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'cl-lib)

(ert-deftest pcase-tests-base ()
  "Test pcase code."
  (should (equal (pcase '(1 . 2) ((app car '2) 6) ((app car '1) 5)) 5)))

(ert-deftest pcase-tests-bugs ()
  (should (equal (pcase '(2 . 3)        ;bug#18554
                   (`(,hd . ,(and (pred atom) tl)) (list hd tl))
                   ((pred consp) nil))
                 '(2 3)))
  (should (equal (pcase '(2 . 3)
                   (`(,hd . ,(and (pred (not consp)) tl)) (list hd tl))
                   ((pred consp) nil))
                 '(2 3))))

(pcase-defmacro pcase-tests-plus (pat n)
  `(app (lambda (v) (- v ,n)) ,pat))

(ert-deftest pcase-tests-macro ()
  (should (equal (pcase 5 ((pcase-tests-plus x 3) x)) 2)))

(defun pcase-tests-grep (fname exp)
  (when (consp exp)
    (or (eq fname (car exp))
        (cl-some (lambda (exp) (pcase-tests-grep fname exp)) (cdr exp)))))

(ert-deftest pcase-tests-tests ()
  (should (pcase-tests-grep 'memq '(or (+ 2 3) (memq x y))))
  (should-not (pcase-tests-grep 'memq '(or (+ 2 3) (- x y)))))

(ert-deftest pcase-tests-member ()
  (should (pcase-tests-grep
           'memq (macroexpand-all '(pcase x ((or 'a 'b 'c) body)))))
  (should (pcase-tests-grep
           'memql (macroexpand-all '(pcase x ((or 1 2 3 'a) body)))))
  (should (pcase-tests-grep
           'member (macroexpand-all '(pcase x ((or "a" 2 3 'a) body)))))
  (should-not (pcase-tests-grep
               'memq (macroexpand-all '(pcase x ((or "a" 2 3) body)))))
  (should-not (pcase-tests-grep
               'memql (macroexpand-all '(pcase x ((or "a" 2 3) body)))))
  (let ((exp (macroexpand-all
                      '(pcase x
                         ("a" body1)
                         (2 body2)
                         ((or "a" 2 3) body)))))
    (should-not (pcase-tests-grep 'memq exp))
    (should-not (pcase-tests-grep 'member exp))))

(ert-deftest pcase-tests-vectors ()
  (should (equal (pcase [1 2] (`[,x] 1) (`[,x ,y] (+ x y))) 3)))

(ert-deftest pcase-tests-bug14773 ()
  (let ((f (lambda (x)
             (pcase 'dummy
               ((and (let var x) (guard var)) 'left)
               ((and (let var (not x)) (guard var)) 'right)))))
    (should (equal (funcall f t) 'left))
    (should (equal (funcall f nil) 'right))))

(ert-deftest pcase-tests-bug46786 ()
  (let ((self 'outer))
    (ignore self)
    (should (equal (cl-macrolet ((show-self () `(list 'self self)))
                     (pcase-let ((`(,self ,_self2) '(inner "2")))
                       (show-self)))
                   '(self inner)))))

(ert-deftest pcase-tests-or-vars ()
  (let ((f (lambda (v)
             (pcase v
               ((or (and 'b1 (let x1 4) (let x2 5))
                    (and 'b2 (let y1 8) (let y2 9)))
                (list x1 x2 y1 y2))))))
    (should (equal (funcall f 'b1) '(4 5 nil nil)))
    (should (equal (funcall f 'b2) '(nil nil 8 9)))))

(ert-deftest pcase-tests-cl-type ()
  (should (equal (pcase 1
                   ((cl-type integer) 'integer))
                 'integer))
  (should (equal (pcase 1
                   ((cl-type (integer 0 2)) 'integer-0<=n<=2))
                 'integer-0<=n<=2))
  (should-error
   ;; Avoid error at compile time due to compiler macro.
   (eval '(pcase 1
            ((cl-type notatype) 'integer))
         t)))

(ert-deftest pcase-tests-setq ()
  (should (equal (let (a b)
                   (pcase-setq `((,a) (,b)) '((1) (2)))
                   (list a b))
                 (list 1 2)))

  (should (equal (list nil nil)
                 (let ((a 'unset)
                       (b 'unset))
                   (pcase-setq `(head ,a ,b) nil)
                   (list a b))))

  (should (equal (let (a b)
                   (pcase-setq `[,a ,b] [1 2])
                   (list a b))
                 '(1 2)))

  (should-error (let (a b)
                  (pcase-setq `[,a ,b] nil)
                  (list a b)))

  (should (equal (let (a b)
                   (pcase-setq a 1 b 2)
                   (list a b))
                 '(1 2)))

  (should (= (let (a)
               (pcase-setq a 1 `(,a) '(2))
               a)
             2))

  (should (equal (let (array list-item array-copy)
                   (pcase-setq (or `(,list-item) array) [1 2 3]
                               array-copy array
                               ;; This re-sets `array' to nil.
                               (or `(,list-item) array) '(4))
                   (list array array-copy list-item))
                 '(nil [1 2 3] 4)))

  (let ((a nil))
    (should-error (pcase-setq a 1 b)
                  :type '(wrong-number-of-arguments))
    (should (eq a nil)))

  (should-error (pcase-setq a)
                :type '(wrong-number-of-arguments)))

;;; Tests for the `cl-lambda' `pcase' pattern.

(ert-deftest pcase-tests-cl-lambda-&whole-should-error ()
  "`&whole' must come first if given, and must be followed by a patter."
  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&whole))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (a b &whole c))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&rest a &whole c))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&key a &whole c))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&aux (a 1) &whole c))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&optional (a 1) &whole c))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&whole whole1 &whole whole2))
                   (list whole1 whole2)))
                :type 'cl--pcase-lambda-list-bad-lambda-list))

(ert-deftest pcase-tests-cl-lambda-&whole ()
  "`&whole' can be a `pcase' pattern."
  (should (equal (list (list 1 2 3) 1 2 3)
                 (pcase (list 1 2 3)
                   ((cl-lambda (&whole whole a b c))
                    (list whole a b c)))))

  (should (equal (list 1 2 3 1 2 3)
                 (pcase (list 1 2 3)
                   ((cl-lambda (&whole `(,a0 ,b0 ,c0) a b c))
                    (list a0 b0 c0 a b c))))))

(ert-deftest pcase-tests-cl-lambda-pos ()
  "Positional variables must match the length of EXPVAL."
  (should (equal (list 1 2 3)
                 (pcase (list 1 2 3)
                   ((cl-lambda (a b c))
                    (list a b c)))))

  (should (equal nil
                 (pcase (list (list 1))
                   ((cl-lambda (a b))
                    (list a b)))))

  (should (equal nil
                 (pcase (list (list 1 2 3))
                   ((cl-lambda (a b))
                    (list a b))))))

(ert-deftest pcase-tests-cl-lambda-pos-sub-patterns ()
  (should (equal (list 1 2 3 4)
                 (pcase (list 1 2 (list 3 4))
                   ((cl-lambda (a b (cl-lambda (c d))))
                    (list a b c d)))))

  (should (equal (list 1 2)
                 (pcase (list (list 1 2))
                   ((cl-lambda (`(,a ,b)))
                    (list a b))))))

(ert-deftest pcase-tests-cl-lambda-&optional-should-error ()
  "`&optional' cannot be used after `&optional', `&rest', `&key', and `&aux'."
  (should-error (equal (list 1 2 3)
                       (pcase (list 1 2 3)
                         ((cl-lambda (&rest a &optional b c))
                          (list a b c))))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&body a &optional b c))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&key a &optional b c))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&allow-other-keys &optional b c))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&aux (a 1) &optional b c))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&optional a &optional b c))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list))

(ert-deftest pcase-tests-cl-lambda-&optional ()
  (should (equal (list 1 2 3)
                 (pcase (list 1 2 3)
                   ((cl-lambda (a b &optional c))
                    (list a b c)))))

  (should (equal (list 1 2 nil)
                 (pcase (list 1 2)
                   ((cl-lambda (a b &optional c))
                    (list a b c)))))

  (should (equal (list 1 2 13)
                 (pcase (list 1 2)
                   ((cl-lambda (a b &optional (c 13)))
                    (list a b c)))))

  (should (equal (list 1 2 13 nil)
                 (pcase (list 1 2)
                   ((cl-lambda (a b &optional (c 13 c-supplied)))
                    (list a b c c-supplied)))))

  (should (equal (list 1 2 3 t)
                 (pcase (list 1 2 3)
                   ((cl-lambda (a b &optional (c 13 c-supplied)))
                    (list a b c c-supplied))))))

(ert-deftest pcase-tests-cl-lambda-&optional-sub-patterns ()
  "Test using sub-patterns in `cl-lambda' pattern.
Sub-patterns must be contained within a sub-list, since a sub-list
also provides a default value."
  (should-error (pcase (list 1 2 (list 3 4))
                  ((cl-lambda (a b &optional `(,c ,d)))
                   (list a b c d))))

  (should (equal (list 1 2 33)
                 (pcase (list 1 2)
                   ((cl-lambda (a b &optional ((and opt1
                                                    (guard (numberp opt1)))
                                               33)))
                    (list a b opt1)))))

  (should (equal nil
                 (pcase (list 1 2 'not-num)
                   ((cl-lambda (a b &optional ((and opt1
                                                    (guard (numberp opt1)))
                                               33)))
                    (list a b opt1)))))

  (should (equal nil
                 (pcase (list 1 2 nil)
                   ((cl-lambda (a b &optional ((and opt1
                                                    (guard (numberp opt1)))
                                               'not-num)))
                    (list a b opt1)))))

  (should (equal (list 1 2 3 4)
                 (pcase (list 1 2 (list 3 4))
                   ((cl-lambda (a b &optional (`(,c ,d))))
                    (list a b c d)))))

  (should (equal (list 1 2 nil nil)
                 (pcase (list 1 2)
                   ((cl-lambda (a b &optional (`(,c ,d))))
                    (list a b c d)))))

  (should (equal (list 1 2 13 14)
                 (pcase (list 1 2)
                   ((cl-lambda (a b &optional (`(,c ,d) (list 13 14))))
                    (list a b c d)))))

  (should (equal (list 1 2 13 14)
                 (pcase (list 1 2)
                   ((cl-lambda ( a b
                                 &optional ((cl-lambda (c &optional (d 14)))
                                            (list 13))))
                    (list a b c d)))))

  (should (equal (list 1 2 13 14 nil)
                 (pcase (list 1 2)
                   ((cl-lambda (a b &optional (`(,c ,d) (list 13 14) cd-supplied)))
                    (list a b c d cd-supplied)))))

  (should (equal (list 1 2 13 14 nil t nil)
                 (pcase (list 1 2)
                   ((cl-lambda ( a b
                                 &optional
                                 ((cl-lambda (&optional (c 27 c-sub-sup)
                                                        (d 14 d-sub-sup)))
                                  (list 13)
                                  cd-supplied)))
                    (list a b c d cd-supplied c-sub-sup d-sub-sup))))))

(ert-deftest pcase-tests-cl-lambda-&rest-should-error ()
  "`&rest' (`&body', `.') cannot be used after `&rest', `&body', `&key',and `&aux'."
  (should-error (equal (list 1 2 3)
                       (pcase (list 1 2 3)
                         ((cl-lambda (&rest a &rest b))
                          (list a b c))))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (equal (list 1 2 3)
                       (pcase (list 1 2 3)
                         ((cl-lambda (&body a &body b))
                          (list a b c))))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (equal (list 1 2 3)
                       (pcase (list 1 2 3)
                         ((cl-lambda (&body a . b))
                          (list a b c))))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&body a &rest b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&rest a &body b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&key a &rest b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&key a &body b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&key a . b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&allow-other-keys &rest b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&allow-other-keys &body b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&allow-other-keys . b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&aux (a 1) &rest b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&aux (a 1) &body b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list 1 2 3)
                  ((cl-lambda (&aux (a 1) . b))
                   (list a b c)))
                :type 'cl--pcase-lambda-list-bad-lambda-list))

(ert-deftest pcase-tests-cl-lambda-&rest-nonlist-cdr ()
  (should (equal (list 1 2)
                 (pcase (cons 1 2)
                   ((cl-lambda (a &rest b))
                    (list a b)))))

  (should (equal (list 1 2)
                 (pcase (cons 1 2)
                   ((cl-lambda (a &body b))
                    (list a b)))))

  (should (equal (list 1 2)
                 (pcase (cons 1 2)
                   ((cl-lambda (a . b))
                    (list a b))))))

(ert-deftest pcase-tests-cl-lambda-&rest-with-&whole ()
  (should (equal (list (cons 1 2) 1 2)
                 (pcase (cons 1 2)
                   ((cl-lambda (&whole whole a &rest b))
                    (list whole a b)))))

  (should (equal (list (cons 1 2) 1 2)
                 (pcase (cons 1 2)
                   ((cl-lambda (&whole whole a &body b))
                    (list whole a b)))))

  (should (equal (list (cons 1 2) 1 2)
                 (pcase (cons 1 2)
                   ((cl-lambda (&whole whole a . b))
                    (list whole a b))))))

(ert-deftest pcase-tests-cl-lambda-&rest-only ()
  "Using only `&rest' should work like `&whole'."
  (should (equal (list (list 1 2))
                 (pcase (list 1 2)
                   ((cl-lambda (&rest a))
                    (list a)))))

  (should (equal (list (cons 1 2))
                 (pcase (cons 1 2)
                   ((cl-lambda (&body a))
                    (list a))))))

(ert-deftest pcase-tests-cl-lambda-&rest-after-&optional ()
  (should (equal (list 1 2 3 (list 4 5))
                 (pcase (list 1 2 3 4 5)
                   ((cl-lambda (&optional a b c &rest d))
                    (list a b c d)))))

  (should (equal (list 1 2 3 (list 4 5))
                 (pcase (list 1 2 3 4 5)
                   ((cl-lambda (&optional a b c &body d))
                    (list a b c d)))))

  (should (equal (list 1 2 3 (list 4 5))
                 (pcase (list 1 2 3 4 5)
                   ((cl-lambda (&optional a b c . d))
                    (list a b c d))))))

(ert-deftest pcase-tests-cl-lambda-&rest-sub-patterns ()
  ;; We can't do (a . `(,b . ,c)), so we don't test that.
  (should (equal (list 1 2 3)
                 (pcase (list 1 2 3)
                   ((cl-lambda (a &rest (cl-lambda (b c))))
                    (list a b c)))))

  (should (equal (list 1 2 3)
                 (pcase (list 1 2 3)
                   ((cl-lambda (a &body `(,b ,c)))
                    (list a b c))))))

(ert-deftest pcase-tests-cl-lambda-&key-should-error ()
  "`&key' cannot be used after `&key', `&allow-other-keys', and `&aux'."
  (should-error (pcase (list :a 1 :b 2)
                  ((cl-lambda (&key a &key b))
                   (list a b)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list :a 1 :b 2)
                  ((cl-lambda (&aux (a 1) &key b))
                   (list a b)))
                :type 'cl--pcase-lambda-list-bad-lambda-list)

  (should-error (pcase (list :a 1 :b 2)
                  ((cl-lambda (&allow-other-keys &key b))
                   (list a b)))
                :type 'cl--pcase-lambda-list-bad-lambda-list))

(ert-deftest pcase-tests-cl-lambda-&key-exact ()
  "`&key' doesn't match unspecified keys unless `&allow-other-keys' or `:allow-other-keys' is given."
  (should (equal (list 1 2)
                 (pcase (list :a 1 :b 2)
                   ((cl-lambda (&key a b))
                    (list a b)))))

  (should (equal nil
                 (pcase (list :a 1 :b 2 :c 3)
                   ((cl-lambda (&key a b))
                    (list a b)))))

  (should (equal (list 1 2 nil)
                 (pcase (list :a 1 :b 2)
                   ((cl-lambda (&key a b c))
                    (list a b c))))))

(ert-deftest pcase-tests-cl-lambda-&key-permissive ()
  "`&key' doesn't match unspecified keys unless `&allow-other-keys' or `:allow-other-keys' is given."
  (should (equal (list 1 2)
                 (pcase (list :a 1 :b 2 :c 3)
                   ((cl-lambda (&key a b &allow-other-keys))
                    (list a b)))))

  (should (equal (list 1 2)
                 (pcase (list :a 1 :b 2 :c 3 :allow-other-keys t)
                   ((cl-lambda (&key a b))
                    (list a b))))))

(ert-deftest pcase-tests-cl-lambda-&key-not-first ()
  "The plist should be after positional values and equal to `&rest'."
  (should (equal (list 1 2 3 11 22)
                 (pcase (list 1 2 3 :k1 11 :k2 22)
                   ((cl-lambda (a b c &key k1 k2))
                    (list a b c k1 k2)))))

  (should (equal (list 1 2 3 (list :k1 11 :k2 22) 11 22)
                 (pcase (list 1 2 3 :k1 11 :k2 22)
                   ((cl-lambda (a b c &rest r1 &key k1 k2))
                    (list a b c r1 k1 k2))))))

(ert-deftest pcase-tests-cl-lambda-&key-full-form ()
  (should (equal (list 1 2)
                 (pcase (list :a 1 :b 2)
                   ((cl-lambda (&key a (b 13)))
                    (list a b)))))

  (should (equal (list 1 13)
                 (pcase (list :a 1)
                   ((cl-lambda (&key a (b 13)))
                    (list a b)))))

  (should (equal (list 1 13 nil)
                 (pcase (list :a 1)
                   ((cl-lambda (&key a (b 13 b-supplied)))
                    (list a b b-supplied)))))

  (should (equal (list 1 2 t)
                 (pcase (list :a 1 :b 2)
                   ((cl-lambda (&key a (b 13 b-supplied)))
                    (list a b b-supplied)))))

  (should (equal (list 1 2 t)
                 (pcase (list :a 1 :bat 2)
                   ((cl-lambda (&key a ((:bat b) 13 b-supplied)))
                    (list a b b-supplied)))))

  (should (equal (list 1 2 t)
                 (let ((key :bat))
                   (pcase (list :a 1 :bat 2)
                     ((cl-lambda (&key a ((key b) 13 b-supplied)))
                      (list a b b-supplied)))))))

(ert-deftest pcase-tests-cl-lambda-&key-sub-patterns ()
  (should (equal '(1 2 (:c 77 :e should-ignore) nil 77 t 99 nil)
                 (pcase '(:ab (1 2))
                   ((cl-lambda (&key
                                ((:ab `(,a ,b)))
                                ((:cd (cl-lambda ( &whole cd
                                                   &key
                                                   (c 88 c-supp)
                                                   ((:d d) 99 d-supp)
                                                   &allow-other-keys)))
                                 (list :c 77 :e 'should-ignore)
                                 cd-supp)))
                    (list a b cd cd-supp c c-supp d d-supp)))))

  (should (equal '( 1 2 (:c 77 :e should-ignore :allow-other-keys t) nil
                    77 t 99 nil)
                 (pcase '(:ab (1 2))
                   ((cl-lambda (&key
                                ((:ab `(,a ,b)))
                                ((:cd (cl-lambda ( &whole cd
                                                   &key
                                                   (c 88 c-supp)
                                                   ((:d d) 99 d-supp))))
                                 (list :c 77 :e 'should-ignore
                                       :allow-other-keys t)
                                 cd-supp)))
                    (list a b cd cd-supp c c-supp d d-supp)))))

  (should (equal nil
                 (pcase '(:ab (1 2))
                   ((cl-lambda (&key
                                ((:ab `(,a ,b)))
                                ((:cd (cl-lambda ( &whole cd
                                                   &key
                                                   (c 88 c-supp)
                                                   ((:d d) 99 d-supp))))
                                 (list :c 77 :e 'should-fail)
                                 cd-supp)))
                    (list a b cd cd-supp c c-supp d d-supp))))))

(ert-deftest pcase-tests-cl-lambda-&aux-should-error ()
  "`&aux' cannot be used after `&aux'."
  (should-error (pcase nil
                  ((cl-lambda (&aux a &aux b))
                   (list a b)))
                :type 'cl--pcase-lambda-list-bad-lambda-list))

(ert-deftest pcase-tests-cl-lambda-&aux ()
  (should (equal (list 1 2 nil nil)
                 (pcase nil
                   ((cl-lambda (&aux (a 1) (b 2) (c) d))
                    (list a b c d)))))

  (should (equal (list 0 1 2 nil nil)
                 (pcase (list 0)
                   ((cl-lambda (z0 &aux (a 1) (b 2) (c) d))
                    (list z0 a b c d))))))

(ert-deftest pcase-tests-cl-lambda-&aux-sub-patterns ()
  (should (equal (list 1 2)
                 (pcase nil
                   ((cl-lambda (&aux (`(,a ,b) (list 1 2))))
                    (list a b ))))))

(ert-deftest pcase-tests-cl-lambda-all ()
  (should (equal '(1 2 3 4 5 (:k1 111 :k2 222) 111 222 333 444)
                 (pcase (list 1 2 3 4 5 :k1 111 :k2 222)
                   ((cl-lambda ( a b c
                                 &optional d e
                                 &rest r
                                 &key k1 k2
                                 &aux (x1 333) (x2 444)))
                    (list a b c d e r k1 k2 x1 x2))))))

;;; pcase-tests.el ends here.
