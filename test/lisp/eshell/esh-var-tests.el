;;; esh-var-tests.el --- esh-var test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

;; Tests for Eshell's variable handling.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

(defvar eshell-test-value nil)

;;; Tests:


;; Variable interpolation

(ert-deftest esh-var-test/interp-var ()
  "Interpolate variable"
  (should (equal (eshell-test-command-result "echo $user-login-name")
                 user-login-name)))

(ert-deftest esh-var-test/interp-quoted-var ()
  "Interpolate quoted variable"
  (should (equal (eshell-test-command-result "echo $'user-login-name'")
                 user-login-name))
  (should (equal (eshell-test-command-result "echo $\"user-login-name\"")
                 user-login-name)))

(ert-deftest esh-var-test/interp-quoted-var-concat ()
  "Interpolate and concat quoted variable"
  (should (equal (eshell-test-command-result "echo $'user-login-name'-foo")
                 (concat user-login-name "-foo")))
  (should (equal (eshell-test-command-result "echo $\"user-login-name\"-foo")
                 (concat user-login-name "-foo"))))

(ert-deftest esh-var-test/interp-var-indices ()
  "Interpolate list variable with indices"
  (let ((eshell-test-value '("zero" "one" "two" "three" "four")))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[0]")
                   "zero"))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[0 2]")
                   '("zero" "two")))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[0 2 4]")
                   '("zero" "two" "four")))))

(ert-deftest esh-var-test/interp-var-split-indices ()
  "Interpolate string variable with indices"
  (let ((eshell-test-value "zero one two three four"))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[0]")
                   "zero"))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[0 2]")
                   '("zero" "two")))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[0 2 4]")
                   '("zero" "two" "four")))))

(ert-deftest esh-var-test/interp-var-string-split-indices ()
  "Interpolate string variable with string splitter and indices"
  (let ((eshell-test-value "zero:one:two:three:four"))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[: 0]")
                   "zero"))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[: 0 2]")
                   '("zero" "two"))))
  (let ((eshell-test-value "zeroXoneXtwoXthreeXfour"))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[X 0]")
                   "zero"))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[X 0 2]")
                   '("zero" "two")))))

(ert-deftest esh-var-test/interp-var-regexp-split-indices ()
  "Interpolate string variable with regexp splitter and indices"
  (let ((eshell-test-value "zero:one!two:three!four"))
    (should (equal (eshell-test-command-result
                    "echo $eshell-test-value['[:!]' 0]")
                   "zero"))
    (should (equal (eshell-test-command-result
                    "echo $eshell-test-value['[:!]' 0 2]")
                   '("zero" "two")))
    (should (equal (eshell-test-command-result
                    "echo $eshell-test-value[\"[:!]\" 0]")
                   "zero"))
    (should (equal (eshell-test-command-result
                    "echo $eshell-test-value[\"[:!]\" 0 2]")
                   '("zero" "two")))))

(ert-deftest esh-var-test/interp-var-assoc ()
  "Interpolate alist variable with index"
  (let ((eshell-test-value '(("foo" . 1))))
    (should (eq (eshell-test-command-result "echo $eshell-test-value[foo]")
                1))))

(ert-deftest esh-var-test/interp-var-length-list ()
  "Interpolate length of list variable"
  (let ((eshell-test-value '((1 2) (3) (5 (6 7 8 9)))))
    (should (eq (eshell-test-command-result "echo $#eshell-test-value") 3))
    (should (eq (eshell-test-command-result "echo $#eshell-test-value[1]") 1))
    (should (eq (eshell-test-command-result "echo $#eshell-test-value[2][1]")
                4))))

(ert-deftest esh-var-test/interp-var-length-string ()
  "Interpolate length of string variable"
  (let ((eshell-test-value "foobar"))
    (should (eq (eshell-test-command-result "echo $#eshell-test-value") 6))))

(ert-deftest esh-var-test/interp-var-length-alist ()
  "Interpolate length of alist variable"
  (let ((eshell-test-value '(("foo" . (1 2 3)))))
    (should (eq (eshell-test-command-result "echo $#eshell-test-value") 1))
    (should (eq (eshell-test-command-result "echo $#eshell-test-value[foo]")
                3))))

(ert-deftest esh-var-test/interp-lisp ()
  "Interpolate Lisp form evaluation"
  (should (equal (eshell-test-command-result "+ $(+ 1 2) 3") 6)))

(ert-deftest esh-var-test/interp-lisp-indices ()
  "Interpolate Lisp form evaluation with index"
  (should (equal (eshell-test-command-result "+ $(list 1 2)[1] 3") 5)))

(ert-deftest esh-var-test/interp-cmd ()
  "Interpolate command result"
  (should (equal (eshell-test-command-result "+ ${+ 1 2} 3") 6)))

(ert-deftest esh-var-test/interp-cmd-indices ()
  "Interpolate command result with index"
  (should (equal (eshell-test-command-result "+ ${listify 1 2}[1] 3") 5)))

(ert-deftest esh-var-test/interp-cmd-external ()
  "Interpolate command result from external command"
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-command-result-p "echo ${*echo hi}"
                            "hi\n")))

(ert-deftest esh-var-test/interp-cmd-external-indices ()
  "Interpolate command result from external command with index"
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-command-result-p "echo ${*echo \"hi\nbye\"}[1]"
                            "bye\n")))

(ert-deftest esh-var-test/interp-temp-cmd ()
  "Interpolate command result redirected to temp file"
  (should (equal (eshell-test-command-result "cat $<echo hi>") "hi")))

(ert-deftest esh-var-test/interp-concat-lisp ()
  "Interpolate and concat Lisp form"
  (should (equal (eshell-test-command-result "+ $(+ 1 2)3 3") 36)))

(ert-deftest esh-var-test/interp-concat-lisp2 ()
  "Interpolate and concat two Lisp forms"
  (should (equal (eshell-test-command-result "+ $(+ 1 2)$(+ 1 2) 3") 36)))

(ert-deftest esh-var-test/interp-concat-cmd ()
  "Interpolate and concat command with literal"
  (should (equal (eshell-test-command-result "+ ${+ 1 2}3 3") 36))
  (should (equal (eshell-test-command-result "echo ${*echo \"foo\nbar\"}-baz")
                 '("foo" "bar-baz")))
  ;; Concatenating to a number in a list should produce a number...
  (should (equal (eshell-test-command-result "echo ${*echo \"1\n2\"}3")
                 '(1 23)))
  ;; ... but concatenating to a string that looks like a number in a list
  ;; should produce a string.
  (should (equal (eshell-test-command-result "echo ${*echo \"hi\n2\"}3")
                 '("hi" "23"))))

(ert-deftest esh-var-test/interp-concat-cmd2 ()
  "Interpolate and concat two commands"
  (should (equal (eshell-test-command-result "+ ${+ 1 2}${+ 1 2} 3") 36)))

(ert-deftest esh-var-test/interp-concat-cmd-external ()
  "Interpolate command result from external command with concatenation"
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-command-result-p "echo ${echo hi}-${*echo there}"
                            "hi-there\n")))

(ert-deftest esh-var-test/quoted-interp-var ()
  "Interpolate variable inside double-quotes"
  (should (equal (eshell-test-command-result "echo \"$user-login-name\"")
                 user-login-name)))

(ert-deftest esh-var-test/quoted-interp-quoted-var ()
  "Interpolate quoted variable inside double-quotes"
  (should (equal (eshell-test-command-result
                  "echo \"hi, $'user-login-name'\"")
                 (concat "hi, " user-login-name)))
  (should (equal (eshell-test-command-result
                  "echo \"hi, $\\\"user-login-name\\\"\"")
                 (concat "hi, " user-login-name))))

(ert-deftest esh-var-test/quoted-interp-var-indices ()
  "Interpolate string variable with indices inside double-quotes"
  (let ((eshell-test-value '("zero" "one" "two" "three" "four")))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[0]\"")
                   "zero"))
    ;; FIXME: These tests would use the 0th index like the other tests
    ;; here, but evaluating the command just above adds an `escaped'
    ;; property to the string "zero".  This results in the output
    ;; printing the string properties, which is probably the wrong
    ;; behavior.  See bug#54486.
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[1 2]\"")
                   "(\"one\" \"two\")"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[1 2 4]\"")
                   "(\"one\" \"two\" \"four\")"))))

(ert-deftest esh-var-test/quoted-interp-var-split-indices ()
  "Interpolate string variable with indices inside double-quotes"
  (let ((eshell-test-value "zero one two three four"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[0]\"")
                   "zero"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[0 2]\"")
                   "(\"zero\" \"two\")"))))

(ert-deftest esh-var-test/quoted-interp-var-string-split-indices ()
  "Interpolate string variable with string splitter and indices
inside double-quotes"
  (let ((eshell-test-value "zero:one:two:three:four"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[: 0]\"")
                   "zero"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[: 0 2]\"")
                   "(\"zero\" \"two\")")))
  (let ((eshell-test-value "zeroXoneXtwoXthreeXfour"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[X 0]\"")
                   "zero"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[X 0 2]\"")
                   "(\"zero\" \"two\")"))))

(ert-deftest esh-var-test/quoted-interp-var-regexp-split-indices ()
  "Interpolate string variable with regexp splitter and indices"
  (let ((eshell-test-value "zero:one!two:three!four"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value['[:!]' 0]\"")
                   "zero"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value['[:!]' 0 2]\"")
                   "(\"zero\" \"two\")"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[\\\"[:!]\\\" 0]\"")
                   "zero"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[\\\"[:!]\\\" 0 2]\"")
                   "(\"zero\" \"two\")"))))

(ert-deftest esh-var-test/quoted-interp-var-assoc ()
  "Interpolate alist variable with index inside double-quotes"
  (let ((eshell-test-value '(("foo" . 1))))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[foo]\"")
                   "1"))))

(ert-deftest esh-var-test/quoted-interp-var-length-list ()
  "Interpolate length of list variable inside double-quotes"
  (let ((eshell-test-value '((1 2) (3) (5 (6 7 8 9)))))
    (should (equal (eshell-test-command-result "echo \"$#eshell-test-value\"")
                   "3"))
    (should (equal (eshell-test-command-result
                    "echo \"$#eshell-test-value[1]\"")
                   "1"))
    (should (equal (eshell-test-command-result
                    "echo \"$#eshell-test-value[2][1]\"")
                   "4"))))

(ert-deftest esh-var-test/quoted-interp-var-length-string ()
  "Interpolate length of string variable inside double-quotes"
  (let ((eshell-test-value "foobar"))
    (should (equal (eshell-test-command-result "echo \"$#eshell-test-value\"")
                   "6"))))

(ert-deftest esh-var-test/quoted-interp-var-length-alist ()
  "Interpolate length of alist variable inside double-quotes"
  (let ((eshell-test-value '(("foo" . (1 2 3)))))
    (should (equal (eshell-test-command-result "echo \"$#eshell-test-value\"")
                   "1"))
    (should (equal (eshell-test-command-result
                    "echo \"$#eshell-test-value[foo]\"")
                   "3"))))

(ert-deftest esh-var-test/quoted-interp-lisp ()
  "Interpolate Lisp form evaluation inside double-quotes"
  (should (equal (eshell-test-command-result
                  "echo \"hi $(concat \\\"the\\\" \\\"re\\\")\"")
                 "hi there")))

(ert-deftest esh-var-test/quoted-interp-lisp-indices ()
  "Interpolate Lisp form evaluation with index"
  (should (equal (eshell-test-command-result "concat \"$(list 1 2)[1]\" cool")
                 "2cool")))

(ert-deftest esh-var-test/quoted-interp-cmd ()
  "Interpolate command result inside double-quotes"
  (should (equal (eshell-test-command-result
                  "echo \"hi ${echo \\\"there\\\"}\"")
                 "hi there")))

(ert-deftest esh-var-test/quoted-interp-cmd-indices ()
  "Interpolate command result with index inside double-quotes"
  (should (equal (eshell-test-command-result
                  "concat \"${listify 1 2}[1]\" cool")
                 "2cool")))

(ert-deftest esh-var-test/quoted-interp-temp-cmd ()
  "Interpolate command result redirected to temp file inside double-quotes"
  (let ((temporary-file-directory
         (file-name-as-directory (make-temp-file "esh-vars-tests" t))))
    (unwind-protect
        (should (equal (eshell-test-command-result "cat \"$<echo hi>\"")
                       "hi"))
      (delete-directory temporary-file-directory t))))

(ert-deftest esh-var-test/quoted-interp-concat-cmd ()
  "Interpolate and concat command with literal"
  (should (equal (eshell-test-command-result
                  "echo \"${echo \\\"foo\nbar\\\"} baz\"")
                 "foo\nbar baz")))


;; Interpolated variable conversion

(ert-deftest esh-var-test/interp-convert-var-number ()
  "Interpolate numeric variable"
  (let ((eshell-test-value 123))
    (should (equal (eshell-test-command-result "type-of $eshell-test-value")
                   'integer))))

(ert-deftest esh-var-test/interp-convert-var-split-indices ()
  "Interpolate and convert string variable with indices"
  ;; Check that numeric forms are converted to numbers.
  (let ((eshell-test-value "000 010 020 030 040"))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[0]")
                   0))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[0 2]")
                   '(0 20))))
  ;; Check that multiline forms are preserved as-is.
  (let ((eshell-test-value "foo\nbar:baz\n"))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[: 0]")
                   "foo\nbar"))
    (should (equal (eshell-test-command-result "echo $eshell-test-value[: 1]")
                   "baz\n"))))

(ert-deftest esh-var-test/interp-convert-quoted-var-number ()
  "Interpolate numeric quoted numeric variable"
  (let ((eshell-test-value 123))
    (should (equal (eshell-test-command-result "type-of $'eshell-test-value'")
                   'integer))
    (should (equal (eshell-test-command-result "type-of $\"eshell-test-value\"")
                   'integer))))

(ert-deftest esh-var-test/interp-convert-quoted-var-split-indices ()
  "Interpolate and convert quoted string variable with indices"
  (let ((eshell-test-value "000 010 020 030 040"))
    (should (equal (eshell-test-command-result "echo $'eshell-test-value'[0]")
                   0))
    (should (equal (eshell-test-command-result "echo $'eshell-test-value'[0 2]")
                   '(0 20)))))

(ert-deftest esh-var-test/interp-convert-cmd-string-newline ()
  "Interpolate trailing-newline command result"
  (should (equal (eshell-test-command-result "echo ${echo \"foo\n\"}") "foo")))

(ert-deftest esh-var-test/interp-convert-cmd-multiline ()
  "Interpolate multi-line command result"
  (should (equal (eshell-test-command-result "echo ${echo \"foo\nbar\"}")
                 '("foo" "bar")))
  ;; Numeric output should be converted to numbers...
  (should (equal (eshell-test-command-result "echo ${echo \"01\n02\n03\"}")
                 '(1 2 3)))
  ;; ... but only if every line is numeric.
  (should (equal (eshell-test-command-result "echo ${echo \"01\n02\nhi\"}")
                 '("01" "02" "hi"))))

(ert-deftest esh-var-test/interp-convert-cmd-number ()
  "Interpolate numeric command result"
  (should (equal (eshell-test-command-result "echo ${echo \"1\"}") 1)))

(ert-deftest esh-var-test/interp-convert-cmd-split-indices ()
  "Interpolate command result with indices"
  (should (equal (eshell-test-command-result "echo ${echo \"000 010 020\"}[0]")
                 0))
  (should (equal (eshell-test-command-result
                  "echo ${echo \"000 010 020\"}[0 2]")
                 '(0 20))))

(ert-deftest esh-var-test/quoted-interp-convert-var-number ()
  "Interpolate numeric variable inside double-quotes"
  (let ((eshell-test-value 123))
    (should (equal (eshell-test-command-result "type-of \"$eshell-test-value\"")
                   'string))))

(ert-deftest esh-var-test/quoted-interp-convert-var-split-indices ()
  "Interpolate string variable with indices inside double-quotes"
  (let ((eshell-test-value "000 010 020 030 040"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[0]\"")
                   "000"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[0 2]\"")
                   "(\"000\" \"020\")"))))

(ert-deftest esh-var-test/quoted-interp-convert-quoted-var-number ()
  "Interpolate numeric quoted variable inside double-quotes"
  (let ((eshell-test-value 123))
    (should (equal (eshell-test-command-result
                    "type-of \"$'eshell-test-value'\"")
                   'string))
    (should (equal (eshell-test-command-result
                    "type-of \"$\\\"eshell-test-value\\\"\"")
                   'string))))

(ert-deftest esh-var-test/quoted-interp-convert-quoted-var-split-indices ()
  "Interpolate quoted string variable with indices inside double-quotes"
  (let ((eshell-test-value "000 010 020 030 040"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[0]\"")
                   "000"))
    (should (equal (eshell-test-command-result
                    "echo \"$eshell-test-value[0 2]\"")
                   "(\"000\" \"020\")"))))

(ert-deftest esh-var-test/quoted-interp-convert-cmd-string-newline ()
  "Interpolate trailing-newline command result inside double-quotes"
  (should (equal (eshell-test-command-result "echo \"${echo \\\"foo\n\\\"}\"")
                 "foo"))
  (should (equal (eshell-test-command-result "echo \"${echo \\\"foo\n\n\\\"}\"")
                 "foo")))

(ert-deftest esh-var-test/quoted-interp-convert-cmd-multiline ()
  "Interpolate multi-line command result inside double-quotes"
  (should (equal (eshell-test-command-result
                  "echo \"${echo \\\"foo\nbar\\\"}\"")
                 "foo\nbar")))

(ert-deftest esh-var-test/quoted-interp-convert-cmd-number ()
  "Interpolate numeric command result inside double-quotes"
  (should (equal (eshell-test-command-result "echo \"${echo \\\"1\\\"}\"")
                 "1")))

(ert-deftest esh-var-test/quoted-interp-convert-cmd-split-indices ()
  "Interpolate command result with indices inside double-quotes"
  (should (equal (eshell-test-command-result
                  "echo \"${echo \\\"000 010 020\\\"}[0]\"")
                 "000")))


;; Built-in variables

(ert-deftest esh-var-test/lines-var ()
  "$LINES should equal (window-body-height nil 'remap)"
  (should (equal (eshell-test-command-result "echo $LINES")
                 (window-body-height nil 'remap))))

(ert-deftest esh-var-test/columns-var ()
  "$COLUMNS should equal (window-body-width nil 'remap)"
  (should (equal (eshell-test-command-result "echo $COLUMNS")
                 (window-body-width nil 'remap))))

(ert-deftest esh-var-test/inside-emacs-var ()
  "Test presence of \"INSIDE_EMACS\" in subprocesses"
  (with-temp-eshell
   (eshell-command-result-p "env"
                            (format "INSIDE_EMACS=%s,eshell"
                                    emacs-version))))

(ert-deftest esh-var-test/inside-emacs-var-split-indices ()
  "Test using \"INSIDE_EMACS\" with split indices"
  (with-temp-eshell
   (eshell-command-result-p "echo $INSIDE_EMACS[, 1]"
                            "eshell")))

(ert-deftest esh-var-test/last-result-var ()
  "Test using the \"last result\" ($$) variable"
  (with-temp-eshell
   (eshell-command-result-p "+ 1 2; + $$ 2"
                            "3\n5\n")))

(ert-deftest esh-var-test/last-result-var2 ()
  "Test using the \"last result\" ($$) variable twice"
  (with-temp-eshell
   (eshell-command-result-p "+ 1 2; + $$ $$"
                            "3\n6\n")))

(ert-deftest esh-var-test/last-result-var-split-indices ()
  "Test using the \"last result\" ($$) variable with split indices"
  (with-temp-eshell
   (eshell-command-result-p
    "string-join (list \"01\" \"02\") :; + $$[: 1] 3"
    "01:02\n5\n")
   (eshell-command-result-p
    "string-join (list \"01\" \"02\") :; echo \"$$[: 1]\""
    "01:02\n02\n")))

(ert-deftest esh-var-test/last-arg-var ()
  "Test using the \"last arg\" ($_) variable"
  (with-temp-eshell
   (eshell-command-result-p "+ 1 2; + $_ 4"
                            "3\n6\n")))

(ert-deftest esh-var-test/last-arg-var-indices ()
  "Test using the \"last arg\" ($_) variable with indices"
  (with-temp-eshell
   (eshell-command-result-p "+ 1 2; + $_[0] 4"
                            "3\n5\n")
   (eshell-command-result-p "+ 1 2; + $_[1] 4"
                            "3\n6\n")))

(ert-deftest esh-var-test/last-arg-var-split-indices ()
  "Test using the \"last arg\" ($_) variable with split indices"
  (with-temp-eshell
   (eshell-command-result-p "concat 01:02 03:04; + $_[0][: 1] 5"
                            "01:0203:04\n7\n")
   (eshell-command-result-p "concat 01:02 03:04; echo \"$_[0][: 1]\""
                            "01:0203:04\n02\n")))

;; esh-var-tests.el ends here
