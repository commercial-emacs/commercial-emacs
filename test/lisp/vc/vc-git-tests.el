;;; vc-git-tests.el --- tests for vc/vc-git.el  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2022 Free Software Foundation, Inc.

;; Author: Justin Schell <justinmschell@gmail.com>
;; Maintainer: emacs-devel@gnu.org

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

;;; Code:

(require 'vc-git)

(ert-deftest vc-git-test-program-version-general ()
  (vc-git-test--run-program-version-test
   "git version 2.30.1.0"
   "2.30.1.0"))

(ert-deftest vc-git-test-program-version-windows ()
  (vc-git-test--run-program-version-test
   "git version 2.30.1.1.windows.1"
   "2.30.1.1"))

(ert-deftest vc-git-test-program-version-apple ()
  (vc-git-test--run-program-version-test
   "git version 2.30.1.2 (Apple Git-130)"
   "2.30.1.2"))

(ert-deftest vc-git-test-program-version-other ()
  (vc-git-test--run-program-version-test
   "git version 2.30.1.3.foo.bar"
   "2.30.1.3"))

(ert-deftest vc-git-test-program-version-invalid-leading-string ()
  (vc-git-test--run-program-version-test
   "git version foo.bar.2.30.1.4"
   "0"))

(ert-deftest vc-git-test-program-version-invalid-leading-dot ()
  (vc-git-test--run-program-version-test
   "git version .2.30.1.5"
   "0"))

(defmacro vc-git-test--mock-repo (&rest body)
  (declare (indent defun))
  `(let* ((dir (make-temp-file "vc-git-tests" t))
          (default-directory dir))
     (unwind-protect
         (progn
           (vc-git-create-repo)
           (vc-git-command nil 0 nil "config" "--add" "user.name" "frou")
           ,@body)
       (delete-directory dir t))))

(ert-deftest vc-git-test-detached-head ()
  (skip-unless (executable-find vc-git-program))
  (require 'log-edit)
  (vc-git-test--mock-repo
    (with-temp-file "foo")
    (condition-case err
        (progn
          (vc-git-register (split-string "foo"))
          (vc-git-checkin (split-string "foo") "No-Verify: yes
his fooness")
          (vc-git-checkout nil (vc-git--rev-parse "HEAD" t)))
      (error (signal (car err) (with-current-buffer "*vc*" (buffer-string)))))
    (find-file-noselect "foo")))

(defun vc-git-test--run-program-version-test
    (mock-version-string expected-output)
  (cl-letf* (((symbol-function 'vc-git--run-command-string)
              (lambda (_file _args) mock-version-string))
             (vc-git--program-version nil)
             (actual-output (vc-git--program-version)))
    (should (equal actual-output expected-output))))

;;; vc-git-tests.el ends here
