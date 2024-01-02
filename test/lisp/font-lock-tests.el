;;; font-lock-tests.el --- Test suite for font-lock. -*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

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

;;; Code:
(require 'ert)

(ert-deftest font-lock-test-append-anonymous-face ()
  "Ensure `font-lock-append-text-property' does not splice anonymous faces."
  (with-temp-buffer
    (insert "foo")
    (add-text-properties 1 3 '(face italic))
    (font-lock-append-text-property 1 3 'face '(:strike-through t))
    (should (equal (get-text-property 1 'face (current-buffer))
                   '(italic (:strike-through t))))))

(ert-deftest font-lock-test-prepend-anonymous-face ()
  "Ensure `font-lock-prepend-text-property' does not splice anonymous faces."
  (with-temp-buffer
    (insert "foo")
    (add-text-properties 1 3 '(face italic))
    (font-lock-prepend-text-property 1 3 'face '(:strike-through t))
    (should (equal (get-text-property 1 'face (current-buffer))
                   '((:strike-through t) italic)))))

(ert-deftest font-lock-test-add-keywords-derived-mode ()
  "Bug#24176 exercises monnier's `font-lock-add-keywords' implicit hack.
To specify keywords for a derived mode without repeating those of
the base mode, the programmer supplies a null first argument to
`font-lock-add-keywords' to append the argument keywords to the
current buffer's keywords.  This calling mode is then deployed as
an `add-hook' to the derived mode."
  (skip-unless (not noninteractive))
  (eval-and-compile
    (define-derived-mode ~/a fundamental-mode "~/a"
      (font-lock-add-keywords nil `(("a" 0 'font-lock-keyword-face))))
    (define-derived-mode ~/b ~/a "~/b"
      (font-lock-add-keywords nil `(("b" 0 'font-lock-builtin-face))))
    (define-derived-mode ~/c ~/b "~/c"
      (font-lock-add-keywords nil `(("c" 0 'font-lock-constant-face)))))
  (let ((unhidden-font-lockable-buffer (get-buffer-create "font-lock-test-akdm")))
    (unwind-protect
        (progn
          (with-current-buffer unhidden-font-lockable-buffer
            (insert "abc")
            (call-interactively #'~/a)
            (font-lock-update)
            (should (eq (get-text-property 1 'face) 'font-lock-keyword-face))
            (should-not (eq (get-text-property 2 'face) 'font-lock-builtin-face))
            (should-not (eq (get-text-property 3 'face) 'font-lock-constant-face))
            (call-interactively #'~/b)
            (font-lock-update)
            (should (eq (get-text-property 1 'face) 'font-lock-keyword-face))
            (should (eq (get-text-property 2 'face) 'font-lock-builtin-face))
            (should-not (eq (get-text-property 3 'face) 'font-lock-constant-face))
            (call-interactively #'~/c)
            (font-lock-update)
            (should (eq (get-text-property 1 'face) 'font-lock-keyword-face))
            (should (eq (get-text-property 2 'face) 'font-lock-builtin-face))
            (should (eq (get-text-property 3 'face) 'font-lock-constant-face))
            (call-interactively #'~/a)
            (font-lock-update)
            (should (eq (get-text-property 1 'face) 'font-lock-keyword-face))
            (should-not (eq (get-text-property 2 'face) 'font-lock-builtin-face))
            (should-not (eq (get-text-property 3 'face) 'font-lock-constant-face))))
      (let (kill-buffer-query-functions)
        (kill-buffer unhidden-font-lockable-buffer)))))

;; font-lock-tests.el ends here
