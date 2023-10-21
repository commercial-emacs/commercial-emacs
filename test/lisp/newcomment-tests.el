;;; newcomment-tests.el --- Tests for newcomment.el  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

(ert-deftest test-uncomment-space-comment-continue ()
  (let ((comment-style 'multi-line)
        (comment-continue "   ")
        (text "  a\n  b"))
    (should
     (equal text
            (with-temp-buffer
              (c-mode)
              (insert text)
              (comment-region (point-min) (point-max))
              (uncomment-region (point-min) (point-max))
              (buffer-string))))))

(ert-deftest local-comment-continue-in-comment-indent-new-line ()
  (with-temp-buffer
    (setq-local comment-start "/* ")
    (setq-local comment-end "*/")
    (insert "foo")
    (newline)
    (insert "bar")
    (forward-line -1)
    (end-of-line)
    (comment-region (point-min) (point-max))
    (should (equal (thing-at-point 'line) "/* foo\n"))
    (comment-indent-new-line)
    (should (equal (thing-at-point 'line) " * \n"))
    (setq-local comment-continue "   ")
    (comment-indent-new-line)
    (should (equal (thing-at-point 'line) "   \n"))))

;;; newcomment-tests.el ends here
