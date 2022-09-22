;;; image-dired-util-tests.el --- Tests for image-dired.el  -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'image-dired)
(require 'image-dired-util)
(require 'xdg)

(ert-deftest image-dired-thumb-name ()
  (skip-unless (file-directory-p (temporary-file-directory)))
  (let* ((image-dired-dir (temporary-file-directory))
         (restore-cache-home (getenv "XDG_CACHE_HOME"))
         (jpg "foo.jpg"))
    (setenv "XDG_CACHE_HOME" (temporary-file-directory))
    (unwind-protect
        (dolist (test-case
                 '((standard . "\\.png$")
                   (image-dired . "thumb\\.jpg$")
                   (per-directory . "foo\\.thumb\\.jpg$")))
          (let* ((image-dired-thumbnail-storage (car test-case))
                 (thumb-name (image-dired-thumb-name jpg)))
            (should (file-name-absolute-p thumb-name))
            (should (string-match-p (cdr test-case)
                                    (file-name-nondirectory thumb-name)))))
      (setenv "XDG_CACHE_HOME" restore-cache-home)
      (should (equal (getenv "XDG_CACHE_HOME") restore-cache-home)))))

;;; image-dired-util-tests.el ends here
