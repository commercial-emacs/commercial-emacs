;;; pdump.el --- load up standardly loaded Lisp files for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 1985-1986, 1992, 1994, 2001-2024 Free Software
;; Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

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

;; A non-nil `pdumper--pure-pool' is the sole differentiator of
;; the final pdump run from the initial bootstrap run.

;;; Code:

(setq pdumper--doctor-load-history
      '(mapc
        (lambda (entry)
          (catch 'fix-entry
            (mapc ; FIXME dolist and dotimes result in gc segv
             (lambda (path)
               (let ((dir (file-name-as-directory path)))
                 (when (string-prefix-p dir (car entry))
                   (setcar entry (concat (file-name-as-directory installed-directory)
                                         (file-name-as-directory "lisp")
                                         (substring (car entry) (length dir))))
                   (throw 'fix-entry nil))))
             (sort (copy-sequence load-path)
                   (lambda (a b) (< (length a) (length b)))))))
        load-history))

(let* ((parent-dir (file-name-directory (car load-path)))
       (load-path (cons (concat parent-dir "admin") load-path)))
  (load "pdump"))

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; pdump.el ends here
