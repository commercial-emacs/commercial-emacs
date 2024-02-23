;;; relative-lisp-files.el --- print loaded files relative to lisp directory  -*- lexical-binding: t; -*-
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

;; This replaces the hack known as lisp.mk.

;;; Code:

(defconst relative-lisp-phony-files '("ldefs-boot.el"))

(defsubst relative-lisp-files-as-list ()
  (delq
   nil
   (mapcar
    (lambda (entry)
      (let* ((ofn (car entry))
             (result (file-name-nondirectory ofn))
             (fn (directory-file-name (file-name-directory ofn))))
        (when (equal "el" (file-name-extension ofn))
          (catch 'done
            (prog1 nil ;if `while' doesn't yield something better
              (let ((wtf 0))
                (while (not (equal (file-name-directory "/") fn))
                  (if (equal "lisp" (file-name-nondirectory fn))
                      (progn
                        (when (member result relative-lisp-phony-files)
                          (setq result nil))
                        (throw 'done result))
	            (setq result (concat (file-name-as-directory
			                  (file-name-nondirectory fn))
			                 result)))
                  (setq fn (directory-file-name (file-name-directory fn)))
                  (setq wtf (1+ wtf))
                  (when (> wtf 10)
                    (princ (format "wtf %s %s" fn (file-name-directory "/"))
                           #'external-debugging-output)
                    (throw 'done nil)))))))))
    load-history)))

(defun relative-lisp-files ()
  "Would use `file-relative-name' but bugs out knowingly under mingw."
  (princ (mapconcat (lambda (f) (concat f "c"))
                    (relative-lisp-files-as-list) " ")))

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; relative-lisp-files.el ends here
