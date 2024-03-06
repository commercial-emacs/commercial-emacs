;;; relative-lisp-files.el --- print `make -C lisp` targets to stdout  -*- lexical-binding: t; -*-
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

(require 'comp)

(defconst relative-lisp-files-explicit '("ldefs-boot")
  "Elisp we don't want `make -C lisp` making without explicit consent.")

(defun relative-lisp-files-as-list (target-extension)
  "Return loaded files not yet compiled to TARGET-EXTENSION.
In the interest of producing `make` targets, append TARGET-EXTENSION to
return values.  A TARGET-EXTENSION of nil intends returning the full
complement of loaded files, whether compiled or not.

For both 'elc' and 'eln' TARGET-EXTENSION, omit files excluded by the
no-byte-compile file-local directive.  For the latter, append the 'elc'
extension in lieu of 'eln' to files containing the no-native-compile
directive."
  (seq-keep
   (lambda (entry)
     (let* ((fn (car entry))
            (path (directory-file-name (file-name-directory fn)))
            (adjusted-extension target-extension)
            result no-byte-compile-p no-native-compile-p path-leaf)
       (with-temp-buffer
         (insert-file-contents (file-name-with-extension
                                (file-name-sans-extension fn) "el"))
         (hack-local-variables '(no-byte-compile no-native-compile))
         (setq no-byte-compile-p no-byte-compile
               no-native-compile-p no-native-compile))
       (setq result (if adjusted-extension
                        (file-name-with-extension
                         (file-name-sans-extension (file-name-nondirectory fn))
                         (if (and (equal adjusted-extension "eln")
                                  (not no-byte-compile-p)
                                  no-native-compile-p)
                             (setq adjusted-extension "elc")
                           adjusted-extension))
                      (file-name-sans-extension (file-name-nondirectory fn))))
       (when (or (null adjusted-extension)
                 (and (not (equal adjusted-extension (file-name-extension fn)))
                      (cond ((equal adjusted-extension "elc")
                             (not no-byte-compile-p))
                            ((equal adjusted-extension "eln")
                             (and (not no-byte-compile-p)
                                  (not no-native-compile-p)))
                            (t t))))
         (catch 'done
           (prog1 nil ;if `while' doesn't `throw' something better
             (while (not (zerop (length (setq path-leaf (file-name-nondirectory path)))))
               (if (equal "lisp" path-leaf)
                   (progn
                     (when (member result relative-lisp-files-explicit)
                       (setq result nil))
                     (throw 'done result))
	         (setq result (concat (file-name-as-directory
			               (file-name-nondirectory path))
			              result)))
               (setq path (directory-file-name (file-name-directory path)))))))))
   load-history))

(defun relative-lisp-files (target-extension)
  "Would use `file-relative-name' but bugs out knowingly under mingw."
  (princ (mapconcat #'identity
                    (sort (relative-lisp-files-as-list target-extension) #'string<)
                    " ")))

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; relative-lisp-files.el ends here
