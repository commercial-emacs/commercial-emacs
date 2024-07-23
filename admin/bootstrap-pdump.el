;;; bootstrap-pdump.el --- load up standardly loaded Lisp files for Emacs  -*- lexical-binding: t; -*-

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

;; Have an uninitialized emacs load a bunch of shit then
;; `dump-emacs-portable' an image (a .pdmp file) so that subsequent
;; emacs invocations conceive fully formed.
;;
;; A non-nil `pdumper--pure-pool' is the sole differentiator of
;; the final pdump run from the initial bootstrap run.

;;; Code:

;; At genesis we can only use C primitives.

(load (concat (file-name-directory (car load-path)) "admin/pdump-common"))

(let ((output-path (expand-file-name (pdumping-output) invocation-directory))
      current-load-list)
  (message "Dumping to %s" output-path)
  (ignore-errors (delete-file output-path))
  (condition-case err
      (let (lexical-binding)
        (dump-emacs-portable output-path))
    (error (ignore-errors (delete-file output-path))
           (apply #'signal err))))

(kill-emacs) ;crude

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; bootstrap-pdump.el ends here
