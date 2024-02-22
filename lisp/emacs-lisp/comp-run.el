;;; comp-runtime.el --- runtime Lisp native compiler code  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

;; Author: Andrea Corallo <acorallo@gnu.org>
;; Keywords: lisp
;; Package: emacs

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

;; While the main native compiler is implemented in comp.el, when
;; commonly used as a jit compiler it is only loaded by Emacs sub
;; processes performing async compilation.  This file contains all
;; the code needed to drive async compilations and any Lisp code
;; needed at runtime to run native code.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'comp-common)
(require 'bytecomp) ;; For `emacs-lisp-compilation-mode'.

(defgroup comp-run nil
  "Emacs Lisp native compiler runtime."
  :group 'lisp)

(defcustom native-comp-jit-compilation-deny-list
  '()
  "List of regexps to exclude matching files from deferred native compilation.
Files whose names match any regexp are excluded from native compilation."
  :type '(repeat regexp)
  :version "28.1")

(make-obsolete-variable 'native-comp-deferred-compilation-deny-list
                        'native-comp-jit-compilation-deny-list
                        "29.1")

;; These variables and functions are defined in comp.c
(defvar comp-installed-trampolines-h)
(defvar native-comp-disable-subr-trampolines)

(declare-function comp--install-trampoline "comp.c")
(declare-function native-elisp-load "comp.c")

(defvar comp-num-cpus nil)

(defvar comp-last-scanned-async-output nil)
(make-variable-buffer-local 'comp-last-scanned-async-output)
;; From warnings.el
(defvar warning-suppress-types)

(defconst comp-valid-source-re (rx ".el" (? ".gz") eos)
  "Regexp to match filename of valid input source files.")

(defconst comp-warn-primitives
  '(null memq gethash and subrp not subr-native-elisp-p
         comp--install-trampoline concat if symbolp symbol-name make-string
         length aset aref length> mapcar expand-file-name
         file-name-as-directory file-exists-p native-elisp-load)
  "List of primitives we want to warn about in case of redefinition.
This are essential for the trampoline machinery to work properly.")

(defun comp-trampoline-search (subr-name)
  "Search a trampoline file for SUBR-NAME.
Return the trampoline if found or nil otherwise."
  (let ((filename (expand-file-name (comp-trampoline-filename subr-name)
                                    comp-trampoline-dir)))
    (when (file-readable-p filename)
      (native-elisp-load filename))))

(declare-function comp-trampoline-compile "comp")
;;;###autoload
(defun comp-subr-trampoline-install (subr-name)
  "Make SUBR-NAME effectively advice-able when called from native code."
  (when (memq subr-name comp-warn-primitives)
    (warn "Redefining `%s' might break native compilation of trampolines."
          subr-name))
  (when (and (not native-comp-disable-subr-trampolines)
             (not (memq subr-name native-comp-never-optimize-functions))
             (not (gethash subr-name comp-installed-trampolines-h)))
    (cl-assert (subr-primitive-p (symbol-function subr-name)))
    (when-let ((trampoline (or (comp-trampoline-search subr-name)
                               (comp-trampoline-compile subr-name))))
      (comp--install-trampoline subr-name trampoline))))

(provide 'comp-run)

;;; comp-run.el ends here
