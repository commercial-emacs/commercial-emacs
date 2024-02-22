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

;; This used to bootstrap native compilation subprocesses forked
;; from the main emacs instance.

;;; Code:

(require 'comp-common)

(defgroup comp-run nil
  "Emacs Lisp native compiler runtime."
  :group 'lisp)

(defvar comp-installed-trampolines-h) ;comp.c
(defvar native-comp-disable-subr-trampolines) ;comp.c

(declare-function comp-trampoline-search "comp")
(declare-function comp-trampoline-compile "comp")
(declare-function comp--install-trampoline "comp.c")
(declare-function native-elisp-load "comp.c")

(defconst comp-warn-primitives
  '(null memq gethash and subrp not subr-native-elisp-p
         comp--install-trampoline concat if symbolp symbol-name make-string
         length aset aref length> mapcar expand-file-name
         file-name-as-directory file-exists-p native-elisp-load)
  "Primitives overridden at one's peril.  Weak.")

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
