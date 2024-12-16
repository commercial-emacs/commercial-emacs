;;; multi-lang-tests.el --- tests for lisp/multi-lang.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Command Line Systems

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

(defmacro multi-lang-tests-doit (text &rest body)
  (declare (indent defun))
  `(let ((dir (make-temp-file "multi-lang-tests" t)))
     (unwind-protect
         (let ((file-name (expand-file-name "multi-lang-tests.Rnw" dir))
               enable-dir-local-variables)
           (with-temp-file file-name (insert ,text))
           (find-file file-name)
           (should font-lock-mode)
           (should jit-lock-mode)
           ,@body)
       (let (kill-buffer-query-functions)
         (set-buffer-modified-p nil)
         (kill-buffer))
       (delete-directory dir t))))

(ert-deftest multi-lang-test-interactive ()
  "Test interactive use."
  (let ((text "
\documentclass{article}
\author{Kate Cowles}
\usepackage{Sweave}

<<>>=
summary( lmout <- lm( y ~ x, data=mydat) )
@

A \LaTeX\ figure.

\begin{center}
<<fig=TRUE,echo=FALSE>>=
plot(lmout)
@
\end{center}
\end{document}
"))
    (multi-lang-tests-doit (replace-regexp-in-string "^\n" "" text)
      (should (equal (multi-lang-highlights (point-min) (point-max))
                     ))
      (goto-char (point-min))
      (forward-line 1))))

(provide 'multi-lang-tests)
;;; multi-lang-tests.el ends here
