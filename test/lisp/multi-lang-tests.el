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
(require 'python)

(defmacro multi-lang-tests-doit (text &rest body)
  (declare (indent defun))
  `(let ((dir (make-temp-file "multi-lang-tests" t)))
     (unwind-protect
         (let ((file-name (expand-file-name "multi-lang-tests.tex" dir))
               font-lock-global-modes ;only works when not interactive
               enable-dir-local-variables)
           (with-temp-file file-name (insert ,text))
           (find-file file-name)
           (should-not (text-property-any (point-min) (point-max) 'fontified t))
           (let (noninteractive)
             (font-lock-mode))
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
\usepackage{listings}
\begin{lstlisting}[language=Python]
def my_function(x, y):
    return x + y
\end{lstlisting}
"))
    (multi-lang-tests-doit (replace-regexp-in-string "^\n" "" text)
      (search-forward "def")
      (backward-word)
      (should-not (get-text-property (point) 'face))
      (make-multi-lang-overlay (line-beginning-position) (line-end-position)
                               'python-mode)
      (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))
      )))

(provide 'multi-lang-tests)
;;; multi-lang-tests.el ends here
