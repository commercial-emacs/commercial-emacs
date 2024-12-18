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
(require 'ert-x)
(require 'python)
(require 'tex-mode)
(eval-when-compile (require 'cl-lib))

(defun multi-lang-tests--exec (&rest args)
  "Borrow nifty command simulator from simple-tests.el."
  (let* ((restore-pre-command-hook (default-value 'pre-command-hook))
         (checks (let ((command-p t)
                       ret)
                   (dolist (arg args)
                     (if command-p
                         (setq command-p nil)
                       (if (eq (type-of arg) 'interpreted-function)
                           (progn
                             (setq command-p t)
                             (push arg ret))
                         (push nil ret))))
                   (cons 'burner (nreverse ret))))
         (commands (seq-filter #'symbolp args))
         i-hate-command-loop
         (hook (lambda ()
                 (unless (minibufferp)
                   (condition-case err
                       (when (eq this-command 'execute-extended-command)
                         (when-let ((check (car checks)))
                           (unless (eq check 'burner)
                             (funcall check)))
                         (setq checks (cdr checks)))
                     (error (setq i-hate-command-loop err))))))
         (kbd-macro (read-kbd-macro
                     (mapconcat
                      (lambda (s) (concat "M-x " (symbol-name s) " RET"))
                      commands "\n"))))
    (unwind-protect
        (progn
          (set-default 'pre-command-hook (list hook))
          (execute-kbd-macro kbd-macro)
          (funcall (pop checks))
          (should-not checks)
          (should-not i-hate-command-loop))
      (set-default 'pre-command-hook restore-pre-command-hook))))

(defmacro multi-lang-tests-doit (ext text &rest body)
  (declare (indent defun))
  `(let ((dir (make-temp-file "multi-lang-tests" t)))
     (unwind-protect
         (let ((file-name (expand-file-name (concat "multi-lang-tests" ,ext) dir))
               font-lock-global-modes ;only works when not interactive
               enable-dir-local-variables)
           (with-temp-file file-name (insert ,text))
           (find-file file-name)
           (should-not (text-property-any (point-min) (point-max) 'fontified t))
           (let (noninteractive)
             (font-lock-mode))
           (font-lock-ensure)
           (should font-lock-fontified)
           (should-not (text-property-any (point-min) (point-max) 'fontified nil))
           (should (text-property-not-all (point-min) (point-max) 'face nil))
           ,@body)
       (let (kill-buffer-query-functions)
         (set-buffer-modified-p nil)
         (kill-buffer))
       (delete-directory dir t))))

(ert-deftest multi-lang-test-interactive ()
  "Test interactive use."
  (let ((text "
\\usepackage{listings}
\\begin{lstlisting}[language=Python]
def my_function(x, y):
    return x + y
\\end{lstlisting}
"))
    (multi-lang-tests-doit ".tex" (replace-regexp-in-string "^\n" "" text)
      (should (eq major-mode 'plain-tex-mode))
      (search-forward "lstlisting")
      (backward-word)
      (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))
      (search-forward "def")
      (backward-word)
      (should-not (get-text-property (point) 'face))
      (search-forward "my_f")
      (backward-word)
      (should (equal (get-text-property (point) 'face) '(subscript)))
      (let* ((base (current-buffer))
             (indirect (make-multi-lang-overlay
                        (save-excursion
                          (goto-char (point-min))
                          (search-forward "def")
                          (line-beginning-position))
                        (save-excursion
                          (goto-char (point-min))
                          (search-forward "\\end")
                          (line-beginning-position))
                        'python-mode)))
        (multi-lang-tests--exec
         'end-of-buffer
         (lambda ()
           (should (eq (current-buffer) base)))
         'beginning-of-buffer
         (lambda ()
           (should (eq (current-buffer) base))
           (search-forward "def"))
         'ignore
         (lambda ()
           (should (eq (current-buffer) indirect))
           (should (eq major-mode 'python-mode))
           (font-lock-ensure))
         'backward-word
         (lambda ()
           (should (eq (get-text-property (point) 'face)
                       'font-lock-keyword-face))
           (search-forward "my_f"))
         'backward-word
         (lambda ()
           (should-not (equal (get-text-property (point) 'face) '(subscript)))))))))

(provide 'multi-lang-tests)
;;; multi-lang-tests.el ends here
