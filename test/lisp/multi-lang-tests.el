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
       (delete-directory dir t))
     (let (kill-buffer-query-functions)
       (set-buffer-modified-p nil)
       (kill-buffer))))

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
      (should (get-text-property (point) 'display))
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
        (ert-command-loop
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
           (should-not (equal (get-text-property (point) 'face) '(subscript)))
           (should-not (get-text-property (point) 'display))))))))

(provide 'multi-lang-tests)
;;; multi-lang-tests.el ends here


;; (package-initialize)
;; (require 'paredit)


;; (defun collect-pos ()
;;   (with-current-buffer "s.tex"
;;     (cl-loop with queue = (list (tree-sitter-root-node))
;; 	     while queue
;; 	     for node = (pop queue)
;; 	     append
;; 	     (prog1 (when-let ((word-p (equal (tree-sitter-node-type node) "word"))
;; 			       (word-text (buffer-substring-no-properties
;; 					   (tree-sitter-node-start node)
;; 					   (tree-sitter-node-end node)))
;; 			       (parent (tree-sitter-node-parent node))
;; 			       (lstlisting-p (and (equal word-text "lstlisting")
;; 						  (equal "begin" (tree-sitter-node-type parent)))))
;; 		      (list (cl-some (lambda (child)
;; 				       (when-let ((bracket-group-p (equal "bracket_group" (tree-sitter-node-type child)))
;; 						  (bracket-text (buffer-substring-no-properties (tree-sitter-node-start child) (tree-sitter-node-end child)))
;; 						  (language-p (string-prefix-p "[language=" bracket-text)))
;; 					 bracket-text))
;; 				     (mapcar (lambda (k)
;; 					       (tree-sitter-node-child parent k))
;; 					     (number-sequence 0 (1- (tree-sitter-node-child-count parent)))))))
;; 	       (dotimes (i (tree-sitter-node-child-count node))
;; 		 (push (tree-sitter-node-child node i) queue))))))





;; (collect-pos)
