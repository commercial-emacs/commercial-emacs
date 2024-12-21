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
(require 'org)
(require 'python)
(require 'tex-mode)
(require 'tree-sitter)
(declare-function tree-sitter--testable "tree-sitter.c")

(defsubst tree-sitter-testable ()
  (when-let ((dylib (expand-file-name "lib/latex.so" tree-sitter-resources-dir)))
    (tree-sitter--testable dylib)))

(defmacro tree-sitter-tests-with-resources-dir (&rest body)
  (declare (indent defun))
  `(let ((tree-sitter-resources-dir
          (expand-file-name (concat (file-name-as-directory "src/tree-sitter-resources")
                                    (if (eq system-type 'darwin)
                                        "darwin"
                                      "")))))
     (skip-unless (tree-sitter-testable))
     ,@body))

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

(ert-deftest multi-lang-test-org ()
  "Dominik's commit afe98df locks down a bespoke font lock scheme."
  (let ((text "
#+BEGIN_SRC emacs-lisp :results output :exports both
  (list 1 2 3)
#+END_SRC
#+BEGIN_SRC python :results output :exports both
  import sys
  print(sys.version)
#+END_SRC
#+begin_src sh :results output :shebang #!/usr/bin/env bash
  # a comment
  if [ -z $1 ]; then
      asked=$USER
  else
      asked=$1
  fi
#+end_src
"))
    (multi-lang-tests-doit ".org" (replace-regexp-in-string "^\n" "" text)
      (should (eq major-mode 'org-mode))
      (goto-char (point-max))
      (while (ignore-errors (org-babel-previous-src-block))
        (when-let ((lang (car (org-babel-get-src-block-info t)))
                   (mode (intern-soft (concat lang "-mode")))
                   (beg (save-excursion
                          (forward-line)
                          (line-beginning-position)))
                   (end (save-excursion
                          (and (re-search-forward org-babel-src-block-regexp nil t)
                               (line-beginning-position)))))
          (make-multi-lang-overlay beg end mode)))
      (search-forward "import")
      (backward-word)
      (let ((face (get-text-property (point) 'face)))
        (should (funcall (if (listp face) #'memq #'eq)
                         'font-lock-keyword-face face)))
      (search-forward "asked")
      (backward-word)
      (let ((face (get-text-property (point) 'face)))
        (should (funcall (if (listp face) #'memq #'eq)
                         'font-lock-variable-name-face face))))))

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

(ert-deftest multi-lang-test-tree-walk ()
  (tree-sitter-tests-with-resources-dir
   (let ((tree-sitter-mode-alist `((latex-mode . "latex") . ,tree-sitter-mode-alist))
         (text "
\\begin{document}
\\section{Foo}
\\begingroup
\\begin{lstlisting}[language=Python]
# a comment
\\end{lstlisting}
\\endgroup

\\section{Bar}
\\begin{lstlisting}[language=C]
#include <stdio.h>
void main (int argc, char **argv)
{
  fprintf (stderr, \"hi\\n\");
}
\\end{lstlisting}

\\section{Baz}
\\begingroup
\\raggedright
\\begin{lstlisting}[language=Python]
# a comment
def flatten(lst):
  return [item for sublist in lst for item in sublist]
\\end{lstlisting}
\\endgroup

\\end{document}
"))
     (multi-lang-tests-doit ".tex" (replace-regexp-in-string "^\n" "" text)
       (should (eq major-mode 'latex-mode))
       (dolist (item (collect-regions))
         (save-excursion
           (cl-destructuring-bind ((beg . end) lang)
	       item
	     (delete-multi-lang-overlay beg)
	     (when-let ((mode (assoc-default
                               lang '(("Python" . python-mode)
		                      ("C" . c-mode)))))
               (make-multi-lang-overlay beg end mode)))))
       (search-forward "section")
       (backward-word)
       (should (eq (get-text-property (point) 'face)
                   'font-lock-keyword-face))
       (search-forward "a comment")
       (backward-word)
       (should (eq (get-text-property (point) 'face)
                   'font-lock-comment-face))
       (search-forward "hi\\n")
       (backward-word)
       (should (eq (get-text-property (point) 'face)
                   'font-lock-string-face))
       (search-forward "flatten")
       (backward-word)
       (should (eq (get-text-property (point) 'face)
                   'font-lock-function-name-face))))))

(defun string-of (node)
  (buffer-substring-no-properties
   (tree-sitter-node-start node)
   (tree-sitter-node-end node)))

(defun children-of (node)
  (mapcar (lambda (k) (tree-sitter-node-child node k))
	  (number-sequence 0 (1- (tree-sitter-node-child-count node)))))

(defun next-sib-until (node until)
  (while (and node (not (funcall until node)))
    (setq node (tree-sitter-node-next-sibling node)))
  node)

(defmacro walk-tree (form)
  `(nreverse
    (cl-loop with queue = (list (tree-sitter-root-node))
	     while queue
	     for node = (pop queue)
	     append (prog1 ,form
                      (dotimes (i (tree-sitter-node-child-count node))
			(push (tree-sitter-node-child node i) queue))))))

(defun collect-regions ()
  (walk-tree
   (when-let ((word-p (equal (tree-sitter-node-type node) "word"))
	      (word-text (string-of node))
	      (parent (tree-sitter-node-parent node))
	      (lstlisting-p (and (equal word-text "lstlisting")
				 (equal "begin" (tree-sitter-node-type parent))))
	      (bracket-text (cl-some (lambda (child)
				       (when-let ((bracket-group-p (equal "bracket_group" (tree-sitter-node-type child)))
						  (bracket-text (string-of child))
						  (language-p (string-prefix-p "[language=" bracket-text)))
					 bracket-text))
				     (children-of parent)))
	      (language (save-match-data
			  (when (string-match "=\\(.+\\)]$" bracket-text)
			    (match-string 1 bracket-text))))
	      (body (tree-sitter-node-next-sibling parent))
	      (newline (save-excursion
			 (goto-char (tree-sitter-node-end parent))
			 (when (zerop (forward-line 1))
			   (when (<= (line-beginning-position)
				     (tree-sitter-node-start body))
			     (line-beginning-position)))))
	      (newline2 (tree-sitter-node-start
                         (next-sib-until
                          body
                          (lambda (next)
                            (and (equal "end" (tree-sitter-node-type next))
                                 (cl-some (lambda (child)
                                            (and (equal "word" (tree-sitter-node-type child))
                                                 (equal "lstlisting" (string-of child))))
                                          (children-of next))))))))
     (list `(,(cons (set-marker (make-marker) newline)
                    (set-marker (make-marker) newline2))
             ,language)))))

(provide 'multi-lang-tests)
;;; multi-lang-tests.el ends here
