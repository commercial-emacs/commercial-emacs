;;; mode-overlay-tests.el --- tests for lisp/mode-overlay.el         -*- lexical-binding: t; -*-

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
(require 'js)
(require 'mode-overlay)
(declare-function tree-sitter--testable "tree-sitter.c")

(defsubst tree-sitter-testable (lang)
  (when-let ((dylib (expand-file-name
                     (concat lang ".so")
                     (expand-file-name "lib" tree-sitter-resources-dir))))
    (tree-sitter--testable dylib)))

(defmacro tree-sitter-tests-with-resources-dir (lang &rest body)
  (declare (indent defun))
  `(let ((tree-sitter-resources-dir
          (expand-file-name (concat (file-name-as-directory "src/tree-sitter-resources")
                                    (if (eq system-type 'darwin)
                                        "darwin"
                                      "")))))
     (skip-unless (tree-sitter-testable ,lang))
     ,@body))

(defmacro mode-overlay-tests-doit (ext text &rest body)
  (declare (indent defun))
  `(let ((dir (make-temp-file "mode-overlay-tests" t)))
     (unwind-protect
         (let ((file-name (expand-file-name (concat "mode-overlay-tests" ,ext) dir))
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

(ert-deftest mode-overlay-point-max ()
  (with-temp-buffer
    (should-error (call-interactively #'make-mode-overlay))))

(ert-deftest mode-overlay-point-min ()
  (with-temp-buffer
    (save-excursion (insert "\n"))
    (should (= (buffer-size) 1))
    (make-mode-overlay 1 1 'js-mode)
    (should (= (buffer-size) 2)) ;newline should have been added
    (should (get-text-property 1 'read-only))
    (should (get-text-property 1 'rear-nonsticky))
    (should (get-text-property 2 'read-only))
    (should-not (get-text-property 2 'rear-nonsticky))))

(ert-deftest mode-overlay-read-only ()
  (with-temp-buffer
    (save-excursion
      (insert "foo\n"))
    (setq buffer-read-only t)
    (should-error (make-mode-overlay 1 3 'js-mode))
    (should-not (cl-some (lambda (b)
                           (cl-search "[js-mode]" (buffer-name b)))
                         (buffer-list)))))

(ert-deftest mode-overlay-error-recovery ()
  (with-temp-buffer
    (save-excursion
      (insert "foo\n"))
    (make-mode-overlay 1 3 'js-mode)
    (should (cl-some (lambda (b)
                       (cl-search "[js-mode]" (buffer-name b)))
                     (buffer-list)))
    (should-error (make-mode-overlay 1 3 'python-mode) :type 'user-error)
    (should (cl-some (lambda (b)
                       (cl-search "[js-mode]" (buffer-name b)))
                     (buffer-list)))
    (should-not (cl-some (lambda (b)
                           (cl-search "[python-mode]" (buffer-name b)))
                         (buffer-list)))))

(ert-deftest mode-overlay-test-org ()
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
    (mode-overlay-tests-doit ".org" (replace-regexp-in-string "^\n" "" text)
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
          (make-mode-overlay beg end mode)))
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

(ert-deftest mode-overlay-test-interactive ()
  "Test interactive use."
  (let ((text "
\\usepackage{listings}
\\begin{lstlisting}[language=Python]
def my_function(x, y):
    return x + y
\\end{lstlisting}
"))
    (mode-overlay-tests-doit ".tex" (replace-regexp-in-string "^\n" "" text)
      (should (eq major-mode 'plain-tex-mode))
      (search-forward "lstlisting")
      (backward-word)
      (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))
      (search-forward "def")
      (backward-word)
      (should-not (get-text-property (point) 'face))
      (search-forward "my_f")
      (backward-word)
      (should (eq (char-after) ?f))
      (should (equal (get-text-property (point) 'face) '(subscript)))
      (should (get-text-property (point) 'display))
      (let* ((base (current-buffer))
             (ov (make-mode-overlay
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "def")
                    (line-beginning-position))
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "\\end")
                    (line-beginning-position))
                  'python-mode))
             (indirect (overlay-buffer ov)))
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
           (should (eq (char-after) ?f))
           (should-not (equal (get-text-property (point) 'face) '(subscript)))
           (should-not (get-text-property (point) 'display))))))))

(ert-deftest mode-overlay-test-tree-walk ()
  (tree-sitter-tests-with-resources-dir "latex"
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
     (mode-overlay-tests-doit ".tex" (replace-regexp-in-string "^\n" "" text)
       (should (eq major-mode 'latex-mode))
       (save-excursion
         (goto-char (point-min))
         (my/walk-tree
          (lambda (node)
            (when-let ((word-p (equal (tree-sitter-node-type node) "word"))
	               (word-text (my/string-of node))
	               (parent (tree-sitter-node-parent node))
	               (lstlisting-p (and (equal word-text "lstlisting")
			                  (equal "begin" (tree-sitter-node-type parent))))
	               (bracket-text (cl-some (lambda (child)
				                (when-let ((bracket-group-p (equal "bracket_group" (tree-sitter-node-type child)))
					                   (bracket-text (my/string-of child))
					                   (language-p (string-prefix-p "[language=" bracket-text)))
				                  bracket-text))
				              (my/children-of parent)))
	               (lang (save-match-data
		               (when (string-match "=\\(.+\\)]$" bracket-text)
		                 (match-string 1 bracket-text))))
                       (mode (assoc-default
                              lang '(("Python" . python-mode)
		                     ("C" . c-mode)
                                     ("Emacs Lisp" . emacs-lisp-mode))))
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
                                                          (equal "lstlisting" (my/string-of child))))
                                                   (my/children-of next))))))))
              (make-mode-overlay
               (set-marker (make-marker) newline)
               (set-marker (make-marker) newline2)
               mode)))))
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

(ert-deftest mode-overlay-test-vue ()
  (tree-sitter-tests-with-resources-dir "html"
   (let ((tree-sitter-mode-alist `((html-mode . "html") . ,tree-sitter-mode-alist))
         (auto-mode-alist '(("\\.vue\\'" . html-mode)))
         (text "
<script>export default { // same line to check newline insertion
    // a comment
    data() { return { count: 0 } }
}
</script>
<template>
  <button>Count is: {{ count }}</button>
</template>
<script>
export default {
    mounted() {
	console.log(`The initial count is ${this.count}.`)
    }
</script>
"))
     (mode-overlay-tests-doit ".vue" (replace-regexp-in-string "^\n" "" text)
       (should (eq major-mode 'html-mode))
       (save-excursion
         (search-forward "a comment")
         (backward-word)
         (should-not (get-text-property (point) 'face))
         (search-forward "initial count")
         (backward-word)
         (should-not (get-text-property (point) 'face)))
       (my/walk-tree
        (lambda (node)
          (when (and (equal "end_tag" (tree-sitter-node-type node))
	             (equal "script_element" (tree-sitter-node-type
					      (tree-sitter-node-parent node))))
            (let ((raw-text node))
	      (while (and raw-text
		          (not (equal "raw_text"
				      (tree-sitter-node-type
				       (setq raw-text
				             (tree-sitter-node-prev-sibling
                                              raw-text)))))))
	      (make-mode-overlay
	       (tree-sitter-node-start raw-text)
	       (tree-sitter-node-start node)
	       'js-mode)))))
       (search-forward "a comment")
       (backward-word)
       (should (eq (get-text-property (point) 'face)
                   'font-lock-comment-face))
       (search-forward "<script>")
       (should-not (overlays-at (point)))
       (forward-line 1)
       (should (mode-overlay-p (car (overlays-at (point)))))
       (search-forward "initial count")
       (backward-word)
       (should (eq (get-text-property (point) 'face)
                   'font-lock-string-face))))))

(defun my/string-of (node)
  (buffer-substring-no-properties
   (tree-sitter-node-start node)
   (tree-sitter-node-end node)))

(defun my/children-of (node)
  (mapcar (lambda (k) (tree-sitter-node-child node k))
	  (number-sequence 0 (1- (tree-sitter-node-child-count node)))))

(defun next-sib-until (node until)
  (while (and node (not (funcall until node)))
    (setq node (tree-sitter-node-next-sibling node)))
  node)

(defun my/walk-tree (doit)
  (save-excursion
    (goto-char (point-min))
    (cl-loop with c = (tree-sitter-cursor-at)
	     do (my/dfs (prog2 (while (tree-sitter-goto-parent c)) c)
                        doit)
	     and do (while (tree-sitter-goto-parent c))
	     while (when-let ((next (tree-sitter-node-next-sibling
				     (tree-sitter-node-of c))))
		     (goto-char (tree-sitter-node-start next))
		     (setq c (tree-sitter-cursor-at))))))

(defun my/dfs (c doit)
  (when-let ((node (tree-sitter-node-of c)))
    (funcall doit node)
    (when (tree-sitter-goto-first-child c)
      (my/dfs c doit)
      (tree-sitter-goto-parent c))
    (when (tree-sitter-goto-next-sibling c)
      (my/dfs c doit))))

(provide 'mode-overlay-tests)
;;; mode-overlay-tests.el ends here
