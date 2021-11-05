;;; tree-sitter-tests.el --- tests for src/tree-sitter.c         -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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
(require 'tree-sitter)

(defsubst tree-sitter-testable ()
  (when-let ((dylib (expand-file-name "lib/c.so" tree-sitter-resources-dir)))
    (tree-sitter--testable dylib)))

(defmacro tree-sitter-tests-with-resources-dir (&rest body)
  (declare (indent defun))
  `(let ((tree-sitter-resources-dir (expand-file-name "src/tree-sitter-resources")))
     (skip-unless (tree-sitter-testable))
     ,@body))

(defmacro tree-sitter-tests-doit (ext text &rest body)
  "font-lock setup is highly circular, redundant, and difficult to isolate."
  (declare (indent defun))
  `(tree-sitter-tests-with-resources-dir
     (let ((dir (make-temp-file "tree-sitter-tests" t))
           (text (replace-regexp-in-string "^\n" "" ,text)))
       (unwind-protect
           (let ((font-lock-support-mode 'tree-sitter-lock-mode)
                 (file-name (expand-file-name (concat "tree-sitter-tests" ,ext) dir))
                 font-lock-global-modes ;; only works when not interactive
                 enable-dir-local-variables)
             (with-temp-file file-name (insert text))
             (find-file file-name)
             (should-not (text-property-not-all (point-min) (point-max) 'face nil))
             (let (noninteractive)
               (turn-on-font-lock))
             (should font-lock-mode)
             (should tree-sitter-lock-mode)
             (should (text-property-not-all (point-min) (point-max) 'face nil))
             ,@body)
         (let (kill-buffer-query-functions)
           (kill-buffer))
         (delete-directory dir t)))))

(ert-deftest tree-sitter-basic-parsing ()
  "Test basic parsing routines."
  (let ((text "
void main (void) {
  return 0;
}
"))
    ;; Says ert-deftest:
    ;; Macros in BODY are expanded when the test is defined, not when it
    ;; is run.  If a macro (possibly with side effects) is to be tested,
    ;; it has to be wrapped in `(eval (quote ...))'.
    (ignore text)
    (eval (quote (tree-sitter-tests-doit ".c" text
                   (should (equal (tree-sitter-highlights (point-min) (point-max))
                                  '(font-lock-type-face (1 . 5) nil (5 . 6) font-lock-function-name-face (6 . 10) nil (10 . 12) font-lock-type-face (12 . 16) nil (16 . 22) font-lock-keyword-face (22 . 28) nil (28 . 29) font-lock-constant-face (29 . 30) nil (30 . 33))))
                   (goto-char (point-min))
                   (forward-line 1)
                   (insert "\n  printf(\"hello world\");\n")
                   (should (equal (tree-sitter-highlights (point-min) (point-max))
                                  '(font-lock-type-face (1 . 5) nil (5 . 6) font-lock-function-name-face (6 . 10) nil (10 . 12) font-lock-type-face (12 . 16) nil (16 . 23) font-lock-function-name-face (23 . 29) nil (29 . 30) font-lock-string-face (30 . 43) nil (43 . 48) font-lock-keyword-face (48 . 54) nil (54 . 55) font-lock-constant-face (55 . 56) nil (56 . 59)))))))))

(ert-deftest tree-sitter-how-fast ()
  "How fast can it fontify xdisp.c"
  (eval (quote (tree-sitter-tests-with-resources-dir
                   (cl-flet ((bench
                                 (file mode reps unfontify fontify)
	                       (save-window-excursion
	                         (find-file-literally file)
	                         (let (font-lock-maximum-size
		                       (font-lock-support-mode mode)
		                       enable-dir-local-variables
		                       font-lock-global-modes
		                       font-lock-fontified)
	                           (set-auto-mode)
	                           (cl-letf (((symbol-function 'font-lock-initial-fontify) #'ignore))
                                     (let (noninteractive)
		                       (turn-on-font-lock)))
	                           (unwind-protect
                                        (benchmark-run reps
			                  (funcall unfontify (point-min) (point-max))
			                  (funcall fontify (point-min) (point-max)))
	                             (let (kill-buffer-query-functions)
		                       (kill-buffer)))))))
                     (let ((fast (car (bench (expand-file-name "src/xdisp.c" "..")
                                             'tree-sitter-lock-mode 1
                                             #'font-lock-unfontify-region
                                             #'font-lock-fontify-region)))
                           (slow (car (bench (expand-file-name "src/xdisp.c" "..")
                                             'jit-lock-mode 1
                                             #'jit-lock-refontify
                                             #'jit-lock-fontify-now))))
                       (message "tree-sitter-how-fast: %s versus %s" fast slow)
                       (should (< fast (/ slow 3)))))))))

(provide 'tree-sitter-tests)
;;; tree-sitter-tests.el ends here
