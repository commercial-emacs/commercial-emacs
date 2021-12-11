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

(declare-function tree-sitter-highlights "tree-sitter.c")
(declare-function tree-sitter--testable "tree-sitter.c")

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
             (should-not (text-property-any (point-min) (point-max) 'fontified t))
             (let (noninteractive)
               (turn-on-font-lock))
             (should font-lock-mode)
             (should tree-sitter-lock-mode)
             (should-not (text-property-any (point-min) (point-max) 'fontified nil))
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
    (tree-sitter-tests-doit ".c" text
      (should (equal (tree-sitter-highlights (point-min) (point-max))
                     '(font-lock-type-face (1 . 5) nil (5 . 6) font-lock-function-name-face (6 . 10) nil (10 . 12) font-lock-type-face (12 . 16) nil (16 . 22) font-lock-keyword-face (22 . 28) nil (28 . 29) font-lock-constant-face (29 . 30) nil (30 . 33))))
      (goto-char (point-min))
      (forward-line 1)
      (insert "\n  printf(\"hello world\");\n")
      (should (equal (tree-sitter-highlights (point-min) (point-max))
                     '(font-lock-type-face (1 . 5) nil (5 . 6) font-lock-function-name-face (6 . 10) nil (10 . 12) font-lock-type-face (12 . 16) nil (16 . 23) font-lock-function-name-face (23 . 29) nil (29 . 30) font-lock-string-face (30 . 43) nil (43 . 48) font-lock-keyword-face (48 . 54) nil (54 . 55) font-lock-constant-face (55 . 56) nil (56 . 59)))))))

(ert-deftest tree-sitter-multibyte ()
  "Cannot simply -1 or +1 to move between buffer and sitter space."
  (let ((text "
void main (void) {
  printf (\"早晨, 你好\");
  return 0;
}
"))
    (tree-sitter-tests-doit ".c" text
      (should t))))





(ert-deftest tree-sitter-bog-customize-option ()
  "When tree-sitter highlighting was implemented as `after-change-functions',
customizing an option bogged (because `tree-sitter-highlight-region' was called
for each change in the insufferable `custom-save-variables.')"
  (let ((text "
(custom-set-variables
 '(c-basic-offset 'set-from-style))
"))
    (tree-sitter-tests-doit ".el" text
      (save-buffer)
      (should (equal font-lock-fontify-region-function
                     #'tree-sitter-fontify-region))
      (let* ((user-init-file (file-truename (buffer-file-name)))
             (custom-file user-init-file)
             negative positive
             (record (lambda (f &rest args)
                       (cl-letf (((symbol-function 'tree-sitter-fontify-region)
                                  (lambda (&rest _args) (setq negative t))))
                         (apply f args)))))
        (add-hook 'after-change-functions
                  (lambda (beg end _len)
                    (when (cl-search "custom-set-variables"
                                     (buffer-substring-no-properties
                                      beg end))
                      (setq positive t)))
                  nil t)
        (load-file user-init-file)
        (unwind-protect
            (progn
              (add-function
               :around
               (symbol-function 'princ)
               record)
              (custom-set-variables '(font-lock-support-mode 'tree-sitter-lock-mode))
              (custom-save-all)
              (with-temp-buffer
                (insert-file-contents user-init-file)
                (should (cl-search "c-basic-offset" (buffer-substring-no-properties
                                                     (point-min)
                                                     (point-max)))))
              (should positive)
              (should-not negative))
          (remove-function (symbol-function 'insert) record))))))

(ert-deftest tree-sitter-how-fast ()
  "How fast can it fontify xdisp.c"
  (tree-sitter-tests-with-resources-dir
    (cl-flet ((bench
                (file mode reps unfontify fontify)
	        (save-window-excursion
	          (find-file-literally file)
	          (let ((font-lock-support-mode mode)
		        enable-dir-local-variables
		        font-lock-global-modes
		        font-lock-fontified)
	            (set-auto-mode)
	            (cl-letf (((symbol-function 'font-lock-initial-fontify) #'ignore))
                      (let (noninteractive)
		        (turn-on-font-lock)))
                    (should-not (text-property-any (point-min) (point-max) 'fontified t))
	            (unwind-protect
                        (benchmark-run reps
			  (funcall unfontify (point-min) (point-max))
                          (should-not (text-property-any (point-min) (point-max) 'fontified t))
			  (funcall fontify (point-min) (point-max))
                          (should-not (text-property-any (point-min) (point-max) 'fontified nil)))
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
        (should (< fast (/ slow 3)))))))

(provide 'tree-sitter-tests)
;;; tree-sitter-tests.el ends here
