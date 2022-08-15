;;; tree-sitter.el --- tree-sitter utilities -*- lexical-binding: t -*-

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

;;; Commentary:

;;; Code:

(require 'font-lock)
(declare-function tree-sitter-changed-range "tree-sitter.c")
(declare-function tree-sitter-highlight-region "tree-sitter.c")
(declare-function tree-sitter-node-prev-sibling "tree-sitter.c")
(declare-function tree-sitter-node-end "tree-sitter.c")
(declare-function tree-sitter-node-at "tree-sitter.c")
(declare-function tree-sitter-node-parent "tree-sitter.c")
(declare-function tree-sitter-goto-parent "tree-sitter.c")
(declare-function tree-sitter-node-equal "tree-sitter.c")
(declare-function tree-sitter-goto-next-sibling "tree-sitter.c")
(declare-function tree-sitter-node-of "tree-sitter.c")
(declare-function tree-sitter-cursor-at "tree-sitter.c")
(declare-function tree-sitter-node-start "tree-sitter.c")

(defgroup tree-sitter
  nil
  "Tree-sitter is an incremental parser."
  :group 'tools)

(defcustom tree-sitter-resources-dir
  (if-let ((interactive (not noninteractive))
           (proper-dir
            (ignore-errors
              (file-name-directory
               (directory-file-name
                (with-temp-buffer
                  (let ((proc (start-process "tree-sitter-resources-dir"
                                             (current-buffer) "tree-sitter" "dump-libpath")))
                    (cl-loop repeat 10
                          while (process-live-p proc)
                          do (sleep-for 0 100)
                          finally (when (process-live-p proc) (kill-process proc)))
                    (car (split-string (buffer-substring-no-properties
                                        (point-min) (point-max)))))))))))
      proper-dir
    (concat (file-name-as-directory
             (or (getenv "XDG_CACHE_HOME")
                 (expand-file-name ".cache" "~")))
            "tree-sitter"))
  "Follows dirs::cache_dir in the Rust dirs crate.
On Linux systems this is $XDG_CACHE_HOME/tree-sitter."
  :group 'tree-sitter
  :type 'directory
  :risky t
  :version "28.1")

(defcustom tree-sitter-mode-alist
  '((c++-mode . "cpp")
    (rust-mode . "rust")
    (rustic-mode . "rust")
    (emacs-lisp-mode . "elisp")
    (csharp-mode . "sharp")
    (haskell-mode . "haskell")
    (go-mode . "go")
    (sh-mode . "bash")
    (c-mode . "c")
    (html-mode . "html")
    (makefile-gmake-mode . "make")
    (makefile-mode . "make")
    (php-mode . "php")
    (ess-mode . "r")
    (sql-mode . "sql")
    (conf-toml-mode . "toml")
    (vhdl-mode . "vhdl")
    (java-mode . "java")
    (js-mode . "javascript")
    (python-mode . "python")
    (ruby-mode . "ruby")
    (tree-sitter-ruby-mode . "ruby")
    (tree-sitter-elisp-mode . "elisp"))
  "Map prog-mode to tree-sitter grammar."
  :group 'tree-sitter
  :type '(alist :key-type (symbol :tag "Prog mode")
                :value-type (string :tag "Tree-sitter symbol"))
  :risky t
  :version "28.1")

(defcustom tree-sitter-highlight-alist
  '(("constant" . font-lock-constant-face)
    ("type.builtin" . font-lock-type-face)
    ("operator" . font-lock-builtin-face)
    ("variable.parameter" . font-lock-variable-name-face)
    ("function.builtin" . font-lock-function-name-face)
    ;; ("punctuation.delimiter" . font-lock-constant-face)
    ("attribute" . font-lock-variable-name-face)
    ;; ("punctuation.bracket" . font-lock-constant-face)
    ("string" . font-lock-string-face)
    ("variable.builtin" . font-lock-builtin-face)
    ("comment" . font-lock-comment-face)
    ("number" . font-lock-constant-face)
    ("type" . font-lock-type-face)
    ("embedded" . font-lock-builtin-face)
    ("function" . font-lock-function-name-face)
    ("keyword" . font-lock-keyword-face)
    ("constructor" . font-lock-function-name-face)
    ("property" . font-lock-variable-name-face)
    ("tag" . font-lock-type-face)
    ("string.special" . font-lock-string-face)
    ("constant.builtin" . font-lock-constant-face))
  "Map tree-sitter highlight name to font lock face."
  :group 'tree-sitter
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (mapc (lambda (x)
                 (when (listp (cdr x))
                   (setcdr x (cadr x))))
               value)
         (if-let ((problem (seq-find (lambda (x)
                                       (or (not (stringp (car x)))
                                           (not (facep (cdr x)))))
                                     value)))
             (error "Bad setting %S" problem)
           (set-default symbol value)))
  :type '(alist :key-type (string :tag "Tree-sitter highlight")
                :value-type (symbol :tag "Font lock face"))
  :risky t
  :version "28.1")

(defun tree-sitter-do-fontify (pos)
  "Analog to `jit-lock-fontify-now' without all the indirection."
  (let ((end (or (text-property-any pos (point-max) 'fontified t)
                 (point-max))))
    (font-lock-fontify-region pos end font-lock-verbose)))

(define-minor-mode tree-sitter-lock-mode
  "Tree-sitter font-lock minor mode."
  :lighter nil
  (unless (assq major-mode tree-sitter-mode-alist)
    (setq tree-sitter-lock-mode nil))
  (if tree-sitter-lock-mode
      (progn
        (setq-local font-lock-fontify-region-function #'tree-sitter-fontify-region)
        (add-hook 'font-lock-mode-hook #'font-lock-ensure nil t)
        (add-hook 'fontification-functions #'tree-sitter-do-fontify nil t)
        (add-hook 'after-change-functions #'tree-sitter-douse-fontify nil t))
    (kill-local-variable 'font-lock-fontify-region-function)
    (remove-hook 'font-lock-mode-hook #'font-lock-ensure t)
    (remove-hook 'fontification-functions #'tree-sitter-do-fontify t)
    (remove-hook 'after-change-functions #'tree-sitter-douse-fontify t)))

(defun tree-sitter-douse-fontify (beg _end old-len)
  "As was the case for `jit-lock-after-change', I repeat,
Make sure we change at least one char (in case of deletions).

Undo, because it restores a non-nil Qfontified prop, requires
similar goosing so that handle_fontified_prop() sees the
NILP(prop) it needs to proceed."
  (when (or (> old-len 0) undo-in-progress)
    (put-text-property beg (min (point-max) (1+ beg)) 'fontified nil)))

(defun tree-sitter-fontify-region (beg end loudly)
  "Presumably widened in `font-lock-fontify-region'."
  (let ((inhibit-point-motion-hooks t))
    (with-silent-modifications
      (save-excursion
        (save-match-data
          (let* ((changed-range (tree-sitter-changed-range))
                 (beg* (if changed-range
                           (min beg (cl-first changed-range))
                         beg))
                 (end* (if changed-range
                           (min (point-max)
                                (max (cl-second changed-range) end))
                         end))
                 (bounds (tree-sitter-highlight-region beg* end*))
                 (leftmost (if bounds (min beg* (car bounds)) beg*))
                 (rightmost (if bounds (max end* (cdr bounds)) end*)))
            (prog1 leftmost
              (put-text-property leftmost rightmost 'fontified t)
              (when loudly
                (princ (format "initial [%d %d], inputs [%d %d] final [%d %d]\n"
                               beg end
                               beg* end*
                               leftmost rightmost)
                       #'external-debugging-output)))))))))

(defun tree-sitter-forward-cursor (&optional arg)
  "Return character position of node ARG siblings from point.
Upon reaching a final sibling, proceed from the parent node.
This is rather useless without an upstream
tree-sitter-goto-prev-sibling."
  (unless (fixnump arg) (setq arg 1))
  (let* ((cursor (tree-sitter-cursor-at))
         (done
          (catch 'done
            (dotimes (_i arg cursor)
              (unless (catch 'next
                        (while t
                          (if-let ((current (tree-sitter-node-of cursor))
                                   (next (tree-sitter-node-of
                                          (tree-sitter-goto-next-sibling cursor))))
                              (if (tree-sitter-node-equal current next)
                                  (let ((parent (tree-sitter-node-of
                                                 (tree-sitter-goto-parent cursor))))
                                    (when (tree-sitter-node-equal current parent)
                                      (throw 'next nil)))
                                (throw 'next next))
                            (throw 'next nil))))
                (throw 'done nil))))))
    (or (tree-sitter-node-start (tree-sitter-node-of done))
        (point-max))))

(defun tree-sitter-sexp-at (&optional pos)
  (unless (fixnump pos) (setq pos (point)))
  (funcall (if (memq (syntax-class (syntax-after pos)) '(4 5))
               #'tree-sitter-node-parent
             #'identity)
           (tree-sitter-node-at pos)))

(defun tree-sitter-node-preceding (node)
  "Left then up."
  (unless (eq (point) (point-min))
    (if-let ((prev (tree-sitter-node-prev-sibling node)))
        prev
      (cl-loop with prev = node
               for parent = (tree-sitter-node-parent prev)
               until (not parent)
               do (setq prev (tree-sitter-node-prev-sibling parent))
               until prev
               finally return (or prev node)))))

(defun tree-sitter-forward-sexp (&optional arg)
  "Candidate for `forward-sexp-function'.
Move point forward an ARG number of balanced expressions.
Return the number of unsatisfiable iterations."
  (unless (fixnump arg) (setq arg 1))
  (let ((ntimes (abs arg))
        (func
         (if (>= arg 0)
             #'tree-sitter-node-end
           (lambda (node)
             (when-let ((found (tree-sitter-node-preceding node)))
               (tree-sitter-node-start found))))))
    (catch 'done
      (prog1 0
        (dotimes (i ntimes)
          (if-let ((c (funcall func (tree-sitter-sexp-at))))
              (goto-char c)
            (throw 'done (- ntimes i))))))))

(defun tree-sitter-node-round-up (pos)
  (let ((node (tree-sitter-node-at pos)))
    (while (and node (<= pos (tree-sitter-node-start node)))
      (setq node (tree-sitter-node-preceding node)))
    node))

(defun tree-sitter-node-round-down (pos)
  (let ((node (tree-sitter-node-at pos)))
    (when (and node (< pos (tree-sitter-node-start node)))
      (setq node (tree-sitter-node-first-child-for-byte node pos)))
    node))

(defun tree-sitter--traverse-defun (beginning-p &optional arg)
  "Two mutually sensitive polarities.

BEGINNING-P   ARG         MOTION
-----------   --------    ---------
          t   positive    backwards
          t   negative    forwards
        nil   positive    forwards
        nil   negative    backwards
"
  (setq arg (or arg 1))
  (let* ((forward-p (or (and beginning-p (< arg 0))
                        (and (not beginning-p) (> arg 0))))
         (ntimes (abs arg))
         (root-node (tree-sitter-root-node))
         (node (if forward-p
                   (tree-sitter-node-round-down (point))
                 (tree-sitter-node-round-up (point)))))
    (catch 'done
      (prog1 0
        (dotimes (i ntimes)
          (unless node
            (throw 'done (- ntimes i)))
          (while (not (tree-sitter-node-equal
                       root-node
                       (tree-sitter-node-parent node)))
            (setq node (tree-sitter-node-parent node)))
          (goto-char (if beginning-p
                         (tree-sitter-node-start node)
                       (tree-sitter-node-end node)))
          (setq node (if forward-p
                         (tree-sitter-node-next-sibling node)
                       (tree-sitter-node-prev-sibling node))))))))

(defun tree-sitter-beginning-of-defun (&optional arg)
  "Candidate for `beginning-of-defun-function'."
  (tree-sitter--traverse-defun t arg))

(defun tree-sitter-end-of-defun (&optional arg)
  "Candidate for `end-of-defun-function'."
  (tree-sitter--traverse-defun nil arg))

(provide 'tree-sitter)

;;; tree-sitter.el ends here
