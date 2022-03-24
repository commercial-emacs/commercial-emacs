;;; ediff-ptch-tests.el --- Tests for ediff-ptch.el  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2022 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>

;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'ediff-ptch)
(require 'ediff-diff) ; For `ediff-diff-program'.
(eval-when-compile (require 'cl-lib))

(ert-deftest ediff-ptch-test-bug25010 ()
  "Test for https://debbugs.gnu.org/25010 ."
  (with-temp-buffer
    (insert "diff --git a/lisp/vc/ediff-ptch.el b/lisp/vc/ediff-ptch.el
index 6a07f80..6e8e947 100644
--- a/lisp/vc/ediff-ptch.el
+++ b/lisp/vc/ediff-ptch.el
@@ -120,11 +120,12 @@ ediff-patch-default-directory
")
    (goto-char (point-min))
    (let ((filename
           (progn
             (re-search-forward ediff-context-diff-label-regexp nil t)
             (match-string 1))))
      (should-not (string-suffix-p "@@" filename)))))


(ert-deftest ediff-ptch-test-bug26084 ()
  "Test for https://debbugs.gnu.org/26084 ."
  (skip-unless (executable-find "git"))
  (skip-unless (executable-find ediff-patch-program))
  (ert-with-temp-directory tmpdir
    (ert-with-temp-file patch
      (let* ((default-directory (file-name-as-directory tmpdir))
             (qux (expand-file-name "qux.txt" tmpdir))
             (bar (expand-file-name "bar.txt" tmpdir))
             (git-program (executable-find "git")))
        ;; Create repository.
        (with-temp-buffer
          (insert "qux here\n")
          (write-region nil nil qux nil 'silent)
          (erase-buffer)
          (insert "bar here\n")
          (write-region nil nil bar nil 'silent))
        (call-process git-program nil nil nil "init")
        (call-process git-program nil nil nil "add" ".")
        (call-process git-program nil nil nil "commit" "-m" "Test repository.")
        ;; Update repo., save the diff and reset to initial state.
        (with-temp-buffer
          (insert "foo here\n")
          (write-region nil nil qux nil 'silent)
          (write-region nil nil bar nil 'silent))
        (call-process git-program nil `(:file ,patch) nil "diff")
        (call-process git-program nil nil nil "reset" "--hard" "HEAD")
        ;; Visit the diff file i.e., patch; extract from it the parts
        ;; affecting just each of the files: store in patch-bar the part
        ;; affecting 'bar', and in patch-qux the part affecting 'qux'.
        (find-file patch)
        (let* ((info
                (progn (ediff-map-patch-buffer (current-buffer)) ediff-patch-map))
               (patch-bar
                (buffer-substring-no-properties
                 (car (nth 3 (car info)))
                 (car (nth 4 (car info)))))
               (patch-qux
                (buffer-substring-no-properties
                 (car (nth 3 (cadr info)))
                 (car (nth 4 (cadr info))))))
          ;; Apply both patches.
          (dolist (x (list (cons patch-bar bar) (cons patch-qux qux)))
            (with-temp-buffer
              ;; Some windows variants require the option '--binary'
              ;; in order to 'patch' create backup files.
              (let ((opts (format "--backup%s"
                                  (if (memq system-type '(windows-nt ms-dos))
                                      " --binary" ""))))
                (insert (car x))
                (call-process-region (point-min)
                                     (point-max)
                                     ediff-patch-program
                                     nil nil nil
                                     opts (cdr x)))))
          ;; Check backup files were saved correctly; in Bug#26084 some
          ;; of the backup files are overwritten with the actual content
          ;; of the updated file.  To ensure that the bug is fixed we just
          ;; need to check that every backup file produced has different
          ;; content that the current updated file.
          (dolist (x (list qux bar))
            (let ((backup
                   (car
                    (directory-files
                     tmpdir 'full
                     (concat (file-name-nondirectory x) ".")))))
              ;; Compare files only if the backup has being created.
              (when backup
                (should-not
                 (string= (with-temp-buffer
                            (insert-file-contents x)
                            (buffer-string))
                          (with-temp-buffer
                            (insert-file-contents backup)
                            (buffer-string))))))))))))

(ert-deftest ediff-ptch-test-bug26028 ()
  "Test for http://debbugs.gnu.org/26028 ."
  (skip-unless (executable-find "git"))
  (skip-unless (executable-find ediff-patch-program))
  (skip-unless (executable-find ediff-diff-program))
  (let ((git-program (executable-find "git"))
        (default-dir default-directory)
        tmpdir buffers)
    ;;; Simple patch: old/src/hello.c /new/src/hello.c
    (unwind-protect
        (let* ((dir (make-temp-file "multipatch-test" t))
               (file1 (expand-file-name "old/src/hello.c" dir))
               (file2 (expand-file-name "new/src/hello.c" dir))
               (patch (expand-file-name "tmp.patch" dir))
               (default-directory (file-name-as-directory dir)))
          (setq tmpdir dir)
          (make-directory (expand-file-name "old/src/" dir) 'parents)
          (make-directory (expand-file-name "new/src/" dir) 'parents)
          (with-temp-buffer
            (insert "void main() { }\n")
            (write-region nil nil file1 nil 'silent)
            (erase-buffer)
            (insert "int main() { return 0; }\n")
            (write-region nil nil file2 nil 'silent)
            (erase-buffer)
            (call-process ediff-diff-program nil t nil "-cr" "old" "new")
            (write-region nil nil patch nil 'silent)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (x) nil))
                      ((symbol-function 'ediff-prompt-for-patch-file)
                       (lambda (&rest x) (find-file-noselect patch)))
                      ((symbol-function 'read-file-name) (lambda (x1 x2 x3 x4 x5) x5))
                      ((symbol-function 'ediff-dispatch-file-patching-job)
                       (lambda (x y) y)))
              (should (equal (file-relative-name file1) (epatch nil patch)))
              (push (get-file-buffer patch) buffers))))
      (when (file-exists-p tmpdir)
        (setq default-directory default-dir)
        (delete-directory tmpdir 'recursive))
      (mapc (lambda (b)
              (when (buffer-live-p b) (kill-buffer b)))
            buffers)
      (setq buffers nil))
    ;;; Simple Git patch: proj/src/hello.c
    (unwind-protect
        (let* ((dir (make-temp-file "multipatch-test" t))
               (rootdir (expand-file-name "proj/src/" dir))
               (file (expand-file-name "hello.c" rootdir))
               (patch (expand-file-name "tmp.patch" dir))
               (default-directory (file-name-as-directory rootdir)))
          (make-directory rootdir 'parents)
          (setq tmpdir dir)
          (with-temp-buffer
            (insert "void main() { }\n")
            (write-region nil nil file nil 'silent)
            (call-process git-program nil nil nil "init")
            (call-process git-program nil nil nil "add" ".")
            (call-process git-program nil nil nil "commit" "-m" "test repository.")
            (erase-buffer)
            (insert "int main() { return 0; }\n")
            (write-region nil nil file nil 'silent)
            (call-process git-program nil `(:file ,patch) nil "diff")
            (call-process git-program nil nil nil "reset" "--hard" "head")
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (x) nil))
                      ((symbol-function 'ediff-prompt-for-patch-file)
                       (lambda (&rest x) (find-file-noselect patch)))
                      ((symbol-function 'read-file-name) (lambda (&rest x) file))
                      ((symbol-function 'read-file-name) (lambda (x1 x2 x3 x4 x5) x5))
                      ((symbol-function 'ediff-dispatch-file-patching-job)
                       (lambda (x y) y)))
              (should (equal file (epatch nil patch)))))
          (push (get-file-buffer patch) buffers))
      ;; clean up
      (when (file-exists-p tmpdir)
        (setq default-directory default-dir)
        (delete-directory tmpdir 'recursive))
      (mapc (lambda (b)
              (when (buffer-live-p b) (kill-buffer b)))
            buffers)
      (setq buffers nil))
    ;;; Git multipatch.
    (unwind-protect
        (let* ((dir (make-temp-file "multipatch-test" t))
               (file1 (expand-file-name "proj/src/hello.c" dir))
               (file2 (expand-file-name "proj/src/bye.c" dir))
               (file3 (expand-file-name "proj/lisp/foo.el" dir))
               (file4 (expand-file-name "proj/lisp/bar.el" dir))
               (file5 (expand-file-name "proj/etc/news" dir))
               (patch (expand-file-name "tmp.patch" dir))
               (default-directory (expand-file-name "proj" dir)))
          (setq tmpdir dir)
          (dolist (d '("src" "lisp" "etc"))
            (setq rootdir (expand-file-name (concat "proj/" d) dir))
            (make-directory rootdir 'parents))
          (with-temp-buffer
            (insert "void main() { }\n")
            (write-region nil nil file1 nil 'silent)
            (write-region nil nil file2 nil 'silent)
            (erase-buffer)
            (insert "(defun foo () nil)\n")
            (write-region nil nil file3 nil 'silent)
            (erase-buffer)
            (insert "(defun bar () nil)\n")
            (write-region nil nil file4 nil 'silent)
            (erase-buffer)
            (insert "new functions 'foo' and 'bar'\n")
            (write-region nil nil file5 nil 'silent)
            (call-process git-program nil nil nil "init")
            (call-process git-program nil nil nil "add" "src" "lisp" "etc")
            (call-process git-program nil nil nil "commit" "-m" "test repository.");)
            (erase-buffer)
            (insert "int main() { return 0;}\n")
            (write-region nil nil file1 nil 'silent)
            (write-region nil nil file2 nil 'silent)
            (erase-buffer)
            (insert "(defun qux () nil)\n")
            (write-region nil nil file3 nil 'silent)
            (erase-buffer)
            (insert "(defun quux () nil)\n")
            (write-region nil nil file4 nil 'silent)
            (erase-buffer)
            (insert "new functions 'qux' and 'quux'\n")
            (write-region nil nil file5 nil 'silent)
            (call-process git-program nil `(:file ,patch) nil "diff")
            (call-process git-program nil nil nil "reset" "--hard" "head"))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (x) nil))
                    ((symbol-function 'ediff-get-patch-file) (lambda (&rest x) patch))
                    ((symbol-function 'read-file-name) (lambda (&rest x) patch)))
            (epatch nil patch)
            (with-current-buffer "*Ediff Session Group Panel*"
              (push (get-file-buffer patch) buffers)
              (should (= 5 (length (cdr ediff-meta-list))))
              ;; don't ask confirmation to exit.
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (x) t)))
                (ediff-quit-meta-buffer)))))
      ;; clean up
      (when (file-exists-p tmpdir)
        (setq default-directory default-dir)
        (delete-directory tmpdir 'recursive))
      (when ediff-registry-buffer
        (push ediff-registry-buffer buffers))
      (mapc (lambda (b)
              (when (buffer-live-p b) (kill-buffer b)))
            buffers)
      (setq buffers nil))))


(provide 'ediff-ptch-tests)
;;; ediff-ptch-tests.el ends here
