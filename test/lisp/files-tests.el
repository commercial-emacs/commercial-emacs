;;; files-tests.el --- tests for files.el.  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

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
(require 'nadvice)
(eval-when-compile (require 'cl-lib))
(require 'bytecomp) ; `byte-compiler-base-file-name'.
(require 'dired) ; `dired-uncache'.
(require 'filenotify) ; `file-notify-add-watch'.

(defvar native-comp-verbose)
(defvar comp-log-time-report)
(with-eval-after-load 'comp
  (setq native-comp-verbose 1
        comp-log-time-report t))

;; Set to t if the local variable was set, `query' if the query was
;; triggered.
(defvar files-test-result nil)

(defvar files-test-safe-result nil)
(put 'files-test-safe-result 'safe-local-variable 'booleanp)

(defun files-test-fun1 ()
  (setq files-test-result t))

;; Test combinations:
;; `enable-local-variables' t, nil, :safe, :all, or something else.
;; `enable-local-eval' t, nil, or something else.

(defvar files-test-local-variable-data
  ;; Unsafe eval form
  '((("eval: (files-test-fun1)")
     (t t         (eq files-test-result t))
     (t nil       (eq files-test-result nil))
     (t maybe     (eq files-test-result 'query))
     (nil t       (eq files-test-result nil))
     (nil nil     (eq files-test-result nil))
     (nil maybe   (eq files-test-result nil))
     (:safe t     (eq files-test-result nil))
     (:safe nil   (eq files-test-result nil))
     (:safe maybe (eq files-test-result nil))
     (:all t      (eq files-test-result t))
     (:all nil    (eq files-test-result nil))
     (:all maybe  (eq files-test-result t)) ; This combination is ambiguous.
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result nil))
     (maybe maybe (eq files-test-result 'query)))
    ;; Unsafe local variable value
    (("files-test-result: t")
     (t t         (eq files-test-result 'query))
     (t nil       (eq files-test-result 'query))
     (t maybe     (eq files-test-result 'query))
     (nil t       (eq files-test-result nil))
     (nil nil     (eq files-test-result nil))
     (nil maybe   (eq files-test-result nil))
     (:safe t     (eq files-test-result nil))
     (:safe nil   (eq files-test-result nil))
     (:safe maybe (eq files-test-result nil))
     (:all t      (eq files-test-result t))
     (:all nil    (eq files-test-result t))
     (:all maybe  (eq files-test-result t))
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result 'query))
     (maybe maybe (eq files-test-result 'query)))
    ;; Safe local variable
    (("files-test-safe-result: t")
     (t t         (eq files-test-safe-result t))
     (t nil       (eq files-test-safe-result t))
     (t maybe     (eq files-test-safe-result t))
     (nil t       (eq files-test-safe-result nil))
     (nil nil     (eq files-test-safe-result nil))
     (nil maybe   (eq files-test-safe-result nil))
     (:safe t     (eq files-test-safe-result t))
     (:safe nil   (eq files-test-safe-result t))
     (:safe maybe (eq files-test-safe-result t))
     (:all t      (eq files-test-safe-result t))
     (:all nil    (eq files-test-safe-result t))
     (:all maybe  (eq files-test-safe-result t))
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result 'query))
     (maybe maybe (eq files-test-result 'query)))
    ;; Safe local variable with unsafe value
    (("files-test-safe-result: 1")
     (t t         (eq files-test-result 'query))
     (t nil       (eq files-test-result 'query))
     (t maybe     (eq files-test-result 'query))
     (nil t       (eq files-test-safe-result nil))
     (nil nil     (eq files-test-safe-result nil))
     (nil maybe   (eq files-test-safe-result nil))
     (:safe t     (eq files-test-safe-result nil))
     (:safe nil   (eq files-test-safe-result nil))
     (:safe maybe (eq files-test-safe-result nil))
     (:all t      (eq files-test-safe-result 1))
     (:all nil    (eq files-test-safe-result 1))
     (:all maybe  (eq files-test-safe-result 1))
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result 'query))
     (maybe maybe (eq files-test-result 'query))))
  "List of file-local variable tests.
Each list element should have the form

  (LOCAL-VARS-LIST . TEST-LIST)

where LOCAL-VARS-LISTS should be a list of local variable
definitions (strings) and TEST-LIST is a list of tests to
perform.  Each entry of TEST-LIST should have the form

 (ENABLE-LOCAL-VARIABLES ENABLE-LOCAL-EVAL FORM)

where ENABLE-LOCAL-VARIABLES is the value to assign to
`enable-local-variables', ENABLE-LOCAL-EVAL is the value to
assign to `enable-local-eval', and FORM is a desired `should'
form.")

(defun file-test--do-local-variables-test (str test-settings)
  (with-temp-buffer
    (insert str)
    (setq files-test-result nil
	  files-test-safe-result nil)
    (let ((enable-local-variables (nth 0 test-settings))
	  (enable-local-eval      (nth 1 test-settings))
	  ;; Prevent any dir-locals file interfering with the tests.
	  (enable-dir-local-variables nil))
      (hack-local-variables)
      (eval (nth 2 test-settings) t))))



(defvar files-test-bug-18141-file
  (ert-resource-file "files-bug18141.el.gz")
  "Test file for bug#18141.")











(defmacro files-tests--with-advice (symbol where function &rest body)
  (declare (indent 3))
  (cl-check-type symbol symbol)
  (cl-check-type where keyword)
  (cl-check-type function function)
  (macroexp-let2 nil function function
    `(progn
       (advice-add #',symbol ,where ,function)
       (unwind-protect
           (progn ,@body)
         (advice-remove #',symbol ,function)))))



(cl-defmacro files-tests--with-temp-non-special
    ((name non-special-name &optional dir-flag) &rest body)
  "Run tests with quoted file name.
NAME is the symbol which contains the name of a created temporary
file.  NON-SPECIAL-NAME is another symbol, which contains the
temporary file name with quoted file name syntax.  If DIR-FLAG is
non-nil, a temporary directory is created instead.
After evaluating BODY, the temporary file or directory is deleted."
  (declare (indent 1) (debug ((symbolp symbolp &optional form) body)))
  (cl-check-type name symbol)
  (cl-check-type non-special-name symbol)
  `(let* ((temporary-file-directory (file-truename temporary-file-directory))
          (temporary-file-directory
           (file-name-as-directory (make-temp-file "files-tests" t)))
          (,name (make-temp-file "files-tests" ,dir-flag))
          (,non-special-name (file-name-quote ,name)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,name)
         (if ,dir-flag (delete-directory ,name t)
           (delete-file ,name)))
       (when (file-exists-p ,non-special-name)
         (if ,dir-flag (delete-directory ,non-special-name t)
           (delete-file ,non-special-name)))
       (when (file-exists-p temporary-file-directory)
         (delete-directory temporary-file-directory t)))))

(defconst files-tests--special-file-name-extension ".special"
  "Trailing string for test file name handler.")

(defconst files-tests--special-file-name-regexp
  (concat (regexp-quote files-tests--special-file-name-extension) "\\'")
  "Regular expression for test file name handler.")

(defun files-tests--special-file-name-handler (operation &rest args)
  "File name handler for files with extension \".special\"."
  (let ((arg args)
        ;; Avoid cyclic call.
        (file-name-handler-alist
         (delete
          (rassoc
           'files-tests--special-file-name-handler file-name-handler-alist)
          file-name-handler-alist)))
    ;; Remove trailing "\\.special\\'" from arguments, if they are not quoted.
    (while arg
      (when (and (stringp (car arg))
                 (not (file-name-quoted-p (car arg)))
                 (string-match files-tests--special-file-name-regexp (car arg)))
        (setcar arg (replace-match "" nil nil (car arg))))
      (setq arg (cdr arg)))
    ;; Call it.
    (apply operation args)))

(cl-defmacro files-tests--with-temp-non-special-and-file-name-handler
    ((name non-special-name &optional dir-flag) &rest body)
  "Run tests with quoted file name, see `files-tests--with-temp-non-special'.
Both file names in NAME and NON-SPECIAL-NAME have the extension
\".special\".  The created temporary file or directory does not have
that extension.
A file name handler is added which is activated for files with
that extension.  It simply removes the extension from file names.
It is expected, that this file name handler works only for
unquoted file names."
  (declare (indent 1) (debug ((symbolp symbolp &optional form) body)))
  (cl-check-type name symbol)
  (cl-check-type non-special-name symbol)
  `(let* ((temporary-file-directory (file-truename temporary-file-directory))
          (temporary-file-directory
           (file-name-as-directory (make-temp-file "files-tests" t)))
          (file-name-handler-alist
           `((,files-tests--special-file-name-regexp
              . files-tests--special-file-name-handler)
             . ,file-name-handler-alist))
          (,name (concat
                  (make-temp-file "files-tests" ,dir-flag)
                  files-tests--special-file-name-extension))
          (,non-special-name (file-name-quote ,name)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,name)
         (if ,dir-flag (delete-directory ,name t)
           (delete-file ,name)))
       (when (file-exists-p ,non-special-name)
         (if ,dir-flag (delete-directory ,non-special-name t)
           (delete-file ,non-special-name)))
       (when (file-exists-p temporary-file-directory)
         (delete-directory temporary-file-directory t)))))

(defun files-tests--new-name (name part)
  (let (file-name-handler-alist)
    (concat (file-name-sans-extension name) part (file-name-extension name t))))



(defun files-tests-file-attributes-equal (attr1 attr2)
  ;; Element 4 is access time, which may be changed by the act of
  ;; checking the attributes.
  (setf (nth 4 attr1) nil)
  (setf (nth 4 attr2) nil)
  ;; Element 9 is unspecified.
  (setf (nth 9 attr1) nil)
  (setf (nth 9 attr2) nil)
  (equal attr1 attr2))







;; Note: we call this test "...-zzdont..." so that it runs near the
;; end, because otherwise the advice it adds to write-region doesn't
;; get removed(??) and breaks the revert-file tests on MS-Windows.


(defvar files-tests-lao "The Way that can be told of is not the eternal Way;
The name that can be named is not the eternal name.
The Nameless is the origin of Heaven and Earth;
The Named is the mother of all things.
Therefore let there always be non-being,
  so we may see their subtlety,
And let there always be being,
  so we may see their outcome.
The two are the same,
But after they are produced,
  they have different names.
")

(defvar files-tests-tzu "The Nameless is the origin of Heaven and Earth;
The named is the mother of all things.

Therefore let there always be non-being,
  so we may see their subtlety,
And let there always be being,
  so we may see their outcome.
The two are the same,
But after they are produced,
  they have different names.
They both may be called deep and profound.
Deeper and more profound,
The door of all subtleties!
")



(defvar sh-shell)

(defun files-tests--check-shebang (shebang expected-mode &optional expected-dialect)
  "Assert that mode for SHEBANG derives from EXPECTED-MODE.

If EXPECTED-MODE is sh-base-mode, DIALECT says what `sh-shell' should be
set to."
  (ert-with-temp-file script-file
    :text shebang
    (find-file script-file)
    (let ((actual-mode (if (derived-mode-p expected-mode)
                           expected-mode
                         major-mode)))
      ;; Tuck all the information we need in the `should' form: input
      ;; shebang, expected mode vs actual.
      (should
       (equal (list shebang actual-mode)
              (list shebang expected-mode)))
      (when (eq expected-mode 'sh-base-mode)
        (should (eq sh-shell expected-dialect))))))



(defun files-tests--save-some-buffers (pred def-pred-bind exp-1 exp-2)
  "Helper function to test `save-some-buffers'.

This function creates two file-visiting buffers, BUF-1, BUF-2 in
different directories at the same level, i.e., none of them is a
subdir of the other; then it modifies both buffers; finally, it
calls `save-some-buffers' from BUF-1 with first arg t, second
arg PRED and `save-some-buffers-default-predicate' let-bound to
DEF-PRED-BIND.

EXP-1 and EXP-2 are the expected values of calling `buffer-modified-p'
on BUF-1 and BUF-2 after the `save-some-buffers' call.

The test is repeated with `save-some-buffers-default-predicate'
let-bound to PRED and passing nil as second arg of
`save-some-buffers'."
  (ert-with-temp-directory dir
    (let* ((file-1 (expand-file-name "subdir-1/file.foo" dir))
           (file-2 (expand-file-name "subdir-2/file.bar" dir))
           (inhibit-message t)
           buf-1 buf-2)
      (unwind-protect
          (progn
            (make-empty-file file-1 'parens)
            (make-empty-file file-2 'parens)
            (setq buf-1 (find-file file-1)
                  buf-2 (find-file file-2))
            (dolist (buf (list buf-1 buf-2))
              (with-current-buffer buf (insert "foobar\n")))
            ;; Run the test.
            (with-current-buffer buf-1
              (let ((save-some-buffers-default-predicate def-pred-bind))
                (save-some-buffers t pred))
              (should (eq exp-1 (buffer-modified-p buf-1)))
              (should (eq exp-2 (buffer-modified-p buf-2))))
            ;; Set both buffers as modified to run another test.
            (dolist (buf (list buf-1 buf-2))
              (with-current-buffer buf (set-buffer-modified-p t)))
            ;; The result of this test must be identical as the previous one.
            (with-current-buffer buf-1
              (let ((save-some-buffers-default-predicate (or pred def-pred-bind)))
                (save-some-buffers t nil))
              (should (eq exp-1 (buffer-modified-p buf-1)))
              (should (eq exp-2 (buffer-modified-p buf-2)))))
        ;; Clean up.
        (dolist (buf (list buf-1 buf-2))
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (kill-buffer buf)))))))







(defun files-tests--with-buffer-offer-save (buffers-offer fn-test args-results)
  "Helper macro to test `save-some-buffers' and `save-buffers-kill-emacs'.

This macro creates several non-file-visiting buffers in different
directories at the same level, i.e., none of them is a subdir of the
other.  Then it modifies the buffers and sets their `buffer-offer-save'
as specified by BUFFERS-OFFER, a list of elements (BUFFER OFFER-SAVE).
Finally, it calls FN-TEST from the first buffer.

FN-TEST is the function to test: either `save-some-buffers' or
`save-buffers-kill-emacs'.  This function is called with
`save-some-buffers-default-predicate' let-bound to a value
specified inside ARGS-RESULTS.

During the call to FN-TEST,`read-event' is overridden with a function that
just returns `n' and `kill-emacs' is overridden to do nothing.

ARGS-RESULTS is a list of elements (FN-ARGS CALLERS-DIR EXPECTED), where
FN-ARGS are the arguments for FN-TEST;
CALLERS-DIR specifies the value to let-bind
\`save-some-buffers-default-predicate';
 EXPECTED is the expected result of the test."
  (let* ((dir (make-temp-file "testdir" 'dir))
         (inhibit-message t)
         (use-dialog-box nil)
         buffers)
    (pcase-dolist (`(,bufsym ,offer-save) buffers-offer)
      (let* ((buf (generate-new-buffer (symbol-name bufsym)))
             (subdir (expand-file-name
                      (format "subdir-%s" (buffer-name buf))
                      dir)))
        (make-directory subdir 'parens)
        (push buf buffers)
        (with-current-buffer buf
          (cd subdir)
          (setq buffer-offer-save offer-save)
          (insert "foobar\n"))))
    (setq buffers (nreverse buffers))
    (let ((nb-saved-buffers 0))
      (unwind-protect
          (pcase-dolist (`(,fn-test-args ,callers-dir ,expected)
                         args-results)
            (setq nb-saved-buffers 0)
            (with-current-buffer (car buffers)
              (cl-letf
                  (((symbol-function 'read-event)
                    ;; Increase counter and answer 'n' when prompted
                    ;; to save a buffer.
                    (lambda (&rest _) (cl-incf nb-saved-buffers) ?n))
                   ;; Do not kill Emacs.
                   ((symbol-function 'kill-emacs) #'ignore)
                   (save-some-buffers-default-predicate callers-dir))
                (apply fn-test fn-test-args)
                (should (equal nb-saved-buffers expected)))))
        ;; Clean up.
        (dolist (buf buffers)
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (kill-buffer buf)))
        (delete-directory dir 'recursive)))))

(defmacro files-tests-with-all-permutations (permutation list &rest body)
  "Execute BODY forms for all permutations of LIST.
Execute the forms with the symbol PERMUTATION bound to the current
permutation."
  (declare (indent 2) (debug (symbol form body)))
  (let ((vec (gensym "vec")))
    `(let ((,vec (vconcat ,list)))
       (cl-labels ((swap (,vec i j)
                         (let ((tmp (aref ,vec j)))
                           (aset ,vec j (aref ,vec i))
                           (aset ,vec i tmp)))
                   (permute (,vec l r)
                            (if (= l r)
                                (let ((,permutation (append ,vec nil)))
                                  ,@body)
                              (cl-loop for idx from l below (1+ r) do
                                       (swap ,vec idx l)
                                       (permute ,vec (1+ l) r)
                                       (swap ,vec idx l)))))
         (permute ,vec 0 (1- (length ,vec)))))))

(ert-deftest afiles-tests-buffer-offer-save ()
  "Test `save-some-buffers' for non-file-visiting buffers.
Check the behavior of `save-some-buffers' for non-file-visiting
buffers under several values of `buffer-offer-save'.
The value of `save-some-buffers-default-predicate' is ignored unless
PRED is nil."
  (princ (format "the fuq0 %S %S %S\n"
                 (emacs-pid) (featurep 'comp) native-comp-verbose)
         #'external-debugging-output)
  (let* ((buffers-offer-init '((buf-1 t) (buf-2 always) (buf-3 nil)))
         (nb-might-save
          (length
           (cl-remove-if (lambda (l) (null (cadr l))) buffers-offer-init)))
         (nb-always-save
          (length
           (cl-remove-if-not (lambda (l) (eq 'always (cadr l))) buffers-offer-init))))
    (files-tests-with-all-permutations
        buffers-offer
        buffers-offer-init
      (dolist (pred `(nil t))
        (dolist (callers-dir `(nil save-some-buffers-root))
          (let* ((head-offer (cadar buffers-offer))
                 (res (cond ((null pred)
                             (if (null callers-dir) nb-always-save (or (and head-offer 1) 0)))
                            (t
                             ;; Save any buffer with `buffer-offer-save' non-nil.
                             (if (eq pred t) nb-might-save
                               ;; Restrict to caller's dir.
                               (or (and head-offer 1) 0)))))
                 (args-res `(((nil ,pred) ,callers-dir ,res))))
            (files-tests--with-buffer-offer-save
             buffers-offer
             #'save-some-buffers
             args-res)))))))

(ert-deftest bfiles-tests-file-name-non-special-make-process ()
  (make-process :name "name" :command (list "false"))
  (let (;(cleanse (make-process :name "name" :command (list "false")))
        (cleanse (start-process "name" nil "false")))
    (while (cl-some (lambda (proc)
                      (prog1 (eq (process-status proc) 'run)
                        (delete-process proc)))
                    (process-list))
      (let ((proc (make-process :name "name" :command (list "false"))))
        (message "%S %S"
                 (process-status proc)
                 (process-attributes (process-id proc))))
      (accept-process-output nil 2)
      )))

(provide 'files-tests)
;;; files-tests.el ends here
