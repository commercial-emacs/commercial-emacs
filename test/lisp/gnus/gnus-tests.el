;;; gnus-tests.el --- Wrapper for the Gnus tests  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2022 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>

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

;; This file should contain nothing but requires for all the Gnus
;; tests that are not standalone.

;;; Code:
;;

(require 'cl-macs)
(require 'message)
(require 'gnus)
(require 'gnus-start)
(require 'nsm)

(defconst gnus-tests-load-file-name (or load-file-name
                                        (buffer-file-name)))

(defmacro gnus-tests-let-customs (bindings &rest forms)
  (declare (indent defun))
  `(let (,@(mapcar #'car bindings))
     (ignore ,@(mapcar #'car bindings))
     (funcall #'custom-set-variables
              ,@(mapcar (apply-partially #'list 'quote) bindings))
     ,@forms))

(cl-defmacro gnus-tests-doit (&rest
                              body
                              &key
                              (select-methods '(default-value 'gnus-select-methods))
                              (customs)
                              &allow-other-keys
                              &aux
                              (body
                               (cl-loop until (not (keywordp (car body)))
                                        do (setq body (nthcdr 2 body))
                                        finally return body)))
  (declare (indent defun))
  `(let* ((parent-dir (file-name-directory gnus-tests-load-file-name))
          (default-directory (file-name-as-directory (concat parent-dir "gnus-tests")))
	  (user-emacs-directory default-directory))
     (unless (file-exists-p default-directory)
       (make-directory default-directory))
     (gnus-tests-let-customs
       ((gnus-verbose 8)
        (gnus-save-dot-newsrc nil)
        (gnus-home-directory default-directory)
        (gnus-use-dribble-file nil)
        (network-security-level (quote low))
        (gnus-interactive-exit (quote quiet))
        (gnus-select-methods ,select-methods)
        (message-directory (concat default-directory "Mail"))
        (mail-source-directory message-directory)
        (mail-source-crash-box (concat default-directory ".whatev"))
        (gnus-newsrc-file (nnheader-concat gnus-home-directory ".newsrc.eld"))
        (gnus-init-file (nnheader-concat gnus-home-directory ".gnus"))
        (gnus-directory (nnheader-concat gnus-home-directory "News/"))
        ,@customs)
       (unwind-protect
           (progn ,@body)
         (cl-macrolet ((safe-delete
                        (x)
                        `(if (cl-search "gnus-tests/" ,x)
                             (delete-directory ,x t)
                           (error "Attempted delete of %s!" ,x))))
           (safe-delete default-directory))))))

(ert-deftest gnus-test-clean-room ()
  (gnus-tests-doit
    (should (equal gnus-select-methods (default-value 'gnus-select-methods)))
    (should-not gnus-save-dot-newsrc))
  (should gnus-save-dot-newsrc))

(ert-deftest gnus-test-select-methods ()
  (gnus-tests-doit :select-methods (quote ((nnfolder "")))
    (should (equal gnus-select-methods (quote ((nnfolder "")))))
    (should (equal gnus-select-method (quote (nnfolder ""))))
    (should-not gnus-secondary-select-methods)))

(ert-deftest gnus-test-be-nicer-to-noobs ()
  "Between the time Linux entered the home and the time I wrote this test,
the innocent user trying `M-x gnus` would be rebuffed with hostility."
  (gnus-tests-doit
    (with-current-buffer "*Messages*"
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((inhibit-message t))
      (call-interactively #'gnus))
    (with-current-buffer "*Messages*"
      (save-excursion
        (goto-char (point-min))
        (should-error (re-search-forward "failed"))))))

(ert-deftest gnus-test-basic-op ()
  (gnus-tests-doit :select-methods (quote ((nnfolder "")))
    (call-interactively #'gnus)))

(provide 'gnus-tests)
;;; gnus-tests.el ends here
