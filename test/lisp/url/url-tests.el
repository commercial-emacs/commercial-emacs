;;; url-retrieve-tests.el --- Test suite for url-retrieve.  -*- lexical-binding:t -*-

;; Copyright (C) 2012-2025 Free Software Foundation, Inc.

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
(require 'url)

(defmacro url-retrieve-tests-server (proc sentinel &rest forms)
  (declare (indent defun))
  `(let* ((,proc (make-network-process
                  :name "free-port"
                  :noquery t
                  :host "127.0.0.1"
                  :buffer nil
                  :server t
                  :stop nil
                  :sentinel ,sentinel
                  :service t))
          (url-debug 1))
     (unwind-protect
         (progn ,@forms)
       (delete-process ,proc))))

(ert-deftest url-retrieve-synchronously ()
  (let ((done-p nil))
    (url-retrieve-tests-server proc
      (lambda (_proc event)
        (should (or done-p
                    (setq done-p (cl-search "open from" event)))))
      (url-retrieve-synchronously (format "http://127.0.0.1:%d"
                                          (process-contact proc :service))
                                  nil nil 1))))
