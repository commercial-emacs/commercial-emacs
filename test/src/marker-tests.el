;;; marker-tests.el --- tests for marker.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2016-2021 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;; The following three tests assert that Emacs survives operations
;; copying a marker whose character position differs from its byte
;; position into a buffer whose character size equals its byte size
;; (Bug#24368).

(ert-deftest marker-set-window-start-from-other-buffer ()
  "`set-window-start' from other buffer's marker."
  (let ((text-quoting-style 'curve))
    (describe-function 'describe-function))
  (let* ((help (get-buffer "*Help*"))
         (marker (with-current-buffer help
                   (copy-marker (point-max)))))
    (should (set-window-start (selected-window) marker))))

(ert-deftest marker-set-window-point-from-other-buffer ()
  "`set-window-point' from another buffer's marker."
  (let ((text-quoting-style 'curve))
    (describe-function 'describe-function))
  (let* ((help (get-buffer "*Help*"))
         (marker (with-current-buffer help
                   (copy-marker (point-max)))))
    (with-selected-window (get-buffer-window help)
      (should (set-window-point (get-buffer-window "*scratch*") marker)))))

(ert-deftest marker-goto-char-from-other-buffer ()
  "`goto-char' from another buffer's marker."
  (let ((text-quoting-style 'curve))
    (describe-function 'describe-function))
  (let ((marker-1 (make-marker))
        (marker-2 (make-marker)))
    (describe-function 'describe-function)
    (with-current-buffer "*Help*"
      (set-marker marker-1 (point-max)))
    (set-marker marker-2 marker-1)
    (should (goto-char marker-2))))

(ert-deftest marker-global-mark-ring ()
  "`pop-global-mark' to retread errors."
  (let* ((global-mark-ring-max 3)
         global-mark-ring
         buffer-list-update-hook
         recorded
         ;; with-temp-buffer creates an invisible buffer,
         ;; which push-global-mark would ignore.
         (buffer (get-buffer-create (make-temp-name "test")))
         (record (lambda () (push (point-marker) recorded)))
         (next-error-hook (list record)))
    (unwind-protect
        (with-current-buffer buffer
          (insert "error 1
error 2
error 3
error 4
")
          (occur "^error")
          (setq global-mark-ring nil)
          (call-interactively #'next-error)
          (should (= (length global-mark-ring) 1))
          (call-interactively #'next-error)
          (call-interactively #'next-error)
          (should (= (length global-mark-ring) global-mark-ring-max))
          (call-interactively #'next-error)
          (should (= (length global-mark-ring) global-mark-ring-max))
          (call-interactively #'pop-global-mark)
          (should (equal (pop recorded) (point-marker)))
          (call-interactively #'pop-global-mark)
          (should (equal (pop recorded) (point-marker)))
          (call-interactively #'pop-global-mark)
          (should (equal (pop recorded) (point-marker)))
          (goto-char (pop recorded))
          (let ((unchanged (copy-tree global-mark-ring)))
            (push-global-mark)
            (should (equal global-mark-ring unchanged)))
          (goto-char (point-min))
          (call-interactively #'isearch-forward-thing-at-point) ;; on "error" string
          (call-interactively #'isearch-repeat-forward)
          (call-interactively #'isearch-exit)
          (let ((second-error-marker (point-marker)))
            (forward-line 2)
            (should-not (equal (point-marker) second-error-marker))
            (call-interactively #'pop-global-mark)
            (should (equal (point-marker) second-error-marker)))
          (let ((unchanged (copy-tree global-mark-ring)))
            (save-excursion (goto-char (point-marker)))
            (should (equal global-mark-ring unchanged))))
      (let (kill-buffer-query-functions)
        (kill-buffer buffer)
        (when (buffer-live-p (get-buffer "*Occur*"))
          (kill-buffer "*Occur*"))))))

;;; marker-tests.el ends here.
