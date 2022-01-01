;;; marker-tests.el --- tests for marker.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2016-2022 Free Software Foundation, Inc.

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
  "`pop-global-mark' to retread buffer traversal."

  (let (global-mark-ring
        (global-mark-ring-max 3)
        extant)
    (cl-letf (((symbol-function 'window-old-buffer)
               (lambda (_window) extant)))
      (setq extant (current-buffer))
      (find-file-read-only (locate-library "lisp.el"))
      (setq extant (current-buffer))
      (find-file-read-only (locate-library "package.el"))
      (setq extant (current-buffer))
      (xref-find-definitions "benchmark-run")
      (setq extant (current-buffer))
      (should (= (length global-mark-ring) global-mark-ring-max))
      (call-interactively #'push-global-mark)
      (should (= (length global-mark-ring) global-mark-ring-max))
      (call-interactively #'pop-global-mark)
      (should-not (equal (buffer-name) "benchmark.el")) ;; get me off same
      (should (equal (buffer-name) "package.el")) ;; i'm aware redundant with above
      (call-interactively #'pop-global-mark)
      (should (equal (buffer-name) "lisp.el"))
      (call-interactively #'pop-global-mark)
      (should (equal (buffer-name) "benchmark.el"))
      (push-global-mark) ;; reverse rotate since pushed == back
      (should (equal (buffer-name (marker-buffer (car (last global-mark-ring))))
                     "lisp.el"))
      (let ((unchanged (copy-tree global-mark-ring)))
        (save-window-excursion
          (find-file-read-only (locate-library "advice.el"))
          (setq extant (current-buffer))
          (find-file-read-only (locate-library "frame.el"))
          (setq extant (current-buffer))
          (should (= (length global-mark-ring) global-mark-ring-max))
          (call-interactively #'pop-global-mark)
          (should (equal (buffer-name) "advice.el")))
        (should (equal global-mark-ring unchanged))))))

;;; marker-tests.el ends here.
