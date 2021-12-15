;;; xdisp-tests.el --- tests for xdisp.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

(defmacro xdisp-tests--in-minibuffer (&rest body)
  (declare (debug t) (indent 0))
  `(catch 'result
     (minibuffer-with-setup-hook
         (lambda ()
           (let ((redisplay-skip-initial-frame nil)
                 (executing-kbd-macro nil)) ;Don't skip redisplay
             (throw 'result (progn . ,body))))
       (let ((executing-kbd-macro t)) ;Force real minibuffer in `read-string'.
         (read-string "toto: ")))))

(ert-deftest xdisp-tests--minibuffer-resizing ()
  "Unvisible overlay string following visible text should not
scroll up.  (Bug#43519) "
  (xdisp-tests--in-minibuffer
   (insert "hello")
   (let ((ol (make-overlay (point) (point)))
         (max-mini-window-height 1)
         (text (let ((s ""))
                 (dotimes (i 137)
                   (setq s (concat s (char-to-string (+ (% i 26) ?a)))))
                 s)))
     (put-text-property 0 1 'cursor t text)
     (overlay-put ol 'after-string text)
     (redisplay)
     (should (equal (window-start) (point-min)))
     (delete-overlay ol))))

(ert-deftest xdisp-tests--minibuffer-scroll ()
  "Deleting the last character shouldn't cause the text
to scroll. (Bug#44070) "
  (xdisp-tests--in-minibuffer
   (let ((max-mini-window-height 4))
     (dotimes (_ 80) (insert "\nhello"))
     (should (equal (progn (goto-char (point-min))
                           (redisplay) (goto-char (point-max))
                           (redisplay) (window-start))
                    (progn (delete-char -1)
                           (redisplay) (window-start))))
     (should (equal (window-start)
                    (progn (goto-char (point-min))
                           (redisplay) (goto-char (point-max))
                           (redisplay) (window-start)))))))

(ert-deftest xdisp-tests--window-text-pixel-size () ;; bug#45748
  (with-temp-buffer
    (insert "xxx")
    (switch-to-buffer (current-buffer))
    (let* ((char-width (frame-char-width))
           (size (window-text-pixel-size nil t t))
           (width-in-chars (/ (car size) char-width)))
      (should (equal width-in-chars 3)))))

(ert-deftest xdisp-tests--window-text-pixel-size-leading-space () ;; bug#45748
  (with-temp-buffer
    (insert " xx")
    (switch-to-buffer (current-buffer))
    (let* ((char-width (frame-char-width))
           (size (window-text-pixel-size nil t t))
           (width-in-chars (/ (car size) char-width)))
      (should (equal width-in-chars 3)))))

(ert-deftest xdisp-tests--window-text-pixel-size-trailing-space () ;; bug#45748
  (with-temp-buffer
    (insert "xx ")
    (switch-to-buffer (current-buffer))
    (let* ((char-width (frame-char-width))
           (size (window-text-pixel-size nil t t))
           (width-in-chars (/ (car size) char-width)))
      (should (equal width-in-chars 3)))))

(ert-deftest xdisp-tests--find-directional-overrides-case-1 ()
  (with-temp-buffer
    (insert "\
int main() {
  bool isAdmin = false;
  /*‮ }⁦if (isAdmin)⁩ ⁦ begin admins only */
  printf(\"You are an admin.\\n\");
  /* end admins only ‮ { ⁦*/
  return 0;
}")
    (goto-char (point-min))
    (should (eq (bidi-find-overridden-directionality (point-min) (point-max)
                                                     nil)
                46))))

(ert-deftest xdisp-tests--find-directional-overrides-case-2 ()
  (with-temp-buffer
    (insert "\
#define is_restricted_user(user)			\\
  !strcmp (user, \"root\") ? 0 :			\\
  !strcmp (user, \"admin\") ? 0 :			\\
  !strcmp (user, \"superuser‮⁦? 0 : 1⁩ ⁦\")⁩‬

int main () {
  printf (\"root: %d\\n\", is_restricted_user (\"root\"));
  printf (\"admin: %d\\n\", is_restricted_user (\"admin\"));
  printf (\"superuser: %d\\n\", is_restricted_user (\"superuser\"));
  printf (\"luser: %d\\n\", is_restricted_user (\"luser\"));
  printf (\"nobody: %d\\n\", is_restricted_user (\"nobody\"));
}")
    (goto-char (point-min))
    (should (eq (bidi-find-overridden-directionality (point-min) (point-max)
                                                     nil)
                138))))

(ert-deftest xdisp-tests--find-directional-overrides-case-3 ()
  (with-temp-buffer
    (insert "\
#define is_restricted_user(user)			\\
  !strcmp (user, \"root\") ? 0 :			\\
  !strcmp (user, \"admin\") ? 0 :			\\
  !strcmp (user, \"superuser‮⁦? '#' : '!'⁩ ⁦\")⁩‬

int main () {
  printf (\"root: %d\\n\", is_restricted_user (\"root\"));
  printf (\"admin: %d\\n\", is_restricted_user (\"admin\"));
  printf (\"superuser: %d\\n\", is_restricted_user (\"superuser\"));
  printf (\"luser: %d\\n\", is_restricted_user (\"luser\"));
  printf (\"nobody: %d\\n\", is_restricted_user (\"nobody\"));
}")
    (goto-char (point-min))
    (should (eq (bidi-find-overridden-directionality (point-min) (point-max)
                                                     nil)
                138))))

(ert-deftest test-get-display-property ()
  (with-temp-buffer
    (insert (propertize "foo" 'face 'bold 'display '(height 2.0)))
    (should (equal (get-display-property 2 'height) 2.0)))
  (with-temp-buffer
    (insert (propertize "foo" 'face 'bold 'display '((height 2.0)
                                                     (space-width 2.0))))
    (should (equal (get-display-property 2 'height) 2.0))
    (should (equal (get-display-property 2 'space-width) 2.0)))
  (with-temp-buffer
    (insert (propertize "foo bar" 'face 'bold
                        'display '[(height 2.0)
                                   (space-width 20)]))
    (should (equal (get-display-property 2 'height) 2.0))
    (should (equal (get-display-property 2 'space-width) 20))))

(ert-deftest xdisp-tests--reconnoiter-image-height ()
  "C-v on image extending beyond window should not signal
end-of-buffer."
  (skip-unless (not noninteractive))
  (skip-unless (> (window-pixel-height) 300))
  (switch-to-buffer "xdisp-tests--reconnoiter-image-height")
  (dotimes (_ (/ (- (window-pixel-height) 100) (line-pixel-height)))
    (insert "line" "\n"))
  (insert-image (create-image (expand-file-name
                               "test/data/image/blank-100x200.png"
                               source-directory)))
  (insert "\n")
  (redisplay)
  (goto-char (point-min))
  (scroll-up)
  (redisplay)
  (let (kill-buffer-query-functions)
    (kill-buffer "xdisp-tests--reconnoiter-image-height")))

;;; xdisp-tests.el ends here
