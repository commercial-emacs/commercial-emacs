;;; xdisp-tests.el --- tests for xdisp.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

(defmacro xdisp-tests--visible-buffer (&rest body)
  (declare (debug t) (indent 0))
  `(progn
     (switch-to-buffer (symbol-name (ert-test-name (ert-running-test))))
     (progn ,@body)
     (let (kill-buffer-query-functions)
       (kill-buffer (symbol-name (ert-test-name (ert-running-test)))))))

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
scroll up.  (Bug#43519)"
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
to scroll. (Bug#44070)"
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
  (xdisp-tests--visible-buffer
    (dotimes (_ (/ (- (window-pixel-height) 100) (line-pixel-height)))
      (insert "line" "\n"))
    (insert-image (create-image (expand-file-name
                                 "test/data/image/blank-100x200.png"
                                 source-directory)))
    (insert "\n")
    (redisplay)
    (goto-char (point-min))
    (scroll-up)
    (redisplay)))

(ert-deftest xdisp-tests--unibyte-echo-area-resize ()
  "When echo area needs to show a unibyte blob, don't think it's bidi."
  (skip-unless (not noninteractive))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally
     (expand-file-name "test/data/decompress/compat.tar.gz" source-directory))
    (buffer-string)))

(ert-deftest xdisp-tests--scroll-down-leaves-cursor-behind ()
  "When first line contains accented, and therefore taller
character, e.g., Óscar, scrolling down (moving window-start up)
has resulted in a no-op."
  (xdisp-tests--visible-buffer
    (insert "Óscar" "\n")
    (dotimes (_ (/ (1+ (window-pixel-height)) (line-pixel-height)))
      (insert "line" "\n"))
    (goto-char (point-max))
    (redisplay)
    (scroll-down)
    (redisplay)
    (should (= (window-start) 1))))

(ert-deftest xdisp-tests--window-text-pixel-size-single-sline ()
  "Verify `window-text-pixel-size' handles one screen line spanned."
  (xdisp-tests--visible-buffer
    (save-excursion
      (insert "xxxx"))
    (should (= (* 2 (frame-char-width))
               (car (window-text-pixel-size
                     nil (1+ (point-min)) (1- (point-max))))))))

(ert-deftest xdisp-tests--window-text-pixel-size-display-property ()
  "Verify `window-text-pixel-size' returns dimensions including
width of display property."
  (xdisp-tests--visible-buffer
    (let ((disp-string "ornery"))
      (save-excursion
        (insert "xxxx"))
      (should
       (= (+ (* (frame-char-width) (1- (length disp-string)))
             (car (window-text-pixel-size
                   nil (line-beginning-position) (line-end-position))))
	  (progn
	    (put-text-property (1- (line-end-position))
                               (line-end-position)
                               'display disp-string)
	    (car (window-text-pixel-size
                  nil (line-beginning-position) (line-end-position)))))))))

(ert-deftest xdisp-tests--visual-line-mode ()
  "A clever test by /u/fragglestickcar0 on /r/emacs."
  (skip-unless (not noninteractive))
  (xdisp-tests--visible-buffer
   (visual-line-mode)
   (save-excursion
    (insert (make-string (/ (window-width) 2) ?x) " "
	    (make-string (window-width) ?x)))
   (should (zerop (- (window-width)
                     (save-excursion
                       (end-of-line)
                       (- (point) (beginning-of-visual-line))))))))

(ert-deftest xdisp-tests--vertical-motion-display-string ()
  "Test line-up and line-down in presence of display string."
  (skip-unless (not noninteractive))
  (xdisp-tests--visible-buffer
   (insert (mapconcat #'number-to-string (number-sequence 1 5) "") "\n")
   (save-excursion (insert "_this\n"))
   (add-text-properties (point) (1+ (point))
                        `(display "See " front-sticky nil rear-nonsticky t))
   (call-interactively #'previous-line)
   (should (looking-at "1"))
   (call-interactively #'forward-char)
   (should (looking-at "2"))
   (call-interactively #'next-line)
   (should (looking-at "_"))
   (call-interactively #'forward-char)
   (should (looking-at "t"))
   (call-interactively #'previous-logical-line) ;; sus
   (should (looking-at "5"))
   (call-interactively #'next-line)
   (should (looking-at "t"))
   (call-interactively #'backward-char)
   (should (looking-at "_"))))

(ert-deftest xdisp-tests--xy-to-charpos ()
  "Truncated portions of lines shouldn't affect iterator geometry."
  (skip-unless (not noninteractive))
  (xdisp-tests--visible-buffer
    (cl-flet ((test-it
                (where oblivious-y nlines)
                (save-excursion
                  (goto-char where)
                  (end-of-line)
                  (should (= nlines (line-number-at-pos)))
                  (redisplay)
                  ;; confusing: `posn-at-point' calls window_start_coordinates()
                  ;; producing a Y as if all glyphs were visible.
                  (cl-destructuring-bind
                      (_w _area (x . y) &rest args)
                      (posn-at-point)
                    ;; now emulate a mouse-click on a window whose contents are
                    ;; oblivious to tall glyphs (due to non-nil truncate-lines).
                    (cl-destructuring-bind
                        (_w _area (x2 . y2) &rest args)
                        (posn-at-x-y x oblivious-y)
                      (should (= x x2))
                      (should (= y y2)))))))
      (let ((tall (propertize
                   "tall words"
                   'face `(:height ,(* 3 (face-attribute 'default :height)))))
            (long (make-string (* (window-width) 2) ?x))
            (nlines 3)
            final oblivious-y)
        (text-mode)
        (toggle-truncate-lines 1)
        (erase-buffer)
        (dotimes (_i nlines)
          (insert ?x ?\n))
        (forward-line -1)
        (redisplay)
        (setq oblivious-y (cl-destructuring-bind
                              (_w _area (_x . y) &rest args)
                              (posn-at-point)
                            y))
        (erase-buffer)
        (dotimes (_i (1- nlines))
          (insert tall ?\n))
        (setq final (point))
        (insert long ?\n)
        (test-it final oblivious-y nlines)
        (erase-buffer)
        (insert tall long tall ?\n)
        (insert tall ?\n)
        (setq final (point))
        (insert long ?\n)
        (test-it final oblivious-y nlines)))))

(ert-deftest xdisp-tests--bidi-chlen-was-ignored ()
  "get_visually_first_element() was repositioning
bidi_it.charpos without also fetching its char."
  (skip-unless (not noninteractive))
  (xdisp-tests--visible-buffer
   (save-excursion
     (insert "给")
     (insert (make-string (window-width) ?x)))
   (redisplay)))

(ert-deftest xdisp-tests--respect-inhibit-message ()
  "It's borderline criminal that inhibit-message still clears echo area."
  (skip-unless (not noninteractive))
  (let ((dont "dont clear me!"))
    (message dont)
    (let ((inhibit-message t))
      (message "nice try")
      (should (equal (current-message) dont)))))

(ert-deftest xdisp-tests--find-automatic-composition ()
  "`find-automatic-composition' could stand yet another rewrite.
Bug present here and in GNU since it doesn't sustain a rerun."
  (skip-unless (not noninteractive))
  (xdisp-tests--visible-buffer
    (save-excursion
      (insert "Hebrew (עִבְרִית)	שָׁלוֹם" "\n")
      (insert "Hindi (हिन्दी)	प्रणाम / पाय लागू" "\n"))
    (dotimes (_i 11)
      (call-interactively #'forward-char))
    (call-interactively #'backward-char)
    (should (equal (char-after) (string-to-char "בְ")))
    (call-interactively #'forward-char)
    (call-interactively #'next-line)
    (should (equal (char-after) ?\)))
    (call-interactively #'forward-char)
    (call-interactively #'forward-char)
    (call-interactively #'previous-line)
    (should (equal (char-after) (string-to-char "ְ")))))

(ert-deftest xdisp-tests--char-pos-reached-within-image-after-images ()
  "Bug#65899."
  (skip-unless (not noninteractive))
  (xdisp-tests--visible-buffer
    (let* ((width 20)
           (height (line-pixel-height))
           (data (with-temp-buffer
                   (insert (format "P1\n%s %s\n" width height))
                   (dotimes (_ height)
                     (insert (make-string width ?1) "\n"))
                   (buffer-string))))
      (dotimes (i 3)
        (erase-buffer)
        (dotimes (_ i)
          (insert-image `(image :type pbm
                                :data ,"P1\n1 10\n1111111111"
                                :ascent center)
                        "t"))
        (let ((from (point)))
          (insert-image `(image :type pbm :data ,data :ascent center) "t")
          (should (equal width (car (window-text-pixel-size nil from (point))))))))))

(ert-deftest xdisp-tests--char-pos-reached-within-image-after-spaces ()
  "Bug#54862."
  (skip-unless (not noninteractive))
  (xdisp-tests--visible-buffer
    (let* ((width 20)
           (height (line-pixel-height))
           (data (with-temp-buffer
                   (insert (format "P1\n%s %s\n" width height))
                   (dotimes (_ height)
                     (insert (make-string width ?1) "\n"))
                   (buffer-string))))
      (dotimes (i 3)
        (erase-buffer)
        (insert (make-string i ? ))
        (let ((from (point)))
          (insert-image `(image :type pbm
                                :data ,data
                                :ascent center)
                        "t")
          (should (equal width (car (window-text-pixel-size nil from (point))))))))))


(ert-deftest xdisp-tests--ornery-display-string ()
  "I dunno awrhygty@outlook.com, but dude makes good tests (Bug#67201)."
  (skip-unless (not noninteractive))
  (xdisp-tests--visible-buffer
   (let ((s (make-string 1000 ?-))
         (v (concat "0123456789"
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    "abcdefghijklmnopqrstuvwxyz"))
         (line-prefix "[start]")
         (wrap-prefix "[wrap]")
         (ww (window-width))
         truncate-lines)
     (dotimes (i (length s))
       (put-text-property i (1+ i) 'display (string (aref v (% i (length v)))) s))
     (add-text-properties 0 (length s)
                          `(line-prefix
                            ,(propertize line-prefix 'face '(:background "red"))
                            wrap-prefix ,wrap-prefix)
                          s)
     (save-excursion (insert s))
     (scroll-up-command 1)
     (should (= (window-start) (point)))
     (should (equal (char-to-string
                     (aref v (% (- (* 1 ww) (+ (length line-prefix)))
                                (length v))))
                    (get-text-property (point) 'display)))
     (scroll-up-command 1)
     (should (= (window-start) (point)))
     (should (equal (char-to-string
                     (aref v (% (- (* 2 ww) (+ (length line-prefix)
                                               (length wrap-prefix)))
                                (length v))))
                    (get-text-property (point) 'display)))
     (forward-char (- ww (length wrap-prefix)))
     (should (equal (char-to-string
                     (aref v (% (- (* 3 ww) (+ (length line-prefix)
                                               (length wrap-prefix)
                                               (length wrap-prefix)))
                                (length v))))
                    (get-text-property (point) 'display))))))

;;; xdisp-tests.el ends here
