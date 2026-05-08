;;; term-tests.el --- tests for term.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2019-2024 Free Software Foundation, Inc.

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
(require 'ert)
(require 'ert-x)
(require 'term)
(require 'cl-macs)
(eval-when-compile (require 'cl-lib))

(defvar term-height)                    ; Number of lines in window.
(defvar term-width)                     ; Number of columns in window.

(defconst yellow-fg-props
  `(:foreground ,(face-foreground 'term-color-yellow nil 'default)))
(defconst yellow-bg-props
  `(:background ,(face-background 'term-color-yellow nil 'default)))
(defconst bright-yellow-fg-props
  `(:foreground ,(face-foreground 'term-color-bright-yellow nil 'default)))
(defconst bright-yellow-bg-props
  `(:background ,(face-background 'term-color-bright-yellow nil 'default)))
(defconst custom-color-fg-props
  `(:foreground "#87FFFF"))

(defconst ansi-test-strings
  `(("\e[33mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face `(,yellow-fg-props)))
    ("\e[43mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face `(,yellow-bg-props)))
    ("\e[93mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face `(,bright-yellow-fg-props)))
    ("\e[103mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face `(,bright-yellow-bg-props)))
    ("\e[1;33mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,yellow-fg-props term-bold))
     ,(propertize "Hello World" 'font-lock-face
                  `(,bright-yellow-fg-props term-bold)))
    ("\e[33;1mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,yellow-fg-props term-bold))
     ,(propertize "Hello World" 'font-lock-face
                  `(,bright-yellow-fg-props term-bold)))
    ("\e[1m\e[33mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,yellow-fg-props term-bold))
     ,(propertize "Hello World" 'font-lock-face
                  `(,bright-yellow-fg-props term-bold)))
    ("\e[33m\e[1mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,yellow-fg-props term-bold))
     ,(propertize "Hello World" 'font-lock-face
                  `(,bright-yellow-fg-props term-bold)))
    ("\e[38;5;3;1mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,yellow-fg-props term-bold))
     ,(propertize "Hello World" 'font-lock-face
                  `(,bright-yellow-fg-props term-bold)))
    ("\e[38;5;123;1mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,custom-color-fg-props term-bold)))
    ("\e[38;2;135;255;255;1mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,custom-color-fg-props term-bold)))))

(defun term-test-screen-from-input (width height input &optional return-var)
  (with-temp-buffer
    (term-mode)
    ;; Keep dimensions independent from window size.
    (remove-function (local 'window-adjust-process-window-size-function)
                     'term-maybe-reset-size)
    (term-exec (current-buffer) "test" "cat" nil nil)
    (term-char-mode)
    (setq term-width width)
    (setq term-height height)
    ;; Pass input directly to `term-emulate-terminal', it's easier to
    ;; control chunking, and we don't have to worry about wrestling
    ;; with stty settings.
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Don't get stuck when we close the buffer.
      (set-process-query-on-exit-flag proc nil)
      (if (consp input)
                (mapc (lambda (input) (term-emulate-terminal proc input)) input)
              (term-emulate-terminal proc input))
      (if return-var (buffer-local-value return-var (current-buffer))
        (buffer-substring (point-min) (point-max))))))

(ert-deftest term-simple-lines ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let ((str "\
first line\r
next line\r\n"))
    (should (equal (term-test-screen-from-input 40 12 str)
                   (string-replace "\r" "" str)))))

(ert-deftest term-carriage-return ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let ((str "\
first line\r_next line\r\n"))
    (should (equal (term-test-screen-from-input 40 12 str)
                   "_next line\n"))))

(ert-deftest term-line-wrap ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (should (string-match-p
           ;; Don't be strict about trailing whitespace.
           "\\`a\\{40\\}\na\\{20\\} *\\'"
           (term-test-screen-from-input 40 12 (make-string 60 ?a))))
  ;; Again, but split input into chunks.
  (should (string-match-p
           "\\`a\\{40\\}\na\\{20\\} *\\'"
           (term-test-screen-from-input 40 12 (let ((str (make-string 30 ?a)))
                                                (list str str))))))

(ert-deftest term-colors ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (pcase-dolist (`(,str ,expected) ansi-test-strings)
    (let ((result (term-test-screen-from-input 40 12 str)))
      (should (equal result expected))
      (should (equal (text-properties-at 0 result)
                     (text-properties-at 0 expected))))))

(ert-deftest term-colors-bold-is-bright ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let ((ansi-color-bold-is-bright t))
    (pcase-dolist (`(,str ,expected ,bright-expected) ansi-test-strings)
      (let ((expected (or bright-expected expected))
            (result (term-test-screen-from-input 40 12 str)))
        (should (equal result expected))
        (should (equal (text-properties-at 0 result)
                       (text-properties-at 0 expected)))))))

(ert-deftest term-cursor-movement ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  ;; Absolute positioning.
  (should (equal "ab\ncd"
                 (term-test-screen-from-input
                  40 12 (concat "\e[2;2Hd"
                                "\e[2;1Hc"
                                "\e[1;2Hb"
                                "\e[1;1Ha"))))
  ;; Send one byte at a time.
  (should (equal "ab\ncd"
                 (term-test-screen-from-input
                  40 12 (split-string (concat "\e[2;2Hd"
                                              "\e[2;1Hc"
                                              "\e[1;2Hb"
                                              "\e[1;1Ha") "" t))))
  (should (equal "abcde    j"
                 (term-test-screen-from-input
                  10 12 '("abcdefghij"
                          "\e[H"  ;move back to point-min
                          "abcde"
                          "    j"))))

  ;; Relative positioning.
  (should (equal "ab\ncd"
                 (term-test-screen-from-input
                  40 12 (concat "\e[B\e[Cd"
                                "\e[D\e[Dc"
                                "\e[Ab"
                                "\e[D\e[Da")))))

(ert-deftest term-scrolling-region ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (should (equal "\
line3
line4
line5
line6
"
                 (term-test-screen-from-input
                  40 12 "\e[1;5r\
line1\r
line2\r
line3\r
line4\r
line5\r
line6\r
")))

  ;; test reverse scrolling
  (should (equal "line1
line7
line6
line2
line5"
                 (term-test-screen-from-input 40 5
                                              '("\e[0;0H"
                                                "\e[J"
                                                "line1\r
line2\r
line3\r
line4\r
line5"
                                                "\e[2;4r"
                                                "\e[2;0H"
                                                "\e[2;0H"
                                                "\eMline6"
                                                "\e[2;0H"
                                                "\eMline7"))))

  ;; test scrolling down
  (should (equal "line1
line3
line4
line7
line5"
                 (term-test-screen-from-input 40 5
                                              '("\e[0;0H"
                                                "\e[J"
                                                "line1\r
line2\r
line3\r
line4\r
line5"
                                                "\e[2;4r"
                                                "\e[2;0H"
                                                "\e[4;5H"
                                                "\n\rline7"))))

  ;; setting the scroll region end beyond the max height should not
  ;; turn on term-scroll-with-delete
  (should (equal "line1
line2
line3
line4
line5
line6
line7"
                 (term-test-screen-from-input 40 5
                                                      '("\e[1;10r"
                                                        "line1\r
line2\r
line3\r
line4\r
line5\r
line6\r
line7"))))


  ;; resetting the terminal should set the scroll region end to (1- term-height).
  (should (equal "
line1
line2
line3
line4
"
                 (term-test-screen-from-input 40 5
                                                      '("\e[1;10r"
                                                        "\ec" ;reset
                                                        "line1\r
line2\r
line3\r
line4\r
line5"
                                                        "\e[1;1H"
                                                        "\e[L"))))

  ;; scroll region should be limited to the (1- term-height).  Note,
  ;; this fixes an off by one error when comparing the scroll region
  ;; end with term-height.
  (should (equal "
line1
line2
line3
line4
"
                 (term-test-screen-from-input 40 5
                                              '("\e[1;6r"
                                                "line1\r
line2\r
line3\r
line4\r
line5"
                                                "\e[1;1H" ;go back to home
                                                "\e[L"    ;insert a new line at the top
                                                ))))

  ;; setting the scroll region to the entire height should not turn on
  ;; term-scroll-with-delete
  (should (equal "line1
line2
line3
line4
line5
line6"
                 (term-test-screen-from-input 40 5
                                                      '("\e[1;5r"
                                                        "line1\r
line2\r
line3\r
line4\r
line5\r
line6"))))

  ;; reset should reset term-scroll-with-delete
  (should (equal "line1
line2
line3
line4
line5
line6
line7"
                 (term-test-screen-from-input 40 5
                                              '("\e[2;5r" ;set the region
                                                "\ec" ;reset
                                                "line1\r
line2\r
line3\r
line4\r
line5\r
line6\r
line7")))))

(ert-deftest term-set-directory ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let ((term-ansi-at-user (user-real-login-name)))
    (should (equal (term-test-screen-from-input
                    40 12 "\eAnSiTc /foo/\n" 'default-directory)
                   "/foo/"))
    ;; Split input (Bug#17231).
    (should (equal (term-test-screen-from-input
                    40 12 (list "\eAnSiTc /f" "oo/\n") 'default-directory)
                   "/foo/"))))

(ert-deftest term-line-wrapping-then-motion ()
  "Make sure we reset the line-wrapping state after moving cursor.
A real-life example is the default zsh prompt which writes spaces
to the end of line (triggering line-wrapping state), and then
sends a carriage return followed by another space to overwrite
the first character of the line."
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let* ((width 10)
         (strs (list "x" (make-string (1- width) ?_)
                     "\r_")))
    (should (equal (term-test-screen-from-input width 12 strs)
                   (make-string width ?_)))))

(ert-deftest term-to-margin ()
  "Test cursor movement at the scroll margin.
This is a reduced example from GNU nano's initial screen."
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let* ((width 10)
         (x (make-string width ?x))
         (y (make-string width ?y)))
    (should (equal (term-test-screen-from-input
                    width 3
                    `("\e[1;3r"       ; Setup 3 line scrolling region.
                      "\e[2;1H"       ; Move to 2nd last line.
                      ,x              ; Fill with 'x'.
                      "\r\e[1B"       ; Next line.
                      ,y))            ; Fill with 'y'.
                   (concat "\n" x "\n" y)))
    ;; Same idea, but moving upwards.
    (should (equal (term-test-screen-from-input
                    width 3
                    `("\e[1;3r" "\e[2;1H" ,x "\r\e[1A" ,y))
                   (concat y "\n" x)))))

(ert-deftest term-decode-partial () ;; Bug#25288.
  "Test multibyte characters sent into multiple chunks."
  ;; Set `locale-coding-system' so test will be deterministic.
  (let* ((locale-coding-system 'utf-8-unix)
         (string (make-string 7 ?ш))
         (bytes (encode-coding-string string locale-coding-system)))
    (should (equal string
                   (term-test-screen-from-input
                    40 1 `(,(substring bytes 0 (/ (length bytes) 2))
                           ,(substring bytes (/ (length bytes) 2))))))))

(ert-deftest term-undecodable-input () ;; Bug#29918.
  "Undecodable bytes should be passed through without error."
  (let* ((locale-coding-system 'utf-8-unix) ; As above.
         (bytes "\376\340\360\370")
         (string (decode-coding-string bytes locale-coding-system)))
    (should (equal string
                   (term-test-screen-from-input
                    40 1 bytes)))))

(defmacro with-tty-subprocess (lines columns proc &rest body)
  (declare (indent 3))
  `(with-temp-buffer
     (cl-letf* ((process-environment process-environment)
                ((symbol-function 'window-screen-lines)
                 (lambda (&rest _) ,lines))
                ((symbol-function 'window-max-chars-per-line)
                 (lambda (&rest _) ,columns))
                ((symbol-function 'pos-visible-in-window-p)
                 (lambda (&rest _) t))
                (,proc (progn
                         (push "TERM=vt100" process-environment)
                         (push ,(format "LINES=%d" lines) process-environment)
                         (push ,(format "COLUMNS=%d" columns) process-environment)
                         (make-process :name "emacs"
                                       :buffer (current-buffer)
                                       :command (list (expand-file-name invocation-name invocation-directory)
                                                      "--eval" "(setq user-emacs-directory-warning nil)"
                                                      "-Q" "-nw")
                                       :connection-type 'pty
                                       :coding 'utf-8-unix
                                       :filter #'term-emulate-terminal))))
       (set-process-query-on-exit-flag ,proc nil)
       (term-mode)
       (term-char-mode)
       (term-send-string proc "\e:(set-terminal-coding-system 'utf-8)\r")
       (unwind-protect
           (progn ,@body)
         (delete-process ,proc)))))

(ert-deftest term-dispnew-erases-border () ;Bug#51521
  (ert-with-temp-file tmpfile
    (with-temp-buffer
      (insert "r\n")
      (insert (make-string 100 ?\n))
      (write-file tmpfile))
    (with-tty-subprocess 55 159 proc
      (term-send-string proc "\e:(setq split-width-threshold 0)\r")
      (term-send-string proc "\e:(set-window-margins nil 1 1)\r")
      (term-send-string proc (format "\0304\006%s\n" tmpfile)) ;C-x C-f
      (term-send-string proc "\e>")
      (dotimes (_ 4)
        (term-send-string proc "\020")) ;C-p
      (term-send-string proc "a\177cookie") ;DEL triggers a redisplay
      (with-timeout (3)
        (while (not (string-match-p "*scratch*" (buffer-string))) ;mode line
          (accept-process-output)))
      (let ((lines (string-split (buffer-string) "\n")))
        (while (length> lines 55)
          (pop lines))
        (pop lines)
        (setq lines (ntake 52 lines))
        (dolist (line lines)
          (should (eq (aref line 79) ?|)))))))

(ert-deftest term-dispnew-truncation-consistency () ;Bug#80900
  "Test truncation indicator appears in both halves of a split window."
  (with-tty-subprocess 24 80 proc
    (term-send-string proc "\e:(setq truncate-lines t tab-width 80)\r")
    (term-send-string proc "\e:(setq split-width-threshold 0)\r")
    (term-send-string proc "\e:(insert \"s\\tx\")\r")
    (term-send-string proc (kbd "C-a"))
    (term-send-string proc (kbd "C-x 3"))
    (with-timeout (3)
      (while (not (string-match-p "\ns" (buffer-string)))
        (accept-process-output)))
    (let* ((lines (string-split (buffer-string) "\n"))
           (line (cl-find-if (lambda (l) (and (> (length l) 39)
                                               (eq (aref l 0) ?s)))
                             lines)))
      (should line)
      (should (eq (aref line 38) ?$))
      (should (eq (aref line 39) ?|)))))

(ert-deftest term-dispnew-cjk-continuation () ;Bug#80975
  "Test continuation glyphs appear with CJK chars in a split TTY window."
  (with-tty-subprocess 55 159 proc
    (term-send-string proc "\e:(setq split-width-threshold 0)\r")
    (term-send-string proc
      "\e:(dotimes (i 20) (insert (make-string i ?.) (apply #'concat (make-list 50 (string ?\s #x5B57))) \"\\n\"))\r")
    (term-send-string proc "\e<")
    (term-send-string proc (kbd "C-x 3"))
    (with-timeout (3)
      (while (not (string-match-p "\n\\." (buffer-string)))
        (accept-process-output)))
    (let* ((lines (string-split (buffer-string) "\n"))
           (line (cl-find-if (lambda (l)
                               (and (> (length l) 53)
                                    (eq (aref l 0) ?\s)
                                    (eq (aref l 1) ?字)))
                             lines)))
      (should line)
      (should (eq (aref line 52) ?\\))
      (should (eq (aref line 53) ?|)))))

(ert-deftest test-dispnew-right-margin () ;Bug#48257
  "Window divider won't cannibalize right margin of non-rightmost window."
  (with-tty-subprocess 24 80 proc
    (term-send-string proc "\e:(setq split-width-threshold 0)\r")
    (term-send-string proc
                      (concat "\e:(funcall (lambda () "
                              "(setq-local right-margin-width 4) "
                              "(set-window-buffer (selected-window) (current-buffer)) "
                              "(insert (propertize \" \" 'display '((margin right-margin) \"1234\") 'default t)) "
                              "(split-window-right)))\r"))
    (term-send-string proc "\e:(insert \"here\")\r")
    (with-timeout (3)
      (while (not (string-match-p "here" (buffer-string)))
        (accept-process-output)))
    (let* ((lines (string-split (buffer-string) "\n"))
           (line (cl-find-if (lambda (l) (string-prefix-p "here" l)) lines)))
      (should line)
      (should (eq (aref line 38) ?4))
      (should (eq (aref line 39) ?|)))))

(provide 'term-tests)

;;; term-tests.el ends here
