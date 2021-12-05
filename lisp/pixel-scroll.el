;;; pixel-scroll.el --- Scroll a line smoothly  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2021 Free Software Foundation, Inc.
;; Author: Tak Kunihiro <tkk@misasa.okayama-u.ac.jp>
;; Keywords: mouse
;; Package: emacs

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

;; Usage:
;;
;; To interactively toggle the mode:
;;
;;   M-x pixel-scroll-mode RET
;;
;; To make the mode permanent, put this in your Init file:
;;
;;   (pixel-scroll-mode 1)

;;; Commentary:

;; This package offers a global minor mode which makes mouse-wheel
;; scroll a line smoothly.
;;
;; Scrolling a line up by `set-window-vscroll' and that by `scroll-up'
;; give similar display as shown below.
;;
;;  A: (scroll-up 1)
;;  B: (set-window-vscroll nil (frame-char-height) t)
;;
;; Also scrolling a pixel up by `set-window-vscroll' and that by
;; `scroll-up' give similar display, when vscroll is the last pixel of
;; the line, as shown below.
;;
;;  A: (scroll-up 1)
;;  B: (set-window-vscroll nil (1- (frame-char-height) t)) (scroll-up 1)
;;
;; When point reaches to the top of a window on scroll by
;; `set-window-vscroll', vscroll is set to zero.  To scroll a line
;; smoothly and continuously, this package scrolls a line by following
;; sequences.
;;
;;  (vertical-motion 1)
;;  (dolist (vs (number-sequence 1 (1- (frame-char-height))))
;;    (set-window-vscroll nil vs t) (sit-for 0))
;;  (scroll-up 1)

;;; Todo:
;;
;; Allowing pixel-level scrolling in Emacs requires a thorough review
;; of the related functionalities, to make sure none of them zeroes
;; out vscroll where users won't want that.

;;; Code:

(require 'mwheel)
(require 'subr-x)
(require 'ring)

(defvar pixel-wait 0
  "Idle time on each step of pixel scroll specified in second.
More wait will result in slow and gentle scroll.")

(defvar pixel-resolution-fine-flag nil
  "Set scrolling resolution to pixels instead of a line.
When it is t, scrolling resolution is number of pixels obtained
by `frame-char-height' instead of a line.  When it is number,
scrolling resolution is set to number of pixels specified.  In
case you need scrolling resolution of a pixel, set to 1.  After a
pixel scroll, typing \\[next-line] or \\[previous-line] scrolls the window to make it
fully visible, and undoes the effect of the pixel-level scroll.")

(defvar pixel-dead-time 0.1
  "Minimal interval in seconds before next smooth scrolling.
If another scrolling request arrives within this period, scrolling
will be carried out without pixel resolution.  If zero, scrolling
is always with pixel resolution.")

(defvar pixel-last-scroll-time 0
  "Time when the last scrolling was made, in second since the epoch.")

(defvar mwheel-coalesce-scroll-events)

(defvar pixel-scroll-precision-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [wheel-down] #'pixel-scroll-precision)
    (define-key map [wheel-up] #'pixel-scroll-precision)
    (define-key map [touch-end] #'pixel-scroll-start-momentum)
    map)
  "The key map used by `pixel-scroll-precision-mode'.")

(defcustom pixel-scroll-precision-use-momentum nil
  "If non-nil, continue to scroll the display after wheel movement stops.
This is only effective if supported by your mouse or touchpad."
  :group 'mouse
  :type 'boolean
  :version "29.1")

(defcustom pixel-scroll-precision-momentum-tick 0.01
  "Number of seconds between each momentum scroll."
  :group 'mouse
  :type 'float
  :version "29.1")

(defcustom pixel-scroll-precision-momentum-seconds 1.75
  "The maximum duration in seconds of momentum scrolling."
  :group 'mouse
  :type 'float
  :version "29.1")

(defcustom pixel-scroll-precision-momentum-min-velocity 10.0
  "The minimum scrolled pixels per second before momentum scrolling starts."
  :group 'mouse
  :type 'float
  :version "29.1")

(defcustom pixel-scroll-precision-initial-velocity-factor 0.25
  "Factor applied to the initial velocity before momentum scrolling begins."
  :group 'mouse
  :type 'float
  :version "29.1")

(defcustom pixel-scroll-precision-large-scroll-height 70
  "Pixels that must be scrolled before an animation is performed.
Nil means to not interpolate such scrolls."
  :group 'mouse
  :type '(choice (const :tag "Do not interpolate large scrolls" nil)
                 number)
  :version "29.1")

(defun pixel-scroll-in-rush-p ()
  "Return non-nil if next scroll should be non-smooth.
When scrolling request is delivered soon after the previous one,
user is in hurry.  When the time since last scroll is larger than
`pixel-dead-time', we are ready for another smooth scroll, and this
function returns nil."
  (let* ((now (current-time))
	 (scroll-in-rush-p (time-less-p
			    (time-subtract now pixel-last-scroll-time)
			    pixel-dead-time)))
    (setq pixel-last-scroll-time (float-time now))
    scroll-in-rush-p))

;;;###autoload
(define-minor-mode pixel-scroll-mode
  "A minor mode to scroll text pixel-by-pixel."
  :init-value nil
  :group 'scrolling
  :global t
  :version "26.1"

  (if pixel-scroll-mode
      (setq mwheel-scroll-up-function 'pixel-scroll-up
            mwheel-scroll-down-function 'pixel-scroll-down)
    (setq mwheel-scroll-up-function 'scroll-up
          mwheel-scroll-down-function 'scroll-down)))

(defun pixel-scroll-up (&optional arg)
  "Scroll text of selected window up ARG lines.
This is an alternative of `scroll-up'.  Scope moves downward."
  (interactive)
  (or arg (setq arg 1))
  (if (pixel-scroll-in-rush-p)
      (scroll-up arg)
    (dotimes (_ arg)                    ; move scope downward
      (let ((amt (if pixel-resolution-fine-flag
                     (if (integerp pixel-resolution-fine-flag)
                         pixel-resolution-fine-flag
                       (frame-char-height))
                   (pixel-line-height))))
        (if (pixel-eob-at-top-p)      ; when end-of-the-buffer is close
            (scroll-up 1)             ; relay on robust method
          (catch 'no-movement
            (while (pixel-point-at-top-p amt) ; prevent too late (multi tries)
              (unless (>= (vertical-motion 1) 1) ; move point downward
                (throw 'no-movement nil)))) ; exit loop when point did not move
          (pixel-scroll-pixel-up amt))))))  ; move scope downward

(defun pixel-scroll-down (&optional arg)
  "Scroll text of selected window down ARG lines.
This is and alternative of `scroll-down'.  Scope moves upward."
  (interactive)
  (or arg (setq arg 1))
  (if (pixel-scroll-in-rush-p)
      (scroll-down arg)
    (dotimes (_ arg)
      (let ((amt (if pixel-resolution-fine-flag
                     (if (integerp pixel-resolution-fine-flag)
                         pixel-resolution-fine-flag
                       (frame-char-height))
                   (pixel-line-height -1))))
        (catch 'no-movement
          (while (pixel-point-at-bottom-p amt) ; prevent too late (multi tries)
            (unless (<= (vertical-motion -1) -1) ; move point upward
              (throw 'no-movement nil)))) ; exit loop when point did not move
        (if (or (pixel-bob-at-top-p amt) ; when beginning-of-the-buffer is seen
                (pixel-eob-at-top-p))    ; for file with a long line
            (scroll-down 1)              ; relay on robust method
          (pixel-scroll-pixel-down amt))))))

(defun pixel-bob-at-top-p (amt)
  "Return non-nil if window-start is at beginning of the current buffer.
Window must be vertically scrolled by not more than AMT pixels."
  (and (equal (window-start) (point-min))
       (< (window-vscroll nil t) amt)))

(defun pixel-eob-at-top-p ()
  "Return non-nil if end of buffer is at top of window."
  (<= (count-lines (window-start) (window-end)) 2)) ; count-screen-lines

(defun pixel-posn-y-at-point ()
  "Return y coordinates of point in pixels of current window.
This returns nil when horizontally scrolled."
  (when (equal (window-hscroll) 0)
    (save-excursion
      ;; When there's an overlay string on a line, move
      ;; point by (beginning-of-visual-line).
      (beginning-of-visual-line)
      ;; (- (cadr (pos-visible-in-window-p (point) nil t))
      ;;    (line-pixel-height))
      (cdr (posn-x-y (posn-at-point))))))

(defun pixel-point-at-top-p (amt)
  "Return if point is located at top of a window on coming scroll of AMT pixels.
When location of point was not obtained, this returns if point is at top
of window."
  (let ((y (pixel-posn-y-at-point))
        top-margin)
    (cond
     (y
      (setq top-margin y)
      (< top-margin amt))
     (t
      (<= (count-lines (window-start) (point)) 1)))))

(defun pixel-point-at-bottom-p (amt)
  "Return if point is located at bottom of window on coming scroll of AMT pixels.
When location of point was not obtained, this returns nil."
  (let* ((edges (window-inside-pixel-edges))
         (height (- (nth 3 edges) (nth 1 edges))) ; (- bottom top)
         (y (pixel-posn-y-at-point))
         bottom-margin)
    (when y
      (setq bottom-margin (- height (+ y (pixel-visual-line-height))))
      (< bottom-margin amt)))) ; coming unseen line

(defun pixel-scroll-pixel-up (amt)
  "Scroll text of selected windows up AMT pixels.
Scope moves downward."
  (while (>= (+ (window-vscroll nil t) amt)
             (pixel-line-height))
    (setq amt (- amt (pixel--whistlestop-line-up)))) ; major scroll
  (pixel--whistlestop-pixel-up amt)) ; minor scroll

(defun pixel-scroll-pixel-down (amt)
  "Scroll text of selected windows down AMT pixels.
Scope moves upward."
  (while (> amt 0)
    (let ((vs (window-vscroll nil t)))
      (if (equal vs 0)
          (progn
            ;; On horizontal scrolling, move cursor.
            (when (> (window-hscroll) 0)
              (vertical-motion -1))
            (pixel-scroll-down-and-set-window-vscroll
             (1- (pixel-line-height -1))))
        (set-window-vscroll nil (1- vs) t))
      (setq amt (1- amt))
      (sit-for pixel-wait))))

(defun pixel--whistlestop-line-up ()
  "Scroll text upward a line with each pixel whistlestopped.
When `vscroll' is non-zero, complete scrolling a line.  When
`vscroll' is larger than height of multiple lines, for example
88, this flushes multiple lines.  At the end, `vscroll' will be
zero.  This assumes that the lines are with the same height.
Scope moves downward.  This function returns number of pixels
that was scrolled."
  (let* ((src (window-vscroll nil t))  ; EXAMPLE (initial)      @0   @8  @88
         (height (pixel-line-height))  ;                        25   25   23
         (line (1+ (/ src height)))    ; catch up + one line     1    1    4
         (dst (* line height))         ; goal                  @25  @25  @92
         (delta (- dst src)))          ; pixels to be scrolled  25   17    4
    (pixel--whistlestop-pixel-up (1- delta)) ; until one less  @24  @24  @91
    (dotimes (_ line)
      ;; On horizontal scrolling, move cursor.
      (when (> (window-hscroll) 0)
        (vertical-motion 1))
      (scroll-up 1))
    (sit-for pixel-wait)               ; scroll 1 pixel         @0   @0   @0
    delta))

(defun pixel--whistlestop-pixel-up (n)
  "Scroll text upward by N pixels with each pixel whistlestopped.
Scope moves downward."
  (when (> n 0)
    (let ((vs0 (window-vscroll nil t)))
      (dolist (vs (number-sequence (1+ vs0) (+ vs0 n)))
        (set-window-vscroll nil vs t) (sit-for pixel-wait)))))

(defun pixel-line-height (&optional pos)
  "Return height in pixels of text line at POS in the selected window.
When POS is nil or negative, height of the first line or the coming
unseen line above the first line, respectively, is provided."
  (or pos (setq pos (window-start)))
  (when (< pos 0)
    (setq pos (pixel-point-at-unseen-line)))
  (let ((vs1 (window-vscroll nil t))
        height)
    (set-window-vscroll nil 0 t)
    (save-excursion
      (goto-char pos)
      (setq height (pixel-visual-line-height))) ; line-pixel-height, frame-char-height
    (set-window-vscroll nil vs1 t)
    height))

(defun pixel-visual-line-height ()
  "Return height in pixels of text line where cursor is in the selected window."
  (let ((pos (pixel-visible-pos-in-window)))
    (cond
     ;; When a char of line is shown, obtain height by
     ;; (line-pixel-height).
     (pos (save-excursion (goto-char pos) (line-pixel-height)))
     ;; When no char of line is shown but the line is at the top,
     ;; obtain height by (line-pixel-height).  This is based on
     ;; expected response from display engine.  See following
     ;; discussion.
     ;; https://lists.gnu.org/r/emacs-devel/2017-10/msg00621.html
     ((equal (count-lines (window-start) (point)) 1)
      (line-pixel-height))
     ;; No char of line is shown and the line is not at the top,
     ;; obtain height by (frame-char-height).
     (t (frame-char-height)))))

(defun pixel-visible-pos-in-window ()
  "Return position shown on text line where cursor is in the selected window.
This will look for positions of point and `end-of-visual-line',
then positions from `beginning-of-visual-line' to
`end-of-visual-line'.  When no char in a line is shown, this
returns nil."
  (let* ((beginning-of-visual-line-pos (save-excursion (beginning-of-visual-line) (point)))
         (end-of-visual-line-pos (save-excursion (end-of-visual-line) (point)))
         (pos-list (number-sequence beginning-of-visual-line-pos end-of-visual-line-pos))
         (edges (window-inside-pixel-edges))
         (width (- (nth 2 edges) (nth 0 edges)))
         posn-x
         visible-pos)
    ;; Optimize list of position to be surveyed.
    (push end-of-visual-line-pos pos-list)
    (push (point) pos-list)
    (delete-dups pos-list)
    ;; Find out a char with position X that is more than zero and less
    ;; than width of screen.
    (while (and (not visible-pos)
                pos-list)
      (setq posn-x (car (pos-visible-in-window-p (car pos-list) nil t)))
      (if (and posn-x
               (<= 0 posn-x)
               (< posn-x width))
          (setq visible-pos (car pos-list))
        (setq pos-list (cdr pos-list))))
    visible-pos))

(defun pixel-point-and-height-at-unseen-line ()
  "Return the position and pixel height of line above the selected window.
The returned value is a cons of the position of the first
character on the unseen line just above the scope of current
window, and the pixel height of that line."
  (let* ((pos0 (window-start))
         (vscroll0 (window-vscroll nil t))
         (line-height nil)
         (pos
          (save-excursion
            (goto-char pos0)
            (if (bobp)
                (point-min)
              (vertical-motion -1)
              (setq line-height (line-pixel-height))
              (point)))))
    ;; restore initial position
    (set-window-start nil pos0 t)
    (set-window-vscroll nil vscroll0 t)
    (cons pos line-height)))

(defun pixel-point-at-unseen-line ()
  "Return the character position of line above the selected window.
The returned value is the position of the first character on the
unseen line just above the scope of current window."
  (car (pixel-point-and-height-at-unseen-line)))

(defun pixel-scroll-down-and-set-window-vscroll (vscroll)
  "Scroll down a line and set VSCROLL in pixels.
It is important to call `set-window-start' to force the display
engine use that particular position as the window-start point.
Otherwise, redisplay will reset the window's vscroll."
  (set-window-start nil (pixel-point-at-unseen-line) t)
  (set-window-vscroll nil vscroll t))

(defun pixel-scroll-precision-scroll-down-page (delta)
  "Scroll the current window down by DELTA pixels.
Note that this function doesn't work if DELTA is larger than
the height of the current window."
  (let* ((desired-pos (posn-at-x-y 0 (+ delta
					(window-tab-line-height)
					(window-header-line-height))))
         (object (posn-object desired-pos))
	 (desired-start (posn-point desired-pos))
	 (desired-vscroll (cdr (posn-object-x-y desired-pos)))
         (edges (window-edges nil t))
         (usable-height (- (nth 3 edges)
                           (nth 1 edges)))
         (next-pos (save-excursion
                     (goto-char desired-start)
                     (when (zerop (vertical-motion (1+ scroll-margin)))
                       (signal 'end-of-buffer nil))
                     (point)))
         (end-pos (posn-at-x-y 0 (+ usable-height
                                    (window-tab-line-height)
				    (window-header-line-height)))))
    (if (or (overlayp object)
            (stringp object)
            (and (consp object)
                 (stringp (car object)))
            (and (consp (posn-object end-pos))
                 (> (cdr (posn-object-x-y end-pos)) 0)))
        ;; We are either on an overlay or a string, so set vscroll
        ;; directly.
        (set-window-vscroll nil (+ (window-vscroll nil t)
                                   delta)
                            t)
      (unless (eq (window-start) desired-start)
        (set-window-start nil (if (zerop (window-hscroll))
                                  desired-start
                                (save-excursion
                                  (goto-char desired-start)
                                  (beginning-of-visual-line)
                                  (point)))
                          t))
      (set-window-vscroll nil desired-vscroll t))
    (if (and (or (< (point) next-pos))
             (let ((pos-visibility (pos-visible-in-window-p next-pos nil t)))
               (and pos-visibility
                    (or (eq (length pos-visibility) 2)
                        (when-let* ((posn (posn-at-point next-pos)))
                          (> (cdr (posn-object-width-height posn))
                             usable-height))))))
        (goto-char next-pos))))

(defun pixel-scroll-precision-scroll-down (delta)
  "Scroll the current window down by DELTA pixels."
  (let ((max-height (- (window-text-height nil t)
                       (frame-char-height))))
    (while (> delta max-height)
      (pixel-scroll-precision-scroll-down-page max-height)
      (setq delta (- delta max-height)))
    (pixel-scroll-precision-scroll-down-page delta)))

(defun pixel-scroll-precision-scroll-up-page (delta)
  "Scroll the current window up by DELTA pixels.
Note that this function doesn't work if DELTA is larger than
the height of the current window."
  (let* ((edges (window-edges nil t nil t))
         (max-y (- (nth 3 edges)
                   (nth 1 edges)))
         (usable-height max-y)
         (posn (posn-at-x-y 0 (+ (window-tab-line-height)
                                 (window-header-line-height)
                                 (- max-y delta))))
         (point (posn-point posn))
         (up-point (save-excursion
                     (goto-char point)
                     (vertical-motion (- (1+ scroll-margin)))
                     (point))))
    (when (> (point) up-point)
      (when (let ((pos-visible (pos-visible-in-window-p up-point nil t)))
              (or (eq (length pos-visible) 2)
                  (when-let* ((posn (posn-at-point up-point))
                              (edges (window-edges nil t))
                              (usable-height (- (nth 3 edges)
                                                (nth 1 edges))))
                    (> (cdr (posn-object-width-height posn))
                       usable-height))))
        (goto-char up-point)))
    (let ((current-vscroll (window-vscroll nil t)))
      (if (<= delta current-vscroll)
          (set-window-vscroll nil (- current-vscroll delta) t)
        (setq delta (- delta current-vscroll))
        (set-window-vscroll nil 0 t)
        (while (> delta 0)
          (let ((position (pixel-point-and-height-at-unseen-line)))
            (unless (cdr position)
              (signal 'beginning-of-buffer nil))
            (set-window-start nil (car position) t)
            ;; If the line above is taller than the window height (i.e. there's
            ;; a very tall image), keep point on it.
            (when (> (cdr position) usable-height)
              (goto-char (car position)))
            (setq delta (- delta (cdr position)))))
        (when (< delta 0)
          (if-let* ((desired-pos (posn-at-x-y 0 (+ (- delta)
					           (window-tab-line-height)
					           (window-header-line-height))))
	            (desired-start (posn-point desired-pos))
	            (desired-vscroll (cdr (posn-object-x-y desired-pos))))
              (progn
                (set-window-start nil (if (zerop (window-hscroll))
                                          desired-start
                                        (save-excursion
                                          (goto-char desired-start)
                                          (beginning-of-visual-line)
                                          (point)))
                                  t)
                (set-window-vscroll nil desired-vscroll t))
            (set-window-vscroll nil (abs delta) t)))))))

(defun pixel-scroll-precision-interpolate (delta)
  "Interpolate a scroll of DELTA pixels.
This results in the window being scrolled by DELTA pixels with an
animation."
  (while-no-input
    (let ((percentage 0)
          (total-time 0.01)
          (time-elapsed 0.0)
          (between-scroll 0.001))
      (while (< percentage 1)
        (sit-for between-scroll)
        (setq time-elapsed (+ time-elapsed between-scroll)
              percentage (/ time-elapsed total-time))
        (if (< delta 0)
            (pixel-scroll-precision-scroll-down
             (ceiling (abs (* delta
                              (/ between-scroll total-time)))))
          (pixel-scroll-precision-scroll-up
           (ceiling (* delta
                       (/ between-scroll total-time)))))
        (redisplay t)))))

(defun pixel-scroll-precision-scroll-up (delta)
  "Scroll the current window up by DELTA pixels."
  (let ((max-height (- (window-text-height nil t)
                       (frame-char-height))))
    (while (> delta max-height)
      (pixel-scroll-precision-scroll-up-page max-height)
      (setq delta (- delta max-height)))
    (pixel-scroll-precision-scroll-up-page delta)))

;; FIXME: This doesn't _always_ work when there's an image above the
;; current line that is taller than the window, and scrolling can
;; sometimes be jumpy in that case.
(defun pixel-scroll-precision (event)
  "Scroll the display vertically by pixels according to EVENT.
Move the display up or down by the pixel deltas in EVENT to
scroll the display according to the user's turning the mouse
wheel."
  (interactive "e")
  (let ((window (mwheel-event-window event)))
    (if (and (nth 4 event))
        (let ((delta (round (cdr (nth 4 event)))))
          (unless (zerop delta)
            (if (> (abs delta) (window-text-height window t))
                (mwheel-scroll event nil)
              (with-selected-window window
                (if (and pixel-scroll-precision-large-scroll-height
                         (> (abs delta)
                            pixel-scroll-precision-large-scroll-height)
                         (let* ((kin-state (pixel-scroll-kinetic-state))
                                (ring (aref kin-state 0))
                                (time (aref kin-state 1)))
                           (or (null time)
                               (> (- (float-time) time) 1.0)
                               (and (consp ring)
                                    (ring-empty-p ring)))))
                    (progn
                      (let ((kin-state (pixel-scroll-kinetic-state)))
                        (aset kin-state 0 (make-ring 10))
                        (aset kin-state 1 nil))
                      (pixel-scroll-precision-interpolate delta))
                  (condition-case nil
                      (progn
                        (if (< delta 0)
	                    (pixel-scroll-precision-scroll-down (- delta))
                          (pixel-scroll-precision-scroll-up delta))
                        (pixel-scroll-accumulate-velocity delta))
                    ;; Do not ding at buffer limits.  Show a message instead.
                    (beginning-of-buffer
                     (message (error-message-string '(beginning-of-buffer))))
                    (end-of-buffer
                     (message (error-message-string '(end-of-buffer))))))))))
      (mwheel-scroll event nil))))

(defun pixel-scroll-kinetic-state ()
  "Return the kinetic scroll state of the current window.
It is a vector of the form [ VELOCITY TIME ]."
  (or (window-parameter nil 'kinetic-state)
      (set-window-parameter nil 'kinetic-state
                            (vector (make-ring 10) nil))))

(defun pixel-scroll-accumulate-velocity (delta)
  "Accumulate DELTA into the current window's kinetic scroll state."
  (let* ((state (pixel-scroll-kinetic-state))
         (ring (aref state 0))
         (time (aref state 1)))
    (when (or (and time (> (- (float-time) time) 0.5))
              (and (not (ring-empty-p ring))
                   (not (eq (< delta 0)
                            (< (cdr (ring-ref ring 0))
                               0)))))
      (aset state 0 (make-ring 10)))
    (ring-insert (aref state 0)
                 (cons (aset state 1 (float-time))
                       delta))))

(defun pixel-scroll-calculate-velocity (state)
  "Calculate velocity from the kinetic state vector STATE."
  (let* ((ring (aref state 0))
         (elts (ring-elements ring))
         (total 0))
    (dolist (tem elts)
      (setq total (+ total (cdr tem))))
    (/ total (* (- (float-time) (caar elts))
                100))))

(defun pixel-scroll-start-momentum (event)
  "Start kinetic scrolling for the touch event EVENT."
  (interactive "e")
  (when pixel-scroll-precision-use-momentum
    (let ((window (mwheel-event-window event))
          (state nil))
      (with-selected-window window
        (setq state (pixel-scroll-kinetic-state))
        (when (and (aref state 1)
                   (listp (aref state 0)))
          (while-no-input
            (unwind-protect (progn
                              (aset state 0 (pixel-scroll-calculate-velocity state))
                              (when (> (abs (aref state 0))
                                       pixel-scroll-precision-momentum-min-velocity)
                                (let* ((velocity (* (aref state 0)
                                                    pixel-scroll-precision-initial-velocity-factor))
                                       (original-velocity velocity)
                                       (time-spent 0))
                                  (if (> velocity 0)
                                      (while (and (> velocity 0)
                                                  (<= time-spent
                                                      pixel-scroll-precision-momentum-seconds))
                                        (when (> (round velocity) 0)
                                          (pixel-scroll-precision-scroll-up (round velocity)))
                                        (setq velocity (- velocity
                                                          (/ original-velocity
                                                             (/ pixel-scroll-precision-momentum-seconds
                                                                pixel-scroll-precision-momentum-tick))))
                                        (redisplay t)
                                        (sit-for pixel-scroll-precision-momentum-tick)
                                        (setq time-spent (+ time-spent
                                                            pixel-scroll-precision-momentum-tick))))
                                  (while (and (< velocity 0)
                                              (<= time-spent
                                                  pixel-scroll-precision-momentum-seconds))
                                    (when (> (round (abs velocity)) 0)
                                      (pixel-scroll-precision-scroll-down (round
                                                                           (abs velocity))))
                                    (setq velocity (+ velocity
                                                      (/ (abs original-velocity)
                                                         (/ pixel-scroll-precision-momentum-seconds
                                                            pixel-scroll-precision-momentum-tick))))
                                    (redisplay t)
                                    (sit-for pixel-scroll-precision-momentum-tick)
                                    (setq time-spent (+ time-spent
                                                        pixel-scroll-precision-momentum-tick))))))
              (aset state 0 (make-ring 10))
              (aset state 1 nil))))))))

;;;###autoload
(define-minor-mode pixel-scroll-precision-mode
  "Toggle pixel scrolling.
When enabled, this minor mode allows to scroll the display
precisely, according to the turning of the mouse wheel."
  :global t
  :group 'mouse
  :keymap pixel-scroll-precision-mode-map
  (setq mwheel-coalesce-scroll-events
        (not pixel-scroll-precision-mode)))

(provide 'pixel-scroll)
;;; pixel-scroll.el ends here
