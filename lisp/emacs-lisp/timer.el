;;; timer.el --- run a function with args at some time in future -*- lexical-binding: t -*-

;; Copyright (C) 1996, 2001-2024 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
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

;;; Commentary:

;; This package gives you the capability to run Emacs Lisp commands at
;; specified times in the future, either as one-shots or periodically.

;;; Code:

(eval-when-compile (require 'cl-lib))
(declare-function cl--defsubst-expand "cl-macs")

(cl-defstruct (timer
               (:constructor nil)
               (:copier nil)
               (:constructor timer--create ())
               (:type vector)         ; undefines timer-p (see timerp)
               (:conc-name timer--))
  ;; For ordinary timers, non-nil marks done.
  ;; For idle timers, non-nil marks done for the current idle stretch,
  ;; It is reset to nil after user input concludes the period.
  triggered
  ;; For ordinary timers, absolute time to trigger.
  ;; For idle timers, idle threshold to trigger.
  high-seconds
  low-seconds
  usecs
  ;; For normal timers, time between repetitions, or nil.  For idle
  ;; timers, non-nil iff repeated
  repeat-delay
  ;; Action to perform upon trigger
  function
  args
  ;; Non-nil for idle timer
  idle-delay
  ;; Additional precision for times / durations
  psecs
  ;; Trigger on timer-next-integral-multiple-of-time with
  ;; REPEAT-DELAY as gridwidth
  integral-multiple
  ;; Timers should be associated with struct thread_state since
  ;; exception_stack is thread-specific, but Vtimer_list
  ;; preceded that realization.
  thread)

(defun timer-create ()
  ;; BEWARE: This is not an eta-redex, because `timer--create' is inlinable
  ;; whereas `timer-create' should not be because we don't want to
  ;; hardcode the shape of timers in other .elc files.
  (timer--create))

(defsubst timerp (object)
  "Return t if OBJECT appears to be a timer.
As the timer struct does not implicitly define a timer-p
predicate (since it explicitly shunts to a vector type), we
attempt an heuristic."
  (and (vectorp object) (= (length object) 11)))

(defsubst timer--check (timer)
  (or (and (timerp timer)
           (integerp (timer--high-seconds timer))
           (integerp (timer--low-seconds timer))
           (integerp (timer--usecs timer))
           (integerp (timer--psecs timer))
           (timer--function timer)
           (threadp (timer--thread timer)))
      (error "Invalid timer %S" timer)))

(defun timer--time-setter (timer time)
  (let ((lt (time-convert time 'list)))
    (setf (timer--high-seconds timer) (nth 0 lt))
    (setf (timer--low-seconds timer) (nth 1 lt))
    (setf (timer--usecs timer) (nth 2 lt))
    (setf (timer--psecs timer) (nth 3 lt))
    time))

;; Pseudo field `time'.
(defun timer--time (timer)
  (declare (gv-setter timer--time-setter))
  (list (timer--high-seconds timer)
        (timer--low-seconds timer)
	(timer--usecs timer)
	(timer--psecs timer)))

(defun timer-set-time (timer time &optional delta)
  "Set the trigger time of TIMER to TIME.
TIME must be a Lisp time value.
If optional third argument DELTA is a positive number, make the timer
fire repeatedly that many seconds apart."
  (setf (timer--time timer) time)
  (setf (timer--repeat-delay timer) (and (numberp delta) (> delta 0) delta))
  timer)

(defun timer-set-idle-time (timer secs &optional repeat)
  ;; FIXME: Merge with timer-set-time.
  "Set the trigger idle time of TIMER to SECS.
SECS may be an integer, floating point number, or the internal
time format returned by, e.g., `current-idle-time'.
If optional third argument REPEAT is non-nil, make the timer
fire each time Emacs is idle for that many seconds."
  (setf (timer--time timer) secs)
  (setf (timer--repeat-delay timer) repeat)
  timer)

(defun timer-next-integral-multiple-of-time (time secs)
  "Marking time since epoch in SECS-wide grids, return next grid after TIME.
For example, a SECS of 60 rounds up to the nearest minute.  SECS
may be less than one."
  (let* ((ticks-hz (time-convert time t))
	 (ticks (car ticks-hz))
	 (hz (cdr ticks-hz))
	 trunc-s-ticks)
    (while (let ((s-ticks (* secs hz)))
	     (setq trunc-s-ticks (truncate s-ticks))
	     (/= s-ticks trunc-s-ticks))
      (setq ticks (ash ticks 1))
      (setq hz (ash hz 1)))
    (let ((more-ticks (+ ticks trunc-s-ticks)))
      (time-convert (cons (- more-ticks (% more-ticks trunc-s-ticks)) hz) t))))

(defun timer-relative-time (time secs &optional usecs psecs)
  "Advance TIME by SECS seconds.

Optionally also advance it by USECS microseconds and PSECS
picoseconds.

SECS may be either an integer or a floating point number."
  (let ((delta secs))
    (if (or usecs psecs)
	(setq delta (time-add delta (list 0 0 (or usecs 0) (or psecs 0)))))
    (time-add time delta)))

(defun timer--time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (time-less-p (timer--time t1) (timer--time t2)))

(defun timer-inc-time (timer secs &optional usecs psecs)
  "Increment the time set in TIMER by SECS seconds.

Optionally also increment it by USECS microseconds, and PSECS
picoseconds.  If USECS or PSECS are omitted, they are treated as
zero.

SECS may be a fraction."
  (setf (timer--time timer)
        (timer-relative-time (timer--time timer) secs usecs psecs)))

(defun timer-set-function (timer function &optional args)
  "Make TIMER call FUNCTION with optional ARGS when triggering."
  (setf (timer--function timer) function)
  (setf (timer--args timer) args)
  timer)

(defsubst timer-activate (timer &optional _triggered-p _reuse-cell)
  "Install TIMER."
  (timer--check timer)
  (cl-pushnew timer timer-list))

(defsubst timer-activate-when-idle (timer &optional _dont-wait _reuse-cell)
  "Install idle TIMER"
  (setf (timer--idle-delay timer) 'idle)
  (timer--check timer)
  (cl-pushnew timer timer-idle-list))

(defun cancel-timer (timer)
  "Remove TIMER from the list of active timers."
  (prog1 nil
    (setq timer-list (delq timer timer-list))
    (setq timer-idle-list (delq timer timer-idle-list))))

(defun cancel-function-timers (function)
  "Cancel all timers which would run FUNCTION.
This affects ordinary timers such as are scheduled by `run-at-time',
and idle timers such as are scheduled by `run-with-idle-timer'."
  (interactive "aCancel timers of function: ")
  (dolist (timer timer-list)
    (if (eq (timer--function timer) function)
        (setq timer-list (delq timer timer-list))))
  (dolist (timer timer-idle-list)
    (if (eq (timer--function timer) function)
        (setq timer-idle-list (delq timer timer-idle-list)))))

;; Record the last few events, for debugging.
(defvar timer-event-last nil
  "Last timer that was run.")
(defvar timer-event-last-1 nil
  "Next-to-last timer that was run.")
(defvar timer-event-last-2 nil
  "Third-to-last timer that was run.")

(defcustom timer-max-repeats 10
  "Maximum number of times to repeat a timer, if many repeats are delayed.
Timer invocations can be delayed because Emacs is suspended or busy,
or because the system's time changes.  If such an occurrence makes it
appear that many invocations are overdue, this variable controls
how many will really happen."
  :type 'integer
  :group 'internal)

(defun timer-until (timer time)
  "Calculate number of seconds from when TIMER will run, until TIME.
TIMER is a timer, and stands for the time when its next repeat is scheduled.
TIME is a Lisp time value."
  (float-time (time-subtract time (timer--time timer))))

(defun timer-event-handler (timer)
  "Call the handler for the timer TIMER.
This function is called, by name, directly by the C code."
  (setq timer-event-last-2 timer-event-last-1
        timer-event-last-1 timer-event-last
        timer-event-last timer)
  (let ((inhibit-quit t)
        (run-handler
         (lambda (timer)
           (condition-case-unless-debug err
               (save-current-buffer
                 (setf (timer--triggered timer) t)
                 (let ((restore-deactivate-mark deactivate-mark))
                   (apply (timer--function timer) (timer--args timer))
                   (setq deactivate-mark restore-deactivate-mark)))
             (error (message "Error running timer%s: %s"
                             (if (symbolp (timer--function timer))
                                 (format-message " '%s'" (timer--function timer))
                               "")
                             (error-message-string err)))))))
    (cond ((memq timer timer-list)
           (unwind-protect
               (funcall run-handler timer)
             (if (not (timer--repeat-delay timer))
                 ;; dequeue
                 (cancel-timer timer)
               ;; requeue at new time
               (setf (timer--triggered timer) nil)
               (if (timer--integral-multiple timer)
                   (setf (timer--time timer)
                         (timer-next-integral-multiple-of-time
		          nil (timer--repeat-delay timer)))
                 (timer-inc-time timer (timer--repeat-delay timer)))
               (when (numberp timer-max-repeats)
                 ;; Limit repetitions in case emacs was unduly suspended
                 (let ((limit (time-subtract nil (* timer-max-repeats
                                                    (timer--repeat-delay timer)))))
                   (when (time-less-p (timer--time timer) limit)
                     (setf (timer--time timer) limit)))))))
          ((memq timer timer-idle-list)
           (unwind-protect
               (funcall run-handler timer)
             (unless (timer--repeat-delay timer)
               (cancel-timer timer)))))))

(defun timeout-event-p (event)
  "Non-nil if EVENT is a timeout event."
  (and (listp event) (eq (car event) 'timer-event)))


(declare-function diary-entry-time "diary-lib" (s))

(defun run-at-time (time repeat function &rest args)
  "Perform an action at time TIME.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
REPEAT may be an integer or floating point number.
TIME should be one of:

- a string giving today's time like \"11:23pm\"
  (the acceptable formats are HHMM, H:MM, HH:MM, HHam, HHAM,
  HHpm, HHPM, HH:MMam, HH:MMAM, HH:MMpm, or HH:MMPM;
  a period `.' can be used instead of a colon `:' to separate
  the hour and minute parts);

- a string giving a relative time like \"90\" or \"2 hours 35 minutes\"
  (the acceptable forms are a number of seconds without units
  or some combination of values using units in `timer-duration-words');

- nil, meaning now;

- a number of seconds from now;

- a value from `encode-time';

- or t (with non-nil REPEAT) meaning the next integral multiple
  of REPEAT.  This is handy when you want the function to run at
  a certain \"round\" number.  For instance, (run-at-time t 60 ...)
  will run at 11:04:00, 11:05:00, etc.

The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in
`cancel-timer'."
  (interactive "sRun at time: \nNRepeat interval: \naFunction: ")

  (when (and repeat
             (numberp repeat)
             (< repeat 0))
    (error "Invalid repetition interval"))

  (let ((timer (timer-create)))
    ;; Special case: nil means "now" and is useful when repeating.
    (unless time
      (setq time (current-time)))

    ;; Special case: t means the next integral multiple of REPEAT.
    (when (and (eq time t) repeat)
      (setq time (timer-next-integral-multiple-of-time nil repeat))
      (setf (timer--integral-multiple timer) t))

    ;; Handle numbers as relative times in seconds.
    (when (numberp time)
      (setq time (timer-relative-time nil time)))

    ;; Handle relative times like "2 hours 35 minutes".
    (when (stringp time)
      (when-let ((secs (timer-duration time)))
	(setq time (timer-relative-time nil secs))))

    ;; Handle "11:23pm" and the like.  Interpret it as meaning today
    ;; which admittedly is rather stupid if we have passed that time
    ;; already.  (Though only Emacs hackers hack Emacs at that time.)
    (when (stringp time)
      (require 'diary-lib)
      (let ((hhmm (diary-entry-time time))
	    (now (decode-time)))
	(when (>= hhmm 0)
	  (setq time (encode-time 0 (% hhmm 100) (/ hhmm 100)
                                  (decoded-time-day now)
			          (decoded-time-month now)
                                  (decoded-time-year now)
                                  (decoded-time-zone now))))))

    (timer-set-time timer time repeat)
    (timer-set-function timer function args)
    (setf (timer--thread timer) (current-thread))
    (timer-activate timer)
    timer))

(defun run-with-timer (secs repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
SECS and REPEAT may be integers or floating point numbers.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'."
  (interactive "sRun after delay (seconds): \nNRepeat interval: \naFunction: ")
  (apply #'run-at-time secs repeat function args))

(defun add-timeout (secs function object &optional repeat)
  "Add a timer to run SECS seconds from now, to call FUNCTION on OBJECT.
If REPEAT is non-nil, repeat the timer every REPEAT seconds.

This function returns a timer object which you can use in `cancel-timer'.
This function is for compatibility; see also `run-with-timer'."
  (declare (obsolete run-with-timer "30.1"))
  (run-with-timer secs repeat function object))

(defun run-with-idle-timer (secs repeat function &rest args)
  "Call FUNCTION on ARGS when idle for SECS seconds.
If REPEAT is non-nil, repeat the behavior until cancelled via
`cancel-timer'.  SECS may be an integer, a floating point number,
or the internal time format returned by, e.g.,
`current-idle-time'."
  (interactive
   (list (read-from-minibuffer "Run after idle (seconds): " nil nil t)
	 (y-or-n-p "Repeat each time Emacs is idle? ")
	 (intern (completing-read "Function: " obarray #'fboundp t))))
  (let ((timer (timer-create)))
    (timer-set-function timer function args)
    (setf (timer--thread timer) (current-thread))
    (timer-set-idle-time timer secs repeat)
    (timer-activate-when-idle timer)
    timer))

(defvar with-timeout-timers nil
  "List of all timers used by currently pending `with-timeout' calls.")

(defmacro with-timeout (list &rest body)
  "Run BODY, but if it doesn't finish in SECONDS seconds, give up.
If we give up, we run the TIMEOUT-FORMS and return the value of the last one.
The timeout is checked whenever Emacs waits for some kind of external
event (such as keyboard input, input from subprocesses, or a certain time);
if the program loops without waiting in any way, the timeout will not
be detected.
\n(fn (SECONDS TIMEOUT-FORMS...) BODY)"
  (declare (indent 1) (debug ((form body) body)))
  (let ((seconds (car list))
	(timeout-forms (cdr list))
        (timeout (make-symbol "timeout")))
    `(let ((-with-timeout-value-
            (catch ',timeout
              (let* ((-with-timeout-timer-
                      (run-with-timer ,seconds nil
                                      (lambda () (throw ',timeout ',timeout))))
                     (with-timeout-timers
                         (cons -with-timeout-timer- with-timeout-timers)))
                (unwind-protect
                    (progn ,@body)
                  (cancel-timer -with-timeout-timer-))))))
       ;; It is tempting to avoid the `if' altogether and instead run
       ;; timeout-forms in the timer, just before throwing `timeout'.
       ;; But that would mean that timeout-forms are run in the deeper
       ;; dynamic context of the timer, with inhibit-quit set etc...
       (if (eq -with-timeout-value- ',timeout)
           (progn ,@timeout-forms)
         -with-timeout-value-))))

(defun with-timeout-suspend ()
  "Stop the clock for `with-timeout'.  Used by debuggers.
The idea is that the time you spend in the debugger should not
count against these timeouts.

The value is a list that the debugger can pass to `with-timeout-unsuspend'
when it exits, to make these timers start counting again."
  (mapcar (lambda (timer)
	    (cancel-timer timer)
	    (list timer (time-subtract (timer--time timer) nil)))
	  with-timeout-timers))

(defun with-timeout-unsuspend (timer-spec-list)
  "Restart the clock for `with-timeout'.
The argument should be a value previously returned by `with-timeout-suspend'."
  (dolist (elt timer-spec-list)
    (let ((timer (car elt))
	  (delay (cadr elt)))
      (timer-set-time timer (time-add nil delay))
      (timer-activate timer))))

(defun y-or-n-p-with-timeout (prompt seconds default-value)
  "Like (y-or-n-p PROMPT), with a timeout.
If the user does not answer after SECONDS seconds, return DEFAULT-VALUE."
  (with-timeout (seconds default-value)
    (y-or-n-p prompt)))

(defconst timer-duration-words
  (list (cons "microsec" 0.000001)
	(cons "microsecond" 0.000001)
        (cons "millisec" 0.001)
	(cons "millisecond" 0.001)
        (cons "sec" 1)
	(cons "second" 1)
	(cons "min" 60)
	(cons "minute" 60)
	(cons "hour" (* 60 60))
	(cons "day" (* 24 60 60))
	(cons "week" (* 7 24 60 60))
	(cons "fortnight" (* 14 24 60 60))
	(cons "month" (* 30 24 60 60))	  ; Approximation
	(cons "year" (* 365.25 24 60 60)) ; Approximation
	)
  "Alist mapping temporal words to durations in seconds.")

(defun timer-duration (string)
  "Return number of seconds specified by STRING, or nil if parsing fails."
  (let ((secs 0)
	(start 0)
	(case-fold-search t))
    (while (string-match
	    "[ \t]*\\([0-9.]+\\)?[ \t]*\\([a-z]+[a-rt-z]\\)s?[ \t]*"
	    string start)
      (let ((count (if (match-beginning 1)
		       (string-to-number (match-string 1 string))
		     1))
	    (itemsize (cdr (assoc (match-string 2 string)
				  timer-duration-words))))
	(if itemsize
	    (setq start (match-end 0)
		  secs (+ secs (* count itemsize)))
	  (setq secs nil
		start (length string)))))
    (if (= start (length string))
	secs
      (if (string-match-p "\\`[0-9.]+\\'" string)
	  (string-to-number string)))))

(defun internal-timer-start-idle ()
  "Mark all idle-time timers as once again candidates for running."
  (dolist (timer timer-idle-list)
    (setf (timer--triggered timer) nil)))

(define-obsolete-function-alias 'disable-timeout #'cancel-timer "30.1")
(provide 'timer)

;;; timer.el ends here
