;;; thread-tests.el --- tests for threads. -*- lexical-binding: t -*-

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

(require 'thread)
(require 'eieio)
(require 'ring)

;; Declare the functions in case Emacs has been configured --without-threads.
(declare-function all-threads "thread.c" ())
(declare-function condition-mutex "thread.c" (cond))
(declare-function condition-name "thread.c" (cond))
(declare-function condition-notify "thread.c" (cond &optional all))
(declare-function condition-wait "thread.c" (cond))
(declare-function current-thread "thread.c" ())
(declare-function make-condition-variable "thread.c" (mutex &optional name))
(declare-function make-mutex "thread.c" (&optional name))
(declare-function make-thread "thread.c" (function &optional name noncooperative))
(declare-function mutex-lock "thread.c" (mutex))
(declare-function mutex-unlock "thread.c" (mutex))
(declare-function thread--blocker "thread.c" (thread))
(declare-function thread-live-p "thread.c" (thread))
(declare-function thread-join "thread.c" (thread))
(declare-function thread-last-error "thread.c" (&optional cleanup))
(declare-function thread-name "thread.c" (thread))
(declare-function thread-signal "thread.c" (thread error-symbol data))
(declare-function thread-yield "thread.c" ())
(defvar main-thread)
(defvar-local thread-test-bug48990 "global")

(defclass thread-test-channel ()
  ((condition :initarg :condition)
   (msg-queue :initarg :msg-queue)))

(cl-defmethod thread-test-channel-send ((channel thread-test-channel) message)
  (with-slots (condition msg-queue) channel
    (with-mutex (condition-mutex condition)
      (while (<= (ring-size msg-queue) (ring-length msg-queue))
        (condition-wait condition))
      (ring-insert msg-queue message)
      (condition-notify condition t))))

(cl-defmethod thread-test-channel-recv ((channel thread-test-channel))
  (with-slots (condition msg-queue) channel
    (with-mutex (condition-mutex condition)
      (while (ring-empty-p msg-queue)
        (condition-wait condition))
      (prog1 (ring-remove msg-queue)
        (condition-notify condition t)))))

(ert-deftest threads-is-one ()
  "Test for existence of a thread."
  (skip-unless (featurep 'threads))
  (should (current-thread)))

(ert-deftest threads-threadp ()
  "Test of threadp."
  (skip-unless (featurep 'threads))
  (should (threadp (current-thread))))

(ert-deftest threads-type ()
  "Test of thread type."
  (skip-unless (featurep 'threads))
  (should (eq (type-of (current-thread)) 'thread)))

(ert-deftest threads-name ()
  "Test for name of a thread."
  (skip-unless (featurep 'threads))
  (should
   (string= "hi bob" (thread-name (make-thread #'ignore "hi bob")))))

(ert-deftest threads-live ()
  "Test for thread liveness."
  (skip-unless (featurep 'threads))
  (should
   (thread-live-p (make-thread #'ignore))))

(ert-deftest threads-all-threads ()
  "Simple test for `all-threads'."
  (skip-unless (featurep 'threads))
  (should (listp (all-threads))))

(ert-deftest threads-main-thread ()
  "Simple test for `all-threads'."
  (skip-unless (featurep 'threads))
  (should (eq main-thread (car (all-threads)))))

(defvar thread-test-global nil)

(defun thread-test-thread1 ()
  (setq thread-test-global 23))

(ert-deftest threads-basic ()
  "Basic thread test."
  (skip-unless (featurep 'threads))
  (should
   (progn
     (setq thread-test-global nil)
     (make-thread #'thread-test-thread1)
     (while (not thread-test-global)
       (thread-yield))
     thread-test-global)))

(ert-deftest threads-join ()
  "Test of `thread-join'."
  (skip-unless (featurep 'threads))
  (should
   (progn
     (setq thread-test-global nil)
     (let ((thread (make-thread #'thread-test-thread1)))
       (and (= (thread-join thread) 23)
            (= thread-test-global 23)
            (not (thread-live-p thread)))))))

(ert-deftest threads-join-self ()
  "Cannot `thread-join' the current thread."
  (skip-unless (featurep 'threads))
  (should-error (thread-join (current-thread))))

(ert-deftest threads-join-error ()
  "Test of error signaling from `thread-join'."
  :tags '(:unstable)
  (skip-unless (featurep 'threads))
  (let ((thread (make-thread #'threads-call-error)))
    (while (thread-live-p thread)
      (thread-yield))
    (should-error (thread-join thread))))

(defvar thread-test-binding nil)

(defun thread-test-thread2 ()
  (let ((thread-test-binding 23))
    (thread-yield))
  (setq thread-test-global 23))

(ert-deftest threads-let-binding ()
  "Simple test of threads and let bindings."
  (skip-unless (featurep 'threads))
  (should
   (progn
     (setq thread-test-global nil)
     (make-thread #'thread-test-thread2)
     (while (not thread-test-global)
       (thread-yield))
     (and (not thread-test-binding)
	  thread-test-global))))

(ert-deftest threads-mutexp ()
  "Simple test of `mutexp'."
  (skip-unless (featurep 'threads))
  (should-not (mutexp 'hi)))

(ert-deftest threads-mutexp-2 ()
  "Another simple test of `mutexp'."
  (skip-unless (featurep 'threads))
  (should (mutexp (make-mutex))))

(ert-deftest threads-mutex-type ()
  "type-of mutex."
  (skip-unless (featurep 'threads))
  (should (eq (type-of (make-mutex)) 'mutex)))

(ert-deftest threads-mutex-lock-unlock ()
  "Test `mutex-lock' and unlock."
  (skip-unless (featurep 'threads))
  (should
   (let ((mx (make-mutex)))
     (mutex-lock mx)
     (mutex-unlock mx)
     t)))

(ert-deftest threads-mutex-recursive ()
  "Test mutex recursion."
  (skip-unless (featurep 'threads))
  (should
   (let ((mx (make-mutex)))
     (mutex-lock mx)
     (mutex-lock mx)
     (mutex-unlock mx)
     (mutex-unlock mx)
     t)))

(defvar threads-mutex nil)
(defvar threads-mutex-key nil)

(defun thread-test-mlock ()
  (mutex-lock threads-mutex)
  (setq threads-mutex-key 23)
  (while threads-mutex-key
    (thread-yield))
  (mutex-unlock threads-mutex))

(ert-deftest threads-mutex-contention ()
  "Test of mutex contention."
  (skip-unless (featurep 'threads))
  (should
   (progn
     (setq threads-mutex (make-mutex))
     (setq threads-mutex-key nil)
     (make-thread #'thread-test-mlock)
     ;; Wait for other thread to get the lock.
     (while (not threads-mutex-key)
       (thread-yield))
     ;; Try now.
     (setq threads-mutex-key nil)
     (mutex-lock threads-mutex)
     (mutex-unlock threads-mutex)
     t)))

(defun thread-test-mlock2 ()
  (setq threads-mutex-key 23)
  (mutex-lock threads-mutex))

(ert-deftest threads-mutex-signal ()
  "Test signaling a blocked thread."
  (skip-unless (featurep 'threads))
  (should-error
   (progn
     (setq threads-mutex (make-mutex))
     (setq threads-mutex-key nil)
     (mutex-lock threads-mutex)
     (let ((thr (make-thread #'thread-test-mlock2)))
       (while (not threads-mutex-key)
	 (thread-yield))
       (thread-signal thr 'quit nil)
       ;; `quit' is not caught by `should-error'.  We must indicate it.
       (condition-case nil
           (thread-join thr)
         (quit (signal 'error nil)))))))

(defun thread-test-io-switch ()
  (setq thread-test-global 23))

(ert-deftest threads-io-switch ()
  "Test that `accept-process-output' causes thread switch."
  (skip-unless (featurep 'threads))
  (should
   (progn
     (setq thread-test-global nil)
     (make-thread #'thread-test-io-switch)
     (while (not thread-test-global)
       (accept-process-output nil 1))
     thread-test-global)))

(ert-deftest threads-condvarp ()
  "Simple test of `condition-variable-p'."
  (skip-unless (featurep 'threads))
  (should-not (condition-variable-p 'hi)))

(ert-deftest threads-condvarp-2 ()
  "Another simple test of `condition-variable-p'."
  (skip-unless (featurep 'threads))
  (should (condition-variable-p (make-condition-variable (make-mutex)))))

(ert-deftest threads-condvar-type ()
  "type-of condvar"
  (skip-unless (featurep 'threads))
  (should (eq (type-of (make-condition-variable (make-mutex)))
	      'condition-variable)))

(ert-deftest threads-condvar-mutex ()
  "Simple test of `condition-mutex'."
  (skip-unless (featurep 'threads))
  (should
   (let ((m (make-mutex)))
     (eq m (condition-mutex (make-condition-variable m))))))

(ert-deftest threads-condvar-name ()
  "Simple test of `condition-name'."
  (skip-unless (featurep 'threads))
  (should
     (eq nil (condition-name (make-condition-variable (make-mutex))))))

(ert-deftest threads-condvar-name-2 ()
  "Another simple test of `condition-name'."
  (skip-unless (featurep 'threads))
  (should
     (string= "hi bob"
	      (condition-name (make-condition-variable (make-mutex)
						       "hi bob")))))

(defun threads-call-error ()
  "Call `error'."
  (error "Error is called"))

;; This signals an error internally; the error should be caught.
(defun threads-custom ()
  (defcustom threads-custom-face 'highlight
    "Face used for thread customizations."
    :type 'face
    :group 'widget-faces))

(ert-deftest threads-errors ()
  "Test what happens when a thread signals an error."
  (skip-unless (featurep 'threads))
  (let (th1 th2)
    (setq th1 (make-thread #'threads-call-error "call-error"))
    (should (threadp th1))
    (while (thread-live-p th1)
      (thread-yield))
    (should (equal (thread-last-error)
                   '(error "Error is called")))
    (should (equal (thread-last-error 'cleanup)
                   '(error "Error is called")))
    (should-not (thread-last-error))
    (setq th2 (make-thread #'threads-custom "threads-custom"))
    (should (threadp th2))))

(ert-deftest threads-sticky-point ()
  "Test bug #25165 with point movement in cloned buffer."
  (skip-unless (featurep 'threads))
  (with-temp-buffer
    (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit.")
    (goto-char (point-min))
    (clone-indirect-buffer nil nil)
    (forward-char 20)
    (sit-for 1)
    (should (= (point) 21))))

(ert-deftest threads-signal-early ()
  "Test signaling a thread as soon as it is started by the OS."
  (skip-unless (featurep 'threads))
  (let ((thread
         (make-thread (lambda ()
                        (while t (thread-yield))))))
    (thread-signal thread 'error nil)
    (funcall (if noninteractive #'sit-for #'sleep-for) 1)
    (should-not (thread-live-p thread))
    (should (equal (thread-last-error) '(error)))))

(ert-deftest threads-signal-main-thread ()
  "Test signaling the main thread."
  (skip-unless (featurep 'threads))
  ;; We cannot use `ert-with-message-capture', because threads do not
  ;; know let-bound variables.
  (with-current-buffer "*Messages*"
    (let (buffer-read-only)
      (erase-buffer))
    (let ((thread
           (make-thread (lambda () (thread-signal main-thread 'error nil)))))
      (while (thread-live-p thread)
        (thread-yield))
      (read-event nil nil 0.1)
      ;; No error has been raised, which is part of the test.
      (should
       (string-match
        (format-message "Error %s: (error nil)" thread)
        (buffer-string ))))))

(defvar threads-condvar nil)

(defun thread-test-condvar-wait ()
  ;; Wait for condvar to be notified.
  (with-mutex (condition-mutex threads-condvar)
    (condition-wait threads-condvar))
  ;; Wait again, it will be signaled.
  (with-mutex (condition-mutex threads-condvar)
    (condition-wait threads-condvar)))

(ert-deftest threads-condvar-wait ()
  "Test waiting on conditional variable."
  (skip-unless (featurep 'threads))
  (skip-when (and (eq system-type 'darwin) (getenv "GITHUB_ACTIONS")))
  (let ((cv-mutex (make-mutex))
        new-thread)
    ;; We could have spurious threads from the previous tests still
    ;; running; wait for them to die.
    (while (> (length (all-threads)) 1)
      (thread-yield))
    (setq threads-condvar (make-condition-variable cv-mutex))
    (setq new-thread (make-thread #'thread-test-condvar-wait))

    ;; Make sure new-thread is alive.
    (should (thread-live-p new-thread))
    (should (= (length (all-threads)) 2))
    ;; Wait for new-thread to become blocked on the condvar.
    (while (not (eq (thread--blocker new-thread) threads-condvar))
      (thread-yield))

    ;; Notify the waiting thread.
    (with-mutex cv-mutex
      (condition-notify threads-condvar t))
    ;; Allow new-thread to process the notification.
    (sleep-for 0.1)
    ;; Make sure the thread is still there.  This used to fail due to
    ;; a bug in thread.c:condition_wait_callback.
    (should (thread-live-p new-thread))
    (should (= (length (all-threads)) 2))
    (should (eq (thread--blocker new-thread) threads-condvar))

    ;; Signal the thread.
    (thread-signal new-thread 'error '("Die, die, die!"))
    (sleep-for 0.1)
    ;; Make sure the thread died.
    (should (= (length (all-threads)) 1))
    (should (equal (thread-last-error) '(error "Die, die, die!")))))

(ert-deftest thread-test-bug33073 ()
  (skip-unless (featurep 'threads))
  (let ((th (make-thread 'ignore)))
    (should-not (equal th main-thread))))

(ert-deftest thread-test-bug48990 ()
  "Bug#48990 shows buffer-local and global variables getting clobbered.
Fixed in b8460fc by transplanting the `backtrace-eval` logic into the context switch
code, the former having already summited that peak."
  (skip-unless (featurep 'threads))
  (thread-last-error t)
  (cl-flet ((doit (global-p)
              (with-temp-buffer
                (let ((success 0))
                  (with-temp-buffer
                    (dotimes (i 5 (progn
                                    (cl-loop repeat 50
                                             until (zerop (1- (length (all-threads))))
                                             do (accept-process-output nil 0.2))
                                    (should-not (thread-last-error))
                                    (should (= success i))
                                    (should (equal thread-test-bug48990
                                                   (format "local-%d" (1- i))))))
                      (setq thread-test-bug48990 (format "local-%d" i))
                      (should (equal thread-test-bug48990 (format "local-%d" i)))
                      (make-thread
                       (lambda ()
                         (let ((body (lambda ()
                                       (let ((thread-test-bug48990 "let"))
                                         (sleep-for (1+ (random 2)))
                                         (when (equal thread-test-bug48990 "let")
                                           (cl-incf success)))))
                               (b (concat "*buffer-" (thread-name (current-thread)) "*")))
                           (unwind-protect
                               (with-current-buffer (if global-p
                                                        "*scratch*"
                                                      (get-buffer-create b))
                                 (funcall body))
                             (let (kill-buffer-query-functions)
                               (when (buffer-live-p b)
                                 (kill-buffer b))))))
                       (format "%d" i)))))
                (should (equal thread-test-bug48990 "global")))))
    (doit t)
    (doit nil)))

(ert-deftest thread-test-bug36609-signal ()
  "Would only fail under TEST_INTERACTIVE=yes, and not every time.
The failure manifests only by being unable to exit the interactive emacs."
  (skip-unless (featurep 'threads))
  ;; 1. only happens under make check, not make test/src/thread-tests
  ;; 2. doesn't appear related to gc
  ;; 3. is hanging on g_main_context_acquire in xg_select?
  ;; 4. still happens on make -j1 check (not emacsen fighting)
  (skip-when (and (cl-search "enable-multithreading" system-configuration-options)
                  (getenv "GITHUB_ACTIONS")))
  (let* ((cv (make-condition-variable (make-mutex) "CV"))
         condition
         (notify (lambda ()
                   (sleep-for 1) ;; let wait() start spinning first
                   (with-mutex (condition-mutex cv)
                     (setq condition t)
                     (condition-notify cv))))
         (wait (lambda ()
                 (with-mutex (condition-mutex cv)
                   (while (not condition)
                     (condition-wait cv)))))
         (herring (make-thread (apply-partially #'sleep-for 1000) "unrelated")))
    ;; HERRING is a nonmain thread that, if the bug is still present,
    ;; could assume the glib context lock when the main thread calls wait()
    (make-thread notify "notify")
    (funcall wait)
    (thread-signal herring 'quit nil))
  (cl-loop repeat 50
           until (zerop (1- (length (all-threads))))
           do (accept-process-output nil 0.2)
           finally (should (zerop (1- (length (all-threads)))))))

(ert-deftest thread-test-glib-lock ()
  "Would only fail under TEST_INTERACTIVE=yes, and not every time.
The failure manifests only by being unable to exit the interactive emacs."
  (skip-unless (featurep 'threads))
  (cl-macrolet ((run-thread
                 (name what)
                 `(make-thread
                   (lambda ()
                     (sleep-for (1+ (random 3)))
                     (funcall ,what))
                   ,name)))
    (let* ((n 3)
           (capacity 1)
           (channel (make-instance
                     'thread-test-channel
                     :condition (make-condition-variable (make-mutex) "channel")
                     :msg-queue (make-ring capacity))))
      (dotimes (i n)
        (let ((send-name (format "send-%d" (1+ i)))
	      (recv-name (format "recv-%d" (- n i))))
          (run-thread send-name
		      (lambda () (thread-test-channel-send channel 42)))
          (run-thread recv-name
		      (lambda () (thread-test-channel-recv channel)))))))
  (cl-loop repeat 50
           until (zerop (1- (length (all-threads))))
           do (accept-process-output nil 0.2)
           finally (should (zerop (1- (length (all-threads)))))))

(ert-deftest thread-test-promiscuous-process ()
  "Hold to Tromey's seemingly arbitrary 2012 edict outlawing
`accept-process-output' of a process started by another thread."
  (skip-unless (featurep 'threads))
  (thread-last-error t)
  (let* ((thread-tests-main (get-buffer-create "thread-tests-main" t))
         (buffers (list thread-tests-main))
         (start-proc (lambda (n b)
                       (apply #'start-process n b "cat" (split-string "/dev/urandom"))))
         (n 3))
    (funcall start-proc "thread-tests-main" (car buffers))
    (dotimes (i (1- n))
      (push (get-buffer-create (format "thread-tests-%d" i) t) buffers)
      (make-thread (apply-partially start-proc
                                    (format "thread-tests-%d" i)
                                    (car buffers))))
    (should (cl-loop repeat 10
                     when (cl-every #'processp (mapcar #'get-buffer-process buffers))
                     return t
                     do (accept-process-output nil 0.1)
                     finally return nil))
    (let ((procs (mapcar #'get-buffer-process buffers)))
      (mapc (lambda (proc) (set-process-thread proc nil)) procs)
      (dotimes (i (1- n))
        (make-thread
         (lambda ()
           (cl-loop repeat 5
                    do (accept-process-output
                        (nth (random (length procs)) procs)
                        0.2
                        nil
                        t)))
         (format "thread-tests-%d" i)))
      (should (cl-loop repeat 20
                       unless (cl-some
                               (lambda (thr)
                                 (cl-search "thread-tests-" (thread-name thr)))
                               (all-threads))
                       return t
                       do (accept-process-output
                           (nth (random (length procs)) procs) 1.0)
                       finally return nil)))
    (mapc (lambda (b) (kill-buffer b)) buffers))
  (should-not (thread-last-error)))

(ert-deftest thread-test-catch-throw ()
  "051da60 is supposed to make this work."
  (skip-unless (featurep 'threads))
  (let (timed-out)
    (make-thread (lambda () (with-timeout (1 (setq timed-out t))
                              (sleep-for 10))))
    (cl-loop repeat 5
             until timed-out
             do (accept-process-output nil 0.3)
             finally (should timed-out))))

(ert-deftest thread-test-backgrounded-buffers-still-killable ()
  "0e2d256 is supposed to make this work (Bug#65095)."
  (skip-unless (featurep 'threads))
  (let (buf)
    (with-temp-buffer
      (setq buf (current-buffer))
      (make-thread (lambda ())))
    (should-not (buffer-live-p buf))))

(ert-deftest thread-test-trivial-multithread ()
  (skip-unless (featurep 'threads))
  (skip-unless (cl-search "enable-multithreading" system-configuration-options))
  (let (results)
    (dotimes (k 4)
      (make-thread
       (lambda ()
         (cl-loop with n = 249597825381677
                  for divisor from 2 upto (truncate (sqrt n))
                  when (zerop (% n divisor))
                  return (push (format "t%d %d = %d(%d)"
                                       k n divisor (/ n divisor))
                               results)))
       nil :multi))
    (while (> (length (all-threads)) 1) (sleep-for 1))
    (mapc (lambda (y) (princ (format "%s\n" y) #'external-debugging-output))
          results)
    (should (= (length results) 4))))

(ert-deftest thread-test-local-var-alist ()
  "Thread obarrays bork buffer local variable lookup (460c423)."
  (skip-unless (featurep 'threads))
  (with-current-buffer (get-buffer-create "hisfooness")
    (let* (check
           (fn (lambda ()
                 (let (commands)
                   ;; cconv--analyze-function wouldn't see (lexical-binding . t)
                   ;; in BVAR (buffer, local_var_alist)
                   (mapc (lambda (zooy) (push (lambda () (push zooy check)) commands))
                         (list 2))
                   (dolist (fun commands)
                     (funcall fun))))))
      (make-thread fn)
      (while (> (length (all-threads)) 1) (sleep-for 1))
      (should (equal check '(2))))))

;;; thread-tests.el ends here
