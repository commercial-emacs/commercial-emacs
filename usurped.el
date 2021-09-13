;;; -*- lexical-binding: t -*-

;; confused-main still fails: printing unbind and rebind for thread switch
;; yielded nothing.  printing reflect_current yielded nothing.
;; it's not saved value.
;; it's not do_one_unbind, printed there too.
;; there must be a deep context switch such that non-main thread's buffer-local
;; prevails.  specbind and the unbind/rebind_for_thread_switch just
;; affect bind->let state.  In other words, it's blv_value for a deep Lisp_Symbol
;; (which I'm not about to swap out according to context).

(require 'cl-lib)
(defvar doomsday "global")
(cl-assert (equal (buffer-name) "*scratch*"))
(when (or (bound-and-true-p confuse-main)
          (bound-and-true-p confuse-thread))
  (make-variable-buffer-local 'doomsday))
(cl-assert (equal (default-value 'doomsday) "global"))
(let ((doomsday "let"))
  (cl-assert (equal doomsday "let")))
(cl-assert (special-variable-p 'doomsday))
(dotimes (i 5)
  (setq doomsday (format "local-%d" i))
  (cl-assert (equal doomsday (format "local-%d" i)))
  (make-thread (lambda ()
                 (let ((body
                        (lambda ()
                          (let ((doomsday "let"))
                            (sleep-for (1+ (random 5)))
                            (princ (format "%s in thread#%s, var is %s\n"
                                           (if (equal doomsday "let")
                                               "succeeded"
                                             "failed")
				           (thread-name (current-thread))
				           doomsday)
			           #'external-debugging-output))))
                       (body2
                        (lambda ()
                          (sleep-for (1+ (random 5))))))
                   (if (bound-and-true-p confuse-main)
                       (with-current-buffer "*scratch*"
                         (funcall body))
                     (let ((b (concat "*buffer-" (thread-name (current-thread)) "*")))
                       (unwind-protect
                           (with-current-buffer (get-buffer-create b)
                             (funcall body))
                         (let (kill-buffer-query-functions)
                           (kill-buffer b)))))))
	       (format "%d" i))
  (when (bound-and-true-p confuse-main) (sleep-for 1))
  (unless (equal doomsday (format "local-%d" i))
    (princ (format "failed in thread#main, var is %s, should be %s\n"
                   doomsday
                   (format "local-%d" i))
           #'external-debugging-output)))

(set-buffer (get-buffer-create "switch"))
(run-at-time t 0.5 #'ignore)
(while (not (zerop (1- (length (all-threads)))))
  (accept-process-output nil 0.1))

(when (bound-and-true-p confuse-main)
  (message "heyho %S" (buffer-local-value 'doomsday (get-buffer "*scratch*"))))


(when (fboundp 'thread-last-error)
  (let ((my-bad (thread-last-error)))
    (when my-bad
      (princ (format "my bad: %s\n" my-bad)
             #'external-debugging-output))))
