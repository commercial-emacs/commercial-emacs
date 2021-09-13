;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(defvar doomsday "global")
(cl-assert (equal (buffer-name) "*scratch*"))
(cl-assert (special-variable-p 'doomsday))
(when (or (bound-and-true-p confuse-main)
          (bound-and-true-p confuse-thread))
  (setq-local doomsday "local")
  (cl-assert (equal doomsday "local")))
(let ((doomsday "let"))
  (cl-assert (equal doomsday "let")))
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
			           #'external-debugging-output)))))
                   (if (bound-and-true-p confuse-main)
                       (with-current-buffer "*scratch*"
                         (funcall body))
                     (with-temp-buffer
                       (funcall body)))))
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
(when (fboundp 'thread-last-error)
  (let ((my-bad (thread-last-error)))
    (when my-bad
      (princ (format "my bad: %s\n" my-bad)
             #'external-debugging-output))))
