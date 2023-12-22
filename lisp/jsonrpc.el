;;; jsonrpc.el --- JSON-RPC library                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2023 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: processes, languages, extensions
;; Version: 1.0.20
;; Package-Requires: ((emacs "25.2"))

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

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

;; This library implements the JSONRPC 2.0 specification as described
;; in https://www.jsonrpc.org/.  As the name suggests, JSONRPC is a
;; generic Remote Procedure Call protocol designed around JSON
;; objects.  To learn how to write JSONRPC programs with this library,
;; see Info node `(elisp)JSONRPC'."
;;
;; This library was originally extracted from eglot.el, an Emacs LSP
;; client, which you should see for an example usage.
;;
;;; Code:

(require 'cl-lib)
(require 'eieio)
(eval-when-compile (require 'subr-x))
(require 'warnings)
(require 'pcase)

;;; Public API
;;;

(defclass jsonrpc-connection ()
  ((name
    :accessor jsonrpc-name
    :initarg :name
    :documentation "A name for the connection")
   (-request-dispatcher
    :accessor jsonrpc--request-dispatcher
    :initform #'ignore
    :initarg :request-dispatcher
    :documentation "Dispatcher for remotely invoked requests.")
   (-notification-dispatcher
    :accessor jsonrpc--notification-dispatcher
    :initform #'ignore
    :initarg :notification-dispatcher
    :documentation "Dispatcher for remotely invoked notifications.")
   (last-error
    :initform nil
    :accessor jsonrpc-last-error
    :documentation "Last JSONRPC error message received from endpoint.")
   (-request-continuations
    :initform (make-hash-table)
    :accessor jsonrpc--request-continuations
    :documentation "A hash table of request ID to continuation lambdas.")
   (-events-buffer
    :initform nil
    :accessor jsonrpc--events-buffer
    :documentation "A buffer pretty-printing the JSONRPC events")
   (-events-buffer-scrollback-size
    :initarg :events-buffer-scrollback-size
    :accessor jsonrpc--events-buffer-scrollback-size
    :documentation "Max size of events buffer.  0 disables, nil means infinite.")
   (-deferred-actions
    :initform (make-hash-table :test #'equal)
    :accessor jsonrpc--deferred-actions
    :documentation "Map (DEFERRED BUF) to (FN TIMER ID).
FN is a deferred request from BUF, to be sent not later than TIMER as ID.")
   (-next-request-id
    :initform 0
    :accessor jsonrpc--next-request-id
    :documentation "Next number used for a request"))
  :documentation "Base class representing a JSONRPC connection.
The following keyword argument initargs are accepted:

:NAME (mandatory), a string naming the connection

:REQUEST-DISPATCHER (optional), a function of three
arguments (CONN METHOD PARAMS) for handling JSONRPC requests.
CONN is a `jsonrpc-connection' object, METHOD is a symbol, and
PARAMS is a plist representing a JSON object.  The function is
expected to return a JSONRPC result, a plist of (:result
RESULT) or signal an error of type `jsonrpc-error'.

:NOTIFICATION-DISPATCHER (optional), a function of three
arguments (CONN METHOD PARAMS) for handling JSONRPC
notifications.  CONN, METHOD and PARAMS are the same as in
:REQUEST-DISPATCHER.

:EVENTS-BUFFER-CONFIG is a plist.  Its `:size' stipulates the
size of the log buffer (0 disables, nil means infinite).  The
`:format' property is a symbol for choosing the log entry format.")

(cl-defmethod initialize-instance :after
  ((c jsonrpc-connection) ((&key (events-buffer-scrollback-size
                                  nil
                                  e-b-s-s-supplied-p)
                                 &allow-other-keys)
                           t))
  (when e-b-s-s-supplied-p
    (warn
     "`:events-buffer-scrollback-size' deprecated. Use `events-buffer-config'.")
    (with-slots ((plist -events-buffer-config)) c
      (setf plist (copy-sequence plist)
            plist (plist-put plist :size events-buffer-scrollback-size)))))

(cl-defmethod slot-missing ((_c jsonrpc-connection)
                            (_n (eql :events-buffer-scrollback-size))
                            (_op (eql oset))
                            _)
  ;; Yuck!  But this just coerces EIEIO to backward-compatibly accept
  ;; the :e-b-s-s initarg that is no longer associated with a slot
  ;; #pineForCLOS..
  )

;;; API mandatory
(cl-defgeneric jsonrpc-connection-send (conn &key id method params result error)
  "Send a JSONRPC message to connection CONN.
ID, METHOD, PARAMS, RESULT and ERROR.")

;;; API optional
(cl-defgeneric jsonrpc-shutdown (conn)
  "Shutdown the JSONRPC connection CONN.")

;;; API optional
(cl-defgeneric jsonrpc-running-p (conn)
  "Tell if the JSONRPC connection CONN is still running.")

;;; API optional
(cl-defgeneric jsonrpc-connection-ready-p (connection what)
  "Tell if CONNECTION is ready for WHAT in current buffer.
If it isn't, a request which was passed a value to the
:deferred keyword argument will be deferred to the future.
WHAT is whatever was passed the as the value to that argument.

By default, all connections are ready for sending all requests
immediately."
  (:method (_s _what)   ;; by default all connections are ready
           t))


;;; Convenience
;;;
(cl-defmacro jsonrpc-lambda (cl-lambda-list &body body)
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (cl-gensym "jsonrpc-lambda-elem")))
    `(lambda (,e) (apply (cl-function (lambda ,cl-lambda-list ,@body)) ,e))))

(defun jsonrpc-events-buffer (connection)
  "Get or create JSONRPC events buffer for CONNECTION."
  (let ((probe (jsonrpc--events-buffer connection)))
    (if (buffer-live-p probe)
        probe
      (with-current-buffer
          (get-buffer-create (format "*%s events*" (jsonrpc-name connection)))
        (special-mode)
        (setf (jsonrpc--events-buffer connection)
              (current-buffer))))))

(defun jsonrpc-forget-pending-continuations (connection)
  "Stop waiting for responses from the current JSONRPC CONNECTION."
  (clrhash (jsonrpc--request-continuations connection)))

(defvar jsonrpc-inhibit-debug-on-error nil
  "Inhibit `debug-on-error' when answering requests.
Some extensions, notably ert.el, set `debug-on-error' to non-nil,
which makes it hard to test the behavior of catching the Elisp
error and replying to the endpoint with an JSONRPC-error.  This
variable can be set around calls like `jsonrpc-request' to
circumvent that.")

(defun jsonrpc-connection-receive (connection message)
  "Process MESSAGE just received from CONNECTION.
This function will destructure MESSAGE and call the appropriate
dispatcher in CONNECTION."
  (cl-destructuring-bind (&key method id error params result &allow-other-keys
                          &aux continuations)
      message
    (jsonrpc--log-event connection message 'server)
    (setf (jsonrpc-last-error connection) error)
    (cond
     ;; A remote request
     ((and method id)
      (apply #'jsonrpc--reply connection
             id (condition-case err
                    `(:result ,(funcall (jsonrpc--request-dispatcher connection)
                                        connection (intern method) params))
                  (error
                   (let ((code (alist-get 'jsonrpc-error-code (cdr err))))
                     `(:error (:code
                               ,(or code -32603)
                               :message
                               ,(or code "Internal error"))))))))
     ;; A remote notification
     (method
      (funcall (jsonrpc--notification-dispatcher connection)
               connection (intern method) params))
     ;; A remote response
     ((setq continuations
            (and id (gethash id (jsonrpc--request-continuations connection))))
      (cancel-timer (nth 2 continuations))
      (remhash id (jsonrpc--request-continuations connection))
      (if error
          (funcall (nth 1 continuations) error)
        (funcall (nth 0 continuations) result))))
    (jsonrpc--call-deferred connection)))

;;; Contacting the remote endpoint
;;;
(defun jsonrpc-error (&rest args)
  "Error out with FORMAT and ARGS.
If invoked inside a dispatcher function, this function is suitable
for replying to the remote endpoint with an error message.

ARGS can be of the form (FORMAT-STRING . MOREARGS) for replying
with a -32603 error code and a message formed by formatting
FORMAT-STRING with MOREARGS.

Alternatively ARGS can be plist representing a JSONRPC error
object, using the keywords :code, :message and :data."
  (if (stringp (car args))
      (let ((msg
             (apply #'format-message (car args) (cdr args))))
        (signal 'jsonrpc-error
                `(,msg
                  (jsonrpc-error-code . -32603)
                  (jsonrpc-error-message . ,msg))))
    (cl-destructuring-bind (&key code message data) args
      (signal 'jsonrpc-error
              `("[jsonrpc] error "
                (jsonrpc-error-code . ,code)
                (jsonrpc-error-message . ,message)
                (jsonrpc-error-data . ,data))))))

(cl-defun jsonrpc-async-request (connection method params
                                 &rest args
                                 &key success-fn error-fn
                                 timeout-fn timeout deferred)
  "Issue request to CONNECTION, then immediately return.

The keyword arguments SUCCESS-FN, ERROR-FN and TIMEOUT-FN (the
success, error, and timeout callbacks, respectively) default to
logging events into `jsonrpc-events-buffer'.

If DEFERRED is non-nil, delay the request until
`jsonrpc-connection-ready-p' is true.  Should the function be
called again with identical buffer and DEFERRED value before the
request is issued, the request is dropped."
  (prog1 nil
    (ignore success-fn error-fn timeout-fn timeout deferred)
    (apply #'jsonrpc--async-request connection method params args)))

(cl-defun jsonrpc-request (connection method params
                           &key deferred timeout
                           cancel-on-input cancel-on-input-retval)
  "Same as `jsonrpc-async-request' but awaits reply.

If successful, returns a jsonrpc result object.  Otherwise
signals `jsonrpc-error'.

If CANCEL-ON-INPUT, returns CANCEL-ON-INPUT-RETVAL immediately
upon user input.  Any subsequent replies to the request are
dropped."
  (let ((deferred-key (list deferred (current-buffer)))
        done-p error-p retval)
    (cl-destructuring-bind (id timer)
        (apply #'jsonrpc--async-request
               connection method params
               :success-fn
               (lambda (result)
                 (setq done-p t
                       retval result))
               :error-fn
               (jsonrpc-lambda (&key code message data)
                 (setq error-p t
                       retval `((jsonrpc-error-code . ,code)
                                (jsonrpc-error-message . ,message)
                                (jsonrpc-error-data . ,data))))
               :timeout-fn
               (lambda ()
                 (setq error-p t
                       retval '((jsonrpc-error-message . "Timed out"))))
               `(,@(when deferred `(:deferred ,deferred))
                 ,@(when timeout `(:timeout ,timeout))))
      (unwind-protect
          (cl-loop when error-p
                   do (signal 'jsonrpc-error
                              (cons (format "request id=%s failed:" id)
                                    retval))
                   end
                   when done-p
                   return retval
                   if cancel-on-input
                     if (not (sit-for 0.1))
                     return cancel-on-input-retval
                     end
                   else
                     do (accept-process-output nil 0.1)
                   end)
        (remhash deferred-key (jsonrpc--deferred-actions connection))
        (when id (remhash id (jsonrpc--request-continuations connection)))
        (when timer (cancel-timer timer))))))

(cl-defun jsonrpc-notify (connection method params)
  "Notify CONNECTION of something, don't expect a reply."
  (jsonrpc-connection-send connection
                           :method method
                           :params params))

(define-obsolete-variable-alias 'jrpc-default-request-timeout
  'jsonrpc-default-request-timeout "28.1")

(defconst jsonrpc-default-request-timeout 10
  "Time in seconds before timing out a JSONRPC request.")

(defclass jsonrpc-process-connection (jsonrpc-connection)
  ((-process
    :initarg :process :accessor jsonrpc--process
    :documentation "Process object wrapped by the this connection.")
   (-expected-bytes
    :initform nil
    :accessor jsonrpc--expected-bytes
    :documentation "How many bytes declared by server.")
   (-on-shutdown
    :accessor jsonrpc--on-shutdown
    :initform #'ignore
    :initarg :on-shutdown
    :documentation "Function run when the process dies.")
   (-autoport-inferior
    :initform nil
    :documentation "Used by `jsonrpc-autoport-bootstrap'."))
  :documentation "A JSONRPC connection over an Emacs process.
The following initargs are accepted:

:PROCESS (mandatory), a live running Emacs process object or a
function producing one such object.  If a function, it is passed
the `jsonrpc-process-connection' object.  The process represents
either a pipe connection to locally running process or a stream
connection to a network host.  The remote endpoint is expected to
understand JSONRPC messages with basic HTTP-style enveloping
headers such as \"Content-Length:\".

:ON-SHUTDOWN (optional), a function of one argument, the
connection object, called when the process dies.

Needs to be rewritten since `initialize-instance' proceeds
to erase whatever the process wrote to its process buffer.")

(cl-defmethod initialize-instance ((conn jsonrpc-process-connection) _slots)
  "Compatibility for rogue eglot guy's obfuscated buffer naming.
CONN's extant process buffer, if any, is potentially orphaned and
replaced with an invisible buffer \" *[CONN name] output*\".  If
:stderr of CONN's process is named '*[CONN name] stderr*', it is
modified to replicate messages to CONN's event buffer, and made
invisible by renaming to \" *[CONN name] stderr*\".

Needs to be rewritten."
  (cl-call-next-method)
  (when (functionp (jsonrpc--process conn))
    (setf (jsonrpc--process conn) (funcall (jsonrpc--process conn))))
  (process-put (jsonrpc--process conn) 'jsonrpc-connection conn)
  (set-process-filter (jsonrpc--process conn) #'jsonrpc--process-filter)
  (set-process-sentinel (jsonrpc--process conn) #'jsonrpc--process-sentinel)
  (let ((stdout-buffer (get-buffer-create
                        (format " *%s output*" (jsonrpc-name conn)))))
    (set-process-buffer (jsonrpc--process conn) stdout-buffer)
    (with-current-buffer stdout-buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (set-marker (process-mark (jsonrpc--process conn)) (point-min))))
  (when-let ((stderr-buffer
              (get-buffer (format "*%s stderr*" (jsonrpc-name conn))))
             (invisible-name (concat " " (buffer-name stderr-buffer))))
    (with-current-buffer stderr-buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (process-put (jsonrpc--process conn) 'jsonrpc-stderr stderr-buffer)
    (when-let ((detritus (get-buffer invisible-name)))
      (let (kill-buffer-query-functions)
        (kill-buffer detritus)))
    (with-current-buffer stderr-buffer
      (rename-buffer invisible-name)
      (add-hook
       'after-change-functions
       (lambda (beg _end _pre-change-len)
         "Mimeograph stderr to events."
         (cl-loop initially (goto-char beg)
                  do (forward-line)
                  when (bolp)
                  for line = (buffer-substring
                              (line-beginning-position 0)
                              (line-end-position 0))
                  do (with-current-buffer (jsonrpc-events-buffer conn)
                       (goto-char (point-max))
                       (let ((inhibit-read-only t))
                         (insert (format "[stderr] %s\n" line))))
                  until (eobp)))
       nil t))))

(defun jsonrpc--stringify-method (args)
  "Normalize :method in ARGS."
  (cl-loop for i below (1- (length args)) by 2
           for keyword = (nth i args)
           while (keywordp keyword)
           do (when (eq :method keyword)
                (let* ((method* (nth (1+ i) args))
                       (method (cond ((keywordp method*)
                                      (substring (symbol-name method*) 1))
                                     ((and method* (symbolp method*))
                                      (symbol-name method*))
                                     ((stringp method*)
                                      method*))))
                  (if method
                      (setcar (nthcdr (1+ i) args) method)
                    (setf (nthcdr i args) (nthcdr (+ 2 i) args)))))
           finally return args))

(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-process-connection)
                                       &rest args)
  (setq args (jsonrpc--stringify-method args))
  (let* ((message `(:jsonrpc "2.0" ,@args))
         (json (jsonrpc--json-encode message))
         (headers `(("Content-Length" . ,(format "%d" (string-bytes json))))))
    (jsonrpc--log-event connection message 'client)
    (process-send-string
     (jsonrpc--process connection)
     (cl-loop for (header . value) in headers
              concat (concat header ": " value "\r\n") into header-section
              finally return (format "%s\r\n%s" header-section json)))))

(defun jsonrpc-process-type (conn)
  "Return the `process-type' of JSONRPC connection CONN."
  (process-type (jsonrpc--process conn)))

(cl-defmethod jsonrpc-running-p ((conn jsonrpc-process-connection))
  "Return non-nil if JSONRPC connection CONN is running."
  (process-live-p (jsonrpc--process conn)))

(cl-defmethod jsonrpc-shutdown ((conn jsonrpc-process-connection)
                                &optional cleanup)
  "Wait for JSONRPC connection CONN to shutdown.
With optional CLEANUP, kill any associated buffers."
  (unwind-protect
      (cl-loop
       with proc = (jsonrpc--process conn) for i from 0
       while (not (process-get proc 'jsonrpc-sentinel-cleanup-started))
       unless (zerop i) do
       (jsonrpc--warn "Sentinel for %s still hasn't run, deleting it!" proc)
       do
       (delete-process proc)
       (accept-process-output nil 0.1))
    (when cleanup
      (kill-buffer (process-buffer (jsonrpc--process conn)))
      (kill-buffer (jsonrpc-stderr-buffer conn)))))

(defun jsonrpc-stderr-buffer (conn)
  "Get CONN's standard error buffer, if any."
  (process-get (jsonrpc--process conn) 'jsonrpc-stderr))

(define-error 'jsonrpc-error "jsonrpc-error")

(defalias 'jsonrpc--json-read
  (if (fboundp 'json-parse-buffer)
      (lambda ()
        (json-parse-buffer :object-type 'plist
                           :null-object nil
                           :false-object :json-false))
    (require 'json)
    (defvar json-object-type)
    (declare-function json-read "json" ())
    (lambda ()
      (let ((json-object-type 'plist))
        (json-read))))
  "Read JSON object in buffer, move point to end of buffer.")

(defalias 'jsonrpc--json-encode
  (if (fboundp 'json-serialize)
      (lambda (object)
        (json-serialize object
                        :false-object :json-false
                        :null-object nil))
    (require 'json)
    (defvar json-false)
    (defvar json-null)
    (declare-function json-encode "json" (object))
    (lambda (object)
      (let ((json-false :json-false)
            (json-null nil))
        (json-encode object))))
  "Encode OBJECT into a JSON string.")

(cl-defun jsonrpc--reply
    (connection id &key (result nil result-supplied-p) (error nil error-supplied-p))
  "Reply to CONNECTION's request ID with RESULT or ERROR."
  (apply #'jsonrpc-connection-send connection
         `( :id ,id
            ,@(and result-supplied-p `(:result ,result))
            ,@(and error-supplied-p `(:error ,error)))))

(defun jsonrpc--call-deferred (connection)
  "Call CONNECTION's deferred actions, who may again defer themselves."
  (when-let ((actions (hash-table-values (jsonrpc--deferred-actions connection))))
    (jsonrpc--run-event-hook
     connection 'internal
     :log-text (format "re-attempting deffered requests %s"
                       (mapcar (apply-partially #'nth 2) actions)))
    (mapc #'funcall (mapcar #'car actions))))

(defun jsonrpc--process-sentinel (proc change)
  "Called when PROC undergoes CHANGE."
  (let ((connection (process-get proc 'jsonrpc-connection)))
    (jsonrpc--debug connection `(:message "Connection state changed" :change ,change))
    (unless (process-live-p proc)
      (with-current-buffer (jsonrpc-events-buffer connection)
        (let ((inhibit-read-only t))
          (insert "\n----------b---y---e---b---y---e----------\n")))
      ;; Cancel outstanding timers
      (maphash (lambda (_id triplet)
                 (pcase-let ((`(,_success ,_error ,timeout) triplet))
                   (when timeout (cancel-timer timeout))))
               (jsonrpc--request-continuations connection))
      (process-put proc 'jsonrpc-sentinel-cleanup-started t)
      (unwind-protect
          ;; Call all outstanding error handlers
          (maphash (lambda (_id triplet)
                     (pcase-let ((`(,_success ,error ,_timeout) triplet))
                       (funcall error '(:code -1 :message "Server died"))))
                   (jsonrpc--request-continuations connection))
        (jsonrpc--message "Server exited with status %s" (process-exit-status proc))
        (delete-process proc)
        (when-let (p (slot-value connection '-autoport-inferior)) (delete-process p))
        (funcall (jsonrpc--on-shutdown connection) connection)))))

(defun jsonrpc--process-filter (proc string)
  "Log STRING and possibly other strings piped in from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (connection (process-get proc 'jsonrpc-connection)))
        ;; Insert the text, advancing the process marker.
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (catch 'done
          (while t
            ;; More than one message might have arrived
            (unless (setf (jsonrpc--expected-bytes connection)
                          (or (jsonrpc--expected-bytes connection)
                              (and (search-forward-regexp
                                    "\\(?:.*: .*\r\n\\)*Content-Length: \
*\\([[:digit:]]+\\)\r\n\\(?:.*: .*\r\n\\)*\r\n"
                                    (+ (point) 100) t)
                                   (string-to-number (match-string 1)))))
              (throw 'done t))

            ;; Attempt to complete a message body
            (let ((available-bytes (- (position-bytes (process-mark proc))
                                      (position-bytes (point))))
                  (message-end (byte-to-position
                                (+ (position-bytes (point))
                                   (jsonrpc--expected-bytes connection)))))
              (if (< available-bytes (jsonrpc--expected-bytes connection))
                  (throw 'done t)       ; message still incomplete
                (unwind-protect
                    (save-restriction
                      (narrow-to-region (point) message-end)
                      (when-let ((json-message
                                  (condition-case err
                                      (jsonrpc--json-read)
                                    (error
                                     (prog1 nil
                                       (jsonrpc--warn "Invalid JSON: %s %s"
                                                      (cdr err) (buffer-string)))))))
                        (with-temp-buffer
                          ;; Calls success-fn and error-fn
                          ;; of jsonrpc-async-request, which
                          ;; can arbitrarily pollute or even kill
                          ;; (process-buffer PROC).  Ergo, buffer-live-p
                          ;; check in unwind clause.
                          (jsonrpc-connection-receive connection json-message))))
                  (when (buffer-live-p (process-buffer proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char message-end)
                      (delete-region (point-min) (point))
                      (setf (jsonrpc--expected-bytes connection) nil))))))))))))

(cl-defun jsonrpc--async-request (connection method params
                                    &rest args
                                    &key success-fn error-fn timeout-fn deferred
                                    (timeout jsonrpc-default-request-timeout)
                                    &aux (orig-pt (point))
                                    (orig-buffer (current-buffer))
                                    (deferred-key (when deferred
                                                    `(,deferred ,orig-buffer))))
  "Returns a list (ID TIMER)."
  (let (id timer)
    (when deferred
      (let ((extant (gethash deferred-key (jsonrpc--deferred-actions connection))))
        (remhash deferred-key (jsonrpc--deferred-actions connection))
        ;; (FN TIMER ID)
        (setq timer (cl-second extant)
              id (or (cl-third extant)
                     (cl-incf (jsonrpc--next-request-id connection))))
        (if (jsonrpc-connection-ready-p connection deferred)
            (when timer (cancel-timer timer))
          ;; Conclude deferment.
          (jsonrpc--debug connection `( :deferring ,method
                                        :id ,id
                                        :params ,params))
          (puthash
           deferred-key
           (list (lambda ()
                   (when (buffer-live-p orig-buffer)
                     (with-current-buffer orig-buffer
                       (save-excursion (goto-char orig-pt)
                                       (apply #'jsonrpc-async-request
                                              connection
                                              method params args)))))
                 (setq timer
                       (or timer
                           (when timeout
                             (run-with-timer
                              timeout nil
                              (lambda ()
                                (remhash deferred-key (jsonrpc--deferred-actions connection))
                                (funcall (or timeout-fn
                                             (apply-partially #'jsonrpc--debug
                                                              connection
                                                              `( :timed-out ,method
                                                                 :id ,id
                                                                 :params ,params)))))))))
                 id)
           (jsonrpc--deferred-actions connection)))))
    (unless (gethash deferred-key (jsonrpc--deferred-actions connection))
      ;; Did not earlier conclude to defer
      (setq id (or id (cl-incf (jsonrpc--next-request-id connection))))
      (jsonrpc-connection-send connection
                               :id id
                               :method method
                               :params params)
      (puthash id
               (list (or success-fn
                         (jsonrpc-lambda (&rest _ignored)
                           (jsonrpc--debug
                            connection (list :message "success ignored"
                                             :id id))))
                     (or error-fn
                         (jsonrpc-lambda (&key code message &allow-other-keys)
                           (jsonrpc--debug
                            connection (list
                                        :message
                                        (format "error ignored, status set (%s)"
                                                message)
                                        :id id :error code))))
                     (setq timer
                           (when timeout
                             (run-with-timer
                              timeout nil
                              (lambda ()
                                (remhash id (jsonrpc--request-continuations connection))
                                (funcall (or timeout-fn
                                             (apply-partially #'jsonrpc--debug
                                                              connection
                                                              `( :timed-out ,method
                                                                 :id ,id
                                                                 :params ,params)))))))))
               (jsonrpc--request-continuations connection)))
    (list id timer)))

(defun jsonrpc--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[jsonrpc] %s" (apply #'format format args)))

(defun jsonrpc--debug (server format &rest args)
  "Debug message for SERVER with FORMAT and ARGS."
  (with-current-buffer (jsonrpc-events-buffer server)
    (jsonrpc--log-event
     server 'internal
     :log-text (apply #'format format args)
     :type 'debug)))

(defun jsonrpc--warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'jsonrpc--message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'jsonrpc
                     (apply #'format format args)
                     :warning)))

(defun jsonrpc--log-event (connection message &optional type)
  "Log a JSONRPC-related event.
CONNECTION is the current connection.  MESSAGE is a JSON-like
plist.  TYPE is a symbol saying if this is a client or server
originated."
  (let ((max (jsonrpc--events-buffer-scrollback-size connection)))
    (when (or (null max) (cl-plusp max))
      (with-current-buffer (jsonrpc-events-buffer connection)
        (cl-destructuring-bind (&key method id error &allow-other-keys) message
          (let* ((inhibit-read-only t)
                 (subtype (cond ((and method id)       'request)
                                (method                'notification)
                                (id                    'reply)
                                (t                     'message)))
                 (type
                  (concat (format "%s" (or type 'internal))
                          (if type
                              (format "-%s" subtype)))))
            (goto-char (point-max))
            (prog1
                (let ((msg (format "[%s]%s%s %s:\n%s"
                                   type
                                   (if id (format " (id:%s)" id) "")
                                   (if error " ERROR" "")
                                   (current-time-string)
                                   (pp-to-string message))))
                  (when error
                    (setq msg (propertize msg 'face 'error)))
                  (insert-before-markers msg))
              ;; Trim the buffer if it's too large
              (when max
                (save-excursion
                  (goto-char (point-min))
                  (while (> (buffer-size) max)
                    (delete-region (point) (progn (forward-line 1)
                                                  (forward-sexp 1)
                                                  (forward-line 2)
                                                  (point)))))))))))))

(defun jsonrpc--forwarding-buffer (name prefix conn)
  "Helper for `jsonrpc-process-connection' helpers.
Make a stderr buffer named NAME, forwarding lines prefixed by
PREFIX to CONN's events buffer."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t))
      (fundamental-mode)
      (erase-buffer)
      (buffer-disable-undo)
      (add-hook
       'after-change-functions
       (lambda (beg _end _pre-change-len)
         (cl-loop initially (goto-char beg)
                  do (forward-line)
                  when (bolp)
                  for line = (buffer-substring
                              (line-beginning-position 0)
                              (line-end-position 0))
                  do (with-current-buffer (jsonrpc-events-buffer conn)
                       (goto-char (point-max))
                       (let ((inhibit-read-only t))
                         (insert
                          (propertize (format "%s %s\n" prefix line)
                                      'face 'shadow))))
                  until (eobp)))
       nil t))
    (current-buffer)))


;;;; More convenience utils
(cl-defun jsonrpc-autoport-bootstrap (name contact
                                           &key connect-args)
  "Use CONTACT to start network server, then connect to it.

Return function suitable for the :PROCESS initarg of
`jsonrpc-process-connection' (which see).

CONTACT is a list where all the elements are strings except for
one, which is usuallky the keyword `:autoport'.

When the returned function is called it will start a program
using a command based on CONTACT, where `:autoport' is
substituted by a locally free network port.  Thereafter, a
network is made to this port.

Instead of the keyword `:autoport', a cons cell (:autoport
FORMAT-FN) is also accepted.  In that case FORMAT-FN is passed
the port number and should return a string used for the
substitution.

The internal processes and control buffers are named after NAME.

CONNECT-ARGS are passed as additional arguments to
`open-network-stream'."
  (lambda (conn)
    (let* ((port-probe (make-network-process :name "jsonrpc-port-probe-dummy"
                                             :server t
                                             :host "localhost"
                                             :service 0))
           (port-number (unwind-protect
                            (process-contact port-probe :service)
                          (delete-process port-probe)))
           (inferior-buffer (jsonrpc--forwarding-buffer
                             (format " *%s inferior output*" name)
                             "[inferior]"
                             conn))
           (cmd (cl-loop for e in contact
                         if (eq e :autoport) collect (format "%s" port-number)
                         else if (eq (car-safe e) :autoport)
                         collect (funcall (cdr e) port-number)
                         else collect e))
           inferior np)
      (unwind-protect
          (progn
            (message "[jsonrpc] Attempting to start `%s'"
                     (string-join cmd " "))
            (setq inferior
                  (make-process
                   :name (format "inferior (%s)" name)
                   :buffer inferior-buffer
                   :noquery t
                   :command cmd))
            (setq np
                  (cl-loop
                   repeat 10 for i from 0
                   do (accept-process-output nil 0.5)
                   while (process-live-p inferior)
                   do (message
                       "[jsonrpc] %sTrying to connect to localhost:%s (attempt %s)"
                       (if (zerop i) "Started.  " "")
                       port-number (1+ i))
                   thereis (ignore-errors
                             (apply #'open-network-stream
                                    (format "autostart (%s)" name)
                                    nil
                                    "localhost" port-number connect-args))))
            (setf (slot-value conn '-autoport-inferior) inferior)
            np)
        (cond ((and (process-live-p np)
                    (process-live-p inferior))
               (message "[jsonrpc] Done, connected to %s!" port-number))
              (t
               (when inferior (delete-process inferior))
               (when np (delete-process np))
               (error "[jsonrpc] Could not start and/or connect")))))))


(provide 'jsonrpc)
;;; jsonrpc.el ends here
