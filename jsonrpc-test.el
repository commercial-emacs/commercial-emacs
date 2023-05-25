;;; -*- lexical-binding:t -*-

(require 'cl-lib)
(require 'xlsp)

(defconst json-1 "{\"id\":12,\"jsonrpc\":\"2.0\",\"result\":[{\"newText\":\"\\n  \",\"range\":{\"end\":{\"character\":0,\"line\":12},\"start\":{\"character\":25,\"line\":11}}},{\"newText\":\"\",\"range\":{\"end\":{\"character\":7,\"line\":12},\"start\":{\"character\":6,\"line\":12}}},{\"newText\":\"\\n      \",\"range\":{\"end\":{\"character\":0,\"line\":13},\"start\":{\"character\":16,\"line\":12}}},{\"newText\":\"\",\"range\":{\"end\":{\"character\":2,\"line\":13},\"start\":{\"character\":0,\"line\":13}}}]}")
(defconst json-2 "{\"id\":13,\"jsonrpc\":\"2.0\",\"result\":[{\"range\":{\"end\":{\"character\":0,\"line\":0},\"start\":{\"character\":0,\"line\":0}},\"uri\":\"file:///tmp/test-xlsppQqQKF/foo.h\"}]}")
(defconst json-3 "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"diagnostics\":[{\"code\":\"-Wmain-return-type\",\"message\":\"Return type of 'main' is not 'int' (fix available)\",\"range\":{\"end\":{\"character\":4,\"line\":10},\"start\":{\"character\":0,\"line\":10}},\"severity\":2,\"source\":\"clang\"},{\"code\":\"expected_semi_after_expr\",\"message\":\"Expected ';' after expression (fix available)\",\"range\":{\"end\":{\"character\":8,\"line\":14},\"start\":{\"character\":2,\"line\":14}},\"severity\":1,\"source\":\"clang\"},{\"code\":\"-Wreturn-type\",\"message\":\"Void function 'main' should not return a value\",\"range\":{\"end\":{\"character\":8,\"line\":14},\"start\":{\"character\":2,\"line\":14}},\"severity\":1,\"source\":\"clang\"}],\"uri\":\"file:///tmp/test-xlsppQqQKF/foo.c\",\"version\":48}}")

(defmacro common (&rest body)
  (declare (indent defun))
  `(let* ((start-time (float-time))
          (file (make-temp-name "foo"))
          (_file (with-temp-file file
                   (cl-flet ((insert-json (which)
                               (let ((len (length which)))
                                 (insert "Content-Length: " (number-to-string len) "\r\n")
                                 (insert "\r\n")
                                 (insert which)
                                 (insert "\r\n"))))
                     (dotimes (i 7000)
                       (insert-json json-1) (insert-json json-2) (insert-json json-3)))))
          (buffer (get-buffer-create "*output*"))
          (timer (run-at-time
                  nil 0.05 (lambda ()
                             (setq unread-command-events
                                   (append unread-command-events
                                           (listify-key-sequence "g")))))))
     ,@body))

(defun test-yyonch (control)
  (if control
      (test-bulk control)
    (require 'lsp-mode)
    (require 'lsp-clangd)
    (common
      (make-thread
       (lambda ()
         (json-rpc (apply #'json-rpc-connection (list "cat" file))
                   (lambda (result err done)
                     (if done
                         (progn
                           (setq unread-command-events nil)
                           (cancel-timer timer)
                           (delete-file file)
                           (message "took %s seconds" (- (float-time) start-time)))
                       (with-current-buffer buffer
                         (prin1 result (current-buffer))
                         (insert "\n"))))
                   :object-type 'plist
                   :null-object nil
                   :false-object nil))))))

(defun test-bulk (control)
  (common
    (let* ((filter (if control
                       (let ((before* ""))
                         (lambda (proc string)
                           (setq string (concat before* string))
                           (catch 'done
                             (while t
                               (if-let ((from (string-match "\r\n\\({.*}\\)\r\n" string))
                                        (honey (match-string 1 string))
                                        (to (+ from (length (match-string 0 string)))))
                                   (progn
                                     (with-current-buffer (process-buffer proc)
                                       (condition-case err
                                           (prin1 (json-parse-string honey :object-type 'plist)
                                                  (current-buffer))
                                         (error (message "fooey %s" honey)))
                                       (insert "\n"))
                                     (setq string (cl-subseq string to)))
                                 (setq before* string)
                                 (throw 'done nil))))))
                     (lambda (proc obj)
                       (with-current-buffer (process-buffer proc)
                         (prin1 obj (current-buffer))
                         (insert "\n")))))
           (process (make-process :name "foo"
			          :command (list "cat" file)
			          :connection-type 'pipe
			          :noquery t
			          :buffer buffer
                                  :sentinel
                                  (lambda (process status)
                                    (when (memq (process-status process) '(exit signal))
                                      (setq unread-command-events nil)
                                      (cancel-timer timer)
                                      (delete-file file)
                                      (message "took %s seconds"
                                               (- (float-time) start-time))))
                                  :filter filter)))
      (unless control
        (make-jsonrpc-thread "foo" process)))))

(defun test-dribs (control)
  (when control
    (fmakunbound 'make-jsonrpc-thread))
  (with-current-buffer (find-file "src/json.c")
    (cl-letf (((symbol-function 'xlsp-heuristic-reuse-matches-p) #'ignore))
      (let* ((time*)
             (times*)
             (start (lambda (&rest _args) (setq time* (float-time))))
             (stop (lambda (&rest _args)
                     (when time*
                       (push (- (float-time) time*) times*)
                       (setq time* nil))))
             (completion-setup-hook))
        (unwind-protect
            (progn
              (add-function :after (symbol-function 'jsonrpc-connection-send) start)
              (add-function :before (symbol-function 'xlsp-completion-callback) stop)
              (dotimes (i 30)
                (goto-char 37036)
                (save-excursion
                  (insert (make-string i ? ))
                  (insert "emacs_")
                  (call-interactively #'completion-at-point))
                (kill-line)))
          (print-out times*)
          (remove-function (symbol-function 'jsonrpc-connection-send) start)
          (remove-function (symbol-function 'xlsp-completion-callback) stop))))))
