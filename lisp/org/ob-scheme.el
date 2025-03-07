;;; ob-scheme.el --- Babel Functions for Scheme      -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2024 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	    Michael Gauland
;; Keywords: literate programming, reproducible research, scheme
;; URL: https://orgmode.org

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

;; Now working with SBCL for both session and external evaluation.
;;
;; This certainly isn't optimally robust, but it seems to be working
;; for the basic use cases.

;;; Requirements:

;; - a working scheme implementation
;;   (e.g. guile https://www.gnu.org/software/guile/guile.html)
;;
;; - for session based evaluation geiser is required, which is available from
;;   ELPA.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'geiser nil t)
(require 'geiser-impl nil t)
(defvar geiser-repl--repl)             ; Defined in geiser-repl.el
(defvar geiser-impl--implementation)   ; Defined in geiser-impl.el
(defvar geiser-scheme-implementation)  ; Defined in geiser-impl.el
(defvar geiser-default-implementation) ; Defined in geiser-impl.el
(defvar geiser-active-implementations) ; Defined in geiser-impl.el
(defvar geiser-debug-show-debug-p)     ; Defined in geiser-debug.el
(defvar geiser-debug-jump-to-debug-p)  ; Defined in geiser-debug.el
(defvar geiser-repl-use-other-window)  ; Defined in geiser-repl.el
(defvar geiser-repl-window-allow-split)	; Defined in geiser-repl.el
(declare-function geiser-connect "ext:geiser-repl" (impl &optional host port))
(declare-function run-geiser "ext:geiser-repl" (impl))
(declare-function geiser "ext:geiser-repl" (impl))
(declare-function geiser-mode "ext:geiser-mode" ())
(declare-function geiser-eval-region "ext:geiser-mode"
                  (start end &optional and-go raw nomsg))
(declare-function geiser-eval-region/wait "ext:geiser-mode"
                  (start end &optional timeout))
(declare-function geiser-repl-exit "ext:geiser-repl" (&optional arg))
(declare-function geiser-eval--retort-output "ext:geiser-eval" (ret))
(declare-function geiser-eval--retort-result-str "ext:geiser-eval" (ret prefix))
(declare-function geiser-eval--retort-error "ext:geiser-eval" (ret))
(declare-function geiser-eval--retort-error-msg "ext:geiser-eval" (err))
(declare-function geiser-eval--error-msg "ext:geiser-eval" (err))

(defcustom org-babel-scheme-null-to 'hline
  "Replace `null' and empty lists in scheme tables with this before returning."
  :group 'org-babel
  :version "26.1"
  :package-version '(Org . "9.1")
  :type 'symbol)

(defvar org-babel-default-header-args:scheme '()
  "Default header arguments for scheme code blocks.")
(defconst org-babel-header-args:scheme '((host . :any)
                                         (port . :any))
  "Header arguments supported in Scheme.")

(defun org-babel-scheme-expand-header-arg-vars (vars)
  "Expand :var header arguments given as VARS."
  (mapconcat
   (lambda (var)
     (format "(define %S %S)" (car var) (cdr var)))
   vars
   "\n"))

(defun org-babel-expand-body:scheme (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
	(prepends (cdr (assq :prologue params)))
	(postpends (cdr (assq :epilogue params))))
    (concat (and prepends (concat prepends "\n"))
	    (if (null vars) body
	      (concat (org-babel-scheme-expand-header-arg-vars vars) "\n" body))
	    (and postpends (concat "\n" postpends)))))


(defvar org-babel-scheme-repl-map (make-hash-table :test #'equal)
  "Map of scheme sessions to session names.")

(defun org-babel-scheme-cleanse-repl-map ()
  "Remove dead buffers from the REPL map."
  (maphash
   (lambda (x y) (unless (buffer-name y) (remhash x org-babel-scheme-repl-map)))
   org-babel-scheme-repl-map))

(defun org-babel-scheme-get-session-buffer (session-name)
  "Look up the scheme buffer for a session; return nil if it doesn't exist."
  (org-babel-scheme-cleanse-repl-map) ; Prune dead sessions
  (gethash session-name org-babel-scheme-repl-map))

(defun org-babel-scheme-set-session-buffer (session-name buffer)
  "Record the scheme buffer used for a given session."
  (puthash session-name buffer org-babel-scheme-repl-map))

(defun org-babel-scheme-get-buffer-impl (buffer)
  "Return the scheme implementation geiser associates with the buffer."
  (with-current-buffer (set-buffer buffer)
    geiser-impl--implementation))

(defun org-babel-scheme-get-repl (impl name &optional host port)
  "Switch to a Scheme REPL, creating it if it doesn't exist.

If the variables HOST and PORT are set, connect to the running Scheme REPL."
  (let ((buffer (org-babel-scheme-get-session-buffer name)))
    (or buffer
	(progn
          (if (fboundp 'geiser)
              (if (and host port)
                  (geiser-connect impl host port)
                (geiser impl))
            ;; Obsolete since Geiser 0.26.
	    (run-geiser impl))
	  (when name
	    (rename-buffer name t)
	    (org-babel-scheme-set-session-buffer name (current-buffer)))
	  (current-buffer)))))

(defun org-babel-scheme-make-session-name (buffer name impl)
  "Generate a NAME for the session BUFFER.

For a named session, the buffer name will be the session name.

If the session is unnamed (nil), generate a name.

If the session is `none', use nil for the session name, and
`org-babel-scheme-execute-with-geiser' will use a temporary session."
  (cond ((not name) (concat buffer " " (symbol-name impl) " REPL"))
	((string= name "none") nil)
	(name)))

(defmacro org-babel-scheme-capture-current-message (&rest body)
  "Capture current message in both interactive and noninteractive mode."
  `(if noninteractive
       (let ((original-message (symbol-function 'message))
             (current-message nil))
         (unwind-protect
             (progn
               (defun message (&rest args)
                 (setq current-message (apply original-message args)))
               ,@body
               current-message)
           (fset 'message original-message)))
     (progn
       ,@body
       (current-message))))

(defun org-babel-scheme-execute-with-geiser (code output impl repl &optional host port)
  "Execute code in specified REPL.
If the REPL doesn't exist, create it using the given scheme
implementation.

Returns the output of executing the code if the OUTPUT parameter
is true; otherwise returns the last value."
  (let ((result nil))
    (with-temp-buffer
      (insert (format ";; -*- geiser-scheme-implementation: %s -*-" impl))
      (newline)
      (let ((beg (point)))
        (insert code)
        (geiser-mode)
        (let ((geiser-repl-window-allow-split nil)
	      (geiser-repl-use-other-window nil))
	  (let ((repl-buffer (save-current-buffer
			       (org-babel-scheme-get-repl impl repl host port))))
	    (when (not (eq impl (org-babel-scheme-get-buffer-impl
			       (current-buffer))))
	      (message "Implementation mismatch: %s (%s) %s (%s)" impl (symbolp impl)
		       (org-babel-scheme-get-buffer-impl (current-buffer))
		       (symbolp (org-babel-scheme-get-buffer-impl
			         (current-buffer)))))
	    (setq geiser-repl--repl repl-buffer)
	    (setq geiser-impl--implementation nil)
	    (let ((geiser-debug-jump-to-debug-p nil)
		  (geiser-debug-show-debug-p nil))
              ;; `geiser-eval-region/wait' was introduced to await the
              ;; result of async evaluation in geiser version 0.22.
	      (let ((ret (funcall (if (fboundp 'geiser-eval-region/wait)
                                      #'geiser-eval-region/wait
                                    #'geiser-eval-region)
                                  ;; Do not include top comment into evaluation.
                                  ;; Apparently, mit-scheme has
                                  ;; problems with the top comment we add:
                                  ;; "Unexpected read restart on: #[textual-i/o-port 27 for console]"
                                  beg
                                  (point-max))))
	        (let ((err (geiser-eval--retort-error ret)))
		  (setq result (cond
			        (output
			         (or (geiser-eval--retort-output ret)
				     "Geiser Interpreter produced no output"))
			        (err nil)
			        (t (geiser-eval--retort-result-str ret ""))))
	          (when (not repl)
	            (save-current-buffer (set-buffer repl-buffer)
				         (geiser-repl-exit))
	            (set-process-query-on-exit-flag (get-buffer-process repl-buffer) nil)
		    (kill-buffer repl-buffer))
		  (when err
		    (let ((msg (geiser-eval--error-msg err)))
		      (org-babel-eval-error-notify
		       nil
		       (concat (if (listp msg) (car msg) msg) "\n")))))))))))
    result))

(defun org-babel-scheme--table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or tuple, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (let ((res (and results (org-babel-script-escape results))))
    (cond ((listp res)
           (mapcar (lambda (el)
		     (if (or (null el) (eq el 'null))
			 org-babel-scheme-null-to
		       el))
                   res))
	  (t res))))

(defun org-babel-execute:scheme (body params)
  "Execute a block of Scheme code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((source-buffer (current-buffer))
	 (source-buffer-name (replace-regexp-in-string ;; zap surrounding *
			      "^ ?\\*\\([^*]+\\)\\*" "\\1"
			      (buffer-name source-buffer))))
    (save-excursion
      (let* ((result-type (cdr (assq :result-type params)))
	     (impl (or (when (cdr (assq :scheme params))
			 (intern (cdr (assq :scheme params))))
		       geiser-scheme-implementation
		       geiser-default-implementation
		       (car geiser-active-implementations)))
             (host (cdr (assq :host params)))
             (port (cdr (assq :port params)))
	     (session (org-babel-scheme-make-session-name
		       source-buffer-name (cdr (assq :session params)) impl))
	     (full-body (org-babel-expand-body:scheme body params))
	     (result-params (cdr (assq :result-params params)))
	     (result
	      (org-babel-scheme-execute-with-geiser
	       full-body		       ; code
	       (string= result-type "output")  ; output?
	       impl			       ; implementation
	       (and (not (string= session "none")) session) ; session
               host ; REPL host
               port))) ; REPL port
	(let ((table
	       (org-babel-reassemble-table
		result
		(org-babel-pick-name (cdr (assq :colname-names params))
				     (cdr (assq :colnames params)))
		(org-babel-pick-name (cdr (assq :rowname-names params))
				     (cdr (assq :rownames params))))))
	  (org-babel-result-cond result-params
	    result
	    (org-babel-scheme--table-or-string table)))))))

(provide 'ob-scheme)

;;; ob-scheme.el ends here
