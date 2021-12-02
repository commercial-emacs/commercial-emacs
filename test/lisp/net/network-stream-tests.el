;;; network-stream-tests.el --- tests for network processes       -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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
(require 'gnutls)
(require 'network-stream)
;; The require above is needed for 'open-network-stream' to work, but
;; it pulls in nsm, which then makes the :nowait t' tests fail unless
;; we disable the nsm, which we do by binding 'network-security-level'

(ert-deftest make-local-unix-server ()
  (skip-unless (featurep 'make-network-process '(:family local)))
  (let* ((file (make-temp-name "/tmp/server-test"))
         (server
          (make-network-process
           :name "server"
           :server t
           :buffer (get-buffer-create "*server*")
           :noquery t
           :family 'local
           :service file)))
    (should (equal (process-contact server :local) file))
    (delete-file (process-contact server :local))))

(ert-deftest make-ipv4-tcp-server-with-unspecified-port ()
  (let ((server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service t
          :host 'local)))
    (should (and (arrayp (process-contact server :local))
                 (numberp (aref (process-contact server :local) 4))
                 (> (aref (process-contact server :local) 4) 0)))
    (delete-process server)))

(ert-deftest make-ipv4-tcp-server-with-specified-port ()
  (let ((server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service 57869
          :host 'local)))
    (should (and (arrayp (process-contact server :local))
                 (= (aref (process-contact server :local) 4) 57869)))
    (delete-process server)))

(ert-deftest make-ipv6-tcp-server-with-unspecified-port ()
  (skip-unless (featurep 'make-network-process '(:family ipv6)))
  (let ((server
         (ignore-errors
           (make-network-process
            :name "server"
            :server t
            :noquery t
            :family 'ipv6
            :service t
            :host 'local))))
    (skip-unless server)
    (should (and (arrayp (process-contact server :local))
                 (numberp (aref (process-contact server :local) 8))
                 (> (aref (process-contact server :local) 8) 0)))
    (delete-process server)))

(ert-deftest make-ipv6-tcp-server-with-specified-port ()
  (skip-unless (featurep 'make-network-process '(:family ipv6)))
  (let ((server
         (ignore-errors
           (make-network-process
            :name "server"
            :server t
            :noquery t
            :family 'ipv6
            :service 57870
            :host 'local))))
    (skip-unless server)
    (should (and (arrayp (process-contact server :local))
                 (= (aref (process-contact server :local) 8) 57870)))
    (delete-process server)))

(defun make-server (host &optional family)
  (make-network-process
   :name "server"
   :server t
   :noquery t
   :family (or family 'ipv4)
   :coding 'raw-text-unix
   :buffer (get-buffer-create "*server*")
   :service t
   :sentinel 'server-sentinel
   :filter 'server-process-filter
   :host host))

(defun server-sentinel (_proc _msg)
  )

(defun server-process-filter (proc string)
  (message "Received %s" string)
  (let ((prev (process-get proc 'previous-string)))
    (when prev
      (setq string (concat prev string))
      (process-put proc 'previous-string nil)))
  (if (and (not (string-search "\n" string))
           (> (length string) 0))
      (process-put proc 'previous-string string))
  (let ((command (split-string string)))
    (cond
     ((equal (car command) "echo")
      (process-send-string proc (concat (cadr command) "\n")))
     (t
      ))))

(defun network-stream-tests--resolve-system-name ()
  (cl-loop for address in (network-lookup-address-info (system-name))
           when (or (and (= (length address) 5)
                         ;; IPv4 localhost addresses start with 127.
                         (= (elt address 0) 127))
                    (and (= (length address) 9)
                         ;; IPv6 localhost address.
                         (equal address [0 0 0 0 0 0 0 1 0])))
           return t))

(defmacro network-stream-tests-retry (&rest body)
  `(cl-loop with status
            repeat 30
            when (setq status (condition-case err
                                  (progn ,@body)
                                (error (prog1 nil
                                         (message "retry: %s"
                                                  (error-message-string err))))))
            return status
            do (accept-process-output nil 0.3)))

(defmacro network-stream-tests-echo-server (make-server iport &rest params)
  `(let* ((server ,make-server)
          (port (aref (process-contact server :local) ,iport))
          (buffer (generate-new-buffer "*foo*"))
          (proc (make-network-process :name "foo"
                                      :buffer buffer
                                      :service port
                                      ,@params)))
     (network-stream-tests-retry (not (eq (process-status proc) 'connect)))
     (unwind-protect
         (with-current-buffer (process-buffer proc)
           (process-send-string proc "echo foo")
           (network-stream-tests-retry (equal (buffer-string) "foo\n")))
       (when (process-live-p proc) (delete-process proc))
       (let (kill-buffer-query-functions)
         (kill-buffer buffer))
       (when (process-live-p server) (delete-process server)))))

(ert-deftest echo-server-with-dns ()
  (skip-unless (network-stream-tests--resolve-system-name))
  (network-stream-tests-echo-server
   (make-server (system-name)) 4
   :host (system-name)))

(ert-deftest echo-server-with-localhost ()
  (network-stream-tests-echo-server
   (make-server 'local) 4
   :host "localhost"))


(ert-deftest echo-server-with-local-ipv4 ()
  (network-stream-tests-echo-server
   (make-server 'local 'ipv4) 4
   :host 'local
   :family 'ipv4))

(ert-deftest echo-server-with-local-ipv6 ()
  (skip-unless (featurep 'make-network-process '(:family ipv6)))
  (network-stream-tests-echo-server
   (make-server 'local 'ipv6) 8
   :host 'local
   :family 'ipv6))

(ert-deftest echo-server-with-ip ()
  (network-stream-tests-echo-server
   (make-server 'local) 4
   :host "127.0.0.1"))

(ert-deftest echo-server-nowait ()
  (network-stream-tests-echo-server
   (make-server 'local) 4
   :host "localhost"
   :nowait t
   :family 'ipv4))

(defun make-tls-server ()
  (let ((free-port (with-temp-buffer
                     (let ((proc (make-network-process
                                  :name "free-port"
                                  :noquery t
                                  :host "127.0.0.1"
                                  :buffer (current-buffer)
                                  :server t
                                  :stop t
                                  :service t)))
                       (prog1 (process-contact proc :service)
                         (delete-process proc))))))
    (cons free-port
          (start-process "gnutls" (generate-new-buffer "*tls*")
                         "gnutls-serv" "--http"
                         "--x509keyfile"
                         (ert-resource-file "key.pem")
                         "--x509certfile"
                         (ert-resource-file "cert.pem")
                         "--port" (format "%s" free-port)))))

(defmacro network-stream-tests-make-network-process (negotiate &rest params)
  `(pcase-let ((`(,port . ,server) (make-tls-server))
               (buffer (generate-new-buffer "*foo*")))
     (unwind-protect
         (network-stream-tests-doit
          port server
          (make-network-process
           :name "bar"
           :buffer buffer
           :service port
           ,@params)
          ,negotiate)
       (let (kill-buffer-query-functions)
         (kill-buffer buffer))
       (when (process-live-p server) (delete-process server)))))

(defmacro network-stream-tests-open-stream (func &rest params)
  `(pcase-let ((`(,port . ,server) (make-tls-server))
               (buffer (generate-new-buffer "*foo*")))
     (unwind-protect
         (network-stream-tests-doit
          port server
          (,func
           "bar"
           buffer
           "localhost"
           port
           ,@params))
       (let (kill-buffer-query-functions)
         (kill-buffer buffer))
       (when (process-live-p server) (delete-process server)))))

(cl-defmacro network-stream-tests-doit (port server form &optional negotiate)
  `(let ((network-security-level 'low)
         proc status)
     (unwind-protect
         (progn
           (with-current-buffer (process-buffer ,server)
             (message "gnutls-serv on %s: %s" ,port (buffer-string)))
           (should (setq proc (network-stream-tests-retry ,form)))
           (,(if negotiate 'funcall 'ignore)
            #'gnutls-negotiate :process proc
            :type 'gnutls-x509pki
            :hostname "localhost")
           (network-stream-tests-retry (not (eq (process-status proc) 'connect)))
           (should (consp (setq status (network-stream-tests-retry
                                        (gnutls-peer-status proc)))))
           (let ((issuer (plist-get (plist-get status :certificate) :issuer)))
             (should (stringp issuer))
             (setq issuer (split-string issuer ","))
             (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC"))))
       (when (process-live-p proc) (delete-process proc)))))

(ert-deftest connect-to-tls-ipv4-wait ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-make-network-process
   t
   :host "localhost"))

(ert-deftest connect-to-tls-ipv4-nowait ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-make-network-process
   nil
   :nowait t
   :family 'ipv4
   :tls-parameters
   (cons 'gnutls-x509pki
         (gnutls-boot-parameters
          :hostname "localhost"))
   :host "localhost"))

(ert-deftest connect-to-tls-ipv6-nowait ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (skip-unless (not (eq system-type 'windows-nt)))
  (skip-unless (featurep 'make-network-process '(:family ipv6)))
  (network-stream-tests-make-network-process
   nil
   :family 'ipv6
   :nowait t
   :tls-parameters
   (cons 'gnutls-x509pki
         (gnutls-boot-parameters
          :hostname "localhost"))
   :host "::1"))

(ert-deftest open-network-stream-tls-wait ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-open-stream
   open-network-stream
   :type 'tls
   :nowait nil))

(ert-deftest open-network-stream-tls-nowait ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-open-stream
   open-network-stream
   :type 'tls
   :nowait t))

(ert-deftest open-network-stream-tls ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-open-stream
   open-network-stream
   :type 'tls))

(ert-deftest open-network-stream-tls-nocert ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-open-stream
   open-network-stream
   :type 'tls
   :client-certificate nil))

(ert-deftest open-gnutls-stream-new-api-default ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-open-stream
   open-gnutls-stream))

(ert-deftest open-gnutls-stream-new-api-wait ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-open-stream
   open-gnutls-stream
   (list :nowait nil)))

(ert-deftest open-gnutls-stream-old-api-wait ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-open-stream
   open-gnutls-stream
   nil))

(ert-deftest open-gnutls-stream-new-api-nowait ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-open-stream
   open-gnutls-stream
   (list :nowait t)))

(ert-deftest open-gnutls-stream-old-api-nowait ()
  :expected-result (if (getenv "CI") t :passed)
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (network-stream-tests-open-stream
   open-gnutls-stream
   t))

(ert-deftest open-gnutls-stream-new-api-errors ()
  (skip-unless (gnutls-available-p))
  (pcase-let ((`(,port . ,server) (make-tls-server)))
    (kill-process server)
    (should-error
     (open-gnutls-stream
      "bar"
      (generate-new-buffer "*foo*")
      "localhost"
      port
      (list t)))
    (should-error
     (open-gnutls-stream
      "bar"
      (generate-new-buffer "*foo*")
      "localhost"
      port
      (vector :nowait t)))))

(ert-deftest check-network-process-coding-system-bind ()
  "Check that binding coding-system-for-{read,write} works."
  (let* ((coding-system-for-read 'binary)
         (coding-system-for-write 'utf-8-unix)
         (server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service t
          :host 'local))
         (coding (process-coding-system server)))
    (should (eq (car coding) 'binary))
    (should (eq (cdr coding) 'utf-8-unix))
    (delete-process server)))

(ert-deftest check-network-process-coding-system-no-override ()
  "Check that coding-system-for-{read,write} is not overridden by :coding nil."
  (let* ((coding-system-for-read 'binary)
         (coding-system-for-write 'utf-8-unix)
         (server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service t
          :coding nil
          :host 'local))
         (coding (process-coding-system server)))
    (should (eq (car coding) 'binary))
    (should (eq (cdr coding) 'utf-8-unix))
    (delete-process server)))

(ert-deftest check-network-process-coding-system-override ()
  "Check that :coding non-nil overrides coding-system-for-{read,write}."
  (let* ((coding-system-for-read 'binary)
         (coding-system-for-write 'utf-8-unix)
         (server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service t
          :coding 'georgian-academy
          :host 'local))
         (coding (process-coding-system server)))
    (should (eq (car coding) 'georgian-academy))
    (should (eq (cdr coding) 'georgian-academy))
    (delete-process server)))
;;; network-stream-tests.el ends here
