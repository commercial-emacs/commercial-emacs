;;; erc-scenarios-base-reuse-buffers.el --- base-reuse-buffers scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(eval-when-compile (require 'erc-join))

(defun erc-scenarios-common--base-reuse-buffers-server-buffers (&optional more)
  "Show that `erc-reuse-buffers' doesn't affect server buffers.
Overlaps some with `clash-of-chans/uniquify'.  Adapted from
rebuffed/reuseless, described in Bug#48598: 28.0.50; buffer-naming
collisions involving bouncers in ERC.  Run EXTRA."
  (erc-scenarios-common-with-cleanup
      ((dumb-server (erc-d-run "localhost" t 'foonet 'barnet))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist)

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name)
                         (format "127.0.0.1:%d/127.0.0.1" port)))
        (erc-d-t-search-for 12 "marked as being away")))

    (ert-info ("Connect to barnet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "barnet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name)
                         (format "127.0.0.1:%d/127.0.0.1<2>" port)))
        (erc-d-t-search-for 45 "marked as being away")))

    (erc-d-t-wait-for 2 (get-buffer (format "127.0.0.1:%d/127.0.0.1" port)))
    (erc-d-t-wait-for 2 (get-buffer (format "127.0.0.1:%d/127.0.0.1<2>" port)))

    (ert-info ("Server buffers are unique, no IP-based names")
      (should (cdr (erc-scenarios-common-buflist "127.0.0.1"))))
    (when more (funcall more port))))

;; XXX maybe remove: already covered many times over by other scenarios
(ert-deftest erc-scenarios-base-reuse-buffers-server-buffers--enabled ()
  :tags '(:expensive-test)
  (with-suppressed-warnings ((obsolete erc-reuse-buffers))
    (should erc-reuse-buffers))
  (let ((erc-scenarios-common-dialog "base/reuse-buffers/server"))
    (erc-scenarios-common-with-cleanup
        ((dumb-server (erc-d-run "localhost" t 'foonet 'barnet))
         (port (process-contact dumb-server :service))
         erc-autojoin-channels-alist)

      (ert-info ("Connect to foonet")
        (with-current-buffer (erc :server "127.0.0.1"
                                  :port port
                                  :nick "tester"
                                  :password "foonet:changeme"
                                  :full-name "tester")
          (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
          (erc-d-t-search-for 12 "marked as being away")))

      (ert-info ("Connect to barnet")
        (with-current-buffer (erc :server "127.0.0.1"
                                  :port port
                                  :nick "tester"
                                  :password "barnet:changeme"
                                  :full-name "tester")
          (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
          (erc-d-t-search-for 45 "marked as being away")))

      (erc-d-t-wait-for 2 (get-buffer "foonet"))
      (erc-d-t-wait-for 2 (get-buffer "barnet"))

      (ert-info ("Server buffers are unique, no IP-based names")
        (should-not (eq (get-buffer "foonet") (get-buffer "barnet")))
        (should-not (erc-scenarios-common-buflist "127.0.0.1"))))))

;; FIXME no sense in running this twice (JOIN variant includes this)
(ert-deftest erc-scenarios-base-reuse-buffers-server-buffers--disabled ()
  :tags '(:expensive-test)
  (with-suppressed-warnings ((obsolete erc-reuse-buffers))
    (should erc-reuse-buffers)
    (let ((erc-scenarios-common-dialog "base/reuse-buffers/server")
          erc-reuse-buffers)
      (erc-scenarios-common--base-reuse-buffers-server-buffers nil))))

;; This also asserts that `erc-cmd-JOIN' is no longer susceptible to a
;; regression introduced in 28.1 (ERC 5.4) that caused phantom target
;; buffers of the form target/server to be created via
;; `switch-to-buffer' ("phantom" because they would go unused").  This
;; would happen (in place of a JOIN being sent out) when a previously
;; used (parted) target buffer existed and `erc-reuse-buffers' was
;; nil.
;;
;; Note: All the `erc-get-channel-user' calls have to do with the fact
;; that `erc-default-target' relies on the ambiguously defined
;; `erc-default-recipients' (meaning it's overloaded in the sense of
;; being used both for retrieving a target name and checking if a
;; channel has been PARTed).  While not ideal, `erc-get-channel-user'
;; can (also) be used to detect the latter.

(defun erc-scenarios-common--base-reuse-buffers-channel-buffers (port)
  "The option `erc-reuse-buffers' is still respected when nil.
Adapted from scenario clash-of-chans/uniquify described in Bug#48598:
28.0.50; buffer-naming collisions involving bouncers in ERC."
  (let* ((expect (erc-d-t-make-expecter))
         (server-buffer-foo
          (get-buffer (format "127.0.0.1:%d/127.0.0.1" port)))
         (server-buffer-bar
          (get-buffer (format "127.0.0.1:%d/127.0.0.1<2>" port)))
         (chan-buffer-foo (get-buffer "#chan/127.0.0.1"))
         (chan-buffer-bar (get-buffer "#chan/127.0.0.1<2>"))
         (server-process-foo (with-current-buffer server-buffer-foo
                               erc-server-process))
         (server-process-bar (with-current-buffer server-buffer-bar
                               erc-server-process)))

    (ert-info ("Unique #chan buffers exist")
      (let ((chan-bufs (erc-scenarios-common-buflist "#chan"))
            (known (list chan-buffer-bar chan-buffer-foo)))
        (should (memq (pop chan-bufs) known))
        (should (memq (pop chan-bufs) known))
        (should-not chan-bufs)))

    (ert-info ("#chan@foonet is exclusive and not contaminated")
      (with-current-buffer chan-buffer-foo
        (funcall expect 1 "<bob>")
        (erc-d-t-absent-for 0.1 "<joe>")
        (funcall expect 1 "strength to climb")
        (should (eq erc-server-process server-process-foo))))

    (ert-info ("#chan@barnet is exclusive and not contaminated")
      (with-current-buffer chan-buffer-bar
        (funcall expect 1 "<joe>")
        (erc-d-t-absent-for 0.1 "<bob>")
        (funcall expect 1 "the loudest noise")
        (should (eq erc-server-process server-process-bar))))

    (ert-info ("Part #chan@foonet")
      (with-current-buffer chan-buffer-foo
        (erc-d-t-search-for 1 "shake my sword")
        (erc-cmd-PART "#chan")
        (funcall expect 3 "You have left channel #chan")
        (erc-cmd-JOIN "#chan")))

    (ert-info ("Part #chan@barnet")
      (with-current-buffer chan-buffer-bar
        (funcall expect 10 "Arm it in rags")
        (should (erc-get-channel-user (erc-current-nick)))
        (erc-cmd-PART "#chan")
        (funcall expect 3 "You have left channel #chan")
        (should-not (erc-get-channel-user (erc-current-nick)))
        (erc-cmd-JOIN "#chan")))

    (erc-d-t-wait-for 3 "New unique target buffer for #chan@foonet created"
      (get-buffer "#chan/127.0.0.1<3>"))

    (ert-info ("Activity continues in new, <n>-suffixed #chan@foonet buffer")
      (with-current-buffer chan-buffer-foo
        (should-not (erc-get-channel-user (erc-current-nick))))
      (with-current-buffer "#chan/127.0.0.1<3>"
        (should (erc-get-channel-user (erc-current-nick)))
        (funcall expect 2 "You have joined channel #chan")
        (funcall expect 2 "#chan was created on")
        (funcall expect 2 "<alice>")
        (should (eq erc-server-process server-process-foo))
        (erc-d-t-absent-for 0.2 "<joe>")))

    (sit-for 3)
    (erc-d-t-wait-for 5 "New unique target buffer for #chan@barnet created"
      (get-buffer "#chan/127.0.0.1<4>"))

    (ert-info ("Activity continues in new, <n>-suffixed #chan@barnet buffer")
      (with-current-buffer chan-buffer-bar
        (should-not (erc-get-channel-user (erc-current-nick))))
      (with-current-buffer "#chan/127.0.0.1<4>"
        (funcall expect 2 "You have joined channel #chan")
        (funcall expect 1 "Users on #chan: @mike joe tester")
        (funcall expect 2 "<mike>")
        (should (eq erc-server-process server-process-bar))
        (erc-d-t-absent-for 0.2 "<bob>")))

    (ert-info ("Two new chans created for a total of four")
      (let* ((bufs (erc-scenarios-common-buflist "#chan"))
             (names (sort (mapcar #'buffer-name bufs) #'string<)))
        (should
         (equal names (mapcar (lambda (f) (concat "#chan/127.0.0.1" f))
                              '("" "<2>" "<3>" "<4>"))))))

    (ert-info ("All output sent")
      (with-current-buffer "#chan/127.0.0.1<3>"
        (funcall expect 10 "most lively"))
      (with-current-buffer "#chan/127.0.0.1<4>"
        (funcall expect 10 "soul black")))

    ;; TODO ensure the exact <N>'s aren't reassigned during killing as
    ;; they are when the option is on.
    (ert-info ("Buffers are exempt from shortening")
      (kill-buffer "#chan/127.0.0.1<4>")
      (kill-buffer "#chan/127.0.0.1<3>")
      (kill-buffer chan-buffer-bar)
      (should-not (get-buffer "#chan"))
      (should chan-buffer-foo))))

(ert-deftest erc-scenarios-base-reuse-buffers-channel-buffers--disabled ()
  :tags '(:expensive-test :unstable)
  (with-suppressed-warnings ((obsolete erc-reuse-buffers))
    (should erc-reuse-buffers)
    (let ((erc-scenarios-common-dialog "base/reuse-buffers/channel")
          (erc-server-flood-penalty 0.1)
          erc-reuse-buffers)
      (erc-scenarios-common--base-reuse-buffers-server-buffers
       #'erc-scenarios-common--base-reuse-buffers-channel-buffers))))

;;; erc-scenarios-base-reuse-buffers.el ends here
