;;; gnus-beta.el --- transitory wrapper for gnus.el  -*- lexical-binding:t -*-

;; Copyright (C) 1987-1990, 1993-1998, 2000-2021 Free Software
;; Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail
;; Version: 5.14pre
;; Package-Requires: ((emacs "28"))

;; This file is part of GNU Emacs.

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

;; Clear already loaded modules from core before compiling, lest the
;; byte-compilation fail on macros only defined in gnus-beta.

;;; Code:

(eval-when-compile
  (mapc (lambda (m) (delq m features))
        '(canlock
          deuglify
          gmm-utils
          gnus-agent
          gnus-art
          gnus-async
          gnus-bcklg
          gnus-bookmark
          gnus-cache
          gnus-cite
          gnus-cloud
          gnus-cus
          gnus-dbus
          gnus-delay
          gnus-demon
          gnus-diary
          gnus-dired
          gnus-draft
          gnus-dup
          gnus-eform
          gnus-fun
          gnus-gravatar
          gnus-group
          gnus-html
          gnus-icalendar
          gnus-int
          gnus-kill
          gnus-logic
          gnus-mh
          gnus-ml
          gnus-mlspl
          gnus-msg
          gnus-notifications
          gnus-picon
          gnus-range
          gnus-registry
          gnus-rfc1843
          gnus-salt
          gnus-score
          gnus-search
          gnus-sieve
          gnus-spec
          gnus-srvr
          gnus-start
          gnus-sum
          gnus-topic
          gnus-undo
          gnus-util
          gnus-uu
          gnus-vm
          gnus-win
          gnus
          gssapi
          legacy-gnus-agent
          mail-source
          message
          mh-compat
          mm-archive
          mm-bodies
          mm-decode
          mm-encode
          mm-extern
          mm-partial
          mm-url
          mm-util
          mm-uu
          mm-view
          mml-sec
          mml-smime
          mml
          mml1991
          mml2015
          nnagent
          nnbabyl
          nndiary
          nndir
          nndoc
          nndraft
          nneething
          nnfolder
          nngateway
          nnheader
          nnimap
          nnir
          nnmail
          nnmaildir
          nnmairix
          nnmbox
          nnmh
          nnml
          nnnil
          nnoo
          nnregistry
          nnrss
          nnselect
          nnspool
          nntp
          nnvirtual
          nnweb
          ol-gnus
          score-mode
          smiley
          smime
          spam-report
          spam-stat
          spam-wash
          spam)))

(require 'gnus)

(provide 'gnus-beta)

;;; gnus-beta.el ends here
