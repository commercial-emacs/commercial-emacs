;;; filenotify.el --- watch files for changes on disk  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2024 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

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

;; Abstraction layer for inotify, kqueue, gfilenotify, and w32notify,
;; collectively known as file notification "backends."

;; A. Politz issued a vague caveat in 158bb85 about a file watch being
;; added to an already watched directory.

;;; Code:

(require 'cl-lib)
(make-obsolete-variable 'file-notify-debug nil "30.1")

(defconst file-notify--library
  (cond ((featurep 'inotify) 'inotify)
        ((featurep 'kqueue) 'kqueue)
        ((featurep 'gfilenotify) 'gfilenotify)
        ((featurep 'w32notify) 'w32notify))
  "Non-nil when Emacs has been compiled with file notification support.
The value is the name of the low-level file notification package
to be used for local file systems.  Remote file notifications
could use another implementation.")

(cl-defstruct (file-notify--watch
               (:constructor nil)
               (:constructor
                file-notify--watch-make (directory filename callback)))
  "Internal bookkeeping used in `file-notify-descriptors'.
FILENAME is relative to DIRECTORY, and nil if entire directory
watched."
  directory filename callback)

(defvar file-notify-descriptors (make-hash-table :test 'equal)
  "Look up registered file notification descriptors.
The key is a file name handler or a descriptor specific to the
`file-notify--library' backend.  The value is a
`file-notify--watch' instance.")

(defun file-notify--watch-absolute-filename (watch)
  "Return the absolute filename observed by WATCH."
  (if (file-notify--watch-filename watch)
      (expand-file-name
       (file-notify--watch-filename watch)
       (file-notify--watch-directory watch))
    (file-notify--watch-directory watch)))

(cl-defstruct (file-notify (:type list) :named)
  "A file system monitoring event, coming from the backends."
  -event -callback)

;;;###autoload
(defun file-notify-handle-event (notify)
  "Call NOTIFY's callback on NOTIFY's event."
  (declare (completion ignore))
  (interactive "e")
  (if (file-notify-p notify)
      (funcall (file-notify--callback notify) (file-notify--event notify))
    (signal 'file-notify-error
	    (cons "Not a valid file-notify-event" notify))))

(cl-defstruct (file-notify--rename
               (:constructor nil)
               (:constructor
                file-notify--rename-make (watch desc from-file cookie)))
  watch desc from-file cookie)

(defvar file-notify--pending-rename nil
  "A pending rename event awaiting the destination file name.
It is nil or a `file-notify--rename' defstruct where the cookie can be nil.")

(defun file-notify--expand-file-name (watch file)
  "Full file name of FILE reported for WATCH."
  (directory-file-name
   (expand-file-name file (file-notify--watch-directory watch))))

(cl-defun file-notify--callback-inotify ((desc actions file
                                          &optional file1-or-cookie))
  "Notification callback for inotify."
  (file-notify--handle-event
   desc
   (seq-keep (lambda (action)
               (cond ((eq action 'create) 'created)
                     ((eq action 'modify) 'changed)
                     ((eq action 'attrib) 'attribute-changed)
                     ((memq action '(delete delete-self move-self)) 'deleted)
                     ((eq action 'moved-from) 'renamed-from)
                     ((eq action 'moved-to) 'renamed-to)
                     ((eq action 'ignored) 'stopped)))
             actions)
   file file1-or-cookie))

(cl-defun file-notify--callback-kqueue ((desc actions file
                                         &optional file1-or-cookie))
  "Notification callback for kqueue."
  (file-notify--handle-event
   desc
   (seq-keep (lambda (action)
               (cond ((eq action 'create) 'created)
                     ((eq action 'write) 'changed)
                     ((memq action '(attrib link)) 'attribute-changed)
                     ((eq action 'delete) 'deleted)
                     ((eq action 'rename) 'renamed)))
             actions)
   file file1-or-cookie))

(cl-defun file-notify--callback-w32notify ((desc actions file
                                            &optional file1-or-cookie))
  "Notification callback for w32notify."
  (let ((action (pcase actions
                 ('added 'created)
                 ('modified 'changed)
                 ('removed 'deleted)
                 ('renamed-from 'renamed-from)
                 ('renamed-to 'renamed-to))))
    (when action
      (file-notify--handle-event desc (list action) file file1-or-cookie))))

(cl-defun file-notify--callback-gfilenotify ((desc actions file
                                              &optional file1-or-cookie))
  "Notification callback for gfilenotify."
  (file-notify--handle-event
   desc
   (seq-keep
    (lambda (action)
      (if (memq action '(created changed attribute-changed deleted))
          action
        (when (eq action 'moved)
          'renamed)))
    (if (consp actions) actions (list actions)))
   file file1-or-cookie))

(cl-defun file-notify-callback ((desc actions file &optional file1-or-cookie))
  "Notification callback for file name handlers."
  (file-notify--handle-event
   desc
   (seq-keep
    (lambda (action)
      (cond ;; gfilenotify actions:
            ((memq action '(created changed attribute-changed deleted))
             action)
            ((eq action 'moved) 'renamed)
            ;; inotify actions:
            ((eq action 'create) 'created)
            ((eq action 'modify) 'changed)
            ((eq action 'attrib) 'attribute-changed)
            ((memq action '(delete delete-self move-self)) 'deleted)
            ((eq action 'moved-from) 'renamed-from)
            ((eq action 'moved-to) 'renamed-to)
            ((eq action 'ignored) 'stopped)))
    (if (consp actions) actions (list actions)))
   file file1-or-cookie))

(defun file-notify--call-handler (watch desc action file file1)
  "Call the handler of WATCH with the arguments DESC, ACTION, FILE and FILE1."
  (when (or
         ;; If there is no relative file name for that
         ;; watch, we watch the whole directory.
         (null (file-notify--watch-filename watch))
         ;; File matches.
         (string-equal
          (file-notify--watch-filename watch)
          (file-name-nondirectory file))

         ;; Directory matches.
         ;;  FIXME: What purpose would this condition serve?
         ;;  Doesn't it just slip through events for files
         ;;  having the same name as the last component of the
         ;;  directory of the file that we are really watching?
         ;;(string-equal
         ;; (file-name-nondirectory file)
         ;; (file-name-nondirectory (file-notify--watch-directory watch)))

         ;; File1 matches.
         (and (stringp file1)
              (string-equal (file-notify--watch-filename watch)
                            (file-name-nondirectory file1))))
    ;; (message
    ;;  "file-notify-callback %S %S %S %S %S %S %S"
    ;;  desc action file file1 watch
    ;;  (file-notify--watch-absolute-filename watch)
    ;;  (file-notify--watch-directory watch))
    (funcall (file-notify--watch-callback watch)
             (if file1
                 (list desc action file file1)
               (list desc action file)))))

(defun file-notify--handle-event (desc actions file file1-or-cookie)
  "Handle an event returned from file notification.
DESC is the back-end descriptor.  ACTIONS is a list of:
 \\='created
 \\='changed
 \\='attribute-changed
 \\='deleted
 \\='renamed       -- FILE is old name, FILE1-OR-COOKIE is new name or nil
 \\='renamed-from  -- FILE is old name, FILE1-OR-COOKIE is cookie or nil
 \\='renamed-to    -- FILE is new name, FILE1-OR-COOKIE is cookie or nil
 \\='stopped       -- no more events after this should be sent"
  (when-let ((watch (gethash desc file-notify-descriptors))
             (file (file-notify--expand-file-name watch file)))
    (while actions
      (let ((action (pop actions)))
        (when file-notify--pending-rename
          ;; We assume {renamed,moved}-{from,to} pairs arrive
          ;; contiguously without intervening events.  If the present
          ;; event does not refer to the pending rename, consider the
          ;; rename a deletion (a rename without a renamed-to)
          (unless (and (equal (file-notify--rename-cookie
                               file-notify--pending-rename)
                              file1-or-cookie)
                       (eq action 'renamed-to))
            (when-let ((callback (file-notify--watch-callback
                                  (file-notify--rename-watch
                                   file-notify--pending-rename))))
              (funcall callback (list (file-notify--rename-desc
                                       file-notify--pending-rename)
                                      'deleted
                                      (file-notify--rename-from-file
                                       file-notify--pending-rename))))
            (setq file-notify--pending-rename nil)))
        (let (file1)
          (cond ((eq action 'renamed)
                 (if file1-or-cookie
                     (setq file1 (file-notify--expand-file-name watch file1-or-cookie))
                   ;; A renamed event without a dest name is a deletion.
                   (setq action 'deleted)))
                ((eq action 'stopped)
                 (file-notify-rm-watch desc)
                 (setq actions nil
                       action nil))
                ;; Make the event pending.
                ((eq action 'renamed-from)
                 (setq file-notify--pending-rename
                       (file-notify--rename-make watch desc file file1-or-cookie)
                       action nil))
                ;; Look for pending event.
                ((eq action 'renamed-to)
                 (if file-notify--pending-rename
                     (let ((callback (file-notify--watch-callback
                                      (file-notify--rename-watch
                                       file-notify--pending-rename)))
                           (pending-desc (file-notify--rename-desc
                                          file-notify--pending-rename))
                           (from-file (file-notify--rename-from-file
                                       file-notify--pending-rename)))
                       (setq file1 file
                             file from-file)
                       ;; If the source is handled by another watch, we
                       ;; must fire the rename event there as well.
                       (when (and callback (not (equal desc pending-desc)))
                         (funcall callback
                                  (list pending-desc 'renamed file file1)))
                       (setq file-notify--pending-rename nil
                             action 'renamed))
                   (setq action 'created))))
          (when action
            (file-notify--call-handler watch desc action file file1)
            (when (and (memq action '(deleted renamed))
                       (string-equal file
                                     (file-notify--watch-absolute-filename watch))
                       ;; Not when FILE is backed up.
                       (not (and (stringp file1) (backup-file-name-p file1))))
              (file-notify-rm-watch desc))))))))

(declare-function inotify-add-watch "inotify.c" (file flags callback))
(declare-function kqueue-add-watch "kqueue.c" (file flags callback))
(declare-function w32notify-add-watch "w32notify.c" (file flags callback))
(declare-function gfile-add-watch "gfilenotify.c" (file flags callback))
(declare-function inotify-valid-p "inotify.c" (desc))
(declare-function kqueue-valid-p "kqueue.c" (desc))
(declare-function w32notify-valid-p "w32notify.c" (desc))
(declare-function gfile-valid-p "gfilenotify.c" (desc))
(declare-function inotify-rm-watch "inotify.c" (desc))
(declare-function kqueue-rm-watch "kqueue.c" (desc))
(declare-function w32notify-rm-watch "w32notify.c" (desc))
(declare-function gfile-rm-watch "gfilenotify.c" (desc))

(defun file-notify--add-watch-inotify (_file dir flags)
  "Add a watch for FILE in DIR with FLAGS, using inotify."
  (inotify-add-watch dir
                     (append
                      '(dont-follow)
                      (when (memq 'change flags)
                        '(create delete delete-self modify move-self move))
                      (when (memq 'attribute-change flags)
                        '(attrib)))
                     #'file-notify--callback-inotify))

(defun file-notify--add-watch-kqueue (file _dir flags)
  "Add a watch for FILE in DIR with FLAGS, using kqueue.
kqueue does not report changes to file contents when watching
directories, so we watch each file directly."
  (kqueue-add-watch file
                    (append
                     (when (memq 'change flags)
	               '(create delete write extend rename))
                     (when (memq 'attribute-change flags)
                       '(attrib)))
                    #'file-notify--callback-kqueue))

(defun file-notify--add-watch-w32notify (_file dir flags)
  "Add a watch for FILE in DIR with FLAGS, using w32notify."
  (w32notify-add-watch dir
                       (append
                        (when (memq 'change flags)
                          '(file-name directory-name size last-write-time))
                        (when (memq 'attribute-change flags)
                          '(attributes)))
                       #'file-notify--callback-w32notify))

(defun file-notify--add-watch-gfilenotify (_file dir flags)
  "Add a watch for FILE in DIR with FLAGS, using gfilenotify."
  (gfile-add-watch dir
                   (append '(watch-mounts send-moved) flags)
                   #'file-notify--callback-gfilenotify))

(defun file-notify-add-watch (file flags callback)
  "Return a backend-specific descriptor for the added watch.

FLAGS is a list of symbols.  Possible symbols include \\='change
which watches for file changes, and \\='attribute-change which
watches for changes to permissions or modtime.

If FILE is a directory, \\='change watches for file creation and
deletion, and depending on the backend may also detect changes to files
(but not subdirectories) within the directory.

Upon receiving an event, CALLBACK is passed a list of the form,

  (DESCRIPTOR ACTION FILE [FILE1])

DESCRIPTOR is backend-specific descriptor.
ACTION is a symbol describing the event, which could be one of
\\='created, \\='deleted, \\='changed, \\='renamed, \\='attribute-changed,
or \\='stopped.
FILE is the watched file name."
  (if (stringp file)
      (setq file (expand-file-name file))
    (signal 'wrong-type-argument `(,file)))
  (when (or (not (listp flags))
            (and (not (memq 'change flags))
                 (not (memq 'attribute-change flags))))
    (signal 'wrong-type-argument `(,flags)))
  (unless (functionp callback)
    (signal 'wrong-type-argument `(,callback)))

  (let* ((handler (find-file-name-handler file 'file-notify-add-watch))
	 (dir (directory-file-name (if (file-directory-p file)
		                       file
		                     (file-name-directory file))))
         (_ (unless (file-directory-p dir)
              (signal 'file-notify-error `("Directory does not exist" ,dir))))
         (desc (if handler
                   (funcall handler 'file-notify-add-watch dir flags callback)
                 (funcall
                  (pcase-exhaustive file-notify--library
                    ('inotify #'file-notify--add-watch-inotify)
                    ('kqueue #'file-notify--add-watch-kqueue)
                    ('gfilenotify #'file-notify--add-watch-gfilenotify)
                    ('w32notify #'file-notify--add-watch-w32notify))
                  file dir flags)))
         (watch (file-notify--watch-make
                 (file-name-unquote dir) ; normalize file names in hash
                 (unless (file-directory-p file)
                   (file-name-nondirectory file))
                 callback)))
    (prog1 desc
      (puthash desc watch file-notify-descriptors))))

(defun file-notify-rm-watch (descriptor)
  "Remove watch corresponding to backend-specific DESCRIPTOR."
  (when-let ((watch (gethash descriptor file-notify-descriptors)))
    (remhash descriptor file-notify-descriptors)
    (condition-case nil
        (if-let ((handler (find-file-name-handler
                           (file-notify--watch-directory watch)
                           'file-notify-rm-watch)))
            (funcall handler 'file-notify-rm-watch descriptor)
          (funcall
           (pcase-exhaustive file-notify--library
             ('inotify #'inotify-rm-watch)
             ('kqueue #'kqueue-rm-watch)
             ('gfilenotify #'gfile-rm-watch)
             ('w32notify #'w32notify-rm-watch))
           descriptor))
      (file-notify-error nil))
    (file-notify-handle-event ; send stopped event
     (make-file-notify
      :-event `(,descriptor
                stopped
                ,(file-notify--watch-absolute-filename watch))
      :-callback (file-notify--watch-callback watch)))))

(defun file-notify-rm-all-watches ()
  (interactive)
  (maphash (lambda (key _value)
             (file-notify-rm-watch key))
           file-notify-descriptors))

(defun file-notify-valid-p (descriptor)
  (when-let ((watch (gethash descriptor file-notify-descriptors)))
    (when (if-let ((handler (find-file-name-handler
                             (file-notify--watch-directory watch)
                             'file-notify-valid-p)))
              (funcall handler 'file-notify-valid-p descriptor)
            (funcall
             (pcase-exhaustive file-notify--library
               ('inotify #'inotify-valid-p)
               ('kqueue #'kqueue-valid-p)
               ('gfilenotify #'gfile-valid-p)
               ('w32notify #'w32notify-valid-p))
             descriptor))
      t)))

(provide 'filenotify)

;;; filenotify.el ends here
