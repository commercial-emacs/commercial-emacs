;;; haiku-win.el --- set up windowing on Haiku -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

;; Support for using Haiku's BeOS derived windowing system.

;;; Code:

(eval-when-compile (require 'cl-lib))
(unless (featurep 'haiku)
  (error "%s: Loading haiku-win without having Haiku"
         invocation-name))

;; Documentation-purposes only: actually loaded in loadup.el.
(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'menu-bar)
(require 'fontset)
(require 'dnd)

(add-to-list 'display-format-alist '(".*" . haiku))

;;;; Command line argument handling.

(defvar x-invocation-args)
(defvar x-command-line-resources)

(defvar haiku-initialized)

(defvar haiku-dnd-selection-value nil
  "The local value of the special `XdndSelection' selection.")

(defvar haiku-dnd-selection-converters '((STRING . haiku-dnd-convert-string)
                                         (text/uri-list . haiku-dnd-convert-uri-list))
  "Alist of X selection types to functions that act as selection converters.
The functions should accept a single argument VALUE, describing
the value of the drag-and-drop selection, and return a list of
two elements TYPE and DATA, where TYPE is a string containing the
MIME type of DATA, and DATA is a unibyte string, or nil if the
data could not be converted.

DATA can optionally have a text property `type', which specifies
the type of DATA inside the system message (see the doc string of
`haiku-drag-message' for more details).")

(defun haiku-dnd-convert-string (value)
  "Convert VALUE to a UTF-8 string and appropriate MIME type.
Return a list of the appropriate MIME type, and UTF-8 data of
VALUE as a unibyte string, or nil if VALUE was not a string."
  (when (stringp value)
    (list "text/plain" (string-to-unibyte
                        (encode-coding-string value 'utf-8)))))

(defun haiku-dnd-convert-uri-list (value)
  "Convert VALUE to a file system reference if it is a file name."
  (when (and (stringp value)
             (file-exists-p value))
    (list "refs" (propertize (expand-file-name value) 'type 'ref))))

(declare-function x-open-connection "haikufns.c")
(declare-function x-handle-args "common-win")
(declare-function haiku-selection-data "haikuselect.c")
(declare-function haiku-selection-put "haikuselect.c")
(declare-function haiku-selection-owner-p "haikuselect.c")
(declare-function haiku-put-resource "haikufns.c")
(declare-function haiku-drag-message "haikuselect.c")

(defun haiku--handle-x-command-line-resources (command-line-resources)
  "Handle command line X resources specified with the option `-xrm'.
The resources should be a list of strings in COMMAND-LINE-RESOURCES."
  (dolist (s command-line-resources)
    (let ((components (split-string s ":")))
      (when (car components)
        (haiku-put-resource (car components)
                            (string-trim-left
                             (mapconcat #'identity (cdr components) ":")))))))

(cl-defmethod window-system-initialization (&context (window-system haiku)
                                                     &optional display)
  "Set up the window system.  WINDOW-SYSTEM must be HAIKU.
DISPLAY may be set to the name of a display that will be initialized."
  (cl-assert (not haiku-initialized))

  (create-default-fontset)
  (when x-command-line-resources
    (haiku--handle-x-command-line-resources
     (split-string x-command-line-resources "\n")))
  (x-open-connection (or display "be") x-command-line-resources t)
  (setq haiku-initialized t))

(cl-defmethod frame-creation-function (params &context (window-system haiku))
  (x-create-frame-with-faces params))

(cl-defmethod handle-args-function (args &context (window-system haiku))
  (x-handle-args args))

(defun haiku--selection-type-to-mime (type)
  "Convert symbolic selection type TYPE to its MIME equivalent.
If TYPE is nil, return \"text/plain\"."
  (cond
   ((eq type 'STRING) "text/plain;charset=iso-8859-1")
   ((eq type 'UTF8_STRING) "text/plain")
   ((stringp type) type)
   ((symbolp type) (symbol-name type))
   (t "text/plain")))

(defun haiku-selection-targets (clipboard)
  "Find the types of data available from CLIPBOARD.
CLIPBOARD should be the symbol `PRIMARY', `SECONDARY' or
`CLIPBOARD'.  Return the available types as a list of strings."
  (mapcar #'car (haiku-selection-data clipboard nil)))

(cl-defmethod gui-backend-get-selection (type data-type
                                              &context (window-system haiku))
  (if (eq data-type 'TARGETS)
      (apply #'vector (mapcar #'intern
                              (haiku-selection-targets type)))
    (if (eq type 'XdndSelection)
        haiku-dnd-selection-value
      (haiku-selection-data type (haiku--selection-type-to-mime data-type)))))

(cl-defmethod gui-backend-set-selection (type value
                                              &context (window-system haiku))
  (if (eq type 'XdndSelection)
      (setq haiku-dnd-selection-value value)
    (haiku-selection-put type "text/plain" value t)))

(cl-defmethod gui-backend-selection-exists-p (selection
                                              &context (window-system haiku))
  (haiku-selection-data selection "text/plain"))

(cl-defmethod gui-backend-selection-owner-p (selection &context (window-system haiku))
  (haiku-selection-owner-p selection))

(declare-function haiku-read-file-name "haikufns.c")

(defun x-file-dialog (prompt dir &optional default-filename mustmatch only-dir-p)
  "SKIP: real doc in xfns.c."
  (if (eq (framep-on-display (selected-frame)) 'haiku)
      (haiku-read-file-name (if (not (string-suffix-p ": " prompt))
                                prompt
                              (substring prompt 0 (- (length prompt) 2)))
                            (selected-frame)
                            (or dir (and default-filename
                                         (file-name-directory default-filename)))
                            mustmatch only-dir-p
                            (file-name-nondirectory default-filename))
    (error "x-file-dialog on a tty frame")))

(defun haiku-dnd-handle-drag-n-drop-event (event)
  "Handle specified drag-n-drop EVENT."
  (interactive "e")
  (let* ((string (caddr event))
	 (window (posn-window (event-start event))))
    (cond
     ((assoc "refs" string)
      (with-selected-window window
        (raise-frame)
        (dolist (filename (cddr (assoc "refs" string)))
          (dnd-handle-one-url window 'private
                              (concat "file:" filename)))))
     ((assoc "text/plain" string)
      (with-selected-window window
        (raise-frame)
        (dolist (text (cddr (assoc "text/plain" string)))
          (goto-char (posn-point (event-start event)))
          (dnd-insert-text window 'private
                           (if (multibyte-string-p text)
                               text
                             (decode-coding-string text 'undecided))))))
     (t (message "Don't know how to drop any of: %s" (mapcar #'car string))))))

(define-key special-event-map [drag-n-drop]
            'haiku-dnd-handle-drag-n-drop-event)

(defvaralias 'haiku-use-system-tooltips 'use-system-tooltips)

(defun haiku-use-system-tooltips-watcher (&rest _ignored)
  "Variable watcher to force a menu bar update when `use-system-tooltip' changes.
This is necessary because on Haiku `use-system-tooltip' doesn't
take effect on menu items until the menu bar is updated again."
  (force-mode-line-update t))

(defun x-begin-drag (targets &optional action frame _return-frame)
  "SKIP: real doc in xfns.c."
  (unless haiku-dnd-selection-value
    (error "No local value for XdndSelection"))
  (let ((message nil))
    (dolist (target targets)
      (let ((selection-converter (cdr (assoc (intern target)
                                             haiku-dnd-selection-converters))))
        (when selection-converter
          (let ((selection-result
                 (funcall selection-converter
                          haiku-dnd-selection-value)))
            (when selection-result
              (let ((field (cdr (assoc (car selection-result) message))))
                (unless (cadr field)
                  ;; Add B_MIME_TYPE to the message if the type was not
                  ;; previously specified, or the type if it was.
                  (push (or (get-text-property 0 'type
                                               (cadr selection-result))
                            1296649541)
                        (alist-get (car selection-result) message
                                   nil nil #'equal))))
              (push (cadr selection-result)
                    (cdr (alist-get (car selection-result) message
                                    nil nil #'equal))))))))
    (prog1 (or action 'XdndActionCopy)
      (haiku-drag-message (or frame (selected-frame))
                          message))))

(add-variable-watcher 'use-system-tooltips #'haiku-use-system-tooltips-watcher)

(provide 'haiku-win)
(provide 'term/haiku-win)

;;; haiku-win.el ends here
