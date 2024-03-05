;;; ediff-hook.el --- setup for Ediff's menus and autoloads  -*- lexical-binding:t -*-

;; Copyright (C) 1995-2024 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: ediff

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

;;;   These must be placed in menu-bar.el in Emacs
;;
;;      (define-key menu-bar-tools-menu [ediff-misc]
;;	'("Ediff Miscellanea" . menu-bar-ediff-misc-menu))
;;      (define-key menu-bar-tools-menu [epatch]
;;	'("Apply Patch" . menu-bar-epatch-menu))
;;      (define-key menu-bar-tools-menu [ediff-merge]
;;	'("Merge" . menu-bar-ediff-merge-menu))
;;      (define-key menu-bar-tools-menu [ediff]
;;	'("Compare" . menu-bar-ediff-menu))

;; allow menus to be set up without ediff-wind.el being loaded

;; initialize menu bar keymaps
(defvar menu-bar-ediff-misc-menu
  (make-sparse-keymap "Ediff Miscellanea"))
(fset 'menu-bar-ediff-misc-menu
      menu-bar-ediff-misc-menu)
(defvar-keymap menu-bar-epatch-menu :name "Apply Patch")
(fset 'menu-bar-epatch-menu menu-bar-epatch-menu)
(defvar-keymap menu-bar-ediff-merge-menu :name "Merge")
(fset 'menu-bar-ediff-merge-menu
      menu-bar-ediff-merge-menu)
(defvar-keymap menu-bar-ediff-menu :name "Compare")
(fset 'menu-bar-ediff-menu menu-bar-ediff-menu)

;; define ediff compare menu
(define-key menu-bar-ediff-menu [ediff-misc]
  `(menu-item ,(purify-if-dumping "Ediff Miscellanea") menu-bar-ediff-misc-menu))
(define-key menu-bar-ediff-menu [separator-ediff-misc] menu-bar-separator)
(define-key menu-bar-ediff-menu [window]
  `(menu-item ,(purify-if-dumping "This Window and Next Window") compare-windows
	      :help ,(purify-if-dumping "Compare the current window and the next window")))
(define-key menu-bar-ediff-menu [ediff-windows-linewise]
  `(menu-item ,(purify-if-dumping "Windows Line-by-line...") ediff-windows-linewise
	      :help ,(purify-if-dumping "Compare windows line-wise")))
(define-key menu-bar-ediff-menu [ediff-windows-wordwise]
  `(menu-item ,(purify-if-dumping "Windows Word-by-word...") ediff-windows-wordwise
	      :help ,(purify-if-dumping "Compare windows word-wise")))
(define-key menu-bar-ediff-menu [separator-ediff-windows] menu-bar-separator)
(define-key menu-bar-ediff-menu [ediff-regions-linewise]
  `(menu-item ,(purify-if-dumping "Regions Line-by-line...") ediff-regions-linewise
	      :help ,(purify-if-dumping "Compare regions line-wise")))
(define-key menu-bar-ediff-menu [ediff-regions-wordwise]
  `(menu-item ,(purify-if-dumping "Regions Word-by-word...") ediff-regions-wordwise
	      :help ,(purify-if-dumping "Compare regions word-wise")))
(define-key menu-bar-ediff-menu [separator-ediff-regions] menu-bar-separator)
(define-key menu-bar-ediff-menu [ediff-dir-revision]
  `(menu-item ,(purify-if-dumping "Directory Revisions...") ediff-directory-revisions
	      :help ,(purify-if-dumping "Compare directory files with their older versions")))
(define-key menu-bar-ediff-menu [ediff-revision]
  `(menu-item ,(purify-if-dumping "File with Revision...") ediff-revision
	      :help ,(purify-if-dumping "Compare file with its older versions")))
(define-key menu-bar-ediff-menu [separator-ediff-directories] menu-bar-separator)
(define-key menu-bar-ediff-menu [ediff-directories3]
  `(menu-item ,(purify-if-dumping "Three Directories...") ediff-directories3
	      :help ,(purify-if-dumping "Compare files common to three directories simultaneously")))
(define-key menu-bar-ediff-menu [ediff-directories]
  `(menu-item ,(purify-if-dumping "Two Directories...") ediff-directories
	      :help ,(purify-if-dumping "Compare files common to two directories simultaneously")))
(define-key menu-bar-ediff-menu [separator-ediff-files] menu-bar-separator)
(define-key menu-bar-ediff-menu [ediff-buffers3]
  `(menu-item ,(purify-if-dumping "Three Buffers...") ediff-buffers3
	      :help ,(purify-if-dumping "Compare three buffers simultaneously")))
(define-key menu-bar-ediff-menu [ediff-files3]
  `(menu-item ,(purify-if-dumping "Three Files...") ediff-files3
	      :help ,(purify-if-dumping "Compare three files simultaneously")))
(define-key menu-bar-ediff-menu [ediff-buffers]
  `(menu-item ,(purify-if-dumping "Two Buffers...") ediff-buffers
	      :help ,(purify-if-dumping "Compare two buffers simultaneously")))
(define-key menu-bar-ediff-menu [ediff-files]
  `(menu-item ,(purify-if-dumping "Two Files...") ediff-files
	      :help ,(purify-if-dumping "Compare two files simultaneously")))

;; define ediff merge menu
(define-key
  menu-bar-ediff-merge-menu [ediff-merge-dir-revisions-with-ancestor]
  `(menu-item ,(purify-if-dumping "Directory Revisions with Ancestor...")
              ediff-merge-directory-revisions-with-ancestor
              :help ,(purify-if-dumping "Merge versions of the files in the same directory by comparing the files with common ancestors")))
(define-key
  menu-bar-ediff-merge-menu [ediff-merge-dir-revisions]
  `(menu-item ,(purify-if-dumping "Directory Revisions...") ediff-merge-directory-revisions
              :help ,(purify-if-dumping "Merge versions of the files in the same directory (without using ancestor information)")))
(define-key
  menu-bar-ediff-merge-menu [ediff-merge-revisions-with-ancestor]
  `(menu-item ,(purify-if-dumping "Revisions with Ancestor...")
              ediff-merge-revisions-with-ancestor
              :help ,(purify-if-dumping "Merge versions of the same file by comparing them with a common ancestor")))
(define-key menu-bar-ediff-merge-menu [ediff-merge-revisions]
  `(menu-item ,(purify-if-dumping "Revisions...") ediff-merge-revisions
              :help ,(purify-if-dumping "Merge versions of the same file (without using ancestor information)")))
(define-key menu-bar-ediff-merge-menu [separator-ediff-merge] menu-bar-separator)
(define-key
  menu-bar-ediff-merge-menu [ediff-merge-directories-with-ancestor]
  `(menu-item ,(purify-if-dumping "Directories with Ancestor...")
              ediff-merge-directories-with-ancestor
              :help ,(purify-if-dumping "Merge files common to a pair of directories by comparing the files with common ancestors")))
(define-key menu-bar-ediff-merge-menu [ediff-merge-directories]
  `(menu-item ,(purify-if-dumping "Directories...") ediff-merge-directories
	      :help ,(purify-if-dumping "Merge files common to a pair of directories")))
(define-key
  menu-bar-ediff-merge-menu [separator-ediff-merge-dirs] menu-bar-separator)
(define-key
  menu-bar-ediff-merge-menu [ediff-merge-buffers-with-ancestor]
  `(menu-item ,(purify-if-dumping "Buffers with Ancestor...") ediff-merge-buffers-with-ancestor
              :help ,(purify-if-dumping "Merge buffers by comparing their contents with a common ancestor")))
(define-key menu-bar-ediff-merge-menu [ediff-merge-buffers]
  `(menu-item ,(purify-if-dumping "Buffers...") ediff-merge-buffers
              :help ,(purify-if-dumping "Merge buffers (without using ancestor information)")))
(define-key menu-bar-ediff-merge-menu [ediff-merge-files-with-ancestor]
  `(menu-item ,(purify-if-dumping "Files with Ancestor...") ediff-merge-files-with-ancestor
              :help ,(purify-if-dumping "Merge files by comparing them with a common ancestor")))
(define-key menu-bar-ediff-merge-menu [ediff-merge-files]
  `(menu-item ,(purify-if-dumping "Files...") ediff-merge-files
              :help ,(purify-if-dumping "Merge files (without using ancestor information)")))

;; define epatch menu
(define-key menu-bar-epatch-menu [ediff-patch-buffer]
  `(menu-item ,(purify-if-dumping "To a Buffer...") ediff-patch-buffer
              :help ,(purify-if-dumping "Apply a patch to the contents of a buffer")))
(define-key menu-bar-epatch-menu [ediff-patch-file]
  `(menu-item ,(purify-if-dumping "To a File...") ediff-patch-file
              :help ,(purify-if-dumping "Apply a patch to a file")))

;; define ediff miscellanea
(define-key menu-bar-ediff-misc-menu [emultiframe]
  `(menu-item ,(purify-if-dumping "Use separate control buffer frame")
              ediff-toggle-multiframe
              :help ,(purify-if-dumping "Switch between the single-frame presentation mode and the multi-frame mode")
              :button (:toggle . (eq (bound-and-true-p ediff-window-setup-function)
		                     #'ediff-setup-windows-multiframe))))
;; FIXME: Port XEmacs's toolbar support!
;; ["Use a toolbar with Ediff control buffer"
;;  ediff-toggle-use-toolbar
;;  :style toggle
;;  :selected (if (featurep 'ediff-tbar)
;;       	 (ediff-use-toolbar-p))]
(define-key menu-bar-ediff-misc-menu [eregistry]
  `(menu-item ,(purify-if-dumping "List Ediff Sessions") ediff-show-registry
	      :help ,(purify-if-dumping "List all active Ediff sessions; it is a convenient way to find and resume such a session")))
(define-key menu-bar-ediff-misc-menu [ediff-cust]
  `(menu-item ,(purify-if-dumping "Customize Ediff") ediff-customize
	      :help ,(purify-if-dumping "Change some of the parameters that govern the behavior of Ediff")))
(define-key menu-bar-ediff-misc-menu [ediff-doc]
  `(menu-item ,(purify-if-dumping "Ediff Manual") ediff-documentation
	      :help ,(purify-if-dumping "Bring up the Ediff manual")))

(provide 'ediff-hook)
;;; ediff-hook.el ends here
