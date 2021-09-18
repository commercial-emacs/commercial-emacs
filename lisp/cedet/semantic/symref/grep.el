;;; semantic/symref/grep.el --- Symref implementation using find/grep  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2021 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;;
;; Implement the symref tool API using the external tools find/grep.
;;
;; The symref GREP tool uses grep in a project to find symbol references.
;; This is a lowest-common-denominator tool with sucky performance that
;; can be used in small projects to find symbol references.

(require 'semantic/symref)
(require 'grep)

;;; Code:

;;; GREP
;;;###autoload
(defclass semantic-symref-tool-grep (semantic-symref-tool-baseclass)
  (
   )
  "A symref tool implementation using grep.
This tool uses EDE to find the root of the project, then executes
`find-grep' in the project.  The output is parsed for hits and
those hits returned.")

(defvar semantic-symref-filepattern-alist
  '((c-mode "*.[ch]")
    (c++-mode "*.[chCH]" "*.[ch]pp" "*.cc" "*.hh")
    (html-mode "*.html" "*.shtml" "*.php")
    (mhtml-mode "*.html" "*.shtml" "*.php") ; FIXME: remove
                                            ; duplication of
                                            ; HTML-related patterns.
                                            ; Maybe they belong in the
                                            ; major mode definition?
    (ruby-mode "*.r[bu]" "*.rake" "*.gemspec" "*.erb" "*.haml"
               "Rakefile" "Thorfile" "Capfile" "Guardfile" "Vagrantfile")
    (python-mode "*.py" "*.pyi" "*.pyw")
    (perl-mode "*.pl" "*.PL")
    (cperl-mode "*.pl" "*.PL")
    (lisp-interaction-mode "*.el" "*.ede" ".emacs" "_emacs")
    )
  "List of major modes and file extension pattern.
See find -name man page for format.")

(defun semantic-symref-derive-find-filepatterns (&optional mode)
  ;; FIXME: This should be moved to grep.el, where it could be used
  ;; for "C-u M-x grep" as well.
  "Derive a list of file (glob) patterns for the current buffer.
Looks first in `semantic-symref-filepattern-alist'.  If it is not
there, it then looks in `auto-mode-alist', and attempts to derive something
from that.
Optional argument MODE specifies the `major-mode' to test."
  ;; First, try the filepattern alist.
  (let* ((mode (or mode major-mode))
	 (pat (cdr (assoc mode semantic-symref-filepattern-alist))))
    (when (not pat)
      ;; No hit, try auto-mode-alist.
      (dolist (X auto-mode-alist)
	(when (and (eq (cdr X) mode)
                   ;; Only take in simple patterns, so try to convert this one.
                   (string-match "\\\\\\.\\([^\\'>]+\\)\\\\'" (car X)))
          (push (concat "*." (match-string 1 (car X))) pat))))
    pat))

(defvar semantic-symref-grep-flags)

(defvar semantic-symref-grep-expand-keywords
  (condition-case nil
      (let* ((kw (copy-alist grep-expand-keywords))
             (C (assoc "<C>" kw)))
        (setcdr C 'semantic-symref-grep-flags)
        kw)
    (error nil))
  "Grep expand keywords used when expanding templates for symref.")

(defun semantic-symref-grep-use-template (rootdir filepattern flags pattern)
  "Use the grep template expand feature to create a grep command.
ROOTDIR is the root location to run the `find' from.
FILEPATTERN is a string representing find flags for searching file patterns.
FLAGS are flags passed to Grep, such as -n or -l.
PATTERN is the pattern used by Grep."
  ;; We have grep-compute-defaults.  Let's use it.
  (grep-compute-defaults)
  (let* ((semantic-symref-grep-flags flags)
         (grep-expand-keywords semantic-symref-grep-expand-keywords)
	 (cmd (grep-expand-template
               (if (memq system-type '(windows-nt ms-dos))
                   ;; FIXME: Is this still needed?
                   ;; grep-find uses '--color=always' on MS-Windows
                   ;; because it wants the colorized output, to show
                   ;; it to the user.  By contrast, here we don't show
                   ;; the output, and the SGR escapes get in the way
                   ;; of parsing the output.
                   (replace-regexp-in-string "--color=always" ""
                                             grep-find-template t t)
                 grep-find-template)
               pattern
               filepattern
               rootdir)))
    cmd))

(defcustom semantic-symref-grep-shell shell-file-name
  "The shell command to use for executing find/grep.
This shell should support pipe redirect syntax."
  :group 'semantic
  :type 'string)

(defcustom semantic-symref-grep-tool 'auto-detect
  "The tool to use for searching in files."
  :type '(choice (const :tag "Auto-detect" auto-detect)
                 (const :tag "Ripgrep" ripgrep)
                 (const :tag "find + grep" find-grep))
  :group 'semantic)

(defcustom semantic-symref-grep-ripgrep-command "rg"
  "Name of the ripgrep command to use."
  :type 'file
  :group 'semantic)

(defun semantic-symref-grep--auto-detect-tool ()
  (let ((have-rg
         (with-temp-buffer
           (let ((hello-file (grep-hello-file)))
             (let ((process-file-side-effects nil))
               (unwind-protect
                   (eql (ignore-errors
                          (process-file
                           semantic-symref-grep-ripgrep-command
                           nil t nil "^Copyright"
                           (file-local-name hello-file)))
                        0)
                 (when (file-remote-p hello-file)
                   (delete-file hello-file))))))))
    (if have-rg
        'ripgrep
      'find-grep)))

(defun semantic-symref-grep--quote-extended (string)
  "Quote STRING as an extended-syntax regexp."
  (replace-regexp-in-string (rx (in ".^$*+?|{}[]()|\\"))
                            (lambda (s) (concat "\\" s))
                            string nil t))

(defun semantic-symref-grep--command (rootdir filepatterns file-name-only
                                      pattern-type pattern)
  "Compute search command to run.
ROOTDIR is the root of the tree to search.  FILEPATTERNS is a
list of glob patterns that match the files to search.
If FILE-NAME-ONLY is non-nil, the search only wants the names of files.
PATTERN-TYPE indicates the type of PATTERN:
 `regexp'     -- PATTERN is an extended (egrep) regexp.
 `symbol'     -- PATTERN is a literal identifier.

Return the command and arguments as a list of strings."
  (when (eq semantic-symref-grep-tool 'auto-detect)
    (setq semantic-symref-grep-tool
          (semantic-symref-grep--auto-detect-tool)))
  (cond
   ((eq semantic-symref-grep-tool 'ripgrep)
    (let ((filepat-args
           (mapcan (lambda (s) (list "-g" s)) filepatterns))
          (flags (cond (file-name-only "-l")
                       ((eq pattern-type 'symbol) "-nw")
                       (t "-n")))
          (pat-arg (if (eq pattern-type 'symbol)
                       (semantic-symref-grep--quote-extended pattern)
                     pattern))
          (dir (expand-file-name rootdir)))
      `(,semantic-symref-grep-ripgrep-command
        ,@filepat-args ,flags "-e" ,pat-arg ,dir)))
   (t
    (let* ((filepat-args
            ;; Convert the list into some find-flags.
            (let ((args `("-name" ,(car filepatterns))))
              (if (cdr filepatterns)
                  `("(" ,@args
                    ,@(mapcan (lambda (s) (list "-o" "-name" s))
                              (cdr filepatterns))
                    ")")
                args)))
           (filepattern (mapconcat #'shell-quote-argument filepat-args " "))
           (grepflags (cond (file-name-only "-l ")
                            ((eq pattern-type 'regexp) "-nE ")
                            (t "-nwE ")))
           (pat-arg (if (eq pattern-type 'symbol)
                        (semantic-symref-grep--quote-extended pattern)
                      pattern))
           (cmd (semantic-symref-grep-use-template
                 (directory-file-name (file-local-name rootdir))
                 filepattern grepflags pat-arg)))
      `(,semantic-symref-grep-shell ,shell-command-switch ,cmd)))))

(cl-defmethod semantic-symref-perform-search ((tool semantic-symref-tool-grep))
  "Perform a search with Grep."
  ;; Grep doesn't support some types of searches.
  (let ((st (oref tool searchtype)))
    (when (not (memq st '(symbol regexp)))
      (error "Symref impl GREP does not support searchtype of %s" st))
    )
  ;; Find the root of the project, and do a find-grep...
  (let* (;; Find the file patterns to use.
	 (rootdir (semantic-symref-calculate-rootdir))
	 (filepatterns (semantic-symref-derive-find-filepatterns))
         (greppat (oref tool searchfor))
         (file-names-only (eq (oref tool resulttype) 'file))
         (search-type (oref tool searchtype))
         (command (semantic-symref-grep--command
                   rootdir filepatterns file-names-only search-type greppat))
	 (b (get-buffer-create "*Semantic SymRef*")))

    (with-current-buffer b
      (erase-buffer)
      (setq default-directory rootdir)
      (apply #'process-file (car command) nil b nil (cdr command)))
    (semantic-symref-parse-tool-output tool b)))

(cl-defmethod semantic-symref-parse-tool-output-one-line ((tool semantic-symref-tool-grep))
  "Parse one line of grep output, and return it as a match list.
Moves cursor to end of the match."
  (pcase-let
      ((`(,grep-re ,file-group ,line-group . ,_) (car grep-regexp-alist)))
    (cond ((eq (oref tool resulttype) 'file)
	   ;; Search for files
	   (when (re-search-forward "^\\([^\n]+\\)$" nil t)
	     (match-string 1)))
          ((eq (oref tool resulttype) 'line-and-text)
           (when (re-search-forward grep-re nil t)
             (list (string-to-number (match-string line-group))
                   (match-string file-group)
                   (buffer-substring-no-properties (point) (line-end-position)))))
	  (t
	   (when (re-search-forward grep-re nil t)
	     (cons (string-to-number (match-string line-group))
		   (match-string file-group))
	     )))))

(provide 'semantic/symref/grep)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/symref/grep"
;; End:

;;; semantic/symref/grep.el ends here
