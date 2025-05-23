;;; find-func.el --- find the definition of the Emacs Lisp function near point  -*- lexical-binding:t -*-

;; Copyright (C) 1997, 1999, 2001-2024 Free Software Foundation, Inc.

;; Author: Jens Petersen <petersen@kurims.kyoto-u.ac.jp>
;; Keywords: emacs-lisp, functions, variables
;; Created: 97/07/25

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
;; The funniest thing about this is that I can't imagine why a package
;; so obviously useful as this hasn't been written before!!
;; ;;; find-func
;; (find-function-mode 1)
;;
;; or just:
;;
;; (load "find-func")
;;
;; if you don't like the given keybindings and away you go!  It does
;; pretty much what you would expect, putting the cursor at the
;; definition of the function or variable at point.
;;
;; The code started out from `describe-function', `describe-key'
;; ("help.el") and `fff-find-loaded-emacs-lisp-function' (Noah Friedman's
;; "fff.el").

;;; Code:

(eval-when-compile (require 'cl-lib))
(declare-function cl-search "cl-seq")

;;; User variables:

(defgroup find-function nil
  "Finds the definition of the Emacs Lisp symbol near point."
;;   :prefix "find-function"
  :group 'lisp)

(defconst find-function-space-re "\\(?:\\s-\\|\n\\|;.*\n\\)+")

(defcustom find-function-prefer-source-directory t
  "Prefer file in `source-directory' corresponding to target."
  :type 'boolean
  :version "29.1"
  :group 'find-function)

(defcustom find-function-regexp
  ;; Match things like (defun foo ...), (defmacro foo ...),
  ;; (define-skeleton foo ...), (define-generic-mode 'foo ...),
  ;;  (define-derived-mode foo ...), (define-minor-mode foo)
  (concat
   "^\\s-*(\\(def\\(ine-skeleton\\|ine-generic-mode\\|ine-derived-mode\\|\
ine\\(?:-global\\)?-minor-mode\\|ine-compilation-mode\\|un-cvs-mode\\|\
foo\\|\\(?:[^icfgv]\\|g[^r]\\)\\(\\w\\|\\s_\\)+\\*?\\)\\|easy-mmode-define-[a-z-]+\\|easy-menu-define\\|\
cl-\\(?:defun\\|defmethod\\|defgeneric\\)\\|\
transient-define-\\(?:prefix\\|suffix\\|infix\\|argument\\)\\|\
menu-bar-make-toggle\\|menu-bar-make-toggle-command\\)"
   find-function-space-re
   "\\('\\|(quote \\)?%s\\(\\s-\\|$\\|[()]\\)")
  "The regexp used by `find-function' to search for a function definition.
Note it must contain a `%s' at the place where `format'
should insert the function name.  The default value avoids `defconst',
`defgroup', `defvar', `defface'.

Please send improvements and fixes to the maintainer."
  :type 'regexp
  :group 'find-function
  :version "21.1")

(defcustom find-variable-regexp
  (concat
   "^\\s-*(\\(def[^fumag]\\(\\w\\|\\s_\\)+\\*?\\|\
easy-mmode-def\\(map\\|syntax\\)\\|easy-menu-define\\)"
   find-function-space-re
   "%s\\(\\s-\\|$\\)")
  "The regexp used by `find-variable' to search for a variable definition.
Note it must contain a `%s' at the place where `format'
should insert the variable name.  The default value
avoids `defun', `defmacro', `defalias', `defadvice', `defgroup', `defface'.

Please send improvements and fixes to the maintainer."
  :type 'regexp
  :group 'find-function
  :version "21.1")

(defcustom find-face-regexp
  (concat"^\\s-*(defface" find-function-space-re "%s\\(\\s-\\|$\\)")
  "The regexp used by `find-face' to search for a face definition.
Note it must contain a `%s' at the place where `format'
should insert the face name.

Please send improvements and fixes to the maintainer."
  :type 'regexp
  :group 'find-function
  :version "22.1")

(defcustom find-feature-regexp
  (concat ";;; Code:")
  "Regexp used by `xref-find-definitions' when searching for a feature definition.
Note it may contain up to one `%s' at the place where `format'
should insert the feature name."
  ;; We search for ";;; Code" rather than (feature '%s) because the
  ;; former is near the start of the code, and the latter is very
  ;; uninteresting. If the regexp is not found, just goes to
  ;; (point-min), which is acceptable in this case.
  :type 'regexp
  :group 'xref
  :version "25.1")

(defcustom find-alias-regexp
  "(defalias +'%s"
  "The regexp used by `xref-find-definitions' to search for an alias definition.
Note it must contain a `%s' at the place where `format'
should insert the feature name."
  :type 'regexp
  :group 'xref
  :version "25.1")

(defcustom find-ert-deftest-regexp
  "(ert-deftest +'%s"
  "The regexp used to search for an `ert-deftest' definition.
Note it must contain a `%s' at the place where `format'
should insert the feature name."
  :type 'regexp
  :group 'xref
  :version "29.1")

(defun find-function--defface (symbol)
  (catch 'found
    (while (re-search-forward (format find-face-regexp symbol) nil t)
      (unless (ppss-comment-or-string-start
               (save-excursion (syntax-ppss (match-beginning 0))))
        ;; We're not in a comment or a string.
        (throw 'found t)))))

(defvar find-function-regexp-alist
  '((nil . find-function-regexp)
    (define-type . find-function-regexp)
    (defvar . find-variable-regexp)
    (defface . find-function--defface)
    (feature . find-feature-regexp)
    (defalias . find-alias-regexp)
    (ert-deftest . find-ert-deftest-regexp))
  "Alist mapping definition types into regexp variables.
Each regexp variable's value should actually be a format string
to be used to substitute the desired symbol name into the regexp.
Instead of regexp variable, types can be mapped to functions as well,
in which case the function is called with one argument (the object
we're looking for) and it should search for it.")
(put 'find-function-regexp-alist 'risky-local-variable t)

(define-obsolete-variable-alias 'find-function-source-path
  'find-library-source-path "28.1")
(defcustom find-library-source-path nil
  "The default list of directories where `find-library' searches.

If this variable is nil then `find-library' searches `load-path' by
default."
  :type '(repeat directory)
  :group 'find-function
  :version "28.1")

(defcustom find-function-recenter-line 1
  "The window line-number from which to start displaying a symbol definition.
A value of nil implies center the beginning of the definition.
See `find-function' and `find-variable'."
  :type '(choice (const :tag "Center" nil)
		 integer)
  :group 'find-function
  :version "20.3")

(defcustom find-function-after-hook nil
  "Hook run after finding symbol definition.

See the functions `find-function' and `find-variable'."
  :type 'hook
  :group 'find-function
  :version "20.3")

(defcustom find-library-include-other-files t
  "If non-nil, `read-library-name' will also include non-library files.
This affects commands like `read-library'.

If nil, only library files (i.e., \".el\" files) will be offered
for completion."
  :type 'boolean
  :version "29.1"
  :group 'find-function)

;;; Functions:

(defalias 'find-function-preferred-message
  (let (messaged-p)
    (lambda (file)
      "find-function result may not reflect loaded definition."
      (unless messaged-p
        (setq messaged-p t)
        (message "Showing result from %s" file)))))

(defun find-library-suffixes ()
  (let ((load-suffixes (get-load-suffixes))
        ret)
    (dolist (suffix '(".so" ".el") (nreverse ret))
      (when (member suffix load-suffixes)
        (dolist (ext load-file-rep-suffixes)
          (push (concat suffix ext) ret))))))

(defun find-library--load-name (library)
  (let ((name library))
    (dolist (dir load-path)
      (let ((rel (file-relative-name library dir)))
        (if (and (not (string-match "\\`\\.\\./" rel))
                 (< (length rel) (length name)))
            (setq name rel))))
    (unless (equal name library) name)))

(defun find-library-name (library)
  "Return the absolute file name of LIBRARY.
If LIBRARY ends in the byte-compiled extension .elc or the
native-compiled extension .eln, find the corresponding source
ending in .el."
  (when (string-match "\\.el\\([cn]\\(\\..*\\)?\\)\\'" library)
    (setq library (replace-match "" t t library)))
  (let ((result
         (or
          (locate-file library
                       (or find-library-source-path load-path)
                       (find-library-suffixes))
          (locate-file library
                       (or find-library-source-path load-path)
                       load-file-rep-suffixes)
          (when-let ((absolute-p (file-name-absolute-p library))
                     (rel (find-library--load-name library)))
            (or
             (locate-file rel
                          (or find-library-source-path load-path)
                          (find-library-suffixes))
             (locate-file rel
                          (or find-library-source-path load-path)
                          load-file-rep-suffixes)))
          (find-library--from-load-history library)
          (signal 'file-error (list "Can't find library" library)))))
    (when find-function-prefer-source-directory
      (when-let ((prefix (file-name-as-directory installed-directory))
                 ;; :end2 says PREFIX matches beginning of RESULT
                 (installed-p (cl-search prefix result
                                         :end2 (length prefix)))
                 (el-or-unsuffixed
                  (expand-file-name
                   (concat
                    (file-name-directory (cl-subseq result (length prefix)))
                    (file-name-nondirectory library))
                   source-directory))
                 (suffixed (locate-file
                            el-or-unsuffixed
                            '("")
                            (nconc (find-library-suffixes)
                                   load-file-rep-suffixes)))
                 (readable-p (file-readable-p suffixed)))
        (funcall (symbol-function 'find-function-preferred-message)
                 (setq result suffixed))))
    result))

(defun find-library--from-load-history (library)
  ;; In `load-history', the file may be ".elc", ".el", ".el.gz", and
  ;; LIBRARY may be "foo.el" or "foo".
  (let ((load-re
         (concat "\\(" (regexp-quote (file-name-sans-extension library)) "\\)"
                 (regexp-opt (get-load-suffixes)) "\\'"))
        (alist load-history)
        elt file found)
    (while (and alist (null found))
      (setq elt (car alist)
            alist (cdr alist)
            file (car elt)
            found (and (stringp file) (string-match load-re file)
                       (let ((dir (substring file 0 (match-beginning 1)))
                             (basename (match-string 1 file)))
                         (locate-file basename (list dir)
                                      (find-library-suffixes))))))
    found))

(defvar find-function-C-source-directory
  (let ((dir (expand-file-name "src" source-directory)))
    (when (file-accessible-directory-p dir) dir))
  "Directory where the C source files of Emacs can be found.
If nil, do not try to find the source code of functions and variables
defined in C.")

(declare-function ad-get-advice-info "advice" (function))

(defun find-function-advised-original (func)
  "Return the original function definition of an advised function FUNC.
If FUNC is not a symbol, return it.  Else, if it's not advised,
return the symbol's function definition."
  (or (and (symbolp func)
           (advice--cd*r (symbol-function func)))
      func))

(defun find-function-C-source (fun-or-var file type)
  "Find the source location where FUN-OR-VAR is defined in FILE.
TYPE should be nil to find a function, or `defvar' to find a variable."
  (let ((dir (or find-function-C-source-directory
                 (read-directory-name "Emacs C source dir: " nil nil t))))
    (setq file (expand-file-name file dir))
    (if (file-readable-p file)
        (unless find-function-C-source-directory
          (setq find-function-C-source-directory dir))
      (error "The C source file %s is not available"
             (file-name-nondirectory file))))
  (unless type
    ;; Either or both an alias and its target might be advised.
    (setq fun-or-var (find-function-advised-original
		      (indirect-function
		       (find-function-advised-original fun-or-var)))))
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (unless (re-search-forward
	     (if type
		 (concat "DEFVAR[A-Z_]*[ \t\n]*([ \t\n]*\""
			 (regexp-quote (symbol-name fun-or-var))
			 "\"")
	       (concat "DEFUN[ \t\n]*([ \t\n]*\""
		       (regexp-quote (subr-name (advice--cd*r fun-or-var)))
		       "\""))
	     nil t)
      (error "Can't find source for %s" fun-or-var))
    (cons (current-buffer) (match-beginning 0))))

;;;###autoload
(defun find-library (library)
  "Find the Emacs Lisp source of LIBRARY.

Interactively, prompt for LIBRARY using the one at or near point.

This function searches `find-library-source-path' if non-nil, and
`load-path' otherwise.

See the `find-library-include-other-files' user option for
customizing the candidate completions."
  (interactive (list (read-library-name)))
  (prog1
      (switch-to-buffer (find-file-noselect (find-library-name library)))
    (run-hooks 'find-function-after-hook)))

(defvar find-function--read-history-library nil)

;;;###autoload
(defun read-library-name ()
  "Read and return a library name, defaulting to the one near point.

A library name is the filename of an Emacs Lisp library located
in a directory under `load-path' (or `find-library-source-path',
if non-nil)."
  (let* ((dirs (or find-library-source-path load-path))
         (suffixes (find-library-suffixes))
         (def (if (eq (function-called-at-point) 'require)
                  ;; `function-called-at-point' may return 'require
                  ;; with `point' anywhere on this line.  So wrap the
                  ;; `save-excursion' below in a `condition-case' to
                  ;; avoid reporting a scan-error here.
                  (condition-case nil
                      (save-excursion
                        (backward-up-list)
                        (forward-char)
                        (forward-sexp 2)
                        (thing-at-point 'symbol))
                    (error nil))
                (thing-at-point 'symbol))))
    (if find-library-include-other-files
        (let ((table (apply-partially #'locate-file-completion-table
                                      dirs suffixes)))
          (when (and def (not (test-completion def table)))
            (setq def nil))
          (completing-read (format-prompt "Library name" def)
                           table nil nil nil
                           'find-function--read-history-library def))
      (let ((files (read-library-name--find-files dirs suffixes)))
        (when (and def (not (member def files)))
          (setq def nil))
        (completing-read (format-prompt "Library name" def)
                         files nil t nil
                         'find-function--read-history-library def)))))

(defun read-library-name--find-files (dirs suffixes)
  "Return a list of all files in DIRS that match SUFFIXES."
  (let ((files nil)
        (regexp (concat (regexp-opt suffixes) "\\'")))
    (dolist (dir dirs)
      (dolist (file (ignore-errors (directory-files dir nil regexp t)))
        (and (string-match regexp file)
             (push (substring file 0 (match-beginning 0)) files))))
    files))

;;;###autoload
(defun find-library-other-window (library)
  "Find the Emacs Lisp source of LIBRARY in another window.

See `find-library' for more details."
  (interactive (list (read-library-name)))
  (prog1
      (switch-to-buffer-other-window (find-file-noselect
                                      (find-library-name library)))
    (run-hooks 'find-function-after-hook)))

;;;###autoload
(defun find-library-other-frame (library)
  "Find the Emacs Lisp source of LIBRARY in another frame.

See `find-library' for more details."
  (interactive (list (read-library-name)))
  (prog1
      (switch-to-buffer-other-frame (find-file-noselect
                                     (find-library-name library)))
    (run-hooks 'find-function-after-hook)))

;;;###autoload
(defun find-function-search-for-symbol (symbol type library)
  "Find SYMBOL's defun, or other definitional TYPE, in LIBRARY.
When TYPE is a non-nil key in `find-function-regexp-alist',
interpret SYMBOL according to the corresponding definitional,
e.g., defvar or defface.

After visiting file LIBRARY, return a cons cell (BUFFER
. POS), where POS is the character position of the definition,
or nil if not found.

LIBRARY can be an absolute or relative file name."
  (unless library
    (error "Don't know where `%s' is defined" symbol))
  ;; Some functions are defined as part of the construct
  ;; that defines something else.
  (while (and (symbolp symbol) (get symbol 'definition-name))
    (setq symbol (get symbol 'definition-name)))
  (if (string-match "\\`src/\\(.*\\.\\(c\\|m\\)\\)\\'" library)
      (find-function-C-source symbol (match-string 1 library) type)
    (when (string-match "\\.el\\(c\\)\\'" library)
      (setq library (substring library 0 (match-beginning 1))))
    ;; Strip extension from .emacs.el to make sure symbol is searched in
    ;; .emacs too.
    (when (string-match "\\.emacs\\(.el\\)\\'" library)
      (setq library (substring library 0 (match-beginning 1))))
    (let ((filename (find-library-name library))
	  (regexp-symbol (cdr (assq type find-function-regexp-alist))))
      (cond ((not filename)
             (error "Could not find '%s'" library))
            ((not regexp-symbol)
             (error "No find-function regexp entry for '%s'" type))
            (t
             (with-current-buffer (find-file-noselect filename)
	       (let ((regexp (if (functionp regexp-symbol)
                                 regexp-symbol
                               (format (symbol-value regexp-symbol)
                                       ;; Entry for ` (backquote) macro in loaddefs.el,
                                       ;; (defalias (quote \`)..., has a \ but
                                       ;; (symbol-name symbol) doesn't.  Add an
                                       ;; optional \ to catch this.
                                       (concat "\\\\?"
                                               (regexp-quote (symbol-name symbol))))))
	             (case-fold-search))
                 (save-excursion
                   (save-restriction
                     (widen)
                     (with-syntax-table emacs-lisp-mode-syntax-table
                       (goto-char (point-min))
                       (if (if (functionp regexp)
                               (funcall regexp symbol)
                             (or (re-search-forward regexp nil t)
                                 ;; `regexp' matches definitions using known forms like
                                 ;; `defun', or `defvar'.  But some functions/variables
                                 ;; are defined using special macros (or functions), so
                                 ;; if `regexp' can't find the definition, we look for
                                 ;; something of the form "(SOMETHING <symbol> ...)".
                                 ;; This fails to distinguish function definitions from
                                 ;; variable declarations (or even uses thereof), but is
                                 ;; a good pragmatic fallback.
                                 (re-search-forward
                                  (concat "^([^ ]+" find-function-space-re "['(]?"
                                          (regexp-quote (symbol-name symbol))
                                          "\\_>")
                                  nil t)))
                           (progn
                             (beginning-of-line)
                             (cons (current-buffer) (point)))
                         ;; If the regexp search didn't find the location of
                         ;; the symbol (for example, because it is generated by
                         ;; a macro), try a slightly more expensive search that
                         ;; expands macros until it finds the symbol.
                         (cons (current-buffer)
                               (find-function--search-by-expanding-macros
                                (current-buffer) symbol type)))))))))))))

(defun find-function--try-macroexpand (form)
  "Try to macroexpand FORM in full or partially.
This is a best-effort operation in which if macroexpansion fails,
this function returns FORM as is."
  (ignore-errors
    (or
     (macroexpand-all form)
     (macroexpand-1 form)
     form)))

(defun find-function--any-subform-p (form pred)
  "Walk FORM and apply PRED to its subexpressions.
Return t if any PRED returns t."
  (cond
   ((not (consp form)) nil)
   ((funcall pred form) t)
   (t
    (let ((left-child (car form))
          (right-child (cdr form)))
      (or
       (find-function--any-subform-p left-child pred)
       (find-function--any-subform-p right-child pred))))))

(defun find-function--search-by-expanding-macros (buf symbol type)
  "Expand macros in BUF to search for the definition of SYMBOL of TYPE."
  (catch 'found
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let ((form (read (current-buffer)))
                    (expected-symbol-p
                      (lambda (form)
                        (cond
                         ((null type)
                          ;; Check if a given form is a `defalias' to
                          ;; SYM, the function name we are searching
                          ;; for.  All functions in Emacs Lisp
                          ;; ultimately expand to a `defalias' form
                          ;; after several steps of macroexpansion.
                          (and (eq (car-safe form) 'defalias)
                               (equal (car-safe (cdr form))
                                      `(quote ,symbol))))
                         ((eq type 'defvar)
                          ;; Variables generated by macros ultimately
                          ;; expand to `defvar'.
                          (and (eq (car-safe form) 'defvar)
                               (eq (car-safe (cdr form)) symbol)))
                         (t nil)))))
                (when (find-function--any-subform-p
                       (find-function--try-macroexpand form)
                       expected-symbol-p)
                  ;; We want to return the location at the beginning
                  ;; of the macro, so move back one sexp.
                  (throw 'found (progn (backward-sexp) (point))))))
          (end-of-file nil))))))

(defun find-function-library (function &optional lisp-only verbose)
  "Return the pair (ORIG-FUNCTION . LIBRARY) for FUNCTION.

ORIG-FUNCTION is the original name, after resolving aliases.
LIBRARY is an absolute file name, a relative
file name inside the C sources directory, or a name of an
autoloaded feature.

If ORIG-FUNCTION is a built-in function and LISP-ONLY is non-nil,
signal an error.

If VERBOSE is non-nil, and FUNCTION is an alias, display a
message about the whole chain of aliases."
  (let ((def (when (symbolp function)
               (or (fboundp function)
                   (signal 'void-function (list function)))
               (find-function-advised-original function)))
        aliases)
    ;; FIXME for completeness, it might be nice to print something like:
    ;; foo (which is advised), which is an alias for bar (which is advised).
    (while (and def (symbolp def))
      (or (eq def function)
          (not verbose)
          (setq aliases (if aliases
                            (concat aliases
                                    (format-message
                                     ", which is an alias for `%s'"
                                     (symbol-name def)))
                          (format-message "`%s' is an alias for `%s'"
                                          function (symbol-name def)))))
      (setq function (find-function-advised-original function)
            def (find-function-advised-original function)))
    (if aliases
        (message "%s" aliases))
    (cons function
          (cond
           ((autoloadp def) (nth 1 def))
           ((subr-primitive-p def)
            (if lisp-only
                (error "%s is a built-in function" function))
            (help-C-file-name def 'subr))
           ((symbol-file function 'defun))))))

;;;###autoload
(defun find-function-noselect (function &optional lisp-only)
  "Return a pair (BUFFER . POINT) pointing to the definition of FUNCTION.

Finds the source file containing the definition of FUNCTION
in a buffer and the point of the definition.  The buffer is
not selected.  If the function definition can't be found in
the buffer, returns (BUFFER).

If FUNCTION is a built-in function, this function normally
attempts to find it in the Emacs C sources; however, if LISP-ONLY
is non-nil, signal an error instead."
  (if (not function)
    (error "You didn't specify a function"))
  (let ((func-lib (find-function-library function lisp-only t)))
    (find-function-search-for-symbol (car func-lib) nil (cdr func-lib))))

(defvar find-function--read-history-function nil)
(defvar find-function--read-history-variable nil)
(defvar find-function--read-history-face nil)

(defun find-function-read (&optional type)
  "Read and return an interned symbol, defaulting to the one near point.

If TYPE is nil, insist on a symbol with a function definition.
Otherwise TYPE should be `defvar' or `defface'.
If TYPE is nil, defaults using `function-called-at-point',
otherwise uses `variable-at-point'."
  (let* ((symb1 (cond ((null type) (function-called-at-point))
                      ((eq type 'defvar) (variable-at-point))
                      ((eq type 'defface) (face-at-point t))
                      (t (variable-at-point t))))
         (symb  (unless (eq symb1 0) symb1))
         (predicate (cdr (assq type '((nil . fboundp)
                                      (defvar . boundp)
                                      (defface . facep)))))
         (prompt-type (cdr (assq type '((nil . "function")
                                        (defvar . "variable")
                                        (defface . "face")))))
         (enable-recursive-minibuffers t))
    (list (intern (completing-read
                   (format-prompt "Find %s" symb prompt-type)
                   obarray predicate
                   'lambda nil
                   (intern (format "find-function--read-history-%s" prompt-type))
                   (and symb (symbol-name symb)))))))

(defun find-function-do-it (symbol type switch-fn)
  "Find Emacs Lisp SYMBOL in a buffer and display it.
TYPE is nil to search for a function definition,
or else `defvar' or `defface'.

The variable `find-function-recenter-line' controls how
to recenter the display.  SWITCH-FN is the function to call
to display and select the buffer.
See also `find-function-after-hook'.

Set mark before moving, if the buffer already existed."
  (let* ((orig-point (point))
	(orig-buffers (buffer-list))
	(buffer-point (save-excursion
			(find-definition-noselect symbol type)))
	(new-buf (car buffer-point))
	(new-point (cdr buffer-point)))
    (when buffer-point
      (when (memq new-buf orig-buffers)
	(push-mark orig-point))
      (funcall switch-fn new-buf)
      (when new-point (goto-char new-point))
      (recenter find-function-recenter-line)
      (run-hooks 'find-function-after-hook))))

;;;###autoload
(defun find-function (function)
  "Find the definition of the FUNCTION near point.

Finds the source file containing the definition of the function
near point (selected by `function-called-at-point') in a buffer and
places point before the definition.
Set mark before moving, if the buffer already existed.

See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (find-function-read))
  (find-function-do-it function nil 'switch-to-buffer))

;;;###autoload
(defun find-function-other-window (function)
  "Find, in another window, the definition of FUNCTION near point.

See `find-function' for more details."
  (interactive (find-function-read))
  (find-function-do-it function nil 'switch-to-buffer-other-window))

;;;###autoload
(defun find-function-other-frame (function)
  "Find, in another frame, the definition of FUNCTION near point.

See `find-function' for more details."
  (interactive (find-function-read))
  (find-function-do-it function nil 'switch-to-buffer-other-frame))

;;;###autoload
(defun find-variable-noselect (variable &optional file)
  "Return a pair `(BUFFER . POINT)' pointing to the definition of VARIABLE.

Finds the library containing the definition of VARIABLE in a buffer and
the point of the definition.  The buffer is not selected.
If the variable's definition can't be found in the buffer, return (BUFFER)."
  (if variable
      (let ((library (or file
                         (symbol-file variable 'defvar)
                         (help-C-file-name variable 'var))))
        (find-function-search-for-symbol variable 'defvar library))
    (error "You didn't specify a variable")))

;;;###autoload
(defun find-variable (variable)
  "Find the definition of the VARIABLE at or before point.

Finds the library containing the definition of the variable
near point (selected by `variable-at-point') in a buffer and
places point before the definition.

Set mark before moving, if the buffer already existed.

See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (find-function-read 'defvar))
  (find-function-do-it variable 'defvar 'switch-to-buffer))

;;;###autoload
(defun find-variable-other-window (variable)
  "Find, in another window, the definition of VARIABLE near point.

See `find-variable' for more details."
  (interactive (find-function-read 'defvar))
  (find-function-do-it variable 'defvar 'switch-to-buffer-other-window))

;;;###autoload
(defun find-variable-other-frame (variable)
  "Find, in another frame, the definition of VARIABLE near point.

See `find-variable' for more details."
  (interactive (find-function-read 'defvar))
  (find-function-do-it variable 'defvar 'switch-to-buffer-other-frame))

;;;###autoload
(defun find-definition-noselect (symbol type &optional file)
  "Return a pair `(BUFFER . POINT)' pointing to the definition of SYMBOL.
If the definition can't be found in the buffer, return (BUFFER).
TYPE says what type of definition: nil for a function, `defvar' for a
variable, `defface' for a face.  This function does not switch to the
buffer nor display it."
  (cond
   ((not symbol)
    (error "You didn't specify a symbol"))
   ((null type)
    (find-function-noselect symbol))
   ((eq type 'defvar)
    (find-variable-noselect symbol file))
   (t
    (let ((library (or file (symbol-file symbol type))))
      (find-function-search-for-symbol symbol type library)))))

;; For symmetry, this should be called find-face; but some programs
;; assume that, if that name is defined, it means something else.
;;;###autoload
(defun find-face-definition (face)
  "Find the definition of FACE.  FACE defaults to the name near point.

Finds the Emacs Lisp library containing the definition of the face
near point (selected by `variable-at-point') in a buffer and
places point before the definition.

Set mark before moving, if the buffer already existed.

See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (find-function-read 'defface))
  (find-function-do-it face 'defface 'switch-to-buffer))

(defun find-function-on-key-do-it (key find-fn)
  "Find the function that KEY invokes.  KEY is a string.
Set mark before moving, if the buffer already existed.

FIND-FN is the function to call to navigate to the function."
  (let (defn)
    (save-excursion
      (let* ((event (and (eventp key) (aref key 0))) ; Null event OK below.
	     (start (event-start event))
	     (modifiers (event-modifiers event))
	     (window (and (or (memq 'click modifiers) (memq 'down modifiers)
			      (memq 'drag modifiers))
			  (posn-window start))))
	;; For a mouse button event, go to the button it applies to
	;; to get the right key bindings.  And go to the right place
	;; in case the keymap depends on where you clicked.
	(when (windowp window)
	  (set-buffer (window-buffer window))
	  (goto-char (posn-point start)))
	(setq defn (key-binding key))))
    (let ((key-desc (key-description key)))
      (if (or (null defn) (integerp defn))
	  (message "%s is unbound" key-desc)
	(if (consp defn)
	    (message "%s runs %s" key-desc (prin1-to-string defn))
	  (funcall find-fn defn))))))

;;;###autoload
(defun find-function-on-key (key)
  "Find the function that KEY invokes.  KEY is a string.
Set mark before moving, if the buffer already existed."
  (interactive "kFind function on key: ")
  (find-function-on-key-do-it key #'find-function))

;;;###autoload
(defun find-function-on-key-other-window (key)
  "Find, in the other window, the function that KEY invokes.
See `find-function-on-key'."
  (interactive "kFind function on key: ")
  (find-function-on-key-do-it key #'find-function-other-window))

;;;###autoload
(defun find-function-on-key-other-frame (key)
  "Find, in the other frame, the function that KEY invokes.
See `find-function-on-key'."
  (interactive "kFind function on key: ")
  (find-function-on-key-do-it key #'find-function-other-frame))

;;;###autoload
(defun find-function-at-point ()
  "Find directly the function at point in the other window."
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function-other-window symb))))

;;;###autoload
(defun find-variable-at-point ()
  "Find directly the variable at point in the other window."
  (interactive)
  (let ((symb (variable-at-point)))
    (when (and symb (not (equal symb 0)))
      (find-variable-other-window symb))))

;;;###autoload
(define-minor-mode find-function-mode
  "Enable some key bindings for the `find-function' family of functions."
  :group 'find-function :version "31.1" :global t :lighter nil
  ;; For compatibility with the historical behavior of the old
  ;; `find-function-setup-keys', define our bindings at the precedence
  ;; level of the global map.
  :keymap nil
  (pcase-dolist (`(,map ,key ,cmd)
                 `((,ctl-x-map   "F" find-function)
                   (,ctl-x-4-map "F" find-function-other-window)
                   (,ctl-x-5-map "F" find-function-other-frame)
                   (,ctl-x-map   "K" find-function-on-key)
                   (,ctl-x-4-map "K" find-function-on-key-other-window)
                   (,ctl-x-5-map "K" find-function-on-key-other-frame)
                   (,ctl-x-map   "V" find-variable)
                   (,ctl-x-4-map "V" find-variable-other-window)
                   (,ctl-x-5-map "V" find-variable-other-frame)
                   (,ctl-x-map   "L" find-library)
                   (,ctl-x-4-map "L" find-library-other-window)
                   (,ctl-x-5-map "L" find-library-other-frame)))
    (if find-function-mode
        (keymap-set map key cmd)
      (keymap-unset map key t))))

;;;###autoload
(defun find-function-setup-keys ()
  "Turn on `find-function-mode', which see."
  (find-function-mode 1))
(make-obsolete 'find-function-setup-keys 'find-function-mode "31.1")

(provide 'find-func)

;;; find-func.el ends here
