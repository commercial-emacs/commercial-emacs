;;; font-lock.el --- Electric font lock mode  -*- lexical-binding:t -*-

;; Copyright (C) 1992-2023 Free Software Foundation, Inc.

;; Author: Jamie Zawinski
;;	Richard Stallman
;;	Stefan Monnier
;; Maintainer: emacs-devel@gnu.org
;; Keywords: languages, faces
;; Package: emacs

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

;; Font Lock is an archaic term from the lisp machine days
;; when keyboards contained a "Font Lock" key that constrained the font
;; just as "Caps Lock" constrained the case.
;;
;; In modern parlance, the term has come to mean syntax highlighting
;; although emacs users continue to use "fontification" as a synonym.
;;
;; Font Lock Mode defaults to a globalized minor mode, that is, all
;; buffers will attempt fontification using whatever
;; `font-lock-defaults' settings are buffer-locally defined.
;;
;; Font Lock Mode can be individually applied by toggling off
;; `global-font-lock-mode' and adding `font-lock-mode' to the major
;; mode's hook, e.g.,
;;
;;  (add-hook \\='c-mode-hook #\\='font-lock-mode)
;;
;;;; Three Text-Propertizing Passes
;;
;; The syntactic keyword pass addresses context-sensitive exceptions
;; to the one-character-one-role syntax table.
;;
;; The syntactic pass applies "face" text properties according to the
;; syntax table and any bespoke "syntax-table" text properties.  This
;; pass addresses stateful constructs like strings and comments.
;;
;; The keyword pass fontifies reserved words specified in
;; `font-lock-keywords'.

;;; Code:

(require 'syntax)

;; Define core `font-lock' group.
(defgroup font-lock '((jit-lock custom-group))
  "Font Lock mode text highlighting package."
  :link '(custom-manual :tag "Emacs Manual" "(emacs)Font Lock")
  :link '(custom-manual :tag "Elisp Manual" "(elisp)Font Lock Mode")
  :group 'faces)

(defgroup font-lock-faces nil
  "Faces for highlighting text."
  :prefix "font-lock-"
  :group 'font-lock)

(defgroup font-lock-extra-types nil
  "Extra mode-specific type names for highlighting declarations."
  :group 'font-lock)

;; User variables.
(make-obsolete-variable 'font-lock-maximum-size nil "24.1")

(defcustom font-lock-maximum-decoration t
  "Maximum decoration level for fontification.  Deprecated.
If nil, use the default decoration (typically the minimum available).
If t, use the maximum decoration available.
If a number, use that level of decoration (or if not available the maximum).
The higher the number, the more decoration is done.
If a list, each element should be a cons pair of the form (MAJOR-MODE . LEVEL),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . t) (c++-mode . 2) (t . 1))
means use the maximum decoration available for buffers in C mode, level 2
decoration for buffers in C++ mode, and level 1 decoration otherwise."
  :type '(choice (const :tag "default" nil)
		 (const :tag "maximum" t)
		 (integer :tag "level" 1)
		 (repeat :menu-tag "mode specific" :tag "mode specific"
			 :value ((t . t))
			 (cons :tag "Instance"
			       (radio :tag "Mode"
				      (const :tag "all" t)
				      (symbol :tag "name"))
			       (radio :tag "Decoration"
				      (const :tag "default" nil)
				      (const :tag "maximum" t)
				      (integer :tag "level" 1)))))
  :group 'font-lock)

(defcustom font-lock-ignore nil
  "Rules to selectively disable fontifications due to `font-lock-keywords'.
If non-nil, the value should be a list of condition sets of the form

  (SYMBOL CONDITION ...)

where:

 - SYMBOL is a symbol, usually a major or minor mode.  The subsequent
   CONDITIONs apply if SYMBOL is bound as variable and its value is non-nil.
   If SYMBOL is a symbol of a mode, that means the buffer has that mode
   enabled (for major modes, it means the buffer's major mode is derived
   from SYMBOL's mode).

 - Each CONDITION can be one of the following:
   - A symbol, typically a face.  It matches any element of
     `font-lock-keywords' that references the symbol.  The symbol is
     interpreted as a glob pattern; in particular, `*' matches
     everything, `?' matches any single character, and `[abcd]'
     matches one character from the set.
   - A string.  It matches any element of `font-lock-keywords' whose
     MATCHER is a regexp that matches the string.  This can be used to
     disable fontification of a particular programming keyword.
   - A form (pred FUNCTION).  It matches an element of `font-lock-keywords'
     if FUNCTION, when called with the element as the argument, returns
     non-nil.
   - A form (not CONDITION).  It matches if CONDITION doesn't.
   - A form (and CONDITION ...).  It matches if all the provided
     CONDITIONs match.
   - A form (or CONDITION ...).  It matches if at least one of the
     provided CONDITIONs matches.
   - A form (except CONDITIONs ...).  This can be used only at top level
     or inside an `or' clause.  It undoes the effect of previous
     matching CONDITIONs on the same level.

In each buffer, fontifications due to the elements of `font-lock-keywords'
that match at least one applicable CONDITION are disabled."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'font-lock
  :version "29.1")

(defcustom font-lock-verbose nil
  "If non-nil, means show status messages for buffer fontification.
If a number, only buffers greater than this size have fontification messages."
  :type '(choice (const :tag "never" nil)
		 (other :tag "always" t)
		 (integer :tag "size"))
  :group 'font-lock
  :version "24.1")

;; Originally these variable values were face names such as `bold' etc.
;; Now we create our own faces, but we keep these variables for compatibility
;; and they give users another mechanism for changing face appearance.
;; We now allow a FACENAME in `font-lock-keywords' to be any expression that
;; returns a face.  So the easiest thing is to continue using these variables,
;; rather than sometimes evalling FACENAME and sometimes not.  sm.

;; Note that in new code, in the vast majority of cases there is no
;; need to create variables that specify face names.  Simply using
;; faces directly is enough.  Font-lock is not a template to be
;; followed in this area.
(defvar font-lock-comment-face		'font-lock-comment-face
  "Face name to use for comments.")

(defvar font-lock-comment-delimiter-face 'font-lock-comment-delimiter-face
  "Face name to use for comment delimiters.")

(defvar font-lock-string-face		'font-lock-string-face
  "Face name to use for strings.")

(defvar font-lock-doc-face		'font-lock-doc-face
  "Face name to use for documentation.")

(defvar font-lock-doc-markup-face       'font-lock-doc-markup-face
  "Face name to use for documentation mark-up.")

(defvar font-lock-keyword-face		'font-lock-keyword-face
  "Face name to use for keywords.")

(defvar font-lock-builtin-face		'font-lock-builtin-face
  "Face name to use for builtins.")

(defvar font-lock-function-name-face	'font-lock-function-name-face
  "Face name to use for function names.")

(defvar font-lock-variable-name-face	'font-lock-variable-name-face
  "Face name to use for variable names.")

(defvar font-lock-type-face		'font-lock-type-face
  "Face name to use for type and class names.")

(defvar font-lock-constant-face		'font-lock-constant-face
  "Face name to use for constant and label names.")

(defvar font-lock-warning-face		'font-lock-warning-face
  "Face name to use for things that should stand out.")

(defvar font-lock-negation-char-face	'font-lock-negation-char-face
  "Face name to use for easy to overlook negation.
This can be an \"!\" or the \"n\" in \"ifndef\".")

(defvar font-lock-preprocessor-face	'font-lock-preprocessor-face
  "Face name to use for preprocessor directives.")

;; Fontification variables:

(defvar font-lock-keywords nil
  "A list of keywords and corresponding font-lock highlighting rules.
There are two kinds of values: user-level, and compiled.

A user-level keywords list is what a major mode or the user would
set up.  Normally the list would come from `font-lock-defaults'.
through selection of a fontification level and evaluation of any
contained expressions.  You can also alter it by calling
`font-lock-add-keywords' or `font-lock-remove-keywords' with MODE = nil.

Each element in a user-level keywords list should have one of these forms:

 MATCHER
 (MATCHER . SUBEXP)
 (MATCHER . FACENAME)
 (MATCHER . HIGHLIGHT)
 (MATCHER HIGHLIGHT ...)
 (eval . FORM)

where MATCHER can be either the regexp to search for, or the
function name to call to make the search (called with one
argument, the limit of the search; it should return non-nil, move
point, and set `match-data' appropriately if it succeeds; like
`re-search-forward' would).  MATCHER regexps can be generated via
the function `regexp-opt'.

FORM is an expression, whose value should be a keyword element
of one of the above forms, evaluated when the keyword is (first)
used in a buffer.  This feature can be used to provide a keyword
that can only be generated when Font Lock mode is actually turned on.

HIGHLIGHT should be either MATCH-HIGHLIGHT or MATCH-ANCHORED.

For highlighting single items, for example each instance of the
word \"foo\", typically only MATCH-HIGHLIGHT is required.
However, if an item or (typically) items are to be highlighted
following the instance of another item (the anchor), for example
each instance of the word \"bar\" following the word \"anchor\"
then MATCH-ANCHORED may be required.

MATCH-HIGHLIGHT should be of the form:

 (SUBEXP FACENAME [OVERRIDE [LAXMATCH]])

SUBEXP is the number of the subexpression of MATCHER to be
highlighted.

FACENAME is an expression whose value is the face to use.
Instead of a face, FACENAME can evaluate to a property list of
the form (face FACE PROP1 VAL1 PROP2 VAL2 ...)  in which case all
the listed text-properties will be set rather than just FACE.  In
such a case, you will most likely want to put those properties in
`font-lock-extra-managed-props' or to override
`font-lock-unfontify-region-function'.

OVERRIDE and LAXMATCH are flags.  If OVERRIDE is t, existing
fontification can be overwritten.  If `keep', only parts not
already fontified are highlighted.  If `prepend' or `append',
existing fontification is merged with the new, in which the new
or existing fontification, respectively, takes precedence.  If
LAXMATCH is non-nil, that means don't signal an error if there is
no match for SUBEXP in MATCHER.

For example, an element of the form highlights (if not already
highlighted):

 \"\\\\\\=<foo\\\\\\=>\"
  Discrete occurrences of \"foo\" in the value of the variable
  `font-lock-keyword-face'.

 (\"fu\\\\(bar\\\\)\" . 1)
  Substring \"bar\" within all occurrences of \"fubar\" in the
  value of `font-lock-keyword-face'.

 (\"fubar\" . fubar-face)
  Occurrences of \"fubar\" in the value of `fubar-face'.

 (\"foo\\\\|bar\" 0 foo-bar-face t)
  Occurrences of either \"foo\" or \"bar\" in the value of
  `foo-bar-face', even if already highlighted.

 (fubar-match 1 fubar-face)
  The first subexpression within all occurrences of whatever the
  function `fubar-match' finds and matches in the value of
  `fubar-face'.

MATCH-ANCHORED should be of the form:

 (MATCHER PRE-MATCH-FORM POST-MATCH-FORM MATCH-HIGHLIGHT ...)

where MATCHER is a regexp to search for or the function name to
call to make the search, as for MATCH-HIGHLIGHT above, but with
one exception; see below.  PRE-MATCH-FORM and POST-MATCH-FORM are
evaluated before the first, and after the last, instance
MATCH-ANCHORED's MATCHER is used.  Therefore they can be used to
initialize before, and cleanup after, MATCHER is used.
Typically, PRE-MATCH-FORM is used to move to some position
relative to the original MATCHER, before starting with
MATCH-ANCHORED's MATCHER.  POST-MATCH-FORM might be used to move
back, before resuming with MATCH-ANCHORED's parent's MATCHER.

For example, an element of the form highlights (if not already
highlighted):

 (\"\\\\\\=<anchor\\\\\\=>\" (0 anchor-face)
  (\"\\\\\\=<item\\\\\\=>\" nil nil (0 item-face)))

  Discrete occurrences of \"anchor\" in the value of
  `anchor-face', and subsequent discrete occurrences of
  \"item\" (on the same line) in the value of `item-face'.
  (Here PRE-MATCH-FORM and POST-MATCH-FORM are nil.  Therefore
  \"item\" is initially searched for starting from the end of the
  match of \"anchor\", and searching for subsequent instances of
  \"anchor\" resumes from where searching for \"item\" concluded.)

The above-mentioned exception is as follows.  The limit of the
MATCHER search defaults to the end of the line after
PRE-MATCH-FORM is evaluated.  However, if PRE-MATCH-FORM returns
a position greater than the position after PRE-MATCH-FORM is
evaluated, that position is used as the limit of the search.  It
is generally a bad idea to return a position greater than the end
of the line, i.e., cause the MATCHER search to span lines.

These regular expressions can match text which spans lines,
although it is better to avoid it if possible since updating them
while editing text is slower, and it is not guaranteed to be
always correct.

This variable is set by major modes via the variable
`font-lock-defaults'.  Be careful when composing regexps for this
list; a poorly written pattern can dramatically slow things down!

A compiled keywords list starts with t.  It is produced
internally by `font-lock-compile-keywords' from a user-level
keywords list.  Its second element is the user-level keywords
list that was compiled.  The remaining elements have the same
form as user-level keywords, but normally their values have been
optimized.")

(defvar font-lock-keywords-alist nil
  "Alist of additional `font-lock-keywords' elements for major modes.

Each element has the form (MODE KEYWORDS . HOW).
Function `font-lock-ensure-keywords' adds the elements in the list KEYWORDS to
`font-lock-keywords' when Font Lock is turned on in major mode MODE.

If HOW is nil, KEYWORDS are added at the beginning of
`font-lock-keywords'.  If it is `set', they are used to replace the
value of `font-lock-keywords'.  If HOW is any other non-nil value,
they are added at the end.

This is normally set via `font-lock-add-keywords' and
`font-lock-remove-keywords'.")
(put 'font-lock-keywords-alist 'risky-local-variable t)

(defvar font-lock-removed-keywords-alist nil
  "Alist of `font-lock-keywords' elements to be removed for major modes.

Each element has the form (MODE . KEYWORDS).  Function
`font-lock-ensure-keywords' removes the elements in the list
KEYWORDS from `font-lock-keywords' when Font Lock is turned on in
major mode MODE.

This is normally set via `font-lock-add-keywords' and
`font-lock-remove-keywords'.")

(defvar font-lock-keywords-only nil
  "Non-nil means Font Lock should not fontify comments or strings.
This is normally set via `font-lock-defaults'.")

(defvar-local font-lock-keywords-case-fold-search nil
  "Non-nil means the patterns in `font-lock-keywords' are case-insensitive.
This is set via the function `font-lock-ensure-keywords', based on
the CASE-FOLD argument of `font-lock-defaults'.")

(defvar-local font-lock-syntactically-fontified 0
  "Point up to which `font-lock-syntactic-keywords' has been applied.
If nil, this is ignored, in which case the syntactic fontification may
sometimes be slightly incorrect.")

(defvar font-lock-syntactic-face-function
  (lambda (state)
    (if (nth 3 state) font-lock-string-face font-lock-comment-face))
  "Function to determine which face to use when fontifying syntactically.
The function is called with a single parameter (the state as returned by
`parse-partial-sexp' at the beginning of the region to highlight) and
should return a face.  This is normally set via `font-lock-defaults'.")

(defvar font-lock-syntactic-keywords nil
  "A list of the syntactic keywords to put syntax properties on.
The value can be the list itself, or the name of a function or variable
whose value is the list.

See `font-lock-keywords' for a description of the form of this list;
only the differences are stated here.  MATCH-HIGHLIGHT should be of the form:

 (SUBEXP SYNTAX OVERRIDE LAXMATCH)

where SYNTAX can be a string (as taken by `modify-syntax-entry'), a syntax
table, a cons cell (as returned by `string-to-syntax') or an expression whose
value is such a form.  OVERRIDE cannot be `prepend' or `append'.

Here are two examples of elements of `font-lock-syntactic-keywords'
and what they do:

 (\"\\\\$\\\\(#\\\\)\" 1 \".\")

 gives a hash character punctuation syntax (\".\") when following a
 dollar-sign character.  Hash characters in other contexts will still
 follow whatever the syntax table says about the hash character.

 (\"\\\\(\\='\\\\).\\\\(\\='\\\\)\"
  (1 \"\\\"\")
  (2 \"\\\"\"))

 gives a pair of apostrophes, which surround a single character, a
 SYNTAX of \"\\\"\" (meaning string quote syntax).  Apostrophes in other

 contexts will not be affected.

This is normally set via `font-lock-defaults'.")
(make-obsolete-variable 'font-lock-syntactic-keywords
                        'syntax-propertize-function "24.1")

(defvar font-lock-syntax-table nil
  "Non-nil means use this syntax table for fontifying.
If this is nil, the major mode's syntax table is used.
This is normally set via `font-lock-defaults'.")
(defvar-local font-lock--syntax-table-affects-ppss nil)

(defvar font-lock-mark-block-function nil
  "Non-nil means use this function to mark a block of text.
When called with no args it should leave point at the beginning of any
enclosing textual block and mark at the end.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-fontify-buffer-function #'font-lock-default-fontify-buffer
  "Function to use for fontifying the buffer.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-unfontify-buffer-function #'font-lock-default-unfontify-buffer
  "Function to use for unfontifying the buffer.
This is used when turning off Font Lock mode.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-fontify-region-function #'font-lock-default-fontify-region
  "Function to use for fontifying a region.
It should take two args, the beginning and end of the region, and an optional
third arg VERBOSE.  If VERBOSE is non-nil, the function should print status
messages.  This is normally set via `font-lock-defaults'.
If it fontifies a larger region, it should ideally return a list of the form
\(jit-lock-bounds BEG . END) indicating the bounds of the region actually
fontified.")

(defvar font-lock-unfontify-region-function #'font-lock-default-unfontify-region
  "Function to use for unfontifying a region.
It should take two args, the beginning and end of the region.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-inhibit-thing-lock nil)
(make-obsolete-variable 'font-lock-inhibit-thing-lock "it does nothing." "25.1")

(defvar-local font-lock-multiline nil
  "Whether font-lock should cater to multiline keywords.
If nil, don't try to handle multiline patterns.
If t, always handle multiline patterns.
If `undecided', don't try to handle multiline patterns until you see one.
Major/minor modes can set this variable if they know which option applies.")

(defvar-local font-lock-fontified nil)	; Whether we have fontified the buffer.

;; Font Lock mode.

(defvar-local font-lock-keywords-set nil)

(defun font-lock-add-keywords (mode keywords &optional how)
  "Add highlighting KEYWORDS for MODE.

MODE can be a major mode symbol, such as \\='c-mode, or nil,
in which case keywords are added to the current buffer.

KEYWORDS is a list of the form laboriously described by
`font-lock-keywords'.  If HOW is the symbol \\='set, KEYWORDS
supplants the prevailing keywords list.  If HOW is any other
non-nil value, KEYWORDS are appended.  Otherwise KEYWORDS are
prepended.

For example:

 (font-lock-add-keywords \\='c-mode
  \\='((\"\\\\\\=<\\\\(FIXME\\\\):\" 1 \\='font-lock-warning-face prepend)
    (\"\\\\\\=<\\\\(and\\\\|or\\\\|not\\\\)\\\\\\=>\" . \\='font-lock-keyword-face)))

adds two fontification patterns to `c-mode', to fontify occurrences of
\"FIXME:\", even within comments, and to fontify \"and\", \"or\" and \"not\".

The above only adds keywords for the C major mode, not for its
derived modes.  To achieve this, modify `c-mode-hook' as follows:

 (add-hook \\='c-mode-hook
  (lambda ()
   (font-lock-add-keywords nil
    \\='((\"\\\\\\=<\\\\(FIXME\\\\):\" 1 \\='font-lock-warning-face prepend)
      (\"\\\\\\=<\\\\(and\\\\|or\\\\|not\\\\)\\\\\\=>\" .
       \\='font-lock-keyword-face)))))

Some modes inject specialized logic for additional patterns.  See
the `c-font-lock-extra-types', `c++-font-lock-extra-types',
`objc-font-lock-extra-types' and `java-font-lock-extra-types'."
  (if mode
      ;; Explicit mode
      (progn
        (let ((spec (cons keywords how)) cell)
	 (if (setq cell (assq mode font-lock-keywords-alist))
	     (if (eq how 'set)
		 (setcdr cell (list spec))
	       (setcdr cell (append (cdr cell) (list spec))))
	   (push (list mode spec) font-lock-keywords-alist)))
        ;; Ensure new keywords not in `font-lock-removed-keywords-alist'.
        (font-lock-update-removed-keyword-alist mode keywords how))
    ;; Implicit mode hack to compose derived modes; see docstring above.
    (font-lock-ensure-keywords)
    (let ((was-compiled (eq (car font-lock-keywords) t)))
      ;; Bring back the user-level (uncompiled) keywords.
      (if was-compiled
	  (setq font-lock-keywords (cadr font-lock-keywords)))
      ;; Now modify or replace them.
      (if (eq how 'set)
	  (setq font-lock-keywords keywords)
	(font-lock-remove-keywords nil keywords) ;to avoid duplicates
	(let ((old (if (eq (car-safe font-lock-keywords) t)
		       (cdr font-lock-keywords)
		     font-lock-keywords)))
	  (setq font-lock-keywords (if how
				       (append old keywords)
				     (append keywords old)))))
      ;; If the keywords were compiled before, compile them again.
      (if was-compiled
	  (setq font-lock-keywords
                (font-lock-compile-keywords font-lock-keywords))))))

(defun font-lock-update-removed-keyword-alist (mode keywords how)
  "Update `font-lock-removed-keywords-alist' when adding new KEYWORDS to MODE.
When font-lock is enabled first all keywords in the list
`font-lock-keywords-alist' are added, then all keywords in the
list `font-lock-removed-keywords-alist' are removed.  If a
keyword was once added, removed, and then added again it must be
removed from the removed-keywords list.  Otherwise the second add
will not take effect."
  (when-let ((cell (assq mode font-lock-removed-keywords-alist)))
    (if (eq how 'set)
	;; A new set of keywords is defined.  Forget all about
	;; our old keywords that should be removed.
	(setq font-lock-removed-keywords-alist
	      (delq cell font-lock-removed-keywords-alist))
      ;; Delete all previously removed keywords.
      (dolist (kword keywords)
	(setcdr cell (delete kword (cdr cell))))
      ;; Delete the mode cell if empty.
      (unless (cdr cell)
	(setq font-lock-removed-keywords-alist
	      (delq cell font-lock-removed-keywords-alist))))))

;; Case study (Anders Lindgren):
;; (I)  The keywords are removed from a major mode.
;;      In this case the keyword could be local (i.e. added earlier by
;;      `font-lock-add-keywords'), global, or both.
;;
;;      (a) In the local case we remove the keywords from the variable
;;          `font-lock-keywords-alist'.
;;
;;      (b) The actual global keywords are not known at this time.
;;          All keywords are added to `font-lock-removed-keywords-alist',
;;          when font-lock is enabled those keywords are removed.
;;
;;      Note that added keywords are taken out of the list of removed
;;      keywords.  This ensure correct operation when the same keyword
;;      is added and removed several times.
;;
;; (II) The keywords are removed from the current buffer.
(defun font-lock-remove-keywords (mode keywords)
  "Remove highlighting KEYWORDS for MODE.

MODE should be a symbol, the major mode command name, such as
`c-mode' or nil.  If nil, highlighting keywords are removed for
the current buffer.

For a description of KEYWORDS, see `font-lock-add-keywords'.

To make the removal apply to modes derived from MODE as well,
pass nil for MODE and add the call to MODE-hook.  This may fail
for some derived modes if some involved major mode does not
follow the standard conventions.  File a bug report if this
happens, so the major mode can be corrected."
  (cond (mode
	 ;; Remove one keyword at the time.
	 (dolist (keyword keywords)
	   (let ((top-cell (assq mode font-lock-keywords-alist)))
	     ;; If MODE is non-nil, remove the KEYWORD from
	     ;; `font-lock-keywords-alist'.
	     (when top-cell
	       (dolist (keyword-list-how-pair (cdr top-cell))
                 ;; `keyword-list-how-pair' is a cons with a list of
		 ;; keywords in the car top-cell and the original how
		 ;; argument in the cdr top-cell.
		 (setcar keyword-list-how-pair
			 (delete keyword (car keyword-list-how-pair))))
	       ;; Remove keyword list/how pair when the keyword list
	       ;; is empty and how doesn't specify `set'.  (If it
	       ;; should be deleted then previously deleted keywords
	       ;; would appear again.)
	       (let ((cell top-cell))
		 (while (cdr cell)
		   (if (and (null (car (car (cdr cell))))
			    (not (eq (cdr (car (cdr cell))) 'set)))
		       (setcdr cell (cdr (cdr cell)))
		     (setq cell (cdr cell)))))
	       ;; Final cleanup, remove major mode cell if last keyword
	       ;; was deleted.
	       (if (null (cdr top-cell))
		   (setq font-lock-keywords-alist
			 (delq top-cell font-lock-keywords-alist))))
	     ;; Remember the keyword in case it is not local.
	     (let ((cell (assq mode font-lock-removed-keywords-alist)))
	       (if cell
		   (unless (member keyword (cdr cell))
		     (nconc cell (list keyword)))
		 (push (cons mode (list keyword))
		       font-lock-removed-keywords-alist))))))
	(t
	 ;; Otherwise remove it immediately.
	 (font-lock-ensure-keywords)
	 (let ((was-compiled (eq (car font-lock-keywords) t)))
	   ;; Bring back the user-level (uncompiled) keywords.
	   (if was-compiled
	       (setq font-lock-keywords (cadr font-lock-keywords)))

	   ;; Edit them.
	   (setq font-lock-keywords (copy-sequence font-lock-keywords))
	   (dolist (keyword keywords)
	     (setq font-lock-keywords
		   (delete keyword font-lock-keywords)))

	   ;; If the keywords were compiled before, compile them again.
	   (if was-compiled
	       (setq font-lock-keywords
                     (font-lock-compile-keywords font-lock-keywords)))))))

(defcustom font-lock-support-mode 'jit-lock-mode
  "The default is \\='jit-lock-mode, a regexp-based vicinity fontifier.
If assigned \\='tree-sitter-lock-mode, use tree-sitter highlighting
if the major mode provides a grammar, otherwise fall back to
\\='jit-lock-mode."
  :type 'symbol
  :version "21.1"
  :group 'font-lock)

(defvar jit-lock-mode)
(defvar tree-sitter-lock-mode)

(declare-function tree-sitter-fontify-region "tree-sitter")
(declare-function tree-sitter-lock-mode "tree-sitter")

(defvar tree-sitter-mode-alist)
(defun font-lock-register ()
  (font-lock-ensure-keywords)
  (if (and (eq font-lock-support-mode 'tree-sitter-lock-mode)
           (assq major-mode tree-sitter-mode-alist))
      (tree-sitter-lock-mode t)
    (setq-local font-lock-fontify-buffer-function #'jit-lock-refontify
                font-lock-flush-function #'jit-lock-refontify
                font-lock-ensure-function #'jit-lock-fontify-now)
    (jit-lock-register #'font-lock-fontify-region (not font-lock-keywords-only))
    (add-hook 'jit-lock-after-change-extend-region-functions
              #'font-lock-extend-jit-lock-region-after-change
              nil t)))

(defun font-lock-deregister ()
  (font-lock-unfontify-buffer)
  (mapc #'kill-local-variable
        '(font-lock-fontify-buffer-function
          font-lock-fontify-region-function
          font-lock-flush-function
          font-lock-ensure-function))
  (cond ((bound-and-true-p tree-sitter-lock-mode)
         (tree-sitter-lock-mode -1))
        ((bound-and-true-p jit-lock-mode)
         (jit-lock-unregister 'font-lock-fontify-region)
         (remove-hook 'jit-lock-after-change-extend-region-functions
                      #'font-lock-extend-jit-lock-region-after-change
                      t))))

(defvar-local font-lock-extend-after-change-region-function nil
  "A function that determines the region to refontify after a change.

This variable is either nil, or is a function that determines the
region to refontify after a change.
It is usually set by the major mode via `font-lock-defaults'.
Font-lock calls this function after each buffer change.

The function is given three parameters, the standard BEG, END, and OLD-LEN
from `after-change-functions'.  It should return either a cons of the beginning
and end buffer positions \(in that order) of the region to refontify, or nil
\(which directs the caller to fontify a default region).
This function should preserve the match data.
The region it returns may start or end in the middle of a line.")

(defun font-lock-fontify-buffer (&optional interactively)
  "Fontify the current buffer the way the function `font-lock-mode' would."
  (declare
   ;; When called from Lisp, this function is a big mess.  The caller usually
   ;; expects one of the following behaviors:
   ;; - refresh the highlighting (because the font-lock-keywords have been
   ;;   changed).
   ;; - apply font-lock highlighting even if font-lock-mode is not enabled.
   ;; - reset the highlighting rules because font-lock-defaults
   ;;   has been changed (and then rehighlight everything).
   ;; Of course, this function doesn't do all of the above in all situations
   ;; (e.g. depending on whether jit-lock is in use) and it can't guess what
   ;; the caller wants.
   (interactive-only "use `font-lock-ensure' or `font-lock-flush' instead."))
  (interactive "p")
  (font-lock-ensure-keywords)
  (let ((font-lock-verbose (or font-lock-verbose interactively)))
    (funcall font-lock-fontify-buffer-function)))

(defun font-lock-unfontify-buffer ()
  (funcall font-lock-unfontify-buffer-function))

(defun font-lock-fontify-region (beg end &optional loudly)
  "Fontify the text between BEG and END.
If LOUDLY is non-nil, print status messages while fontifying.
This works by calling `font-lock-fontify-region-function'."
  (font-lock-ensure-keywords)
  (save-restriction
    (unless font-lock-dont-widen (widen))
    (funcall font-lock-fontify-region-function beg end loudly)))

(defun font-lock-unfontify-region (beg end)
  "Unfontify the text between BEG and END.
This works by calling `font-lock-unfontify-region-function'."
  (with-silent-modifications
    (funcall font-lock-unfontify-region-function beg end)))

(defvar font-lock-flush-function #'font-lock-after-change-function
  "Function to use to mark a region for refontification.
Called with two arguments BEG and END.")

(defun font-lock-flush (&optional beg end)
  "Declare the region BEG...END's fontification as out-of-date.
If the region is not specified, it defaults to the entire
accessible portion of the current buffer."
  (and font-lock-mode
       font-lock-fontified
       (funcall font-lock-flush-function
                (or beg (point-min)) (or end (point-max)))))

(defvar font-lock-ensure-function
  (lambda (beg end)
    (unless font-lock-fontified
      (save-excursion
        (font-lock-fontify-region beg end))))
  "Function to make sure a region has been fontified.
Called with two arguments BEG and END.")

(defun font-lock-ensure (&optional beg end)
  "Make sure the region BEG...END has been fontified.
If the region is not specified, it defaults to the entire accessible
portion of the buffer."
  (unless font-lock-mode
    (font-lock-mode))
  (funcall font-lock-ensure-function
           (or beg (point-min)) (or end (point-max)))
  (setq font-lock-fontified t))

(defun font-lock-update (&optional arg)
  "Update the syntax highlighting in this buffer.
Refontify the accessible portion of this buffer, or enable Font Lock mode
in this buffer if it is currently disabled.  With prefix ARG, toggle Font
Lock mode."
  (interactive "P")
  (save-excursion
    (if (and (not arg) font-lock-mode)
        (font-lock-fontify-region (point-min) (point-max))
      (font-lock-unfontify-region (point-min) (point-max))
      (font-lock-mode 'toggle))))

(defun font-lock-default-fontify-buffer ()
  "Fontify the whole buffer using `font-lock-fontify-region-function'."
  (let ((verbose (if (numberp font-lock-verbose)
		     (> (buffer-size) font-lock-verbose)
		   font-lock-verbose)))
    (with-temp-message
	(when verbose
	  (format "Fontifying %s..." (buffer-name)))
      ;; Make sure we fontify etc. in the whole buffer.
      (save-restriction
        (unless font-lock-dont-widen (widen))
	(condition-case nil
	    (save-excursion
	      (save-match-data
		(font-lock-fontify-region (point-min) (point-max) verbose)
		(setq font-lock-fontified t)))
	  ;; We don't restore the old fontification, so it's best to unfontify.
	  (quit (font-lock-unfontify-buffer)))))))

(defun font-lock-default-unfontify-buffer ()
  "Unfontify the whole buffer using `font-lock-unfontify-region-function'."
  ;; Make sure we unfontify etc. in the whole buffer.
  (save-restriction
    (widen)
    (font-lock-unfontify-region (point-min) (point-max))
    (setq font-lock-fontified nil)))

(defvar font-lock-dont-widen nil
  "If non-nil, font-lock will work on the non-widened buffer.
Useful for things like RMAIL and Info where the whole buffer is not
a very meaningful entity to highlight.")


(defvar font-lock-beg) (defvar font-lock-end)
(defvar-local font-lock-extend-region-functions
  '(font-lock-extend-region-wholelines
    ;; This use of font-lock-multiline property is unreliable but is just
    ;; a handy heuristic: in case you don't have a function that does
    ;; /identification/ of multiline elements, you may still occasionally
    ;; discover them by accident (or you may /identify/ them but not in all
    ;; cases), in which case the font-lock-multiline property can help make
    ;; sure you will properly *re*identify them during refontification.
    font-lock-extend-region-multiline)
  "Special hook run just before proceeding to fontify a region.
This is used to allow major modes to help font-lock find safe buffer positions
as beginning and end of the fontified region.  Its most common use is to solve
the problem of /identification/ of multiline elements by providing a function
that tries to find such elements and move the boundaries such that they do
not fall in the middle of one.
Each function is called with no argument; it is expected to adjust the
dynamically bound variables `font-lock-beg' and `font-lock-end'; and return
non-nil if it did make such an adjustment.
These functions are run in turn repeatedly until they all return nil.
Put first the functions more likely to cause a change and cheaper to compute.")
;; Mark it as a special hook which doesn't use any global setting
;; (i.e. doesn't obey the element t in the buffer-local value).

(defun font-lock-extend-region-multiline ()
  "Move fontification boundaries away from any `font-lock-multiline' property."
  (let ((changed nil))
    (when (and (> font-lock-beg (point-min))
               (get-text-property (1- font-lock-beg) 'font-lock-multiline))
      (setq changed t)
      (setq font-lock-beg (or (previous-single-property-change
                               font-lock-beg 'font-lock-multiline)
                              (point-min))))
    ;; If `font-lock-multiline' starts at `font-lock-end', do not
    ;; extend the region.
    (let ((before-end (max (point-min) (1- font-lock-end)))
          (new-end nil))
      (when (get-text-property before-end 'font-lock-multiline)
        (setq new-end (or (text-property-any before-end (point-max)
                                             'font-lock-multiline nil)
                          (point-max)))
        (when (/= new-end font-lock-end)
          (setq changed t)
          (setq font-lock-end new-end))))
    changed))

(defun font-lock-extend-region-wholelines ()
  "Move fontification boundaries to beginning of lines."
  (let ((new (syntax-propertize-wholelines font-lock-beg font-lock-end)))
    (when new
      (setq font-lock-beg (car new))
      (setq font-lock-end (cdr new))
      t)))

(defun font-lock-default-fontify-region (beg end loudly)
  "Fontify the text between BEG and END.
If LOUDLY is non-nil, print status messages while fontifying.
This function is the default `font-lock-fontify-region-function'."
  (with-silent-modifications
   ;; Use the fontification syntax table, if any.
   (with-syntax-table (or font-lock-syntax-table (syntax-table))
     ;; Extend the region to fontify so that it starts and ends at
     ;; safe places.
     (let ((funs font-lock-extend-region-functions)
           (font-lock-beg beg)
           (font-lock-end end))
       (while funs
         (setq funs (if (or (not (funcall (car funs)))
                            (eq funs font-lock-extend-region-functions))
                        (cdr funs)
                      ;; If there's been a change, we should go through
                      ;; the list again since this new position may
                      ;; warrant a different answer from one of the fun
                      ;; we've already seen.
                      font-lock-extend-region-functions)))
       (setq beg font-lock-beg end font-lock-end))
     ;; Now do the fontification.
     (font-lock-unfontify-region beg end)
     (when (and font-lock-syntactic-keywords
                (null syntax-propertize-function))
       ;; Ensure the beginning of the file is properly syntactic-fontified.
       (let ((start beg))
         (when (< font-lock-syntactically-fontified start)
           (setq start (max font-lock-syntactically-fontified (point-min)))
           (setq font-lock-syntactically-fontified end))
         (font-lock-fontify-syntactic-keywords-region start end)))
     (unless font-lock-keywords-only
       (font-lock-fontify-syntactically-region beg end loudly))
     (font-lock-fontify-keywords-region beg end loudly)
     `(jit-lock-bounds ,beg . ,end))))

(defvar font-lock-extra-managed-props nil
  "Additional text properties managed by font-lock.
This is used by `font-lock-default-unfontify-region' to decide
what properties to clear before refontifying a region.")

(defun font-lock-default-unfontify-region (beg end)
  "Unfontify the text between BEG and END.
This function is the default `font-lock-unfontify-region-function'."
  (remove-list-of-text-properties
   beg end (append
	    font-lock-extra-managed-props
	    `(,@(when font-lock-syntactic-keywords '(syntax-table))
              face font-lock-multiline))))

(defun font-lock-after-change-function (beg end &optional old-len)
  "Called when any modification is made to buffer text."
  (save-excursion
    (let ((inhibit-quit t)
          (region (when font-lock-extend-after-change-region-function
                    (funcall font-lock-extend-after-change-region-function
                             beg end old-len))))
      (save-match-data
	(if region
	    ;; Fontify the region the major mode has specified.
	    (setq beg (car region) end (cdr region))
	  ;; Fontify the whole lines which enclose the region.
          ;; Actually, this is not needed because
          ;; font-lock-default-fontify-region already rounds up to a whole
          ;; number of lines.
	  ;; (setq beg (progn (goto-char beg) (line-beginning-position))
	  ;;       end (progn (goto-char end) (line-beginning-position 2)))
	  (unless (eq end (point-max))
	    ;; Rounding up to a whole number of lines should include the
	    ;; line right after `end'.  Typical case: the first char of
	    ;; the line was deleted.  Or a \n was inserted in the middle
	    ;; of a line.
	    (setq end (1+ end))))
	(font-lock-fontify-region beg end)))))

(defvar jit-lock-start)
(defvar jit-lock-end)
(defun font-lock-extend-jit-lock-region-after-change (beg end old-len)
  "Function meant for `jit-lock-after-change-extend-region-functions'.
This function does 2 things:
- extend the region so that it not only includes the part that was modified
  but also the surrounding text whose highlighting may change as a consequence.
- anticipate (part of) the region extension that will happen later in
  `font-lock-default-fontify-region', in order to avoid the need for
  double-redisplay in `jit-lock-fontify-now'."
  (save-excursion
    ;; First extend the region as font-lock-after-change-function would.
    (let ((region (if font-lock-extend-after-change-region-function
                      (funcall font-lock-extend-after-change-region-function
                               beg end old-len))))
      (if region
          (setq beg (min jit-lock-start (car region))
                end (max jit-lock-end (cdr region))))
      ;; Then extend the region obeying font-lock-multiline properties,
      ;; indicating which part of the buffer needs to be refontified.
      ;; !!! This is the *main* user of font-lock-multiline property !!!
      ;; font-lock-after-change-function could/should also do that, but it
      ;; doesn't need to because font-lock-default-fontify-region does
      ;; it anyway.  Here OTOH we have no guarantee that
      ;; font-lock-default-fontify-region will be executed on this region
      ;; any time soon.
      ;; Note: contrary to font-lock-default-fontify-region, we do not do
      ;; any loop here because we are not looking for a safe spot: we just
      ;; mark the text whose appearance may need to change as a result of
      ;; the buffer modification.
      (when (and (> beg (point-min))
                 (get-text-property (1- beg) 'font-lock-multiline))
        (setq beg (or (previous-single-property-change
                       beg 'font-lock-multiline)
                      (point-min))))
      (when (< end (point-max))
        (setq end
              (cond
               ((get-text-property end 'font-lock-multiline)
                (or (text-property-any end (point-max)
                                       'font-lock-multiline nil)
                    (point-max)))
               ;; If `end' has been set by the function above, don't corrupt it.
               (font-lock-extend-after-change-region-function end)
                ;; Rounding up to a whole number of lines should include the
                ;; line right after `end'.  Typical case: the first char of
                ;; the line was deleted.  Or a \n was inserted in the middle
                ;; of a line.
               (t (1+ end)))))
      ;; Finally, pre-enlarge the region to a whole number of lines, to try
      ;; and anticipate what font-lock-default-fontify-region will do, so as to
      ;; avoid double-redisplay.
      ;; We could just run `font-lock-extend-region-functions', but since
      ;; the only purpose is to avoid the double-redisplay, we prefer to
      ;; do here only the part that is cheap and most likely to be useful.
      (when (memq 'font-lock-extend-region-wholelines
                  font-lock-extend-region-functions)
        (goto-char beg)
        (setq beg (min jit-lock-start (line-beginning-position)))
        (goto-char end)
        (setq end
              (max jit-lock-end
                   (if (bolp) (point) (line-beginning-position 2)))))
      (setq jit-lock-start beg
	    jit-lock-end end))))

(defun font-lock-fontify-block (&optional arg)
  "Fontify some lines the way `font-lock-fontify-buffer' would.
The lines could be a function or paragraph, or a specified number of lines.
If ARG is given, fontify that many lines before and after point, or 16 lines if
no ARG is given and `font-lock-mark-block-function' is nil.
If `font-lock-mark-block-function' non-nil and no ARG is given, it is used to
delimit the region to fontify."
  (interactive "P")
  (let (deactivate-mark)
    ;; Make sure we have the right `font-lock-keywords' etc.
    (unless font-lock-mode (font-lock-ensure-keywords))
    (save-mark-and-excursion
      (save-match-data
	(condition-case error-data
	    (if (or arg (not font-lock-mark-block-function))
		(let ((lines (if arg (prefix-numeric-value arg) 16)))
		  (font-lock-fontify-region
		   (save-excursion (forward-line (- lines)) (point))
		   (save-excursion (forward-line lines) (point))))
	      (funcall font-lock-mark-block-function)
	      (font-lock-fontify-region (point) (mark)))
	  ((error quit) (message "Fontifying block...%s" error-data)))))))

(defun font-lock--add-text-property (start end prop value object append)
  "Add an element to a property of the text from START to END.
Arguments PROP and VALUE specify the property and value to add to
the value already in place.  The resulting property values are
always lists.  Argument OBJECT is the string or buffer containing
the text.  If argument APPEND is non-nil, VALUE will be appended,
otherwise it will be prepended."
  (let ((val (if (and (listp value) (not (keywordp (car value))))
                 ;; Already a list of faces.
                 value
               ;; A single face (e.g. a plist of face properties).
               (list value)))
        next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      ;; Canonicalize old forms of face property.
      (and (memq prop '(face font-lock-face))
	   (listp prev)
	   (or (keywordp (car prev))
	       (memq (car prev) '(foreground-color background-color)))
	   (setq prev (list prev)))
      (let* ((list-prev (if (listp prev) prev (list prev)))
             (new-value (if append
                           (append list-prev val)
                         (append val list-prev))))
        (put-text-property start next prop new-value object))
      (setq start next))))

(defun font-lock-prepend-text-property (start end prop value &optional object)
  "Prepend to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to prepend to the value
already in place.  The resulting property values are always lists.
Optional argument OBJECT is the string or buffer containing the text."
  (font-lock--add-text-property start end prop value object nil))

(defun font-lock-append-text-property (start end prop value &optional object)
  "Append to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to append to the value
already in place.  The resulting property values are always lists.
Optional argument OBJECT is the string or buffer containing the text."
  (font-lock--add-text-property start end prop value object t))

(defun font-lock-fillin-text-property (start end prop value &optional object)
  "Fill in one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to put where none are
already in place.  Therefore existing property values are not overwritten.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((start (text-property-any start end prop nil object)) next)
    (while start
      (setq next (next-single-property-change start prop object end))
      (put-text-property start next prop value object)
      (setq start (text-property-any next end prop nil object)))))

(defun font-lock--remove-face-from-text-property (start
						  end
						  prop value &optional object)
  "Remove a specific property value from text from START to END.
Arguments PROP and VALUE specify the property and value to remove.  The
resulting property values are not `eq' to VALUE nor lists containing VALUE.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((start (text-property-not-all start end prop nil object)) next prev)
    (while start
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      (cond ((or (atom prev)
		 (keywordp (car prev))
		 (eq (car prev) 'foreground-color)
		 (eq (car prev) 'background-color))
	     (when (eq value prev)
	       (remove-list-of-text-properties start next (list prop) object)))
	    ((memq value prev)		;Assume prev is not dotted.
	     (let ((new (remq value prev)))
	       (cond ((null new)
		      (remove-list-of-text-properties start next (list prop)
						      object))
		     ((= (length new) 1)
		      (put-text-property start next prop (car new) object))
		     (t
		      (put-text-property start next prop new object))))))
      (setq start (text-property-not-all next end prop nil object)))))

;; Bristling with youthful exuberance, someone (monnier?) defined
;; syntactic and non-syntactic versions of apply-highlight,
;; fontify-keywords-region, and fontify-anchored-keywords.
;; Effete but longwinded reasons are given in the git history.

(defun font-lock-apply-syntactic-highlight (highlight)
  "Apply HIGHLIGHT following a match.
HIGHLIGHT should be of the form MATCH-HIGHLIGHT,
see `font-lock-syntactic-keywords'."
  (let* ((match (nth 0 highlight))
	 (start (match-beginning match)) (end (match-end match))
	 (value (nth 1 highlight))
	 (override (nth 2 highlight)))
    (if (not start)
	;; No match but we might not signal an error.
	(or (nth 3 highlight)
	    (error "No match %d in highlight %S" match highlight))
      (when (and (consp value) (not (numberp (car value))))
	(setq value (eval value t)))
      (when (stringp value) (setq value (string-to-syntax value)))
      ;; Flush the syntax-cache.  I believe this is not necessary for
      ;; font-lock's use of syntax-ppss, but I'm not 100% sure and it can
      ;; still be necessary for other users of syntax-ppss anyway.
      (syntax-ppss-invalidate-cache start)
      (cond
       ((not override)
	;; Cannot override existing fontification.
	(or (text-property-not-all start end 'syntax-table nil)
	    (put-text-property start end 'syntax-table value)))
       ((eq override t)
	;; Override existing fontification.
	(put-text-property start end 'syntax-table value))
       ((eq override 'keep)
	;; Keep existing fontification.
	(font-lock-fillin-text-property start end 'syntax-table value))))))

(defun font-lock-fontify-syntactic-anchored-keywords (keywords limit)
  "Fontify according to KEYWORDS until LIMIT.
KEYWORDS should be of the form MATCH-ANCHORED, see `font-lock-keywords',
LIMIT can be modified by the value of its PRE-MATCH-FORM."
  (let ((matcher (nth 0 keywords)) (lowdarks (nthcdr 3 keywords)) highlights
	;; Evaluate PRE-MATCH-FORM.
	(pre-match-value (eval (nth 1 keywords) t)))
    ;; Set LIMIT to value of PRE-MATCH-FORM or the end of line.
    (if (and (numberp pre-match-value) (> pre-match-value (point)))
	(setq limit pre-match-value)
      (setq limit (line-end-position)))
    (save-match-data
      ;; Find an occurrence of `matcher' before `limit'.
      (while (if (stringp matcher)
		 (re-search-forward matcher limit t)
	       (funcall matcher limit))
	;; Apply each highlight to this instance of `matcher'.
	(setq highlights lowdarks)
	(while highlights
	  (font-lock-apply-syntactic-highlight (car highlights))
	  (setq highlights (cdr highlights)))))
    ;; Evaluate POST-MATCH-FORM.
    (eval (nth 2 keywords) t)))

(defun font-lock-fontify-syntactic-keywords-region (start end)
  "Fontify according to `font-lock-syntactic-keywords' between START and END.
START should be at the beginning of a line."
  (unless parse-sexp-lookup-properties
    ;; We wouldn't go through so much trouble if we didn't intend to use those
    ;; properties, would we?
    (setq-local parse-sexp-lookup-properties t))
  ;; If `font-lock-syntactic-keywords' is a symbol, get the real keywords.
  (when (symbolp font-lock-syntactic-keywords)
    (setq font-lock-syntactic-keywords (font-lock-eval-keywords
					font-lock-syntactic-keywords)))
  ;; If `font-lock-syntactic-keywords' is not compiled, compile it.
  (unless (eq (car font-lock-syntactic-keywords) t)
    (setq font-lock-syntactic-keywords (font-lock-compile-keywords
					font-lock-syntactic-keywords
					t)))
  ;; Get down to business.
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(keywords (cddr font-lock-syntactic-keywords))
	keyword matcher highlights)
    (while keywords
      ;; Find an occurrence of `matcher' from `start' to `end'.
      (setq keyword (car keywords) matcher (car keyword))
      (goto-char start)
      (while (and (< (point) end)
                  (if (stringp matcher)
                      (re-search-forward matcher end t)
                    (funcall matcher end)))
	;; Apply each highlight to this instance of `matcher', which may be
	;; specific highlights or more keywords anchored to `matcher'.
	(setq highlights (cdr keyword))
	(while highlights
	  (if (numberp (car (car highlights)))
	      (font-lock-apply-syntactic-highlight (car highlights))
	    (font-lock-fontify-syntactic-anchored-keywords (car highlights)
							   end))
	  (setq highlights (cdr highlights))))
      (setq keywords (cdr keywords)))))

(defvar font-lock-comment-start-skip nil
  "If non-nil, Font Lock mode uses this instead of `comment-start-skip'.")

(defvar font-lock-comment-end-skip nil
  "If non-nil, Font Lock mode uses this instead of `comment-end-skip'.")

(defun font-lock-fontify-syntactically-region (start end &optional loudly)
  "Put proper face on each string and comment between START and END.
START should be at the beginning of a line."
  (syntax-propertize end)  ; Apply any needed syntax-table properties.
  (with-syntax-table (or syntax-ppss-table (syntax-table))
    (when (and comment-start (not comment-end-skip)) (comment-normalize-vars))
    (let (;; Find the `start' state.
          (state (if (or syntax-ppss-table
                         (not font-lock--syntax-table-affects-ppss))
                     (syntax-ppss start)
                   ;; If `syntax-ppss' doesn't have its own syntax-table and
                   ;; we have installed our own syntax-table which
                   ;; differs from the standard one in ways which affects PPSS,
                   ;; then we can't use `syntax-ppss' since that would pollute
                   ;; and be polluted by its cache.
                   (parse-partial-sexp (point-min) start)))
          face beg)
      (if loudly (message "Fontifying %s... (syntactically...)" (buffer-name)))
      ;;
      ;; Find each interesting place between here and `end'.
      (while
	  (progn
	    (when (or (nth 3 state) (nth 4 state))
	      (setq face (funcall font-lock-syntactic-face-function state))
	      (setq beg (max (nth 8 state) start))
	      (setq state (parse-partial-sexp (point) end nil nil state
					      'syntax-table))
	      (when face (put-text-property beg (point) 'face face))
	      (when (and (eq face 'font-lock-comment-face)
                         (or font-lock-comment-start-skip
			     comment-start-skip))
	        ;; Find the comment delimiters
	        ;; and use font-lock-comment-delimiter-face for them.
	        (save-excursion
		  (goto-char beg)
		  (if (looking-at (or font-lock-comment-start-skip
				      comment-start-skip))
		      (put-text-property beg (match-end 0) 'face
				         font-lock-comment-delimiter-face)))
	        (if (looking-back (or font-lock-comment-end-skip
				      comment-end-skip)
                                  (line-beginning-position) t)
		    (put-text-property (match-beginning 0) (point) 'face
				       font-lock-comment-delimiter-face))))
	    (< (point) end))
        (setq state (parse-partial-sexp (point) end nil nil state
				        'syntax-table))))))

(defsubst font-lock-apply-highlight (highlight)
  "Apply HIGHLIGHT following a match.
HIGHLIGHT should be of the form MATCH-HIGHLIGHT, see `font-lock-keywords'."
  (let* ((match (nth 0 highlight))
	 (start (match-beginning match)) (end (match-end match))
	 (override (nth 2 highlight)))
    (if (not start)
	;; No match but we might not signal an error.
	(or (nth 3 highlight)
	    (error "No match %d in highlight %S" match highlight))
      (let ((val (eval (nth 1 highlight) t)))
	(when (eq (car-safe val) 'face)
	  (add-text-properties start end (cddr val))
	  (setq val (cadr val)))
	(cond
	 ((not (or val (eq override t)))
	  ;; If `val' is nil, don't do anything.  It is important to do it
	  ;; explicitly, because when adding nil via things like
	  ;; font-lock-append-text-property, the property is actually
	  ;; changed from <face> to (<face>) which is undesirable.  --Stef
	  nil)
	 ((not override)
	  ;; Cannot override existing fontification.
	  (or (text-property-not-all start end 'face nil)
	      (put-text-property start end 'face val)))
	 ((eq override t)
	  ;; Override existing fontification.
	  (put-text-property start end 'face val))
	 ((eq override 'prepend)
	  ;; Prepend to existing fontification.
	  (font-lock-prepend-text-property start end 'face val))
	 ((eq override 'append)
	  ;; Append to existing fontification.
	  (font-lock-append-text-property start end 'face val))
	 ((eq override 'keep)
	  ;; Keep existing fontification.
	  (font-lock-fillin-text-property start end 'face val)))))))

(defsubst font-lock-fontify-anchored-keywords (keywords limit)
  "Fontify according to KEYWORDS until LIMIT.
KEYWORDS should be of the form MATCH-ANCHORED, see `font-lock-keywords',
LIMIT can be modified by the value of its PRE-MATCH-FORM."
  (let ((matcher (nth 0 keywords)) (lowdarks (nthcdr 3 keywords)) highlights
	(lead-start (match-beginning 0))
	;; Evaluate PRE-MATCH-FORM.
	(pre-match-value (eval (nth 1 keywords) t)))
    ;; Set LIMIT to value of PRE-MATCH-FORM or the end of line.
    (if (not (and (numberp pre-match-value) (> pre-match-value (point))))
	(setq limit (line-end-position))
      (setq limit pre-match-value)
      (when (and font-lock-multiline (>= limit (line-beginning-position 2)))
	;; this is a multiline anchored match
	;; (setq font-lock-multiline t)
	(put-text-property (if (= limit (line-beginning-position 2))
			       (1- limit)
			     (min lead-start (point)))
			   limit
			   'font-lock-multiline t)))
    (save-match-data
      ;; Find an occurrence of `matcher' before `limit'.
      (while (and (< (point) limit)
		  (if (stringp matcher)
		      (re-search-forward matcher limit t)
		    (funcall matcher limit)))
	;; Apply each highlight to this instance of `matcher'.
	(setq highlights lowdarks)
	(while highlights
	  (font-lock-apply-highlight (car highlights))
	  (setq highlights (cdr highlights)))))
    ;; Evaluate POST-MATCH-FORM.
    (eval (nth 2 keywords) t)))

(defun font-lock-fontify-keywords-region (start end &optional loudly)
  "Fontify according to `font-lock-keywords' between START and END.
START should be at the beginning of a line.
LOUDLY, if non-nil, allows progress-meter bar."
  (unless (eq (car font-lock-keywords) t)
    (setq font-lock-keywords
	  (font-lock-compile-keywords font-lock-keywords)))
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(keywords (cddr font-lock-keywords))
	(bufname (buffer-name)) (count 0)
        (pos (make-marker))
	keyword matcher highlights)
    ;;
    ;; Fontify each item in `font-lock-keywords' from `start' to `end'.
    (while keywords
      (if loudly (message "Fontifying %s... (regexps..%s)" bufname
			  (make-string (cl-incf count) ?.)))
      ;;
      ;; Find an occurrence of `matcher' from `start' to `end'.
      (setq keyword (car keywords) matcher (car keyword))
      (goto-char start)
      (while (and (< (point) end)
		  (if (stringp matcher)
		      (re-search-forward matcher end t)
		    (funcall matcher end))
                  ;; Beware empty string matches since they will
                  ;; loop indefinitely.
                  (or (> (point) (match-beginning 0))
                      (progn (forward-char 1) t)))
	(when (and font-lock-multiline
		   (>= (point)
		       (save-excursion (goto-char (match-beginning 0))
				       (forward-line 1) (point))))
	  ;; this is a multiline regexp match
	  ;; (setq font-lock-multiline t)
	  (put-text-property (if (= (point)
				    (save-excursion
				      (goto-char (match-beginning 0))
				      (forward-line 1) (point)))
				 (1- (point))
			       (match-beginning 0))
			     (point)
			     'font-lock-multiline t))
	;; Apply each highlight to this instance of `matcher', which may be
	;; specific highlights or more keywords anchored to `matcher'.
	(setq highlights (cdr keyword))
	(while highlights
	  (if (numberp (car (car highlights)))
	      (font-lock-apply-highlight (car highlights))
	    (set-marker pos (point))
            (font-lock-fontify-anchored-keywords (car highlights) end)
            ;; Ensure forward progress.  `pos' is a marker because anchored
            ;; keyword may add/delete text (this happens e.g. in grep.el).
            (if (< (point) pos) (goto-char pos)))
	  (setq highlights (cdr highlights))))
      (setq keywords (cdr keywords)))
    (set-marker pos nil)))

(defun font-lock-compile-keywords (keywords &optional _syntactic-keywords)
  "Compile KEYWORDS into the form (t KEYWORDS COMPILED...)
Here each COMPILED is of the form (MATCHER HIGHLIGHT ...) as shown in the
`font-lock-keywords' doc string."
  (unless font-lock-keywords-set
    ;; This should never happen.  But some external packages sometimes
    ;; call font-lock in unexpected and incorrect ways.  It's important to
    ;; stop processing at this point, otherwise we may end up changing the
    ;; global value of font-lock-keywords and break highlighting in many
    ;; other buffers.
    (error "Font-lock trying to use keywords before setting them up"))
  (if (eq (car-safe keywords) t)
      keywords
    (cons t (cons keywords
		  (mapcar #'font-lock-compile-keyword keywords)))))

(defun font-lock-compile-keyword (keyword)
  (cond ((or (functionp keyword) (nlistp keyword)) ; MATCHER
	 (list keyword '(0 font-lock-keyword-face)))
	((eq (car keyword) 'eval)		; (eval . FORM)
	 (font-lock-compile-keyword (eval (cdr keyword) t)))
	((eq (car-safe (cdr keyword)) 'quote)	; (MATCHER . 'FORM)
	 ;; If FORM is a FACENAME then quote it.  Otherwise ignore the quote.
	 (if (symbolp (nth 2 keyword))
	     (list (car keyword) (list 0 (cdr keyword)))
	   (font-lock-compile-keyword (cons (car keyword) (nth 2 keyword)))))
	((numberp (cdr keyword))		; (MATCHER . MATCH)
	 (list (car keyword) (list (cdr keyword) 'font-lock-keyword-face)))
	((symbolp (cdr keyword))		; (MATCHER . FACENAME)
	 (list (car keyword) (list 0 (cdr keyword))))
	((nlistp (nth 1 keyword))		; (MATCHER . HIGHLIGHT)
	 (list (car keyword) (cdr keyword)))
	(t					; (MATCHER HIGHLIGHT ...)
	 keyword)))

(defun font-lock-eval-keywords (keywords)
  "Evaluate KEYWORDS if a function (funcall) or variable (eval) name."
  (if (listp keywords)
      keywords
    (font-lock-eval-keywords (if (fboundp keywords)
				 (funcall keywords)
			       (eval keywords t)))))

(defun font-lock-choose-keywords (keywords level)
  "Return LEVELth element of KEYWORDS.
A LEVEL of nil is equal to a LEVEL of 0, a LEVEL of t is equal to
\(1- (length KEYWORDS))."
  (cond ((not (and (listp keywords) (symbolp (car keywords))))
	 keywords)
	((numberp level)
	 (or (nth level keywords) (car (last keywords))))
	((eq level t)
	 (car (last keywords)))
	(t
	 (car keywords))))

(defun font-lock--match-keyword (rule keyword)
  "Return non-nil if font-lock KEYWORD matches RULE.
See `font-lock-ignore' for the possible rules."
  (pcase-exhaustive rule
    ('* t)
    ((pred symbolp)
     (let ((regexp (when (string-match-p "[*?]" (symbol-name rule))
                     (wildcard-to-regexp (symbol-name rule)))))
       (named-let search ((obj keyword))
         (cond
          ((consp obj) (or (search (car obj)) (search (cdr obj))))
          ((not regexp) (eq rule obj))
          ((symbolp obj) (string-match-p regexp (symbol-name obj)))))))
    ((pred stringp) (when (stringp (car keyword))
                      (string-match-p (concat "\\`\\(?:" (car keyword) "\\)")
                                      rule)))
    (`(or . ,rules) (let ((match nil))
                      (while rules
                        (pcase-exhaustive (pop rules)
                          (`(except ,rule)
                           (when match
                             (setq match (not (font-lock--match-keyword rule keyword)))))
                          (rule
                           (unless match
                             (setq match (font-lock--match-keyword rule keyword))))))
                      match))
    (`(not ,rule) (not (font-lock--match-keyword rule keyword)))
    (`(and . ,rules) (seq-every-p (lambda (rule)
                                    (font-lock--match-keyword rule keyword))
                                  rules))
    (`(pred ,fun) (funcall fun keyword))))

(defalias 'font-lock-set-defaults #'font-lock-ensure-keywords)
(defun font-lock-ensure-keywords ()
  "Parse `font-lock-defaults' into `font-lock-keywords'."
  (unless font-lock-keywords-set
    (setq font-lock-keywords-set t)
    (let* ((defaults font-lock-defaults)
	   (keywords
	    (font-lock-choose-keywords
             (nth 0 defaults)
             (if (consp font-lock-maximum-decoration)
                 (cdr (or (assq major-mode font-lock-maximum-decoration)
                          (assq t font-lock-maximum-decoration)))
               font-lock-maximum-decoration)))
	   (local (cdr (assq major-mode font-lock-keywords-alist)))
	   (removed-keywords
	    (cdr-safe (assq major-mode font-lock-removed-keywords-alist))))
      ;; Syntactic fontification?
      (setq-local font-lock-keywords-only (nth 1 defaults))
      ;; Case fold during regexp fontification?
      (setq-local font-lock-keywords-case-fold-search (nth 2 defaults))
      ;; Syntax table for regexp and syntactic fontification?
      (kill-local-variable 'font-lock--syntax-table-affects-ppss)
      (if (null (nth 3 defaults))
          (setq-local font-lock-syntax-table nil)
	(setq-local font-lock-syntax-table (copy-syntax-table (syntax-table)))
	(dolist (selem (nth 3 defaults))
	  ;; The character to modify may be a single CHAR or a STRING.
	  (let ((syntax (cdr selem)))
	    (dolist (char (if (numberp (car selem))
			      (list (car selem))
			    (mapcar #'identity (car selem))))
	      (unless (memq (car (aref font-lock-syntax-table char))
	                    '(1 2 3))    ;"." "w" "_"
	        (setq font-lock--syntax-table-affects-ppss t))
	      (modify-syntax-entry char syntax font-lock-syntax-table)
	      (unless (memq (car (aref font-lock-syntax-table char))
	                    '(1 2 3))    ;"." "w" "_"
	        (setq font-lock--syntax-table-affects-ppss t))))))
      ;; rest is (var . val) bindings
      (dolist (x (nthcdr 4 defaults))
        (when (consp x)
	  (set (make-local-variable (car x)) (cdr x))))
      ;; Set this last to capture earlier side effects.
      (setq-local font-lock-keywords
                  (font-lock-eval-keywords keywords))
      ;; Local fontification?
      (while local
	(font-lock-add-keywords nil (car (car local)) (cdr (car local)))
	(setq local (cdr local)))
      (when removed-keywords
	(font-lock-remove-keywords nil removed-keywords))
      ;; Now compile the keywords.
      (unless (eq (car font-lock-keywords) t)
	(setq font-lock-keywords
              (font-lock-compile-keywords font-lock-keywords))))
    (font-lock-flush)))

(defface font-lock-comment-face
  '((((class grayscale) (background light))
     :foreground "DimGray" :weight bold :slant italic)
    (((class grayscale) (background dark))
     :foreground "LightGray" :weight bold :slant italic)
    (((class color) (min-colors 88) (background light))
     :foreground "Firebrick")
    (((class color) (min-colors 88) (background dark))
     :foreground "chocolate1")
    (((class color) (min-colors 16) (background light))
     :foreground "red")
    (((class color) (min-colors 16) (background dark))
     :foreground "red1")
    (((class color) (min-colors 8) (background light))
     :foreground "red")
    (((class color) (min-colors 8) (background dark))
     :foreground "yellow")
    (t :weight bold :slant italic))
  "Font Lock mode face used to highlight comments."
  :group 'font-lock-faces)

(defface font-lock-comment-delimiter-face
  '((default :inherit font-lock-comment-face))
  "Font Lock mode face used to highlight comment delimiters."
  :group 'font-lock-faces)

(defface font-lock-string-face
  '((((class grayscale) (background light)) :foreground "DimGray" :slant italic)
    (((class grayscale) (background dark))  :foreground "LightGray" :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "VioletRed4")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 16) (background light)) :foreground "RosyBrown")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 8)) :foreground "green")
    (t :slant italic))
  "Font Lock mode face used to highlight strings."
  :group 'font-lock-faces)

(defface font-lock-doc-face
  '((t :inherit font-lock-string-face))
  "Font Lock mode face used to highlight documentation embedded in program code.
It is typically used for special documentation comments or strings."
  :group 'font-lock-faces)

(defface font-lock-doc-markup-face
  '((t :inherit font-lock-constant-face))
  "Font Lock mode face used to highlight embedded documentation mark-up.
It is meant for mark-up elements in text that uses `font-lock-doc-face', such
as the constructs of Haddock, Javadoc and similar systems."
  :version "28.1"
  :group 'font-lock-faces)

(defface font-lock-keyword-face
  '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "Purple")
    (((class color) (min-colors 88) (background dark))  :foreground "Cyan1")
    (((class color) (min-colors 16) (background light)) :foreground "Purple")
    (((class color) (min-colors 16) (background dark))  :foreground "Cyan")
    (((class color) (min-colors 8)) :foreground "cyan" :weight bold)
    (t :weight bold))
  "Font Lock mode face used to highlight keywords."
  :group 'font-lock-faces)

(defface font-lock-builtin-face
  '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "dark slate blue")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSteelBlue")
    (((class color) (min-colors 16) (background light)) :foreground "Orchid")
    (((class color) (min-colors 16) (background dark)) :foreground "LightSteelBlue")
    (((class color) (min-colors 8)) :foreground "blue" :weight bold)
    (t :weight bold))
  "Font Lock mode face used to highlight builtins."
  :group 'font-lock-faces)

(defface font-lock-function-name-face
  '((((class color) (min-colors 88) (background light)) :foreground "Blue1")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSkyBlue")
    (((class color) (min-colors 16) (background light)) :foreground "Blue")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSkyBlue")
    (((class color) (min-colors 8)) :foreground "blue" :weight bold)
    (t :inverse-video t :weight bold))
  "Font Lock mode face used to highlight function names."
  :group 'font-lock-faces)

(defface font-lock-variable-name-face
  '((((class grayscale) (background light))
     :foreground "Gray90" :weight bold :slant italic)
    (((class grayscale) (background dark))
     :foreground "DimGray" :weight bold :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "sienna")
    (((class color) (min-colors 88) (background dark))  :foreground "LightGoldenrod")
    (((class color) (min-colors 16) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 16) (background dark))  :foreground "LightGoldenrod")
    (((class color) (min-colors 8)) :foreground "yellow" :weight light)
    (t :weight bold :slant italic))
  "Font Lock mode face used to highlight variable names."
  :group 'font-lock-faces)

(defface font-lock-type-face
  '((((class grayscale) (background light)) :foreground "Gray90" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))  :foreground "PaleGreen")
    (((class color) (min-colors 16) (background light)) :foreground "ForestGreen")
    (((class color) (min-colors 16) (background dark))  :foreground "PaleGreen")
    (((class color) (min-colors 8)) :foreground "green")
    (t :weight bold :underline t))
  "Font Lock mode face used to highlight type and class names."
  :group 'font-lock-faces)

(defface font-lock-constant-face
  '((((class grayscale) (background light))
     :foreground "LightGray" :weight bold :underline t)
    (((class grayscale) (background dark))
     :foreground "Gray50" :weight bold :underline t)
    (((class color) (min-colors 88) (background light)) :foreground "dark cyan")
    (((class color) (min-colors 88) (background dark))  :foreground "Aquamarine")
    (((class color) (min-colors 16) (background light)) :foreground "CadetBlue")
    (((class color) (min-colors 16) (background dark))  :foreground "Aquamarine")
    (((class color) (min-colors 8)) :foreground "magenta")
    (t :weight bold :underline t))
  "Font Lock mode face used to highlight constants and labels."
  :group 'font-lock-faces)

(defface font-lock-warning-face
  '((t :inherit error))
  "Font Lock mode face used to highlight warnings."
  :group 'font-lock-faces)

(defface font-lock-negation-char-face
  '((t nil))
  "Font Lock mode face used to highlight easy to overlook negation."
  :group 'font-lock-faces)

(defface font-lock-preprocessor-face
  '((t :inherit font-lock-builtin-face))
  "Font Lock mode face used to highlight preprocessor directives."
  :group 'font-lock-faces)

(defface font-lock-regexp-grouping-backslash
  '((t :inherit bold))
  "Font Lock mode face for backslashes in Lisp regexp grouping constructs."
  :group 'font-lock-faces)

(defface font-lock-regexp-grouping-construct
  '((t :inherit bold))
  "Font Lock mode face used to highlight grouping constructs in Lisp regexps."
  :group 'font-lock-faces)

(defun font-lock-match-c-style-declaration-item-and-skip-to-next (limit)
  "Match, and move over, any declaration/definition item after point.
Matches after point, but ignores leading whitespace and `*' characters.
Does not move further than LIMIT.

The expected syntax of a declaration/definition item is `word' (preceded by
optional whitespace and `*' characters and proceeded by optional whitespace)
optionally followed by a `('.  Everything following the item (but belonging to
it) is expected to be skip-able by `scan-sexps', and items are expected to be
separated with a `,' and to be terminated with a `;'.

Thus the regexp matches after point:	word (
					^^^^ ^
Where the match subexpressions are:	  1  2

The item is delimited by (match-beginning 1) and (match-end 1).
If (match-beginning 2) is non-nil, the item is followed by a `('.

This function could be MATCHER in a MATCH-ANCHORED `font-lock-keywords' item.

Despite its name, this function is defined in font-lock.el since it is
used outside cc-fonts.el."
  (when (looking-at "[ \n\t*]*\\(\\sw+\\)[ \t\n]*\\(((?\\)?")
    (when (and (match-end 2) (> (- (match-end 2) (match-beginning 2)) 1))
      ;; If `word' is followed by a double open-paren, it's probably
      ;; a macro used for "int myfun P_ ((int arg1))".  Let's go back one
      ;; word to try and match `myfun' rather than `P_'.
      (let ((pos (point)))
	(skip-chars-backward " \t\n")
	(skip-syntax-backward "w")
	(unless (looking-at "\\(\\sw+\\)[ \t\n]*\\sw+[ \t\n]*\\(((?\\)?")
	  ;; Looks like it was something else, so go back to where we
	  ;; were and reset the match data by rematching.
	  (goto-char pos)
	  (looking-at "[ \n\t*]*\\(\\sw+\\)[ \t\n]*\\(((?\\)?"))))
    (save-match-data
      (condition-case nil
	  (save-restriction
	    ;; Restrict to the LIMIT.
	    (narrow-to-region (point-min) limit)
	    (goto-char (match-end 1))
	    ;; Move over any item value, etc., to the next item.
	    (while (not (looking-at "[ \t\n]*\\(\\(,\\)\\|;\\|\\'\\)"))
	      (goto-char (or (scan-sexps (point) 1) (point-max))))
	    (if (match-end 2)
		(goto-char (match-end 2))))
	(error t)))))

;; C preprocessor(cpp) is used outside of C, C++ and Objective-C source file.
;; e.g. assembler code and GNU linker script in Linux kernel.
;; `cpp-font-lock-keywords' is handy for modes for the files.
;;
;; Here we cannot use `regexp-opt' because regex-opt is not preloaded
;; while font-lock.el is preloaded to emacs. So values pre-calculated with
;; regexp-opt are used here.

;; `cpp-font-lock-keywords-source-directives' is calculated from:
;;
;;	    (regexp-opt
;;	     '("define"  "elif" "else" "endif" "error" "file" "if" "ifdef"
;;	       "ifndef" "import" "include" "line" "pragma" "undef" "warning"))
;;
(defconst cpp-font-lock-keywords-source-directives
  "define\\|e\\(?:l\\(?:if\\|se\\)\\|ndif\\|rror\\)\\|file\\|i\\(?:f\\(?:n?def\\)?\\|mport\\|nclude\\)\\|line\\|pragma\\|undef\\|warning"
  "Regular expression used in `cpp-font-lock-keywords'.")

;; `cpp-font-lock-keywords-source-depth' is calculated from:
;;
;;          (regexp-opt-depth (regexp-opt
;;		       '("define"  "elif" "else" "endif" "error" "file" "if" "ifdef"
;;			 "ifndef" "import" "include" "line" "pragma" "undef" "warning")))
;;
(defconst cpp-font-lock-keywords-source-depth 0
  "Regular expression depth of `cpp-font-lock-keywords-source-directives'.
This should be an integer.  Used in `cpp-font-lock-keywords'.")

(defconst cpp-font-lock-keywords
  (let* ((directives cpp-font-lock-keywords-source-directives)
	 (directives-depth cpp-font-lock-keywords-source-depth))
    (list
     ;;
     ;; Fontify error directives.
     '("^#[ \t]*\\(?:error\\|warning\\)[ \t]+\\(.+\\)" 1 font-lock-warning-face prepend)
     ;;
     ;; Fontify filenames in #include <...> preprocessor directives as strings.
     '("^#[ \t]*\\(?:import\\|include\\)[ \t]*\\(<[^>\"\n]*>?\\)"
       1 font-lock-string-face prepend)
     ;;
     ;; Fontify function macro names.
     '("^#[ \t]*define[ \t]+\\([[:alpha:]_][[:alnum:]_$]*\\)("
       (1 font-lock-function-name-face prepend)
       ;;
       ;; Macro arguments.
       ((lambda (limit)
	  (re-search-forward
	   "\\(?:\\([[:alpha:]_][[:alnum:]_]*\\)[,]?\\)"
	   (or (save-excursion (re-search-forward ")" limit t))
	       limit)
	   t))
	nil nil (1 font-lock-variable-name-face prepend)))
     ;;
     ;; Fontify symbol names in #elif or #if ... defined preprocessor directives.
     '("^#[ \t]*\\(?:elif\\|if\\)\\>"
       ("\\<\\(defined\\)\\>[ \t]*(?\\([[:alpha:]_][[:alnum:]_]*\\)?" nil nil
	(1 font-lock-builtin-face prepend) (2 font-lock-variable-name-face prepend t)))
     ;;
     ;; Fontify otherwise as symbol names, and the preprocessor directive names.
     (list
      (concat "^\\(#[ \t]*\\(?:" directives
	      "\\)\\)\\>[ \t!]*\\([[:alpha:]_][[:alnum:]_]*\\)?")
      '(1 font-lock-preprocessor-face prepend)
      (list (+ 2 directives-depth)
	    'font-lock-variable-name-face nil t))))
  "Font lock keywords for C preprocessor directives.
`c-mode', `c++-mode' and `objc-mode' have their own font lock keywords
for C preprocessor directives.  This definition is for the other modes
in which C preprocessor directives are used, e.g. `asm-mode' and
`ld-script-mode'.")

(provide 'font-lock)

;;; font-lock.el ends here
