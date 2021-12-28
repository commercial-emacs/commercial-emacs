;;; syntax.el --- helper functions to find syntactic context  -*- lexical-binding: t -*-

;; Copyright (C) 2000-2021 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal

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

;; The main exported function is `syntax-ppss'.
;; PPSS stands for "parse partial sexp state".

;;; Code:

(eval-when-compile (require 'cl-lib))
(declare-function cl-position "cl-seq")

(defvar syntax-propertize-function nil
  "Mode-specific workhorse of `syntax-propertize', which is called
by things like font-Lock and indentation.")

(defconst syntax-propertize-chunk-size 500)

(defvar-local syntax-propertize-extend-region-functions
  '(syntax-propertize-wholelines)
  "Special hook run just before proceeding to propertize a region.
This is used to allow major modes to help `syntax-propertize' find safe buffer
positions as beginning and end of the propertized region.  Its most common use
is to solve the problem of /identification/ of multiline elements by providing
a function that tries to find such elements and move the boundaries such that
they do not fall in the middle of one.
Each function is called with two arguments (START and END) and it should return
either a cons (NEW-START . NEW-END) or nil if no adjustment should be made.
These functions are run in turn repeatedly until they all return nil.
Put first the functions more likely to cause a change and cheaper to compute.")

(cl-defstruct (ppss
               (:constructor make-ppss)
               (:copier nil)
               (:type list))
  (depth nil :documentation "Depth in parens.")
  (innermost-start
   nil :documentation
   "Character address of start of innermost containing list; nil if none.")
  (last-complete-sexp-start
   nil :documentation
   "Character address of start of last complete sexp terminated.")
  (string-terminator nil :documentation "\
Non-nil if inside a string.
\(it is the character that will terminate the string, or t if the
string should be terminated by a generic string delimiter.)")
  (comment-depth nil :documentation "\
nil if outside a comment, t if inside a non-nestable comment,
else an integer (the current comment nesting).")
  (quoted-p nil :documentation "t if following a quote character.")
  (min-depth
   nil :documentation "The minimum depth in parens encountered during this scan.")
  (comment-style nil :documentation "Style of comment, if any.")
  (comment-or-string-start
   nil :documentation
   "Character address of start of comment or string; nil if not in one.")
  (open-parens
   nil :documentation
   "List of positions of currently open parens, outermost first.")
  (two-character-syntax nil :documentation "\
When the last position scanned holds the first character of a
\(potential) two character construct, the syntax of that position,
otherwise nil.  That construct can be a two character comment
delimiter or an Escaped or Char-quoted character."))

(defun syntax-propertize-wholelines (start end)
  "Extend the region delimited by START and END to whole lines.
This function is useful for
`syntax-propertize-extend-region-functions';
see Info node `(elisp) Syntax Properties'."
  (goto-char start)
  (cons (line-beginning-position)
        (progn (goto-char end)
               (if (bolp) (point) (line-beginning-position 2)))))

(defun syntax-propertize-multiline (beg end)
  "Let `syntax-propertize' pay attention to the syntax-multiline property."
  (when (and (> beg (point-min))
	     (get-text-property (1- beg) 'syntax-multiline))
    (setq beg (or (previous-single-property-change beg 'syntax-multiline)
		  (point-min))))
  ;;
  (when (get-text-property end 'syntax-multiline)
    (setq end (or (text-property-any end (point-max)
				     'syntax-multiline nil)
		  (point-max))))
  (cons beg end))

(defun syntax-propertize--shift-groups-and-backrefs (re n)
  (let ((new-re (replace-regexp-in-string
                 "\\\\(\\?\\([0-9]+\\):"
                 (lambda (s)
                   (replace-match
                    (number-to-string
                     (+ n (string-to-number (match-string 1 s))))
                    t t s 1))
                 re t t))
        (pos 0))
    (while (string-match "\\\\\\([0-9]+\\)" new-re pos)
      (setq pos (+ 1 (match-beginning 1)))
      (when (save-match-data
              ;; With \N, the \ must be in a subregexp context, i.e.,
              ;; not in a character class or in a \{\} repetition.
              (subregexp-context-p new-re (match-beginning 0)))
        (let ((shifted (+ n (string-to-number (match-string 1 new-re)))))
          (when (> shifted 9)
            (error "There may be at most nine back-references"))
          (setq new-re (replace-match (number-to-string shifted)
                                      t t new-re 1)))))
    new-re))

(defmacro syntax-propertize-precompile-rules (&rest rules)
  "Return a precompiled form of RULES to pass to `syntax-propertize-rules'.
The arg RULES can be of the same form as in `syntax-propertize-rules'.
The return value is an object that can be passed as a rule to
`syntax-propertize-rules'.
I.e. this is useful only when you want to share rules among several
`syntax-propertize-function's."
  (declare (debug syntax-propertize-rules))
  ;; Precompile?  Yeah, right!
  ;; Seriously, tho, this is a macro for 2 reasons:
  ;; - we could indeed do some pre-compilation at some point in the future,
  ;;   e.g. fi/when we switch to a DFA-based implementation of
  ;;   syntax-propertize-rules.
  ;; - this lets Edebug properly annotate the expressions inside RULES.
  `',rules)

(defmacro syntax-propertize-rules (&rest rules)
  "Make a function that applies RULES for use in `syntax-propertize-function'.
The function will scan the buffer, applying the rules where they match.
The buffer is scanned a single time, like \"lex\" would, rather than once
per rule.

Each RULE can be a symbol, in which case that symbol's value should be,
at macro-expansion time, a precompiled set of rules, as returned
by `syntax-propertize-precompile-rules'.

Otherwise, RULE should have the form (REGEXP HIGHLIGHT1 ... HIGHLIGHTn), where
REGEXP is an expression (evaluated at time of macro-expansion) that returns
a regexp, and where HIGHLIGHTs have the form (NUMBER SYNTAX) which means to
apply the property SYNTAX to the chars matched by the subgroup NUMBER
of the regular expression, if NUMBER did match.
SYNTAX is an expression that returns a value to apply as `syntax-table'
property.  Some expressions are handled specially:
- if SYNTAX is a string, then it is converted with `string-to-syntax';
- if SYNTAX has the form (prog1 EXP . EXPS) then the value returned by EXP
  will be applied to the buffer before running EXPS and if EXP is a string it
  is also converted with `string-to-syntax'.
The SYNTAX expression is responsible to save the `match-data' if needed
for subsequent HIGHLIGHTs.
Also SYNTAX is free to move point, in which case RULES may not be applied to
some parts of the text or may be applied several times to other parts.

Note: There may be at most nine back-references in the REGEXPs of
all RULES in total."
  (declare (debug (&rest &or symbolp    ;FIXME: edebug this eval step.
                         (form &rest
                               (numberp
                                [&or stringp ;FIXME: Use &wrap
                                     ("prog1" [&or stringp def-form] def-body)
                                     def-form])))))
  (let ((newrules nil))
    (while rules
      (if (symbolp (car rules))
          (setq rules (append (symbol-value (pop rules)) rules))
        (push (pop rules) newrules)))
    (setq rules (nreverse newrules)))
  (let* ((offset 0)
         (branches '())
         ;; We'd like to use a real DFA-based lexer, usually, but since Emacs
         ;; doesn't have one yet, we fallback on building one large regexp
         ;; and use groups to determine which branch of the regexp matched.
         (re
          (mapconcat
           (lambda (rule)
             (let* ((orig-re (eval (car rule) t))
                    (re orig-re))
               (when (and (assq 0 rule) (cdr rules))
                 ;; If there's more than 1 rule, and the rule want to apply
                 ;; highlight to match 0, create an extra group to be able to
                 ;; tell when *this* match 0 has succeeded.
                 (cl-incf offset)
                 (setq re (concat "\\(" re "\\)")))
               (setq re (syntax-propertize--shift-groups-and-backrefs re offset))
               (let ((code '())
                     (condition
                      (cond
                       ((assq 0 rule) (if (zerop offset) t
                                        `(match-beginning ,offset)))
                       ((and (cdr rule) (null (cddr rule)))
                        `(match-beginning ,(+ offset (car (cadr rule)))))
                       (t
                        `(or ,@(mapcar
                                (lambda (case)
                                  `(match-beginning ,(+ offset (car case))))
                                (cdr rule))))))
                     (nocode t)
                     (offset offset))
                 ;; If some of the subgroup rules include Elisp code, then we
                 ;; need to set the match-data so it's consistent with what the
                 ;; code expects.  If not, then we can simply use shifted
                 ;; offset in our own code.
                 (unless (zerop offset)
                   (dolist (case (cdr rule))
                     (unless (stringp (cadr case))
                       (setq nocode nil)))
                   (unless nocode
                     (push `(let ((md (match-data 'ints)))
                              ;; Keep match 0 as is, but shift everything else.
                              (setcdr (cdr md) (nthcdr ,(* (1+ offset) 2) md))
                              (set-match-data md))
                           code)
                     (setq offset 0)))
                 ;; Now construct the code for each subgroup rules.
                 (dolist (case (cdr rule))
                   (cl-assert (null (cddr case)))
                   (let* ((gn (+ offset (car case)))
                          (action (nth 1 case))
                          (thiscode
                           (cond
                            ((stringp action)
                             `((put-text-property
                                (match-beginning ,gn) (match-end ,gn)
                                'syntax-table
                                ',(string-to-syntax action))))
                            ((eq (car-safe action) 'ignore)
                             (cdr action))
                            ((eq (car-safe action) 'prog1)
                             (if (stringp (nth 1 action))
                                 `((put-text-property
                                    (match-beginning ,gn) (match-end ,gn)
                                    'syntax-table
                                    ',(string-to-syntax (nth 1 action)))
                                   ,@(nthcdr 2 action))
                               `((let ((mb (match-beginning ,gn))
                                       (me (match-end ,gn)))
                                   ,(macroexp-let2 nil syntax (nth 1 action)
                                      `(progn
                                         (if ,syntax
                                             (put-text-property
                                              mb me 'syntax-table ,syntax))
                                         ,@(nthcdr 2 action)))))))
                            (t
                             `((let ((mb (match-beginning ,gn))
                                     (me (match-end ,gn))
                                     (syntax ,action))
                                 (if syntax
                                     (put-text-property
                                      mb me 'syntax-table syntax))))))))

                     (if (or (not (cddr rule)) (zerop gn))
                         (setq code (nconc (nreverse thiscode) code))
                       (push `(if (match-beginning ,gn)
                                  ;; Try and generate clean code with no
                                  ;; extraneous progn.
                                  ,(if (null (cdr thiscode))
                                       (car thiscode)
                                     `(progn ,@thiscode)))
                             code))))
                 (push (cons condition (nreverse code))
                       branches))
               (cl-incf offset (regexp-opt-depth orig-re))
               re))
           rules
           "\\|")))
    `(lambda (start end)
       (goto-char start)
       (while (and (< (point) end)
                   (re-search-forward ,re end t))
         (cond ,@(nreverse branches))))))

(defun syntax-propertize-via-font-lock (keywords)
  "Propertize for syntax using font-lock syntax.
KEYWORDS obeys the format used in `font-lock-syntactic-keywords'.
The return value is a function (with two parameters, START and
END) suitable for `syntax-propertize-function'."
  (lambda (start end)
    (with-no-warnings
      (let ((font-lock-syntactic-keywords keywords))
        (font-lock-fontify-syntactic-keywords-region start end)
        ;; In case it was eval'd/compiled.
        (setq keywords font-lock-syntactic-keywords)))))

(defvar-local syntax-ppss-table nil
  "Syntax-table to use during `syntax-ppss', if any.")

(defun syntax-propertize (pos)
  "Ensure that syntax-table properties are set through POS."
  (if (not (functionp syntax-propertize-function))
      (setq syntax-propertize--done (point-max))
    (when (< syntax-propertize--done pos)
      (setq-local parse-sexp-lookup-properties t)
      (save-excursion
        (with-silent-modifications
          (with-syntax-table (or syntax-ppss-table (syntax-table))
            (let* ((start (max (min syntax-propertize--done
                                    (point-max))
                               (point-min)))
                   (end (max (min (+ start syntax-propertize-chunk-size)
                                  (point-max))
                             pos)))
              (run-hook-wrapped
               'syntax-propertize-extend-region-functions
               (lambda (f &rest _args)
                 (when-let ((new (funcall f start end)))
                   (setq start (min start (car new)))
                   (setq end (max end (cdr new))))))
              (remove-text-properties start end
                                      '(syntax-table nil syntax-multiline nil))
              (syntax-ppss-invalidate-cache start)
              (setq syntax-propertize--done end)
              (funcall syntax-propertize-function start end))))))))

;;; Link syntax-propertize with syntax.c.

(defvar syntax-propertize-chunks
  ;; We're not sure how far we'll go.  In my tests, using chunks of 2000
  ;; brings the overhead to something negligible.  Passing ‘charpos’ directly
  ;; also works (basically works line-by-line) but results in an overhead which
  ;; I thought was a bit too high (like around 50%).
  2000)

(defun internal--syntax-propertize (charpos)
  ;; FIXME: Called directly from C.
  (save-match-data
    (syntax-propertize (min (+ syntax-propertize-chunks charpos) (point-max)))))

;;; Incrementally compute and memoize parser state.

(defsubst syntax-ppss-depth (ppss)
  (nth 0 ppss))

(defun syntax-ppss-toplevel-pos (ppss)
  "Returns the syntactically outermost position from a PPSS state.
Outermost refers to the encompassing parenthetical expression,
comment, or string, in that order.  Returns nil if PPSS ended
its scan outside any such syntactic grouping."
  (or (car (nth 9 ppss))
      (nth 8 ppss)))

(defsubst syntax-ppss-context (ppss)
  "Say whether PPSS is a string, a comment, or something else.
If PPSS is a string, the symbol `string' is returned.  If it's a
comment, the symbol `comment' is returned.  If it's something
else, nil is returned."
  (cond
   ((nth 3 ppss) 'string)
   ((nth 4 ppss) 'comment)
   (t nil)))

(defvar syntax-ppss-max-span 20000
  "Threshold below which cache info is deemed unnecessary.
We try to make sure that cache entries are at least this far apart
from each other, to avoid keeping too much useless info.")

(make-obsolete-variable 'syntax-begin-function nil "25.1")

(defvar-local syntax-ppss-wide nil
  "((LAST-POS . LAST-PPS) . CACHE)
LAST-POS and LAST-PPS reflect the last invocation of `syntax-propertize'.
CACHE is a list of (POS . PPSS) pairs, in decreasing POS order.")

(defvar-local syntax-ppss-narrow nil
  "Same as `syntax-ppss-wide' but for a narrowed buffer.
This was a bad idea.")

(defvar-local syntax-ppss-narrow-start nil
  "Start position of the narrowing for `syntax-ppss-narrow'.")

(define-obsolete-function-alias 'syntax-ppss-after-change-function
  #'syntax-ppss-invalidate-cache "28.1")

(define-obsolete-function-alias 'syntax-ppss-flush-cache
  #'syntax-ppss-invalidate-cache "28.1")

(defun syntax-ppss-invalidate-cache (beg &rest _args)
  "Invalidate ppss data after BEG."
  (setq syntax-propertize--done (min beg syntax-propertize--done))
  (cl-destructuring-bind ((last-pos . last-ppss) . cache)
      (syntax-ppss--data)
    (if (<= beg (or (syntax-ppss-toplevel-pos last-ppss) 0))
	(setcar (syntax-ppss--data) nil)
      (when (< beg (or last-pos 0))
        (setcar (syntax-ppss--data) (cons nil last-ppss))))
    ;; cl-lib contains a bootstrap (temacs) clause excluding cl-seq.
    (when (fboundp 'cl-position)
      (setcdr
       (syntax-ppss--data)
       ;; first sorted (desc) cache entry whose pos is less than BEG
       (when-let ((valid (cl-position beg cache :key #'car :test #'>)))
         (nthcdr valid cache))))))

(defun syntax-ppss--data ()
  (if (eq (point-min) 1)
      (setq syntax-ppss-wide (or syntax-ppss-wide '((nil))))
    (unless (eq (point-min) syntax-ppss-narrow-start)
      (setq syntax-ppss-narrow-start (point-min)
            syntax-ppss-narrow nil))
    (setq syntax-ppss-narrow (or syntax-ppss-narrow '((nil))))))

(defun syntax-ppss (&optional pos)
  "Parse-Partial-Sexp State at POS, defaulting to point.
The returned value is the same as that of `parse-partial-sexp'
run from `point-min' to POS except that values at positions 2 and 6
in the returned list (counting from 0) cannot be relied upon.
Point is at POS when this function returns."
  (setq pos (or pos (point)))
  (syntax-propertize pos)
  (with-syntax-table (or syntax-ppss-table (syntax-table))
    (cl-destructuring-bind ((pt-last . ppss-last) . ppss-cache)
        (syntax-ppss--data)
      (condition-case nil
	  (cond ((and pt-last
                      (<= pt-last pos)
                      (< (- pos pt-last) 2500))
	         (parse-partial-sexp pt-last pos nil nil ppss-last))
	        ((and ppss-last
                      (when-let ((pt-top (or (syntax-ppss-toplevel-pos ppss-last)
				             (nth 2 ppss-last))))
		        (and (<= pt-top pos)
                             (< (- pos pt-top) syntax-ppss-max-span))))
	         (let ((ppss (parse-partial-sexp
                              (or (syntax-ppss-toplevel-pos ppss-last)
				  (nth 2 ppss-last))
                              pos)))
                   (prog1 ppss
                     (setcar (syntax-ppss--data) (cons pos ppss)))))
	        (t
	         (when (and (not ppss-cache) (not ppss-last))
		   (add-hook 'before-change-functions
                             #'syntax-ppss-invalidate-cache
                             ;; ersatz ordinal range (-100, 100)
                             99 t))
	         (let* ((pt-best (if (and (fixnump pt-last) (<= pt-last pos))
                                     pt-last
                                   (point-min)))
                        (ppss-best (when (eq pt-best pt-last)
                                     ppss-last)))
                   ;; first sorted (desc) cache entry before POS
	           (when-let ((valid (cl-position pos ppss-cache :key #'car :test #'>))
                              (elem (nth valid ppss-cache))
                              (pt-cache (car elem)))
	             (when (and (fixnump pt-cache) (> pt-cache pt-best))
		       (setq pt-best pt-cache
                             ppss-best (cdr elem))))

                   (cl-assert (<= pt-best pos))
	           (if (< (- pos pt-best) syntax-ppss-max-span)
                       (let ((ppss (parse-partial-sexp pt-best pos nil nil ppss-best)))
                         (prog1 ppss
                           (setcar (syntax-ppss--data) (cons pos ppss))))
                     (let ((new-cache ppss-cache)
                           (ppss ppss-best))
                       (dotimes (i (/ (- pos pt-best) syntax-ppss-max-span))
                         (let* ((beg (+ pt-best (* i syntax-ppss-max-span)))
                                (end (+ pt-best (* (1+ i) syntax-ppss-max-span)))
                                (before (cl-position beg new-cache :key #'car :test #'>)))
	                   (setq ppss (parse-partial-sexp beg end nil nil ppss))
                           (if (fixnump before)
                               (cl-fill new-cache (cons beg ppss) :start before)
                             (push (cons beg ppss) new-cache))))
                       (prog1 ppss
                         (setcar (syntax-ppss--data) (cons pos ppss))
                         (setcdr (syntax-ppss--data) new-cache)))))))
        (args-out-of-range
         ;; Narrowed buffers; naively parse from point-min.
         (parse-partial-sexp (point-min) pos))))))

(provide 'syntax)

;;; syntax.el ends here
