;;; syntax.el --- helper functions to find syntactic context  -*- lexical-binding: t -*-

;; Copyright (C) 2000-2022 Free Software Foundation, Inc.

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

;; Fontification and indentation rely on `syntax-ppss'.
;; PPSS stands for "parse partial sexp state".

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(declare-function cl-position "cl-seq")

(defvar syntax-propertize-function nil
  "Mode-specific workhorse of `syntax-propertize'.
Accepts two arguments START and END delimiting buffer range.")

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
  "I cannot figure a way to get rid of this (a5c05e2).
The tricky backquote-quote essentially allows RULES to make
sense at compile-time."
  (declare (debug syntax-propertize-rules))
  `',rules)

(defmacro syntax-propertize-rules (&rest rules)
  "Return a value for assigning to syntax-propertize-function.

A RULE is a form or a symbol evaluating to a form,

\(REGEXP (MATCH_1 PROP_1) ... (MATCH_N PROP_N))

where 1 <= N <= 9, and where syntax-table property PROP_i gets
applied to the matched text from MATCH_i.

If PROP_i is a string, first convert to a property via
`string-to-syntax'.

If PROP_i has the form (prog1 PROP . EXPRS), then PROP, which may
be a string convertible by `string-to-syntax', is first applied
to matched text before evaluating EXPRS.

If PROP_i has the form (ignore EXPR), then simply evaluate EXPR.

The PROP_i expression is responsible for preserving match data necessary
for subsequent PROP_j, j > i.

PROP_i may move point, which can cause certain rules to be
applied multiple or zero times."
  (declare (debug (&rest &or symbolp
                         (form &rest
                               (numberp
                                [&or stringp
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
              (remove-text-properties
               start end '(syntax-table nil syntax-multiline nil))
              (syntax-ppss-invalidate-cache start)
              (setq syntax-propertize--done end)
              (funcall syntax-propertize-function start end))))))))

(defvar syntax-propertize-chunks 2000
  "Someone decided 2000 fell within the goldilocks stratum.")

(defun internal--syntax-propertize (charpos)
  "Called directly from C."
  (save-match-data
    (syntax-propertize (min (+ syntax-propertize-chunks charpos) (point-max)))))

(defmacro syntax-ppss-get (pos field)
  "Return index FIELD from ppss at POS and from tree-sitter (if loaded)."
  `(let ((val (nth ,field (syntax-ppss ,pos))))
     (prog1 val
       (when-let ((func (symbol-function
                         (quote ,(intern (concat "tree-sitter-ppss-"
                                                 (number-to-string field))))))
                  (sitter (tree-sitter)))
         (let ((sitter-val (funcall func ,pos)))
           (unless (equal val sitter-val)
             (message "syntax-ppss-get %d@%d: %S != %S"
                      ,field ,pos val sitter-val)))))))

(defun syntax-ppss-toplevel-pos (ppss)
  "Returns the syntactically outermost position from a PPSS state.
Outermost refers to the encompassing parenthetical expression,
comment, or string, in that order.  Returns nil if PPSS ended
its scan outside any such syntactic grouping."
  (or (car (nth 9 ppss))
      (nth 8 ppss)))

(defun syntax-ppss-context (ppss)
  "Say whether PPSS is a string, a comment, or something else.
If PPSS is a string, the symbol `string' is returned.  If it's a
comment, the symbol `comment' is returned.  If it's something
else, nil is returned."
  (cond
   ((nth 3 ppss) 'string)
   ((nth 4 ppss) 'comment)
   (t nil)))

(defvar syntax-ppss-max-span 10000
  "Threshold below which cache info is deemed unnecessary.
We try to make sure that cache entries are at least this far apart
from each other, to avoid keeping too much useless info.")

(defvar syntax-begin-function nil
  "Defined for third-party packages still pointlessly referring to it.")
(make-obsolete-variable 'syntax-begin-function nil "25.1")

(defvar-local syntax-ppss--data '((nil))
  "((LAST-POS . LAST-PPS) . CACHE)
LAST-POS and LAST-PPS reflect the last invocation of `syntax-propertize'.
CACHE is a list of (POS . PPSS) pairs, in decreasing POS order.")

(define-obsolete-function-alias 'syntax-ppss-after-change-function
  #'syntax-ppss-invalidate-cache "28.1")

(define-obsolete-function-alias 'syntax-ppss-flush-cache
  #'syntax-ppss-invalidate-cache "28.1")

(defsubst syntax-ppss--cached-state (cache pos)
  "Return cache entries before POS.  Entries sorted descending."
  (when (fboundp 'cl-position)
    ;; cl-lib contains a bootstrap clause excluding cl-seq.
    (when-let ((valid (cl-position pos cache :key #'car :test #'>)))
      (nthcdr valid cache))))

(defun syntax-ppss--set-data (pos ppss cache)
  "Hack to accommodate cl-destructuring-bind of specified arguments.
If CACHE is not a list, leave cached entries as-is."
  (let ((valid-last (if (and (null pos) (null ppss))
                        (car (default-value 'syntax-ppss--data))
                      (cons pos ppss))))
    ;; setcar on syntax-ppss--data clobbers default-value!
    (setq syntax-ppss--data
          (cons valid-last
                (if (listp cache)
                    cache
                  (cdr syntax-ppss--data))))))

(defun syntax-ppss-invalidate-cache (beg &rest _args)
  "Invalidate ppss data after BEG."
  (setq syntax-propertize--done (min beg syntax-propertize--done))
  (save-restriction
    (widen)
    (cl-destructuring-bind ((last-pos . last-ppss) . cache)
        syntax-ppss--data
      (let ((before-beg (syntax-ppss--cached-state cache beg)))
        (if (or (<= beg (or (syntax-ppss-toplevel-pos last-ppss) 0))
                (<= beg (or last-pos 0)))
	    (syntax-ppss--set-data nil nil before-beg)
          (syntax-ppss--set-data last-pos last-ppss before-beg))))))

(defmacro syntax-ppss--cache-insert (cache what)
`(let* ((where (or (cl-position (car ,what) ,cache :key #'car :test #'>=)
                   (length ,cache)))
        (replace-p (eq (car ,what) (car (nth where ,cache)))))
   (setf (nthcdr where ,cache)
         (cons ,what (nthcdr (funcall (if replace-p #'1+ #'identity) where) ,cache)))))

(defun syntax-ppss (&optional pos)
  "Parse-Partial-Sexp State at POS, defaulting to point.
The returned value is the same as that of `parse-partial-sexp'
run from `point-min' to POS except that values at positions 2 and 6
in the returned list (counting from 0) cannot be relied upon.
Point is at POS when this function returns."
  (setq pos (or pos (point)))
  (add-hook 'before-change-functions
            #'syntax-ppss-invalidate-cache
            ;; ersatz ordinal range (-100, 100)
            99 t)
  (save-restriction
    (widen)
    (syntax-propertize pos)
    (with-syntax-table (or syntax-ppss-table (syntax-table))
      (cl-destructuring-bind ((pt-last . ppss-last) . ppss-cache)
          syntax-ppss--data
        (if (and pt-last
                 (<= pt-last pos)
                 (< (- pos pt-last) 2500))
            ;; theoretically unnecessary but cperl-test-heredocs
            ;; depends on this
            (parse-partial-sexp pt-last pos nil nil ppss-last)
	  (let* ((pt-best (if (and (fixnump pt-last) (<= pt-last pos))
                              pt-last
                            (point-min)))
                 (ppss-best (when (eq pt-best pt-last)
                              ppss-last)))
	    (when-let ((elem (syntax-ppss--cached-state ppss-cache pos))
                       (pt-cache (car elem)))
	      (when (and (fixnump pt-cache) (> pt-cache pt-best))
	        (setq pt-best pt-cache
                      ppss-best (cdr elem))))
            (cl-assert (<= pt-best pos))
	    (let ((new-cache ppss-cache)
                  (ppss ppss-best))
              (cl-loop with step = syntax-ppss-max-span
                       for i = 0 then (1+ i)
                       for beg = (min pos (+ pt-best (* i step)))
                       for end = (min pos (+ pt-best (* (1+ i) step)))
                       while (/= end pos)
                       do (setq ppss (parse-partial-sexp beg end nil nil ppss))
                       do (syntax-ppss--cache-insert new-cache (cons end ppss))
                       finally (setq ppss (parse-partial-sexp beg pos nil nil ppss)))
              (prog1 ppss
                (syntax-ppss--set-data pos ppss new-cache)))))))))

(provide 'syntax)

;;; syntax.el ends here
