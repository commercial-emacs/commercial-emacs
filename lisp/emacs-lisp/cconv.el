;;; cconv.el --- Closure conversion for statically scoped Emacs Lisp. -*- lexical-binding: t -*-

;; Copyright (C) 2011-2024 Free Software Foundation, Inc.

;; Author: Igor Kuzmin <kzuminig@iro.umontreal.ca>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: lisp
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

;; The user entry point is `cconv-closure-convert'.  All macros should
;; be expanded beforehand.
;;
;; First `cconv-analyze-form' identifies mutated variables (updated
;; by setq), variables captured by closure, and functions amenable to
;; lambda lifting.

;; Then `cconv-convert' rewrites the parse tree as follows:
;;
;; 1. Lifts lambdas where possible,
;; (lambda (v0) fv0 fv1) => ; fv are free variables
;;   (lambda (v0 fv0 fv1) fv0 fv1)
;;
;; 2. Builds closures where needed,
;; (lambda (v0) fv0 fv1) => ; fv are free variables
;;   (internal-make-closure (v0) (fv0) <doc>
;;    (internal-get-closed-var 0) (internal-get-closed-var 1))
;;
;; 3. Eliminates mutable variable that is also captured by closure.
;;            val => (list val)
;;            var => (car-safe var)
;; (setq var val) => (setcar var val)
;;
;; 4. Similarly wraps closure-mutable formal args in defuns.
;; (defun foo (mut) ...) =>
;; (defun foo (mut) (let ((mut (list mut))) ...))
;;
;;; Code:

;; TODO: (not just for cconv but lexbind generally)
;; - In edebug, find lexical bindings from the stack.
;; - Make eval-region do the eval-sexp-add-defvars dance.
;; - Apply byte-optimize-form before cconv (shift the "unused
;;   variable" warnings from cconv-analyze earlier to account for
;;   optimized away variables.
;; - Compiler hint for macros tying let-bindings to the same source,
;;   so that unused variable warnings take all uses into account.
;; - Interactive specs which return a function to construct the args.
;; - Standardize macro-expand so we don't have to handle (let (var) body)
;;   and other oddities.
;; - New byte codes for unwind-protect so that closures aren't needed at all.
;; - Static knowledge of a variable's unchangingness could be passed
;;   to byte-compiler, e.g. by using a new `immutable-let'.
;; - Call known non-escaping functions with `goto' rather than `call'.

(eval-when-compile (require 'cl-lib))

(defvar byte-compile-lexical-variables)
(defvar byte-compile-bound-variables)

(defconst cconv-lift-below 7
  "Combined number of arguments and free variables below which
lambda lifting attempted.")

(defvar cconv--results nil
  "Alist of (((VAR VAL) . PARENT-FORM) . REASON) entries.
REASON is one of :cconv-unused, :cconv-lift, or :cconv-mutcap.")

(defvar cconv--fv-alist nil
  "Alist of (FUNBODY . (FREE-VARIABLES)).")

(defvar cconv--interactive-form-funs
  (make-hash-table :test #'eq :weakness 'key)
  "Functions internally created for interactive forms.")

(defvar cconv--dynvars-at-large nil
  "Known to be dynamic but not yet `special-variable-p'.
Recall closure conversion is a pre-compilation step.")

(defvar cconv--dynvars-seen nil
  "Dynamic variables encountered during closure conversion.")

;;;###autoload
(defun cconv-closure-convert (form)
  "Returns FORM with free variables removed from lambdas."
  (let ((cconv--dynvars-at-large byte-compile-bound-variables)
	cconv--fv-alist cconv--results)
    (cconv-analyze-form form nil)
    (setq cconv--fv-alist (nreverse cconv--fv-alist))
    (prog1 (cconv-convert form nil nil) ; Env initially empty.
      (cl-assert (null cconv--fv-alist)))))

(defun cconv--set-diff (s1 s2)
  "Return elements of set S1 that are not in set S2."
  (let (res)
    (dolist (x s1)
      (unless (memq x s2) (push x res)))
    (nreverse res)))

(defun cconv--set-diff-map (s m)
  "Return elements of set S that are not in Dom(M)."
  (let (res)
    (dolist (x s)
      (unless (assq x m) (push x res)))
    (nreverse res)))

(defun cconv--map-diff (m1 m2)
  "Return the submap of map M1 that has Dom(M2) removed."
  (let (res)
    (dolist (x m1)
      (unless (assq (car x) m2) (push x res)))
    (nreverse res)))

(defun cconv--map-diff-elem (m x)
  "Return the map M minus any mapping for X."
  ;; Assume X appears at most once in M.
  (let* ((b (assq x m))
         (res (if b (remq b m) m)))
    (prog1 res
      (cl-assert (null (assq x res))))))

(defun cconv--map-diff-set (m s)
  "Return the map M minus any mapping for elements of S."
  ;; Assume X appears at most once in M.
  (let (res)
    (dolist (b m)
      (unless (memq (car b) s) (push b res)))
    (nreverse res)))

(defun cconv--convert-function (args body env parent-form &optional docstring)
  "Return closure referencing only those free variables used."
  (cl-assert (equal body (car (cl-first cconv--fv-alist))))
  (cl-destructuring-bind (body . fvs)
      (pop cconv--fv-alist)
    (let ((i 0) used-formal used-body used-env)
      ;; OClosure hack: nreverse puts most closely bound variables first.
      (dolist (fv (nreverse fvs))
        (let ((exp (or (cdr (assq fv env)) fv)))
          (pcase exp
            (`(car-safe ,iexp . ,_)
             (push iexp used-formal)
             (push `(,fv . (car-safe (internal-get-closed-var ,i))) used-env))
            (_
             (push exp used-formal)
             (push `(,fv . (internal-get-closed-var ,i)) used-env))))
        (cl-incf i))
      (setq used-formal (nreverse used-formal)
            used-env (nreverse used-env)
            used-body (cconv--convert-funbody args body used-env parent-form))
      (if (or used-formal docstring)
          `(internal-make-closure ,args ,used-formal ,docstring . ,used-body)
        ;; No free variables - do nothing.
        `(function (lambda ,args . ,used-body))))))

(defun cconv--remap-llv (new-env var closed-sym)
  ;; Consider,
  ;;   (let* ((fun (lambda (x) (+ x y))) (y 1)) (funcall fun 1))
  ;; A naive lambda lift would return,
  ;;   (let* ((fun (lambda (y x) (+ x y))) (y 1)) (funcall fun y 1))
  ;; where the outer Y is mistakenly captured by the inner Y.
  ;; So we instead rewrite as,
  ;;   (let* ((closed-y y) (fun (lambda (y x) (+ x y))) (y 1))
  ;;     (funcall fun closed-y 1))
  ;; which we do even in the absence of a FUN since avoiding it
  ;; would require traversing the tree twice.
  (mapcar (lambda (mapping)
            (if (not (eq (cadr mapping) #'apply-partially))
                mapping
              (cl-assert (eq (car mapping) (nth 2 mapping)))
              `(,(car mapping)
                apply-partially
                ,(car mapping)
                ,@(mapcar (lambda (arg)
                            (if (eq var arg)
                                closed-sym
                              arg))
                          (nthcdr 3 mapping)))))
          new-env))

(defun cconv--warn-unused-msg (var varkind)
  (when (and (intern-soft var)
             (not (eq ?_ (aref (symbol-name var) 0))))
    (let ((suggestions (help-uni-confusable-suggestions (symbol-name var))))
      (format "Unused lexical %s `%S'%s"
              varkind var (if suggestions
                              (concat "\n  " suggestions)
                            "")))))

(define-inline cconv--result (var-val parent-form)
  (inline-quote
   (cdr (assoc (cons ,var-val ,parent-form) cconv--results))))

(defun cconv--convert-funbody (args body env parent-form)
  "ENV would not include ARGS."
  (let (wrappers)
    (dolist (arg args)
      (pcase (cconv--result (list arg) parent-form)
        (:cconv-mutcap
         (push `(,arg . (car-safe ,arg)) env)
         (push (lambda (body) `(let ((,arg (list ,arg))) ,body)) wrappers))
        ((and :cconv-unused
              (let (and (pred stringp) msg)
                (cconv--warn-unused-msg arg "argument")))
         (when (assq arg env)
           (push `(,arg . nil) env)) ;needed?
         (push (lambda (body) (macroexp--warn-wrap msg body 'lexical)) wrappers))
        (_
         (when (assq arg env)
           (push `(,arg . nil) env)))))
    (setq body (mapcar (lambda (form) (cconv-convert form env nil)) body))
    (if wrappers
        (pcase-let ((`(,decls . ,body) (macroexp-parse-body body)))
          (let ((body (macroexp-progn body)))
            (dolist (wrapper wrappers) (setq body (funcall wrapper body)))
            `(,@decls ,@(macroexp-unprogn body))))
      body)))

(defun cconv--lifted-arg (var env)
  "Return argument to use for VAR in λ-lifted calls."
  (let ((mapping (cdr (assq var env))))
    (pcase-exhaustive mapping
      (`(internal-get-closed-var . ,_)
       ;; VAR is captured.
       mapping)
      (`(car-safe ,exp)
       ;; VAR is mutably captured; unwrap since VAR is passed "by
       ;; reference" to the λ-lifted function.
       exp)
      (_
       ;; VAR is not captured; use its shadowed value (if MAPPING were
       ;; `(car-safe EXP)`, EXP would always be VAR).
       var))))

(defmacro cconv-convert--let-shadow (var env new-env new-extend new-var-vals)
  (declare (indent defun))
  `(when (memq ,var ,new-extend)
     ;; VAR is shadowed.  Add a reference to the occluded binding.
     (let ((var-def (cconv--lifted-arg ,var ,env))
           (closed-sym (make-symbol (format "closed-%s" ,var))))
       (setq ,new-env (cconv--remap-llv ,new-env ,var closed-sym))
       ;; Adding CLOSED-SYM while unnecessary facilitates
       ;; writing the apply-partially assertion above.
       (setq ,new-extend (cons closed-sym (remq ,var ,new-extend)))
       (push `(,closed-sym ,var-def) ,new-var-vals))))

(defun cconv-convert (form env extend)
  "Return version of FORM having closed all its lambdas.
Entries in ENV take one of the following forms:
 (VAR . (car-safe EXP))
   EXP evaluates to a cons cell whose car is VAR.
 (VAR . (internal-get-closed-var N))
   VAR was copied to the closure environment's Nth slot.
 (VAR . (apply-partially F ARG1 ARG2 ..))
   VAR was λ-lifted and takes additional arguments ARGs.
 (VAR . nil)
   VAR was shadowed.
Incomprehensible English failed to describe EXTEND, a cache
of variables for referencing in arbitrary scopes."
  (cl-assert
   (null (delq nil (mapcar (lambda (mapping)
                             (when (eq (cadr mapping) #'apply-partially)
                               (cconv--set-diff (cdr (cddr mapping)) extend)))
                           env))))
  ;; Free variables versus environments.  Consider,
  ;;   (lambda (..) fvr (let ((fvr 1)) (+ fvr 1)))
  ;; where only the first FVR should be replaced by (aref env ...).
  ;; Initially free variables and envs are identical.  Then parsing
  ;; of `let` deletes FVR from the former since variable order only
  ;; matters in envs.  Incomprehensible English followed.
  (pcase form
    (`(,(and letsym (or 'let* 'let)) ,var-vals . ,body)
     (let ((new-env env)
           (new-extend extend)
           new-var-vals)
       (dolist (var-val var-vals)
         (unless (consp var-val)
           (setq var-val (list var-val)))
         (when (cddr var-val)
           (byte-compile-warn
            "Malformed `%S' binding: %S"
            letsym var-val))
         (let ((var (car var-val))
               (val (cadr var-val)))
           (cond
            ((not (symbolp var))
             (byte-compile-warn "attempt to let-bind nonvariable `%S'" var))
            ((or (booleanp var) (keywordp var))
             (byte-compile-warn "attempt to let-bind constant `%S'" var))
            (t
             (let ((new-val
		    (pcase (cconv--result var-val form)
                      ((and :cconv-lift
                            (guard
                             (progn
                               (cl-assert (and (eq (car val) 'function)
                                               (eq (car (cadr val)) 'lambda)))
                               (cl-assert (equal (cddr (cadr val))
                                                 (caar cconv--fv-alist)))
                               (let* ((fvs (cdr (car cconv--fv-alist)))
                                      (fun (cadr val))
                                      (funargs (cadr fun))
                                      (funvars (append fvs funargs)))
                                 (and fvs (< (length funvars) cconv-lift-below))))))
                       ;; VAR can be lambda lifted
                       (let* ((fvs (cdr (pop cconv--fv-alist)))
                              (fun (cadr val))
                              (funargs (cadr fun))
                              (funvars (append fvs funargs))
                              (funbody (cddr fun))
                              funbody-env)
                         (push `(,var . (apply-partially ,var . ,fvs)) new-env)
                         (dolist (fv fvs)
                           (cl-pushnew fv new-extend)
                           (when (and (eq 'car-safe (car-safe (cdr (assq fv env))))
                                      (not (memq fv funargs)))
                             (push `(,fv . (car-safe ,fv)) funbody-env)))
                         `(function
                           (lambda ,funvars
                             . ,(cconv--convert-funbody
                                 funargs funbody funbody-env val)))))
                      (:cconv-mutcap
                       ;; Turn into a ref-cell.
                       (push `(,var . (car-safe ,var)) new-env)
                       `(list ,(cconv-convert val env extend)))
                      (:cconv-unused
                       ;; Turn into a ref-cell.
                       (when (assq var new-env)
                         (push `(,var) new-env)) ;needed?
                       (let ((newval `(ignore ,(cconv-convert val env extend)))
                             (msg (cconv--warn-unused-msg var "variable")))
                         (if msg
                             (macroexp--warn-wrap msg newval 'lexical)
                           newval)))
                      (_
                       ;; Normal case.
                       (when (assq var new-env)
                         (push `(,var) new-env))
                       (cconv-convert val env extend)))))
               (when (eq letsym 'let*)
                 ;; Analogous treatment for `let` below (Bug#24171).
                 (cconv-convert--let-shadow var env new-env new-extend new-var-vals)
                 (setq env new-env
                       extend new-extend))
               (push `(,var ,new-val) new-var-vals))))))
       (when (eq letsym 'let)
         ;; Analogous treatment for `let*` above (Bug#24171).
         (dolist (var-val new-var-vals)
           (cconv-convert--let-shadow
             (car-safe var-val) env new-env new-extend new-var-vals)))
       `(,letsym ,(nreverse new-var-vals)
                 . ,(mapcar (lambda (form)
                              (cconv-convert
                               form new-env new-extend))
                            body))))
    (`(,(and `(lambda . ,_) fun) . ,args)
     ;; FIXME: it's silly to create a closure just to call it.
     ;; Running byte-optimize-form earlier would resolve this.
     `(funcall
       ,(cconv-convert `(function ,fun) env extend)
       ,@(mapcar (lambda (form)
                   (cconv-convert form env extend))
                 args)))
    (`(cond . ,cond-forms)
     `(cond . ,(mapcar (lambda (branch)
                         (mapcar (lambda (form)
                                   (cconv-convert form env extend))
                                 branch))
                       cond-forms)))
    (`(function (lambda ,args . ,body) . ,rest)
     (let* ((docstring (if (eq :documentation (car-safe (car body)))
                           (cconv-convert (cadr (pop body)) env extend)))
            (body-form (if (stringp (car body)) (cdr body) body))
            (iact-form (when (eq 'interactive (car-safe (car body-form)))
                         (gethash form cconv--interactive-form-funs)))
            (wrapped (pcase iact-form (`#'(lambda (&rest _cconv--dummy) .,_) t)))
            (conv-iact-form (when iact-form (cconv-convert iact-form env extend))))
       ;; TODO: This gets ugly because we non-destructively modify body.
       ;; This should ideally be moved to cconv--convert-function.
       (pcase conv-iact-form
         ('nil (setq body-form nil))
         (`#',f
          (pcase-let ((`((,f1 . (,_ . ,f2)) . ,f3) body-form))
            (setq body-form `((,f1 . (,(if wrapped (nth 2 f) conv-iact-form)
                                      . ,f2))
                              . ,f3)))
          (setq conv-iact-form nil))
         ;; The interactive form needs special treatment, so the form
         ;; inside the `interactive' won't be used any further.
         (_ (pcase-let ((`((,f1 . (,_ . ,f2)) . ,f3) body-form))
              (setq body-form `((,f1 . (nil . ,f2)) . ,f3)))))
       (when body-form
         ;; If we modified BODY-FORM, rebuild BODY and FORM as
         ;; copies with the modified bits.
         (setq body (if (stringp (car body))
                        (cons (car body) body-form)
                      body-form)
               form `(function (lambda ,args . ,body) . ,rest))
         ;; And replace free variables alist entry
         (let ((entry (pop cconv--fv-alist)))
           (push (cons body (cdr entry)) cconv--fv-alist)))
       (let ((conv-form (cconv--convert-function args body env form docstring)))
         (if conv-iact-form
             `(cconv--interactive-helper
               ,conv-form
               ,(if wrapped conv-iact-form `(list 'quote ,conv-iact-form)))
           ;; Normal case, the interactive form needs no special treatment.
           conv-form))))
    (`(internal-make-closure . ,_)
     (defvar byte-compile-abort-elc)
     (prog1 nil
       (setq byte-compile-abort-elc t)
       (byte-compile-warn "%s" "Internal error in compiler: cconv called twice?")))
    (`(quote . ,_) form)
    (`(function . ,_) form)
    (`(,(and sym (or 'defconst 'defvar)) ,definedsymbol . ,forms)
     `(,sym ,definedsymbol
            . ,(when (consp forms)
                 (cons (cconv-convert (car forms) env extend)
                       ;; The rest (i.e. docstring, of any) is not evaluated,
                       ;; and may be an invalid expression (e.g. ($# . 678)).
                       (cdr forms)))))
    (`(condition-case ,var ,protected-form . ,handlers)
     (let* ((reason (and var (cconv--result (list var) form)))
            (new-env (cond ((eq reason :cconv-mutcap)
                            (cons `(,var . (car-safe ,var)) env))
                           ((assq var env) (cons `(,var) env))
                           (t env)))
            (msg (when (eq reason :cconv-unused)
                   (cconv--warn-unused-msg var "variable")))
            (new-protform (cconv-convert protected-form env extend)))
       `(,(car form) ,var
         ,(if msg
              (macroexp--warn-wrap msg new-protform 'lexical)
            new-protform)
         ,@(mapcar
            (lambda (handler)
              `(,(car handler)
                ,@(let ((body (mapcar (lambda (form)
                                        (cconv-convert form new-env extend))
                                      (cdr handler))))
                    (if (eq reason :cconv-mutcap)
                        `((let ((,var (list ,var))) ,@body))
                      body))))
            handlers))))
    (`(unwind-protect ,form1 . ,body)
     `(,(car form) ,(cconv-convert form1 env extend)
       :fun-body ,(cconv--convert-function () body env form1)))
    (`(setq ,var ,expr)
     (let ((var-new (or (cdr (assq var env)) var))
           (val (cconv-convert expr env extend)))
       (pcase var-new
         ((pred symbolp) `(,(car form) ,var-new ,val))
         (`(car-safe ,iexp) `(setcar ,iexp ,val))
         ;; Should never get here, but for variables which are
         ;; mutated+captured+unused, we may end up trying to `setq'
         ;; on a closed-over variable, so just drop the setq (wtf,
         ;; you said you couldn't get here).
         (_ val))))
    (`(,(and (or 'funcall 'apply) callsym) ,fun . ,args)
     ;; non-special forms treated separately for lambda lifting
     (let ((mapping (cdr (assq fun env))))
       (pcase mapping
         (`(apply-partially ,_ . ,(and fvs `(,_ . ,_)))
          (cl-assert (eq (cadr mapping) fun))
          `(,callsym ,fun
                     ,@(mapcar (lambda (fv)
                                 (let ((exp (or (cdr (assq fv env)) fv)))
                                   (pcase exp
                                     (`(car-safe ,iexp . ,_) iexp)
                                     (_ exp))))
                               fvs)
                     ,@(mapcar (lambda (arg)
                                 (cconv-convert arg env extend))
                               args)))
         (_ `(,callsym ,@(mapcar (lambda (arg)
                                   (cconv-convert arg env extend))
                                 (cons fun args)))))))
    ;; FORM converted beforehand in lambda case.
    (`(interactive . ,_) form)
    (`(oclosure--fix-type (ignore . ,vars) ,exp)
     (dolist (var vars)
       (let ((x (assq var env)))
         (pcase (cdr x)
           (`(car-safe . ,_) (error "Slot %S should not be mutated" var))
           (_ (cl-assert (null (cdr x)))))))
     (cconv-convert exp env extend))
    (`(,func . ,forms)
     (if (symbolp func)
         ;; First element is function or whatever function-like forms are:
         ;; or, and, if, catch, progn, prog1, while, until
         `(,func . ,(mapcar (lambda (form)
                              (cconv-convert form env extend))
                            forms))
       (prog1 nil
         (byte-compile-warn "Malformed function `%S'" func))))
    (_ (or (cdr (assq form env)) form))))

(defun cconv--dynvar-p (var)
  (or (not lexical-binding)
      (not (symbolp var))
      (special-variable-p var)
      (memq var cconv--dynvars-at-large)))

(defun cconv--analyze-use (vardata form varkind)
  "Analyze the use of a variable.
VARDATA should be (VAR-VAL READ MUTATED CAPTURED CALLED).
VARKIND is the name of the kind of variable.
FORM is the parent form that binds this var."
  ;; use = `(,var-val ,read ,mutated ,captured ,called)
  (pcase vardata
    (`(,_ nil nil nil nil) nil)
    (`((,(and var (guard (eq ?_ (aref (symbol-name var) 0)))) . ,_)
       ,_ ,_ ,_ ,_)
     ;; FIXME: Convert this warning to use `macroexp--warn-wrap'
     ;; so as to give better position information.
     (when (byte-compile-warning-enabled-p 'not-unused var)
       (byte-compile-warn "%s `%S' not left unused" varkind var)))
    ((and (let (or 'let* 'let) (car form))
          `((,var) ;; (or `(,var nil) : Too many false positives: bug#47080
            t nil ,_ ,_))
     ;; FIXME: Convert this warning to use `macroexp--warn-wrap'
     ;; so as to give better position information and obey
     ;; `byte-compile-warnings'.
     (unless (not (intern-soft var))
       (byte-compile-warn "Variable `%S' left uninitialized" var))))
  (pcase vardata
    (`(,var-val nil ,_ ,_ nil)
     (push (cons (cons var-val form) :cconv-unused) cconv--results))
    ;; If it's unused, there's no point converting it into a cons-cell, even if
    ;; it's captured and mutated.
    (`(,var-val ,_ t t ,_)
     (push (cons (cons var-val form) :cconv-mutcap)
           cconv--results))
    (`(,(and var-val `(,_ (function (lambda . ,_)))) nil nil nil t)
     (push (cons (cons var-val form) :cconv-lift)
           cconv--results))))

(defun cconv--analyze-function (args body env parent-form)
  (let* ((cconv--dynvars-at-large cconv--dynvars-at-large)
         (fv (cons body nil)) ;(FUNBODY . (FREE-VARIABLES))
         ;; Analyze body from clean slate env to distinguish uses
         ;; inside function from uses outside of it (wtf?)
         (envcopy (mapcar (lambda (vdata) (list (car vdata) nil nil nil nil)) env))
         (new-env envcopy)
         new-vars)
    (push fv cconv--fv-alist)
    (when lexical-binding
      (dolist (arg args)
        (cond
         ((eq ?& (aref (symbol-name arg) 0)) nil) ;Ignore &rest, &optional, ...
         (t (let ((var-struct (list arg nil nil nil nil)))
              (cl-pushnew arg byte-compile-lexical-variables)
              (push (cons (list arg) (cdr var-struct)) new-vars)
              (push var-struct new-env))))))

    (dolist (form body)                   ;Analyze body forms.
      (cconv-analyze-form form new-env))
    ;; Summarize resulting data about arguments.
    (dolist (var-data new-vars)
      (cconv--analyze-use var-data parent-form "argument"))
    ;; Transfer uses collected in envcopy (via new-env) back to env
    ;; and compute free variables.
    (while env
      (cl-assert (and envcopy (eq (caar env) (caar envcopy))))
      (let ((x (cdr (car env)))
            (y (cdr (car envcopy)))
            free)
        (while x
          (when (car y) (setcar x t) (setq free t))
          (setq x (cdr x) y (cdr y)))
        (when free
          (push (caar env) (cdr fv))
          (setf (nth 3 (car env)) t)))
      (setq env (cdr env) envcopy (cdr envcopy)))))

(defun cconv-analyze-form (form env)
  "Collate analysis results in CCONV--RESULTS.
ENV contains entries (VAR . (READ MUTATED CAPTURED CALLED))."
  (pcase form
    (`(,(and (or 'let* 'let) letsym) ,var-vals . ,body-forms) ; let special form
     (let ((cconv--dynvars-at-large cconv--dynvars-at-large)
           (orig-env env)
           new-vars var val)
       (dolist (var-val var-vals)
         (if (consp var-val)
             (progn
               (setq var (car var-val)
                     val (cadr var-val))
               (cconv-analyze-form val (if (eq letsym 'let*) env orig-env)))
           (setq var var-val
                 val nil
                 var-val (list var-val)))
         (if (cconv--dynvar-p var)
             (cl-pushnew var cconv--dynvars-seen)
           (cl-pushnew var byte-compile-lexical-variables)
           (let ((var-struct (list var nil nil nil nil)))
             (push (cons var-val (cdr var-struct)) new-vars)
             (push var-struct env))))
       (dolist (form body-forms)       ; Analyze body forms.
         (cconv-analyze-form form env))
       (dolist (var-data new-vars)
         (cconv--analyze-use var-data form "variable"))))
    (`(function (lambda ,vrs . ,body-forms))
     (when (eq :documentation (car-safe (car body-forms)))
       (cconv-analyze-form (cadr (pop body-forms)) env))
     (let ((body-form (if (stringp (car body-forms)) (cdr body-forms) body-forms)))
       (when (eq 'interactive (car-safe (car body-form)))
         (let ((iact-form (cadr (car body-form))))
           (unless (macroexp-const-p iact-form) ;Optimize this common case.
             (let ((iact-form* (if (eq 'function (car-safe iact-form))
                                   iact-form
                                 `#'(lambda (&rest _cconv--dummy) ,iact-form))))
               (setf (gethash form cconv--interactive-form-funs) iact-form*)
               (cconv-analyze-form iact-form* env))))))
     (cconv--analyze-function vrs body-forms env form))
    (`(setq ,var ,expr)
     ;; Mark mutated
     (when-let ((v (assq var env)))
       (setf (nth 2 v) t))
     (cconv-analyze-form expr env))
    (`((lambda . ,_) . ,_)      ; First element is lambda expression.
     (byte-compile-warn
      "Use of deprecated ((lambda %s ...) ...) form" (nth 1 (car form)))
     (dolist (exp `((function ,(car form)) . ,(cdr form)))
       (cconv-analyze-form exp env)))
    (`(cond . ,cond-forms)             ; cond special form
     (dolist (forms cond-forms)
       (dolist (form forms) (cconv-analyze-form form env))))
    (`(quote . ,_) nil)                ; quote form
    (`(function . ,_) nil)             ; same as quote
    (`(condition-case ,var ,protected-form . ,handlers)
     (cconv-analyze-form protected-form env)
     (unless lexical-binding
       (setq var nil))
     (when (and var (symbolp var) (cconv--dynvar-p var))
       (byte-compile-warn
        "Lexical variable shadows the dynamic variable %S" var))
     (let* ((var-struct (list var nil nil nil nil)))
       (when var (push var-struct env))
       (dolist (handler handlers)
         (dolist (form (cdr handler))
           (cconv-analyze-form form env)))
       (when var (cconv--analyze-use (cons (list var) (cdr var-struct))
                                     form "variable"))))
    ;; FIXME: The bytecode for unwind-protect forces us to wrap the unwind.
    (`(unwind-protect ,form . ,body)
     (cconv-analyze-form form env)
     (cconv--analyze-function nil body env form))
    (`(defvar ,var) (push var cconv--dynvars-at-large))
    (`(,(or 'defconst 'defvar) ,var ,val . ,_)
     (push var cconv--dynvars-at-large)
     (cconv-analyze-form val env))
    (`(,(or 'funcall 'apply) ,fun . ,args)
     ;; Here we ignore FUN because funcall and apply are the only two
     ;; functions whose argument could be a candidate for lambda
     ;; lifting.  So, if we see FUN elsewhere, we'll delete it from
     ;; lambda candidates (wtf?)
     (let ((fdata (and (symbolp fun) (assq fun env))))
       (if fdata
           (setf (nth 4 fdata) t)
         (cconv-analyze-form fun env)))
     (dolist (form args)
       (cconv-analyze-form form env)))
    (`(interactive . ,_) nil)
    (`(,_ . ,body-forms)   ; First element is a function or whatever.
     (unless (listp body-forms)
       (signal 'wrong-type-argument (list 'proper-list-p form)))
     (dolist (form body-forms)
       (cconv-analyze-form form env)))
    ((pred symbolp)
     (when-let ((declared-and-visible (assq form env)))
       (setf (nth 1 declared-and-visible) t)))))

(defun cconv-fv (form lexvars dynvars)
  "Extract lexical and dynamic variables actually used by FORM.
Return a cons of the form (LEXV . DYNV) where LEXV and DYNV
are subsets of LEXVARS and DYNVARS, respectively."
  (let ((cconv--dynvars-at-large dynvars)
        byte-compile-lexical-variables
        cconv--dynvars-seen
        cconv--results
        cconv--fv-alist)
    (cconv-analyze-form
     `#'(lambda () ,form) ; requires "simple lambda" wrapper
     (mapcar (lambda (v) (list v nil nil nil nil)) lexvars))
    (setf cconv--fv-alist (nreverse cconv--fv-alist))
    (cl-destructuring-bind (_body . fvs)
        (cl-first cconv--fv-alist)
      (cons (nreverse fvs)
            (seq-keep (lambda (var) (car (memq var dynvars)))
                      cconv--dynvars-seen)))))

(defun cconv-make-interpreted-closure (fun env)
  "Make a closure for the interpreter at runtime.
FUN must be a lambda expression.  ENV is the runtime
representation of the lexical environment, i.e. elements are
dynamically scoped plain symbols or lexically scoped (SYMBOL
. VAL) pairs."
  (cl-assert (eq (car-safe fun) 'lambda))
  (let ((lexvars (delq nil (mapcar #'car-safe env))))
    (if (or (null lexvars)
            ;; exception keeping context untrimmed (Bug#59213)
            (and (eq :closure-dont-trim-context (nth 2 fun))
                 (nthcdr 3 fun)))
        ;; Return trivial closure if the lexical environment is empty,
        ;; or needs to be preserved.  Attempting to replace ,(cdr fun)
        ;; by a macroexpanded version causes bootstrap to fail.
        `(closure ,env . ,(cdr fun))
      (let* ((form `#',fun)
             (expanded-form
              (let ((lexical-binding t)
                    (macroexp--dynvars (append env macroexp--dynvars)))
                (macroexpand-all form macroexpand-all-environment)))
             (expanded-fun-cdr
              (pcase expanded-form
                (`#'(lambda . ,cdr) cdr)
                (_ (cdr fun))))
             (dynvars (delq nil (mapcar (lambda (b) (when (symbolp b) b)) env)))
             (new-env
              (cl-destructuring-bind (lexv . dynv)
                  (cconv-fv expanded-form lexvars dynvars)
                (nconc (mapcar (lambda (fv) (assq fv env)) lexv) dynv))))
        `(closure ,(or new-env '(t)) . ,expanded-fun-cdr)))))

(provide 'cconv)
;;; cconv.el ends here
