;;; advice.el --- An overloading mechanism for Emacs Lisp functions  -*- lexical-binding: t -*-

;; Copyright (C) 1993-2024 Free Software Foundation, Inc.

;; Author: Hans Chalupsky <hans@cs.buffalo.edu>
;; Maintainer: emacs-devel@gnu.org
;; Created: 12 Dec 1992
;; Keywords: extensions, lisp, tools
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

;; Advice is documented in the Emacs Lisp Manual.

;; @ Introduction:
;; ===============
;; This package implements a full-fledged Lisp-style advice mechanism
;; for Emacs Lisp.  Advice is a clean and efficient way to modify the
;; behavior of Emacs Lisp functions without having to keep  personal
;; modified copies of such functions around.  A great number of such
;; modifications can be achieved by treating the original function as a
;; black box and specifying a different execution environment for it
;; with a piece of advice.  Think of a piece of advice as a kind of fancy
;; hook that you can attach to any function/macro/subr.

;; @ Highlights:
;; =============
;; - Clean definition of multiple, named before/around/after advices
;;   for functions and macros.
;; - Full control over the arguments an advised function will receive,
;;   the binding environment in which it will be executed, as well as the
;;   value it will return.
;; - Allows re/definition of interactive behavior for commands.
;; - Every piece of advice can have its documentation string.
;; - The execution of every piece of advice can be protected against error
;;   and non-local exits in preceding code or advices.
;; - Simple argument access either by name, or, more portable but as
;;   efficient, via access macros
;; - Allows the specification of a different argument list for the advised
;;   version of a function.
;; - Advised functions can be byte-compiled either at file-compile time
;;   (see preactivation) or activation time.
;; - Separation of advice definition and activation.
;; - Forward advice is possible, that is
;;   as yet undefined or autoload functions can be advised without having to
;;   preload the file in which they are defined.
;; - Forward redefinition is possible because around advice can be used to
;;   completely redefine a function.
;; - A caching mechanism for advised definition provides for cheap deactivation
;;   and reactivation of advised functions.
;; - Preactivation allows efficient construction and compilation of advised
;;   definitions at file compile time without giving up the flexibility of
;;   the advice mechanism.
;; - En/disablement mechanism allows the use of  different "views" of advised
;;   functions depending on what pieces of advice are currently en/disabled
;; - Provides manipulation mechanisms for sets of advised functions via
;;   regular expressions that match advice names.

;; @ Overview, or how to read this file:
;; =====================================
;; You can use `outline-mode' to help you read this documentation (set
;; `outline-regexp' to `";; @+"').
;;
;; The four major sections of this file are:
;;
;;   @ This initial information       ...installation, customization etc.
;;   @ Advice documentation:          ...general documentation
;;   @ Foo games: An advice tutorial  ...teaches about Advice by example
;;   @ Advice implementation:         ...actual code, yeah!!
;;
;; The latter three are actual headings which you can search for
;; directly in case `outline-mode' doesn't work for you.

;; @ Restrictions:
;; ===============
;; - Advised functions/macros/subrs will only exhibit their advised behavior
;;   when they are invoked via their function cell.  This means that advice will
;;   not work for the following:
;;   + advised subrs that are called directly from other subrs or C-code
;;   + advised subrs that got replaced with their byte-code during
;;     byte-compilation (e.g., car)
;;   + advised macros which were expanded during byte-compilation before
;;     their advice was activated.

;; @ Credits:
;; ==========
;; This package is an extension and generalization of packages such as
;; insert-hooks.el written by Noah S. Friedman, and advise.el written by
;; Raul J. Acevedo.  Some ideas used in here come from these packages,
;; others come from the various Lisp advice mechanisms I've come across
;; so far, and a few are simply mine.

;; @ Safety Rules and Emergency Exits:
;; ===================================
;; Before we begin: CAUTION!!
;; Advice provides you with a lot of rope to hang yourself on very
;; easily accessible trees, so, here are a few important things you
;; should know:
;;
;; If you experience any strange behavior/errors etc. that you attribute to
;; Advice or to some ill-advised function do one of the following:

;; - M-x ad-deactivate FUNCTION (if you have a definite suspicion what
;;                               function gives you problems)
;; - M-x ad-deactivate-all      (if you don't have a clue what's going wrong)
;; - M-x ad-recover-normality   (for real emergencies)
;; - If none of the above solves your Advice-related problem go to another
;;   terminal, kill your Emacs process and send me some hate mail.

;; The first two measures have restarts, i.e., once you've figured out
;; the problem you can reactivate advised functions with either `ad-activate',
;; or `ad-activate-all'.  `ad-recover-normality' unadvises
;; everything so you won't be able to reactivate any advised functions, you'll
;; have to stick with their standard incarnations for the rest of the session.

;; RELAX: Advice is pretty safe even if you are oblivious to the above.
;; I use it extensively and haven't run into any serious trouble in a long
;; time.  Just wanted you to be warned.

;; @ Customization:
;; ================

;; Look at the documentation of `ad-redefinition-action' for possible values
;; of this variable.  Its default value is `warn' which will print a warning
;; message when an already defined advised function gets redefined with a
;; new original definition and de/activated.

;; Look at the documentation of `ad-default-compilation-action' for possible
;; values of this variable.  Its default value is `maybe' which will compile
;; advised definitions during activation in case the byte-compiler is already
;; loaded.  Otherwise, it will leave them uncompiled.

;; @ Motivation:
;; =============
;; Before I go on explaining how advice works, here are four simple examples
;; how this package can be used.  The first three are very useful, the last one
;; is just a joke:

;;(defadvice switch-to-buffer (before existing-buffers-only activate)
;;  "When called interactively switch to existing buffers only, unless
;;when called with a prefix argument."
;;  (interactive
;;   (list (read-buffer "Switch to buffer: " (other-buffer)
;;                      (null current-prefix-arg)))))
;;
;;(defadvice switch-to-buffer (around confirm-non-existing-buffers activate)
;;  "Switch to non-existing buffers only upon confirmation."
;;  (interactive "BSwitch to buffer: ")
;;  (if (or (get-buffer (ad-get-arg 0))
;;          (y-or-n-p (format-message "`%s' does not exist, create? "
;;                                    (ad-get-arg 0))))
;;      ad-do-it))
;;
;;(defadvice find-file (before existing-files-only activate)
;;  "Find existing files only"
;;  (interactive "fFind file: "))
;;
;;(defadvice car (around interactive activate)
;;  "Make `car' an interactive function."
;;   (interactive "xCar of list: ")
;;   ad-do-it
;;   (if (called-interactively-p 'interactive)
;;       (message "%s" ad-return-value)))


;; @ Advice documentation:
;; =======================
;; Below is general documentation of the various features of advice.  For more
;; concrete examples check the corresponding sections in the tutorial part.

;; @@ Terminology:
;; ===============
;; - Emacs: Emacs as released by the GNU Project
;; - Advice: The name of this package.
;; - advices: Short for "pieces of advice".

;; @@ Defining a piece of advice with `defadvice':
;; ===============================================
;; The main means of defining a piece of advice is the macro `defadvice',
;; there is no interactive way of specifying a piece of advice.  A call to
;; `defadvice' has the following syntax which is similar to the syntax of
;; `defun/defmacro':
;;
;; (defadvice <function> (<class> <name> [<position>] [<arglist>] {<flags>}*)
;;   [ [<documentation-string>] [<interactive-form>] ]
;;   {<body-form>}* )

;; <function> is the name of the function/macro/subr to be advised.

;; <class> is the class of the advice which has to be one of `before',
;; `around', `after', `activation' or `deactivation' (the last two allow
;; definition of special act/deactivation hooks).

;; <name> is the name of the advice which has to be a non-nil symbol.
;; Names uniquely identify a piece of advice in a certain advice class,
;; hence, advices can be redefined by defining an advice with the same class
;; and name.  Advice names are global symbols, hence, the same name space
;; conventions used for function names should be applied.

;; An optional <position> specifies where in the current list of advices of
;; the specified <class> this new advice will be placed.  <position> has to
;; be either `first', `last' or a number that specifies a zero-based
;; position (`first' is equivalent to 0).  If no position is specified
;; `first' will be used as a default.  If this call to `defadvice' redefines
;; an already existing advice (see above) then the position argument will
;; be ignored and the position of the already existing advice will be used.

;; An optional <arglist> which has to be a list can be used to define the
;; argument list of the advised function.  This argument list should of
;; course be compatible with the argument list of the original function,
;; otherwise functions that call the advised function with the original
;; argument list in mind will break.  If more than one advice specify an
;; argument list then the first one (the one with the smallest position)
;; found in the list of before/around/after advices will be used.

;; <flags> is a list of symbols that specify further information about the
;; advice.  All flags can be specified with unambiguous initial substrings.
;;   `activate': Specifies that the advice information of the advised
;;              function should be activated right after this advice has been
;;              defined.  In forward advices `activate' will be ignored.
;;   `protect': Specifies that this advice should be protected against
;;              non-local exits and errors in preceding code/advices.
;;   `compile': Specifies that the advised function should be byte-compiled.
;;              This flag will be ignored unless `activate' is also specified.
;;   `disable': Specifies that the defined advice should be disabled, hence,
;;              it will not be used in an activation until somebody enables it.
;;   `preactivate': Specifies that the advised function should get preactivated
;;              at macro-expansion/compile time of this `defadvice'.  This
;;              generates a compiled advised definition according to the
;;              current advice state which will be used during activation
;;              if appropriate.  Only use this if the `defadvice' gets
;;              actually compiled.

;; An optional <documentation-string> can be supplied to document the advice.
;; On call of the `documentation' function it will be combined with the
;; documentation strings of the original function and other advices.

;; An optional <interactive-form> form can be supplied to change/add
;; interactive behavior of the original function.  If more than one advice
;; has an `(interactive ...)' specification then the first one (the one
;; with the smallest position) found in the list of before/around/after
;; advices will be used.

;; A possibly empty list of <body-forms> specifies the body of the advice in
;; an implicit progn.  The body of an advice can access/change arguments,
;; the return value, the binding environment, and can have all sorts of
;; other side effects.

;; @@ Assembling advised definitions:
;; ==================================
;; Suppose a function/macro/subr/special-form has N pieces of before advice,
;; M pieces of around advice and K pieces of after advice.  Assuming none of
;; the advices is protected, its advised definition will look like this
;; (body-form indices correspond to the position of the respective advice in
;; that advice class):

;;    ([macro] lambda <arglist>
;;       [ [<advised-docstring>] [(interactive ...)] ]
;;       (let (ad-return-value)
;;         {<before-0-body-form>}*
;;               ....
;;         {<before-N-1-body-form>}*
;;         {<around-0-body-form>}*
;;            {<around-1-body-form>}*
;;                  ....
;;               {<around-M-1-body-form>}*
;;                  (setq ad-return-value
;;                        <apply original definition to <arglist>>)
;;               {<other-around-M-1-body-form>}*
;;                  ....
;;            {<other-around-1-body-form>}*
;;         {<other-around-0-body-form>}*
;;         {<after-0-body-form>}*
;;               ....
;;         {<after-K-1-body-form>}*
;;         ad-return-value))

;; Macros are redefined as macros, hence the optional [macro] in the
;; beginning of the definition.

;; <arglist> is either the argument list of the original function or the
;; first argument list defined in the list of before/around/after advices.
;; The values of <arglist> variables can be accessed/changed in the body of
;; an advice by simply referring to them by their original name, however,
;; more portable argument access macros are also provided (see below).

;; <advised-docstring> is an optional, special documentation string which will
;; be expanded into a proper documentation string upon call of `documentation'.

;; (interactive ...) is an optional interactive form either taken from the
;; original function or from a before/around/after advice.  For advised
;; interactive subrs that do not have an interactive form specified in any
;; advice we have to use (interactive) and then call the subr interactively
;; if the advised function was called interactively, because the
;; interactive specification of subrs is not accessible.  This is the only
;; case where changing the values of arguments will not have an affect
;; because they will be reset by the interactive specification of the subr.
;; If this is a problem one can always specify an interactive form in a
;; before/around/after advice to gain control over argument values that
;; were supplied interactively.
;;
;; Then the body forms of the various advices in the various classes of advice
;; are assembled in order.  The forms of around advice L are normally part of
;; one of the forms of around advice L-1.  An around advice can specify where
;; the forms of the wrapped or surrounded forms should go with the special
;; keyword `ad-do-it', which will run the forms of the surrounded code.

;; The innermost part of the around advice onion is
;;      <apply original definition to <arglist>>
;; whose form depends on the type of the original function.  The variable
;; `ad-return-value' will be set to its result.  This variable is visible to
;; all pieces of advice which can access and modify it before it gets returned.
;;
;; The semantic structure of advised functions that contain protected pieces
;; of advice is the same.  The only difference is that `unwind-protect' forms
;; make sure that the protected advice gets executed even if some previous
;; piece of advice had an error or a non-local exit.  If any around advice is
;; protected then the whole around advice onion will be protected.

;; @@ Argument access in advised functions:
;; ========================================
;; As already mentioned, the simplest way to access the arguments of an
;; advised function in the body of an advice is to refer to them by name.
;; To do that, the advice programmer needs to know either the names of the
;; argument variables of the original function, or the names used in the
;; argument list redefinition given in a piece of advice.  While this simple
;; method might be sufficient in many cases, it has the disadvantage that it
;; is not very portable because it hardcodes the argument names into the
;; advice.  If the definition of the original function changes the advice
;; might break even though the code might still be correct.  Situations like
;; that arise, for example, if one advises a subr like `eval-region' which
;; gets redefined in a non-advice style into a function by the edebug
;; package.  If the advice assumes `eval-region' to be a subr it might break
;; once edebug is loaded.  Similar situations arise when one wants to use the
;; same piece of advice across different versions of Emacs.

;; As a solution to that advice provides argument list access macros that get
;; translated into the proper access forms at activation time, i.e., when the
;; advised definition gets constructed.  Access macros access actual arguments
;; by position regardless of how these actual argument get distributed onto
;; the argument variables of a function.  The rational behind this is that in
;; Emacs Lisp the semantics of an argument is strictly determined by its
;; position (there are no keyword arguments).

;; Suppose the function `foo' is defined as
;;
;;    (defun foo (x y &optional z &rest r) ....)
;;
;; and is then called with
;;
;;    (foo 0 1 2 3 4 5 6)

;; which means that X=0, Y=1, Z=2 and R=(3 4 5 6).  The assumption is that
;; the semantics of an actual argument is determined by its position.  It is
;; this semantics that has to be known by the advice programmer.  Then s/he
;; can access these arguments in a piece of advice with some of the
;; following macros (the arrows indicate what value they will return):

;;    (ad-get-arg 0) -> 0
;;    (ad-get-arg 1) -> 1
;;    (ad-get-arg 2) -> 2
;;    (ad-get-arg 3) -> 3
;;    (ad-get-args 2) -> (2 3 4 5 6)
;;    (ad-get-args 4) -> (4 5 6)

;; `(ad-get-arg <position>)' will return the actual argument that was supplied
;; at <position>, `(ad-get-args <position>)' will return the list of actual
;; arguments supplied starting at <position>.  Note that these macros can be
;; used without any knowledge about the form of the actual argument list of
;; the original function.

;; Similarly, `(ad-set-arg <position> <value-form>)' can be used to set the
;; value of the actual argument at <position> to <value-form>.  For example,
;;
;;   (ad-set-arg 5 "five")
;;
;; will have the effect that R=(3 4 "five" 6) once the original function is
;; called.  `(ad-set-args <position> <value-list-form>)' can be used to set
;; the list of actual arguments starting at <position> to <value-list-form>.
;; For example,
;;
;;   (ad-set-args 0 '(5 4 3 2 1 0))
;;
;; will have the effect that X=5, Y=4, Z=3 and R=(2 1 0) once the original
;; function is called.

;; All these access macros are text macros rather than real Lisp macros.  When
;; the advised definition gets constructed they get replaced with actual access
;; forms depending on the argument list of the advised function, i.e., after
;; that argument access is in most cases as efficient as using the argument
;; variable names directly.

;; @@@ Accessing argument bindings of arbitrary functions:
;; =======================================================
;; Some functions (such as `trace-function' defined in trace.el) need a
;; method of accessing the names and bindings of the arguments of an
;; arbitrary advised function.  To do that within an advice one can use the
;; special keyword `ad-arg-bindings' which is a text macro that will be
;; substituted with a form that will evaluate to a list of binding
;; specifications, one for every argument variable.  These binding
;; specifications can then be examined in the body of the advice.  For
;; example, somewhere in an advice we could do this:
;;
;;   (let* ((bindings ad-arg-bindings)
;;          (firstarg (car bindings))
;;          (secondarg (car (cdr bindings))))
;;     ;; Print info about first argument
;;     (print (format "%s=%s (%s)"
;;                    (ad-arg-binding-field firstarg 'name)
;;                    (ad-arg-binding-field firstarg 'value)
;;                    (ad-arg-binding-field firstarg 'type)))
;;     ....)
;;
;; The `type' of an argument is either `required', `optional' or `rest'.
;; Wherever `ad-arg-bindings' appears a form will be inserted that evaluates
;; to the list of bindings, hence, in order to avoid multiple unnecessary
;; evaluations one should always bind it to some variable.

;; @@@ Argument list mapping:
;; ==========================
;; Because `defadvice' allows the specification of the argument list
;; of the advised function we need a mapping mechanism that maps this
;; argument list onto that of the original function.  Hence SYM and
;; NEWDEF have to be properly mapped onto the &rest variable when the
;; original definition is called.  Advice automatically takes care of
;; that mapping, hence, the advice programmer can specify an argument
;; list without having to know about the exact structure of the
;; original argument list as long as the new argument list takes a
;; compatible number/magnitude of actual arguments.

;; @@ Activation and deactivation:
;; ===============================
;; The definition of an advised function does not change until all its advice
;; gets actually activated.  Activation can either happen with the `activate'
;; flag specified in the `defadvice', with an explicit call or interactive
;; invocation of `ad-activate', or at the time an already advised function
;; gets defined.

;; When a function gets first activated its original definition gets saved,
;; all defined and enabled pieces of advice will get combined with the
;; original definition, the resulting definition might get compiled depending
;; on some conditions described below, and then the function will get
;; redefined with the advised definition.  This also means that undefined
;; functions cannot get activated even though they might be already advised.

;; The advised definition will get compiled either if `ad-activate' was called
;; interactively with a prefix argument, or called explicitly with its second
;; argument as t, or, if `ad-default-compilation-action' justifies it according
;; to the current system state.  If the advised definition was
;; constructed during "preactivation" (see below) then that definition will
;; be already compiled because it was constructed during byte-compilation of
;; the file that contained the `defadvice' with the `preactivate' flag.

;; `ad-deactivate' can be used to back-define an advised function to its
;; original definition.  It can be called interactively or directly.  Because
;; `ad-activate' caches the advised definition the function can be
;; reactivated via `ad-activate' with only minor overhead (it is checked
;; whether the current advice state is consistent with the cached
;; definition, see the section on caching below).

;; `ad-activate-regexp' and `ad-deactivate-regexp' can be used to de/activate
;; all currently advised function that have a piece of advice with a name that
;; contains a match for a regular expression.  These functions can be used to
;; de/activate sets of functions depending on certain advice naming
;; conventions.

;; Finally, `ad-activate-all' and `ad-deactivate-all' can be used to
;; de/activate all currently advised functions.  These are useful to
;; (temporarily) return to an un/advised state.

;; @@@ Reasons for the separation of advice definition and activation:
;; ===================================================================
;; As already mentioned, advising happens in two stages:

;;   1) definition of various pieces of advice
;;   2) activation of all advice currently defined and enabled

;; The advantage of this is that various pieces of advice can be defined
;; before they get combined into an advised definition which avoids
;; unnecessary constructions of intermediate advised definitions.  The more
;; important advantage is that it allows the implementation of forward advice.
;; Advice information for a certain function accumulates as the value of the
;; `advice-info' property of the function symbol.  This accumulation is
;; completely independent of the fact that the function might not yet be
;; defined.  The macros `defun' and `defmacro' check whether the
;; function/macro they defined had advice information
;; associated with it.  If so and forward advice is enabled, the original
;; definition will be saved, and then the advice will be activated.

;; @@ Enabling/disabling pieces or sets of advice:
;; ===============================================
;; A major motivation for the development of this advice package was to bring
;; a little bit more structure into the function overloading chaos in Emacs
;; Lisp.  Many packages achieve some of their functionality by adding a little
;; bit (or a lot) to the standard functionality of some Emacs Lisp function.
;; ange-ftp is a very popular package that used to achieve its magic by
;; overloading most Emacs Lisp functions that deal with files.  A popular
;; function that's overloaded by many packages is `expand-file-name'.
;; The situation that one function is multiply overloaded can arise easily.

;; Once in a while it would be desirable to be able to disable some/all
;; overloads of a particular package while keeping all the rest.  Ideally -
;; at least in my opinion - these overloads would all be done with advice,
;; I know I am dreaming right now... In that ideal case the enable/disable
;; mechanism of advice could be used to achieve just that.

;; Every piece of advice is associated with an enablement flag.  When the
;; advised definition of a particular function gets constructed (e.g., during
;; activation) only the currently enabled pieces of advice will be considered.
;; This mechanism allows one to have different "views" of an advised function
;; dependent on what pieces of advice are currently enabled.

;; Another motivation for this mechanism is that it allows one to define a
;; piece of advice for some function yet keep it dormant until a certain
;; condition is met.  Until then activation of the function will not make use
;; of that piece of advice.  Once the condition is met the advice can be
;; enabled and a reactivation of the function will add its functionality as
;; part of the new advised definition.  Hence, if somebody
;; else advised these functions too and activates them the advices defined
;; by advice will get used only if they are intended to be used.

;; The main interface to this mechanism are the interactive functions
;; `ad-enable-advice' and `ad-disable-advice'.  For example, the following
;; would disable a particular advice of the function `foo':
;;
;;    (ad-disable-advice 'foo 'before 'my-advice)
;;
;; This call by itself only changes the flag, to get the proper effect in
;; the advised definition too one has to activate `foo' with
;;
;;    (ad-activate 'foo)
;;
;; or interactively.  To disable whole sets of advices one can use a regular
;; expression mechanism.  For example, let us assume that ange-ftp actually
;; used advice to overload all its functions, and that it used the
;; "ange-ftp-" prefix for all its advice names, then we could temporarily
;; disable all its advices with
;;
;;    (ad-disable-regexp "\\`ange-ftp-")
;;
;; and the following call would put that actually into effect:
;;
;;    (ad-activate-regexp "\\`ange-ftp-")
;;
;; A safer way would have been to use
;;
;;    (ad-update-regexp "\\`ange-ftp-")
;;
;; instead which would have only reactivated currently actively advised
;; functions, but not functions that were currently inactive.  All these
;; functions can also be called interactively.

;; A certain piece of advice is considered a match if its name contains a
;; match for the regular expression.  To enable ange-ftp again we would use
;; `ad-enable-regexp' and then activate or update again.

;; @@ Forward advice, automatic advice activation:
;; ===============================================
;; Because most Emacs Lisp packages are loaded on demand via an autoload
;; mechanism it is essential to be able to "forward advise" functions.
;; Otherwise, proper advice definition and activation would make it necessary
;; to preload every file that defines a certain function before it can be
;; advised, which would partly defeat the purpose of the advice mechanism.

;; In the following, "forward advice" always implies its automatic activation
;; once a function gets defined, and not just the accumulation of advice
;; information for a possibly undefined function.

;; Advice implements forward advice mainly via the following: 1) Separation
;; of advice definition and activation that makes it possible to accumulate
;; advice information without having the original function already defined,
;; 2) Use of the `defalias-fset-function' symbol property which lets
;; us advise the function when it gets defined.

;; Automatic advice activation means, that whenever a function gets defined
;; with either `defun', `defmacro', `defalias' or by loading a byte-compiled
;; file, and the function has some advice-info stored with it then that
;; advice will get activated right away.

;; @@ Caching of advised definitions:
;; ==================================
;; After an advised definition got constructed it gets cached as part of the
;; advised function's advice-info so it can be reused, for example, after an
;; intermediate deactivation.  Because the advice-info of a function might
;; change between the time of caching and reuse a cached definition gets
;; a cache-id associated with it so it can be verified whether the cached
;; definition is still valid (the main application of this is preactivation
;; - see below).

;; When an advised function gets activated and a verifiable cached definition
;; is available, then that definition will be used instead of creating a new
;; advised definition from scratch.  If you want to make sure that a new
;; definition gets constructed then you should use `ad-clear-cache' before you
;; activate the advised function.

;; @@ Preactivation:
;; =================
;; Constructing an advised definition is moderately expensive.  In a situation
;; where one package defines a lot of advised functions it might be
;; prohibitively expensive to do all the advised definition construction at
;; runtime.  Preactivation is a mechanism that allows compile-time construction
;; of compiled advised definitions that can be activated cheaply during
;; runtime.  Preactivation uses the caching mechanism to do that.  Here's how
;; it works:

;; When the byte-compiler compiles a `defadvice' that has the `preactivate'
;; flag specified, it uses the current original definition of the advised
;; function plus the advice specified in this `defadvice' (even if it is
;; specified as disabled) and all other currently enabled pieces of advice to
;; construct an advised definition and an identifying cache-id and makes them
;; part of the `defadvice' expansion which will then be compiled by the
;; byte-compiler.
;; When the file with the compiled, preactivating `defadvice' gets loaded the
;; precompiled advised definition will be cached on the advised function's
;; advice-info.  When it gets activated (can be immediately on execution of the
;; `defadvice' or any time later) the cache-id gets checked against the
;; current state of advice and if it is verified the precompiled definition
;; will be used directly (the verification is pretty cheap).  If it couldn't
;; get verified a new advised definition for that function will be built from
;; scratch, hence, the efficiency added by the preactivation mechanism does not
;; at all impair the flexibility of the advice mechanism.

;; MORAL: In order get all the efficiency out of preactivation the advice
;;        state of an advised function at the time the file with the
;;        preactivating `defadvice' gets byte-compiled should be exactly
;;        the same as it will be when the advice of that function gets
;;        actually activated.  If it is not there is a high chance that the
;;        cache-id will not match and hence a new advised definition will
;;        have to be constructed at runtime.

;; Preactivation and forward advice do not contradict each other.  It is
;; perfectly ok to load a file with a preactivating `defadvice' before the
;; original definition of the advised function is available.  The constructed
;; advised definition will be used once the original function gets defined and
;; its advice gets activated.  The only constraint is that at the time the
;; file with the preactivating `defadvice' got compiled the original function
;; definition was available.

;; TIPS: Here are some indications that a preactivation did not work the way
;;       you intended it to work:
;;       - Activation of the advised function takes longer than usual/expected
;;       - The byte-compiler gets loaded while an advised function gets
;;         activated
;;       - `byte-compile' is part of the `features' variable even though you
;;         did not use the byte-compiler
;;       Right now advice does not provide an elegant way to find out whether
;;       and why a preactivation failed.  What you can do is to trace the
;;       function `ad-cache-id-verification-code' (with the function
;;       `trace-function-background' defined in my trace.el package) before
;;       any of your advised functions get activated.  After they got
;;       activated check whether all calls to `ad-cache-id-verification-code'
;;       returned `verified' as a result.  Other values indicate why the
;;       verification failed which should give you enough information to
;;       fix your preactivation/compile/load/activation sequence.

;; IMPORTANT: There is one case (that I am aware of) that can make
;; preactivation fail, i.e., a preconstructed advised definition that does
;; NOT match the current state of advice gets used nevertheless.  That case
;; arises if one package defines a certain piece of advice which gets used
;; during preactivation, and another package incompatibly redefines that
;; very advice (i.e., same function/class/name), and it is the second advice
;; that is available when the preconstructed definition gets activated, and
;; that was the only definition of that advice so far (`ad-add-advice'
;; catches advice redefinitions and clears the cache in such a case).
;; Catching that would make the cache verification too expensive.

;; MORAL-II: Redefining somebody else's advice is BAAAAD (to speak with
;; George Walker Bush), and why would you redefine your own advice anyway?
;; Advice is a mechanism to facilitate function redefinition, not advice
;; redefinition (wait until I write Meta-Advice :-).  If you really have
;; to undo somebody else's advice, try to write a "neutralizing" advice.

;; @@ Advising macros and other dangerous things:
;; ==============================================
;; Look at the corresponding tutorial sections for more information on
;; these topics.  Here it suffices to point out that the special treatment
;; of macros can lead to problems when they get advised.  Macros can create
;; problems because they get expanded at compile or load time, hence, they
;; might not have all the necessary runtime support and such advice cannot be
;; de/activated or changed as it is possible for functions.
;;
;; Special forms cannot be advised.
;;
;; MORAL: - Only advise macros when you are absolutely sure what you are doing.

;; @@ Adding a piece of advice with `ad-add-advice':
;; =================================================
;; The non-interactive function `ad-add-advice' can be used to add a piece of
;; advice to some function without using `defadvice'.  This is useful if advice
;; has to be added somewhere by a function (also look at `ad-make-advice').

;; @@ Activation/deactivation advices, file load hooks:
;; ====================================================
;; There are two special classes of advice called `activation' and
;; `deactivation'.  The body forms of these advices are not included into the
;; advised definition of a function, rather they are assembled into a hook
;; form which will be evaluated whenever the advice-info of the advised
;; function gets activated or deactivated.  One application of this mechanism
;; is to define file load hooks for files that do not provide such hooks.
;; For example, suppose you want to print a message whenever `file-x' gets
;; loaded, and suppose the last function defined in `file-x' is
;; `file-x-last-fn'.  Then we can define the following advice:
;;
;;   (defadvice file-x-last-fn (activation file-x-load-hook)
;;      "Executed whenever file-x is loaded"
;;      (if load-in-progress (message "Loaded file-x")))
;;
;; This will constitute a forward advice for function `file-x-last-fn' which
;; will get activated when `file-x' is loaded (only if forward advice is
;; enabled of course).  Because there are no "real" pieces of advice
;; available for it, its definition will not be changed, but the activation
;; advice will be run during its activation which is equivalent to having a
;; file load hook for `file-x'.

;; @@ Summary of main advice concepts:
;; ===================================
;; - Definition:
;;     A piece of advice gets defined with `defadvice' and added to the
;;     `advice-info' property of a function.
;; - Enablement:
;;     Every piece of advice has an enablement flag associated with it.  Only
;;     enabled advices are considered during construction of an advised
;;     definition.
;; - Activation:
;;     Redefine an advised function with its advised definition.  Constructs
;;     an advised definition from scratch if no verifiable cached advised
;;     definition is available and caches it.
;; - Deactivation:
;;     Back-define an advised function to its original definition.
;; - Update:
;;     Reactivate an advised function but only if its advice is currently
;;     active.  This can be used to bring all currently advised function up
;;     to date with the current state of advice without also activating
;;     currently inactive functions.
;; - Caching:
;;     Is the saving of an advised definition and an identifying cache-id so
;;     it can be reused, for example, for activation after deactivation.
;; - Preactivation:
;;     Is the construction of an advised definition according to the current
;;     state of advice during byte-compilation of a file with a preactivating
;;     `defadvice'.  That advised definition can then rather cheaply be used
;;     during activation without having to construct an advised definition
;;     from scratch at runtime.

;; @@ Summary of interactive advice manipulation functions:
;; ========================================================
;; The following interactive functions can be used to manipulate the state
;; of advised functions (all of them support completion on function names,
;; advice classes and advice names):

;; - ad-activate to activate the advice of a FUNCTION
;; - ad-deactivate to deactivate the advice of a FUNCTION
;; - ad-update   to activate the advice of a FUNCTION unless it was not
;;               yet activated or is currently inactive.
;; - ad-unadvise deactivates a FUNCTION and removes all of its advice
;;               information, hence, it cannot be activated again
;; - ad-recover  tries to redefine a FUNCTION to its original definition and
;;               discards all advice information (a low-level `ad-unadvise').
;;               Use only in emergencies.

;; - ad-remove-advice removes a particular piece of advice of a FUNCTION.
;;               You still have to do call `ad-activate' or `ad-update' to
;;               activate the new state of advice.
;; - ad-enable-advice enables a particular piece of advice of a FUNCTION.
;; - ad-disable-advice disables a particular piece of advice of a FUNCTION.
;; - ad-enable-regexp maps over all currently advised functions and enables
;;               every advice whose name contains a match for a regular
;;               expression.
;; - ad-disable-regexp disables matching advices.

;; - ad-activate-regexp   activates all advised function with a matching advice
;; - ad-deactivate-regexp deactivates all advised function with matching advice
;; - ad-update-regexp     updates all advised function with a matching advice
;; - ad-activate-all      activates all advised functions
;; - ad-deactivate-all    deactivates all advised functions
;; - ad-update-all        updates all advised functions
;; - ad-unadvise-all      unadvises all advised functions
;; - ad-recover-all       recovers all advised functions

;; - ad-compile byte-compiles a function/macro if it is compilable.

;; @@ Summary of forms with special meanings when used within an advice:
;; =====================================================================
;;   ad-return-value   name of the return value variable (get/settable)
;;   (ad-get-arg <pos>), (ad-get-args <pos>),
;;   (ad-set-arg <pos> <value>), (ad-set-args <pos> <value-list>)
;;                     argument access text macros to get/set the values of
;;                     actual arguments at a certain position
;;   ad-arg-bindings   text macro that returns the actual names, values
;;                     and types of the arguments as a list of bindings.  The
;;                     order of the bindings corresponds to the order of the
;;                     arguments.  The individual fields of every binding (name,
;;                     value and type) can be accessed with the function
;;                     `ad-arg-binding-field' (see example above).
;;   ad-do-it          text macro that identifies the place where the original
;;                     or wrapped definition should go in an around advice


;; @ Foo games: An advice tutorial
;; ===============================
;; The following tutorial was created in Emacs 18.59.  Left-justified
;; s-expressions are input forms followed by one or more result forms.
;;
;; We start by defining an innocent looking function `foo' that simply
;; adds 1 to its argument X:
;;
;; (defun foo (x)
;;   "Add 1 to X."
;;   (1+ x))
;; foo
;;
;; (foo 3)
;; 4
;;
;; @@ Defining a simple piece of advice:
;; =====================================
;; Now let's define the first piece of advice for `foo'.  To do that we
;; use the macro `defadvice' which takes a function name, a list of advice
;; specifiers and a list of body forms as arguments.  The first element of
;; the advice specifiers is the class of the advice, the second is its name,
;; the third its position and the rest are some flags.  The class of our
;; first advice is `before', its name is `fg-add2', its position among the
;; currently defined before advices (none so far) is `first', and the advice
;; will be `activate'ed immediately.  Advice names are global symbols, hence,
;; the name space conventions used for function names should be applied.  All
;; advice names in this tutorial will be prefixed with `fg' for `Foo Games'
;; (because everybody has the right to be inconsistent all the function names
;; used in this tutorial do NOT follow this convention).
;;
;; In the body of an advice we can refer to the argument variables of the
;; original function by name.  Here we add 1 to X so the effect of calling
;; `foo' will be to actually add 2. All of the advice definitions below only
;; have one body form for simplicity, but there is no restriction to that
;; extent.  Every piece of advice can have a documentation string which will
;; be combined with the documentation of the original function.
;;
;; (defadvice foo (before fg-add2 first activate)
;;   "Add 2 to X."
;;   (setq x (1+ x)))
;; foo
;;
;; (foo 3)
;; 5
;;
;; @@ Specifying the position of an advice:
;; ========================================
;; Now we define the second before advice which will cancel the effect of
;; the previous advice.  This time we specify the position as 0 which is
;; equivalent to `first'.  A number can be used to specify the zero-based
;; position of an advice among the list of advices in the same class.  This
;; time we already have one before advice hence the position specification
;; actually has an effect.  So, after the following definition the position
;; of the previous advice will be 1 even though we specified it with `first'
;; above, the reason for this is that the position argument is relative to
;; the currently defined pieces of advice which by now has changed.
;;
;; (defadvice foo (before fg-cancel-add2 0 activate)
;;   "Again only add 1 to X."
;;   (setq x (1- x)))
;; foo
;;
;; (foo 3)
;; 4
;;
;; @@ Redefining a piece of advice:
;; ================================
;; Now we define an advice with the same class and same name but with a
;; different position.  Defining an advice in a class in which an advice with
;; that name already exists is interpreted as a redefinition of that
;; particular advice, in which case the position argument will be ignored
;; and the previous position of the redefined piece of advice is used.
;; Advice flags can be specified with non-ambiguous initial substrings, hence,
;; from now on we'll use `act' instead of the verbose `activate'.
;;
;; (defadvice foo (before fg-cancel-add2 last act)
;;   "Again only add 1 to X."
;;   (setq x (1- x)))
;; foo
;;
;; @@ Assembly of advised documentation:
;; =====================================
;; The documentation strings of the various pieces of advice are assembled
;; in order which shows that advice `fg-cancel-add2' is still the first
;; `before' advice even though we specified position `last' above:
;;
;; (documentation 'foo)
;; "Add 1 to X.
;;
;; This function is advised with the following advice(s):
;;
;; fg-cancel-add2 (before):
;; Again only add 1 to X.
;;
;; fg-add2 (before):
;; Add 2 to X."
;;
;; @@ Advising interactive behavior:
;; =================================
;; We can make a function interactive (or change its interactive behavior)
;; by specifying an interactive form in one of the before or around
;; advices (there could also be body forms in this advice).  The particular
;; definition always assigns 5 as an argument to X which gives us 6 as a
;; result when we call foo interactively:
;;
;; (defadvice foo (before fg-inter last act)
;;   "Use 5 as argument when called interactively."
;;   (interactive (list 5)))
;; foo
;;
;; (call-interactively 'foo)
;; 6
;;
;; If more than one advice have an interactive declaration, then the one of
;; the advice with the smallest position will be used (before advices go
;; before around and after advices), hence, the declaration below does
;; not have any effect:
;;
;; (defadvice foo (before fg-inter2 last act)
;;   (interactive (list 6)))
;; foo
;;
;; (call-interactively 'foo)
;; 6
;;
;; @@ Around advices:
;; ==================
;; Now we'll try some `around' advices.  An around advice is a wrapper around
;; the original definition.  It can shadow or establish bindings for the
;; original definition, and it can look at and manipulate the value returned
;; by the original function.  The position of the special keyword `ad-do-it'
;; specifies where the code of the original function will be executed.  The
;; keyword can appear multiple times which will result in multiple calls of
;; the original function in the resulting advised code.  Note, that if we don't
;; specify a position argument (i.e., `first', `last' or a number), then
;; `first' (or 0) is the default):
;;
;; (defadvice foo (around fg-times-2 act)
;;   "First double X."
;;   (let ((x (* x 2)))
;;     ad-do-it))
;; foo
;;
;; (foo 3)
;; 7
;;
;; Around advices are assembled like onion skins where the around advice
;; with position 0 is the outermost skin and the advice at the last position
;; is the innermost skin which is directly wrapped around the call of the
;; original definition of the function.  Hence, after the next `defadvice' we
;; will first multiply X by 2 then add 1 and then call the original
;; definition (i.e., add 1 again):
;;
;; (defadvice foo (around fg-add-1 last act)
;;   "Add 1 to X."
;;   (let ((x (1+ x)))
;;     ad-do-it))
;; foo
;;
;; (foo 3)
;; 8
;;
;; @@ Controlling advice activation:
;; =================================
;; In every `defadvice' so far we have used the flag `activate' to activate
;; the advice immediately after its definition, and that's what we want in
;; most cases.  However, if we define multiple pieces of advice for a single
;; function then activating every advice immediately is inefficient.  A
;; better way to do this is to only activate the last defined advice.
;; For example:
;;
;; (defadvice foo (after fg-times-x)
;;   "Multiply the result with X."
;;   (setq ad-return-value (* ad-return-value x)))
;; foo
;;
;; This still yields the same result as before:
;; (foo 3)
;; 8
;;
;; Now we define another advice and activate which will also activate the
;; previous advice `fg-times-x'.  Note the use of the special variable
;; `ad-return-value' in the body of the advice which is set to the result of
;; the original function.  If we change its value then the value returned by
;; the advised function will be changed accordingly:
;;
;; (defadvice foo (after fg-times-x-again act)
;;   "Again multiply the result with X."
;;   (setq ad-return-value (* ad-return-value x)))
;; foo
;;
;; Now the advices have an effect:
;;
;; (foo 3)
;; 72
;;
;; @@ Protecting advice execution:
;; ===============================
;; Once in a while we define an advice to perform some cleanup action,
;; for example:
;;
;; (defadvice foo (after fg-cleanup last act)
;;   "Do some cleanup."
;;   (print "Let's clean up now!"))
;; foo
;;
;; However, in case of an error the cleanup won't be performed:
;;
;; (condition-case error
;;     (foo t)
;;   (error 'error-in-foo))
;; error-in-foo
;;
;; To make sure a certain piece of advice gets executed even if some error or
;; non-local exit occurred in any preceding code, we can protect it by using
;; the `protect' keyword (if any of the around advices is protected then the
;; whole around advice onion will be protected):
;;
;; (defadvice foo (after fg-cleanup prot act)
;;   "Do some protected cleanup."
;;   (print "Let's clean up now!"))
;; foo
;;
;; Now the cleanup form will be executed even in case of an error:
;;
;; (condition-case error
;;     (foo t)
;;   (error 'error-in-foo))
;; "Let's clean up now!"
;; error-in-foo
;;
;; @@ Compilation of advised definitions:
;; ======================================
;; Finally, we can specify the `compile' keyword in a `defadvice' to say
;; that we want the resulting advised function to be byte-compiled
;; (`compile' will be ignored unless we also specified `activate'):
;;
;; (defadvice foo (after fg-cleanup prot act comp)
;;   "Do some protected cleanup."
;;   (print "Let's clean up now!"))
;; foo
;;
;; Now `foo's advice is compiled:
;;
;; (compiled-function-p 'ad-Advice-foo)
;; t
;;
;; (foo 3)
;; "Let's clean up now!"
;; 72
;;
;; @@ Enabling and disabling pieces of advice:
;; ===========================================
;; Once in a while it is desirable to temporarily disable a piece of advice
;; so that it won't be considered during activation, for example, if two
;; different packages advise the same function and one wants to temporarily
;; neutralize the effect of the advice of one of the packages.
;;
;; The following disables the after advice `fg-times-x' in the function `foo'.
;; All that does is to change a flag for this particular advice.  All the
;; other information defining it will be left unchanged (e.g., its relative
;; position in this advice class, etc.).
;;
;; (ad-disable-advice 'foo 'after 'fg-times-x)
;; nil
;;
;; For this to have an effect we have to activate `foo':
;;
;; (ad-activate 'foo)
;; foo
;;
;; (foo 3)
;; "Let's clean up now!"
;; 24
;;
;; If we want to disable all multiplication advices in `foo' we can use a
;; regular expression that matches the names of such advices.  Actually, any
;; advice name that contains a match for the regular expression will be
;; called a match.  A special advice class `any' can be used to consider
;; all advice classes:
;;
;; (ad-disable-advice 'foo 'any "^fg-.*times")
;; nil
;;
;; (ad-activate 'foo)
;; foo
;;
;; (foo 3)
;; "Let's clean up now!"
;; 5
;;
;; To enable the disabled advice we could use either `ad-enable-advice'
;; similar to `ad-disable-advice', or as an alternative `ad-enable-regexp'
;; which will enable matching advices in ALL currently advised functions.
;; Hence, this can be used to dis/enable advices made by a particular
;; package to a set of functions as long as that package obeys standard
;; advice name conventions.  We prefixed all advice names with `fg-', hence
;; the following will do the trick (`ad-enable-regexp' returns the number
;; of matched advices):
;;
;; (ad-enable-regexp "^fg-")
;; 9
;;
;; The following will activate all currently active advised functions that
;; contain some advice matched by the regular expression.  This is a save
;; way to update the activation of advised functions whose advice changed
;; in some way or other without accidentally also activating currently
;; inactive functions:
;;
;; (ad-update-regexp "^fg-")
;; nil
;;
;; (foo 3)
;; "Let's clean up now!"
;; 72
;;
;; Another use for the dis/enablement mechanism is to define a piece of advice
;; and keep it "dormant" until a particular condition is satisfied, i.e., until
;; then the advice will not be used during activation.  The `disable' flag lets
;; one do that with `defadvice':
;;
;; (defadvice foo (before fg-1-more dis)
;;   "Add yet 1 more."
;;   (setq x (1+ x)))
;; foo
;;
;; (ad-activate 'foo)
;; foo
;;
;; (foo 3)
;; "Let's clean up now!"
;; 72
;;
;; (ad-enable-advice 'foo 'before 'fg-1-more)
;; nil
;;
;; (ad-activate 'foo)
;; foo
;;
;; (foo 3)
;; "Let's clean up now!"
;; 160
;;
;; @@ Caching:
;; ===========
;; Advised definitions get cached to allow efficient activation/deactivation
;; without having to reconstruct them if nothing in the advice-info of a
;; function has changed.  The following idiom can be used to temporarily
;; deactivate functions that have a piece of advice defined by a certain
;; package (we save the old definition to check out caching):
;;
;; (setq old-definition (symbol-function 'ad-Advice-foo))
;; (lambda (x) ....)
;;
;; (ad-deactivate-regexp "^fg-")
;; nil
;;
;; (foo 3)
;; 4
;;
;; (ad-activate-regexp "^fg-")
;; nil
;;
;; (eq old-definition (symbol-function 'ad-Advice-foo))
;; t
;;
;; (foo 3)
;; "Let's clean up now!"
;; 160
;;
;; @@ Forward advice:
;; ==================
;;
;; Let's define a piece of advice for an undefined function:
;;
;; (defadvice bar (before fg-sub-1-more act)
;;   "Subtract one more from X."
;;   (setq x (1- x)))
;; bar
;;
;; `bar' is not yet defined:
;; (fboundp 'bar)
;; nil
;;
;; Now we define it and the forward advice will get activated:
;;
;; (defun bar (x)
;;   "Subtract 1 from X."
;;   (1- x))
;; bar
;;
;; (bar 4)
;; 2
;;
;; Redefinition will activate any available advice if the value of
;; `ad-redefinition-action' is either `warn', `accept' or `discard':
;;
;; (defun bar (x)
;;   "Subtract 2 from X."
;;   (- x 2))
;; bar
;;
;; (bar 4)
;; 1
;;
;; @@ Preactivation:
;; =================
;; Constructing advised definitions is moderately expensive, hence, it is
;; desirable to have a way to construct them at byte-compile time.
;; Preactivation is a mechanism that allows one to do that.
;;
;; (defun fie (x)
;;   "Multiply X by 2."
;;   (* x 2))
;; fie
;;
;; (defadvice fie (before fg-times-4 preact)
;;   "Multiply X by 4."
;;   (setq x (* x 2)))
;; fie
;;
;; This advice did not affect `fie'...
;;
;; (fie 2)
;; 4
;;
;; ...but it constructed a cached definition that will be used once `fie' gets
;; activated as long as its current advice state is the same as it was during
;; preactivation:
;;
;; (setq cached-definition (ad-get-cache-definition 'fie))
;; (lambda (x) ....)
;;
;; (ad-activate 'fie)
;; fie
;;
;; (eq cached-definition (symbol-function 'ad-Advice-fie))
;; t
;;
;; (fie 2)
;; 8
;;
;; If you put a preactivating `defadvice' into a Lisp file that gets byte-
;; compiled then the constructed advised definition will get compiled by
;; the byte-compiler.  For that to occur in a v18 Emacs you had to put the
;; `defadvice' inside a `defun' because the v18 compiler did not compile
;; top-level forms other than `defun' or `defmacro', for example,
;;
;; (defun fg-defadvice-fum ()
;;   (defadvice fum (before fg-times-4 preact act)
;;     "Multiply X by 4."
;;     (setq x (* x 2))))
;; fg-defadvice-fum
;;
;; So far, no `defadvice' for `fum' got executed, but when we compile
;; `fg-defadvice-fum' the `defadvice' will be expanded by the byte compiler.
;; In order for preactivation to be effective we have to have a proper
;; definition of `fum' around at preactivation time, hence, we define it now:
;;
;; (defun fum (x)
;;   "Multiply X by 2."
;;   (* x 2))
;; fum
;;
;; Now we compile the defining function which will construct an advised
;; definition during expansion of the `defadvice', compile it and store it
;; as part of the compiled `fg-defadvice-fum':
;;
;; (ad-compile-function 'fg-defadvice-fum)
;; (lambda nil (byte-code ...))
;;
;; `fum' is still completely unaffected:
;;
;; (fum 2)
;; 4
;;
;; (ad-get-advice-info 'fum)
;; nil
;;
;; (fg-defadvice-fum)
;; fum
;;
;; Now the advised version of `fum' is compiled because the compiled definition
;; constructed during preactivation was used, even though we did not specify
;; the `compile' flag:
;;
;; (compiled-function-p 'ad-Advice-fum)
;; t
;;
;; (fum 2)
;; 8
;;
;; A preactivated definition will only be used if it matches the current
;; function definition and advice information.  If it does not match it
;; will simply be discarded and a new advised definition will be constructed
;; from scratch.  For example, let's first remove all advice-info for `fum':
;;
;; (ad-unadvise 'fum)
;; (("fie") ("bar") ("foo") ...)
;;
;; And now define a new piece of advice:
;;
;; (defadvice fum (before fg-interactive act)
;;   "Make fum interactive."
;;   (interactive "nEnter x: "))
;; fum
;;
;; When we now try to use a preactivation it will not be used because the
;; current advice state is different from the one at preactivation time.  This
;; is no tragedy, everything will work as expected just not as efficient,
;; because a new advised definition has to be constructed from scratch:
;;
;; (fg-defadvice-fum)
;; fum
;;
;; A new uncompiled advised definition got constructed:
;;
;; (compiled-function-p 'ad-Advice-fum)
;; nil
;;
;; (fum 2)
;; 8
;;
;; MORAL: To get all the efficiency out of preactivation the function
;; definition and advice state at preactivation time must be the same as the
;; state at activation time.  Preactivation does work with forward advice, all
;; that's necessary is that the definition of the forward advised function is
;; available when the `defadvice' with the preactivation gets compiled.
;;
;; @@ Portable argument access:
;; ============================
;; So far, we always used the actual argument variable names to access an
;; argument in a piece of advice.  For many advice applications this is
;; perfectly ok and keeps advices simple.  However, it decreases portability
;; of advices because it assumes specific argument variable names.  For example,
;; if one advises a subr such as `eval-region' which then gets redefined by
;; some package (e.g., edebug) into a function with different argument names,
;; then a piece of advice written for `eval-region' that was written with
;; the subr arguments in mind will break.
;;
;; Argument access text macros allow one to access arguments of an advised
;; function in a portable way without having to worry about all these
;; possibilities.  These macros will be translated into the proper access forms
;; at activation time, hence, argument access will be as efficient as if
;; the arguments had been used directly in the definition of the advice.
;;
;; (defun fuu (x y z)
;;   "Add 3 numbers."
;;   (+ x y z))
;; fuu
;;
;; (fuu 1 1 1)
;; 3
;;
;; Argument access macros specify actual arguments at a certain position.
;; Position 0 access the first actual argument, position 1 the second etc.
;; For example, the following advice adds 1 to each of the 3 arguments:
;;
;; (defadvice fuu (before fg-add-1-to-all act)
;;   "Adds 1 to all arguments."
;;   (ad-set-arg 0 (1+ (ad-get-arg 0)))
;;   (ad-set-arg 1 (1+ (ad-get-arg 1)))
;;   (ad-set-arg 2 (1+ (ad-get-arg 2))))
;; fuu
;;
;; (fuu 1 1 1)
;; 6
;;
;; Now suppose somebody redefines `fuu' with a rest argument.  Our advice
;; will still work because we used access macros (note, that automatic
;; advice activation is still in effect, hence, the redefinition of `fuu'
;; will automatically activate all its advice):
;;
;; (defun fuu (&rest numbers)
;;   "Add NUMBERS."
;;   (apply '+ numbers))
;; fuu
;;
;; (fuu 1 1 1)
;; 6
;;
;; (fuu 1 1 1 1 1 1)
;; 9
;;
;; What's important to notice is that argument access macros access actual
;; arguments regardless of how they got distributed onto argument variables.
;; In Emacs Lisp the semantics of an actual argument is determined purely
;; by position, hence, as long as nobody changes the semantics of what a
;; certain actual argument at a certain position means the access macros
;; will do the right thing.
;;
;; Because of &rest arguments we need a second kind of access macro that
;; can access all actual arguments starting from a certain position:
;;
;; (defadvice fuu (before fg-print-args act)
;;   "Print all arguments."
;;   (print (ad-get-args 0)))
;; fuu
;;
;; (fuu 1 2 3 4 5)
;; (1 2 3 4 5)
;; 18
;;
;; (defadvice fuu (before fg-set-args act)
;;   "Swaps 2nd and 3rd arg and discards all the rest."
;;   (ad-set-args 1 (list (ad-get-arg 2) (ad-get-arg 1))))
;; fuu
;;
;; (fuu 1 2 3 4 4 4 4 4 4)
;; (1 3 2)
;; 9
;;
;; (defun fuu (x y z)
;;   "Add 3 numbers."
;;   (+ x y z))
;;
;; (fuu 1 2 3)
;; (1 3 2)
;; 9
;;
;; @@ Defining the argument list of an advised function:
;; =====================================================
;; Once in a while it might be desirable to advise a function and additionally
;; give it an extra argument that controls the advised code, for example, one
;; might want to make an interactive function sensitive to a prefix argument.
;; For such cases `defadvice' allows the specification of an argument list
;; for the advised function.  Similar to the redefinition of interactive
;; behavior, the first argument list specification found in the list of before/
;; around/after advices will be used.  Of course, the specified argument list
;; should be downward compatible with the original argument list, otherwise
;; functions that call the advised function with the original argument list
;; in mind will break.
;;
;; (defun fii (x)
;;   "Add 1 to X."
;;   (1+ x))
;; fii
;;
;; Now we advise `fii' to use an optional second argument that controls the
;; amount of incrementing.  A list following the (optional) position
;; argument of the advice will be interpreted as an argument list
;; specification.  This means you cannot specify an empty argument list, and
;; why would you want to anyway?
;;
;; (defadvice fii (before fg-inc-x (x &optional incr) act)
;;   "Increment X by INCR (default is 1)."
;;   (setq x (+ x (1- (or incr 1)))))
;; fii
;;
;; (fii 3)
;; 4
;;
;; (fii 3 2)
;; 5
;;
;; @@ Advising interactive subrs:
;; ==============================
;; For the most part there is no difference between advising functions and
;; advising subrs.  There is one situation though where one might have to write
;; slightly different advice code for subrs than for functions.  This case
;; arises when one wants to access subr arguments in a before/around advice
;; when the arguments were determined by an interactive call to the subr.
;; Advice cannot determine what `interactive' form determines the interactive
;; behavior of the subr, hence, when it calls the original definition in an
;; interactive subr invocation it has to use `call-interactively' to generate
;; the proper interactive behavior.  Thus up to that call the arguments of the
;; interactive subr will be nil.  For example, the following advice for
;; `kill-buffer' will not work in an interactive invocation...
;;
;; (defadvice kill-buffer (before fg-kill-buffer-hook first act preact comp)
;;   (my-before-kill-buffer-hook (ad-get-arg 0)))
;; kill-buffer
;;
;; ...because the buffer argument will be nil in that case.  The way out of
;; this dilemma is to provide an `interactive' specification that mirrors
;; the interactive behavior of the unadvised subr, for example, the following
;; will do the right thing even when `kill-buffer' is called interactively:
;;
;; (defadvice kill-buffer (before fg-kill-buffer-hook first act preact comp)
;;   (interactive "bKill buffer: ")
;;   (my-before-kill-buffer-hook (ad-get-arg 0)))
;; kill-buffer
;;
;; @@ Advising macros:
;; ===================
;; Advising macros is slightly different because there are two significant
;; time points in the invocation of a macro: Expansion and evaluation time.
;; For an advised macro instead of evaluating the original definition we
;; use `macroexpand', that is, changing argument values and binding
;; environments by pieces of advice has an affect during macro expansion
;; but not necessarily during evaluation.  In particular, any side effects
;; of pieces of advice will occur during macro expansion.  To also affect
;; the behavior during evaluation time one has to change the value of
;; `ad-return-value' in a piece of after advice.  For example:
;;
;; (defmacro foom (x)
;;   `(list ,x))
;; foom
;;
;; (foom '(a))
;; ((a))
;;
;; (defadvice foom (before fg-print-x act)
;;   "Print the value of X."
;;   (print x))
;; foom
;;
;; The following works as expected because evaluation immediately follows
;; macro expansion:
;;
;; (foom '(a))
;; (quote (a))
;; ((a))
;;
;; However, the printing happens during expansion (or byte-compile) time:
;;
;; (macroexpand '(foom '(a)))
;; (quote (a))
;; (list (quote (a)))
;;
;; If we want it to happen during evaluation time we have to do the
;; following (first remove the old advice):
;;
;; (ad-remove-advice 'foom 'before 'fg-print-x)
;; nil
;;
;; (defadvice foom (after fg-print-x act)
;;   "Print the value of X."
;;   (setq ad-return-value
;;         `(progn (print ,x)
;;                 ,ad-return-value)))
;; foom
;;
;; (macroexpand '(foom '(a)))
;; (progn (print (quote (a))) (list (quote (a))))
;;
;; (foom '(a))
;; (a)
;; ((a))
;;
;; While this method might seem somewhat cumbersome, it is very general
;; because it allows one to influence macro expansion as well as evaluation.
;; In general, advising macros should be a rather rare activity anyway, in
;; particular, because compile-time macro expansion takes away a lot of the
;; flexibility and effectiveness of the advice mechanism.  Macros that were
;; compile-time expanded before the advice was activated will of course never
;; exhibit the advised behavior.

;;; Code:

;; @ Advice implementation:
;; ========================

;; @@ Compilation idiosyncrasies:
;; ==============================

(require 'macroexp)
(eval-when-compile (require 'cl-lib))

;; @@ Variable definitions:
;; ========================

(defgroup advice nil
  "An overloading mechanism for Emacs Lisp functions."
  :prefix "ad-"
  :link '(custom-manual "(elisp)Advising Functions")
  :group 'lisp)

;;;###autoload
(defcustom ad-redefinition-action 'warn
  "Defines what to do with redefinitions during Advice de/activation.
Redefinition occurs if a previously activated function that already has an
original definition associated with it gets redefined and then de/activated.
In such a case we can either accept the current definition as the new
original definition, discard the current definition and replace it with the
old original, or keep it and raise an error.  The values `accept', `discard',
`error' or `warn' govern what will be done.  `warn' is just like `accept' but
it additionally prints a warning message.  All other values will be
interpreted as `error'."
  :type '(choice (const accept) (const discard) (const warn)
		 (other :tag "error" error))
  :group 'advice)

;;;###autoload
(defcustom ad-default-compilation-action 'maybe
  "Defines whether to compile advised definitions during activation.
A value of `always' will result in unconditional compilation, `never' will
always avoid compilation, `maybe' will compile if the byte-compiler is already
loaded, and `like-original' will compile if the original definition of the
advised function is compiled or a built-in function.  Every other value will
be interpreted as `maybe'.  This variable will only be considered if the
COMPILE argument of `ad-activate' was supplied as nil."
  :type '(choice (const always) (const never) (const like-original)
		 (other :tag "maybe" maybe))
  :group 'advice)



;; @@ Some utilities:
;; ==================

;; We don't want the local arguments to interfere with anything
;; referenced in the supplied functions => the cryptic casing:
(defun ad-substitute-tree (sUbTrEe-TeSt fUnCtIoN tReE)
  "Substitute qualifying subTREEs with result of FUNCTION(subTREE).
Only proper subtrees are considered, for example, if TREE is (1 (2 (3)) 4)
then the subtrees will be 1 (2 (3)) 2 (3) 3 4, dotted structures are
allowed too.  Once a qualifying subtree has been found its subtrees will
not be considered anymore.  (ad-substitute-tree \\='atom \\='identity tree)
generates a copy of TREE."
  (cond ((consp tReE)
         (cons (if (funcall sUbTrEe-TeSt (car tReE))
                   (funcall fUnCtIoN (car tReE))
                 (if (consp (car tReE))
                     (ad-substitute-tree sUbTrEe-TeSt fUnCtIoN (car tReE))
                   (car tReE)))
               (ad-substitute-tree sUbTrEe-TeSt fUnCtIoN (cdr tReE))))
        ((funcall sUbTrEe-TeSt tReE)
         (funcall fUnCtIoN tReE))
        (t tReE)))

;; @@ Advice info access fns:
;; ==========================

;; Advice information for a particular function is stored on the
;; advice-info property of the function symbol.  It is stored as an
;; alist of the following format:
;;
;;      ((active . t/nil)
;;       (before adv1 adv2 ...)
;;       (around adv1 adv2 ...)
;;       (after  adv1 adv2 ...)
;;       (activation  adv1 adv2 ...)
;;       (deactivation  adv1 adv2 ...)
;;       (advicefunname . <symbol fbound to assembled advice function>)
;;       (cache . (<advised-definition> . <id>)))

;; List of currently advised though not necessarily activated functions
;; (this list is maintained as a completion table):
(defvar ad-advised-functions nil)

(defun ad-pushnew-advised-function (function)
  "Add FUNCTION to `ad-advised-functions' unless its already there."
  (add-to-list 'ad-advised-functions (symbol-name function)))

(defun ad-pop-advised-function (function)
  "Remove FUNCTION from `ad-advised-functions'."
  (setq ad-advised-functions
        (delete (symbol-name function) ad-advised-functions)))

(defmacro ad-do-advised-functions (varform &rest body)
  "`dolist'-style iterator that maps over advised functions.
\(ad-do-advised-functions (VAR)
   BODY-FORM...)
On each iteration VAR will be bound to the name of an advised function
\(a symbol)."
  (declare (indent 1))
  `(dolist (,(car varform) ad-advised-functions)
     (setq ,(car varform) (intern ,(car varform)))
     ,@body))

(defsubst ad-get-advice-info (function)
  (get function 'ad-advice-info))

(define-obsolete-function-alias 'ad-get-advice-info-macro
  #'ad-get-advice-info "27.1")

(defsubst ad-set-advice-info (function advice-info)
  (cond (advice-info
         (add-function :around (get function 'defalias-fset-function)
                       #'ad--defalias-fset))
        ((get function 'defalias-fset-function)
         (remove-function (get function 'defalias-fset-function)
                          #'ad--defalias-fset)))
  (put function 'ad-advice-info advice-info))

(defsubst ad-copy-advice-info (function)
  (copy-tree (get function 'ad-advice-info)))

(defalias 'ad-is-advised #'ad-get-advice-info
  "Return non-nil if FUNCTION has any advice info associated with it.
This does not mean that the advice is also active.")

(defun ad-initialize-advice-info (function)
  "Initialize the advice info for FUNCTION.
Assumes that FUNCTION has not yet been advised."
  (ad-pushnew-advised-function function)
  (ad-set-advice-info function (list (cons 'active nil))))

(defsubst ad-get-advice-info-field (function field)
  "Retrieve the value of the advice info FIELD of FUNCTION."
  (cdr (assq field (ad-get-advice-info function))))

(defun ad-set-advice-info-field (function field value)
  "Destructively modify VALUE of the advice info FIELD of FUNCTION."
  (let ((info (ad-get-advice-info function)))
    (and info
         (cond ((assq field info)
	        ;; A field with that name is already present:
                (rplacd (assq field info) value))
	       (t;; otherwise, create a new field with that name:
	        (nconc info (list (cons field value))))))))

;; Don't make this a macro so we can use it as a predicate:
(defun ad-is-active (function)
  "Return non-nil if FUNCTION is advised and activated."
  (ad-get-advice-info-field function 'active))


;; @@ Access fns for single pieces of advice and related predicates:
;; =================================================================

(defun ad-make-advice (name protect enable definition)
  "Constructs single piece of advice to be stored in some advice-info.
NAME should be a non-nil symbol, PROTECT and ENABLE should each be
either t or nil, and DEFINITION should be a list of the form
`(advice lambda ARGLIST [DOCSTRING] [INTERACTIVE-FORM] BODY...)'."
  (list name protect enable definition))

;; ad-find-advice uses the alist structure directly ->
;; change if this data structure changes!!
(defsubst ad-advice-name (advice) (car advice))
(defsubst ad-advice-protected (advice) (nth 1 advice))
(defsubst ad-advice-enabled (advice) (nth 2 advice))
(defsubst ad-advice-definition (advice) (nth 3 advice))

(defun ad-advice-set-enabled (advice flag)
  (rplaca (cdr (cdr advice)) flag))

(defvar ad-advice-classes '(before around after activation deactivation)
  "List of defined advice classes.")

(defun ad-class-p (thing)
  (memq thing ad-advice-classes))
(defun ad-name-p (thing)
  (and thing (symbolp thing)))
(defun ad-position-p (thing)
  (or (natnump thing)
      (memq thing '(first last))))


;; @@ Advice access functions:
;; ===========================

(defun ad-has-enabled-advice (function class)
  "True if at least one of FUNCTION's advices in CLASS is enabled."
  (cl-dolist (advice (ad-get-advice-info-field function class))
    (if (ad-advice-enabled advice) (cl-return t))))

(defun ad-has-redefining-advice (function)
  "True if FUNCTION's advice info defines at least 1 redefining advice.
Redefining advices affect the construction of an advised definition."
  (and (ad-is-advised function)
       (or (ad-has-enabled-advice function 'before)
	   (ad-has-enabled-advice function 'around)
	   (ad-has-enabled-advice function 'after))))

(defun ad-has-any-advice (function)
  "True if the advice info of FUNCTION defines at least one advice."
  (and (ad-is-advised function)
       (cl-dolist (class ad-advice-classes)
	 (if (ad-get-advice-info-field function class)
	     (cl-return t)))))

(defun ad-get-enabled-advices (function class)
  "Return the list of enabled advices of FUNCTION in CLASS."
  (let (enabled-advices)
    (dolist (advice (ad-get-advice-info-field function class))
      (if (ad-advice-enabled advice)
	  (push advice enabled-advices)))
    (reverse enabled-advices)))


;; @@ Dealing with automatic advice activation via `fset/defalias':
;; ================================================================

;; Automatic activation happens when a function gets defined via `defalias',
;; which calls the `defalias-fset-function' (which we set to
;; `ad--defalias-fset') instead of `fset', if non-nil.

;; Whether advised definitions created by automatic activations will be
;; compiled depends on the value of `ad-default-compilation-action'.

(defalias 'ad-activate-internal 'ad-activate)

(defun ad-make-advicefunname (function)
  "Make name to be used to call the assembled advice function."
  (intern (format "ad-Advice-%s" function)))

(defun ad-get-orig-definition (function) ;FIXME: Rename to "-unadvised-".
  (if (symbolp function)
      (setq function (if (fboundp function)
                         (advice--strip-macro (symbol-function function)))))
  (advice--cd*r function))

(defun ad-clear-advicefunname-definition (function)
  (let ((advicefunname (ad-get-advice-info-field function 'advicefunname)))
    (advice-remove function advicefunname)
    (fmakunbound advicefunname)))


;; @@ Interactive input functions:
;; ===============================

(declare-function function-called-at-point "help")

(defun ad-read-advised-function (&optional prompt predicate default)
  "Read name of advised function with completion from the minibuffer.
An optional PROMPT will be used to prompt for the function.  PREDICATE
plays the same role as for `try-completion' (which see).  DEFAULT will
be returned on empty input (defaults to the first advised function or
function at point for which PREDICATE returns non-nil)."
  (if (null ad-advised-functions)
      (error "ad-read-advised-function: There are no advised functions"))
  (setq default
	(or default
	    ;; Prefer func name at point, if it's an advised function etc.
	    (let ((function (progn
                              (function-called-at-point))))
	      (and function
		   (member (symbol-name function) ad-advised-functions)
		   (or (null predicate)
		       (funcall predicate function))
		   function))
            (cl-block nil
              (ad-do-advised-functions (function)
                (if (or (null predicate)
                        (funcall predicate function))
                    (cl-return function))))
	    (error "ad-read-advised-function: %s"
		   "There are no qualifying advised functions")))
  (let* ((function
	  (completing-read
	   (format-prompt (or prompt "Function") default)
	   ad-advised-functions
	   (if predicate
               (lambda (function)
                 (funcall predicate (intern function))))
	   t)))
    (if (equal function "")
	(if (ad-is-advised default)
	    default
	  (error "ad-read-advised-function: `%s' is not advised" default))
      (intern function))))

(defvar ad-advice-class-completion-table
  (mapcar (lambda (class) (list (symbol-name class)))
	  ad-advice-classes))

(defun ad-read-advice-class (function &optional prompt default)
  "Read a valid advice class with completion from the minibuffer.
An optional PROMPT will be used to prompt for the class.  DEFAULT will
be returned on empty input (defaults to the first non-empty advice
class of FUNCTION)."
  (setq default
	(or default
	    (cl-dolist (class ad-advice-classes)
	      (if (ad-get-advice-info-field function class)
		  (cl-return class)))
	    (error "ad-read-advice-class: `%s' has no advices" function)))
  (let ((class (completing-read
		(format-prompt (or prompt "Class") default)
		ad-advice-class-completion-table nil t)))
    (if (equal class "")
	default
      (intern class))))

(defun ad-read-advice-name (function class &optional prompt)
  "Read name of existing advice of CLASS for FUNCTION with completion.
An optional PROMPT is used to prompt for the name."
  (let* ((name-completion-table
          (mapcar (lambda (advice)
                    (list (symbol-name (ad-advice-name advice))))
		  (ad-get-advice-info-field function class)))
	 (default
	   (if (null name-completion-table)
	       (error "ad-read-advice-name: `%s' has no %s advice"
		      function class)
	     (car (car name-completion-table))))
	 (name (completing-read (format-prompt (or prompt "Name") default)
                                name-completion-table nil t)))
    (if (equal name "")
	(intern default)
      (intern name))))

(defun ad-read-advice-specification (&optional prompt)
  "Read a complete function/class/name specification from minibuffer.
The list of read symbols will be returned.  The optional PROMPT will
be used to prompt for the function."
  (let* ((function (ad-read-advised-function prompt))
	 (class (ad-read-advice-class function))
	 (name (ad-read-advice-name function class)))
    (list function class name)))

;; Use previous regexp as a default:
(defvar ad-last-regexp "")

(defun ad-read-regexp (&optional prompt)
  "Read a regular expression from the minibuffer."
  (let ((regexp (read-from-minibuffer
                 (format-prompt (or prompt "Regular expression")
                                (and (not (equal ad-last-regexp ""))
                                     ad-last-regexp)))))
    (setq ad-last-regexp
	  (if (equal regexp "") ad-last-regexp regexp))))


;; @@ Finding, enabling, adding and removing pieces of advice:
;; ===========================================================

(defsubst ad-find-advice (function class name)
  "Find the first advice of FUNCTION in CLASS with NAME."
  (assq name (ad-get-advice-info-field function class)))

(defun ad-advice-position (function class name)
  "Return position of first advice of FUNCTION in CLASS with NAME."
  (let* ((found-advice (ad-find-advice function class name))
	 (advices (ad-get-advice-info-field function class)))
    (if found-advice
	(- (length advices) (length (memq found-advice advices))))))

(defun ad-find-some-advice (function class name)
  "Find the first of FUNCTION's advices in CLASS matching NAME.
NAME can be a symbol or a regular expression matching part of an advice name.
If CLASS is `any' all valid advice classes will be checked."
  (if (ad-is-advised function)
      (let (found-advice)
	(cl-dolist (advice-class ad-advice-classes)
	  (if (or (eq class 'any) (eq advice-class class))
	      (setq found-advice
		    (cl-dolist (advice (ad-get-advice-info-field
					function advice-class))
		      (if (or (and (stringp name)
				   (string-match
				    name (symbol-name
					  (ad-advice-name advice))))
			      (eq name (ad-advice-name advice)))
			  (cl-return advice)))))
	  (if found-advice (cl-return found-advice))))))

(defun ad-enable-advice-internal (function class name flag)
  "Set enable FLAG of FUNCTION's advices in CLASS matching NAME.
If NAME is a string rather than a symbol then it's interpreted as a regular
expression and all advices whose name contain a match for it will be
affected.  If CLASS is `any' advices in all valid advice classes will be
considered.  The number of changed advices will be returned (or nil if
FUNCTION was not advised)."
  (if (ad-is-advised function)
      (let ((matched-advices 0))
	(dolist (advice-class ad-advice-classes)
	  (if (or (eq class 'any) (eq advice-class class))
	      (dolist (advice (ad-get-advice-info-field
                               function advice-class))
		(cond ((or (and (stringp name)
				(string-match
				 name (symbol-name (ad-advice-name advice))))
			   (eq name (ad-advice-name advice)))
		       (setq matched-advices (1+ matched-advices))
		       (ad-advice-set-enabled advice flag))))))
	matched-advices)))

;;;###autoload
(defun ad-enable-advice (function class name)
  "Enables the advice of FUNCTION with CLASS and NAME."
  (interactive (ad-read-advice-specification "Enable advice of"))
  (if (ad-is-advised function)
      (if (eq (ad-enable-advice-internal function class name t) 0)
	  (error "ad-enable-advice: `%s' has no %s advice matching `%s'"
		 function class name))
    (error "ad-enable-advice: `%s' is not advised" function)))

;;;###autoload
(defun ad-disable-advice (function class name)
  "Disable the advice of FUNCTION with CLASS and NAME."
  (interactive (ad-read-advice-specification "Disable advice of"))
  (if (ad-is-advised function)
      (if (eq (ad-enable-advice-internal function class name nil) 0)
	  (error "ad-disable-advice: `%s' has no %s advice matching `%s'"
		 function class name))
    (error "ad-disable-advice: `%s' is not advised" function)))

(defun ad-enable-regexp-internal (regexp class flag)
  "Set enable FLAGs of all CLASS advices whose name contains a REGEXP match.
If CLASS is `any' all valid advice classes are considered.  The number of
affected advices will be returned."
  (let ((matched-advices 0))
    (ad-do-advised-functions (advised-function)
      (setq matched-advices
	    (+ matched-advices
	       (or (ad-enable-advice-internal
		    advised-function class regexp flag)
		   0))))
    matched-advices))

(defun ad-enable-regexp (regexp)
  "Enables all advices with names that contain a match for REGEXP.
All currently advised functions will be considered."
  (interactive
   (list (ad-read-regexp "Enable advices via regexp")))
  (let ((matched-advices (ad-enable-regexp-internal regexp 'any t)))
    (if (called-interactively-p 'interactive)
	(message "%d matching advices enabled" matched-advices))
    matched-advices))

(defun ad-disable-regexp (regexp)
  "Disable all advices with names that contain a match for REGEXP.
All currently advised functions will be considered."
  (interactive
   (list (ad-read-regexp "Disable advices via regexp")))
  (let ((matched-advices (ad-enable-regexp-internal regexp 'any nil)))
    (if (called-interactively-p 'interactive)
	(message "%d matching advices disabled" matched-advices))
    matched-advices))

(defun ad-remove-advice (function class name)
  "Remove FUNCTION's advice with NAME from its advices in CLASS.
If such an advice was found it will be removed from the list of advices
in that CLASS."
  (interactive (ad-read-advice-specification "Remove advice of"))
  (if (ad-is-advised function)
      (let ((advice-to-remove (ad-find-advice function class name)))
	(if advice-to-remove
	    (ad-set-advice-info-field
	     function class
	     (delq advice-to-remove (ad-get-advice-info-field function class)))
	  (error "ad-remove-advice: `%s' has no %s advice `%s'"
		 function class name)))
    (error "ad-remove-advice: `%s' is not advised" function)))

;;;###autoload
(defun ad-add-advice (function advice class position)
  "Add a piece of ADVICE to FUNCTION's list of advices in CLASS.

ADVICE has the form (NAME PROTECTED ENABLED DEFINITION), where
NAME is the advice name; PROTECTED is a flag specifying whether
to protect against non-local exits; ENABLED is a flag specifying
whether to initially enable the advice; and DEFINITION has the
form (advice . LAMBDA), where LAMBDA is a lambda expression.

If FUNCTION already has a piece of advice with the same name,
then POSITION is ignored, and the old advice is overwritten with
the new one.

If FUNCTION already has one or more pieces of advice of the
specified CLASS, then POSITION determines where the new piece
goes.  POSITION can either be `first', `last' or a number (where
0 corresponds to `first', and numbers outside the valid range are
mapped to the closest extremal position).

If FUNCTION was not advised already, its advice info will be
initialized.  Redefining a piece of advice whose name is part of
the cache-id will clear the cache."
  (cond ((not (ad-is-advised function))
         (ad-initialize-advice-info function)
	 (ad-set-advice-info-field
	  function 'advicefunname (ad-make-advicefunname function))))
  (let* ((previous-position
	  (ad-advice-position function class (ad-advice-name advice)))
	 (advices (ad-get-advice-info-field function class))
	 ;; Determine a numerical position for the new advice:
	 (position (cond (previous-position)
			 ((eq position 'first) 0)
			 ((eq position 'last) (length advices))
			 ((numberp position)
			  (max 0 (min position (length advices))))
			 (t 0))))
    ;; Check whether we have to clear the cache:
    (if (memq (ad-advice-name advice) (ad-get-cache-class-id function class))
        (ad-clear-cache function))
    (if previous-position
	(setcar (nthcdr position advices) advice)
      (if (= position 0)
	  (ad-set-advice-info-field function class (cons advice advices))
	(setcdr (nthcdr (1- position) advices)
		(cons advice (nthcdr position advices)))))))


;; @@ Accessing and manipulating function definitions:
;; ===================================================

(defsubst ad-macrofy (definition)
  "Take a lambda function DEFINITION and make a macro out of it."
  (cons 'macro definition))

(defalias 'ad-lambdafy #'cdr
  "Take a macro function DEFINITION and make a lambda out of it.")

(defsubst ad-lambda-p (definition)
  ;;"non-nil if DEFINITION is a lambda expression."
  (eq (car-safe definition) 'lambda))

;; see ad-make-advice for the format of advice definitions:
(defsubst ad-advice-p (definition)
  ;;"non-nil if DEFINITION is a piece of advice."
  (eq (car-safe definition) 'advice))

(defsubst ad-compiled-p (definition)
  "Return non-nil if DEFINITION is a compiled byte-code object."
  (or (compiled-function-p definition)
      (and (macrop definition)
           (compiled-function-p (ad-lambdafy definition)))))

(defsubst ad-compiled-code (compiled-definition)
  "Return the byte-code object of a COMPILED-DEFINITION."
  (if (macrop compiled-definition)
    (ad-lambdafy compiled-definition)
    compiled-definition))

(defun ad-lambda-expression (definition)
  "Return the lambda expression of a function/macro/advice DEFINITION."
  (cond ((ad-lambda-p definition)
	 definition)
	((macrop definition)
	 (ad-lambdafy definition))
	((ad-advice-p definition)
	 (cdr definition))
	(t nil)))

(defun ad-arglist (definition)
  "Return the argument list of DEFINITION."
  (help-function-arglist
   (if (or (macrop definition) (ad-advice-p definition))
       (cdr definition)
     definition)
   'preserve-names))

(defun ad-docstring (definition)
  "Return the unexpanded docstring of DEFINITION."
  (let ((docstring
	 (if (ad-compiled-p definition)
	     (documentation definition t)
	   (car (cdr (cdr (ad-lambda-expression definition)))))))
    (if (or (stringp docstring)
	    (natnump docstring))
	docstring)))

(defun ad-interactive-form (definition)
  "Return the interactive form of DEFINITION.
Like `interactive-form', but also works on pieces of advice."
  (interactive-form
   (if (ad-advice-p definition)
       (ad-lambda-expression definition)
     definition)))

(defun ad-body-forms (definition)
  "Return the list of body forms of DEFINITION."
  (cond ((ad-compiled-p definition)
	 nil)
	((consp definition)
	 (nthcdr (+ (if (ad-docstring definition) 1 0)
		    (if (ad-interactive-form definition) 1 0))
		 (cdr (cdr (ad-lambda-expression definition)))))))

(defun ad-definition-type (definition)
  "Return symbol that describes the type of DEFINITION."
  ;; These symbols are only ever used to check a cache entry's validity.
  ;; The suffix `2' reflects the fact that we're using version 2 of advice
  ;; representations, so cache entries preactivated with version
  ;; 1 can't be used.
  (cond
   ((macrop definition) 'macro2)
   ((subrp definition) 'subr2)
   ((or (ad-lambda-p definition) (ad-compiled-p definition)) 'fun2)
   ((ad-advice-p definition) 'advice2))) ;; FIXME: Can this ever happen?

(defun ad-has-proper-definition (function)
  "True if FUNCTION is a symbol with a proper definition.
For that it has to be fbound with a non-autoload definition."
  (and (symbolp function)
       (fboundp function)
       (not (autoloadp (symbol-function function)))))

;; The following two are necessary for the sake of packages such as
;; ange-ftp which redefine functions via fcell indirection:
(defun ad-real-definition (function)
  "Find FUNCTION's definition at the end of function cell indirection."
  (if (ad-has-proper-definition function)
      (let ((definition (symbol-function function)))
	(if (symbolp definition)
	    (ad-real-definition definition)
	  definition))))

(defun ad-real-orig-definition (function)
  (let* ((fun1 (ad-get-orig-definition function))
         (fun2 (indirect-function fun1)))
    (unless (autoloadp fun2) fun2)))

(defun ad-is-compilable (function)
  "True if FUNCTION has an interpreted definition that can be compiled."
  (and (ad-has-proper-definition function)
       (or (ad-lambda-p (symbol-function function))
	   (macrop (symbol-function function)))
       (not (ad-compiled-p (symbol-function function)))))

(defvar warning-suppress-types)         ;From warnings.el.
(defun ad-compile-function (function)
  "Byte-compile the assembled advice function."
  (require 'bytecomp)
  (let ((byte-compile-warnings byte-compile-warnings)
        ;; Don't pop up windows showing byte-compiler warnings.
        (warning-suppress-types '((bytecomp))))
    (byte-compile (ad-get-advice-info-field function 'advicefunname))))

;; @@@ Accessing argument lists:
;; =============================

(defun ad-parse-arglist (arglist)
  "Parse ARGLIST into its required, optional and rest parameters.
A three-element list is returned, where the 1st element is the list of
required arguments, the 2nd is the list of optional arguments, and the 3rd
is the name of an optional rest parameter (or nil)."
  (let (required optional rest)
    (setq rest (car (cdr (memq '&rest arglist))))
    (if rest (setq arglist (reverse (cdr (memq '&rest (reverse arglist))))))
    (setq optional (cdr (memq '&optional arglist)))
    (if optional
	(setq required (reverse (cdr (memq '&optional (reverse arglist)))))
      (setq required arglist))
    (list required optional rest)))

(defun ad-retrieve-args-form (arglist)
  "Generate a form which evaluates into names/values/types of ARGLIST.
When the form gets evaluated within a function with that argument list
it will result in a list with one entry for each argument, where the
first element of each entry is the name of the argument, the second
element is its actual current value, and the third element is either
`required', `optional' or `rest' depending on the type of the argument."
  (let* ((parsed-arglist (ad-parse-arglist arglist))
	 (rest (nth 2 parsed-arglist)))
    `(list
      ,@(mapcar (lambda (req)
                  `(list ',req ,req 'required))
                (nth 0 parsed-arglist))
      ,@(mapcar (lambda (opt)
                  `(list ',opt ,opt 'optional))
                (nth 1 parsed-arglist))
      ,@(if rest (list `(list ',rest ,rest 'rest))))))

(defun ad-arg-binding-field (binding field)
  (cond ((eq field 'name) (car binding))
	((eq field 'value) (car (cdr binding)))
	((eq field 'type) (car (cdr (cdr binding))))))

(defun ad-list-access (position list)
  (cond ((= position 0) list)
	((= position 1) (list 'cdr list))
	(t (list 'nthcdr position list))))

(defun ad-element-access (position list)
  (cond ((= position 0) (list 'car list))
	((= position 1) `(car (cdr ,list)))
	(t (list 'nth position list))))

(defun ad-access-argument (arglist index)
  "Tell how to access ARGLIST's actual argument at position INDEX.
For a required/optional arg it simply returns it, if a rest argument has
to be accessed, it returns a list with the index and name."
  (let* ((parsed-arglist (ad-parse-arglist arglist))
	 (reqopt-args (append (nth 0 parsed-arglist)
			      (nth 1 parsed-arglist)))
	 (rest-arg (nth 2 parsed-arglist)))
    (cond ((< index (length reqopt-args))
	   (nth index reqopt-args))
	  (rest-arg
	   (list (- index (length reqopt-args)) rest-arg)))))

(defun ad-get-argument (arglist index)
  "Return form to access ARGLIST's actual argument at position INDEX.
INDEX counts from zero."
  (let ((argument-access (ad-access-argument arglist index)))
    (cond ((consp argument-access)
	   (ad-element-access
	    (car argument-access) (car (cdr argument-access))))
	  (argument-access))))

(defun ad-set-argument (arglist index value-form)
  "Return form to set ARGLIST's actual arg at INDEX to VALUE-FORM.
INDEX counts from zero."
  (let ((argument-access (ad-access-argument arglist index)))
    (cond ((consp argument-access)
	   ;; should this check whether there actually is something to set?
	   `(setcar ,(ad-list-access
                      (car argument-access) (car (cdr argument-access)))
             ,value-form))
	  (argument-access
	   `(setq ,argument-access ,value-form))
	  (t (error "ad-set-argument: No argument at position %d of `%s'"
		    index arglist)))))

(defun ad-get-arguments (arglist index)
  "Return form to access all actual arguments starting at position INDEX."
  (let* ((parsed-arglist (ad-parse-arglist arglist))
	 (reqopt-args (append (nth 0 parsed-arglist)
			      (nth 1 parsed-arglist)))
	 (rest-arg (nth 2 parsed-arglist))
	 args-form)
    (if (< index (length reqopt-args))
	(setq args-form `(list ,@(nthcdr index reqopt-args))))
    (if rest-arg
	(if args-form
	    (setq args-form `(nconc ,args-form ,rest-arg))
            (setq args-form (ad-list-access (- index (length reqopt-args))
                                            rest-arg))))
    args-form))

(defun ad-set-arguments (arglist index values-form)
  "Make form to assign elements of VALUES-FORM as actual ARGLIST args.
The assignment starts at position INDEX."
  (let ((values-index 0)
	argument-access set-forms)
    (while (setq argument-access (ad-access-argument arglist index))
      (push (if (symbolp argument-access)
                (ad-set-argument
                 arglist index
                 (ad-element-access values-index 'ad-vAlUeS))
              (setq arglist nil) ;; Terminate loop.
              (if (= (car argument-access) 0)
                  `(setq
                    ,(car (cdr argument-access))
                    ,(ad-list-access values-index 'ad-vAlUeS))
                `(setcdr
                  ,(ad-list-access (1- (car argument-access))
                                   (car (cdr argument-access)))
                  ,(ad-list-access values-index 'ad-vAlUeS))))
            set-forms)
      (setq index (1+ index))
      (setq values-index (1+ values-index)))
    (if (null set-forms)
	(error "ad-set-arguments: No argument at position %d of `%s'"
	       index arglist)
        (if (= (length set-forms) 1)
            ;; For exactly one set-form we can use values-form directly,...
            (ad-substitute-tree
             (lambda (form) (eq form 'ad-vAlUeS))
             (lambda (_form) values-form)
             (car set-forms))
            ;; ...if we have more we have to bind it to a variable:
            `(let ((ad-vAlUeS ,values-form))
              ,@(reverse set-forms)
              ;; work around the old backquote bug:
              ,'ad-vAlUeS)))))

(defun ad-insert-argument-access-forms (definition arglist)
  "Expands arg-access text macros in DEFINITION according to ARGLIST."
  (ad-substitute-tree
   (lambda (form)
     (or (eq form 'ad-arg-bindings)
         (and (memq (car-safe form)
                    '(ad-get-arg ad-get-args ad-set-arg ad-set-args))
              (integerp (car-safe (cdr form))))))
   (lambda (form)
     (if (eq form 'ad-arg-bindings)
         (ad-retrieve-args-form arglist)
       (let ((accessor (car form))
             (index (car (cdr form)))
             (val (car (cdr (ad-insert-argument-access-forms
                             (cdr form) arglist)))))
         (cond ((eq accessor 'ad-get-arg)
                (ad-get-argument arglist index))
               ((eq accessor 'ad-set-arg)
                (ad-set-argument arglist index val))
               ((eq accessor 'ad-get-args)
                (ad-get-arguments arglist index))
               ((eq accessor 'ad-set-args)
                (ad-set-arguments arglist index val))))))
		   definition))

;; @@@ Mapping argument lists:
;; ===========================
;; Here is the problem:
;; Suppose function foo was called with (foo 1 2 3 4 5), and foo has the
;; argument list (x y &rest z), and we want to call the function bar which
;; has argument list (a &rest b) with a combination of x, y and z so that
;; the effect is just as if we had called (bar 1 2 3 4 5) directly.
;; The mapping should work for any two argument lists.

(defun ad-map-arglists (source-arglist target-arglist)
  "Make `funcall/apply' form to map SOURCE-ARGLIST to TARGET-ARGLIST.
The arguments supplied to TARGET-ARGLIST will be taken from SOURCE-ARGLIST just
as if they had been supplied to a function with TARGET-ARGLIST directly.
Excess source arguments will be neglected, missing source arguments will be
supplied as nil.  Returns a `funcall' or `apply' form with the second element
being `function' which has to be replaced by an actual function argument.
Example:
   (ad-map-arglists \\='(a &rest args) \\='(w x y z)) will return
   (funcall ad--addoit-function a (car args) (car (cdr args)) (nth 2 args))."
  (let* ((parsed-source-arglist (ad-parse-arglist source-arglist))
	 (source-reqopt-args (append (nth 0 parsed-source-arglist)
				     (nth 1 parsed-source-arglist)))
	 (source-rest-arg (nth 2 parsed-source-arglist))
	 (parsed-target-arglist (ad-parse-arglist target-arglist))
	 (target-reqopt-args (append (nth 0 parsed-target-arglist)
				     (nth 1 parsed-target-arglist)))
	 (target-rest-arg (nth 2 parsed-target-arglist))
	 (need-apply (and source-rest-arg target-rest-arg))
	 (target-arg-index -1))
    ;; This produces ``error-proof'' target function calls with the exception
    ;; of a case like (&rest a) mapped onto (x &rest y) where the actual args
    ;; supplied to A might not be enough to supply the required target arg X
    (append (list (if need-apply 'apply 'funcall) 'ad--addoit-function)
	    (cond (need-apply
		   ;; `apply' can take care of that directly:
		   (append source-reqopt-args (list source-rest-arg)))
		  (t (mapcar (lambda (_arg)
                               (setq target-arg-index (1+ target-arg-index))
                               (ad-get-argument
                                source-arglist target-arg-index))
			     (append target-reqopt-args
				     (and target-rest-arg
					  ;; If we have a rest arg gobble up
					  ;; remaining source args:
					  (nthcdr (length target-reqopt-args)
						  source-reqopt-args)))))))))


;; @@@ Making an advised documentation string:
;; ===========================================
;; New policy: The documentation string for an advised function will be built
;; at the time the advised `documentation' function is called.  This has the
;; following advantages:
;;   1) command-key substitutions will automatically be correct
;;   2) No wasted string space due to big advised docstrings in caches or
;;      compiled files that contain preactivations
;; The overall overhead for this should be negligible because people normally
;; don't lookup documentation for the same function over and over again.

(defun ad-make-single-advice-docstring (advice class &optional style)
  (let ((advice-docstring (ad-docstring (ad-advice-definition advice))))
    (cond ((eq style 'plain)
	   advice-docstring)
	  (t (if advice-docstring
		 (format "%s-advice `%s':\n%s"
			 (capitalize (symbol-name class))
			 (ad-advice-name advice)
			 advice-docstring)
	       (format "%s-advice `%s'."
		       (capitalize (symbol-name class))
		       (ad-advice-name advice)))))))

(defun ad--make-advised-docstring (function &optional style)
  "Construct a documentation string for the advised FUNCTION.
Concatenate the original documentation with the documentation
strings of the individual pieces of advice.  Optional argument
STYLE specifies how to format the pieces of advice; it can be
`plain', or any other value which means the default formatting.

The advice documentation is shown in order of before/around/after
advice type, obeying the priority in each of these types."
  ;; Retrieve the original function documentation
  (let* ((fun (get function 'function-documentation))
	 (origdoc (unwind-protect
		      (progn (put function 'function-documentation nil)
			     (documentation function t))
		    (put function 'function-documentation fun))))
    (if (and (symbolp function)
	     (string-match "\\`ad-+Advice-" (symbol-name function)))
	(setq function
	      (intern (substring (symbol-name function) (match-end 0)))))
    (let* ((usage (help-split-fundoc origdoc function))
	   paragraphs advice-docstring)
      (setq usage (if (null usage) t (setq origdoc (cdr usage)) (car usage)))
      (if origdoc (setq paragraphs (list origdoc)))
      (dolist (class ad-advice-classes)
	(dolist (advice (ad-get-enabled-advices function class))
	  (setq advice-docstring
		(ad-make-single-advice-docstring advice class style))
	  (if advice-docstring
	      (push advice-docstring paragraphs))))
      (setq origdoc (if paragraphs
			(mapconcat 'identity (nreverse paragraphs)
				   "\n\n")))
      (help-add-fundoc-usage origdoc usage))))


;; @@@ Accessing overriding arglists and interactive forms:
;; ========================================================

(defun ad-advised-arglist (function)
  "Find first defined arglist in FUNCTION's redefining advices."
  (cl-dolist (advice (append (ad-get-enabled-advices function 'before)
			     (ad-get-enabled-advices function 'around)
			     (ad-get-enabled-advices function 'after)))
    (let ((arglist (ad-arglist (ad-advice-definition advice))))
      (if arglist
	  ;; We found the first one, use it:
	  (cl-return arglist)))))

(defun ad-advised-interactive-form (function)
  "Find first interactive form in FUNCTION's redefining advices."
  (cl-dolist (advice (append (ad-get-enabled-advices function 'before)
			     (ad-get-enabled-advices function 'around)
			     (ad-get-enabled-advices function 'after)))
    (let ((interactive-form
	   (ad-interactive-form (ad-advice-definition advice))))
      (if interactive-form
	  ;; We found the first one, use it:
	  (cl-return interactive-form)))))

;; @@@ Putting it all together:
;; ============================

(defun ad-make-advised-definition (function)
  "Generate an advised definition of FUNCTION from its advice info."
  (if (and (ad-is-advised function)
	   (ad-has-redefining-advice function))
      (let* ((origdef (ad-real-orig-definition function))
	     ;; Construct the individual pieces that we need for assembly:
	     (orig-arglist (let ((args (ad-arglist origdef)))
                             ;; The arglist may still be unknown.
                             (if (listp args) args '(&rest args))))
	     (advised-arglist (or (ad-advised-arglist function)
				  orig-arglist))
	     (interactive-form (ad-advised-interactive-form function))
	     (orig-form
              (ad-map-arglists advised-arglist orig-arglist)))

	;; Finally, build the sucker:
	(ad-assemble-advised-definition
	 advised-arglist
	 nil
	 interactive-form
	 orig-form
	 (ad-get-enabled-advices function 'before)
	 (ad-get-enabled-advices function 'around)
	 (ad-get-enabled-advices function 'after)))))

(defun ad-assemble-advised-definition
    (args docstring interactive orig &optional befores arounds afters)
  "Assemble the advices into an overall advice function.
ARGS is the argument list that has to be used,
DOCSTRING if non-nil defines the documentation of the definition,
INTERACTIVE if non-nil is the interactive form to be used,
ORIG is a form that calls the body of the original unadvised function,
and BEFORES, AROUNDS and AFTERS are the lists of advices with which ORIG
should be modified.  The assembled function will be returned."
  ;; The ad-do-it call should always have the right number of arguments,
  ;; but the compiler might signal a bogus warning because it checks the call
  ;; against the advertised calling convention.
  (let ((around-form `(setq ad-return-value (with-no-warnings ,orig)))
        before-forms around-form-protected after-forms definition)
    (dolist (advice befores)
      (cond ((and (ad-advice-protected advice)
                  before-forms)
             (setq before-forms
                   `((unwind-protect
                         ,(macroexp-progn before-forms)
                       ,@(ad-body-forms
                          (ad-advice-definition advice))))))
            (t (setq before-forms
                     (append before-forms
                             (ad-body-forms (ad-advice-definition advice)))))))

    (dolist (advice (reverse arounds))
      ;; If any of the around advices is protected then we
      ;; protect the complete around advice onion:
      (if (ad-advice-protected advice)
          (setq around-form-protected t))
      (setq around-form
            (ad-substitute-tree
             (lambda (form) (eq form 'ad-do-it))
             (lambda (_form) around-form)
             (macroexp-progn (ad-body-forms (ad-advice-definition advice))))))

    (setq after-forms
	  (if (and around-form-protected before-forms)
	      `((unwind-protect
                     ,(macroexp-progn before-forms)
                  ,around-form))
              (append before-forms (list around-form))))
    (dolist (advice afters)
      (cond ((and (ad-advice-protected advice)
                  after-forms)
             (setq after-forms
                   `((unwind-protect
                         ,(macroexp-progn after-forms)
                       ,@(ad-body-forms
                          (ad-advice-definition advice))))))
            (t (setq after-forms
                     (append after-forms
                             (ad-body-forms (ad-advice-definition advice)))))))

    (setq definition
	  `(lambda (ad--addoit-function ,@args)
            ,@(if docstring (list docstring))
            ,@(if interactive (list interactive))
            (let (ad-return-value)
              ,@after-forms
              ad-return-value)))

    (ad-insert-argument-access-forms definition args)))

;; This is needed for activation/deactivation hooks:
(defun ad-make-hook-form (function hook-name)
  "Make hook-form from FUNCTION's advice bodies in class HOOK-NAME."
  (let ((hook-forms
         (mapcar (lambda (advice)
                   (ad-body-forms (ad-advice-definition advice)))
		 (ad-get-enabled-advices function hook-name))))
    (if hook-forms
	(macroexp-progn (apply 'append hook-forms)))))


;; @@ Caching:
;; ===========
;; Generating an advised definition of a function is moderately expensive,
;; hence, it makes sense to cache it so we can reuse it in appropriate
;; circumstances.  Of course, it only makes sense to reuse a cached
;; definition if the current advice and function definition state is the
;; same as it was at the time when the cached definition was generated.
;; For that purpose we associate every cache with an id so we can verify
;; if it is still valid at a certain point in time.  This id mechanism
;; makes it possible to preactivate advised functions, write the compiled
;; advised definitions to a file and reuse them during the actual
;; activation without having to risk that the resulting definition will be
;; incorrect, well, almost.
;;
;; A cache id is a list with six elements:
;; 1) the list of names of enabled before advices
;; 2) the list of names of enabled around advices
;; 3) the list of names of enabled after advices
;; 4) the type of the original function (macro, subr, etc.)
;; 5) the arglist of the original definition (or t if it was equal to the
;;    arglist of the cached definition)
;; 6) t if the interactive form of the original definition was equal to the
;;    interactive form of the cached definition
;;
;; Here's how a cache can get invalidated or be incorrect:
;; A) a piece of advice used in the cache gets redefined
;; B) the current list of enabled advices is different from the ones used
;;    for the cache
;; C) the type of the original function changed, e.g., a function became a
;;    macro, or a subr became a function
;; D) the arglist of the original function changed
;; E) the interactive form of the original function changed
;; F) a piece of advice used in the cache got redefined before the
;;    defadvice with the cached definition got loaded: This is a PROBLEM!
;;
;; Cases A and B are the normal ones.  A is taken care of by `ad-add-advice'
;; which clears the cache in such a case, B is easily checked during
;; verification at activation time.
;;
;; Cases C, D and E have to be considered if one is slightly paranoid, i.e.,
;; if one considers the case that the original function could be different
;; from the one available at caching time (e.g., for forward advice of
;; functions that get redefined by some packages - such as `eval-region' gets
;; redefined by edebug).  All these cases can be easily checked during
;; verification.  Element 4 of the id lets one check case C, element 5 takes
;; care of case D (using t in the equality case saves some space, because the
;; arglist can be recovered at validation time from the cached definition),
;; and element 6 takes care of case E which is only a problem if the original
;; was actually a function whose interactive form was not overridden by a
;; piece of advice.
;;
;; Case F is the only one which will lead to an incorrect advised function.
;; There is no way to avoid this without storing the complete advice definition
;; in the cache-id which is not feasible.
;;
;; The cache-id of a typical advised function with one piece of advice and
;; no arglist redefinition takes 7 conses which is a small price to pay for
;; the added efficiency.  The validation itself is also pretty cheap, certainly
;; a lot cheaper than reconstructing an advised definition.

(defsubst ad-get-cache-definition (function)
  (car (ad-get-advice-info-field function 'cache)))

(defsubst ad-get-cache-id (function)
  (cdr (ad-get-advice-info-field function 'cache)))

(defsubst ad-set-cache (function definition id)
  (ad-set-advice-info-field
    function 'cache (cons definition id)))

(defun ad-clear-cache (function)
  "Clears a previously cached advised definition of FUNCTION.
Clear the cache if you want to force `ad-activate' to construct a new
advised definition from scratch."
  (interactive
   (list (ad-read-advised-function "Clear cached definition of")))
  (ad-set-advice-info-field function 'cache nil))

(defun ad-make-cache-id (function)
  "Generate an identifying image of the current advices of FUNCTION."
  (let ((original-definition (ad-real-orig-definition function))
	(cached-definition (ad-get-cache-definition function)))
    (list (mapcar #'ad-advice-name
		  (ad-get-enabled-advices function 'before))
	  (mapcar #'ad-advice-name
		  (ad-get-enabled-advices function 'around))
	  (mapcar #'ad-advice-name
		  (ad-get-enabled-advices function 'after))
	  (ad-definition-type original-definition)
	  (if (equal (ad-arglist original-definition)
		     (ad-arglist cached-definition))
	      t
	    (ad-arglist original-definition))
	  (if (eq (ad-definition-type original-definition) 'function)
	      (equal (interactive-form original-definition)
		     (interactive-form cached-definition))))))

(defun ad-get-cache-class-id (function class)
  "Return the part of FUNCTION's cache id that identifies CLASS."
  (let ((cache-id (ad-get-cache-id function)))
    (if (eq class 'before)
	(car cache-id)
      (if (eq class 'around)
	  (nth 1 cache-id)
	(nth 2 cache-id)))))

(defun ad-verify-cache-class-id (cache-class-id advices)
  (cl-dolist (advice advices (null cache-class-id))
    (if (ad-advice-enabled advice)
	(if (eq (car cache-class-id) (ad-advice-name advice))
	    (setq cache-class-id (cdr cache-class-id))
	  (cl-return nil)))))

;; There should be a way to monitor if and why a cache verification failed
;; in order to determine whether a certain preactivation could be used or
;; not.  Right now the only way to find out is to trace
;; `ad-cache-id-verification-code'.  The code it returns indicates where the
;; verification failed.  Tracing `ad-verify-cache-class-id' might provide
;; some additional useful information.

(defun ad-cache-id-verification-code (function)
  (let ((cache-id (ad-get-cache-id function))
	(code 'before-advice-mismatch))
    (and (ad-verify-cache-class-id
	  (car cache-id) (ad-get-advice-info-field function 'before))
	 (setq code 'around-advice-mismatch)
	 (ad-verify-cache-class-id
	  (nth 1 cache-id) (ad-get-advice-info-field function 'around))
	 (setq code 'after-advice-mismatch)
	 (ad-verify-cache-class-id
	  (nth 2 cache-id) (ad-get-advice-info-field function 'after))
	 (setq code 'definition-type-mismatch)
	 (let ((original-definition (ad-real-orig-definition function))
	       (cached-definition (ad-get-cache-definition function)))
	   (and (eq (nth 3 cache-id) (ad-definition-type original-definition))
		(setq code 'arglist-mismatch)
		(equal (if (eq (nth 4 cache-id) t)
			   (ad-arglist original-definition)
			 (nth 4 cache-id) )
		       (ad-arglist cached-definition))
		(setq code 'interactive-form-mismatch)
		(or (null (nth 5 cache-id))
		    (equal (interactive-form original-definition)
			   (interactive-form cached-definition)))
		(setq code 'verified))))
    code))

(defun ad-verify-cache-id (function)
  "True if FUNCTION's cache-id is compatible with its current advices."
  (eq (ad-cache-id-verification-code function) 'verified))


;; @@ Preactivation:
;; =================
;; Preactivation can be used to generate compiled advised definitions
;; at compile time without having to give up the dynamic runtime flexibility
;; of the advice mechanism.  Preactivation is a special feature of `defadvice',
;; it involves the following steps:
;;  - remembering the function's current state (definition and advice-info)
;;  - advising it with the defined piece of advice
;;  - clearing its cache
;;  - generating an interpreted advised definition by activating it, this will
;;    make use of all its current active advice and its current definition
;;  - saving the so generated cached definition and id
;;  - resetting the function's advice and definition state to what it was
;;    before the preactivation
;;  - Returning the saved definition and its id to be used in the expansion of
;;    `defadvice' to assign it as an initial cache, hence it will be compiled
;;    at time the `defadvice' gets compiled.
;; Naturally, for preactivation to be effective it has to be applied/compiled
;; at the right time, i.e., when the current state of advices and function
;; definition exactly reflects the state at activation time.  Should that not
;; be the case, the precompiled definition will just be discarded and a new
;; advised definition will be generated.

(defun ad-preactivate-advice (function advice class position)
  "Preactivate FUNCTION and return the constructed cache."
  (let* ((advicefunname (ad-get-advice-info-field function 'advicefunname))
         (old-advice (symbol-function advicefunname))
	 (old-advice-info (ad-copy-advice-info function))
	 (ad-advised-functions ad-advised-functions))
    (unwind-protect
	(progn
	  (ad-add-advice function advice class position)
	  (ad-enable-advice function class (ad-advice-name advice))
	  (ad-clear-cache function)
	  (ad-activate function -1)
	  (if (and (ad-is-active function)
	           (ad-get-cache-definition function))
	      (list (ad-get-cache-definition function)
		    (ad-get-cache-id function))))
      (ad-set-advice-info function old-advice-info)
      (advice-remove function advicefunname)
      (if advicefunname (fset advicefunname old-advice))
      (if old-advice (advice-add function :around advicefunname)))))


;; @@ Activation and definition handling:
;; ======================================

(defun ad-should-compile (function compile)
  "Return non-nil if the advised FUNCTION should be compiled.
If COMPILE is non-nil and not a negative number then it returns t.
If COMPILE is a negative number then it returns nil.
If COMPILE is nil then the result depends on the value of
`ad-default-compilation-action' (which see)."
  (cond
   ;; Don't compile until the real function definition is known (bug#12965).
   ((not (ad-real-orig-definition function)) nil)
   ((integerp compile) (>= compile 0))
   (compile)
   ((eq ad-default-compilation-action 'never) nil)
   ((eq ad-default-compilation-action 'always) t)
   ((eq ad-default-compilation-action 'like-original)
    (or (subrp (ad-get-orig-definition function))
        (ad-compiled-p (ad-get-orig-definition function))))
   ;; everything else means `maybe':
   (t (featurep 'byte-compile))))

(defun ad-activate-advised-definition (function compile)
  "Redefine FUNCTION with its advised definition from cache or scratch.
The resulting FUNCTION will be compiled if `ad-should-compile' returns t.
The current definition and its cache-id will be put into the cache."
  (let* ((verified-cached-definition
          (if (ad-verify-cache-id function)
              (ad-get-cache-definition function)))
         (advicefunname (ad-get-advice-info-field function 'advicefunname))
         (old-ispec (interactive-form advicefunname)))
    (fset advicefunname
          (or verified-cached-definition
              (eval
               (ad-make-advised-definition function)
               ;; We don't keep track of the `lexical-binding' of the
               ;; various chunks: assume it's the old dynbound dialect.
               nil)))
    (put advicefunname 'function-documentation
	 `(ad--make-advised-docstring ',advicefunname))
    (unless (equal (interactive-form advicefunname) old-ispec)
      ;; If the interactive-spec of advicefunname has changed, force nadvice to
      ;; refresh its copy.
      (advice-remove function advicefunname))
    (advice-add function :around advicefunname)
    (if (ad-should-compile function compile)
	(ad-compile-function function))
    (if verified-cached-definition
	(if (not (eq verified-cached-definition
                     (symbol-function advicefunname)))
	    ;; we must have compiled, cache the compiled definition:
	    (ad-set-cache function (symbol-function advicefunname)
                          (ad-get-cache-id function)))
      ;; We created a new advised definition, cache it with a proper id:
      (ad-clear-cache function)
      ;; ad-make-cache-id needs the new cached definition:
      (ad-set-cache function (symbol-function advicefunname) nil)
      (ad-set-cache
       function (symbol-function advicefunname) (ad-make-cache-id function)))))

(defun ad--defalias-fset (fsetfun function newdef)
  "Specialized `fset' for `defalias'.
When a function alias is re/defined, automatically activate its advices."
  (if-let ((current-definition (ad-get-orig-definition newdef))
           (original-definition (ad-get-orig-definition function)))
      (if (not (memq ad-redefinition-action '(accept discard warn)))
	  (error "Advised defalias `%s' unchanged for ad-redefinition-action '%s'"
                 function ad-redefinition-action)
        (when (and (not (eq ad-redefinition-action 'discard))
                   (not (eq current-definition original-definition)))
          ;; We have a redefinition.
          (funcall (or fsetfun #'fset) function newdef)
          (ad-activate-internal function)
	  (when (eq ad-redefinition-action 'warn)
	    (message "Advised defalias `%s' redefined" function))))
    ;; We have a new definition (null original) or undefinition (null current).
    (funcall (or fsetfun #'fset) function newdef)
    (when current-definition
      (ad-activate-internal function))))


;; @@ The top-level advice interface:
;; ==================================

;;;###autoload
(defun ad-activate (function &optional compile)
  "Activate all the advice information of an advised FUNCTION.
If FUNCTION has a proper original definition then an advised
definition will be generated from FUNCTION's advice info and the
definition of FUNCTION will be replaced with it.  If a previously
cached advised definition was available, it will be used.
The optional COMPILE argument determines whether the resulting function
or a compilable cached definition will be compiled.  If it is negative
no compilation will be performed, if it is positive or otherwise non-nil
the resulting function will be compiled, if it is nil the behavior depends
on the value of `ad-default-compilation-action' (which see).
Activation of an advised function that has an advice info but no actual
pieces of advice is equivalent to a call to `ad-unadvise'.  Activation of
an advised function that has actual pieces of advice but none of them are
enabled is equivalent to a call to `ad-deactivate'.  The current advised
definition will always be cached for later usage."
  (interactive
   (list (ad-read-advised-function "Activate advice of")
	 current-prefix-arg))
  (cond
   ((not (ad-is-advised function))
    (error "ad-activate: `%s' is not advised" function))
   ;; Just return for forward advised and not yet defined functions:
   ((not (ad-get-orig-definition function)) nil)
   ((not (ad-has-any-advice function)) (ad-unadvise function))
   ;; Otherwise activate the advice:
   ((ad-has-redefining-advice function)
    (ad-activate-advised-definition function compile)
    (ad-set-advice-info-field function 'active t)
    (eval (ad-make-hook-form function 'activation))
    function)
   ;; Here we are if we have all disabled advices:
   (t (ad-deactivate function))))

(defalias 'ad-activate-on 'ad-activate)

(defun ad-deactivate (function)
  "Deactivate the advice of an actively advised FUNCTION.
If FUNCTION has a proper original definition, then the current
definition of FUNCTION will be replaced with it.  All the advice
information will still be available so it can be activated again with
a call to `ad-activate'."
  (interactive
   (list (ad-read-advised-function "Deactivate advice of" 'ad-is-active)))
  (if (not (ad-is-advised function))
      (error "ad-deactivate: `%s' is not advised" function)
    (cond ((ad-is-active function)
	   (if (not (ad-get-orig-definition function))
	       (error "ad-deactivate: `%s' has no original definition"
		      function)
             (ad-clear-advicefunname-definition function)
	     (ad-set-advice-info-field function 'active nil)
	     (eval (ad-make-hook-form function 'deactivation))
	     function)))))

(defun ad-update (function &optional compile)
  "Update the advised definition of FUNCTION if its advice is active.
See `ad-activate' for documentation on the optional COMPILE argument."
  (interactive
   (list (ad-read-advised-function
	  "Update advised definition of" 'ad-is-active)))
  (if (ad-is-active function)
      (ad-activate function compile)))

(defun ad-unadvise (function)
  "Deactivate FUNCTION and then remove all its advice information.
If FUNCTION was not advised this will be a noop."
  (interactive
   (list (ad-read-advised-function "Unadvise function")))
  (cond ((ad-is-advised function)
	 (if (ad-is-active function)
	     (ad-deactivate function))
	 (ad-clear-advicefunname-definition function)
	 (ad-set-advice-info function nil)
	 (ad-pop-advised-function function))))

(defun ad-recover (function)
  "Try to recover FUNCTION's original definition, and unadvise it.
This is more low-level than `ad-unadvise' in that it does not do
deactivation, which might run hooks and get into other trouble.
Use in emergencies."
  ;; Use more primitive interactive behavior here: Accept any symbol that's
  ;; currently defined in obarray, not necessarily with a function definition:
  (interactive
   (list (intern
	  (completing-read "Recover advised function: " obarray nil t))))
  (cond ((ad-is-advised function)
         (ad-clear-advicefunname-definition function)
	 (ad-set-advice-info function nil)
	 (ad-pop-advised-function function))))

(defun ad-activate-regexp (regexp &optional compile)
  "Activate functions with an advice name containing a REGEXP match.
This activates the advice for each function
that has at least one piece of advice whose name includes a match for REGEXP.
See `ad-activate' for documentation on the optional COMPILE argument."
  (interactive
   (list (ad-read-regexp "Activate via advice regexp")
	 current-prefix-arg))
  (ad-do-advised-functions (function)
    (if (ad-find-some-advice function 'any regexp)
	(ad-activate function compile))))

(defun ad-deactivate-regexp (regexp)
  "Deactivate functions with an advice name containing REGEXP match.
This deactivates the advice for each function
that has at least one piece of advice whose name includes a match for REGEXP."
  (interactive
   (list (ad-read-regexp "Deactivate via advice regexp")))
  (ad-do-advised-functions (function)
    (if (ad-find-some-advice function 'any regexp)
	(ad-deactivate function))))

(defun ad-update-regexp (regexp &optional compile)
  "Update functions with an advice name containing a REGEXP match.
This reactivates the advice for each function
that has at least one piece of advice whose name includes a match for REGEXP.
See `ad-activate' for documentation on the optional COMPILE argument."
  (interactive
   (list (ad-read-regexp "Update via advice regexp")
	 current-prefix-arg))
  (ad-do-advised-functions (function)
    (if (ad-find-some-advice function 'any regexp)
	(ad-update function compile))))

(defun ad-activate-all (&optional compile)
  "Activate all currently advised functions.
See `ad-activate' for documentation on the optional COMPILE argument."
  (interactive "P")
  (ad-do-advised-functions (function)
    (ad-activate function compile)))

(defun ad-deactivate-all ()
  "Deactivate all currently advised functions."
  (interactive)
  (ad-do-advised-functions (function)
    (ad-deactivate function)))

(defun ad-update-all (&optional compile)
  "Update all currently advised functions.
With prefix argument, COMPILE resulting advised definitions."
  (interactive "P")
  (ad-do-advised-functions (function)
    (ad-update function compile)))

(defun ad-unadvise-all ()
  "Unadvise all currently advised functions."
  (interactive)
  (ad-do-advised-functions (function)
    (ad-unadvise function)))

(defun ad-recover-all ()
  "Recover all currently advised functions.  Use in emergencies.
To recover a function means to try to find its original (pre-advice)
definition, and delete all advice.
This is more low-level than `ad-unadvise' in that it does not do
deactivation, which might run hooks and get into other trouble."
  (interactive)
  (ad-do-advised-functions (function)
    (condition-case nil
	(ad-recover function)
      (error nil))))


;; Completion alist of valid `defadvice' flags
(defconst ad-defadvice-flags
  '("protect" "disable" "activate" "compile" "preactivate"))

;;;###autoload
(defmacro defadvice (function args &rest body)
  "Define a piece of advice for FUNCTION (a symbol).
The syntax of `defadvice' is as follows:

  (defadvice FUNCTION (CLASS NAME [POSITION] [ARGLIST] FLAG...)
    [DOCSTRING] [INTERACTIVE-FORM]
    BODY...)

FUNCTION ::= Name of the function to be advised.
CLASS ::= `before' | `around' | `after' | `activation' | `deactivation'.
NAME ::= Non-nil symbol that names this piece of advice.
POSITION ::= `first' | `last' | NUMBER.  Optional, defaults to `first',
    see also `ad-add-advice'.
ARGLIST ::= An optional argument list to be used for the advised function
    instead of the argument list of the original.  The first one found in
    before/around/after-advices will be used.
FLAG ::= `protect'|`disable'|`activate'|`compile'|`preactivate'.
    All flags can be specified with unambiguous initial substrings.
DOCSTRING ::= Optional documentation for this piece of advice.
INTERACTIVE-FORM ::= Optional interactive form to be used for the advised
    function.  The first one found in before/around/after-advices will be used.
BODY ::= Any s-expression.

Semantics of the various flags:
`protect': The piece of advice will be protected against non-local exits in
any code that precedes it.  If any around-advice of a function is protected
then automatically all around-advices will be protected (the complete onion).

`activate': All advice of FUNCTION will be activated immediately if
FUNCTION has been properly defined prior to this application of `defadvice'.

`compile': In conjunction with `activate' specifies that the resulting
advised function should be compiled.

`disable': The defined advice will be disabled, hence, it will not be used
during activation until somebody enables it.

`preactivate': Preactivates the advised FUNCTION at macro-expansion/compile
time.  This generates a compiled advised definition according to the current
advice state that will be used during activation if appropriate.  Only use
this if the `defadvice' gets actually compiled.

usage: (defadvice FUNCTION (CLASS NAME [POSITION] [ARGLIST] FLAG...)
          [DOCSTRING] [INTERACTIVE-FORM]
          BODY...)"
  (declare (doc-string 3) (indent 2)
           (obsolete "use `advice-add' or `define-advice'" "30.1")
           (debug (&define name  ;; thing being advised.
                           (name ;; class is [&or "before" "around" "after"
                                 ;;               "activation" "deactivation"]
                            name ;; name of advice
                            &rest sexp ;; optional position and flags
                            )
                           [&optional stringp]
                           [&optional ("interactive" interactive)]
                           def-body)))
  (if (not (ad-name-p function))
      (error "defadvice: Invalid function name: %s" function))
  (let* ((class (car args))
	 (name (if (not (ad-class-p class))
		   (error "defadvice: Invalid advice class: %s" class)
                   (nth 1 args)))
	 (position (if (not (ad-name-p name))
		       (error "defadvice: Invalid advice name: %s" name)
                       (setq args (nthcdr 2 args))
                       (if (ad-position-p (car args))
                           (prog1 (car args)
                             (setq args (cdr args))))))
	 (arglist (if (listp (car args))
		      (prog1 (car args)
			(setq args (cdr args)))))
	 (flags
	  (mapcar
           (lambda (flag)
             (let ((completion
                    (try-completion (symbol-name flag) ad-defadvice-flags)))
               (cond ((eq completion t) flag)
                     ((member completion ad-defadvice-flags)
                      (intern completion))
                     (t (error "defadvice: Invalid or ambiguous flag: %s"
                               flag)))))
	   args))
	 (advice (ad-make-advice
		  name (memq 'protect flags)
		  (not (memq 'disable flags))
		  `(advice lambda ,arglist ,@body)))
	 (preactivation (if (memq 'preactivate flags)
			    (ad-preactivate-advice
			     function advice class position))))
    ;; Now for the things to be done at evaluation time:
    `(progn
       (ad-add-advice ',function ',advice ',class ',position)
       ,@(if preactivation
             `((ad-set-cache
                ',function
                ;; the function will get compiled:
                ,(cond ((macrop (car preactivation))
                        `(ad-macrofy
                          (function
                           ,(ad-lambdafy
                             (car preactivation)))))
                       (t `(function
                            ,(car preactivation))))
                ',(car (cdr preactivation)))))
       ,@(if (memq 'activate flags)
             `((ad-activate ',function
                            ,(if (memq 'compile flags) t))))
       ',function)))


;; @@ Tools:
;; =========

(defmacro ad-with-originals (functions &rest body)
  "Binds FUNCTIONS to their original definitions and execute BODY.
For any members of FUNCTIONS that are not currently advised the rebinding will
be a noop.  Any modifications done to the definitions of FUNCTIONS will be
undone on exit of this macro."
  (declare (indent 1) (obsolete nil "27.1"))
  (let* ((index -1)
	 ;; Make let-variables to store current definitions:
	 (current-bindings
          (mapcar (lambda (function)
                    (setq index (1+ index))
                    (list (intern (format "ad-oRiGdEf-%d" index))
                          `(symbol-function ',function)))
		  functions)))
    `(let ,current-bindings
      (unwind-protect
           (progn
             ,@(progn
                ;; Make forms to redefine functions to their
                ;; original definitions if they are advised:
                (setq index -1)
                (mapcar (lambda (function)
                          (setq index (1+ index))
                           `(fset ',function
                            (or (ad-get-orig-definition ',function)
                                ,(car (nth index current-bindings)))))
                        functions))
             ,@body)
        ,@(progn
           ;; Make forms to back-define functions to the definitions
           ;; they had outside this macro call:
           (setq index -1)
           (mapcar (lambda (function)
                     (setq index (1+ index))
                       `(fset ',function
                       ,(car (nth index current-bindings))))
                   functions))))))


;; @@ Starting, stopping and recovering from the advice package magic:
;; ===================================================================

(defun ad-recover-normality ()
  "Undo all advice related redefinitions and unadvises everything.
Use only in REAL emergencies."
  (interactive)
  (ad-recover-all)
  (ad-do-advised-functions (function)
    (message "Oops! Left over advised function %S" function)
    (ad-pop-advised-function function)))

(defconst ad-version "2.14")
(make-obsolete-variable 'ad-version 'emacs-version "29.1")

(provide 'advice)

;;; advice.el ends here
