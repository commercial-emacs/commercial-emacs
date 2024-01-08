/* Evaluator for GNU Emacs Lisp interpreter.

Copyright (C) 1985-1987, 1993-1995, 1999-2024 Free Software Foundation,
Inc.

This file is NOT part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>
#include <limits.h>
#include <stdlib.h>
#include "lisp.h"
#include "blockinput.h"
#include "commands.h"
#include "keyboard.h"
#include "dispextern.h"
#include "buffer.h"
#include "pdumper.h"
#include "atimer.h"
#include "syntax.h"

/* Non-nil means record all fset's and provide's, to be undone
   if the file being autoloaded is not fully loaded.
   They are recorded by being consed onto the front of Vautoload_queue:
   (FUN . ODEF) for a defun, (0 . OFEATURES) for a provide.  */
Lisp_Object Vautoload_queue;

/* This holds either the symbol `run-hooks' or nil.
   It is nil at an early stage of startup, and when Emacs
   is shutting down.  */
Lisp_Object Vrun_hooks;

/* These would ordinarily be static, but they need to be visible to GDB.  */
bool xbacktrace_valid_p (union specbinding *) EXTERNALLY_VISIBLE;
Lisp_Object *xbacktrace_args (union specbinding *) EXTERNALLY_VISIBLE;
Lisp_Object xbacktrace_function (union specbinding *) EXTERNALLY_VISIBLE;
union specbinding *xbacktrace_next (union specbinding *) EXTERNALLY_VISIBLE;
union specbinding *xbacktrace_top (void) EXTERNALLY_VISIBLE;

static Lisp_Object funcall_lambda (Lisp_Object, ptrdiff_t, Lisp_Object *);
static Lisp_Object lambda_arity (Lisp_Object);

static Lisp_Object *
specpdl_symbol_addr (union specbinding *pdl)
{
  eassert (pdl->kind >= SPECPDL_LET);
  return &pdl->let.symbol;
}

static Lisp_Object
specpdl_symbol (union specbinding *pdl)
{
  return *specpdl_symbol_addr (pdl);
}

static enum specbind_tag
specpdl_kind (union specbinding *pdl)
{
  eassert (pdl->kind >= SPECPDL_LET);
  return pdl->let.kind;
}

static Lisp_Object *
specpdl_value_addr (union specbinding *pdl)
{
  Lisp_Object *result = NULL;
  switch (pdl->kind)
    {
    case SPECPDL_LET:
    case SPECPDL_LET_BLD:
    case SPECPDL_LET_BLV:
      result = &pdl->let.value;
      break;
    case SPECPDL_LEXICAL_ENVIRONMENT:
      result = &pdl->lexical_environment.value;
      break;
    default:
      emacs_abort ();
      break;
    }
  return result;
}

static Lisp_Object
specpdl_value (union specbinding *pdl)
{
  return *specpdl_value_addr (pdl);
}

static Lisp_Object *
specpdl_buffer_addr (union specbinding *pdl)
{
  return &pdl->let.buffer;
}

static Lisp_Object
specpdl_buffer (union specbinding *pdl)
{
  return *specpdl_buffer_addr (pdl);
}

static Lisp_Object *
specpdl_arg_addr (union specbinding *pdl)
{
  eassert (pdl->kind == SPECPDL_UNWIND);
  return &pdl->unwind.arg;
}

static Lisp_Object *
backtrace_function_addr (union specbinding *pdl)
{
  eassert (pdl->kind == SPECPDL_BACKTRACE);
  return &pdl->bt.function;
}

static Lisp_Object
backtrace_function (union specbinding *pdl)
{
  return *backtrace_function_addr (pdl);
}

Lisp_Object
xbacktrace_function (union specbinding *pdl)
{
  return backtrace_function (pdl);
}

static ptrdiff_t
backtrace_nargs (union specbinding *pdl)
{
  eassert (pdl->kind == SPECPDL_BACKTRACE);
  return pdl->bt.nargs;
}

static Lisp_Object *
backtrace_args (union specbinding *pdl)
{
  eassert (pdl->kind == SPECPDL_BACKTRACE);
  return pdl->bt.args;
}

Lisp_Object *
xbacktrace_args (union specbinding *pdl)
{
  return backtrace_args (pdl);
}

static void
set_backtrace_args (union specbinding *pdl, Lisp_Object *args, ptrdiff_t nargs)
{
  eassert (pdl->kind == SPECPDL_BACKTRACE);
  pdl->bt.args = args;
  pdl->bt.nargs = nargs;
}

static void
set_backtrace_debug_on_exit (union specbinding *pdl, bool q_debug)
{
  eassert (pdl->kind == SPECPDL_BACKTRACE);
  pdl->bt.debug_on_exit = q_debug;
}

static bool
backtrace_valid_p (const struct thread_state *thr, union specbinding *pdl)
{
  return thr->m_specpdl && pdl >= thr->m_specpdl;
}

bool
xbacktrace_valid_p (union specbinding *pdl)
{
  return backtrace_valid_p (current_thread, pdl);
}

static union specbinding *
backtrace_top (const struct thread_state *thr)
{
  union specbinding *pdl = thr->m_specpdl ? thr->m_specpdl_ptr : NULL;
  if (pdl)
    while (backtrace_valid_p (thr, --pdl) && pdl->kind != SPECPDL_BACKTRACE);
  return pdl;
}

union specbinding *
xbacktrace_top (void)
{
  return backtrace_top (current_thread);
}

static union specbinding *
backtrace_next (const struct thread_state *thr, union specbinding *pdl)
{
  while (backtrace_valid_p (thr, --pdl) && pdl->kind != SPECPDL_BACKTRACE);
  return pdl;
}

union specbinding *
xbacktrace_next (union specbinding *pdl)
{
  return backtrace_next (current_thread, pdl);
}

static void init_eval_once_for_pdumper (void);

void
init_eval_once (void)
{
  /* Don't forget to update docs (lispref node "Eval").  */
  max_lisp_eval_depth = 1600;
  Vrun_hooks = Qnil;
  pdumper_do_now_and_after_load (init_eval_once_for_pdumper);
}

static void
init_eval_once_for_pdumper (void)
{
  enum { size = 50 };
  union specbinding *pdlvec = malloc ((size + 1) * sizeof *specpdl);
  specpdl = specpdl_ptr = pdlvec + 1;
  specpdl_end = specpdl + size;
}

void
init_eval (void)
{
  specpdl_ptr = specpdl;
  current_thread->exception_stack_capacity = 16;
  current_thread->exception_stack_bottom = (struct handler *) xmalloc
    (current_thread->exception_stack_capacity
     * sizeof (*current_thread->exception_stack_bottom));
  current_thread->exception_stack_top = NULL;
  Vquit_flag = Qnil;
  debug_on_next_call = false;
  lisp_eval_depth = 0;
  when_entered_debugger = -1;
}

/* Call the Lisp debugger, giving it argument ARG.  */

Lisp_Object
call_debugger (Lisp_Object arg)
{
  bool debug_while_redisplaying;
  specpdl_ref count = SPECPDL_INDEX ();
  Lisp_Object val;

#ifdef HAVE_WINDOW_SYSTEM
  cancel_hourglass ();
#endif

  debug_on_next_call = false;
  when_entered_debugger = num_nonmacro_input_events;

  /* Resetting redisplaying_p to 0 makes sure that debug output is
     displayed if the debugger is invoked during redisplay.  */
  debug_while_redisplaying = redisplaying_p;
  redisplaying_p = false;
  specbind (intern ("debugger-may-continue"),
	    debug_while_redisplaying ? Qnil : Qt);
  specbind (Qinhibit_redisplay, Qnil);
  specbind (Qinhibit_debugger, Qt);

  /* If we are debugging an error while `inhibit-changing-match-data'
     is bound to non-nil (e.g., within a call to `string-match-p'),
     then make sure debugger code can still use match data.  */
  specbind (Qinhibit_changing_match_data, Qnil);

  val = apply1 (Vdebugger, arg);

  /* Interrupting redisplay and resuming it later is not safe under
     all circumstances.  So, when the debugger returns, abort the
     interrupted redisplay by going back to the top-level.  */
  if (debug_while_redisplaying)
    Ftop_level ();

  return unbind_to (count, val);
}

void
do_debug_on_call (Lisp_Object code, specpdl_ref count)
{
  debug_on_next_call = false;
  set_backtrace_debug_on_exit (specpdl_ref_to_ptr (count), true);
  call_debugger (list1 (code));
}

DEFUN ("or", For, Sor, 0, UNEVALLED, 0,
       doc: /* Eval args until one of them yields non-nil, then return that value.
The remaining args are not evalled at all.
If all args return nil, return nil.
usage: (or CONDITIONS...)  */)
  (Lisp_Object args)
{
  Lisp_Object val = Qnil;

  while (CONSP (args))
    {
      Lisp_Object arg = XCAR (args);
      args = XCDR (args);
      val = eval_form (arg);
      if (!NILP (val))
	break;
    }

  return val;
}

DEFUN ("and", Fand, Sand, 0, UNEVALLED, 0,
       doc: /* Eval args until one of them yields nil, then return nil.
The remaining args are not evalled at all.
If no arg yields nil, return the last arg's value.
usage: (and CONDITIONS...)  */)
  (Lisp_Object args)
{
  Lisp_Object val = Qt;

  while (CONSP (args))
    {
      Lisp_Object arg = XCAR (args);
      args = XCDR (args);
      val = eval_form (arg);
      if (NILP (val))
	break;
    }

  return val;
}

DEFUN ("if", Fif, Sif, 2, UNEVALLED, 0,
       doc: /* If COND yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE's.
THEN must be one expression, but ELSE... can be zero or more expressions.
If COND yields nil, and there are no ELSE's, the value is nil.
usage: (if COND THEN ELSE...)  */)
  (Lisp_Object args)
{
  Lisp_Object cond;

  cond = eval_form (XCAR (args));

  if (!NILP (cond))
    return eval_form (CAR (XCDR (args)));
  return Fprogn (CDR (XCDR (args)));
}

DEFUN ("cond", Fcond, Scond, 0, UNEVALLED, 0,
       doc: /* Try each clause until one succeeds.
Each clause looks like (CONDITION BODY...).  CONDITION is evaluated
and, if the value is non-nil, this clause succeeds:
then the expressions in BODY are evaluated and the last one's
value is the value of the cond-form.
If a clause has one element, as in (CONDITION), then the cond-form
returns CONDITION's value, if that is non-nil.
If no clause succeeds, cond returns nil.
usage: (cond CLAUSES...)  */)
  (Lisp_Object args)
{
  Lisp_Object val = args;

  while (CONSP (args))
    {
      Lisp_Object clause = XCAR (args);
      val = eval_form (CAR (clause));
      if (!NILP (val))
	{
	  if (!NILP (XCDR (clause)))
	    val = Fprogn (XCDR (clause));
	  break;
	}
      args = XCDR (args);
    }

  return val;
}

DEFUN ("progn", Fprogn, Sprogn, 0, UNEVALLED, 0,
       doc: /* Eval BODY forms sequentially and return value of last one.
usage: (progn BODY...)  */)
  (Lisp_Object body)
{
  Lisp_Object CACHEABLE val = Qnil;

  while (CONSP (body))
    {
      Lisp_Object form = XCAR (body);
      body = XCDR (body);
      val = eval_form (form);
    }

  return val;
}

/* Evaluate BODY sequentially, discarding its value.  */

void
prog_ignore (Lisp_Object body)
{
  Fprogn (body);
}

DEFUN ("prog1", Fprog1, Sprog1, 1, UNEVALLED, 0,
       doc: /* Eval FIRST and BODY sequentially; return value from FIRST.
The value of FIRST is saved during the evaluation of the remaining args,
whose values are discarded.
usage: (prog1 FIRST BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object val = eval_form (XCAR (args));
  prog_ignore (XCDR (args));
  return val;
}

/* Avoid Fassq overhead.  */

static inline Lisp_Object
find_lexbind (struct thread_state *thr, Lisp_Object sym)
{
  CHECK_SYMBOL (sym);
  Lisp_Object lexbind = Qnil, tail = thr->lexical_environment;
  FOR_EACH_TAIL (tail)
    {
      if (CONSP (XCAR (tail)) && EQ (XCAR (XCAR (tail)), sym))
	{
	  lexbind = XCAR (tail);
	  break;
	}
    }
  return lexbind;
}

DEFUN ("setq", Fsetq, Ssetq, 0, UNEVALLED, 0,
       doc: /* Set each SYM to the value of its VAL.
The symbols SYM are variables; they are literal (not evaluated).
The values VAL are expressions; they are evaluated.
Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
The second VAL is not computed until after the first SYM is set, and so on;
each VAL can use the new value of variables set earlier in the `setq'.
The return value of the `setq' form is the value of the last VAL.
usage: (setq [SYM VAL]...)  */)
  (Lisp_Object args)
{
  Lisp_Object val = args, tail = args;
  for (EMACS_INT nargs = 0; CONSP (tail); nargs += 2)
    {
      Lisp_Object sym = XCAR (tail);
      tail = XCDR (tail);
      if (! CONSP (tail))
	xsignal2 (Qwrong_number_of_arguments, Qsetq, make_fixnum (nargs + 1));
      Lisp_Object arg = XCAR (tail);
      tail = XCDR (tail);
      val = eval_form (arg);
      Lisp_Object lexbind = SYMBOLP (sym)
	? find_lexbind (current_thread, sym)
	: Qnil;
      if (! NILP (lexbind))
	XSETCDR (lexbind, val); /* SYM lexically bound.  */
      else
	Fset (sym, val); /* SYM dynamically bound.  */
    }
  return val;
}

DEFUN ("quote", Fquote, Squote_, 1, UNEVALLED, 0,
       doc: /* Return the argument, without evaluating it.  `(quote x)' yields `x'.
Warning: `quote' does not construct its return value, but just returns
the value that was pre-constructed by the Lisp reader (see info node
`(elisp)Printed Representation').
This means that \\='(a . b) is not identical to (cons \\='a \\='b): the former
does not cons.  Quoting should be reserved for constants that will
never be modified by side-effects, unless you like self-modifying code.
See the common pitfall in info node `(elisp)Rearrangement' for an example
of unexpected results when a quoted object is modified.
usage: (quote ARG)  */)
  (Lisp_Object args)
{
  if (! NILP (XCDR (args)))
    xsignal2 (Qwrong_number_of_arguments, Qquote, Flength (args));
  return XCAR (args);
}

DEFUN ("function", Ffunction, Sfunction, 1, UNEVALLED, 0,
       doc: /* Like `quote', but potentially byte-compiles or macroexpands ARG.
usage: (function ARG)  */)
  (Lisp_Object args)
{
  Lisp_Object quoted = XCAR (args);
  if (! NILP (XCDR (args)))
    xsignal2 (Qwrong_number_of_arguments, Qfunction, Flength (args));

  if (! NILP (current_thread->lexical_environment)
      && CONSP (quoted)
      && EQ (XCAR (quoted), Qlambda))
    {
      /* A lambda expression within a lexical environment.  Return an
	 interpreted closure.  */
      Lisp_Object cdr = XCDR (quoted);
      Lisp_Object tmp = cdr;
      if (CONSP (tmp)
	  && (tmp = XCDR (tmp), CONSP (tmp))
	  && (tmp = XCAR (tmp), CONSP (tmp))
	  && (EQ (QCdocumentation, XCAR (tmp))))
	{
	  /* Build docstring from (:documentation <form>).  */
	  Lisp_Object docstring = eval_form (CAR (XCDR (tmp)));
	  if (SYMBOLP (docstring) && ! NILP (docstring))
	    /* OClosure docstrings are just their types, and thus symbols.  */
	    docstring = Fsymbol_name (docstring);
	  CHECK_STRING (docstring);
	  cdr = Fcons (XCAR (cdr), Fcons (docstring, XCDR (XCDR (cdr))));
	}
      return (! NILP (Vinternal_make_interpreted_closure_function)
	      && main_thread_p (current_thread))
	? call2 (Vinternal_make_interpreted_closure_function,
		 Fcons (Qlambda, cdr),
		 current_thread->lexical_environment)
	: Fcons (Qclosure, Fcons (current_thread->lexical_environment, cdr));
    }
  else
    /* Simply quote the argument.  */
    return quoted;
}


DEFUN ("defvaralias", Fdefvaralias, Sdefvaralias, 2, 3, 0,
       doc: /* Make NEW-ALIAS a variable alias for symbol BASE-VARIABLE.
Aliased variables always have the same value; setting one sets the other.
Third arg DOCSTRING, if non-nil, is documentation for NEW-ALIAS.  If it is
omitted or nil, NEW-ALIAS gets the documentation string of BASE-VARIABLE,
or of the variable at the end of the chain of aliases, if BASE-VARIABLE is
itself an alias.  If NEW-ALIAS is bound, and BASE-VARIABLE is not,
then the value of BASE-VARIABLE is set to that of NEW-ALIAS.
The return value is BASE-VARIABLE.

If the resulting chain of variable definitions would contain a loop,
signal a `cyclic-variable-indirection' error.  */)
  (Lisp_Object new_alias, Lisp_Object base_variable, Lisp_Object docstring)
{
  CHECK_SYMBOL (new_alias);
  CHECK_SYMBOL (base_variable);

  if (SYMBOL_CONSTANT_P (new_alias))
    /* Making it an alias effectively changes its value.  */
    error ("Cannot make a constant an alias: %s",
	   SDATA (SYMBOL_NAME (new_alias)));

  struct Lisp_Symbol *sym = XSYMBOL (new_alias);

  /* Ensure non-circularity.  */
  struct Lisp_Symbol *s = XSYMBOL (base_variable);
  for (;;)
    {
      if (s == sym)
	xsignal1 (Qcyclic_variable_indirection, base_variable);
      if (s->u.s.type != SYMBOL_VARALIAS)
	break;
      s = SYMBOL_ALIAS (s);
    }

  switch (sym->u.s.type)
    {
    case SYMBOL_KBOARD:
    case SYMBOL_PER_BUFFER:
    case SYMBOL_FORWARDED:
      error ("Cannot make a built-in variable an alias: %s",
	     SDATA (SYMBOL_NAME (new_alias)));
    case SYMBOL_LOCAL_SOMEWHERE:
      error ("Don't know how to make a buffer-local variable an alias: %s",
	     SDATA (SYMBOL_NAME (new_alias)));
    case SYMBOL_PLAINVAL:
    case SYMBOL_VARALIAS:
      break;
    default:
      emacs_abort ();
      break;
    }

  /* https://lists.gnu.org/r/emacs-devel/2008-04/msg00834.html
     If n_a is bound, but b_v is not, set the value of b_v to n_a,
     so that old-code that affects n_a before the aliasing is setup
     still works.  */
  if (NILP (Fboundp (base_variable)))
    set_internal (base_variable, find_symbol_value (XSYMBOL (new_alias), NULL),
		  Qnil, SET_INTERNAL_BIND);
  else if (!NILP (Fboundp (new_alias))
           && !EQ (find_symbol_value (XSYMBOL (new_alias), NULL),
                   find_symbol_value (XSYMBOL (base_variable), NULL)))
    call2 (intern ("display-warning"),
           list3 (Qdefvaralias, intern ("losing-value"), new_alias),
           CALLN (Fformat_message,
                  build_string
                  ("Overwriting value of `%s' by aliasing to `%s'"),
                  new_alias, base_variable));

  for (union specbinding *p = specpdl_ptr - 1; p > specpdl; --p)
    if (p->kind >= SPECPDL_LET
	&& EQ (new_alias, specpdl_symbol (p)))
      error ("Don't know how to make a let-bound variable an alias: %s",
	     SDATA (SYMBOL_NAME (new_alias)));

  if (sym->u.s.trapped_write == SYMBOL_TRAPPED_WRITE)
    notify_variable_watchers (new_alias, base_variable, Qdefvaralias, Qnil);

  sym->u.s.declared_special = true;
  XSYMBOL (base_variable)->u.s.declared_special = true;
  sym->u.s.type = SYMBOL_VARALIAS;
  SET_SYMBOL_ALIAS (sym, XSYMBOL (base_variable));
  sym->u.s.trapped_write = XSYMBOL (base_variable)->u.s.trapped_write;
  LOADHIST_ATTACH (new_alias);
  /* Even if docstring is nil: remove old docstring.  */
  Fput (new_alias, Qvariable_documentation, docstring);

  return base_variable;
}

static union specbinding *
default_toplevel_binding (Lisp_Object symbol)
{
  union specbinding *binding = NULL;
  for (union specbinding *pdl = specpdl_ptr - 1; pdl > specpdl; --pdl)
    {
      switch (pdl->kind)
	{
	case SPECPDL_LET_BLD:
	case SPECPDL_LET:
	  if (EQ (specpdl_symbol (pdl), symbol))
	    binding = pdl;
	  break;
	default:
	  break;
	}
    }
  return binding;
}

DEFUN ("default-toplevel-value", Fdefault_toplevel_value, Sdefault_toplevel_value, 1, 1, 0,
       doc: /* Return SYMBOL's toplevel default value.
"Toplevel" means outside of any let binding.  */)
  (Lisp_Object symbol)
{
  union specbinding *binding = default_toplevel_binding (symbol);
  Lisp_Object value
    = binding ? specpdl_value (binding) : Fdefault_value (symbol);
  if (!EQ (value, Qunbound))
    return value;
  xsignal1 (Qvoid_variable, symbol);
}

DEFUN ("set-default-toplevel-value", Fset_default_toplevel_value,
       Sset_default_toplevel_value, 2, 2, 0,
       doc: /* Set SYMBOL's toplevel default value to VALUE.
"Toplevel" means outside of any let binding.  */)
     (Lisp_Object symbol, Lisp_Object value)
{
  union specbinding *binding = default_toplevel_binding (symbol);
  if (binding)
    binding->let.value = value;
  else
    Fset_default (symbol, value);
  return Qnil;
}

DEFUN ("internal--define-uninitialized-variable",
       Finternal__define_uninitialized_variable,
       Sinternal__define_uninitialized_variable, 1, 2, 0,
       doc: /* Define SYMBOL as a variable, with DOC as its docstring.
This is like `defvar' and `defconst' but without affecting the variable's
value.  */)
  (Lisp_Object symbol, Lisp_Object doc)
{
  XSYMBOL (symbol)->u.s.declared_special = true;
  if (! NILP (doc))
    {
      if (! NILP (Vloadup_pure_table))
	doc = Fpurecopy (doc);
      Fput (symbol, Qvariable_documentation, doc);
    }
  LOADHIST_ATTACH (symbol);
  return Qnil;
}

static Lisp_Object
defvar (Lisp_Object sym, Lisp_Object initvalue, Lisp_Object docstring, bool eval)
{
  CHECK_SYMBOL (sym);
  Lisp_Object tem = Fdefault_boundp (sym);

  /* Do it before evaluating the initial value, for self-references.  */
  Finternal__define_uninitialized_variable (sym, docstring);

  if (NILP (tem))
    Fset_default (sym, eval ? eval_form (initvalue) : initvalue);
  else
    {
      /* Check if there is really a global binding rather than just a let
	 binding that shadows the global unboundness of the var.  */
      union specbinding *binding = default_toplevel_binding (sym);
      if (binding && EQ (specpdl_value (binding), Qunbound))
	binding->let.value = eval ? eval_form (initvalue) : initvalue;
    }
  return sym;
}

DEFUN ("defvar", Fdefvar, Sdefvar, 1, UNEVALLED, 0,
       doc: /* Define SYMBOL as a variable, and return SYMBOL.
You are not required to define a variable in order to use it, but
defining it lets you supply an initial value and documentation, which
can be referred to by the Emacs help facilities and other programming
tools.

If SYMBOL's value is void and the optional argument INITVALUE is
provided, INITVALUE is evaluated and the result used to set SYMBOL's
value.  If SYMBOL is buffer-local, its default value is what is set;
buffer-local values are not affected.  If INITVALUE is missing,
SYMBOL's value is not set.

If INITVALUE is provided, the `defvar' form also declares the variable
as \"special\", so that it is always dynamically bound even if
`lexical-binding' is t.  If INITVALUE is missing, the form marks the
variable \"special\" locally (i.e., within the current
lexical scope, or the current file, if the form is at top-level),
and does nothing if `lexical-binding' is nil.

If SYMBOL is let-bound, then this form does not affect the local let
binding but the toplevel default binding instead, like
`set-toplevel-default-binding`.
(`defcustom' behaves similarly in this respect.)

The optional argument DOCSTRING is a documentation string for the
variable.

To define a user option, use `defcustom' instead of `defvar'.

To define a buffer-local variable, use `defvar-local'.
usage: (defvar SYMBOL &optional INITVALUE DOCSTRING)  */)
  (Lisp_Object args)
{
  Lisp_Object sym, tail;

  sym = XCAR (args);
  tail = XCDR (args);

  CHECK_SYMBOL (sym);

  if (NILP (tail)) /* a pithy (defvar FOO) */
    {
      if (! NILP (current_thread->lexical_environment)
	  && ! XSYMBOL (sym)->u.s.declared_special)
	/* Make a special variable for duration of environment.  */
	current_thread->lexical_environment = Fcons (sym, current_thread->lexical_environment);
      /* Otherwise (defvar FOO) is a no-op.  */
    }
  else
    {
      if (! NILP (XCDR (tail)) && ! NILP (XCDR (XCDR (tail))))
	error ("Too many arguments");
      Lisp_Object exp = XCAR (tail);
      tail = XCDR (tail);
      return defvar (sym, exp, CAR (tail), true);
    }
  return sym;
}

DEFUN ("defvar-1", Fdefvar_1, Sdefvar_1, 2, 3, 0,
       doc: /* Like `defvar' but as a function.
More specifically behaves like (defvar SYM 'INITVALUE DOCSTRING).  */)
  (Lisp_Object sym, Lisp_Object initvalue, Lisp_Object docstring)
{
  return defvar (sym, initvalue, docstring, false);
}

DEFUN ("defconst", Fdefconst, Sdefconst, 2, UNEVALLED, 0,
       doc: /* Define SYMBOL as a constant variable.
This declares that neither programs nor users should ever change the
value.  This constancy is not actually enforced by Emacs Lisp, but
SYMBOL is marked as a special variable so that it is never lexically
bound.

The `defconst' form always sets the value of SYMBOL to the result of
evalling INITVALUE.  If SYMBOL is buffer-local, its default value is
what is set; buffer-local values are not affected.  If SYMBOL has a
local binding, then this form sets the local binding's value.
However, you should normally not make local bindings for variables
defined with this form.

The optional DOCSTRING specifies the variable's documentation string.
usage: (defconst SYMBOL INITVALUE [DOCSTRING])  */)
  (Lisp_Object args)
{
  Lisp_Object sym, tem;

  sym = XCAR (args);
  CHECK_SYMBOL (sym);
  Lisp_Object docstring = Qnil;
  if (! NILP (XCDR (XCDR (args))))
    {
      if (!NILP (XCDR (XCDR (XCDR (args)))))
	error ("Too many arguments");
      docstring = XCAR (XCDR (XCDR (args)));
    }
  tem = eval_form (XCAR (XCDR (args)));
  return Fdefconst_1 (sym, tem, docstring);
}

DEFUN ("defconst-1", Fdefconst_1, Sdefconst_1, 2, 3, 0,
       doc: /* Like `defconst' but as a function.
More specifically, behaves like (defconst SYM 'INITVALUE DOCSTRING).  */)
  (Lisp_Object sym, Lisp_Object initvalue, Lisp_Object docstring)
{
  CHECK_SYMBOL (sym);
  Lisp_Object tem = initvalue;
  Finternal__define_uninitialized_variable (sym, docstring);
  if (! NILP (Vloadup_pure_table))
    tem = Fpurecopy (tem);
  Fset_default (sym, tem);      /* FIXME: set-default-toplevel-value? */
  Fput (sym, Qrisky_local_variable, Qt); /* FIXME: Why?  */
  return sym;
}

/* Make SYMBOL lexically scoped.  */
DEFUN ("internal-make-var-non-special", Fmake_var_non_special,
       Smake_var_non_special, 1, 1, 0,
       doc: /* Internal function.  */)
     (Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  XSYMBOL (symbol)->u.s.declared_special = false;
  return Qnil;
}

static inline Lisp_Object
thread_symbol (Lisp_Object symbol)
{
#ifdef HAVE_GCC_TLS
  if (! NILP (current_thread->obarray))
    {
      Lisp_Object name = SYMBOL_NAME (symbol);
      struct Lisp_Symbol *xsymbol = XSYMBOL (symbol);
      eassert (! main_thread_p (current_thread));
      Lisp_Object thread_symbol = Fintern (name, current_thread->obarray);
      if (! EQ (symbol, thread_symbol))
	{
	  symbol = thread_symbol;
	  XSYMBOL (symbol)->u.s.type = xsymbol->u.s.type;
	  XSYMBOL (symbol)->u.s.trapped_write = xsymbol->u.s.trapped_write;
	  XSYMBOL (symbol)->u.s.declared_special = xsymbol->u.s.declared_special;
	  XSYMBOL (symbol)->u.s.val = xsymbol->u.s.val;
	  XSYMBOL (symbol)->u.s.function = xsymbol->u.s.function;
	  XSYMBOL (symbol)->u.s.plist = xsymbol->u.s.plist;
	  XSYMBOL (symbol)->u.s.buffer_local_only = xsymbol->u.s.buffer_local_only;
	  XSYMBOL (symbol)->u.s.buffer_local_default = xsymbol->u.s.buffer_local_default;
	  XSYMBOL (symbol)->u.s.c_variable = xsymbol->u.s.c_variable;
	  XSYMBOL (symbol)->u.s.buffer_local_buffer = xsymbol->u.s.buffer_local_buffer;
	}
    }
#endif
  return symbol;
}

static void
let_bind (Lisp_Object prevailing_env, Lisp_Object var, Lisp_Object val, bool *q_pushed)
{
  bool q_lexical_binding = ! NILP (prevailing_env); /* flukey indicator */
  if (q_lexical_binding
      && SYMBOLP (var)
      && ! XSYMBOL (var)->u.s.declared_special
      && NILP (Fmemq (var, prevailing_env)))
    {
      /* Lexically bind VAR.  */
      if (! *q_pushed)
	{
	  /* Just push the env previous to the first lexical let
	     binding.  Nested envs produced by subsequent bindings in
	     a let* would never be reverted to.  */
	  *q_pushed = true;
	  record_lexical_environment ();
	}
      current_thread->lexical_environment
	= Fcons (Fcons (var, val), current_thread->lexical_environment);
    }
  else
    {
      /* Dynamically bind VAR.  */
      specbind (var, val);
      /* If lexical binding was off, it should still be off.  */
      eassert (q_lexical_binding || NILP (current_thread->lexical_environment));
    }
}

static Lisp_Object
eval_let (Lisp_Object args, bool nested_envs)
{
  specpdl_ref count = SPECPDL_INDEX ();
  Lisp_Object tail = XCAR (args), post_eval = Qnil;
  bool q_pushed = false; /* whether specpdl saved the old env */

  CHECK_LIST (tail);
  FOR_EACH_TAIL (tail)
    {
      Lisp_Object var, val, bind = XCAR (tail);
      if (SYMBOLP (bind))
	{
	  var = bind;
	  val = Qnil;
	}
      else
	{
	  var = CAR (bind);
	  if (! NILP (CDR (XCDR (bind))))
	    signal_error ("`let' bindings can have only one value-form", bind);
	  val = eval_form (CAR (XCDR (bind)));
	}
      if (nested_envs)
	let_bind (current_thread->lexical_environment, var, val, &q_pushed);
      else
	post_eval = Fcons (Fcons (var, val), post_eval);
    }
  CHECK_LIST_END (tail, XCAR (args));

  /* For parallel let (sans star), the resultant environment must
     incorporate any side effects from evaluating the bindings.  Thus,
     the two passes.  */
  Lisp_Object prevailing_env = current_thread->lexical_environment;
  post_eval = Fnreverse (post_eval);
  FOR_EACH_TAIL (post_eval) /* second pass */
    {
      Lisp_Object bind = XCAR (post_eval),
	var = XCAR (bind),
	val = XCDR (bind);
      eassert (! nested_envs);
      let_bind (prevailing_env, var, val, &q_pushed);
    }
  return unbind_to (count, Fprogn (XCDR (args)));
}

DEFUN ("let*", FletX, SletX, 1, UNEVALLED, 0,
       doc: /* Bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
Each VALUEFORM can refer to the symbols already bound by this VARLIST.
usage: (let* VARLIST BODY...)  */)
  (Lisp_Object args)
{
  return eval_let (args, true);
}

DEFUN ("let", Flet, Slet, 1, UNEVALLED, 0,
       doc: /* Bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
All the VALUEFORMs are evalled before any symbols are bound.
usage: (let VARLIST BODY...)  */)
  (Lisp_Object args)
{
  return eval_let (args, false);
}

DEFUN ("while", Fwhile, Swhile, 1, UNEVALLED, 0,
       doc: /* If TEST yields non-nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns nil.

The value of a `while' form is always nil.

usage: (while TEST BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object test, body;

  test = XCAR (args);
  body = XCDR (args);
  while (!NILP (eval_form (test)))
    {
      maybe_quit ();
      prog_ignore (body);
    }

  return Qnil;
}

static void
restore_gl_state (void *state)
{
  gl_state = *(struct gl_state_s *)state;
}

static void
with_delayed_message_display (struct atimer *timer)
{
  specpdl_ref count = SPECPDL_INDEX ();
  struct gl_state_s restore_state = gl_state;
  record_unwind_protect_ptr (restore_gl_state, &restore_state);
  message3 (build_string (timer->client_data));
  unbind_to (count, Qnil);
}

static void
with_delayed_message_cancel (void *timer)
{
  xfree (((struct atimer *) timer)->client_data);
  cancel_atimer (timer);
}

DEFUN ("funcall-with-delayed-message",
       Ffuncall_with_delayed_message, Sfuncall_with_delayed_message,
       3, 3, 0,
       doc: /* Like `funcall', but display MESSAGE if FUNCTION takes longer than TIMEOUT.
TIMEOUT is a number of seconds, and can be an integer or a floating
point number.

If FUNCTION takes less time to execute than TIMEOUT seconds, MESSAGE
is not displayed.  */)
  (Lisp_Object timeout, Lisp_Object message, Lisp_Object function)
{
  specpdl_ref count = SPECPDL_INDEX ();

  CHECK_NUMBER (timeout);
  CHECK_STRING (message);

  /* Set up the atimer.  */
  struct timespec interval = dtotimespec (XFLOATINT (timeout));
  struct atimer *timer = start_atimer (ATIMER_RELATIVE, interval,
				       with_delayed_message_display,
				       xstrdup (SSDATA (message)));
  record_unwind_protect_ptr (with_delayed_message_cancel, timer);

  Lisp_Object result = CALLN (Ffuncall, function);

  return unbind_to (count, result);
}

DEFUN ("macroexpand", Fmacroexpand, Smacroexpand, 1, 2, 0,
       doc: /* Return result of expanding macros at top level of FORM.
If FORM is not a macro call, it is returned unchanged.
Otherwise, the macro is expanded and the expansion is considered
in place of FORM.  When a non-macro-call results, it is returned.

The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation.  */)
  (Lisp_Object form, Lisp_Object environment)
{
  /* With cleanups from Hallvard Furuseth.  */
  register Lisp_Object expander, sym, def, tem;

  while (1)
    {
      /* Come back here each time we expand a macro call,
	 in case it expands into another macro call.  */
      if (!CONSP (form))
	break;
      /* Set SYM, give DEF and TEM right values in case SYM is not a symbol. */
      def = sym = XCAR (form);
      tem = Qnil;
      /* Trace symbols aliases to other symbols
	 until we get a symbol that is not an alias.  */
      while (SYMBOLP (def))
	{
	  maybe_quit ();
	  sym = def;
	  tem = Fassq (sym, environment);
	  if (NILP (tem))
	    {
	      def = XSYMBOL (sym)->u.s.function;
	      if (!NILP (def))
		continue;
	    }
	  break;
	}
      /* Right now TEM is the result from SYM in ENVIRONMENT,
	 and if TEM is nil then DEF is SYM's function definition.  */
      if (NILP (tem))
	{
	  /* SYM is not mentioned in ENVIRONMENT.
	     Look at its function definition.  */
	  def = Fautoload_do_load (def, sym, Qmacro);
	  if (!CONSP (def))
	    /* Not defined or definition not suitable.  */
	    break;
	  if (!EQ (XCAR (def), Qmacro))
	    break;
	  else expander = XCDR (def);
	}
      else
	{
	  expander = XCDR (tem);
	  if (NILP (expander))
	    break;
	}
      {
	Lisp_Object newform = apply1 (expander, XCDR (form));
	if (EQ (form, newform))
	  break;
	else
	  form = newform;
      }
    }
  return form;
}

DEFUN ("catch", Fcatch, Scatch, 1, UNEVALLED, 0,
       doc: /* Eval BODY allowing nonlocal exits using `throw'.
TAG is evalled to get the tag to use; it must not be nil.

Then the BODY is executed.
Within BODY, a call to `throw' with the same TAG exits BODY and this `catch'.
If no throw happens, `catch' returns the value of the last BODY form.
If a throw happens, it specifies the value to return from `catch'.
usage: (catch TAG BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object tag = eval_form (XCAR (args));
  return internal_catch (tag, Fprogn, XCDR (args));
}

/* Install catch on TAG, then call FUNC on argument ARG.  */

Lisp_Object
internal_catch (Lisp_Object tag, Lisp_Object (*func) (Lisp_Object),
		Lisp_Object arg)
{
  struct handler *c = push_exception (tag, CATCHER);
#ifdef ENABLE_CHECKING
  size_t ocount = exception_stack_count (current_thread);
  Lisp_Object owhat = c->what;
#else
  size_t ocount = 0;
  Lisp_Object owhat;
#endif
  if (sys_setjmp (c->jmp))
    {
      Lisp_Object val = current_thread->exception_stack_top->val;
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return val;
    }
  else
    {
      Lisp_Object val = func (arg);
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return val;
    }
}

/* Unwind exception stack to CATCH, then jmp to CATCH returning VALUE.

   This is the guts of Fthrow and Fsignal; they differ only in TYPE.
*/

static AVOID
unwind_to_catch (struct handler *catch, enum nonlocal_exit type,
                 Lisp_Object value)
{
  catch->nonlocal_exit = type;
  catch->val = value;

  /* Restore C variables.  */
  set_poll_suppress_count (catch->poll_suppress_count);
  unblock_input_to (catch->interrupt_input_blocked);

#ifdef HAVE_X_WINDOWS
  x_unwind_errors_to (catch->x_error_handler_depth);
#endif

  do
    {
      /* Unwind to next recorded frame... */
      unbind_to (current_thread->exception_stack_top->pdlcount, Qnil);
      if (current_thread->exception_stack_top == catch)
	break;
      /* ... then restore corresponding handlers. */
      exception_stack_pop (current_thread);
    }
  while (true);

  lisp_eval_depth = catch->f_lisp_eval_depth;
  set_act_rec (current_thread, catch->act_rec);
  sys_longjmp (catch->jmp, 1);
}

DEFUN ("throw", Fthrow, Sthrow, 2, 2, 0,
       doc: /* Throw to the catch for TAG and return VALUE from it.
Both TAG and VALUE are evalled.  */
       attributes: noreturn)
  (register Lisp_Object tag, Lisp_Object value)
{
  if (! NILP (tag))
    for (size_t count = exception_stack_count (current_thread);
	 count > 0;
	 --count)
      {
	struct handler *c = current_thread->exception_stack_bottom + count - 1;
	if (c->type == OMNIBUS)
          unwind_to_catch (c, NONLOCAL_EXIT_THROW, Fcons (tag, value));
        if (c->type == CATCHER && EQ (c->what, tag))
	  unwind_to_catch (c, NONLOCAL_EXIT_THROW, value);
      }
  xsignal2 (Qno_catch, tag, value);
}

DEFUN ("unwind-protect", Funwind_protect, Sunwind_protect, 1, UNEVALLED, 0,
       doc: /* Do BODYFORM, protecting with UNWINDFORMS.
If BODYFORM completes normally, its value is returned
after executing the UNWINDFORMS.
If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway.
usage: (unwind-protect BODYFORM UNWINDFORMS...)  */)
  (Lisp_Object args)
{
  Lisp_Object val;
  specpdl_ref count = SPECPDL_INDEX ();

  record_unwind_protect (prog_ignore, XCDR (args));
  val = eval_form (XCAR (args));
  return unbind_to (count, val);
}

DEFUN ("condition-case", Fcondition_case, Scondition_case, 2, UNEVALLED, 0,
       doc: /* Regain control when an error is signaled.
Executes BODYFORM and returns its value if no error happens.
Each element of HANDLERS looks like (CONDITION-NAME BODY...)
or (:success BODY...), where the BODY is made of Lisp expressions.

A handler is applicable to an error if CONDITION-NAME is one of the
error's condition names.  Handlers may also apply when non-error
symbols are signaled (e.g., `quit').  A CONDITION-NAME of t applies to
any symbol, including non-error symbols.  If multiple handlers are
applicable, only the first one runs.

The car of a handler may be a list of condition names instead of a
single condition name; then it handles all of them.  If the special
condition name `debug' is present in this list, it allows another
condition in the list to run the debugger if `debug-on-error' and the
other usual mechanisms say it should (otherwise, `condition-case'
suppresses the debugger).

When a handler handles an error, control returns to the `condition-case'
and it executes the handler's BODY...
with VAR bound to (ERROR-SYMBOL . SIGNAL-DATA) from the error.
\(If VAR is nil, the handler can't access that information.)
Then the value of the last BODY form is returned from the `condition-case'
expression.

The special handler (:success BODY...) is invoked if BODYFORM terminated
without signaling an error.  BODY is then evaluated with VAR bound to
the value returned by BODYFORM.

See also the function `signal' for more info.
usage: (condition-case VAR BODYFORM &rest HANDLERS)  */)
  (Lisp_Object args)
{
  Lisp_Object var = XCAR (args);
  Lisp_Object bodyform = XCAR (XCDR (args));
  Lisp_Object clauses = XCDR (XCDR (args));

  return internal_lisp_condition_case (var, bodyform, clauses);
}

Lisp_Object
internal_lisp_condition_case (Lisp_Object var, Lisp_Object bodyform,
			      Lisp_Object clauses)
{
  Lisp_Object CACHEABLE triggered_clause = Qnil,
    success_clause = Qnil, result = Qnil, rev = Qnil;

  CHECK_SYMBOL (var);

  for (Lisp_Object tail = clauses; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object clause = XCAR (tail);
      if (! (NILP (clause)
	     || (CONSP (clause)
		 && (SYMBOLP (XCAR (clause))
		     || CONSP (XCAR (clause))))))
	error ("Invalid condition handler: %s",
	       SDATA (Fprin1_to_string (clause, Qt, Qnil)));
      if (CONSP (clause) && EQ (XCAR (clause), QCsuccess))
	success_clause = clause;
      else
	/* Reverse since push_exception() reverses again.  */
	rev = Fcons (clause, rev);
    }

  const size_t before_count = exception_stack_count (current_thread);
  for (Lisp_Object tail = rev; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object clause = XCAR (tail);
      Lisp_Object errsyms = CONSP (clause) ? XCAR (clause) : Qnil;
      struct handler *c = push_exception (CONSP (errsyms) ? errsyms : list1 (errsyms),
					  CONDITION_CASE);
      if (sys_setjmp (c->jmp))
	{
	  /* eval_form errored.  The index of the triggered clause in
	     REV is however many stack entries back to
	     BEFORE_COUNT (minus one for RESULT).  */
	  result = current_thread->exception_stack_top->val;
	  Lisp_Object tail = rev;
	  for (size_t count = exception_stack_count (current_thread) - 1;
	       count > before_count;
	       --count)
	    tail = XCDR (tail);
	  triggered_clause = XCAR (tail);
	  goto done;
	}
    }

  result = eval_form (bodyform); /* an err jmps to sys_setjmp above */
 done: ;
  for (size_t count = exception_stack_count (current_thread);
       count > before_count;
       --count)
    exception_stack_pop (current_thread);
  eassert (before_count == exception_stack_count (current_thread));
  Lisp_Object clause = NILP (triggered_clause) ? success_clause : triggered_clause;
  if (! NILP (clause))
    {
      if (NILP (var))
	result = Fprogn (XCDR (clause));
      else
	{
	  specpdl_ref count = SPECPDL_INDEX ();
	  if (! NILP (current_thread->lexical_environment))
	    {
	      record_lexical_environment ();
	      current_thread->lexical_environment
		= Fcons (Fcons (var, result),
			 current_thread->lexical_environment);
	    }
	  else
	    specbind (var, result);
	  result = unbind_to (count, Fprogn (XCDR (clause)));
	}
    }
  return result;
}

/* Call no-argument BFUN and handle any error CONDITIONS by calling
   HFUN which takes one argument (SIGNALNAME . DATA).  CONDITIONS is
   list of error symbols with special interpretations for Qt the catch-all
   condition, and Qerror the catch-all condition invoking the
   debugger.  */

Lisp_Object
internal_condition_case (Lisp_Object (*bfun) (void),
			 Lisp_Object conditions,
			 Lisp_Object (*hfun) (Lisp_Object))
{
  struct handler *c = push_exception (conditions, CONDITION_CASE);
#ifdef ENABLE_CHECKING
  size_t ocount = exception_stack_count (current_thread);
  Lisp_Object owhat = c->what;
#else
  size_t ocount = 0;
  Lisp_Object owhat;
#endif
  if (sys_setjmp (c->jmp))
    {
      Lisp_Object val = current_thread->exception_stack_top->val;
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return hfun (val);
    }
  else
    {
      Lisp_Object val = bfun ();
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return val;
    }
}

/* Like internal_condition_case but call BFUN with ARG as its argument.  */

Lisp_Object
internal_condition_case_1 (Lisp_Object (*bfun) (Lisp_Object), Lisp_Object arg,
			   Lisp_Object handlers,
			   Lisp_Object (*hfun) (Lisp_Object))
{
  struct handler *c = push_exception (handlers, CONDITION_CASE);
#ifdef ENABLE_CHECKING
  size_t ocount = exception_stack_count (current_thread);
  Lisp_Object owhat = c->what;
#else
  size_t ocount = 0;
  Lisp_Object owhat;
#endif
  if (sys_setjmp (c->jmp))
    {
      Lisp_Object val = current_thread->exception_stack_top->val;
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return hfun (val);
    }
  else
    {
      Lisp_Object val = bfun (arg);
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return val;
    }
}

/* Like internal_condition_case_1 but call BFUN with ARG1 and ARG2 as
   its arguments.  */

Lisp_Object
internal_condition_case_2 (Lisp_Object (*bfun) (Lisp_Object, Lisp_Object),
			   Lisp_Object arg1,
			   Lisp_Object arg2,
			   Lisp_Object handlers,
			   Lisp_Object (*hfun) (Lisp_Object))
{
  struct handler *c = push_exception (handlers, CONDITION_CASE);
#ifdef ENABLE_CHECKING
  size_t ocount = exception_stack_count (current_thread);
  Lisp_Object owhat = c->what;
#else
  size_t ocount = 0;
  Lisp_Object owhat;
#endif
  if (sys_setjmp (c->jmp))
    {
      Lisp_Object val = current_thread->exception_stack_top->val;
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return hfun (val);
    }
  else
    {
      Lisp_Object val = bfun (arg1, arg2);
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return val;
    }
}

/* Like internal_condition_case but call BFUN with NARGS as first,
   and ARGS as second argument.  */

Lisp_Object
internal_condition_case_n (Lisp_Object (*bfun) (ptrdiff_t, Lisp_Object *),
			   ptrdiff_t nargs,
			   Lisp_Object *args,
			   Lisp_Object handlers,
			   Lisp_Object (*hfun) (Lisp_Object err,
						ptrdiff_t nargs,
						Lisp_Object *args))
{
  struct handler *c = push_exception (handlers, CONDITION_CASE);
#ifdef ENABLE_CHECKING
  size_t ocount = exception_stack_count (current_thread);
  Lisp_Object owhat = c->what;
#else
  size_t ocount = 0;
  Lisp_Object owhat;
#endif
  if (sys_setjmp (c->jmp))
    {
      Lisp_Object val = current_thread->exception_stack_top->val;
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return hfun (val, nargs, args);
    }
  else
    {
      Lisp_Object val = bfun (nargs, args);
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return val;
    }
}

static Lisp_Object Qcatch_all_memory_full;

/* Like a combination of internal_condition_case_1 and internal_catch.
   Catches all signals and throws.  Never exits nonlocally; returns
   Qcatch_all_memory_full if no handler could be allocated.  */

Lisp_Object
internal_catch_all (Lisp_Object (*function) (void *), void *argument,
                    Lisp_Object (*handler) (enum nonlocal_exit, Lisp_Object))
{
  struct handler *c = push_exception (Qt, OMNIBUS);
  if (! c)
    return Qcatch_all_memory_full;
#ifdef ENABLE_CHECKING
  size_t ocount = exception_stack_count (current_thread);
  Lisp_Object owhat = c->what;
#else
  size_t ocount = 0;
  Lisp_Object owhat;
#endif
  if (sys_setjmp (c->jmp))
    {
      enum nonlocal_exit type = current_thread->exception_stack_top->nonlocal_exit;
      Lisp_Object val = current_thread->exception_stack_top->val;
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return handler (type, val);
    }
  else
    {
      Lisp_Object val = function (argument);
      eassert (ocount == exception_stack_count (current_thread));
      eassert (EQ (owhat, current_thread->exception_stack_top->what));
      exception_stack_pop (current_thread);
      return val;
    }
}

/* WHAT is ridiculously overloaded.  It could be,

   1. a list of catch tags
   2. a list of condition-case error symbols
   3. Qt, for catching all exceptions
   4. Qerror, as Qt but invoke debugger.
*/

struct handler *
push_exception (Lisp_Object what, enum exception_type type)
{
  struct handler *top = exception_stack_push (current_thread);
  top->type = type;
  top->what = what;
  top->val = Qnil;
  top->f_lisp_eval_depth = lisp_eval_depth;
  top->pdlcount = SPECPDL_INDEX ();
  top->act_rec = get_act_rec (current_thread);
  top->poll_suppress_count = poll_suppress_count;
  top->interrupt_input_blocked = interrupt_input_blocked;
#ifdef HAVE_X_WINDOWS
  top->x_error_handler_depth = x_error_message_count;
#endif
  return top;
}

static Lisp_Object signal_or_quit (Lisp_Object, Lisp_Object);
static bool maybe_call_debugger (Lisp_Object conditions, Lisp_Object sig,
				 Lisp_Object data);

DEFUN ("signal", Fsignal, Ssignal, 2, 2, 0,
       doc: /* Signal an error.  Args are ERROR-SYMBOL and associated DATA.
This function does not return.

When `noninteractive' is non-nil (in particular, in batch mode), an
unhandled error calls `kill-emacs', which terminates the Emacs
session with a non-zero exit code.

An error symbol is a symbol with an `error-conditions' property
that is a list of condition names.  The symbol should be non-nil.
A handler for any of those names will get to handle this signal.
The symbol `error' should normally be one of them.

DATA should be a list.  Its elements are printed as part of the error message.
See Info anchor `(elisp)Definition of signal' for some details on how this
error message is constructed.
If the signal is handled, DATA is made available to the handler.
See also the function `condition-case'.  */
       attributes: noreturn)
  (Lisp_Object error_symbol, Lisp_Object data)
{
  signal_or_quit (error_symbol, data);
  eassume (false);
}

/* Keyboard quit request.  */

Lisp_Object
quit (void)
{
  return signal_or_quit (Qquit, Qnil);
}

static Lisp_Object
signal_or_quit (Lisp_Object error_symbol, Lisp_Object data)
{
  /* Edebug hook if specpdl not overflowed.  */
  if (! NILP (Vsignal_hook_function)
      && specpdl_ptr < specpdl_end)
    call2 (Vsignal_hook_function, error_symbol, data);

  /* Skip frames for `signal' itself and Qerror.  */
  {
    union specbinding *pdl
      = backtrace_next (current_thread, backtrace_top (current_thread));
    if (backtrace_valid_p (current_thread, pdl)
	&& EQ (backtrace_function (pdl), Qerror))
      pdl = backtrace_next (current_thread, pdl);
  }

  struct handler *handler = NULL;
  for (size_t count = exception_stack_count (current_thread);
       count > 0 && handler == NULL;
       --count)
    {
      struct handler *h = current_thread->exception_stack_bottom + count - 1;
      if (h->type == OMNIBUS)
	handler = h;
      else if (h->type == CONDITION_CASE)
	{
	  if (EQ (h->what, Qt) || EQ (h->what, Qerror))
	    handler = h;
	  else if (CONSP (h->what))
	    {
	      Lisp_Object errsyms = Fget (error_symbol, Qerror_conditions);
	      Lisp_Object tail = h->what;
	      FOR_EACH_TAIL_SAFE (tail)
		{
		  Lisp_Object errsym = XCAR (tail);
		  if (! NILP (Fmemq (errsym, errsyms)) || EQ (errsym, Qt))
		    {
		      handler = h;
		      break;
		    }
		}
	    }
	}
    }

  if ((! NILP (Vdebug_on_signal)
       || ! handler
       /* Clause containing special 'debug symbol */
       || (CONSP (handler->what) && ! NILP (Fmemq (Qdebug, handler->what)))
       /* Special symbol invoking debugger */
       || EQ (handler->what, Qerror))
      && maybe_call_debugger (Fget (error_symbol, Qerror_conditions),
			      error_symbol, data))
    {
      if (EQ (error_symbol, Qquit))
	return Qnil;
    }
  else if ((! handler || EQ (handler->what, Qerror))
	   && noninteractive
	   && backtrace_on_error_noninteractive
	   && NILP (Vinhibit_debugger)
	   && ! NILP (Ffboundp (Qdebug_early)))
    {
      /* `debug-early' does not interfere with ERT or customized debuggers */
      specpdl_ref count = SPECPDL_INDEX ();
      specbind (Qdebugger, Qdebug_early);
      call_debugger (list2 (Qerror, Fcons (error_symbol, data)));
      unbind_to (count, Qnil);
    }

  if (handler)
    unwind_to_catch (handler, NONLOCAL_EXIT_SIGNAL, Fcons (error_symbol, data));

  Fthrow (Qtop_level, Qt);
  eassume (false);
}

/* Like xsignal, but takes 0, 1, 2, or 3 args instead of a list.  */

void
xsignal0 (Lisp_Object error_symbol)
{
  xsignal (error_symbol, Qnil);
}

void
xsignal1 (Lisp_Object error_symbol, Lisp_Object arg)
{
  xsignal (error_symbol, list1 (arg));
}

void
xsignal2 (Lisp_Object error_symbol, Lisp_Object arg1, Lisp_Object arg2)
{
  xsignal (error_symbol, list2 (arg1, arg2));
}

void
xsignal3 (Lisp_Object error_symbol, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  xsignal (error_symbol, list3 (arg1, arg2, arg3));
}

/* Signal `error' with message S, and additional arg ARG.
   If ARG is not a proper list, make it a one-element list.  */

void
signal_error (const char *s, Lisp_Object arg)
{
  if (NILP (Fproper_list_p (arg)))
    arg = list1 (arg);

  xsignal (Qerror, Fcons (build_string (s), arg));
}

void
define_error (Lisp_Object name, const char *message, Lisp_Object parent)
{
  eassert (SYMBOLP (name));
  eassert (SYMBOLP (parent));
  Lisp_Object parent_conditions = Fget (parent, Qerror_conditions);
  eassert (CONSP (parent_conditions));
  eassert (!NILP (Fmemq (parent, parent_conditions)));
  eassert (NILP (Fmemq (name, parent_conditions)));
  Fput (name, Qerror_conditions, pure_cons (name, parent_conditions));
  Fput (name, Qerror_message, build_pure_c_string (message));
}

/* Use this for arithmetic overflow, e.g., when an integer result is
   too large even for a bignum.  */
void
overflow_error (void)
{
  xsignal0 (Qoverflow_error);
}


/* Return true if LIST is a non-nil atom or
   a list containing one of CONDITIONS.  */

static bool
wants_debugger (Lisp_Object list, Lisp_Object conditions)
{
  if (NILP (list))
    return 0;
  if (! CONSP (list))
    return 1;

  while (CONSP (conditions))
    {
      Lisp_Object this, tail;
      this = XCAR (conditions);
      for (tail = list; CONSP (tail); tail = XCDR (tail))
	if (EQ (XCAR (tail), this))
	  return 1;
      conditions = XCDR (conditions);
    }
  return 0;
}

/* Return true if an error with condition-symbols CONDITIONS,
   and described by SIGNAL-DATA, should skip the debugger
   according to debugger-ignored-errors.  */

static bool
skip_debugger (Lisp_Object conditions, Lisp_Object data)
{
  Lisp_Object tail;
  bool first_string = 1;
  Lisp_Object error_message;

  error_message = Qnil;
  for (tail = Vdebug_ignored_errors; CONSP (tail); tail = XCDR (tail))
    {
      if (STRINGP (XCAR (tail)))
	{
	  if (first_string)
	    {
	      error_message = Ferror_message_string (data);
	      first_string = 0;
	    }

	  if (fast_string_match (XCAR (tail), error_message) >= 0)
	    return 1;
	}
      else
	{
	  Lisp_Object contail;

	  for (contail = conditions; CONSP (contail); contail = XCDR (contail))
	    if (EQ (XCAR (tail), XCAR (contail)))
	      return 1;
	}
    }

  return 0;
}

/* Say whether SIGNAL is a `quit' symbol (or inherits from it).  */
bool
signal_quit_p (Lisp_Object signal)
{
  Lisp_Object list;

  return EQ (signal, Qquit)
    || (!NILP (Fsymbolp (signal))
	&& CONSP (list = Fget (signal, Qerror_conditions))
	&& !NILP (Fmemq (Qquit, list)));
}

/* Call the debugger if calling it is currently enabled for CONDITIONS.  */
static bool
maybe_call_debugger (Lisp_Object conditions, Lisp_Object sig, Lisp_Object data)
{
  Lisp_Object combined_data = Fcons (sig, data);

  if (
      /* Don't try to run the debugger with interrupts blocked.
	 The editing loop would return anyway.  */
      ! input_blocked_p ()
      && NILP (Vinhibit_debugger)
      /* Does user want to enter debugger for this kind of error?  */
      && (signal_quit_p (sig)
	  ? debug_on_quit
	  : wants_debugger (Vdebug_on_error, conditions))
      && ! skip_debugger (conditions, combined_data)
      /* See commentary on definition of
         `internal-when-entered-debugger'.  */
      && when_entered_debugger < num_nonmacro_input_events)
    {
      call_debugger (list2 (Qerror, combined_data));
      return 1;
    }

  return 0;
}

/* Format and return a string; called like vprintf.  */
Lisp_Object
vformat_string (const char *m, va_list ap)
{
  char buf[4000];
  ptrdiff_t size = sizeof buf;
  ptrdiff_t size_max = STRING_BYTES_BOUND + 1;
  char *buffer = buf;
  ptrdiff_t used;
  Lisp_Object string;

  used = evxprintf (&buffer, &size, buf, size_max, m, ap);
  string = make_string (buffer, used);
  if (buffer != buf)
    xfree (buffer);

  return string;
}

/* Dump an error message; called like vprintf.  */
void
verror (const char *m, va_list ap)
{
  xsignal1 (Qerror, vformat_string (m, ap));
}


/* Dump an error message; called like printf.  */

void
error (const char *m, ...)
{
  va_list ap;
  va_start (ap, m);
  verror (m, ap);
}

DEFUN ("commandp", Fcommandp, Scommandp, 1, 2, 0,
       doc: /* Non-nil if FUNCTION makes provisions for interactive calling.
This means it contains a description for how to read arguments to give it.
The value is nil for an invalid function or a symbol with no function
definition.

Interactively callable functions include strings and vectors (treated
as keyboard macros), lambda-expressions that contain a top-level call
to `interactive', autoload definitions made by `autoload' with non-nil
fourth argument, and some of the built-in functions of Lisp.

Also, a symbol satisfies `commandp' if its function definition does so.

If the optional argument FOR-CALL-INTERACTIVELY is non-nil,
then strings and vectors are not accepted.  */)
  (Lisp_Object function, Lisp_Object for_call_interactively)
{
  register Lisp_Object fun;
  bool genfun = false; /* If true, we should consult `interactive-form'.  */

  fun = function;

  fun = indirect_function (fun);
  if (NILP (fun))
    return Qnil;

  /* Emacs primitives are interactive if their DEFUN specifies an
     interactive spec.  */
  if (SUBRP (fun))
    {
      if (XSUBR (fun)->intspec.string)
        return Qt;
    }
  /* Bytecode objects are interactive if they are long enough to
     have an element whose index is COMPILED_INTERACTIVE, which is
     where the interactive spec is stored.  */
  else if (COMPILEDP (fun))
    {
      if (PVSIZE (fun) > COMPILED_INTERACTIVE)
        return Qt;
      else if (PVSIZE (fun) > COMPILED_DOC_STRING)
        {
          Lisp_Object doc = AREF (fun, COMPILED_DOC_STRING);
          /* An invalid "docstring" is a sign that we have an OClosure.  */
          genfun = ! (NILP (doc) || VALID_DOCSTRING_P (doc));
        }
    }

#ifdef HAVE_MODULES
  /* Module functions are interactive if their `interactive_form'
     field is non-nil. */
  else if (MODULE_FUNCTIONP (fun))
    {
      if (!NILP (module_function_interactive_form (XMODULE_FUNCTION (fun))))
        return Qt;
    }
#endif

  /* Strings and vectors are keyboard macros.  */
  else if (STRINGP (fun) || VECTORP (fun))
    return (NILP (for_call_interactively) ? Qt : Qnil);

  /* Lists may represent commands.  */
  else if (!CONSP (fun))
    return Qnil;
  else
    {
      Lisp_Object funcar = XCAR (fun);
      if (EQ (funcar, Qautoload))
        {
          if (!NILP (CAR (CDR (CDR (XCDR (fun))))))
            return Qt;
        }
      else
        {
          Lisp_Object body = CDR_SAFE (XCDR (fun));
          if (EQ (funcar, Qclosure))
            body = CDR_SAFE (body);
          else if (!EQ (funcar, Qlambda))
	    return Qnil;
	  if (!NILP (Fassq (Qinteractive, body)))
	    return Qt;
	  else if (VALID_DOCSTRING_P (CAR_SAFE (body)))
            /* A "docstring" is a sign that we may have an OClosure.  */
	    genfun = true;
	}
    }

  /* By now, if it's not a function we already returned nil.  */

  /* Check an `interactive-form' property if present, analogous to the
     function-documentation property.  */
  fun = function;
  while (SYMBOLP (fun))
    {
      Lisp_Object tmp = Fget (fun, Qinteractive_form);
      if (!NILP (tmp))
	error ("Found an 'interactive-form' property!");
      fun = Fsymbol_function (fun);
    }

  /* If there's no immediate interactive form but it's an OClosure,
     then delegate to the generic-function in case it has
     a type-specific interactive-form.  */
  if (genfun)
    {
      Lisp_Object iform = call1 (Qinteractive_form, fun);
      return NILP (iform) ? Qnil : Qt;
    }
  else
    return Qnil;
}

DEFUN ("autoload", Fautoload, Sautoload, 2, 5, 0,
       doc: /* Define FUNCTION to autoload from FILE.
FUNCTION is a symbol; FILE is a file name string to pass to `load'.

Third arg DOCSTRING is documentation for the function.

Fourth arg INTERACTIVE if non-nil says function can be called
interactively.  If INTERACTIVE is a list, it is interpreted as a list
of modes the function is applicable for.

Fifth arg TYPE indicates the type of the object:
   nil or omitted says FUNCTION is a function,
   `keymap' says FUNCTION is really a keymap, and
   `macro' or t says FUNCTION is really a macro.

Third through fifth args give info about the real definition.
They default to nil.

If FUNCTION is already defined other than as an autoload,
this does nothing and returns nil.  */)
  (Lisp_Object function, Lisp_Object file, Lisp_Object docstring, Lisp_Object interactive, Lisp_Object type)
{
  CHECK_SYMBOL (function);
  CHECK_STRING (file);

  /* If function is defined and not as an autoload, don't override.  */
  if (!NILP (XSYMBOL (function)->u.s.function)
      && !AUTOLOADP (XSYMBOL (function)->u.s.function))
    return Qnil;

  if (! NILP (Vloadup_pure_table) && EQ (docstring, make_fixnum (0)))
    /* `read1' in lread.c has found the docstring starting with "\
       and assumed the docstring will be provided by Snarf-documentation, so it
       passed us 0 instead.  But that leads to accidental sharing in purecopy's
       hash-consing, so we use a (hopefully) unique integer instead.  */
    docstring = make_ufixnum (XHASH (function));
  return Fdefalias (function,
		    list5 (Qautoload, file, docstring, interactive, type),
		    Qnil);
}

static void
un_autoload (Lisp_Object oldqueue)
{
  /* Queue to unwind is current value of Vautoload_queue.
     oldqueue is the shadowed value to leave in Vautoload_queue.  */
  Lisp_Object queue = Vautoload_queue;
  Vautoload_queue = oldqueue;
  while (CONSP (queue))
    {
      Lisp_Object first = XCAR (queue);
      if (CONSP (first) && EQ (XCAR (first), make_fixnum (0)))
	Vfeatures = XCDR (first);
      else
	Ffset (first, CAR (CDR (Fget (first, Qfunction_history))));
      queue = XCDR (queue);
    }
}

Lisp_Object
load_with_autoload_queue
  (Lisp_Object file, Lisp_Object noerror, Lisp_Object nomessage,
   Lisp_Object nosuffix, Lisp_Object must_suffix)
{
  specpdl_ref count = SPECPDL_INDEX ();

  /* If autoloading gets an error (which includes the error of failing
     to define the function being called), we use Vautoload_queue
     to undo function definitions and `provide' calls made by
     the function.  We do this in the specific case of autoloading
     because autoloading is not an explicit request "load this file",
     but rather a request to "call this function".

     The value saved here is to be restored into Vautoload_queue.  */
  record_unwind_protect (un_autoload, Vautoload_queue);
  Vautoload_queue = Qt;
  Lisp_Object tem
    = save_match_data_load (file, noerror, nomessage, nosuffix, must_suffix);

  /* Once loading finishes, don't undo it.  */
  Vautoload_queue = Qt;
  unbind_to (count, Qnil);
  return tem;
}

/* Load an autoloaded function.
   FUNNAME is the symbol which is the function's name.
   FUNDEF is the autoload definition (a list).  */

DEFUN ("autoload-do-load", Fautoload_do_load, Sautoload_do_load, 1, 3, 0,
       doc: /* Load FUNDEF which should be an autoload.
If non-nil, FUNNAME should be the symbol whose function value is FUNDEF,
in which case the function returns the new autoloaded function value.
If equal to `macro', MACRO-ONLY specifies that FUNDEF should only be loaded if
it defines a macro.  */)
  (Lisp_Object fundef, Lisp_Object funname, Lisp_Object macro_only)
{
  if (!CONSP (fundef) || !EQ (Qautoload, XCAR (fundef)))
    return fundef;

  Lisp_Object kind = Fnth (make_fixnum (4), fundef);
  if (EQ (macro_only, Qmacro)
      && !(EQ (kind, Qt) || EQ (kind, Qmacro)))
    return fundef;

  /* This is to make sure that loadup.el gives a clear picture
     of what files are preloaded and when.  */
  if (will_dump_p () && !will_bootstrap_p ())
    {
      /* Avoid landing here recursively while outputting the
	 backtrace from the error.  */
      gflags.will_dump_ = false;
      error ("Attempt to autoload %s while preparing to dump",
	     SDATA (SYMBOL_NAME (funname)));
    }

  CHECK_SYMBOL (funname);

  /* If `macro_only' is set and fundef isn't a macro, assume this autoload to
     be a "best-effort" (e.g. to try and find a compiler macro),
     so don't signal an error if autoloading fails.  */
  Lisp_Object ignore_errors
    = (EQ (kind, Qt) || EQ (kind, Qmacro)) ? Qnil : macro_only;
  load_with_autoload_queue (CAR (CDR (fundef)), ignore_errors, Qt, Qnil, Qt);

  if (NILP (funname) || !NILP (ignore_errors))
    return Qnil;
  else
    {
      Lisp_Object fun = Findirect_function (funname, Qnil);

      if (!NILP (Fequal (fun, fundef)))
	error ("Autoloading file %s failed to define function %s",
	       SDATA (CAR (CAR (Vload_history))),
	       SDATA (SYMBOL_NAME (funname)));
      else
	return fun;
    }
}

static Lisp_Object list_of_t;  /* Never-modified constant containing (t).  */

DEFUN ("eval", Feval, Seval, 1, 2, 0,
       doc: /* Return evaluated FORM.
For historical consistency, evaluation defaults to dynamic scoping.
To apply lexical scoping, explicitly set LEXICAL to non-nil.  LEXICAL
can also specify the lexical environment to apply; please see the Info
node `(elisp)Eval' for its form.  */)
  (Lisp_Object form, Lisp_Object lexical)
{
  specpdl_ref count = SPECPDL_INDEX ();
  record_lexical_environment ();
  current_thread->lexical_environment = ! NILP (Flistp (lexical))
    ? lexical /* dynamic if LEXICAL is nil, bespoke otherwise */
    : list_of_t /* No bespoke environment but still lexical.  */
    ;
  return unbind_to (count, eval_form (form));
}

void
grow_specpdl_allocation (void)
{
  eassert (specpdl_ptr == specpdl_end);

  specpdl_ref count = SPECPDL_INDEX ();
  ptrdiff_t max_size = PTRDIFF_MAX - 1000;
  union specbinding *pdlvec = specpdl - 1;
  ptrdiff_t size = specpdl_end - specpdl;
  ptrdiff_t pdlvecsize = size + 1;
  eassert (max_size > size);
  pdlvec = xpalloc (pdlvec, &pdlvecsize, 1, max_size + 1, sizeof *specpdl);
  specpdl = pdlvec + 1;
  specpdl_end = specpdl + pdlvecsize - 1;
  specpdl_ptr = specpdl_ref_to_ptr (count);
}

static inline void
populate_evaluated_args (Lisp_Object args,
			 Lisp_Object *evaluated_args,
			 const ptrdiff_t capacity)
{
  for (ptrdiff_t i = 0; i < capacity; ++i)
    evaluated_args[i] = Qnil;

  ptrdiff_t argnum = 0;
  Lisp_Object tail = args;
  FOR_EACH_TAIL_SAFE (tail)
    {
      Lisp_Object arg = XCAR (tail);
      eassert (argnum < capacity);
      evaluated_args[argnum++] = eval_form (arg);
    }
  CHECK_LIST_END (tail, args);
}

/* Evaluate non-byte-compiled FORM within prevailing lexical
   scope.  */

Lisp_Object
eval_form (Lisp_Object form)
{
  static const unsigned char subr_max_nargs = 8; /* !!! tweak switch cases.  */
  Lisp_Object result = form;
  if (SYMBOLP (form))
    {
      Lisp_Object lexbind = find_lexbind (current_thread, form);
      result = ! NILP (lexbind) ? XCDR (lexbind) : Fsymbol_value (form);
    }
  else if (CONSP (form))
    {
      check_eval_depth (Qexcessive_lisp_nesting);
      maybe_quit ();
      // temporary ifndef
#ifndef HAVE_GCC_TLS
      maybe_garbage_collect ();
#endif
      Lisp_Object original_fun = XCAR (form);
      Lisp_Object args = XCDR (form);
      CHECK_LIST (args);
      specpdl_ref count = record_in_backtrace (original_fun, &args, UNEVALLED);
      if (debug_on_next_call)
	do_debug_on_call (Qt, count);

      USE_SAFE_ALLOCA;
      Lisp_Object fun;
    retry:
      /* Optimize for no indirection.  */
      fun = original_fun;
      if (! SYMBOLP (fun))
	fun = Ffunction (list1 (fun));
      else if (! NILP (fun) && (fun = XSYMBOL (fun)->u.s.function, SYMBOLP (fun)))
	fun = indirect_function (fun);

      if (SUBRP (fun) && ! SUBR_NATIVE_COMPILED_DYNP (fun))
	{
	  const ptrdiff_t nargs = list_length (args);
	  if (nargs < XSUBR (fun)->min_args
	      || (XSUBR (fun)->max_args >= 0
		  && XSUBR (fun)->max_args < nargs))
	    {
	      SAFE_FREE ();
	      xsignal2 (Qwrong_number_of_arguments, original_fun,
			make_fixnum (nargs));
	    }
	  else if (XSUBR (fun)->max_args == UNEVALLED)
	    result = (XSUBR (fun)->function.aUNEVALLED) (args);
	  else
	    {
	      const ptrdiff_t capacity = max (nargs, XSUBR (fun)->max_args);
	      Lisp_Object *evaluated_args;
	      SAFE_ALLOCA_LISP (evaluated_args, capacity);
	      populate_evaluated_args (args, evaluated_args, capacity);
	      set_backtrace_args (specpdl_ref_to_ptr (count), evaluated_args, nargs);

	      if (XSUBR (fun)->max_args == MANY
		  || XSUBR (fun)->max_args > subr_max_nargs)
		result = XSUBR (fun)->function.aMANY (nargs, evaluated_args);
	      else
		switch (XSUBR (fun)->max_args)
		  {
		  case 0:
		    result = (XSUBR (fun)->function.a0 ());
		    break;
		  case 1:
		    result = (XSUBR (fun)->function.a1
			      (evaluated_args[0]));
		    break;
		  case 2:
		    result = (XSUBR (fun)->function.a2
			      (evaluated_args[0], evaluated_args[1]));
		    break;
		  case 3:
		    result = (XSUBR (fun)->function.a3
			      (evaluated_args[0], evaluated_args[1], evaluated_args[2]));
		    break;
		  case 4:
		    result = (XSUBR (fun)->function.a4
			      (evaluated_args[0], evaluated_args[1], evaluated_args[2],
			       evaluated_args[3]));
		    break;
		  case 5:
		    result = (XSUBR (fun)->function.a5
			      (evaluated_args[0], evaluated_args[1], evaluated_args[2],
			       evaluated_args[3], evaluated_args[4]));
		    break;
		  case 6:
		    result = (XSUBR (fun)->function.a6
			      (evaluated_args[0], evaluated_args[1], evaluated_args[2],
			       evaluated_args[3], evaluated_args[4], evaluated_args[5]));
		    break;
		  case 7:
		    result = (XSUBR (fun)->function.a7
			      (evaluated_args[0], evaluated_args[1], evaluated_args[2],
			       evaluated_args[3], evaluated_args[4], evaluated_args[5],
			       evaluated_args[6]));
		    break;
		  case 8:
		    result = (XSUBR (fun)->function.a8
			      (evaluated_args[0], evaluated_args[1], evaluated_args[2],
			       evaluated_args[3], evaluated_args[4], evaluated_args[5],
			       evaluated_args[6], evaluated_args[7]));
		    break;
		  default:
		    SAFE_FREE ();
		    emacs_abort ();
		  }
	    }
	}
      else if (COMPILEDP (fun)
	       || SUBR_NATIVE_COMPILED_DYNP (fun)
	       || MODULE_FUNCTIONP (fun))
	{
	  const ptrdiff_t nargs = list_length (args);
	  Lisp_Object *evaluated_args;
	  SAFE_ALLOCA_LISP (evaluated_args, nargs);
	  populate_evaluated_args (args, evaluated_args, nargs);
	  set_backtrace_args (specpdl_ref_to_ptr (count), evaluated_args, nargs);
	  result = funcall_lambda (fun, nargs, evaluated_args);
	}
      else if (NILP (fun))
	{
	  SAFE_FREE ();
	  xsignal1 (Qvoid_function, original_fun);
	}
      else if (! CONSP (fun))
	{
	  SAFE_FREE ();
	  xsignal1 (Qinvalid_function, original_fun);
	}
      else if (! SYMBOLP (XCAR (fun)))
	{
	  SAFE_FREE ();
	  xsignal1 (Qinvalid_function, original_fun);
	}
      else if (EQ (XCAR (fun), Qautoload))
	{
	  Fautoload_do_load (fun, original_fun, Qnil);
	  goto retry;
	}
      else if (EQ (XCAR (fun), Qmacro))
	{
	  specpdl_ref count1 = SPECPDL_INDEX ();
	  Lisp_Object exp;
	  /* Set `lexical-binding' now so ensuing macroexpansion is
	     aware of how it will be interpreted, i.e., with lexical
	     scoping or not.  */
	  specbind (Qlexical_binding,
		    ! NILP (current_thread->lexical_environment) ? Qt : Qnil);

	  /* Apprise macro of any defvar declarations in scope. */
	  Lisp_Object dynvars = Vmacroexp__dynvars;
	  for (Lisp_Object p = current_thread->lexical_environment;
	       ! NILP (p);
	       p = XCDR(p))
	    {
	      Lisp_Object defvar = XCAR (p);
	      if (SYMBOLP (defvar))
		dynvars = Fcons(defvar, dynvars);
	    }
	  if (! EQ (dynvars, Vmacroexp__dynvars))
	    specbind (Qmacroexp__dynvars, dynvars);

	  exp = apply1 (CDR (fun), args);
	  exp = unbind_to (count1, exp);
	  result = eval_form (exp);
	}
      else if (EQ (XCAR (fun), Qlambda)
	       || EQ (XCAR (fun), Qclosure))
	{
	  const ptrdiff_t nargs = list_length (args);
	  Lisp_Object *evaluated_args;
	  SAFE_ALLOCA_LISP (evaluated_args, nargs);
	  populate_evaluated_args (args, evaluated_args, nargs);
	  set_backtrace_args (specpdl_ref_to_ptr (count), evaluated_args, nargs);
	  result = funcall_lambda (fun, nargs, evaluated_args);
	}
      else
	{
	  SAFE_FREE ();
	  xsignal1 (Qinvalid_function, original_fun);
	}
      pop_eval_frame (specpdl_ref_to_ptr (count), &result, sa_count); /* SAFE_FREEs */
    }
  return result;
}

DEFUN ("apply", Fapply, Sapply, 1, MANY, 0,
       doc: /* Call FUNCTION with our remaining args, using our last arg as list of args.
Then return the value FUNCTION returns.
With a single argument, call the argument's first element using the
other elements as args.
Thus, (apply \\='+ 1 2 \\='(3 4)) returns 10.
usage: (apply FUNCTION &rest ARGUMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i, funcall_nargs;
  Lisp_Object *funcall_args = NULL;
  Lisp_Object spread_arg = args[nargs - 1];
  Lisp_Object fun = args[0];
  USE_SAFE_ALLOCA;

  ptrdiff_t numargs = list_length (spread_arg);

  if (numargs == 0)
    return Ffuncall (max (1, nargs - 1), args);
  else if (numargs == 1)
    {
      args [nargs - 1] = XCAR (spread_arg);
      return Ffuncall (nargs, args);
    }

  numargs += nargs - 2;

  /* Optimize for no indirection.  */
  if (SYMBOLP (fun) && !NILP (fun)
      && (fun = XSYMBOL (fun)->u.s.function, SYMBOLP (fun)))
    {
      fun = indirect_function (fun);
      if (NILP (fun))
	/* Let funcall get the error.  */
	fun = args[0];
    }

  if (SUBRP (fun) && XSUBR (fun)->max_args > numargs
      /* Don't hide an error by adding missing arguments.  */
      && numargs >= XSUBR (fun)->min_args)
    {
      /* Avoid making funcall cons up a yet another new vector of arguments
	 by explicitly supplying nil's for optional values.  */
      SAFE_ALLOCA_LISP (funcall_args, 1 + XSUBR (fun)->max_args);
      memclear (funcall_args + numargs + 1,
		(XSUBR (fun)->max_args - numargs) * word_size);
      funcall_nargs = 1 + XSUBR (fun)->max_args;
    }
  else
    { /* We add 1 to numargs because funcall_args includes the
	 function itself as well as its arguments.  */
      SAFE_ALLOCA_LISP (funcall_args, 1 + numargs);
      funcall_nargs = 1 + numargs;
    }

  memcpy (funcall_args, args, nargs * word_size);
  /* Spread the last arg we got.  Its first element goes in
     the slot that it used to occupy, hence this value of I.  */
  i = nargs - 1;
  while (!NILP (spread_arg))
    {
      funcall_args [i++] = XCAR (spread_arg);
      spread_arg = XCDR (spread_arg);
    }

  Lisp_Object retval = Ffuncall (funcall_nargs, funcall_args);

  SAFE_FREE ();
  return retval;
}

/* Run hook variables in various ways.  */

static Lisp_Object
funcall_nil (ptrdiff_t nargs, Lisp_Object *args)
{
  Ffuncall (nargs, args);
  return Qnil;
}

DEFUN ("run-hooks", Frun_hooks, Srun_hooks, 0, MANY, 0,
       doc: /* Run each hook in HOOKS.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments.

Major modes should not use this function directly to run their mode
hook; they should use `run-mode-hooks' instead.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hooks &rest HOOKS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;

  for (i = 0; i < nargs; i++)
    run_hook (args[i]);

  return Qnil;
}

DEFUN ("run-hook-with-args", Frun_hook_with_args,
       Srun_hook_with_args, 1, MANY, 0,
       doc: /* Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  The value of HOOK
may be nil, a function, or a list of functions.  Call each
function in order with arguments ARGS.  The final return value
is unspecified.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hook-with-args HOOK &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return run_hook_with_args (nargs, args, funcall_nil);
}

/* NB this one still documents a specific non-nil return value.
   (As did run-hook-with-args and run-hook-with-args-until-failure
   until they were changed in 24.1.)  */
DEFUN ("run-hook-with-args-until-success", Frun_hook_with_args_until_success,
       Srun_hook_with_args_until_success, 1, MANY, 0,
       doc: /* Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  The value of HOOK
may be nil, a function, or a list of functions.  Call each
function in order with arguments ARGS, stopping at the first
one that returns non-nil, and return that value.  Otherwise (if
all functions return nil, or if there are no functions to call),
return nil.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hook-with-args-until-success HOOK &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return run_hook_with_args (nargs, args, Ffuncall);
}

static Lisp_Object
funcall_not (ptrdiff_t nargs, Lisp_Object *args)
{
  return NILP (Ffuncall (nargs, args)) ? Qt : Qnil;
}

DEFUN ("run-hook-with-args-until-failure", Frun_hook_with_args_until_failure,
       Srun_hook_with_args_until_failure, 1, MANY, 0,
       doc: /* Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  The value of HOOK
may be nil, a function, or a list of functions.  Call each
function in order with arguments ARGS, stopping at the first
one that returns nil, and return nil.  Otherwise (if all functions
return non-nil, or if there are no functions to call), return non-nil
\(do not rely on the precise return value in this case).

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hook-with-args-until-failure HOOK &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return NILP (run_hook_with_args (nargs, args, funcall_not)) ? Qt : Qnil;
}

static Lisp_Object
run_hook_wrapped_funcall (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object tmp = args[0], ret;
  args[0] = args[1];
  args[1] = tmp;
  ret = Ffuncall (nargs, args);
  args[1] = args[0];
  args[0] = tmp;
  return ret;
}

DEFUN ("run-hook-wrapped", Frun_hook_wrapped, Srun_hook_wrapped, 2, MANY, 0,
       doc: /* Run HOOK, passing each function through WRAP-FUNCTION.
I.e. instead of calling each function FUN directly with arguments ARGS,
it calls WRAP-FUNCTION with arguments FUN and ARGS.
As soon as a call to WRAP-FUNCTION returns non-nil, `run-hook-wrapped'
aborts and returns that value.
usage: (run-hook-wrapped HOOK WRAP-FUNCTION &rest ARGS)  */)
     (ptrdiff_t nargs, Lisp_Object *args)
{
  return run_hook_with_args (nargs, args, run_hook_wrapped_funcall);
}

/* ARGS[0] should be a hook symbol.
   Call each of the functions in the hook value, passing each of them
   as arguments all the rest of ARGS (all NARGS - 1 elements).
   FUNCALL specifies how to call each function on the hook.  */

Lisp_Object
run_hook_with_args (ptrdiff_t nargs, Lisp_Object *args,
		    Lisp_Object (*funcall) (ptrdiff_t nargs, Lisp_Object *args))
{
  Lisp_Object sym, val, ret = Qnil;

  /* If we are dying or still initializing,
     don't do anything--it would probably crash if we tried.  */
  if (NILP (Vrun_hooks))
    return Qnil;

  sym = args[0];
  CHECK_SYMBOL (sym);
  val = find_symbol_value (XSYMBOL (sym), NULL);

  if (EQ (val, Qunbound) || NILP (val))
    return ret;
  else if (!CONSP (val) || FUNCTIONP (val))
    {
      args[0] = val;
      return funcall (nargs, args);
    }
  else
    {
      Lisp_Object global_vals = Qnil;

      for (;
	   CONSP (val) && NILP (ret);
	   val = XCDR (val))
	{
	  if (EQ (XCAR (val), Qt))
	    {
	      /* t indicates this hook has a local binding;
		 it means to run the global binding too.  */
	      global_vals = Fdefault_value (sym);
	      if (NILP (global_vals)) continue;

	      if (!CONSP (global_vals) || EQ (XCAR (global_vals), Qlambda))
		{
		  args[0] = global_vals;
		  ret = funcall (nargs, args);
		}
	      else
		{
		  for (;
		       CONSP (global_vals) && NILP (ret);
		       global_vals = XCDR (global_vals))
		    {
		      args[0] = XCAR (global_vals);
		      /* In a global value, t should not occur.  If it does, we
			 must ignore it to avoid an endless loop.  */
		      if (!EQ (args[0], Qt))
			ret = funcall (nargs, args);
		    }
		}
	    }
	  else
	    {
	      args[0] = XCAR (val);
	      ret = funcall (nargs, args);
	    }
	}

      return ret;
    }
}

/* Run the hook HOOK, giving each function no args.  */

void
run_hook (Lisp_Object hook)
{
  Frun_hook_with_args (1, &hook);
}

/* Run the hook HOOK, giving each function the two args ARG1 and ARG2.  */

void
run_hook_with_args_2 (Lisp_Object hook, Lisp_Object arg1, Lisp_Object arg2)
{
  CALLN (Frun_hook_with_args, hook, arg1, arg2);
}

/* Apply fn to arg.  */
Lisp_Object
apply1 (Lisp_Object fn, Lisp_Object arg)
{
  return NILP (arg) ? Ffuncall (1, &fn) : CALLN (Fapply, fn, arg);
}

DEFUN ("functionp", Ffunctionp, Sfunctionp, 1, 1, 0,
       doc: /* Return t if OBJECT is a function.

An object is a function if it is callable via `funcall'; this includes
symbols with function bindings, but excludes macros and special forms.

Ordinarily return nil if OBJECT is not a function, although t might be
returned in rare cases.  */)
     (Lisp_Object object)
{
  if (FUNCTIONP (object))
    return Qt;
  return Qnil;
}

bool
FUNCTIONP (Lisp_Object object)
{
  if (SYMBOLP (object) && !NILP (Ffboundp (object)))
    {
      object = Findirect_function (object, Qt);

      if (CONSP (object) && EQ (XCAR (object), Qautoload))
	{
	  /* Autoloaded symbols are functions, except if they load
	     macros or keymaps.  */
	  for (int i = 0; i < 4 && CONSP (object); i++)
	    object = XCDR (object);

	  return ! (CONSP (object) && !NILP (XCAR (object)));
	}
    }

  if (SUBRP (object))
    return XSUBR (object)->max_args != UNEVALLED;
  else if (COMPILEDP (object) || MODULE_FUNCTIONP (object))
    return true;
  else if (CONSP (object))
    {
      Lisp_Object car = XCAR (object);
      return EQ (car, Qlambda) || EQ (car, Qclosure);
    }
  else
    return false;
}

Lisp_Object
funcall_general (Lisp_Object fun, ptrdiff_t numargs, Lisp_Object *args)
{
  Lisp_Object original_fun = fun;
 retry:
  if (SYMBOLP (fun) && !NILP (fun)
      && (fun = XSYMBOL (fun)->u.s.function, SYMBOLP (fun)))
    fun = indirect_function (fun);

  if (SUBRP (fun) && !SUBR_NATIVE_COMPILED_DYNP (fun))
    return funcall_subr (XSUBR (fun), numargs, args);
  else if (COMPILEDP (fun)
	   || SUBR_NATIVE_COMPILED_DYNP (fun)
	   || MODULE_FUNCTIONP (fun))
    return funcall_lambda (fun, numargs, args);
  else
    {
      if (NILP (fun))
	xsignal1 (Qvoid_function, original_fun);
      if (!CONSP (fun))
	xsignal1 (Qinvalid_function, original_fun);
      Lisp_Object funcar = XCAR (fun);
      if (!SYMBOLP (funcar))
	xsignal1 (Qinvalid_function, original_fun);
      if (EQ (funcar, Qlambda)
	  || EQ (funcar, Qclosure))
	return funcall_lambda (fun, numargs, args);
      else if (EQ (funcar, Qautoload))
	{
	  Fautoload_do_load (fun, original_fun, Qnil);
	  fun = original_fun;
	  goto retry;
	}
      else
	xsignal1 (Qinvalid_function, original_fun);
    }
}

DEFUN ("funcall", Ffuncall, Sfuncall, 1, MANY, 0,
       doc: /* Call first argument as a function, passing remaining arguments to it.
Return the value that function returns.
Thus, (funcall \\='cons \\='x \\='y) returns (x . y).
usage: (funcall FUNCTION &rest ARGUMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = record_in_backtrace (args[0], &args[1], nargs - 1);

  check_eval_depth (Qexcessive_lisp_nesting);
  maybe_quit ();
  maybe_garbage_collect ();

  if (debug_on_next_call)
    do_debug_on_call (Qlambda, count);

  Lisp_Object val = funcall_general (args[0], nargs - 1, args + 1);
  pop_eval_frame (specpdl_ref_to_ptr (count), &val, make_invalid_specpdl_ref ());
  return val;
}

static Lisp_Object
safe_eval_handler (Lisp_Object arg, ptrdiff_t nargs, Lisp_Object *args)
{
  add_to_log ("Error muted by safe_call: %S signaled %S",
	      Flist (nargs, args), arg);
  return Qnil;
}

Lisp_Object
safe_funcall (ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();
  /* FIXME: This function started its life in 'xdisp.c' for use internally
     by the redisplay.  So it was important to inhibit redisplay.
     Not clear if we still need this 'specbind' now that 'xdisp.c' has its
     own version of this code.  */
  specbind (Qinhibit_redisplay, Qt);
  /* Use Qt to ensure debugger does not run.  */
  Lisp_Object val = internal_condition_case_n (Ffuncall, nargs, args, Qt,
				               safe_eval_handler);
  return unbind_to (count, val);
}

Lisp_Object
safe_eval (Lisp_Object sexp)
{
  return safe_calln (Qeval, sexp, Qt);
}

/* Apply a C subroutine SUBR to the NUMARGS evaluated arguments in ARG_VECTOR
   and return the result of evaluation.  */

Lisp_Object
funcall_subr (struct Lisp_Subr *subr, ptrdiff_t numargs, Lisp_Object *args)
{
  eassume (numargs >= 0);
  if (numargs >= subr->min_args)
    {
      /* Conforming call to finite-arity subr.  */
      ptrdiff_t maxargs = subr->max_args;
      if (numargs <= maxargs && maxargs <= 8)
	{
	  Lisp_Object argbuf[8];
	  Lisp_Object *a;
	  if (numargs < maxargs)
	    {
	      eassume (maxargs <= ARRAYELTS (argbuf));
	      a = argbuf;
	      memcpy (a, args, numargs * word_size);
	      memclear (a + numargs, (maxargs - numargs) * word_size);
	    }
	  else
	    a = args;
	  switch (maxargs)
	    {
	    case 0:
	      return subr->function.a0 ();
	    case 1:
	      return subr->function.a1 (a[0]);
	    case 2:
	      return subr->function.a2 (a[0], a[1]);
	    case 3:
	      return subr->function.a3 (a[0], a[1], a[2]);
	    case 4:
	      return subr->function.a4 (a[0], a[1], a[2], a[3]);
	    case 5:
	      return subr->function.a5 (a[0], a[1], a[2], a[3], a[4]);
	    case 6:
	      return subr->function.a6 (a[0], a[1], a[2], a[3], a[4], a[5]);
	    case 7:
	      return subr->function.a7 (a[0], a[1], a[2], a[3], a[4], a[5],
					a[6]);
	    case 8:
	      return subr->function.a8 (a[0], a[1], a[2], a[3], a[4], a[5],
					a[6], a[7]);
	    }
	  eassume (false);	/* In case the compiler is too stupid.  */
	}

      /* Call to n-adic subr.  */
      if (maxargs == MANY || maxargs > 8)
	return subr->function.aMANY (numargs, args);
    }

  /* Anything else is an error.  */
  Lisp_Object fun;
  XSETSUBR (fun, subr);
  if (subr->max_args == UNEVALLED)
    xsignal1 (Qinvalid_function, fun);
  else
    xsignal2 (Qwrong_number_of_arguments, fun, make_fixnum (numargs));
}

/* Call the compiled Lisp function FUN.  If we have not yet read FUN's
   bytecode string and constants vector, fetch them from the file first.  */

static Lisp_Object
fetch_and_exec_byte_code (Lisp_Object fun, ptrdiff_t args_template,
			  ptrdiff_t nargs, Lisp_Object *args)
{
  if (CONSP (AREF (fun, COMPILED_BYTECODE)))
    Ffetch_bytecode (fun);

  return exec_byte_code (fun, args_template, nargs, args);
}

/* Return the result of applying FUN to NARGS number of evaluated
   arguments in ARG_VECTOR.  FUN is one of a lambda-expression, a
   compiled-code object, or a module function.  */

static Lisp_Object
funcall_lambda (Lisp_Object fun, ptrdiff_t nargs, Lisp_Object *arg_vector)
{
  specpdl_ref count = SPECPDL_INDEX ();
  Lisp_Object args, lexenv;
  ptrdiff_t args_template = -1;

  if (COMPILEDP (fun))
    {
      Lisp_Object first_slot = AREF (fun, COMPILED_ARGLIST);
      eassert (! CONSP (fun));
      if (FIXNUMP (first_slot))
	{
	  /* An integer first slot denotes the number of lexically
	     scoped arguments.  Defer all argument binding to the
	     bytecode interpreter.  */
	  args_template = XFIXNUM (first_slot);
	  goto funcall_lambda_return;
	}
      else
	{
	  /* Otherwise the slot represents a formal arglist of
	     dynamically scoped variables.  */
	  args = first_slot;
	  lexenv = Qnil;
	}
    }
  else if (CONSP (fun))
    {
      if (EQ (XCAR (fun), Qclosure))
	{
	  Lisp_Object cdr = XCDR (fun);	/* Drop 'closure.  */
	  if (! CONSP (cdr))
	    xsignal1 (Qinvalid_function, fun);
	  fun = cdr;
	  lexenv = XCAR (fun);
	}
      else
	lexenv = Qnil;
      args = XCDR (fun);
      if (CONSP (args))
	args = XCAR (args);
      else
	xsignal1 (Qinvalid_function, fun);
    }
#ifdef HAVE_MODULES
  else if (MODULE_FUNCTIONP (fun))
    return funcall_module (fun, nargs, arg_vector);
#endif
#ifdef HAVE_NATIVE_COMP
  else if (SUBR_NATIVE_COMPILED_DYNP (fun))
    {
      args = XSUBR (fun)->lambda_list;
      lexenv = Qnil;
    }
#endif
  else
    emacs_abort ();

  bool optional = false,
    rest = false,
    previous_rest = false;
  ptrdiff_t i = 0;
  FOR_EACH_TAIL (args)
    {
      Lisp_Object sym = XCAR (args);
      if (! SYMBOLP (sym))
	xsignal1 (Qinvalid_function, fun);

      if (EQ (sym, Qand_rest))
        {
          if (rest || previous_rest)
            xsignal1 (Qinvalid_function, fun);
          rest = true;
	  previous_rest = true;
        }
      else if (EQ (sym, Qand_optional))
        {
          if (optional || rest || previous_rest)
            xsignal1 (Qinvalid_function, fun);
          optional = true;
        }
      else
	{
	  Lisp_Object arg;
	  if (rest)
	    {
	      arg = Flist (nargs - i, &arg_vector[i]);
	      i = nargs;
	    }
	  else if (i < nargs)
	    arg = arg_vector[i++];
	  else if (! optional)
	    xsignal2 (Qwrong_number_of_arguments, fun, make_fixnum (nargs));
	  else
	    arg = Qnil;

	  if (! NILP (lexenv) && SYMBOLP (sym))
	    /* Lexically bind SYM.  */
	    lexenv = Fcons (Fcons (sym, arg), lexenv);
	  else
	    /* Dynamically bind SYM.  */
	    specbind (sym, arg);
	  previous_rest = false;
	}
    }

  if (! NILP (args) || previous_rest)
    xsignal1 (Qinvalid_function, fun);
  else if (i < nargs)
    xsignal2 (Qwrong_number_of_arguments, fun, make_fixnum (nargs));
  else if (! EQ (lexenv, current_thread->lexical_environment))
    {
      record_lexical_environment ();
      current_thread->lexical_environment = lexenv;
    }

  eassert (! SUBR_NATIVE_COMPILEDP (fun)
	   || SUBR_NATIVE_COMPILED_DYNP (fun));
  Lisp_Object retval;
 funcall_lambda_return:
  retval = CONSP (fun)
    ? Fprogn (XCDR (XCDR (fun)))
    : SUBR_NATIVE_COMPILEDP (fun)
    /* save call to funcall_subr since 0 args by construction */
    ? XSUBR (fun)->function.a0 ()
    : args_template < 0
    ? fetch_and_exec_byte_code (fun, 0, 0, NULL)
    : fetch_and_exec_byte_code (fun, args_template, nargs, arg_vector);
  return specpdl_ptr == specpdl_ref_to_ptr (count)
    ? retval : unbind_to (count, retval);
}

DEFUN ("func-arity", Ffunc_arity, Sfunc_arity, 1, 1, 0,
       doc: /* Return minimum and maximum number of args allowed for FUNCTION.
FUNCTION must be a function of some kind.
The returned value is a cons cell (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number, or the symbol `many', for a
function with `&rest' args, or `unevalled' for a special form.  */)
  (Lisp_Object function)
{
  Lisp_Object original;
  Lisp_Object funcar;
  Lisp_Object result;

  original = function;

 retry:

  /* Optimize for no indirection.  */
  function = original;
  if (SYMBOLP (function) && !NILP (function))
    {
      function = XSYMBOL (function)->u.s.function;
      if (SYMBOLP (function))
	function = indirect_function (function);
    }

  if (CONSP (function) && EQ (XCAR (function), Qmacro))
    function = XCDR (function);

  if (SUBRP (function))
    result = Fsubr_arity (function);
  else if (COMPILEDP (function))
    result = lambda_arity (function);
#ifdef HAVE_MODULES
  else if (MODULE_FUNCTIONP (function))
    result = module_function_arity (XMODULE_FUNCTION (function));
#endif
  else
    {
      if (NILP (function))
	xsignal1 (Qvoid_function, original);
      if (!CONSP (function))
	xsignal1 (Qinvalid_function, original);
      funcar = XCAR (function);
      if (!SYMBOLP (funcar))
	xsignal1 (Qinvalid_function, original);
      if (EQ (funcar, Qlambda)
	  || EQ (funcar, Qclosure))
	result = lambda_arity (function);
      else if (EQ (funcar, Qautoload))
	{
	  Fautoload_do_load (function, original, Qnil);
	  goto retry;
	}
      else
	xsignal1 (Qinvalid_function, original);
    }
  return result;
}

/* FUN must be either a lambda-expression or a compiled-code object.  */
static Lisp_Object
lambda_arity (Lisp_Object fun)
{
  Lisp_Object syms_left;

  if (CONSP (fun))
    {
      if (EQ (XCAR (fun), Qclosure))
	{
	  fun = XCDR (fun);	/* Drop `closure'.  */
	  CHECK_CONS (fun);
	}
      syms_left = XCDR (fun);
      if (CONSP (syms_left))
	syms_left = XCAR (syms_left);
      else
	xsignal1 (Qinvalid_function, fun);
    }
  else if (COMPILEDP (fun))
    {
      syms_left = AREF (fun, COMPILED_ARGLIST);
      if (FIXNUMP (syms_left))
        return get_byte_code_arity (syms_left);
    }
  else
    emacs_abort ();

  EMACS_INT minargs = 0, maxargs = 0;
  bool optional = false;
  for (; CONSP (syms_left); syms_left = XCDR (syms_left))
    {
      Lisp_Object sym = XCAR (syms_left);
      if (! SYMBOLP (sym))
	xsignal1 (Qinvalid_function, fun);

      if (EQ (sym, Qand_rest))
	return Fcons (make_fixnum (minargs), Qmany);
      else if (EQ (sym, Qand_optional))
	optional = true;
      else
	{
          if (!optional)
            minargs++;
          maxargs++;
        }
    }

  if (!NILP (syms_left))
    xsignal1 (Qinvalid_function, fun);

  return Fcons (make_fixnum (minargs), make_fixnum (maxargs));
}

DEFUN ("fetch-bytecode", Ffetch_bytecode, Sfetch_bytecode,
       1, 1, 0,
       doc: /* If byte-compiled OBJECT is lazy-loaded, fetch it now.  */)
  (Lisp_Object object)
{
  Lisp_Object tem;

  if (COMPILEDP (object))
    {
      if (CONSP (AREF (object, COMPILED_BYTECODE)))
	{
	  tem = read_doc_string (AREF (object, COMPILED_BYTECODE));
	  if (! (CONSP (tem) && STRINGP (XCAR (tem))
		 && VECTORP (XCDR (tem))))
	    {
	      tem = AREF (object, COMPILED_BYTECODE);
	      if (CONSP (tem) && STRINGP (XCAR (tem)))
		error ("Invalid byte code in %s", SDATA (XCAR (tem)));
	      else
		error ("Invalid byte code");
	    }

	  Lisp_Object bytecode = XCAR (tem);
	  if (STRING_MULTIBYTE (bytecode))
	    {
	      /* BYTECODE must have been produced by Emacs 20.2 or earlier
		 because it produced a raw 8-bit string for byte-code and now
		 such a byte-code string is loaded as multibyte with raw 8-bit
		 characters converted to multibyte form.  Convert them back to
		 the original unibyte form.  */
	      bytecode = Fstring_as_unibyte (bytecode);
	    }

	  pin_string (bytecode);
	  ASET (object, COMPILED_BYTECODE, bytecode);
	  ASET (object, COMPILED_CONSTANTS, XCDR (tem));
	}
    }
  return object;
}

/* Return true in the circumstance where SYMBOL was let-bound when the
   buffer had not yet assigned its own bespoke buffer local value for
   SYMBOL.

   In hewing to Blandy's buffer local semantics, Monnier decided that
   assignment within such a `let' closure should change the SYMBOL's
   default, not value, binding.
*/

bool
locally_unbound_blv_let_bounded (struct Lisp_Symbol *symbol)
{
  for (union specbinding *pdl = specpdl_ptr - 1; pdl > specpdl; --pdl)
    if (pdl->kind == SPECPDL_LET_BLD
	&& XSYMBOL (specpdl_symbol (pdl)) == symbol
	&& EQ (specpdl_buffer (pdl), Fcurrent_buffer ()))
      return true;
  return false;
}

void
specbind (Lisp_Object argsym, Lisp_Object value)
{
  Lisp_Object symbol;
  struct Lisp_Symbol *xsymbol;

  CHECK_SYMBOL (argsym);
  symbol = argsym;
  xsymbol = XSYMBOL (symbol);

  if (xsymbol->u.s.type == SYMBOL_VARALIAS)
    {
      xsymbol = SYMBOL_ALIAS (xsymbol);
      XSETSYMBOL (symbol, xsymbol);
    }

#ifdef HAVE_GCC_TLS
  symbol = thread_symbol (symbol);
  xsymbol = XSYMBOL (symbol);
#endif

  /* First, push old value onto let-stack.  Unintuitively, its KIND
     depends on ARGSYM's type, not its own.  */
  switch (xsymbol->u.s.type)
    {
    case SYMBOL_PLAINVAL:
      specpdl_ptr->let.kind = SPECPDL_LET;
      specpdl_ptr->let.symbol = symbol;
      specpdl_ptr->let.value = SYMBOL_VAL (xsymbol);
      break;
    case SYMBOL_LOCAL_SOMEWHERE:
      specpdl_ptr->let.kind = SPECPDL_LET_BLV;
      specpdl_ptr->let.symbol = symbol;
      specpdl_ptr->let.value = find_symbol_value (xsymbol, current_buffer);
      specpdl_ptr->let.buffer = Fcurrent_buffer ();
      eassert (XBUFFER (xsymbol->u.s.buffer_local_buffer) == current_buffer);
      /* See locally_unbound_blv_let_bounded.  */
      if (NILP (Flocal_variable_p (symbol, Fcurrent_buffer ())))
	specpdl_ptr->let.kind = SPECPDL_LET_BLD;
      break;
    case SYMBOL_FORWARDED:
    case SYMBOL_KBOARD:
      specpdl_ptr->let.kind = SPECPDL_LET;
      specpdl_ptr->let.symbol = symbol;
      specpdl_ptr->let.value = find_symbol_value (xsymbol, current_buffer);
      specpdl_ptr->let.buffer = Fcurrent_buffer ();
      break;
    case SYMBOL_PER_BUFFER:
      /* See locally_unbound_blv_let_bounded.  */
      specpdl_ptr->let.kind =
	NILP (Flocal_variable_p (symbol, Fcurrent_buffer ()))
	? SPECPDL_LET_BLD
	: SPECPDL_LET_BLV;
      specpdl_ptr->let.symbol = symbol;
      specpdl_ptr->let.value = find_symbol_value (xsymbol, current_buffer);
      specpdl_ptr->let.buffer = Fcurrent_buffer ();
      break;
    default:
      emacs_abort ();
      break;
    }
  grow_specpdl ();

  /* Second, set SYMBOL to the new value.  */
  if (xsymbol->u.s.type == SYMBOL_PER_BUFFER
      && specpdl_kind (specpdl_ptr - 1) == SPECPDL_LET_BLD)
    set_default_internal (specpdl_symbol (specpdl_ptr - 1), value, SET_INTERNAL_BIND);
  else
    set_internal (specpdl_symbol (specpdl_ptr - 1), value, Qnil, SET_INTERNAL_BIND);
}

void
record_unwind_protect (void (*function) (Lisp_Object), Lisp_Object arg)
{
  specpdl_ptr->unwind.kind = SPECPDL_UNWIND;
  specpdl_ptr->unwind.func = function;
  specpdl_ptr->unwind.arg = arg;

  specpdl_ptr->unwind.eval_depth = lisp_eval_depth;
  grow_specpdl ();
}

void
record_unwind_protect_array (Lisp_Object *array, ptrdiff_t nelts)
{
  specpdl_ptr->unwind_array.kind = SPECPDL_UNWIND_ARRAY;
  specpdl_ptr->unwind_array.array = array;
  specpdl_ptr->unwind_array.nelts = nelts;
  grow_specpdl ();
}

void
record_unwind_protect_ptr (void (*function) (void *), void *arg)
{
  specpdl_ptr->unwind_ptr.kind = SPECPDL_UNWIND_PTR;
  specpdl_ptr->unwind_ptr.func = function;
  specpdl_ptr->unwind_ptr.arg = arg;
  specpdl_ptr->unwind_ptr.mark = NULL;
  grow_specpdl ();
}

/* Like `record_unwind_protect_ptr', but also specifies a function
   for GC-marking Lisp objects only reachable through ARG.  */
void
record_unwind_protect_ptr_mark (void (*function) (void *), void *arg,
				void (*mark) (void *))
{
  specpdl_ptr->unwind_ptr.kind = SPECPDL_UNWIND_PTR;
  specpdl_ptr->unwind_ptr.func = function;
  specpdl_ptr->unwind_ptr.arg = arg;
  specpdl_ptr->unwind_ptr.mark = mark;
  grow_specpdl ();
}

void
record_unwind_protect_int (void (*function) (int), int arg)
{
  specpdl_ptr->unwind_int.kind = SPECPDL_UNWIND_INT;
  specpdl_ptr->unwind_int.func = function;
  specpdl_ptr->unwind_int.arg = arg;
  grow_specpdl ();
}

void
record_unwind_protect_intmax (void (*function) (intmax_t), intmax_t arg)
{
  specpdl_ptr->unwind_intmax.kind = SPECPDL_UNWIND_INTMAX;
  specpdl_ptr->unwind_intmax.func = function;
  specpdl_ptr->unwind_intmax.arg = arg;
  grow_specpdl ();
}

void
record_unwind_protect_excursion (void)
{
  specpdl_ptr->unwind_excursion.kind = SPECPDL_UNWIND_EXCURSION;
  save_excursion_save (specpdl_ptr);
  grow_specpdl ();
}

void
record_unwind_protect_void (void (*function) (void))
{
  specpdl_ptr->unwind_void.kind = SPECPDL_UNWIND_VOID;
  specpdl_ptr->unwind_void.func = function;
  grow_specpdl ();
}

void
record_unwind_protect_module (enum specbind_tag kind, void *ptr)
{
  specpdl_ptr->unwind_ptr.kind = kind;
  specpdl_ptr->unwind_ptr.func = NULL;
  specpdl_ptr->unwind_ptr.arg = ptr;
  specpdl_ptr->unwind_ptr.mark = NULL;
  grow_specpdl ();
}

void
record_lexical_environment (void)
{
  specpdl_ptr->lexical_environment.kind = SPECPDL_LEXICAL_ENVIRONMENT;
  specpdl_ptr->lexical_environment.value = current_thread->lexical_environment;
  grow_specpdl ();
}

static void
do_nothing (void)
{}

/* Push an unwind-protect entry that does nothing, so that
   set_unwind_protect_ptr can overwrite it later.  */

void
record_unwind_protect_nothing (void)
{
  record_unwind_protect_void (do_nothing);
}

/* Clear the unwind-protect entry COUNT, so that it does nothing.
   It need not be at the top of the stack.  */

void
clear_unwind_protect (specpdl_ref count)
{
  union specbinding *p = specpdl_ref_to_ptr (count);
  p->unwind_void.kind = SPECPDL_UNWIND_VOID;
  p->unwind_void.func = do_nothing;
}

/* Set the unwind-protect entry COUNT so that it invokes FUNC (ARG).
   It need not be at the top of the stack.  Discard the entry's
   previous value without invoking it.  */

void
set_unwind_protect (specpdl_ref count, void (*func) (Lisp_Object),
		    Lisp_Object arg)
{
  union specbinding *p = specpdl_ref_to_ptr (count);
  p->unwind.kind = SPECPDL_UNWIND;
  p->unwind.func = func;
  p->unwind.arg = arg;
  p->unwind.eval_depth = lisp_eval_depth;
}

void
set_unwind_protect_ptr (specpdl_ref count, void (*func) (void *), void *arg)
{
  union specbinding *p = specpdl_ref_to_ptr (count);
  p->unwind_ptr.kind = SPECPDL_UNWIND_PTR;
  p->unwind_ptr.func = func;
  p->unwind_ptr.arg = arg;
  p->unwind_ptr.mark = NULL;
}

/* Pop entries from unwind-protect stack until specpdl_ptr reaches the
   specbinding at depth COUNT.  Pointer decrementing gets confusing
   because specpdl_ptr is one past the last operative specbinding
   (specpdl_ptr points to the "on deck" batter) and is not itself
   unbind-able.  Return VALUE.  */

Lisp_Object
unbind_to (specpdl_ref count, Lisp_Object value)
{
  const Lisp_Object restore_quit_flag = Vquit_flag;
  Vquit_flag = Qnil;

  while (specpdl_ptr != specpdl_ref_to_ptr (count))
    {
      eassert (specpdl_ptr > specpdl_ref_to_ptr (count));
      switch ((--specpdl_ptr)->kind)
	{
	case SPECPDL_UNWIND:
	  lisp_eval_depth = specpdl_ptr->unwind.eval_depth;
	  specpdl_ptr->unwind.func (specpdl_ptr->unwind.arg);
	  break;
	case SPECPDL_UNWIND_ARRAY:
	  xfree (specpdl_ptr->unwind_array.array);
	  break;
	case SPECPDL_UNWIND_PTR:
	  specpdl_ptr->unwind_ptr.func (specpdl_ptr->unwind_ptr.arg);
	  break;
	case SPECPDL_UNWIND_INT:
	  specpdl_ptr->unwind_int.func (specpdl_ptr->unwind_int.arg);
	  break;
	case SPECPDL_UNWIND_INTMAX:
	  specpdl_ptr->unwind_intmax.func (specpdl_ptr->unwind_intmax.arg);
	  break;
	case SPECPDL_UNWIND_VOID:
	  specpdl_ptr->unwind_void.func ();
	  break;
	case SPECPDL_UNWIND_EXCURSION:
	  save_excursion_restore (specpdl_ptr->unwind_excursion.marker,
				  specpdl_ptr->unwind_excursion.window);
	  break;
	case SPECPDL_BACKTRACE:
	case SPECPDL_NOP:
	  break;
#ifdef HAVE_MODULES
	case SPECPDL_MODULE_RUNTIME:
	  finalize_runtime_unwind (specpdl_ptr->unwind_ptr.arg);
	  break;
	case SPECPDL_MODULE_ENVIRONMENT:
	  finalize_environment_unwind (specpdl_ptr->unwind_ptr.arg);
	  break;
#endif
	case SPECPDL_LEXICAL_ENVIRONMENT:
	  current_thread->lexical_environment = specpdl_value (specpdl_ptr);
	  break;
	case SPECPDL_LET:
	  {
	    Lisp_Object sym = specpdl_symbol (specpdl_ptr);
	    if (XSYMBOL (sym)->u.s.type == SYMBOL_PLAINVAL)
	      {
		set_internal (sym, specpdl_value (specpdl_ptr),
			      Qnil, SET_INTERNAL_UNBIND);
		break;
	      }
	  }
	  /* gets here under `make-local-variable' on symbol not earlier
	     let-bound.  */
	  FALLTHROUGH;
	case SPECPDL_LET_BLD:
	  set_default_internal (specpdl_symbol (specpdl_ptr),
				specpdl_value (specpdl_ptr),
				SET_INTERNAL_UNBIND);
	  break;
	case SPECPDL_LET_BLV:
	  {
	    Lisp_Object symbol = specpdl_symbol (specpdl_ptr);
	    Lisp_Object val = specpdl_value (specpdl_ptr);
	    Lisp_Object where = specpdl_buffer (specpdl_ptr);
	    eassert (BUFFERP (where));
	    if (! NILP (Flocal_variable_p (symbol, where)))
	      set_internal (symbol, val, where, SET_INTERNAL_UNBIND);
	  }
	  break;
	default:
	  break;
	}
    }

  eassert (specpdl_ptr == specpdl_ref_to_ptr (count));
  if (NILP (Vquit_flag))
    Vquit_flag = restore_quit_flag;
  return value;
}

DEFUN ("special-variable-p", Fspecial_variable_p, Sspecial_variable_p, 1, 1, 0,
       doc: /* Return non-nil if SYMBOL's global binding has been declared special.
A special variable is one that will be bound dynamically, even in a
context where binding is lexical by default.  */)
  (Lisp_Object symbol)
{
   CHECK_SYMBOL (symbol);
   return XSYMBOL (symbol)->u.s.declared_special ? Qt : Qnil;
}

static union specbinding *
get_backtrace_starting_at (Lisp_Object base)
{
  union specbinding *pdl = backtrace_top (current_thread);

  if (! NILP (base))
    {
      /* Skip up to BASE.  */
      int offset = 0;
      if (CONSP (base) && FIXNUMP (XCAR (base)))
        {
          offset = XFIXNUM (XCAR (base));
          base = XCDR (base);
        }
      base = Findirect_function (base, Qt);
      while (backtrace_valid_p (current_thread, pdl)
             && ! EQ (base, Findirect_function (backtrace_function (pdl), Qt)))
        pdl = backtrace_next (current_thread, pdl);
      while (backtrace_valid_p (current_thread, pdl) && offset-- > 0)
        pdl = backtrace_next (current_thread, pdl);
    }
  return pdl;
}

static union specbinding *
get_backtrace_frame (Lisp_Object base, ptrdiff_t nframes)
{
  union specbinding *pdl = specpdl ? get_backtrace_starting_at (base) : NULL;
  if (pdl)
    for (ptrdiff_t i = 0;
	 i < nframes && backtrace_valid_p (current_thread, pdl);
	 ++i)
      pdl = backtrace_next (current_thread, pdl);
  return pdl;
}

static Lisp_Object
backtrace_frame_apply (Lisp_Object function, union specbinding *pdl)
{
  Lisp_Object result = Qnil;
  if (backtrace_valid_p (current_thread, pdl))
    {
      Lisp_Object flags = backtrace_debug_on_exit (pdl)
	? list2 (QCdebug_on_exit, Qt)
	: Qnil;
      bool unevalled = (backtrace_nargs (pdl) == UNEVALLED);
      result = call4 (function,
		      unevalled ? Qnil : Qt,
		      backtrace_function (pdl),
		      unevalled ? *backtrace_args (pdl) : Flist (backtrace_nargs (pdl),
								 backtrace_args (pdl)),
		      flags);
    }
  return result;
}

DEFUN ("backtrace-debug", Fbacktrace_debug, Sbacktrace_debug, 2, 3, 0,
       doc: /* Debug on exit the frame LEVEL levels from top.
LEVEL and BASE are as `backtrace-frame'.  Turns off debugging
if FLAG is nil.  */)
  (Lisp_Object level, Lisp_Object flag, Lisp_Object base)
{
  CHECK_FIXNAT (level);
  union specbinding *pdl = get_backtrace_frame (base, XFIXNAT (level));
  if (backtrace_valid_p (current_thread, pdl))
    set_backtrace_debug_on_exit (pdl, ! NILP (flag));
  return flag;
}

DEFUN ("mapbacktrace", Fmapbacktrace, Smapbacktrace, 1, 2, 0,
       doc: /* Call FUNCTION for each frame in backtrace.
If BASE is non-nil, it should be a function and iteration will start
from its nearest activation frame.
FUNCTION is called with 4 arguments: EVALD, FUNC, ARGS, and FLAGS.  If
a frame has not evaluated its arguments yet or is a special form,
EVALD is nil and ARGS is a list of forms.  If a frame has evaluated
its arguments and called its function already, EVALD is t and ARGS is
a list of values.
FLAGS is a plist of properties of the current frame: currently, the
only supported property is :debug-on-exit.  `mapbacktrace' always
returns nil.  */)
     (Lisp_Object function, Lisp_Object base)
{
  union specbinding *pdl = get_backtrace_starting_at (base);
  while (backtrace_valid_p (current_thread, pdl))
    {
      ptrdiff_t i = pdl - specpdl;
      backtrace_frame_apply (function, pdl);
      /* Beware! PDL is no longer valid here because FUNCTION might
         have caused grow_specpdl to reallocate pdlvec.  We must use
         the saved index, cf. Bug#27258.  */
      pdl = backtrace_next (current_thread, specpdl + i);
    }
  return Qnil;
}

DEFUN ("backtrace-frame--internal", Fbacktrace_frame_internal,
       Sbacktrace_frame_internal, 3, 3, NULL,
       doc: /* Call FUNCTION on stack frame NFRAMES away from BASE.
Return the result of FUNCTION, or nil if no matching frame could be found. */)
     (Lisp_Object function, Lisp_Object nframes, Lisp_Object base)
{
  return backtrace_frame_apply (function, get_backtrace_frame (base, XFIXNAT (nframes)));
}

DEFUN ("backtrace--frames-from-thread", Fbacktrace_frames_from_thread,
       Sbacktrace_frames_from_thread, 1, 1, NULL,
       doc: /* Return the list of backtrace frames from current execution point in THREAD.
If a frame has not evaluated the arguments yet (or is a special form),
the value of the list element is (nil FUNCTION ARG-FORMS...).
If a frame has evaluated its arguments and called its function already,
the value of the list element is (t FUNCTION ARG-VALUES...).
A &rest arg is represented as the tail of the list ARG-VALUES.
FUNCTION is whatever was supplied as car of evaluated list,
or a lambda expression for macro calls.  */)
     (Lisp_Object thread)
{
  struct thread_state *tstate;
  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  union specbinding *pdl = backtrace_top (tstate);
  Lisp_Object list = Qnil;

  while (backtrace_valid_p (tstate, pdl))
    {
      Lisp_Object frame;
      if (backtrace_nargs (pdl) == UNEVALLED)
	frame = Fcons (Qnil,
		      Fcons (backtrace_function (pdl), *backtrace_args (pdl)));
      else
	{
	  Lisp_Object tem = Flist (backtrace_nargs (pdl), backtrace_args (pdl));
	  frame = Fcons (Qt, Fcons (backtrace_function (pdl), tem));
	}
      list = Fcons (frame, list);
      pdl = backtrace_next (tstate, pdl);
    }
  return Fnreverse (list);
}

/* For `backtrace-eval', we unwind to a specific frame, evaluate a
   subexpression, then rewind back.  For thread switches, we unwind
   the from-thread's let-stack, then rewind the to-thread's
   (previously unwound) let-stack since symbol data are global.

   Values to be rewound are stored directly in existing specpdl
   elements, much like Schorr-Waite autophagiac graph marking.  */

static void
specpdl_internal_walk (union specbinding *pdl, int step, int distance,
		       enum specbind_tag kind_or_above)
{
  eassert (distance >= 0);
  for (union specbinding *bind = pdl + step; distance > 0; --distance, bind += step)
    {
      if (bind->kind >= kind_or_above)
	{
	  switch (bind->kind)
	    {
	    case SPECPDL_UNWIND:
	      if (bind->unwind.func == set_buffer_if_live)
		{
		  Lisp_Object oldarg = bind->unwind.arg;
		  bind->unwind.arg = Fcurrent_buffer ();
		  set_buffer_if_live (oldarg);
		}
	      break;
	    case SPECPDL_UNWIND_EXCURSION:
	      {
		Lisp_Object marker = bind->unwind_excursion.marker;
		Lisp_Object window = bind->unwind_excursion.window;
		save_excursion_save (bind);
		save_excursion_restore (marker, window);
	      }
	      break;
	    case SPECPDL_LEXICAL_ENVIRONMENT:
	      {
		Lisp_Object value = specpdl_value (bind);
		bind->lexical_environment.value = current_thread->lexical_environment;
		current_thread->lexical_environment = value;
	      }
	      break;
	    case SPECPDL_LET:
	      {
		Lisp_Object sym = specpdl_symbol (bind);
		if (XSYMBOL (sym)->u.s.type == SYMBOL_PLAINVAL)
		  {
		    Lisp_Object value = specpdl_value (bind);
		    bind->let.value = SYMBOL_VAL (XSYMBOL (sym));
		    SET_SYMBOL_VAL (XSYMBOL (sym), value);
		    break;
		  }
	      }
	      /* gets here under `make-local-variable' on symbol not
		 earlier let-bound.  */
	      FALLTHROUGH;
	    case SPECPDL_LET_BLD:
	      {
		Lisp_Object sym = specpdl_symbol (bind);
		Lisp_Object value = specpdl_value (bind);
		bind->let.value = default_value (sym);
		set_default_internal (sym, value, SET_INTERNAL_THREAD_SWITCH);
	      }
	      break;
	    case SPECPDL_LET_BLV:
	      {
		Lisp_Object symbol = specpdl_symbol (bind);
		Lisp_Object where = specpdl_buffer (bind);
		Lisp_Object value = specpdl_value (bind);
		eassert (BUFFERP (where));

		if (! NILP (Flocal_variable_p (symbol, where)))
		  {
		    bind->let.value
		      = find_symbol_value (XSYMBOL (symbol), XBUFFER (where));
		    set_internal (symbol, value, where,
				  SET_INTERNAL_THREAD_SWITCH);
		  }
		else
		  {
		    /* SYMBOL no longer being local is fine so long as
		       it stays non-local.  Otherwise, we'll misuse
		       this entry "in the wrong direction" (wtf
		       b8460fc). */
		    bind->kind = SPECPDL_NOP;
		  }
	      }
	      break;
	    default:
	      break;
	    }
	}
    }
}

void specpdl_rewind (union specbinding *pdl, int distance,
		     enum specbind_tag kind_or_above)
{
  specpdl_internal_walk (pdl - distance - 1, 1, distance, kind_or_above);
}

void specpdl_unwind (union specbinding *pdl, int distance,
		     enum specbind_tag kind_or_above)
{
  specpdl_internal_walk (pdl, -1, distance, kind_or_above);
}

static void
backtrace_eval_rewind (int distance)
{
  specpdl_rewind (specpdl_ptr, distance, SPECPDL_UNWIND);
}

static void
backtrace_eval_unwind (int distance)
{
  specpdl_unwind (specpdl_ptr, distance, SPECPDL_UNWIND);
}

DEFUN ("backtrace-eval", Fbacktrace_eval, Sbacktrace_eval, 2, 3, NULL,
       doc: /* Evaluate EXP in the context of some activation frame.
NFRAMES and BASE are as `backtrace-frame'.  */)
     (Lisp_Object exp, Lisp_Object nframes, Lisp_Object base)
{
  CHECK_FIXNAT (nframes);
  union specbinding *pdl = get_backtrace_frame (base, XFIXNAT (nframes));
  specpdl_ref count = SPECPDL_INDEX ();
  ptrdiff_t distance = specpdl_ptr - pdl;
  eassert (distance >= 0);

  if (! backtrace_valid_p (current_thread, pdl))
    error ("Activation frame not found!");

  backtrace_eval_unwind (distance);
  record_unwind_protect_int (backtrace_eval_rewind, distance);

  /* Use eval_form rather than Feval since the main motivation behind
     backtrace-eval is to be able to get/set the value of lexical variables
     from the debugger.  */
  return unbind_to (count, eval_form (exp));
}

DEFUN ("backtrace--locals", Fbacktrace__locals, Sbacktrace__locals, 1, 2, NULL,
       doc: /* Return alist of (variable . value) within a particular frame.
NFRAMES and BASE are as `backtrace-frame'.  */)
  (Lisp_Object nframes, Lisp_Object base)
{
  Lisp_Object result = Qnil;
  CHECK_FIXNAT (nframes);
  union specbinding *exit = get_backtrace_frame (base, XFIXNAT (nframes) - 1),
    /* next'ing decrements pdl ptr, goes back in time.  */
    *enter = backtrace_next (current_thread, exit);
  const ptrdiff_t distance = specpdl_ptr - enter;

  /* Bookend with unwind-rewind to and back from ENTER.  */
  backtrace_eval_unwind (distance);
  for (union specbinding *bind = exit; bind > enter; --bind)
    {
      /* Traverses frame backwards, but also conses backwards
	 resulting in a forwards-ordered union of bindings */
      switch (bind->kind)
        {
        case SPECPDL_LEXICAL_ENVIRONMENT:
          {
            Lisp_Object tail = specpdl_value (bind);
            FOR_EACH_TAIL_SAFE (tail)
              {
                Lisp_Object binding = XCAR (tail);
                if (CONSP (binding))
                  result = Fcons (binding, result);
              }
            CHECK_LIST_END (tail, specpdl_value (bind));
          }
          break;
        case SPECPDL_LET:
        case SPECPDL_LET_BLD:
        case SPECPDL_LET_BLV:
          result = Fcons (Fcons (specpdl_symbol (bind),
				 specpdl_value (bind)),
			  result);
          break;
        default:
          break;
        }
    }
  backtrace_eval_rewind (distance);
  return result;
}

void
mark_specpdl (union specbinding *first, union specbinding *ptr)
{
  for (union specbinding *pdl = first; pdl != ptr; ++pdl)
    {
      switch (pdl->kind)
        {
	case SPECPDL_UNWIND:
	  mark_object (specpdl_arg_addr (pdl));
	  break;
	case SPECPDL_UNWIND_ARRAY:
	  mark_objects (pdl->unwind_array.array, pdl->unwind_array.nelts);
	  break;
	case SPECPDL_UNWIND_EXCURSION:
	  mark_object (&pdl->unwind_excursion.marker);
	  mark_object (&pdl->unwind_excursion.window);
	  break;
	case SPECPDL_BACKTRACE:
	  {
	    ptrdiff_t nargs = backtrace_nargs (pdl);
	    mark_object (backtrace_function_addr (pdl));
	    if (nargs == UNEVALLED)
	      nargs = 1;
	    mark_objects (backtrace_args (pdl), nargs);
	  }
	  break;
#ifdef HAVE_MODULES
        case SPECPDL_MODULE_RUNTIME:
          break;
        case SPECPDL_MODULE_ENVIRONMENT:
          mark_module_environment (pdl->unwind_ptr.arg);
          break;
#endif
	case SPECPDL_LEXICAL_ENVIRONMENT:
	  mark_object (specpdl_value_addr (pdl));
	  break;
	case SPECPDL_LET_BLD:
	case SPECPDL_LET_BLV:
	  mark_object (specpdl_buffer_addr (pdl));
	  FALLTHROUGH;
	case SPECPDL_LET:
	  mark_object (specpdl_symbol_addr (pdl));
	  mark_object (specpdl_value_addr (pdl));
	  break;
	case SPECPDL_UNWIND_PTR:
	  if (pdl->unwind_ptr.mark)
	    pdl->unwind_ptr.mark (pdl->unwind_ptr.arg);
	  break;
	case SPECPDL_UNWIND_INT:
	case SPECPDL_UNWIND_INTMAX:
        case SPECPDL_UNWIND_VOID:
	case SPECPDL_NOP:
	  break;
	default:
	  emacs_abort ();
	  break;
	}
    }
}

void
get_backtrace (Lisp_Object array)
{
  union specbinding *pdl = backtrace_top (current_thread);
  /* Copy the backtrace contents into working memory.  */
  for (ptrdiff_t i = 0, asize = ASIZE (array); i < asize; ++i)
    {
      if (backtrace_valid_p (current_thread, pdl))
	{
	  ASET (array, i, backtrace_function (pdl));
	  pdl = backtrace_next (current_thread, pdl);
	}
      else
	ASET (array, i, Qnil);
    }
}

Lisp_Object backtrace_top_function (void)
{
  union specbinding *pdl = backtrace_top (current_thread);
  return backtrace_valid_p (current_thread, pdl) ? backtrace_function (pdl) : Qnil;
}

void
syms_of_eval (void)
{
  DEFVAR_INT ("max-lisp-eval-depth", max_lisp_eval_depth,
	      doc: /* Maximum function call depth.  */);

  DEFVAR_LISP ("quit-flag", Vquit_flag,
	       doc: /* Non-nil causes `eval' to signal quit.
Takes on the values nil, t, 'kill-emacs, and in the special case of
`while-no-input', the symbol value of 'throw-on-input.  */);
  Vquit_flag = Qnil;

  DEFVAR_LISP ("inhibit-quit", Vinhibit_quit,
	       doc: /* Non-nil belays normal processing of `quit-flag'.
Note `quit-flag' is set by C-g regardless so that a quit is signalled
as soon as `inhibit-quit' becomes nil.  This can be averted by
clearing `quit-flag' before clearing `inhibit-quit'.  */);
  Vinhibit_quit = Qnil;

  DEFSYM (Qsetq, "setq");
  DEFSYM (Qinhibit_quit, "inhibit-quit");
  DEFSYM (Qautoload, "autoload");
  DEFSYM (Qinhibit_debugger, "inhibit-debugger");
  DEFSYM (Qmacro, "macro");

  /* Note that the process handling also uses Qexit, but we don't want
     to staticpro it twice, so we just do it here.  */
  DEFSYM (Qexit, "exit");

  DEFSYM (Qinteractive, "interactive");
  DEFSYM (Qcommandp, "commandp");
  DEFSYM (Qand_rest, "&rest");
  DEFSYM (Qand_optional, "&optional");
  DEFSYM (Qclosure, "closure");
  DEFSYM (QCdocumentation, ":documentation");
  DEFSYM (Qdebug, "debug");
  DEFSYM (Qdebug_early, "debug-early");

  DEFVAR_LISP ("inhibit-debugger", Vinhibit_debugger,
	       doc: /* Non-nil means never enter the debugger.
Normally set while the debugger is already active, to avoid recursive
invocations.  */);
  Vinhibit_debugger = Qnil;

  DEFVAR_LISP ("debug-on-error", Vdebug_on_error,
	       doc: /* Non-nil means enter debugger if an error is signaled.
Does not apply to errors handled by `condition-case' or those
matched by `debug-ignored-errors'.
If the value is a list, an error only means to enter the debugger
if one of its condition symbols appears in the list.
When you evaluate an expression interactively, this variable
is temporarily non-nil if `eval-expression-debug-on-error' is non-nil.
The command `toggle-debug-on-error' toggles this.
See also the variable `debug-on-quit' and `inhibit-debugger'.  */);
  Vdebug_on_error = Qnil;

  DEFVAR_LISP ("debug-ignored-errors", Vdebug_ignored_errors,
    doc: /* List of errors for which the debugger should not be called.
Each element may be a condition-name or a regexp that matches error messages.
If any element applies to a given error, that error skips the debugger
and just returns to top level.
If you invoke Emacs with --debug-init, and want to remove some
elements from the default value of this variable, use `setq' to
change the value of the variable to a new list, rather than `delq'
to remove some errors from the list.
This overrides the variable `debug-on-error'.
It does not apply to errors handled by `condition-case'.  */);
  Vdebug_ignored_errors = Qnil;

  DEFVAR_BOOL ("debug-on-quit", debug_on_quit,
    doc: /* Non-nil means enter debugger if quit is signaled (C-g, for example).
Does not apply if quit is handled by a `condition-case'.  */);
  debug_on_quit = 0;

  DEFVAR_BOOL ("debug-on-next-call", debug_on_next_call,
	       doc: /* Non-nil means enter debugger before next `eval', `apply' or `funcall'.  */);

  DEFVAR_BOOL ("debugger-may-continue", debugger_may_continue,
	       doc: /* Non-nil means debugger may continue execution.
This is nil when the debugger is called under circumstances where it
might not be safe to continue.  */);
  debugger_may_continue = 1;

  DEFVAR_BOOL ("debugger-stack-frame-as-list", debugger_stack_frame_as_list,
	       doc: /* Non-nil means display call stack frames as lists. */);
  debugger_stack_frame_as_list = 0;

  DEFSYM (Qdebugger, "debugger");
  DEFVAR_LISP ("debugger", Vdebugger,
	       doc: /* Function to call to invoke debugger.
If due to frame exit, arguments are `exit' and the value being returned;
 this function's value will be returned instead of that.
If due to error, arguments are `error' and a list of arguments to `signal'.
If due to `apply' or `funcall' entry, one argument, `lambda'.
If due to `eval' entry, one argument, t.
IF the desired entry point of the debugger is higher in the call stack,
it can be specified with the keyword argument `:backtrace-base', whose
format should be the same as the BASE argument of `backtrace-frame'.  */);
  Vdebugger = Qdebug_early;

  DEFVAR_LISP ("signal-hook-function", Vsignal_hook_function,
	       doc: /* If non-nil, this is a function for `signal' to call.
It receives the same arguments that `signal' was given.
The Edebug package uses this to regain control.  */);
  Vsignal_hook_function = Qnil;

  DEFVAR_LISP ("debug-on-signal", Vdebug_on_signal,
	       doc: /* Non-nil means call the debugger regardless of condition handlers.
Note that `debug-on-error', `debug-on-quit' and friends
still determine whether to handle the particular condition.  */);
  Vdebug_on_signal = Qnil;

  DEFVAR_BOOL ("backtrace-on-error-noninteractive",
               backtrace_on_error_noninteractive,
               doc: /* Non-nil means print backtrace on error in batch mode.
If this is nil, errors in batch mode will just print the error
message upon encountering an unhandled error, without showing
the Lisp backtrace.  */);
  backtrace_on_error_noninteractive = true;

  /* The value of num_nonmacro_input_events as of the last time we
   started to enter the debugger.  If we decide to enter the debugger
   again when this is still equal to num_nonmacro_input_events, then we
   know that the debugger itself has an error, and we should just
   signal the error instead of entering an infinite loop of debugger
   invocations.  */
  DEFSYM (Qinternal_when_entered_debugger, "internal-when-entered-debugger");
  DEFVAR_INT ("internal-when-entered-debugger", when_entered_debugger,
              doc: /* The number of keyboard events as of last time `debugger' was called.
Used to avoid infinite loops if the debugger itself has an error.
Don't set this unless you're sure that can't happen.  */);

  DEFVAR_LISP ("internal-make-interpreted-closure-function",
	       Vinternal_make_interpreted_closure_function,
	       doc: /* Function to filter the env when constructing a closure.  */);
  Vinternal_make_interpreted_closure_function = Qnil;

  staticpro (&Vautoload_queue);
  Vautoload_queue = Qnil;

  staticpro (&Vrun_hooks);
  Vrun_hooks = intern_c_string ("run-hooks");

  staticpro (&Qcatch_all_memory_full);
  /* Make sure Qcatch_all_memory_full is a unique object.  We could
     also use something like Fcons (Qnil, Qnil), but json.c treats any
     cons cell as error data, so use an uninterned symbol instead.  */
  Qcatch_all_memory_full
    = Fmake_symbol (build_pure_c_string ("catch-all-memory-full"));

  staticpro (&list_of_t);
  list_of_t = list1 (Qt);

  defsubr (&Sor);
  defsubr (&Sand);
  defsubr (&Sif);
  defsubr (&Scond);
  defsubr (&Sprogn);
  defsubr (&Sprog1);
  defsubr (&Ssetq);
  defsubr (&Squote_);
  defsubr (&Sfunction);
  defsubr (&Sdefault_toplevel_value);
  defsubr (&Sset_default_toplevel_value);
  defsubr (&Sdefvar);
  defsubr (&Sdefvar_1);
  defsubr (&Sdefvaralias);
  DEFSYM (Qdefvaralias, "defvaralias");
  defsubr (&Sdefconst);
  defsubr (&Sdefconst_1);
  defsubr (&Sinternal__define_uninitialized_variable);
  defsubr (&Smake_var_non_special);
  defsubr (&Slet);
  defsubr (&SletX);
  defsubr (&Swhile);
  defsubr (&Sfuncall_with_delayed_message);
  defsubr (&Smacroexpand);
  defsubr (&Scatch);
  defsubr (&Sthrow);
  defsubr (&Sunwind_protect);
  defsubr (&Scondition_case);
  DEFSYM (QCsuccess, ":success");
  defsubr (&Ssignal);
  defsubr (&Scommandp);
  defsubr (&Sautoload);
  defsubr (&Sautoload_do_load);
  defsubr (&Seval);
  defsubr (&Sapply);
  defsubr (&Sfuncall);
  defsubr (&Sfunc_arity);
  defsubr (&Srun_hooks);
  defsubr (&Srun_hook_with_args);
  defsubr (&Srun_hook_with_args_until_success);
  defsubr (&Srun_hook_with_args_until_failure);
  defsubr (&Srun_hook_wrapped);
  defsubr (&Sfetch_bytecode);
  defsubr (&Sbacktrace_debug);
  DEFSYM (QCdebug_on_exit, ":debug-on-exit");
  defsubr (&Smapbacktrace);
  defsubr (&Sbacktrace_frame_internal);
  defsubr (&Sbacktrace_frames_from_thread);
  defsubr (&Sbacktrace_eval);
  defsubr (&Sbacktrace__locals);
  defsubr (&Sspecial_variable_p);
  DEFSYM (Qfunctionp, "functionp");
  defsubr (&Sfunctionp);
}
