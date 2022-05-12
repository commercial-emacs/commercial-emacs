#include <alloc.h>

/* struct Lisp_Symbol symbols[BLOCK_NSYMBOLS]; */
// Subtraction of pointers is only defined when they both point into
// the same array and a void pointer can't point into any array
// because you can't have an array with void elements.
typedef struct gc_semispace {
  void *objects;
  void *alloc_ptr;
  void *scan_ptr;
  size_t words_total, words_used;
} gc_semispace;

static gc_semispace space0;
static gc_semispace space1;
static gc_semispace *space_in_use = &space0;

enum
{
  BLOCK_NBITS = 13,
  BLOCK_NBYTES = (1 << BLOCK_NBITS),
  BLOCK_NWORDS = (BLOCK_NBYTES / word_size),
};

static gc_semispace *
realloc_semispace (gc_semispace *space)
{
  /* If realloc() fails, the original block is left un-touched; it is
     not freed or moved. -- MALLOC(3) */
  void *resized = lrealloc ((void *) space->objects,
			    space->words_total * word_size + BLOCK_NBYTES);
  if (resized)
    {
      if (resized != (void *) space->objects)
	{
	  // uh oh
	}
      space->objects = resized;
      space->words_total += BLOCK_NWORDS;
      if (! space->alloc_ptr)
	space->alloc_ptr = space->objects;
      if (! space->scan_ptr)
	space->scan_ptr = space->alloc_ptr;
    }
  return space;
}

static void *
bump_alloc_ptr (gc_semispace *space, size_t nbytes)
{
  void *retval;
  space->words_used += nbytes / word_size;
  if (space->words_used > space->words_total)
    realloc_semispace (space);
  retval = space->alloc_ptr;
  INT_ADD_WRAPV ((intptr_t) space->alloc_ptr, nbytes, (intptr_t *) &space->alloc_ptr);
  return retval;
}

DEFUN ("ccons", Fccons, Sccons, 2, 2, 0,
       doc: /* Create a new cons, give it CAR and CDR as components,
	       and return it.  */)
  (Lisp_Object car, Lisp_Object cdr)
{
  Lisp_Object val;
  size_t nbytes = sizeof (struct Lisp_Cons);
  XSETCONS (val, bump_alloc_ptr (space_in_use, nbytes));
  XSETCAR (val, car);
  XSETCDR (val, cdr);
  bytes_since_gc += nbytes;
  return val;
}

DEFUN ("cvector", Fcvector, Scvector, 0, MANY, 0,
       doc: /* Return a newly created vector with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (vector &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  struct Lisp_Vector *p;
  Lisp_Object val;

  if (nargs > min (MOST_POSITIVE_FIXNUM,
		   (min (PTRDIFF_MAX, SIZE_MAX) - header_size) / word_size))
    memory_full (SIZE_MAX);

  if (nargs == 0)
    val = make_lisp_ptr (XVECTOR (zero_vector), Lisp_Vectorlike);
  else
    {
      ptrdiff_t nbytes = nargs * sizeof (Lisp_Object);
      XSETVECTOR (val, bump_alloc_ptr (space_in_use, nbytes));
      bytes_since_gc += header_size + nbytes;
      vector_cells_consed += nargs;
    }
  p = XVECTOR (val);
  p->header.size = nargs;
  memcpy (p->contents, args, nargs * sizeof *args);

  return val;
}

DEFUN ("cmake-byte-code", Fcmake_byte_code, Scmake_byte_code, 4, MANY, 0,
       doc: /* Create a byte-code object with specified arguments as elements.
The arguments should be the ARGLIST, bytecode-string BYTE-CODE, constant
vector CONSTANTS, maximum stack size DEPTH, (optional) DOCSTRING,
and (optional) INTERACTIVE-SPEC.
The first four arguments are required; at most six have any
significance.
The ARGLIST can be either like the one of `lambda', in which case the arguments
will be dynamically bound before executing the byte code, or it can be an
integer of the form NNNNNNNRMMMMMMM where the 7bit MMMMMMM specifies the
minimum number of arguments, the 7-bit NNNNNNN specifies the maximum number
of arguments (ignoring &rest) and the R bit specifies whether there is a &rest
argument to catch the left-over arguments.  If such an integer is used, the
arguments will not be dynamically bound but will be instead pushed on the
stack before executing the byte-code.
usage: (make-byte-code ARGLIST BYTE-CODE CONSTANTS DEPTH &optional DOCSTRING INTERACTIVE-SPEC &rest ELEMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  if (! ((FIXNUMP (args[COMPILED_ARGLIST])
	  || CONSP (args[COMPILED_ARGLIST])
	  || NILP (args[COMPILED_ARGLIST]))
	 && STRINGP (args[COMPILED_BYTECODE])
	 && !STRING_MULTIBYTE (args[COMPILED_BYTECODE])
	 && VECTORP (args[COMPILED_CONSTANTS])
	 && FIXNATP (args[COMPILED_STACK_DEPTH])))
    error ("Invalid byte-code object");

  pin_string (args[COMPILED_BYTECODE]);  // Bytecode must be immovable.

  /* We used to purecopy everything here, if loadup-pure-table was set.  This worked
     OK for Emacs-23, but with Emacs-24's lexical binding code, it can be
     dangerous, since make-byte-code is used during execution to build
     closures, so any closure built during the preload phase would end up
     copied into pure space, including its free variables, which is sometimes
     just wasteful and other times plainly wrong (e.g. those free vars may want
     to be setcar'd).  */
  Lisp_Object val = Fvector (nargs, args);
  XSETPVECTYPE (XVECTOR (val), PVEC_COMPILED);
  return val;
}

DEFUN ("cmake-symbol", Fcmake_symbol, Scmake_symbol, 1, 1, 0,
       doc: /* Return an uninterned, unbound symbol whose name is NAME. */)
  (Lisp_Object name)
{
  Lisp_Object val;
  size_t nbytes = sizeof (struct Lisp_Symbol);

  CHECK_STRING (name);
  XSETSYMBOL (val, bump_alloc_ptr (space_in_use, nbytes));
  init_symbol (val, name);
  bytes_since_gc += nbytes;
  return val;
}

DEFUN ("cmake-marker", Fcmake_marker, Scmake_marker, 0, 0, 0,
       doc: /* Return a newly allocated marker which does not point at any place.  */)
  (void)
{
  struct Lisp_Marker *p = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Marker,
						       PVEC_MARKER);
  p->buffer = 0;
  p->bytepos = 0;
  p->charpos = 0;
  p->next = NULL;
  p->insertion_type = 0;
  p->need_adjustment = 0;
  return make_lisp_ptr (p, Lisp_Vectorlike);
}

DEFUN ("cgarbage-collect", Fcgarbage_collect, Scgarbage_collect, 0, 0, "",
       doc: /* Reclaim storage for Lisp objects no longer needed.
Garbage collection happens automatically if you cons more than
`gc-cons-threshold' bytes of Lisp data since previous garbage collection.
`garbage-collect' normally returns a list with info on amount of space in use,
where each entry has the form (NAME SIZE USED FREE), where:
- NAME is a symbol describing the kind of objects this entry represents,
- SIZE is the number of bytes used by each one,
- USED is the number of those objects that were found live in the heap,
- FREE is the number of those objects that are not live but that Emacs
  keeps around for future allocations (maybe because it does not know how
  to return them to the OS).

However, if there was overflow in pure space, and Emacs was dumped
using the 'unexec' method, `garbage-collect' returns nil, because
real GC can't be done.

Note that calling this function does not guarantee that absolutely all
unreachable objects will be garbage-collected.  Emacs uses a
mark-and-sweep garbage collector, but is conservative when it comes to
collecting objects in some circumstances.

For further details, see Info node `(elisp)Garbage Collection'.  */)
  (void)
{
  garbage_collect ();
  return Qnil;
}

DEFUN ("cmemory-protect-now", Fcmemory_protect_now, Scmemory_protect_now, 0, 0, "",
       doc: /* Call mprotect().  */)
  (void)
{
  if (CONSP (Vmemory__protect_p))
    {
      int pagesize = getpagesize ();
      char *page_start =
	(char *) ((uintptr_t) (XLP (Vmemory__protect_p)) & ~(pagesize - 1));
      mprotect (page_start, pagesize, PROT_READ);
    }
  return Vmemory__protect_p;
}

DEFUN ("cmemory-use-counts", Fcmemory_use_counts, Scmemory_use_counts, 0, 0, 0,
       doc: /* Return a list of counters that measure how much consing there has been.
Each of these counters increments for a certain kind of object.
The counters wrap around from the largest positive integer to zero.
Garbage collection does not decrease them.
The elements of the value are as follows:
  (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS INTERVALS STRINGS)
All are in units of 1 = one object consed
except for VECTOR-CELLS and STRING-CHARS, which count the total length of
objects consed.
Frames, windows, buffers, and subprocesses count as vectors
  (but the contents of a buffer's text do not count here).  */)
  (void)
{
  return  list (make_int (cons_cells_consed),
		make_int (floats_consed),
		make_int (vector_cells_consed),
		make_int (symbols_consed),
		make_int (string_chars_consed),
		make_int (intervals_consed),
		make_int (strings_consed));
}

bool
gc_handle_sigsegv (void *const fault_address)
{
  if (! NILP (Vmemory__protect_p))
    {
      Vmemory__protect_p = Qnil;
      if (mem_find (fault_address))
	{
	  int pagesize = getpagesize ();
	  char *page_start = (char *) ((uintptr_t) fault_address & ~(pagesize - 1));
	  mprotect (page_start, pagesize, PROT_READ | PROT_WRITE);
	  return true;
	}
    }
  return false;
}

DEFUN ("cmake-bool-vector", Fcmake_bool_vector, Scmake_bool_vector, 2, 2, 0,
       doc: /* Return a new bool-vector of length LENGTH, using INIT for each element.
LENGTH must be a number.  INIT matters only in whether it is t or nil.  */)
  (Lisp_Object length, Lisp_Object init)
{
  Lisp_Object val;

  CHECK_FIXNAT (length);
  val = make_uninit_bool_vector (XFIXNAT (length));
  return bool_vector_fill (val, init);
}

DEFUN ("cbool-vector", Fcbool_vector, Scbool_vector, 0, MANY, 0,
       doc: /* Return a new bool-vector with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (bool-vector &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;
  Lisp_Object vector;

  vector = make_uninit_bool_vector (nargs);
  for (i = 0; i < nargs; i++)
    bool_vector_set (vector, i, ! NILP (args[i]));

  return vector;
}

void
syms_of_calloc (void)
{
  DEFVAR_LISP ("memory--protect-p", Vmemory__protect_p,
	       doc: /* How does mprotect work?  */);
  Vmemory__protect_p = Qnil;

  defsubr (&Sccons);
  defsubr (&Scvector);
  defsubr (&Scmake_byte_code);
  defsubr (&Scmake_bool_vector);
  defsubr (&Scbool_vector);
  defsubr (&Scmake_symbol);
  defsubr (&Scmake_marker);
  defsubr (&Scgarbage_collect);
  defsubr (&Scmemory_protect_now);
  defsubr (&Scmemory_use_counts);
}

void test_me (void)
{
  (void) space1;
  bump_alloc_ptr (space_in_use, sizeof (struct Lisp_Symbol));
  bump_alloc_ptr (space_in_use, sizeof (struct Lisp_Float));
  struct Lisp_Symbol *p = (struct Lisp_Symbol *) space_in_use->objects;
  p->u.s.pinned = false;
}
