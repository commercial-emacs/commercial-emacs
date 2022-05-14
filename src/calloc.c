#include <alloc.h>

/* struct Lisp_Symbol symbols[BLOCK_NSYMBOLS]; */
// Subtraction of pointers is only defined when they both point into
// the same array and a void pointer can't point into any array
// because you can't have an array with void elements.
typedef struct gc_semispace {
  void **block_addrs;
  void *alloc_ptr;
  void *scan_ptr;
  size_t nblocks;
  size_t words_used;
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

/* Return new allocation pointer, or NULL for failed realloc().  */

static void *
realloc_semispace (gc_semispace *space)
{
  void *new_addr, *resized =
    realloc (space->block_addrs, (1 + space->nblocks) * sizeof (uintptr_t));
  if (resized && (new_addr = xmalloc (BLOCK_NBYTES)))
    {
      space->block_addrs = resized;
      space->block_addrs[space->nblocks++] = new_addr;
      space->alloc_ptr = new_addr;
      space->words_used = 0;
      if (! space->scan_ptr)
	space->scan_ptr = space->alloc_ptr;
      return space->alloc_ptr;
    }
  return NULL;
}

/* Before: SPACE->alloc_ptr points to ready-for-use slot X.
           SPACE->words_used does not include slot X.

   After: SPACE->words_used includes slot X.
          SPACE->alloc_ptr points to next ready-for-use slot Y.

   Returns slot X, or NULL if failed.
*/

static void *
bump_alloc_ptr (gc_semispace *space, size_t nbytes)
{
  void *retval = space->alloc_ptr;
  size_t nwords = nbytes / word_size;
  eassert (nbytes > word_size);

  if (! retval || space->words_used + nwords > BLOCK_NWORDS)
    retval = realloc_semispace (space);

  if (retval)
    {
      space->words_used += nwords;
      INT_ADD_WRAPV ((intptr_t) space->alloc_ptr, nbytes,
		     (intptr_t *) &space->alloc_ptr);
    }
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

static void
restore_vector_allocator (void *ptr)
{
  static_vector_allocator = ptr;
}

static struct Lisp_Vector *
allocate_vector (ptrdiff_t nargs, bool q_clear)
{
  struct Lisp_Vector *ret = NULL;
  eassert (nargs >= 0);

  if (nargs > min (MOST_POSITIVE_FIXNUM,
		   (min (PTRDIFF_MAX, SIZE_MAX) - header_size) / word_size))
    memory_full (SIZE_MAX);
  else if (nargs == 0)
    ret = XVECTOR (zero_vector);
  else
    {
      ptrdiff_t nbytes = header_size + nargs * sizeof (Lisp_Object);
      ret = bump_alloc_ptr (space_in_use, nbytes);
      ret->header.size = nargs;
      bytes_since_gc += nbytes;
      vector_cells_consed += nargs;
    }
  return ret;
}

DEFUN ("cmake-vector", Fcmake_vector, Scmake_vector, 2, 2, 0,
       doc: /* Return a new vector of LENGTH instances of INIT.  */)
  (Lisp_Object length, Lisp_Object init)
{
  specpdl_ref count = SPECPDL_INDEX ();
  CHECK_TYPE (FIXNATP (length) && XFIXNAT (length) <= PTRDIFF_MAX,
	      Qwholenump, length);
  record_unwind_protect_ptr (restore_vector_allocator, static_vector_allocator);
  static_vector_allocator = &allocate_vector;
  return unbind_to (count, initialize_vector (XFIXNAT (length), init));
}

DEFUN ("cvector", Fcvector, Scvector, 0, MANY, 0,
       doc: /* Return a new vector containing the specified ARGS.
ARGS can be empty, yielding the empty vector.  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (restore_vector_allocator, static_vector_allocator);
  static_vector_allocator = &allocate_vector;
  return unbind_to (count, Fvector (nargs, args));
}

static void
restore_string_allocator (void *ptr)
{
  static_string_allocator = ptr;
}

static struct Lisp_String *
allocate_string (void)
{
  struct Lisp_String *s =
    bump_alloc_ptr (space_in_use, sizeof (struct Lisp_String));
  if (s)
    {
      ++strings_consed;
      bytes_since_gc += sizeof *s;
    }
  return s;
}

DEFUN ("cmake-string", Fcmake_string, Scmake_string, 2, 3, 0,
       doc: /* Return a newly created string of LENGTH instances of
INIT, an integer representing a character, e.g., ?x.  The return value
is unibyte unless INIT is not ASCII or MULTIBYTE is non-nil.  */)
  (Lisp_Object length, Lisp_Object init, Lisp_Object multibyte)
{
  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (restore_string_allocator, static_string_allocator);
  static_string_allocator = &allocate_string;
  return unbind_to (count, Fmake_string (length, init, multibyte));
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
  specpdl_ref count = SPECPDL_INDEX ();

  CHECK_FIXNAT (length);
  record_unwind_protect_ptr (restore_vector_allocator, static_vector_allocator);
  static_vector_allocator = &allocate_vector;
  val = unbind_to (count, make_bool_vector(XFIXNAT (length)));
  return bool_vector_fill (val, init);
}

DEFUN ("cbool-vector", Fcbool_vector, Scbool_vector, 0, MANY, 0,
       doc: /* Return a new bool-vector with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (bool-vector &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object val;
  specpdl_ref count = SPECPDL_INDEX ();

  record_unwind_protect_ptr (restore_vector_allocator, static_vector_allocator);
  static_vector_allocator = &allocate_vector;
  val = unbind_to (count, make_bool_vector (nargs));
  for (ptrdiff_t i = 0; i < nargs; i++)
    bool_vector_set (val, i, ! NILP (args[i]));
  return val;
}

void
syms_of_calloc (void)
{
  DEFVAR_LISP ("memory--protect-p", Vmemory__protect_p,
	       doc: /* How does mprotect work?  */);
  Vmemory__protect_p = Qnil;

  defsubr (&Sccons);
  defsubr (&Scvector);
  defsubr (&Scmake_vector);
  defsubr (&Scmake_string);
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
  struct Lisp_Symbol *p = (struct Lisp_Symbol *) space_in_use->block_addrs[0];
  p->u.s.pinned = false;
}
