#include <alloc.h>
#include "bitset.h"

// Subtraction of pointers is only defined when they both point into
// the same array and a void pointer can't point into any array
// because you can't have an array with void elements.

enum
{
  BLOCK_NBITS = 13,
  BLOCK_NBYTES = (1 << BLOCK_NBITS),
  BLOCK_NWORDS = (BLOCK_NBYTES / word_size),
};

typedef struct block_typemap
{
  bitset bitsets[MEM_TYPE_NTYPES];
} block_typemap;

typedef struct mgc_semispace
{
  void **block_addrs;
  block_typemap *block_typemaps;
  void *alloc_ptr;
  void *scan_ptr;
  size_t nblocks;
  size_t block_words_used;
  size_t current_block;
} mgc_semispace;

static mgc_semispace space0;
static mgc_semispace space1;
static mgc_semispace *space_in_use = &space0;

/* i.e., next word is the forwarded address in to-space */
static const uintptr_t next_fwd_magic = 0xF00DF4CE;
static const uintptr_t term_block_magic = 0xDEADBEEF;

#define TERM_BLOCK(addr) (*(uintptr_t *) addr = term_block_magic)
#define TERM_BLOCK_P(addr) (*(uintptr_t *)addr == term_block_magic)
#define FORWARD_XPNTR_SET(from,to)		\
  do {						\
    *(uintptr_t *) from = next_fwd_magic;	\
    *((uintptr_t *) from + 1) = (uintptr_t) to;	\
  } while (0);
#define FORWARD_XPNTR_GET(addr)			\
  ((*(uintptr_t *)addr == next_fwd_magic)	\
   ? (void *)(*((uintptr_t *)addr + 1))		\
   : NULL)

void *
mgc_fwd_xpntr (const void *addr)
{
  return FORWARD_XPNTR_GET (addr);
}

/* Return new allocation pointer, or NULL for failed realloc().  */
static bool
realloc_semispace (mgc_semispace *space)
{
  void *new_addr;
  void **resized_addrs =
    (void **) realloc (space->block_addrs,
		       (1 + space->nblocks) * sizeof (uintptr_t));
  block_typemap *resized_typemaps =
    (block_typemap *) realloc (space->block_typemaps,
			       (1 + space->nblocks) * sizeof (block_typemap));
  if (resized_addrs
      && resized_typemaps
      && (new_addr = xmalloc (BLOCK_NBYTES))) // laligned, chill.
    {
      space->block_typemaps = resized_typemaps;
      memset (&space->block_typemaps[space->nblocks], 0, sizeof (block_typemap));
      space->block_addrs = resized_addrs;
      space->block_addrs[space->nblocks++] = new_addr;
      return true;
    }
  return false;
}

static
size_t block_of_xpntr (const void *xpntr)
{
  uintptr_t xaddr = (uintptr_t) xpntr;
  for (size_t i = 0; i < space_in_use->nblocks; ++i)
    {
      uintptr_t start = (uintptr_t) space_in_use->block_addrs[i], end;
      INT_ADD_WRAPV (start, BLOCK_NBYTES, &end);
      if (start <= xaddr && xaddr < end)
	return i;
    }
  return BLOCK_NOT_FOUND;
}

static enum Lisp_Type
xpntr_at (const mgc_semispace *space, size_t block, ptrdiff_t word, void **xpntr)
{
  enum Lisp_Type ret = Lisp_Type_Unused0;
  size_t mem_i, modulus;
  block_typemap *map = &space->block_typemaps[block];

  eassert (word < BLOCK_NWORDS - 1); // -1 for term_block_magic

  if (bitset_test (map->bitsets[MEM_TYPE_CONS], (bitset_bindex) word))
    {
      ret = Lisp_Cons;
      modulus = sizeof (struct Lisp_Cons) / word_size;
      mem_i = MEM_TYPE_CONS;
    }
  else if (bitset_test (map->bitsets[MEM_TYPE_STRING], (bitset_bindex) word))
    {
      ret = Lisp_String;
      modulus = sizeof (struct Lisp_String) / word_size;
      mem_i = MEM_TYPE_STRING;
    }
  else if (bitset_test (map->bitsets[MEM_TYPE_SYMBOL], (bitset_bindex) word))
    {
      ret = Lisp_Symbol;
      modulus = sizeof (struct Lisp_Symbol) / word_size;
      mem_i = MEM_TYPE_SYMBOL;
    }
  else if (bitset_test (map->bitsets[MEM_TYPE_FLOAT], (bitset_bindex) word))
    {
      ret = Lisp_Float;
      modulus = sizeof (struct Lisp_Float) / word_size;
      mem_i = MEM_TYPE_FLOAT;
    }
  else if (bitset_test (map->bitsets[MEM_TYPE_VECTORLIKE], (bitset_bindex) word))
    {
      ret = Lisp_Vectorlike;
      modulus = 0;
      mem_i = MEM_TYPE_VECTORLIKE;
    }
  else
    emacs_abort ();

  if (xpntr != NULL)
    {
      bitset_bindex i = word;
      for ((void) i; i >= 0 && bitset_test (map->bitsets[mem_i], i); --i);
      ptrdiff_t presumed_start = word - ((word - (i + 1)) % modulus);
      void *block_addr = space->block_addrs[block];
      INT_ADD_WRAPV ((uintptr_t) block_addr, presumed_start * word_size,
		     (uintptr_t *) xpntr);
    }

  return ret;
}

static size_t
nbytes_of (enum Lisp_Type xpntr_type, const void *xpntr)
{
  size_t result = 0;
  switch (xpntr_type)
    {
    case Lisp_Symbol:
      result = sizeof (struct Lisp_Symbol);
      break;
    case Lisp_String:
      result = sizeof (struct Lisp_String);
      break;
    case Lisp_Vectorlike:
      result = FLEXSIZEOF (struct Lisp_Vector, contents,
			   sizeof (Lisp_Object)
			   * (PSEUDOVECTOR_SIZE_MASK
			      & ((const struct Lisp_Vector *) xpntr)->header.size));
      break;
    case Lisp_Cons:
      result = sizeof (struct Lisp_Cons);
      break;
    case Lisp_Float:
      result = sizeof (struct Lisp_Float);
      break;
    default:
      break;
    }
  if (! result)
    emacs_abort ();
  return result;
}

enum Lisp_Type
mgc_find_xpntr (void *p, void **xpntr)
{
  size_t block = block_of_xpntr (p);
  if (block != BLOCK_NOT_FOUND)
    {
      char *block_start = space_in_use->block_addrs[block];
      ptrdiff_t word = ((char *) p - block_start) / word_size;
      return xpntr_at (space_in_use, block, word, xpntr);
    }
  return Lisp_Type_Unused0;
}

static void
reset_space (mgc_semispace *space)
{
  space->alloc_ptr = NULL;
  space->scan_ptr = NULL;
  space->block_words_used = 0;
  space->current_block = -1;
}

void
mgc_initialize_spaces (void)
{
  reset_space (&space0);
  reset_space (&space1);
}

static void *
next_block (mgc_semispace *space)
{
  bool q_error = false;
  size_t restore_current_block = space->current_block;

  if (space->alloc_ptr)
    TERM_BLOCK (space->alloc_ptr);
  else
    eassert ((int) space->current_block < 0);

  eassert ((int) space->current_block <= (int) space->nblocks);

  if (++space->current_block == space->nblocks)
    q_error = ! realloc_semispace (space);

  if (! q_error)
    {
      space->alloc_ptr = space->block_addrs[space->current_block];
      if (! space->scan_ptr)
	space->scan_ptr = space->alloc_ptr;
      space->block_words_used = 0;
      for (int i = 0; i < MEM_TYPE_NTYPES; ++i)
	{
	  bitset *ptr = &space->block_typemaps[space->current_block].bitsets[i];
	  if (*ptr)
	    bitset_zero (*ptr);
	  else
	    *ptr = bitset_create (BLOCK_NWORDS, BITSET_FIXED);
	}
      return space->alloc_ptr;
    }

  space->current_block = restore_current_block;
  return NULL;
}

/* Before: SPACE->alloc_ptr points to ready-for-use slot X.
           SPACE->block_words_used does not include slot X.
	   SPACE->flat_idx awaits XPNTR_TYPE.

   After: SPACE->block_words_used includes slot X.
          SPACE->alloc_ptr points to next ready-for-use slot Y.
	  SPACE->flat_idx awaits next XPNTR_TYPE.

   Returns slot X, or NULL if failed.
*/

static void *
bump_alloc_ptr (mgc_semispace *space, size_t nbytes, enum Lisp_Type xpntr_type)
{
  void *retval = space->alloc_ptr;
  size_t nwords = nbytes / word_size;
  eassert (nbytes >= 2 * word_size);
  eassert ((int) space->current_block <= (int) space->nblocks);

  if (! retval
      /* -1 so we've room for term_block_magic.  */
      || space->block_words_used + nwords > (BLOCK_NWORDS - 1))
    retval = next_block (space);

  if (retval)
    {
      enum mem_type mem_i;
      switch (xpntr_type)
	{
	case Lisp_Cons:
	  mem_i = MEM_TYPE_CONS;
	  break;
	case Lisp_String:
	  mem_i = MEM_TYPE_STRING;
	  break;
	case Lisp_Symbol:
	  mem_i = MEM_TYPE_SYMBOL;
	  break;
	case Lisp_Float:
	  mem_i = MEM_TYPE_FLOAT;
	  break;
	case Lisp_Vectorlike:
	  mem_i = MEM_TYPE_VECTORLIKE;
	  break;
	default:
	  emacs_abort ();
	  break;
	}
      for (size_t w = 0; w < nwords; ++w)
	bitset_set (space->block_typemaps[space->current_block].bitsets[mem_i],
		    (bitset_bindex) (space->block_words_used + w));
      space->block_words_used += nwords;
      INT_ADD_WRAPV ((uintptr_t) space->alloc_ptr, nbytes,
		     (uintptr_t *) &space->alloc_ptr);
    }
  return retval;
}

void *
mgc_flip_xpntr (void *xpntr, enum Lisp_Type xpntr_type)
{
  mgc_semispace *from = space_in_use,
    *to = (from == &space0) ? &space1 : &space0;
  void *ret = FORWARD_XPNTR_GET (xpntr);
  if (! ret)
    {
      size_t nbytes = nbytes_of (xpntr_type, xpntr);
      ret = bump_alloc_ptr (to, nbytes, xpntr_type);
      memcpy (ret, xpntr, nbytes);
      FORWARD_XPNTR_SET (xpntr, ret);
      eassert (FORWARD_XPNTR_GET (xpntr) == ret);
    }
  return ret;
}

void
mgc_flip_space (void)
{
  mgc_semispace *from = space_in_use,
    *to = (from == &space0) ? &space1 : &space0;
  for (int b = 0; b <= (int) from->current_block; ++b)
    {
      size_t w = 0;
      for (void *xpntr = from->block_addrs[b];
	   (w < space_in_use->block_words_used
	    && xpntr != from->alloc_ptr
	    && ! TERM_BLOCK_P (xpntr));
	   (void) xpntr)
	{
	  enum Lisp_Type xpntr_type = xpntr_at (from, b, w, NULL);
	  eassert (xpntr_type != Lisp_Type_Unused0);
	  size_t bytespan = nbytes_of (xpntr_type, xpntr);
	  if (xpntr_type == Lisp_String && ! FORWARD_XPNTR_GET (xpntr))
	    {
	      /* S goes to dead state.  */
	      struct Lisp_String *s = (struct Lisp_String *) xpntr;
	      if (s->u.s.data != NULL)
		{
		  sdata *data = SDATA_OF_LISP_STRING (s);
		  /* Save length so that sweep_sdata() knows how far
		     to move the hare-tortoise pointers.  */
		  eassert (data->nbytes == STRING_BYTES (s));

		  /* sweep_sdata() needs this for compaction.  */
		  data->string = NULL;

		  /* Invariant of free-list Lisp_String.  */
		  s->u.s.data = NULL;
		}
	    }
	  w += bytespan / word_size;
	  INT_ADD_WRAPV ((uintptr_t) xpntr, bytespan, (uintptr_t *) &xpntr);
	}
    }
  reset_space (from);
  space_in_use = to;
}

DEFUN ("mgc-cons", Fmgc_cons, Smgc_cons, 2, 2, 0,
       doc: /* Create a new cons, give it CAR and CDR as components,
	       and return it.  */)
  (Lisp_Object car, Lisp_Object cdr)
{
  Lisp_Object val;
  size_t nbytes = sizeof (struct Lisp_Cons);
  XSETCONS (val, bump_alloc_ptr (space_in_use, nbytes, Lisp_Cons));
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
  (void) q_clear;
  eassert (nargs >= 0);

  if (nargs > min (MOST_POSITIVE_FIXNUM,
		   (min (PTRDIFF_MAX, SIZE_MAX) - header_size) / word_size))
    memory_full (SIZE_MAX);
  else if (nargs == 0)
    ret = XVECTOR (zero_vector);
  else
    {
      ptrdiff_t nbytes = FLEXSIZEOF (struct Lisp_Vector, contents,
				     nargs * sizeof (Lisp_Object));
      ret = bump_alloc_ptr (space_in_use, nbytes, Lisp_Vectorlike);
      ret->header.size = nargs;
      bytes_since_gc += nbytes;
      vector_cells_consed += nargs;
    }
  return ret;
}

DEFUN ("mgc-make-vector", Fmgc_make_vector, Smgc_make_vector, 2, 2, 0,
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

DEFUN ("mgc-vector", Fmgc_vector, Smgc_vector, 0, MANY, 0,
       doc: /* Return a new vector containing the specified ARGS.
usage: (cvector &rest ARGS)
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
    bump_alloc_ptr (space_in_use, sizeof (struct Lisp_String), Lisp_String);
  if (s)
    {
      ++strings_consed;
      bytes_since_gc += sizeof *s;
    }
  return s;
}

DEFUN ("mgc-make-string", Fmgc_make_string, Smgc_make_string, 2, 3, 0,
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

DEFUN ("mgc-make-symbol", Fmgc_make_symbol, Smgc_make_symbol, 1, 1, 0,
       doc: /* Return an uninterned, unbound symbol whose name is NAME. */)
  (Lisp_Object name)
{
  Lisp_Object val;
  size_t nbytes = sizeof (struct Lisp_Symbol);

  CHECK_STRING (name);
  XSETSYMBOL (val, bump_alloc_ptr (space_in_use, nbytes, Lisp_Symbol));
  init_symbol (val, name);
  bytes_since_gc += nbytes;
  return val;
}

DEFUN ("mgc-make-marker", Fmgc_make_marker, Smgc_make_marker, 0, 0, 0,
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

DEFUN ("memory-protect-now", Fmemory_protect_now, Smemory_protect_now, 0, 0, "",
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

bool
mgc_handle_sigsegv (void *const fault_address)
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

DEFUN ("mgc-make-bool-vector", Fmgc_make_bool_vector, Smgc_make_bool_vector, 2, 2, 0,
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

DEFUN ("mgc-bool-vector", Fmgc_bool_vector, Smgc_bool_vector, 0, MANY, 0,
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

DEFUN ("mgc-counts", Fmgc_counts, Smgc_counts, 0, 0, 0,
       doc: /* Return alist of Lisp types and their current counts.  */)
  (void)
{
  size_t tally[Lisp_Type_Max] = {0};
  for (int b = 0; b <= (int) space_in_use->current_block; ++b)
    {
      size_t w = 0;
      for (void *xpntr = space_in_use->block_addrs[b];
	   (w < space_in_use->block_words_used
	    && xpntr != space_in_use->alloc_ptr
	    && ! TERM_BLOCK_P (xpntr));
	   (void) xpntr)
	{
	  enum Lisp_Type xpntr_type = xpntr_at (space_in_use, b, w, NULL);
	  eassert (xpntr_type != Lisp_Type_Unused0);
	  tally[xpntr_type]++;
	  size_t bytespan = nbytes_of (xpntr_type, xpntr);
	  w += bytespan / word_size;
	  INT_ADD_WRAPV ((uintptr_t) xpntr, bytespan, (uintptr_t *) &xpntr);
	}
    }
  return list5 (Fcons (Fmake_symbol (build_string ("symbol")),
		       make_fixnum (tally[Lisp_Symbol])),
		Fcons (Fmake_symbol (build_string ("string")),
		       make_fixnum (tally[Lisp_String])),
		Fcons (Fmake_symbol (build_string ("vectorlike")),
		       make_fixnum (tally[Lisp_Vectorlike])),
		Fcons (Fmake_symbol (build_string ("cons")),
		       make_fixnum (tally[Lisp_Cons])),
		Fcons (Fmake_symbol (build_string ("float")),
		       make_fixnum (tally[Lisp_Float])));
}

void
syms_of_mgc (void)
{
  DEFVAR_LISP ("memory--protect-p", Vmemory__protect_p,
	       doc: /* How does mprotect work?  */);
  Vmemory__protect_p = Qnil;

  defsubr (&Smgc_cons);
  defsubr (&Smgc_vector);
  defsubr (&Smgc_make_vector);
  defsubr (&Smgc_make_string);
  defsubr (&Smgc_make_bool_vector);
  defsubr (&Smgc_bool_vector);
  defsubr (&Smgc_make_symbol);
  defsubr (&Smgc_make_marker);
  defsubr (&Smgc_counts);
  defsubr (&Smemory_protect_now);
}

bool mgc_xpntr_p (const void *xpntr)
{
  return block_of_xpntr (xpntr) != BLOCK_NOT_FOUND;
}

bool wrong_xpntr_p (const void *xpntr)
{
  mgc_semispace *wrong = (space_in_use == &space0) ? &space1 : &space0;
  uintptr_t xaddr = (uintptr_t) xpntr;
  for (size_t i = 0; i < wrong->nblocks; ++i)
    {
      uintptr_t start = (uintptr_t) wrong->block_addrs[i], end;
      INT_ADD_WRAPV (start, BLOCK_NBYTES, &end);
      if (start <= xaddr && xaddr < end)
	return true;
    }
  return false;
}
