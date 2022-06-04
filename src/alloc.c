/* Allocator and garbage collector.

Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2022 Free Software
Foundation, Inc.

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

/* The core gc task is marking Lisp objects in so-called vectorlikes,
   an unfortunate umbrella term for Emacs's various structs (buffers,
   windows, frames, etc.). "Vectorlikes" is meant to capture their
   role as containers of both Lisp_Objects (the "vector" part) and
   non-Lisp fields (the "like" part).  Not content with just one
   confusing moniker, Emacs also refers to vectorlikes as
   "pseudovectors".

   Pervasive in the GC code is casting the vectorlike as a `struct
   Lisp_Vector *`, then iterating over its N Lisp objects, say to mark
   them from reclamation, where N is masked off from a specialized
   header (PSEUDOVECTOR_SIZE_MASK).

   The vectorlike is an obfuscator, yes, but also a mnemonic to remind
   us that it must be so cast-able, that is, led by the word-sized
   header, then immediately followed by a variable number of
   Lisp_Objects.  Non-lisp fields must be placed at the end, and
   are cleaned up outside the gc in free_by_pvtype().

   The confabulation of Lisp_String and `struct sdata`.

   Lisp_Strings are doled from `struct string_block`s, and hold no
   actual data.

   Storage for actual data is doled from `struct sblock`s in units of
   `struct sdata`.  Strings larger than LARGE_STRING_THRESH, get their
   own sblock.

   A back-pointer from sdata points to its parent Lisp_String.  The
   Lisp_String, sadly, does not point to its sdata, but rather its
   sdata's DATA member field.  This necessitates the obfuscatory macro
   SDATA_OF_LISP_STRING() which performs an offset calculation to
   recover the whole sdata.  Sadly, again, SDATA_OF_LISP_STRING() is
   easily confused with SDATA() and SSDATA(), the latter two taking
   Lisp_Object (not `struct Lisp_String`) as the argument and
   returning its sdata's DATA member field (not the whole sdata).
*/

#include "alloc.h"

/* MALLOC_SIZE_NEAR (N) is a good number to pass to malloc when
   allocating a block of memory with size close to N bytes.
   For best results N should be a power of 2.

   When calculating how much memory to allocate, GNU malloc (SIZE)
   adds sizeof (size_t) to SIZE for internal overhead, and then rounds
   up to a multiple of MALLOC_ALIGNMENT.  Emacs can improve
   performance a bit on GNU platforms by arranging for the resulting
   size to be a power of two.  This heuristic is good for glibc 2.26
   (2017) and later, and does not affect correctness on other
   platforms.  */

#define MALLOC_SIZE_NEAR(n) \
  (ROUNDUP (max (n, sizeof (size_t)), MALLOC_ALIGNMENT) - sizeof (size_t))
#ifdef __i386
enum { MALLOC_ALIGNMENT = 16 };
#else
enum { MALLOC_ALIGNMENT = max (2 * sizeof (size_t), alignof (long double)) };
#endif

static bool gc_inhibited;
struct Lisp_String *(*static_string_allocator) (void);
struct Lisp_Vector *(*static_vector_allocator) (ptrdiff_t len, bool q_clear);
INTERVAL (*static_interval_allocator) (void);

#ifdef HAVE_PDUMPER
/* Number of finalizers run: used to loop over GC until we stop
   generating garbage.  */
int number_finalizers_run;
#endif

/* Exposed to lisp.h so that maybe_garbage_collect() can inline.  */

EMACS_INT bytes_since_gc;
EMACS_INT bytes_between_gc;
Lisp_Object Vmemory_full;
bool gc_in_progress;

/* Last recorded live and free-list counts.  */
static struct
{
  size_t total_conses, total_free_conses;
  size_t total_symbols, total_free_symbols;
  size_t total_strings, total_free_strings;
  size_t total_string_bytes;
  size_t total_vectors, total_vector_slots, total_free_vector_slots;
  size_t total_floats, total_free_floats;
  size_t total_intervals, total_free_intervals;
  size_t total_buffers;
} gcstat;

enum _GL_ATTRIBUTE_PACKED sdata_type
{
  Sdata_Unibyte = -1,
  Sdata_Pure = -2,
  Sdata_Pinned = -3,
};

/* Conservative stack scanning (mark_maybe_pointer) needs to
   trace an arbitrary address back to its respective memory block.

   To make these searches efficient, new blocks are stored in a global
   red-black tree which is "fixed" after every insertion or deletion
   such that:

   1. Every node is either red or black.
   2. Every leaf is black.
   3. If a node is red, then both its children are black.
   4. Every simple path from a node to a descendant leaf contains
      the same number of black nodes.
   5. The root is always black.

   These invariants balance the tree so that its height can be no
   greater than 2 log(N+1), where N is the number of internal nodes.
   Searches, insertions and deletions are done in O(log N).
  */

struct mem_node
{
  /* Children of this node.  These pointers are never NULL.  When there
     is no child, the value is CMEM_NIL, which points to a dummy node.  */
  struct mem_node *left, *right;

  /* The parent of this node.  In the root node, this is NULL.  */
  struct mem_node *parent;

  /* Start and end of allocated region.  */
  void *start, *end;

  /* Node color.  */
  enum {MEM_BLACK, MEM_RED} color;

  /* Memory type.  */
  enum mem_type type;
};

struct mem_node mem_z;
#define MEM_NIL &mem_z

/* True if malloc (N) is known to return storage suitably aligned for
   Lisp objects whenever N is a multiple of LISP_ALIGNMENT, or,
   equivalently when alignof (max_align_t) is a multiple of
   LISP_ALIGNMENT.  This works even for buggy platforms like MinGW
   circa 2020, where alignof (max_align_t) is 16 even though the
   malloc alignment is only 8, and where Emacs still works because it
   never does anything that requires an alignment of 16.  */
enum { MALLOC_IS_LISP_ALIGNED = alignof (max_align_t) % LISP_ALIGNMENT == 0 };

#define MALLOC_PROBE(size)			\
  do {						\
    if (profiler_memory_running)		\
      malloc_probe (size);			\
  } while (0)

/* Initialize it to a nonzero value to force it into data space
   (rather than bss space).  That way unexec will remap it into text
   space (pure), on some systems.  We have not implemented the
   remapping on more recent systems because this is less important
   nowadays than in the days of small memories and timesharing.  */

EMACS_INT pure[(PURESIZE + sizeof (EMACS_INT) - 1) / sizeof (EMACS_INT)] = {1,};
#define PUREBEG (char *) pure

/* Pointer to the pure area, and its size.  */

static char *purebeg;
static ptrdiff_t pure_size;

/* Number of bytes of pure storage used before pure storage overflowed.
   If this is non-zero, this implies that an overflow occurred.  */

static ptrdiff_t pure_bytes_used_before_overflow;

/* Index in pure at which next pure Lisp object will be allocated..  */

static ptrdiff_t pure_bytes_used_lisp;

/* Number of bytes allocated for non-Lisp objects in pure storage.  */

static ptrdiff_t pure_bytes_used_non_lisp;

/* If nonzero, this is a warning delivered by malloc and not yet
   displayed.  */

const char *pending_malloc_warning;

/* Pointer sanity only on request.  FIXME: Code depending on
   SUSPICIOUS_OBJECT_CHECKING is obsolete; remove it entirely.  */
#ifdef ENABLE_CHECKING
#define SUSPICIOUS_OBJECT_CHECKING 1
#endif

#ifdef SUSPICIOUS_OBJECT_CHECKING
struct suspicious_free_record
{
  void *suspicious_object;
  void *backtrace[128];
};
static void *suspicious_objects[32];
static int suspicious_object_index;
struct suspicious_free_record suspicious_free_history[64] EXTERNALLY_VISIBLE;
static int suspicious_free_history_index;
/* Find the first currently-monitored suspicious pointer in range
   [begin,end) or NULL if no such pointer exists.  */
static void *find_suspicious_object_in_range (void *begin, void *end);
static void detect_suspicious_free (void *ptr);
#else
# define find_suspicious_object_in_range(begin, end) ((void *) NULL)
# define detect_suspicious_free(ptr) ((void) 0)
#endif

static void unchain_finalizer (struct Lisp_Finalizer *);
static void mark_terminals (void);
static void gc_sweep (void);
static Lisp_Object make_pure_vector (ptrdiff_t);
static void mark_buffer (struct buffer *);

static void sweep_sdata (void);
extern Lisp_Object which_symbols (Lisp_Object, EMACS_INT) EXTERNALLY_VISIBLE;

static bool vectorlike_marked_p (const union vectorlike_header *);
static void set_vectorlike_marked (union vectorlike_header *);
static bool vector_marked_p (const struct Lisp_Vector *);
static void set_vector_marked (struct Lisp_Vector *);
static bool interval_marked_p (INTERVAL);
static void set_interval_marked (INTERVAL);
static void mark_interval_tree (INTERVAL *i);

static bool
deadp (Lisp_Object x)
{
  return EQ (x, dead_object ());
}

/* Root of the tree describing allocated Lisp memory.  */

static struct mem_node *mem_root;

/* Lowest and highest known address in the heap.  */

static void *min_heap_address, *max_heap_address;

/* Sentinel node of the tree.  */

static struct mem_node *mem_insert (void *, void *, enum mem_type);
static void mem_insert_fixup (struct mem_node *);
static void mem_rotate_left (struct mem_node *);
static void mem_rotate_right (struct mem_node *);
static void mem_delete (struct mem_node *);
static void mem_delete_fixup (struct mem_node *);

/* Addresses of staticpro'd variables.  Initialize it to a nonzero
   value if we might unexec; otherwise some compilers put it into
   BSS.  */

Lisp_Object const *staticvec[NSTATICS];

/* Index of next unused slot in staticvec.  */

int staticidx;

static void *pure_alloc (size_t, int);

/* Return PTR rounded up to the next multiple of ALIGNMENT.  */

static void *
pointer_align (void *ptr, int alignment)
{
  return (void *) ROUNDUP ((uintptr_t) ptr, alignment);
}

/* Extract the lisp struct payload of A.  */

static ATTRIBUTE_NO_SANITIZE_UNDEFINED void *
XPNTR (Lisp_Object a)
{
  return (SYMBOLP (a)
	  ? (char *) lispsym + (XLI (a) - LISP_WORD_TAG (Lisp_Symbol))
	  : (char *) XLP (a) - (XLI (a) & ~VALMASK));
}

static void
XFLOAT_INIT (Lisp_Object f, double n)
{
  XFLOAT (f)->u.data = n;
}

/* Head of a circularly-linked list of extant finalizers. */
struct Lisp_Finalizer finalizers;

/* Head of a circularly-linked list of finalizers that must be invoked
   because we deemed them unreachable.  This list must be global, and
   not a local inside garbage_collect, in case we GC again while
   running finalizers.  */
struct Lisp_Finalizer doomed_finalizers;


#if defined SIGDANGER || (!defined SYSTEM_MALLOC && !defined HYBRID_MALLOC)

/* Function malloc calls this if it finds we are near exhausting storage.  */

void
malloc_warning (const char *str)
{
  pending_malloc_warning = str;
}

#endif

/* Display an already-pending malloc warning.  */

void
display_malloc_warning (void)
{
  call3 (intern ("display-warning"),
	 intern ("alloc"),
	 build_string (pending_malloc_warning),
	 intern (":emergency"));
  pending_malloc_warning = 0;
}

/* True if a malloc-returned pointer P is suitably aligned for SIZE,
   where Lisp object alignment may be needed if SIZE is a multiple of
   LISP_ALIGNMENT.  */

static bool
laligned (void *p, size_t size)
{
  return (MALLOC_IS_LISP_ALIGNED
	  || (intptr_t) p % LISP_ALIGNMENT == 0
	  || size % LISP_ALIGNMENT != 0);
}

/* Like malloc but check for no memory and block interrupt input.  */

void *
xmalloc (size_t size)
{
  void *val = lmalloc (size, false);
  if (! val)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}

/* Like the above, but zeroes out the memory just allocated.  */

void *
xzalloc (size_t size)
{
  void *val = lmalloc (size, true);
  if (! val)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}

/* Like realloc but check for no memory and block interrupt input.  */

void *
xrealloc (void *block, size_t size)
{
  /* We can but won't assume realloc (NULL, size) works.  */
  void *val = block ? lrealloc (block, size) : lmalloc (size, false);
  if (! val)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}

/* Like free() but check pdumper_object_p().  */

void
xfree (void *block)
{
  if (block && ! pdumper_object_p (block))
    free (block);
}

/* Other parts of Emacs pass large int values to allocator functions
   expecting ptrdiff_t.  This is portable in practice, but check it to
   be safe.  */
verify (INT_MAX <= PTRDIFF_MAX);

/* Allocate an array of NITEMS items, each of size ITEM_SIZE.
   Signal an error on memory exhaustion, and block interrupt input.  */

void *
xnmalloc (ptrdiff_t nitems, ptrdiff_t item_size)
{
  eassert (0 <= nitems && 0 < item_size);
  ptrdiff_t nbytes;
  if (INT_MULTIPLY_WRAPV (nitems, item_size, &nbytes) || SIZE_MAX < nbytes)
    memory_full (SIZE_MAX);
  return xmalloc (nbytes);
}

/* Reallocate an array PA to make it of NITEMS items, each of size ITEM_SIZE.
   Signal an error on memory exhaustion, and block interrupt input.  */

void *
xnrealloc (void *pa, ptrdiff_t nitems, ptrdiff_t item_size)
{
  eassert (0 <= nitems && 0 < item_size);
  ptrdiff_t nbytes;
  if (INT_MULTIPLY_WRAPV (nitems, item_size, &nbytes) || SIZE_MAX < nbytes)
    memory_full (SIZE_MAX);
  return xrealloc (pa, nbytes);
}

/* Grow PA, which points to an array of *NITEMS items, and return the
   location of the reallocated array, updating *NITEMS to reflect its
   new size.  The new array will contain at least NITEMS_INCR_MIN more
   items, but will not contain more than NITEMS_MAX items total.
   ITEM_SIZE is the size of each item, in bytes.

   ITEM_SIZE and NITEMS_INCR_MIN must be positive.  *NITEMS must be
   nonnegative.  If NITEMS_MAX is -1, it is treated as if it were
   infinity.

   If PA is null, then allocate a new array instead of reallocating
   the old one.

   Block interrupt input as needed.  If memory exhaustion occurs, set
   *NITEMS to zero if PA is null, and signal an error (i.e., do not
   return).

   Thus, to grow an array A without saving its old contents, do
   { xfree (A); A = NULL; A = xpalloc (NULL, &AITEMS, ...); }.
   The A = NULL avoids a dangling pointer if xpalloc exhausts memory
   and signals an error, and later this code is reexecuted and
   attempts to free A.  */

void *
xpalloc (void *pa, ptrdiff_t *nitems, ptrdiff_t nitems_incr_min,
	 ptrdiff_t nitems_max, ptrdiff_t item_size)
{
  ptrdiff_t n0 = *nitems;
  eassume (0 < item_size && 0 < nitems_incr_min && 0 <= n0 && -1 <= nitems_max);

  /* The approximate size to use for initial small allocation
     requests.  This is the largest "small" request for the GNU C
     library malloc.  */
  enum { DEFAULT_MXFAST = 64 * sizeof (size_t) / 4 };

  /* If the array is tiny, grow it to about (but no greater than)
     DEFAULT_MXFAST bytes.  Otherwise, grow it by about 50%.
     Adjust the growth according to three constraints: NITEMS_INCR_MIN,
     NITEMS_MAX, and what the C language can represent safely.  */

  ptrdiff_t n, nbytes;
  if (INT_ADD_WRAPV (n0, n0 >> 1, &n))
    n = PTRDIFF_MAX;
  if (0 <= nitems_max && nitems_max < n)
    n = nitems_max;

  ptrdiff_t adjusted_nbytes
    = ((INT_MULTIPLY_WRAPV (n, item_size, &nbytes) || SIZE_MAX < nbytes)
       ? min (PTRDIFF_MAX, SIZE_MAX)
       : nbytes < DEFAULT_MXFAST ? DEFAULT_MXFAST : 0);
  if (adjusted_nbytes)
    {
      n = adjusted_nbytes / item_size;
      nbytes = adjusted_nbytes - adjusted_nbytes % item_size;
    }

  if (! pa)
    *nitems = 0;
  if (n - n0 < nitems_incr_min
      && (INT_ADD_WRAPV (n0, nitems_incr_min, &n)
	  || (0 <= nitems_max && nitems_max < n)
	  || INT_MULTIPLY_WRAPV (n, item_size, &nbytes)))
    memory_full (SIZE_MAX);
  pa = xrealloc (pa, nbytes);
  *nitems = n;
  return pa;
}

/* Like strdup(), but uses xmalloc().  */

char *
xstrdup (const char *s)
{
  ptrdiff_t size;
  eassert (s);
  size = strlen (s) + 1;
  return memcpy (xmalloc (size), s, size);
}

/* Like above, but duplicates Lisp string to C string.  */

char *
xlispstrdup (Lisp_Object string)
{
  ptrdiff_t size = SBYTES (string) + 1;
  return memcpy (xmalloc (size), SSDATA (string), size);
}

/* Assign to *PTR a copy of STRING, freeing any storage *PTR formerly
   pointed to.  If STRING is null, assign it without copying anything.
   Allocate before freeing, to avoid a dangling pointer if allocation
   fails.  */

void
dupstring (char **ptr, char const *string)
{
  char *old = *ptr;
  *ptr = string ? xstrdup (string) : 0;
  xfree (old);
}

/* Like putenv, but (1) use the equivalent of xmalloc and (2) the
   argument is a const pointer.  */

void
xputenv (char const *string)
{
  if (putenv ((char *) string) != 0)
    memory_full (0);
}

/* Return a newly allocated memory block of SIZE bytes, remembering
   to free it when unwinding.  */
void *
record_xmalloc (size_t size)
{
  void *p = xmalloc (size);
  record_unwind_protect_ptr (xfree, p);
  return p;
}

/* Like malloc but used for allocating Lisp data.  NBYTES is the
   number of bytes to allocate, TYPE describes the intended use of the
   allocated memory block (for strings, for conses, ...).  */

#if ! USE_LSB_TAG
void *lisp_malloc_loser EXTERNALLY_VISIBLE;
#endif

static void *
lisp_malloc (size_t nbytes, bool q_clear, enum mem_type type)
{
  register void *val;
  val = lmalloc (nbytes, q_clear);

#if ! USE_LSB_TAG
  /* If the memory just allocated cannot be addressed thru a Lisp
     object's pointer, and it needs to be, that's equivalent to
     running out of memory.  */
  if (val && type != MEM_TYPE_NON_LISP)
    {
      Lisp_Object tem;
      XSETCONS (tem, (char *) val + nbytes - 1);
      if ((char *) XCONS (tem) != (char *) val + nbytes - 1)
	{
	  lisp_malloc_loser = val;
	  free (val);
	  val = 0;
	}
    }
#endif

  if (val && type != MEM_TYPE_NON_LISP)
    mem_insert (val, (char *) val + nbytes, type);

  if (! val)
    memory_full (nbytes);
  MALLOC_PROBE (nbytes);
  return val;
}

/* Free BLOCK.  This must be called to free memory allocated with a
   call to lisp_malloc.  */

static void
lisp_free (void *block)
{
  if (block && ! pdumper_object_p (block))
    {
      free (block);
      mem_delete (mem_find (block));
    }
}

/* The allocator malloc's blocks of BLOCK_ALIGN bytes.

   Structs for blocks are statically defined, so calculate (at compile-time)
   how many of each type will fit into its respective block.

   For simpler blocks consisting only of an object array and a next pointer,
   the numerator need only subtract off the size of the next pointer.

   For blocks with an additional gcmarkbits array, say float_block, we
   solve for y in the inequality:

     BLOCK_ALIGN > y * sizeof (Lisp_Float) + sizeof (bits_word) * (y /
     BITS_PER_BITS_WORD + 1) + sizeof (struct float_block *)
*/

enum
{
  /* Arbitrarily set in 2012 in commit 0dd6d66.  */
  GC_DEFAULT_THRESHOLD = (1 << 17) * word_size,

  BLOCK_NBITS = 10,
  BLOCK_ALIGN = 1 << BLOCK_NBITS,
  BLOCK_NBYTES = BLOCK_ALIGN - sizeof (uintptr_t), // subtract next ptr
  BLOCK_NINTERVALS = (BLOCK_NBYTES) / sizeof (struct interval),
  BLOCK_NSTRINGS = (BLOCK_NBYTES) / sizeof (struct Lisp_String),
  BLOCK_NSYMBOLS = (BLOCK_NBYTES) / sizeof (struct Lisp_Symbol),
  BLOCK_NFLOATS = ((BITS_PER_BITS_WORD / sizeof (bits_word))
		   * (BLOCK_NBYTES - sizeof (bits_word))
		   / ((BITS_PER_BITS_WORD / sizeof (bits_word))
		      * sizeof (struct Lisp_Float)
		      + 1)),
  BLOCK_NCONS = ((BITS_PER_BITS_WORD / sizeof (bits_word))
		 * (BLOCK_NBYTES - sizeof (bits_word))
		 / ((BITS_PER_BITS_WORD / sizeof (bits_word))
		    * sizeof (struct Lisp_Cons)
		    + 1)),

  /* Size `struct vector_block` */
  VBLOCK_ALIGN = (1 << PSEUDOVECTOR_SIZE_BITS),
  VBLOCK_NBYTES = VBLOCK_ALIGN - sizeof (uintptr_t), // subtract next ptr
  LISP_VECTOR_MIN = header_size + sizeof (Lisp_Object), // vector of one
  LARGE_VECTOR_THRESH = (VBLOCK_NBYTES >> 1) - word_size,

  /* Amazingly, free list per vector word-length.  */
  VBLOCK_NFREE_LISTS = 1 + (VBLOCK_NBYTES - LISP_VECTOR_MIN) / word_size,

  SBLOCK_NBITS = 13,
  SBLOCK_NBYTES = MALLOC_SIZE_NEAR(1 << SBLOCK_NBITS),
  LARGE_STRING_THRESH = (SBLOCK_NBYTES >> 3),
};
// should someone decide to muck with VBLOCK_ALIGN...
verify (VBLOCK_ALIGN % LISP_ALIGNMENT == 0);
verify (VBLOCK_ALIGN <= (1 << PSEUDOVECTOR_SIZE_BITS));

#if (defined HAVE_ALIGNED_ALLOC			\
     || (defined HYBRID_MALLOC			\
	 ? defined HAVE_POSIX_MEMALIGN		\
	 : !defined SYSTEM_MALLOC))
# define USE_ALIGNED_ALLOC 1
#elif !defined HYBRID_MALLOC && defined HAVE_POSIX_MEMALIGN
# define USE_ALIGNED_ALLOC 1
# define aligned_alloc my_aligned_alloc /* Avoid collision with lisp.h.  */
static void *
aligned_alloc (size_t alignment, size_t size)
{
  /* Permit suspect assumption that ALIGNMENT is either BLOCK_ALIGN or
     LISP_ALIGNMENT since we'll rarely get here.  */
  eassume (alignment == BLOCK_ALIGN
	   || (! MALLOC_IS_LISP_ALIGNED && alignment == LISP_ALIGNMENT));

  /* Verify POSIX invariant ALIGNMENT = (2^x) * sizeof (void *).  */
  verify (BLOCK_ALIGN % sizeof (void *) == 0
	  && POWER_OF_2 (BLOCK_ALIGN / sizeof (void *)));
  verify (MALLOC_IS_LISP_ALIGNED
	  || (LISP_ALIGNMENT % sizeof (void *) == 0
	      && POWER_OF_2 (LISP_ALIGNMENT / sizeof (void *))));

  void *p;
  return posix_memalign (&p, alignment, size) == 0 ? p : 0;
}
#endif

/* Request at least SIZE bytes from malloc, ensuring returned
   pointer is Lisp-aligned.

   If T is an enum Lisp_Type and L = make_lisp_ptr (P, T), then
   code seeking P such that XPNTR (L) == P and XTYPE (L) == T, or,
   in less formal terms, seeking to allocate a Lisp object, should
   call lmalloc().

   Q_CLEAR uses calloc() instead of malloc().
   */

void *
lmalloc (size_t size, bool q_clear)
{
  /* xrealloc() relies on lmalloc() returning non-NULL even for SIZE
     == 0.  So, if ! MALLOC_0_IS_NONNULL, must avoid malloc'ing 0.  */
  size_t adjsize = MALLOC_0_IS_NONNULL ? size : max (size, LISP_ALIGNMENT);

  /* Prefer malloc() but if ! MALLOC_IS_LISP_ALIGNED, an exceptional
     case, then prefer aligned_alloc(), provided SIZE is a multiple of
     ALIGNMENT which aligned_alloc() requires.  */
#ifdef USE_ALIGNED_ALLOC
  if (! MALLOC_IS_LISP_ALIGNED)
    {
      if (adjsize % LISP_ALIGNMENT == 0)
	{
	  void *p = aligned_alloc (LISP_ALIGNMENT, adjsize);
	  if (q_clear && p && adjsize)
	    memclear (p, adjsize);
	  return p;
	}
      else
	{
	  /* Otherwise resign ourselves to loop that may never
	     terminate.  */
	}
    }
#endif
  void *p = NULL;
  for (;;)
    {
      p = q_clear ? calloc (1, adjsize) : malloc (adjsize);
      if (! p || MALLOC_IS_LISP_ALIGNED || laligned (p, adjsize))
	break;
      free (p);
      adjsize = max (adjsize, adjsize + LISP_ALIGNMENT);
    }
  eassert (! p || laligned (p, adjsize));
  return p;
}

void *
lrealloc (void *p, size_t size)
{
  /* xrealloc() relies on lrealloc() returning non-NULL even for size
     == 0.  MALLOC_0_IS_NONNULL does not mean REALLOC_0_IS_NONNULL.  */
  size_t adjsize = max (size, LISP_ALIGNMENT);
  void *newp = p;
  for (;;)
    {
      newp = realloc (newp, adjsize);
      if (! adjsize || ! newp || MALLOC_IS_LISP_ALIGNED || laligned (newp, adjsize))
	break;
      adjsize = max (adjsize, adjsize + LISP_ALIGNMENT);
    }
  eassert (! newp || laligned (newp, adjsize));
  return newp;
}

/* An aligned block of memory.  */
struct ablock
{
  union
  {
    char payload[BLOCK_NBYTES];
    struct ablock *next_free;
  } x;

  /* ABASE is the aligned base of the ablocks.  It is overloaded to
     hold a virtual "busy" field that counts twice the number of used
     ablock values in the parent ablocks, plus one if the real base of
     the parent ablocks is ABASE (if the "busy" field is even, the
     word before the first ablock holds a pointer to the real base).
     The first ablock has a "busy" ABASE, and the others have an
     ordinary pointer ABASE.  To tell the difference, the code assumes
     that pointers, when cast to uintptr_t, are at least 2 *
     ABLOCKS_NBLOCKS + 1.  */
  struct ablocks *abase;
};
verify (sizeof (struct ablock) % BLOCK_ALIGN == 0);

#define ABLOCKS_NBLOCKS (1 << 4)

struct ablocks
{
  struct ablock blocks[ABLOCKS_NBLOCKS];
};
verify (sizeof (struct ablocks) % BLOCK_ALIGN == 0);

#define ABLOCK_ABASE(block) \
  (((uintptr_t) (block)->abase) <= (1 + 2 * ABLOCKS_NBLOCKS)	\
   ? (struct ablocks *) (block)					\
   : (block)->abase)

/* Virtual "busy" field.  */
#define ABLOCKS_BUSY(a_base) ((a_base)->blocks[0].abase)

/* Pointer to the (not necessarily aligned) malloc block.  */
#ifdef USE_ALIGNED_ALLOC
#define ABLOCKS_BASE(abase) (abase)
#else
#define ABLOCKS_BASE(abase) \
  (1 & (intptr_t) ABLOCKS_BUSY (abase) ? abase : ((void **) (abase))[-1])
#endif

static struct ablock *free_ablock;

/* Allocate an aligned block of NBYTES.  */
static void *
lisp_align_malloc (size_t nbytes, enum mem_type type)
{
  void *base, *val;
  struct ablocks *abase;

  eassert (nbytes < BLOCK_ALIGN);

  if (! free_ablock)
    {
      int i;
      bool aligned;

#ifdef USE_ALIGNED_ALLOC
      abase = base = aligned_alloc (BLOCK_ALIGN, sizeof (struct ablocks));
#else
      base = malloc (sizeof (struct ablocks));
      abase = pointer_align (base, BLOCK_ALIGN);
#endif

      if (! base)
	memory_full (sizeof (struct ablocks));

      aligned = (base == abase);
      if (! aligned)
	((void **) abase)[-1] = base;

#if ! USE_LSB_TAG
      /* If the memory just allocated cannot be addressed thru a Lisp
	 object's pointer, and it needs to be, that's equivalent to
	 running out of memory.  */
      if (type != MEM_TYPE_NON_LISP)
	{
	  Lisp_Object tem;
	  char *end = (char *) base + sizeof (struct ablocks) - 1;
	  XSETCONS (tem, end);
	  if ((char *) XCONS (tem) != end)
	    {
	      lisp_malloc_loser = base;
	      free (base);
	      memory_full (SIZE_MAX);
	    }
	}
#endif

      /* Initialize the blocks and put them on the free list.
	 If BASE was not properly aligned, we can't use the last block.  */
      for (i = 0; i < (aligned ? ABLOCKS_NBLOCKS : ABLOCKS_NBLOCKS - 1); ++i)
	{
	  abase->blocks[i].abase = abase;
	  abase->blocks[i].x.next_free = free_ablock;
	  free_ablock = &abase->blocks[i];
	}
      intptr_t ialigned = aligned;
      ABLOCKS_BUSY (abase) = (struct ablocks *) ialigned;

      eassert ((uintptr_t) abase % BLOCK_ALIGN == 0);
      eassert (ABLOCK_ABASE (&abase->blocks[3]) == abase); /* 3 is arbitrary */
      eassert (ABLOCK_ABASE (&abase->blocks[0]) == abase);
      eassert (ABLOCKS_BASE (abase) == base);
      eassert ((intptr_t) ABLOCKS_BUSY (abase) == aligned);
    }

  abase = ABLOCK_ABASE (free_ablock);
  ABLOCKS_BUSY (abase)
    = (struct ablocks *) (2 + (intptr_t) ABLOCKS_BUSY (abase));
  val = free_ablock;
  free_ablock = free_ablock->x.next_free;

  if (type != MEM_TYPE_NON_LISP)
    mem_insert (val, (char *) val + nbytes, type);

  MALLOC_PROBE (nbytes);

  eassert (0 == ((uintptr_t) val) % BLOCK_ALIGN);
  return val;
}

static void
lisp_align_free (void *block)
{
  struct ablock *ablock = block;
  struct ablocks *abase = ABLOCK_ABASE (ablock);

  mem_delete (mem_find (block));

  /* Put on free list.  */
  ablock->x.next_free = free_ablock;
  free_ablock = ablock;
  /* Update busy count.  */
  intptr_t busy = (intptr_t) ABLOCKS_BUSY (abase) - 2;
  eassume (0 <= busy && busy <= 2 * ABLOCKS_NBLOCKS - 1);
  ABLOCKS_BUSY (abase) = (struct ablocks *) busy;

  if (busy < 2)
    { /* All the blocks are free.  */
      int i = 0;
      bool aligned = busy;
      struct ablock **tem = &free_ablock;
      struct ablock *atop = &abase->blocks[aligned ? ABLOCKS_NBLOCKS : ABLOCKS_NBLOCKS - 1];

      while (*tem)
	{
	  if (*tem >= (struct ablock *) abase && *tem < atop)
	    {
	      i++;
	      *tem = (*tem)->x.next_free;
	    }
	  else
	    tem = &(*tem)->x.next_free;
	}
      eassert ((aligned & 1) == aligned);
      eassert (i == (aligned ? ABLOCKS_NBLOCKS : ABLOCKS_NBLOCKS - 1));
#ifdef USE_POSIX_MEMALIGN
      eassert ((uintptr_t) ABLOCKS_BASE (abase) % BLOCK_ALIGN == 0);
#endif
      free (ABLOCKS_BASE (abase));
    }
}

struct interval_block
{
  /* Data first, to preserve alignment.  */
  struct interval intervals[BLOCK_NINTERVALS];
  struct interval_block *next;
};

static struct interval_block *interval_block;
static int interval_block_index = BLOCK_NINTERVALS;
static INTERVAL interval_free_list;

static INTERVAL
allocate_interval (void)
{
  INTERVAL val;

  if (interval_free_list)
    {
      val = interval_free_list;
      interval_free_list = INTERVAL_PARENT (interval_free_list);
    }
  else
    {
      if (interval_block_index == BLOCK_NINTERVALS)
	{
	  struct interval_block *newi
	    = lisp_malloc (sizeof *newi, false, MEM_TYPE_NON_LISP);

	  newi->next = interval_block;
	  interval_block = newi;
	  interval_block_index = 0;
	}
      val = &interval_block->intervals[interval_block_index++];
    }

  bytes_since_gc += sizeof (struct interval);
  intervals_consed++;
  RESET_INTERVAL (val);
  val->gcmarkbit = false;
  return val;
}

static void
mark_interval_tree_functor (INTERVAL *i, void *dummy)
{
  eassert (! interval_marked_p (*i));
  set_interval_marked (*i);
  mark_object (&(*i)->plist);
}

static void
mark_interval_tree_functor_mgc (INTERVAL *i, void *dummy)
{
  eassert (! interval_marked_p (*i));
  if (mgc_xpntr_p (*i) && ! mgc_fwd_xpntr (*i))
    {
      *i = mgc_flip_xpntr (*i, Space_Interval);
      if (INTERVAL_HAS_OBJECT (*i))
	mark_object (&(*i)->up.obj);
      else if (INTERVAL_HAS_PARENT (*i))
	mark_interval_tree (&(*i)->up.interval);
      mark_object (&(*i)->plist);
    }
}

/* Mark the interval tree rooted in I.  */

static void
mark_interval_tree (INTERVAL *i)
{
  if (! *i)
    return;
  if (mgc_xpntr_p (*i))
    {
      /* INTERVAL *root = i; */
      /* for ( ; ! NULL_PARENT (*root); root = &(*root)->up.interval); */
      /* eassert (mgc_xpntr_p (*root)); */
      traverse_intervals_noorder (i, mark_interval_tree_functor_mgc, NULL);
    }
  else if (! interval_marked_p (*i))
    traverse_intervals_noorder (i, mark_interval_tree_functor, NULL);
}

struct sblock
{
  struct sblock *next;

  /* Points to next available sdata in DATA.
     Points past end of sblock if none available.  */
  sdata *next_free;
  sdata data[FLEXIBLE_ARRAY_MEMBER];
};

struct string_block
{
  /* Data first, to preserve alignment.  */
  struct Lisp_String strings[BLOCK_NSTRINGS];
  struct string_block *next;
};

/* The NEXT pointers point in the direction of oldest_sblock to
   current_sblock.  We always allocate from current_sblock.  */

#define NEXT_FREE_LISP_STRING(S) ((S)->u.next)

static struct sblock *oldest_sblock, *current_sblock;
static struct sblock *large_sblocks;
static struct string_block *string_blocks;
static struct Lisp_String *string_free_list;

#ifdef GC_CHECK_STRING_OVERRUN

/* Check for overrun in string data blocks by appending a small
   "cookie" after each allocated string data block, and check for the
   presence of this cookie during GC.  */
# define GC_STRING_OVERRUN_COOKIE_SIZE ROUNDUP (4, alignof (sdata))
static char const string_overrun_cookie[GC_STRING_OVERRUN_COOKIE_SIZE] =
  { '\xde', '\xad', '\xbe', '\xef', /* Perhaps some zeros here.  */ };

#else
# define GC_STRING_OVERRUN_COOKIE_SIZE 0
#endif

/* Return the size of an sdata structure large enough to hold N bytes
   of string data.  This counts the sdata structure, the N bytes, a
   terminating NUL byte, and alignment padding.  */

static ptrdiff_t
sdata_size (ptrdiff_t n)
{
  /* Reserve space for the nbytes union member even when N + 1 is less
     than the size of that member.  */
  ptrdiff_t unaligned_size = max (FLEXSIZEOF (struct sdata, data, 0) + n + 1,
				  sizeof (sdata));
  int sdata_align = max (FLEXALIGNOF (sdata), alignof (sdata));
  return (unaligned_size + sdata_align - 1) & ~(sdata_align - 1);
}

/* Extra bytes to allocate for each string.  */
#define GC_STRING_EXTRA GC_STRING_OVERRUN_COOKIE_SIZE

/* Exact bound on the number of bytes in a string, not counting the
   terminating null.  A string cannot contain more bytes than
   STRING_BYTES_BOUND, nor can it be so long that the size_t
   arithmetic in allocate_sdata would overflow while it is
   calculating a value to be passed to malloc.  */
static ptrdiff_t const STRING_BYTES_MAX =
  min (STRING_BYTES_BOUND,
       ((SIZE_MAX
	 - GC_STRING_EXTRA
	 - FLEXSIZEOF (struct sblock, data, 0)
	 - FLEXSIZEOF (struct sdata, data, 0))
	& ~(sizeof (EMACS_INT) - 1)));

static void
init_strings (void)
{
  empty_unibyte_string = make_pure_string ("", 0, 0, 0);
  staticpro (&empty_unibyte_string);
  empty_multibyte_string = make_pure_string ("", 0, 0, 1);
  staticpro (&empty_multibyte_string);
}

#ifdef GC_CHECK_STRING_BYTES

static int check_string_bytes_count;

/* Like macro STRING_BYTES, but with debugging check.  Can be
   called during GC, so pay attention to the mark bit.  */

ptrdiff_t
string_bytes (struct Lisp_String *s)
{
  ptrdiff_t nbytes =
    (s->u.s.size_byte < 0 ? s->u.s.size & ~ARRAY_MARK_FLAG : s->u.s.size_byte);

  if (! PURE_P (s) && ! pdumper_object_p (s) && s->u.s.data
      && nbytes != SDATA_OF_LISP_STRING (s)->nbytes)
    emacs_abort ();
  return nbytes;
}

/* Check validity of Lisp strings' string_bytes member in B.  */

static void
check_sblock (struct sblock *b)
{
  for (sdata *from = b->data, *end = b->next_free; from < end; )
    {
      ptrdiff_t nbytes = sdata_size (from->string
				     ? string_bytes (from->string)
				     : from->nbytes);
      from = (sdata *) ((char *) from + nbytes + GC_STRING_EXTRA);
    }
}

/* Check validity of Lisp strings' string_bytes member.  ALL_P
   means check all strings, otherwise check only most
   recently allocated strings.  Used for hunting a bug.  */

static void
check_string_bytes (bool all_p)
{
  if (all_p)
    {
      struct sblock *b;

      for (b = large_sblocks; b; b = b->next)
	{
	  struct Lisp_String *s = b->data[0].string;
	  if (s)
	    string_bytes (s);
	}

      for (b = oldest_sblock; b; b = b->next)
	check_sblock (b);
    }
  else if (current_sblock)
    check_sblock (current_sblock);
}

#else /* not GC_CHECK_STRING_BYTES */

#define check_string_bytes(all) ((void) 0)

#endif /* GC_CHECK_STRING_BYTES */

#ifdef GC_CHECK_STRING_FREE_LIST

/* Walk through the string free list looking for bogus next pointers.
   This may catch buffer overrun from a previous string.  */

static void
check_string_free_list (void)
{
  struct Lisp_String *s;

  /* Pop a Lisp_String off the free list.  */
  s = string_free_list;
  while (s != NULL)
    {
      if ((uintptr_t) s < BLOCK_ALIGN)
	emacs_abort ();
      s = NEXT_FREE_LISP_STRING (s);
    }
}
#else
#define check_string_free_list()
#endif

/* Return a new Lisp_String.  */

static struct Lisp_String *
allocate_string (void)
{
  struct Lisp_String *s;

  /* Our normal scheme: chunks begin life on a newly allocated block,
     then get reclaimed as links in the free list.

     For strings, however, chunks begin life on the free list.

     It's largely a bureacratic distinction since in either scheme, we
     only allocate new blocks when all the free list is spoken for.  */
  if (string_free_list == NULL)
    {
      struct string_block *b = lisp_malloc (sizeof *b, false, MEM_TYPE_STRING);
      b->next = string_blocks;
      string_blocks = b;

      for (int i = BLOCK_NSTRINGS - 1; i >= 0; --i)
	{
	  s = b->strings + i;
	  s->u.s.data = NULL; /* invariant for free-list strings */
	  NEXT_FREE_LISP_STRING (s) = string_free_list;
	  string_free_list = s;
	}
    }

  check_string_free_list ();

  /* Pop a Lisp_String off the free list.  */
  s = string_free_list;
  string_free_list = NEXT_FREE_LISP_STRING (s);

  ++strings_consed;
  bytes_since_gc += sizeof *s;

#ifdef GC_CHECK_STRING_BYTES
  if (! noninteractive)
    {
      if (++check_string_bytes_count == 200)
	{
	  check_string_bytes_count = 0;
	  check_string_bytes (1);
	}
      else
	check_string_bytes (0);
    }
#endif /* GC_CHECK_STRING_BYTES */

  return s;
}

/* Populate S with the next free sdata in CURRENT_SBLOCK.

   Large strings get their own sblock in LARGE_SBLOCKS.
*/

static void
allocate_sdata (struct Lisp_String *s,
		EMACS_INT nchars, EMACS_INT nbytes,
		bool immovable)
{
  sdata *the_data;
  struct sblock *b;
  ptrdiff_t sdata_nbytes;

  eassert (nbytes >= nchars);

  if (nbytes > STRING_BYTES_MAX)
    error ("Requested %ld bytes exceeds %ld", nbytes, STRING_BYTES_MAX);

  sdata_nbytes = sdata_size (nbytes);

  if (nbytes > LARGE_STRING_THRESH || immovable)
    {
      size_t size = FLEXSIZEOF (struct sblock, data, sdata_nbytes);
      b = lisp_malloc (size + GC_STRING_EXTRA, false, MEM_TYPE_NON_LISP);
      the_data = b->data;
      b->next = large_sblocks;
      b->next_free = the_data;
      large_sblocks = b;
    }
  else
    {
      b = current_sblock;

      if (b == NULL
	  || ((SBLOCK_NBYTES - GC_STRING_EXTRA) <
	      ((char *) b->next_free - (char *) b + sdata_nbytes)))
	{
	  /* Not enough room in the current sblock.  */
	  b = lisp_malloc (SBLOCK_NBYTES, false, MEM_TYPE_NON_LISP);
	  the_data = b->data;
	  b->next = NULL;
	  b->next_free = the_data;

	  if (current_sblock)
	    current_sblock->next = b;
	  else
	    oldest_sblock = b;
	  current_sblock = b;
	}

      the_data = b->next_free;
    }

  the_data->string = s;
  b->next_free = (sdata *) ((char *) the_data + sdata_nbytes + GC_STRING_EXTRA);
  eassert ((uintptr_t) b->next_free % alignof (sdata) == 0);

  the_data->nbytes = nbytes;
  s->u.s.data = the_data->data;
  s->u.s.size = nchars;
  s->u.s.size_byte = nbytes;
  s->u.s.data[nbytes] = '\0'; /* NBYTES is exclusive of the NUL terminator. */
#ifdef GC_CHECK_STRING_OVERRUN
  memcpy ((char *) the_data + sdata_nbytes, string_overrun_cookie,
	  GC_STRING_OVERRUN_COOKIE_SIZE);
#endif

  bytes_since_gc += sdata_nbytes;
}

/* Reallocate multibyte STRING data when a single character is replaced.
   The character is at byte offset CIDX_BYTE in the string.
   The character being replaced is CLEN bytes long,
   and the character that will replace it is NEW_CLEN bytes long.
   Return the address where the caller should store the new character.  */

unsigned char *
resize_string_data (Lisp_Object string, ptrdiff_t cidx_byte,
		    int clen, int new_clen)
{
  eassume (STRING_MULTIBYTE (string));
  sdata *old_sdata = SDATA_OF_LISP_STRING (XSTRING (string));
  ptrdiff_t nchars = SCHARS (string);
  ptrdiff_t nbytes = SBYTES (string);
  ptrdiff_t new_nbytes = nbytes + (new_clen - clen);
  unsigned char *data = SDATA (string);
  unsigned char *new_charaddr;

  if (sdata_size (nbytes) == sdata_size (new_nbytes))
    {
      /* No need to reallocate, as the size change falls within the
	 alignment slop.  */
      XSTRING (string)->u.s.size_byte = new_nbytes;
      old_sdata->nbytes = new_nbytes;
      new_charaddr = data + cidx_byte;
      memmove (new_charaddr + new_clen, new_charaddr + clen,
	       nbytes - (cidx_byte + (clen - 1)));
    }
  else
    {
      allocate_sdata (XSTRING (string), nchars, new_nbytes, false);
      unsigned char *new_data = SDATA (string);
      new_charaddr = new_data + cidx_byte;
      memcpy (new_charaddr + new_clen, data + cidx_byte + clen,
	      nbytes - (cidx_byte + clen));
      memcpy (new_data, data, cidx_byte);

      /* Mark old string data as free by setting its string back-pointer
	 to null, and record the size of the data in it.  */
      old_sdata->nbytes = nbytes;
      old_sdata->string = NULL;
    }

  clear_string_char_byte_cache ();

  return new_charaddr;
}

static void
sweep_strings (void)
{
  struct string_block *live_blocks = NULL;

  gcstat.total_string_bytes
    = gcstat.total_strings
    = gcstat.total_free_strings
    = 0;

  string_free_list = NULL;

  for (struct string_block *next, *b = string_blocks; b != NULL; b = next)
    {
      int i, nfree = 0;
      struct Lisp_String *restore_free_list = string_free_list;

      next = b->next; /* B might not exist later, so store NEXT now.  */

      for (i = 0; i < BLOCK_NSTRINGS; ++i)
	{
	  struct Lisp_String *s = b->strings + i;
	  if (s->u.s.data != NULL) /* means S is live but is it marked?  */
	    {
	      if (XSTRING_MARKED_P (s))
		{
		  /* S shall remain in live state.  */
		  XUNMARK_STRING (s);

		  /* Do not use string_(set|get)_intervals here.  */
		  s->u.s.intervals = balance_intervals (s->u.s.intervals);

		  gcstat.total_strings++;
		  gcstat.total_string_bytes += STRING_BYTES (s);
		}
	      else
		{
		  /* S goes to dead state.  */
		  sdata *data = SDATA_OF_LISP_STRING (s);

		  /* Save length so that sweep_sdata() knows how far
		     to move the hare-tortoise pointers.  */
		  eassert (data->nbytes == STRING_BYTES (s));

		  /* sweep_sdata() needs this for compaction.  */
		  data->string = NULL;

		  /* Invariant of free-list Lisp_Strings.  */
		  s->u.s.data = NULL;

		  /* Put on free list.  */
		  NEXT_FREE_LISP_STRING (s) = string_free_list;
		  string_free_list = s;
		  ++nfree;
		}
	    }
	  else /* s->u.s.data == NULL */
	    {
	      /* S inexplicably not already on free list.  */
	      NEXT_FREE_LISP_STRING (s) = string_free_list;
	      string_free_list = s;
	      ++nfree;
	    }
	} /* for each Lisp_String in block B.  */

      if (/* B contains only free-list entries...  */
	  nfree >= BLOCK_NSTRINGS
	  /* ... and B not among first two such reclaimed blocks.  */
	  && gcstat.total_free_strings > BLOCK_NSTRINGS)
	{
	  /* Harvest B back to OS.  */
	  lisp_free (b);
	  string_free_list = restore_free_list;
	}
      else
	{
	  gcstat.total_free_strings += nfree;
	  b->next = live_blocks;
	  live_blocks = b;
	}
    } /* for each string block B.  */

  check_string_free_list ();

  string_blocks = live_blocks;
  sweep_sdata ();

  check_string_free_list ();
}

static void
sweep_sdata (void)
{
  /* Simple sweep of large sblocks.  Side effect: reverses list.  */
  struct sblock *swept_large_sblocks = NULL;
  for (struct sblock *next, *b = large_sblocks; b != NULL; b = next)
    {
      next = b->next;

      if (b->data[0].string == NULL)
	lisp_free (b);
      else
	{
	  b->next = swept_large_sblocks;
	  swept_large_sblocks = b;
	}
    }
  large_sblocks = swept_large_sblocks;

  /* Less simple compaction of non-large sblocks.

     TB is the "to block", or the prevailing memmove destination sblock.
     B is the "from block", or the current memmove source sblock.

     The TO sdata tortoise lags with TB.
     The FROM sdata hare iterates over current B.

     We memmove FROM to TO for all B in the sblock list
     (oldest_sblock...current_sblock), and therein lies the
     compaction.
  */
  struct sblock *tb = oldest_sblock;
  if (tb)
    {
      sdata *end_tb = (sdata *) ((char *) tb + SBLOCK_NBYTES);
      sdata *to = tb->data;

      for (struct sblock *b = tb; b != NULL; b = b->next)
	{
	  eassert ((char *) b->next_free <= (char *) b + SBLOCK_NBYTES);
	  for (sdata *next_from, *end_from = b->next_free, *from = b->data;
	       from < end_from;
	       from = next_from)
	    {
	      struct Lisp_String *s = from->string;
	      const ptrdiff_t nbytes = from->nbytes;
	      const ptrdiff_t step = sdata_size (nbytes) + GC_STRING_EXTRA;
	      eassert (!s || ! XSTRING_MARKED_P (s));
	      eassert (nbytes <= LARGE_STRING_THRESH);

#ifdef GC_CHECK_STRING_BYTES
	      /* Check that the string size recorded in the string is the
		 same as the one recorded in the sdata structure.  */
	      if (s && string_bytes (s) != from->nbytes)
		emacs_abort ();
#endif /* GC_CHECK_STRING_BYTES */

	      /* Compute NEXT_FROM now before FROM can mutate.  */
	      next_from = (sdata *) ((char *) from + step);

#ifdef GC_CHECK_STRING_OVERRUN
	      if (memcmp (string_overrun_cookie,
			  (char *) next_from - GC_STRING_OVERRUN_COOKIE_SIZE,
			  GC_STRING_OVERRUN_COOKIE_SIZE))
		emacs_abort ();
#endif

	      if (s != NULL) /* a live string to be compacted */
		{
		  sdata *next_to = (sdata *) ((char *) to + step);
		  if (next_to > end_tb)
		    {
		      /* TB is full, proceed with the next sblock.  */
		      tb->next_free = to;
		      tb = tb->next;
		      end_tb = (sdata *) ((char *) tb + SBLOCK_NBYTES);
		      to = tb->data;
		      next_to = (sdata *) ((char *) to + step);
		    }

		  if (from != to)
		    {
		      eassert (tb != b || to < from);
		      memmove (to, from, step);
		      to->string->u.s.data = to->data;
		    }

		  to = next_to;
		}
	    }
	}

      /* Any sblocks following TB can be free'd.  */
      for (struct sblock *b = tb->next; b != NULL; )
	{
	  struct sblock *next = b->next;
	  lisp_free (b);
	  b = next;
	}

      tb->next_free = to;
      tb->next = NULL;
    }
  current_sblock = tb;
}

static Lisp_Object new_lisp_string (EMACS_INT, EMACS_INT, bool);

DEFUN ("make-string", Fmake_string, Smake_string, 2, 3, 0,
       doc: /* Make a string.
Return a string of LENGTH instances of INIT, which should
be a numeric character code, e.g., ?x.
The string is unibyte unless INIT is not ASCII or MULTIBYTE is non-nil.
*/)
  (Lisp_Object length, Lisp_Object init, Lisp_Object multibyte)
{
  Lisp_Object val;
  int init_char;
  CHECK_FIXNAT (length);
  CHECK_CHARACTER (init);
  init_char = XFIXNAT (init);

  if (ASCII_CHAR_P (init_char) && NILP (multibyte))
    {
      EMACS_INT nbytes = XFIXNUM (length);
      val = new_lisp_string (nbytes, nbytes, false);
      memset (SDATA (val), init_char, nbytes);
      SDATA (val)[nbytes] = 0;
    }
  else
    {
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      ptrdiff_t len = CHAR_STRING (init_char, str);
      EMACS_INT nbytes, string_len = XFIXNUM (length);

      if (INT_MULTIPLY_WRAPV (len, string_len, &nbytes))
	error ("Product of %ld * %ld overflows", len, string_len);

      val = new_lisp_string (string_len, nbytes, true);
      unsigned char *beg = SDATA (val), *end = beg + nbytes;
      for (unsigned char *p = beg; p < end; p += len)
	{
	  /* First time we just copy STR to the data of VAL.  */
	  if (p == beg)
	    memcpy (p, str, len);
	  else
	    {
	      /* Next time we copy largest possible chunk from
		 initialized to uninitialized part of VAL.  */
	      len = min (p - beg, end - p);
	      memcpy (p, beg, len);
	    }
	}
    }
  return val;
}

/* Fill A with 1 bits if INIT is non-nil, and with 0 bits otherwise.
   Return A.  */

Lisp_Object
bool_vector_fill (Lisp_Object a, Lisp_Object init)
{
  EMACS_INT nbits = bool_vector_size (a);
  if (0 < nbits)
    {
      unsigned char *data = bool_vector_uchar_data (a);
      int pattern = NILP (init) ? 0 : (1 << BOOL_VECTOR_BITS_PER_CHAR) - 1;
      ptrdiff_t nbytes = bool_vector_bytes (nbits);
      int last_mask = ~ (~0u << ((nbits - 1) % BOOL_VECTOR_BITS_PER_CHAR + 1));
      memset (data, pattern, nbytes - 1);
      data[nbytes - 1] = pattern & last_mask;
    }
  return a;
}

/* Return a newly allocated, uninitialized bool vector of size NBITS.  */

Lisp_Object
make_bool_vector (EMACS_INT nbits)
{
  Lisp_Object val;
  EMACS_INT words = bool_vector_words (nbits);
  EMACS_INT word_bytes = words * sizeof (bits_word);
  EMACS_INT needed_elements = ((bool_header_size - header_size + word_bytes
				+ word_size - 1)
			       / word_size);
  if (PTRDIFF_MAX < needed_elements)
    memory_full (SIZE_MAX);
  struct Lisp_Bool_Vector *p
    = (struct Lisp_Bool_Vector *) static_vector_allocator (needed_elements, false);
  XSETVECTOR (val, p);
  XSETPVECTYPESIZE (XVECTOR (val), PVEC_BOOL_VECTOR, 0, 0);
  p->size = nbits;

  /* Clear padding at the end.  */
  if (words)
    p->data[words - 1] = 0;

  return val;
}

DEFUN ("make-bool-vector", Fmake_bool_vector, Smake_bool_vector, 2, 2, 0,
       doc: /* Return a new bool-vector of length LENGTH, using INIT for each element.
LENGTH must be a number.  INIT matters only in whether it is t or nil.  */)
  (Lisp_Object length, Lisp_Object init)
{
  Lisp_Object val;

  CHECK_FIXNAT (length);
  val = make_bool_vector (XFIXNAT (length));
  return bool_vector_fill (val, init);
}

DEFUN ("bool-vector", Fbool_vector, Sbool_vector, 0, MANY, 0,
       doc: /* Return a new bool-vector with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (bool-vector &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;
  Lisp_Object vector;

  vector = make_bool_vector (nargs);
  for (i = 0; i < nargs; i++)
    bool_vector_set (vector, i, ! NILP (args[i]));

  return vector;
}

/* Make a string from NBYTES bytes at CONTENTS, and compute the number
   of characters from the contents.  This string may be unibyte or
   multibyte, depending on the contents.  */

Lisp_Object
make_string (const char *contents, ptrdiff_t nbytes)
{
  register Lisp_Object val;
  ptrdiff_t nchars, multibyte_nbytes;

  parse_str_as_multibyte ((const unsigned char *) contents, nbytes,
			  &nchars, &multibyte_nbytes);
  if (nbytes == nchars || nbytes != multibyte_nbytes)
    /* CONTENTS contains no multibyte sequences or contains an invalid
       multibyte sequence.  We must make unibyte string.  */
    val = make_unibyte_string (contents, nbytes);
  else
    val = make_multibyte_string (contents, nchars, nbytes);
  return val;
}

/* Make a unibyte string from LENGTH bytes at CONTENTS.

   If CONTENTS is NULL, leave string uninitialized.  */

Lisp_Object
make_unibyte_string (const char *contents, ptrdiff_t length)
{
  register Lisp_Object val;
  val = new_lisp_string (length, length, false);
  if (contents)
    memcpy (SDATA (val), contents, length);
  return val;
}

/* If NCHARS is -1 we attempt calculating NCHARS.

   If CONTENTS is NULL, leave string uninitialized.  */

Lisp_Object
make_multibyte_string (const char *contents, ptrdiff_t nchars, ptrdiff_t nbytes)
{
  Lisp_Object val = Qnil;
  if (contents && nchars < 0)
    {
      nchars = multibyte_chars_in_text
	((const unsigned char *) contents, nbytes);
      eassert (nbytes >= nchars);
    }
  if (nchars >= 0)
    {
      val = new_lisp_string (nchars, nbytes, true);
      if (contents)
	memcpy (SDATA (val), contents, nbytes);
    }
  return val;
}

Lisp_Object
new_lisp_string (EMACS_INT nchars, EMACS_INT nbytes, bool multibyte)
{
  Lisp_Object string;
  struct Lisp_String *s;

  eassume (nchars >= 0 && nbytes >= nchars);
  if (nbytes == 0)
    return multibyte ? empty_multibyte_string : empty_unibyte_string;

  s = static_string_allocator ();
  s->u.s.intervals = NULL;
  allocate_sdata (s, nchars, nbytes, false);
  XSETSTRING (string, s);
  string_chars_consed += nbytes;
  if (! multibyte)
    STRING_SET_UNIBYTE (string);
  return string;
}

/* Print arguments to BUF according to a FORMAT, then return
   a Lisp_String initialized with the data from BUF.  */

Lisp_Object
make_formatted_string (char *buf, const char *format, ...)
{
  va_list ap;
  int length;

  va_start (ap, format);
  length = vsprintf (buf, format, ap);
  va_end (ap);
  return make_string (buf, length);
}

/* Pin a unibyte string in place so that it won't move during GC.  */
void
pin_string (Lisp_Object string)
{
  eassert (STRINGP (string) && ! STRING_MULTIBYTE (string));
  struct Lisp_String *s = XSTRING (string);
  ptrdiff_t size = STRING_BYTES (s);
  unsigned char *data = s->u.s.data;

  if (size <= LARGE_STRING_THRESH
      && ! PURE_P (data) && ! pdumper_object_p (data)
      && s->u.s.size_byte != Sdata_Pinned)
    {
      eassert (s->u.s.size_byte == Sdata_Unibyte);
      eassert (s->u.s.data != NULL);
      sdata *old_sdata = SDATA_OF_LISP_STRING (s);
      allocate_sdata (s, size, size, true);
      memcpy (s->u.s.data, data, size);
      old_sdata->string = NULL;
      eassert (old_sdata->nbytes == size);
    }
  s->u.s.size_byte = Sdata_Pinned;
}

#define GETMARKBIT(block,n)				\
  (((block)->gcmarkbits[(n) / BITS_PER_BITS_WORD]	\
    >> ((n) % BITS_PER_BITS_WORD))			\
   & 1)

#define SETMARKBIT(block,n)				\
  ((block)->gcmarkbits[(n) / BITS_PER_BITS_WORD]	\
   |= (bits_word) 1 << ((n) % BITS_PER_BITS_WORD))

#define UNSETMARKBIT(block,n)				\
  ((block)->gcmarkbits[(n) / BITS_PER_BITS_WORD]	\
   &= ~((bits_word) 1 << ((n) % BITS_PER_BITS_WORD)))

#define FLOAT_BLOCK(fptr) \
  (eassert (! pdumper_object_p (fptr)),                                  \
   ((struct float_block *) (((uintptr_t) (fptr)) & ~(BLOCK_ALIGN - 1))))

#define FLOAT_INDEX(fptr) \
  ((((uintptr_t) (fptr)) & (BLOCK_ALIGN - 1)) / sizeof (struct Lisp_Float))

struct float_block
{
  /* Data first, to preserve alignment.  */
  struct Lisp_Float floats[BLOCK_NFLOATS];
  bits_word gcmarkbits[1 + BLOCK_NFLOATS / BITS_PER_BITS_WORD];
  struct float_block *next;
};

#define XFLOAT_MARKED_P(fptr) \
  GETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX ((fptr)))

#define XFLOAT_MARK(fptr) \
  SETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX ((fptr)))

#define XFLOAT_UNMARK(fptr) \
  UNSETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX ((fptr)))

static struct float_block *float_block;
static int float_block_index = BLOCK_NFLOATS;
static struct Lisp_Float *float_free_list;

Lisp_Object
make_float (double float_value)
{
  register Lisp_Object val;

  if (float_free_list)
    {
      XSETFLOAT (val, float_free_list);
      float_free_list = float_free_list->u.chain;
    }
  else
    {
      if (float_block_index == BLOCK_NFLOATS)
	{
	  struct float_block *new
	    = lisp_align_malloc (sizeof *new, MEM_TYPE_FLOAT);
	  new->next = float_block;
	  memset (new->gcmarkbits, 0, sizeof new->gcmarkbits);
	  float_block = new;
	  float_block_index = 0;
	}
      XSETFLOAT (val, &float_block->floats[float_block_index]);
      float_block_index++;
    }

  XFLOAT_INIT (val, float_value);
  eassert (! XFLOAT_MARKED_P (XFLOAT (val)));
  bytes_since_gc += sizeof (struct Lisp_Float);
  floats_consed++;
  return val;
}

#define CONS_BLOCK(fptr) \
  (eassert (! pdumper_object_p (fptr)),                                  \
   ((struct cons_block *) ((uintptr_t) (fptr) & ~(BLOCK_ALIGN - 1))))

#define CONS_INDEX(fptr) \
  (((uintptr_t) (fptr) & (BLOCK_ALIGN - 1)) / sizeof (struct Lisp_Cons))

struct cons_block
{
  /* Data first, to preserve alignment.  */
  struct Lisp_Cons conses[BLOCK_NCONS];
  bits_word gcmarkbits[1 + BLOCK_NCONS / BITS_PER_BITS_WORD];
  struct cons_block *next;
};

#define XCONS_MARKED_P(fptr) \
  GETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX ((fptr)))

#define XMARK_CONS(fptr) \
  SETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX ((fptr)))

#define XUNMARK_CONS(fptr) \
  UNSETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX ((fptr)))

static struct cons_block *cons_block;
static int cons_block_index = BLOCK_NCONS;
static struct Lisp_Cons *cons_free_list;

/* Explicitly free a cons cell by putting it on the free list.  */

void
free_cons (struct Lisp_Cons *ptr)
{
  ptr->u.s.u.chain = cons_free_list;
  ptr->u.s.car = dead_object ();
  cons_free_list = ptr;
  ptrdiff_t nbytes = sizeof *ptr;
  bytes_since_gc -= nbytes;
}

DEFUN ("cons", Fcons, Scons, 2, 2, 0,
       doc: /* Create a new cons, give it CAR and CDR as components, and return it.  */)
  (Lisp_Object car, Lisp_Object cdr)
{
  register Lisp_Object val;

  if (cons_free_list)
    {
      XSETCONS (val, cons_free_list);
      cons_free_list = cons_free_list->u.s.u.chain;
    }
  else
    {
      if (cons_block_index == BLOCK_NCONS)
	{
	  struct cons_block *new
	    = lisp_align_malloc (sizeof *new, MEM_TYPE_CONS);
	  memset (new->gcmarkbits, 0, sizeof new->gcmarkbits);
	  new->next = cons_block;
	  cons_block = new;
	  cons_block_index = 0;
	}
      XSETCONS (val, &cons_block->conses[cons_block_index]);
      cons_block_index++;
    }

  XSETCAR (val, car);
  XSETCDR (val, cdr);
  eassert (! XCONS_MARKED_P (XCONS (val)));
  bytes_since_gc += sizeof (struct Lisp_Cons);
  cons_cells_consed++;

  return val;
}

/* Make a list of 1, 2, 3, 4 or 5 specified objects.  */

Lisp_Object
list1 (Lisp_Object arg1)
{
  return Fcons (arg1, Qnil);
}

Lisp_Object
list2 (Lisp_Object arg1, Lisp_Object arg2)
{
  return Fcons (arg1, Fcons (arg2, Qnil));
}


Lisp_Object
list3 (Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Qnil)));
}

Lisp_Object
list4 (Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3, Lisp_Object arg4)
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Fcons (arg4, Qnil))));
}

Lisp_Object
list5 (Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3, Lisp_Object arg4,
       Lisp_Object arg5)
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Fcons (arg4,
						       Fcons (arg5, Qnil)))));
}

/* Make a list of COUNT Lisp_Objects, where ARG is the first one.
   Use CONS to construct the pairs.  AP has any remaining args.  */
static Lisp_Object
cons_listn (ptrdiff_t count, Lisp_Object arg,
	    Lisp_Object (*cons) (Lisp_Object, Lisp_Object), va_list ap)
{
  eassume (0 < count);
  Lisp_Object val = cons (arg, Qnil);
  Lisp_Object tail = val;
  for (ptrdiff_t i = 1; i < count; i++)
    {
      Lisp_Object elem = cons (va_arg (ap, Lisp_Object), Qnil);
      XSETCDR (tail, elem);
      tail = elem;
    }
  return val;
}

/* Make a list of COUNT Lisp_Objects, where ARG1 is the first one.  */
Lisp_Object
listn (ptrdiff_t count, Lisp_Object arg1, ...)
{
  va_list ap;
  va_start (ap, arg1);
  Lisp_Object val = cons_listn (count, arg1, Fcons, ap);
  va_end (ap);
  return val;
}

/* Make a pure list of COUNT Lisp_Objects, where ARG1 is the first one.  */
Lisp_Object
pure_listn (ptrdiff_t count, Lisp_Object arg1, ...)
{
  va_list ap;
  va_start (ap, arg1);
  Lisp_Object val = cons_listn (count, arg1, pure_cons, ap);
  va_end (ap);
  return val;
}

DEFUN ("list", Flist, Slist, 0, MANY, 0,
       doc: /* Return a newly created list with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (list &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  register Lisp_Object val;
  val = Qnil;

  while (nargs > 0)
    {
      nargs--;
      val = Fcons (args[nargs], val);
    }
  return val;
}

DEFUN ("make-list", Fmake_list, Smake_list, 2, 2, 0,
       doc: /* Return a newly created list of length LENGTH, with each element being INIT.  */)
  (Lisp_Object length, Lisp_Object init)
{
  Lisp_Object val = Qnil;
  CHECK_FIXNAT (length);

  for (EMACS_INT size = XFIXNAT (length); 0 < size; size--)
    {
      val = Fcons (init, val);
      rarely_quit (size);
    }

  return val;
}

/* Sometimes a vector's contents are merely a pointer internally used
   in vector allocation code.  On the rare platforms where a null
   pointer cannot be tagged, represent it with a Lisp 0.
   Usually you don't want to touch this.  */

static struct Lisp_Vector *
next_vector (struct Lisp_Vector *v)
{
  return XUNTAG (v->contents[0], Lisp_Int0, struct Lisp_Vector);
}

static void
set_next_vector (struct Lisp_Vector *v, struct Lisp_Vector *p)
{
  v->contents[0] = make_lisp_ptr (p, Lisp_Int0);
}

/* Advance vector pointer over a block data.  */

static struct Lisp_Vector *
ADVANCE (struct Lisp_Vector *v, ptrdiff_t nbytes)
{
  void *vv = v;
  char *cv = vv;
  void *p = cv + nbytes;
  return p;
}

static ptrdiff_t
VINDEX (ptrdiff_t nbytes)
{
  eassume (LISP_VECTOR_MIN <= nbytes);
  return (nbytes - LISP_VECTOR_MIN) / word_size;
}

/* So-called large vectors are managed outside vector blocks.

   As C99 does not allow one struct to hold a
   flexible-array-containing struct such as Lisp_Vector, we append the
   Lisp_Vector to the large_vector in memory, and retrieve it via
   large_vector_contents().
*/

struct large_vector
{
  struct large_vector *next;
};

enum
  {
    large_vector_contents_offset = ROUNDUP (sizeof (struct large_vector), LISP_ALIGNMENT)
  };

static struct Lisp_Vector *
large_vector_contents (struct large_vector *p)
{
  return (struct Lisp_Vector *) ((char *) p + large_vector_contents_offset);
}

/* This internal type is used to maintain an underlying storage
   for small vectors.  */

struct vector_block
{
  char data[VBLOCK_NBYTES];
  struct vector_block *next;
};

/* Chain of vector blocks.  */

static struct vector_block *vector_blocks;

/* Each IDX points to a chain of vectors of word-length IDX+1.  */

static struct Lisp_Vector *vector_free_lists[VBLOCK_NFREE_LISTS];

/* Singly-linked list of large vectors.  */

static struct large_vector *large_vectors;

/* The only vector with 0 slots, allocated from pure space.  */

Lisp_Object zero_vector;

static void
add_vector_free_lists (struct Lisp_Vector *v, ptrdiff_t nbytes)
{
  eassume (header_size <= nbytes);
  ptrdiff_t nwords = (nbytes - header_size) / word_size;
  XSETPVECTYPESIZE (v, PVEC_FREE, 0, nwords);
  eassert (nbytes % word_size == 0);
  ptrdiff_t vindex = VINDEX (nbytes);
  eassert (vindex < VBLOCK_NFREE_LISTS);
  set_next_vector (v, vector_free_lists[vindex]);
  vector_free_lists[vindex] = v;
}

static struct vector_block *
allocate_vector_block (void)
{
  struct vector_block *block = xmalloc (sizeof *block);

  mem_insert (block->data, block->data + VBLOCK_NBYTES,
	      MEM_TYPE_VBLOCK);

  block->next = vector_blocks;
  vector_blocks = block;
  return block;
}

static void
init_vectors (void)
{
  zero_vector = make_pure_vector (0);
  staticpro (&zero_vector);
}

/* Nonzero if VECTOR pointer is valid pointer inside BLOCK.  */

#define VECTOR_IN_BLOCK(vector, block)		\
  ((char *) (vector) <= (block)->data		\
   + VBLOCK_NBYTES - LISP_VECTOR_MIN)

/* Return nbytes of vector with HDR.  */

ptrdiff_t
vectorlike_nbytes (const union vectorlike_header *hdr)
{
  ptrdiff_t nwords;
  ptrdiff_t size = hdr->size & ~ARRAY_MARK_FLAG;

  switch (PVTYPE ((const struct Lisp_Vector *) hdr))
    {
    case PVEC_NORMAL_VECTOR:
      nwords = size;
      break;
    case PVEC_BOOL_VECTOR:
      {
	struct Lisp_Bool_Vector *bv = (struct Lisp_Bool_Vector *) hdr;
	ptrdiff_t word_bytes = (bool_vector_words (bv->size)
				* sizeof (bits_word));
	ptrdiff_t boolvec_bytes = bool_header_size + word_bytes;
	verify (header_size <= bool_header_size);
	nwords = (boolvec_bytes - header_size + word_size - 1) / word_size;
      }
      break;
    default:
      eassert (size & PSEUDOVECTOR_FLAG);
      nwords = ((size & PSEUDOVECTOR_SIZE_MASK)
		+ ((size & PSEUDOVECTOR_REST_MASK)
		   >> PSEUDOVECTOR_SIZE_BITS));
      break;
    }
  return header_size + word_size * nwords;
}

/* Convert a pseudovector pointer P to its underlying struct T pointer.
   Verify that the struct is small, since free_by_pvtype() is called
   only on small vector-like objects.  */

#define PSEUDOVEC_STRUCT(p, t) \
  verify_expr ((header_size + VECSIZE (struct t) * word_size \
		<= LARGE_VECTOR_THRESH), \
	       (struct t *) (p))

/* Release extra resources still in use by VECTOR, which may be any
   small vector-like object.  */

static void
free_by_pvtype (struct Lisp_Vector *vector)
{
  detect_suspicious_free (vector);

  switch (PVTYPE (vector))
    {
    case PVEC_BIGNUM:
      mpz_clear (PSEUDOVEC_STRUCT (vector, Lisp_Bignum)->value);
      break;
    case PVEC_FINALIZER:
      unchain_finalizer (PSEUDOVEC_STRUCT (vector, Lisp_Finalizer));
      break;
    case PVEC_FONT:
      if (FONT_OBJECT_MAX == (vector->header.size & PSEUDOVECTOR_SIZE_MASK))
	{
	  struct font *font = PSEUDOVEC_STRUCT (vector, font);
	  struct font_driver const *drv = font->driver;

	  /* DRV could be NULL for interrupts on startup. Bug#16140  */
	  if (drv)
	    {
	      eassume (valid_font_driver (drv));
	      drv->close_font (font);
	    }
	}
      break;
    case PVEC_THREAD:
      finalize_one_thread (PSEUDOVEC_STRUCT (vector, thread_state));
      break;
    case PVEC_MUTEX:
      finalize_one_mutex (PSEUDOVEC_STRUCT (vector, Lisp_Mutex));
      break;
    case PVEC_CONDVAR:
      finalize_one_condvar (PSEUDOVEC_STRUCT (vector, Lisp_CondVar));
      break;
    case PVEC_MARKER:
      /* sweep_buffer() ought to have unchained it.  */
      eassert (! PSEUDOVEC_STRUCT (vector, Lisp_Marker)->buffer);
      break;
    case PVEC_USER_PTR:
      {
	struct Lisp_User_Ptr *uptr = PSEUDOVEC_STRUCT (vector, Lisp_User_Ptr);
	if (uptr->finalizer)
	  uptr->finalizer (uptr->p);
      }
      break;
#ifdef HAVE_MODULES
    case PVEC_MODULE_FUNCTION:
      {
	ATTRIBUTE_MAY_ALIAS struct Lisp_Module_Function *function
	  = (struct Lisp_Module_Function *) vector;
	module_finalize_function (function);
      }
      break;
#endif
#ifdef HAVE_NATIVE_COMP
    case PVEC_NATIVE_COMP_UNIT:
      {
	struct Lisp_Native_Comp_Unit *cu
	  = PSEUDOVEC_STRUCT (vector, Lisp_Native_Comp_Unit);
	unload_comp_unit (cu);
      }
      break;
    case PVEC_SUBR:
      {
	struct Lisp_Subr *subr = PSEUDOVEC_STRUCT (vector, Lisp_Subr);
	if (! NILP (subr->native_comp_u))
	  {
	    xfree ((char *) subr->symbol_name);
	    xfree (subr->native_c_name);
	  }
      }
      break;
#endif
#ifdef HAVE_TREE_SITTER
    case PVEC_TREE_SITTER:
      {
	struct Lisp_Tree_Sitter *lisp_parser
	  = PSEUDOVEC_STRUCT (vector, Lisp_Tree_Sitter);
	if (lisp_parser->highlight_names != NULL)
	  xfree (lisp_parser->highlight_names);
	if (lisp_parser->highlights_query != NULL)
	  xfree (lisp_parser->highlights_query);
	if (lisp_parser->highlighter != NULL)
	  ts_highlighter_delete (lisp_parser->highlighter);
	if (lisp_parser->tree != NULL)
	  ts_tree_delete(lisp_parser->tree);
	if (lisp_parser->prev_tree != NULL)
	  ts_tree_delete(lisp_parser->prev_tree);
	if (lisp_parser->parser != NULL)
	  ts_parser_delete(lisp_parser->parser);
      }
      break;
    case PVEC_TREE_SITTER_NODE:
      /* currently nothing to clean up.  */
      break;
#endif
#ifdef HAVE_SQLITE3
    case PVEC_SQLITE:
      /* clean s___ up.  To be implemented.  */
      break;
#endif
    default:
      break;
    }
}

/* Reclaim space used by unmarked vectors.  */

static void
sweep_vectors (void)
{
  memset (vector_free_lists, 0, sizeof (vector_free_lists));

  gcstat.total_vectors =
    gcstat.total_vector_slots =
    gcstat.total_free_vector_slots = 0;

  /* Non-large vectors in VECTOR_BLOCKS.  */
  for (struct vector_block *block = vector_blocks,
	 **bprev = &vector_blocks;
       block != NULL;
       block = *bprev)
    {
      ptrdiff_t run_bytes = 0;
      struct Lisp_Vector *run_vector = NULL;
      for (struct Lisp_Vector *vector = (struct Lisp_Vector *) block->data;
	   VECTOR_IN_BLOCK (vector, block);
	   (void) vector)
	{
	  ptrdiff_t nbytes = vector_nbytes (vector);
	  if (vector_marked_p (vector))
	    {
	      if (run_vector)
		{
		  eassume (run_bytes && run_bytes % word_size == 0);
		  add_vector_free_lists (run_vector, run_bytes);
		  gcstat.total_free_vector_slots += run_bytes / word_size;
		  run_bytes = 0;
		  run_vector = NULL;
		}
	      XUNMARK_VECTOR (vector);
	      gcstat.total_vectors++;
	      gcstat.total_vector_slots += nbytes / word_size;
	    }
	  else
	    {
	      free_by_pvtype (vector);
	      if (run_vector == NULL)
		{
		  eassert (run_bytes == 0);
		  run_vector = vector;
		}
	      run_bytes += nbytes;
	    }
	  vector = ADVANCE (vector, nbytes);
	}

      if (run_vector == (struct Lisp_Vector *) block->data)
	{
	  /* If RUN_VECTOR never wavered from its initial
	     assignment, then nothing in the block was marked.
	     Harvest it back to OS.  */
	  *bprev = block->next;
	  mem_delete (mem_find (block->data));
	  xfree (block);
	}
      else
	{
	  bprev = &block->next;
	  if (run_vector)
	    {
	      /* block ended in an unmarked vector */
	      add_vector_free_lists (run_vector, run_bytes);
	      gcstat.total_free_vector_slots += run_bytes / word_size;
	    }
	}
    }

  /* Free floating large vectors.  */
  for (struct large_vector *lv = large_vectors,
	 **lvprev = &large_vectors;
       lv != NULL;
       lv = *lvprev)
    {
      struct Lisp_Vector *vector = large_vector_contents (lv);
      if (XVECTOR_MARKED_P (vector))
	{
	  XUNMARK_VECTOR (vector);
	  gcstat.total_vectors++;
	  gcstat.total_vector_slots
	    += (vector->header.size & PSEUDOVECTOR_FLAG
		? vector_nbytes (vector) / word_size
		: header_size / word_size + vector->header.size);
	  lvprev = &lv->next;
	}
      else
	{
	  *lvprev = lv->next;
	  lisp_free (lv);
	}
    }
}

/* Maximum number of elements in a vector.  This is a macro so that it
   can be used in an integer constant expression.  */

#define VECTOR_ELTS_MAX \
  ((ptrdiff_t) \
   min (((min (PTRDIFF_MAX, SIZE_MAX) - header_size - large_vector_contents_offset) \
	 / word_size), \
	MOST_POSITIVE_FIXNUM))

/* Return a newly allocated Lisp_Vector.

   For whatever reason, LEN words consuming more than half VBLOCK_NBYTES
   is considered "large."
  */

static struct Lisp_Vector *
allocate_vector (ptrdiff_t len, bool q_clear)
{
  ptrdiff_t nbytes = header_size + len * word_size;
  struct Lisp_Vector *p = NULL;

  if (len == 0)
    return XVECTOR (zero_vector);

  if (len > VECTOR_ELTS_MAX)
    error ("Requested %ld > %ld vector elements", len, VECTOR_ELTS_MAX);

  if (nbytes > LARGE_VECTOR_THRESH)
    {
      struct large_vector *lv = lisp_malloc (large_vector_contents_offset + nbytes,
					     q_clear, MEM_TYPE_VECTORLIKE);
      lv->next = large_vectors;
      large_vectors = lv;
      p = large_vector_contents (lv);
    }
  else
    {
      ptrdiff_t restbytes = 0;

      eassume (LISP_VECTOR_MIN <= nbytes && nbytes <= LARGE_VECTOR_THRESH);
      eassume (nbytes % word_size == 0);

      for (ptrdiff_t exact = VINDEX (nbytes), index = exact;
	   index < VBLOCK_NFREE_LISTS; ++index)
	{
	  restbytes = index * word_size + LISP_VECTOR_MIN - nbytes;
	  eassert (restbytes || index == exact);
	  /* Either leave no residual or one big enough to sustain a
	     non-degenerate vector.  A hanging chad of MEM_TYPE_VBLOCK
	     triggers all manner of ENABLE_CHECKING failures.  */
	  if (! restbytes || restbytes >= LISP_VECTOR_MIN)
	    if (vector_free_lists[index])
	      {
		p = vector_free_lists[index];
		vector_free_lists[index] = next_vector (p);
		break;
	      }
	}

      if (! p)
	{
	  /* Need new block */
	  p = (struct Lisp_Vector *) allocate_vector_block ()->data;
	  restbytes = VBLOCK_NBYTES - nbytes;
	}

      if (restbytes)
	{
	  /* Tack onto free list corresponding to VINDEX(RESTBYTES).  */
	  eassert (restbytes % word_size == 0);
	  eassert (restbytes >= LISP_VECTOR_MIN);
	  add_vector_free_lists (ADVANCE (p, nbytes), restbytes);
	}

      if (q_clear)
	memclear (p, nbytes);
    }

  if (find_suspicious_object_in_range (p, (char *) p + nbytes))
    emacs_abort ();

  bytes_since_gc += nbytes;
  vector_cells_consed += len;

  p->header.size = len;
  return p;
}

struct Lisp_Vector *
allocate_pseudovector (int memlen, int lisplen,
		       int zerolen, enum pvec_type tag)
{
  /* Catch bogus values.  */
  enum { size_max = VBLOCK_ALIGN - 1 };
  enum { rest_max = (1 << PSEUDOVECTOR_REST_BITS) - 1 };
  verify (size_max + rest_max <= VECTOR_ELTS_MAX);
  eassert (0 <= tag && tag <= PVEC_FONT);
  eassert (0 <= lisplen && lisplen <= zerolen && zerolen <= memlen);
  eassert (lisplen <= size_max);
  eassert (memlen <= size_max + rest_max);

  struct Lisp_Vector *vec = static_vector_allocator (memlen, false);
  /* Only the first LISPLEN slots will be traced normally by the GC.  */
  memclear (vec->contents, zerolen * word_size);
  XSETPVECTYPESIZE (vec, tag, lisplen, memlen - lisplen);
  return vec;
}

struct buffer *
allocate_buffer (void)
{
  struct buffer *b
    = ALLOCATE_PSEUDOVECTOR (struct buffer, cursor_in_non_selected_windows_,
			     PVEC_BUFFER);
  BUFFER_PVEC_INIT (b);
  /* Note that the rest fields of B are not initialized.  */
  return b;
}

/* Allocate a record with COUNT slots.  COUNT must be positive, and
   includes the type slot.  */

static struct Lisp_Vector *
allocate_record (EMACS_INT count)
{
  if (count > PSEUDOVECTOR_SIZE_MASK)
    error ("Attempt to allocate a record of %"pI"d slots; max is %d",
	   count, PSEUDOVECTOR_SIZE_MASK);
  struct Lisp_Vector *p = static_vector_allocator (count, false);
  p->header.size = count;
  XSETPVECTYPE (p, PVEC_RECORD);
  return p;
}


DEFUN ("make-record", Fmake_record, Smake_record, 3, 3, 0,
       doc: /* Create a new record.
TYPE is its type as returned by `type-of'; it should be either a
symbol or a type descriptor.  SLOTS is the number of non-type slots,
each initialized to INIT.  */)
  (Lisp_Object type, Lisp_Object slots, Lisp_Object init)
{
  CHECK_FIXNAT (slots);
  EMACS_INT size = XFIXNAT (slots) + 1;
  struct Lisp_Vector *p = allocate_record (size);
  p->contents[0] = type;
  for (ptrdiff_t i = 1; i < size; i++)
    p->contents[i] = init;
  return make_lisp_ptr (p, Lisp_Vectorlike);
}

DEFUN ("record", Frecord, Srecord, 1, MANY, 0,
       doc: /* Create a new record.
TYPE is its type as returned by `type-of'; it should be either a
symbol or a type descriptor.  SLOTS is used to initialize the record
slots with shallow copies of the arguments.
usage: (record TYPE &rest SLOTS) */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  struct Lisp_Vector *p = allocate_record (nargs);
  memcpy (p->contents, args, nargs * sizeof *args);
  return make_lisp_ptr (p, Lisp_Vectorlike);
}

DEFUN ("make-vector", Fmake_vector, Smake_vector, 2, 2, 0,
       doc: /* Return a new vector of LENGTH instances of INIT.  */)
  (Lisp_Object length, Lisp_Object init)
{
  CHECK_TYPE (FIXNATP (length) && XFIXNAT (length) <= PTRDIFF_MAX,
	      Qwholenump, length);
  return initialize_vector (XFIXNAT (length), init);
}

Lisp_Object
initialize_vector (ptrdiff_t length, Lisp_Object init)
{
  struct Lisp_Vector *p = static_vector_allocator (length, NILP (init));
  if (! NILP (init))
    for (ptrdiff_t i = 0; i < length; ++i)
      p->contents[i] = init;
  return make_lisp_ptr (p, Lisp_Vectorlike);
}

DEFUN ("vector", Fvector, Svector, 0, MANY, 0,
       doc: /* Return a new vector containing the specified ARGS.
usage: (vector &rest ARGS)
ARGS can be empty, yielding the empty vector.  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object val = make_vector (nargs);
  struct Lisp_Vector *p = XVECTOR (val);
  memcpy (p->contents, args, nargs * sizeof *args);
  return val;
}

DEFUN ("make-byte-code", Fmake_byte_code, Smake_byte_code, 4, MANY, 0,
       doc: /* Create a byte-code object.
usage: (make-byte-code ARGLIST BYTE-CODE CONSTANTS DEPTH &optional DOCSTRING INTERACTIVE-SPEC &rest ELEMENTS)

ARGLIST is either a list of formal args to be dynamically bound
(congruent to that of a lambda expresssion), or a short-length bit
sequence NNNNNNNRMMMMMMM instructing the runtime how to interpret the
object's static arguments.  The 7 bits MMMMMMM specify the minimum
arity, the 7 bits NNNNNNN specify the maximum arity (ignoring &rest),
and the R bit flags the presence of &rest arguments.
*/)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object val;
  if (! ((FIXNUMP (args[COMPILED_ARGLIST])
	  || CONSP (args[COMPILED_ARGLIST])
	  || NILP (args[COMPILED_ARGLIST]))
	 && STRINGP (args[COMPILED_BYTECODE])
	 && ! STRING_MULTIBYTE (args[COMPILED_BYTECODE])
	 && VECTORP (args[COMPILED_CONSTANTS])
	 && FIXNATP (args[COMPILED_STACK_DEPTH])))
    error ("Invalid byte-code object");

  pin_string (args[COMPILED_BYTECODE]); /* Bytecode is immovable. */

  /* Under lexical binding, closures can no longer be pure copied. */
  val = Fvector (nargs, args);
  XSETPVECTYPE (XVECTOR (val), PVEC_COMPILED);
  return val;
}

DEFUN ("make-closure", Fmake_closure, Smake_closure, 1, MANY, 0,
       doc: /* Create a byte-code closure from PROTOTYPE and CLOSURE-VARS.
Return a copy of PROTOTYPE, a byte-code object, with CLOSURE-VARS
replacing the elements in the beginning of the constant-vector.
usage: (make-closure PROTOTYPE &rest CLOSURE-VARS) */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object protofun = args[0];
  CHECK_TYPE (COMPILEDP (protofun), Qbyte_code_function_p, protofun);

  /* Create a copy of the constant vector, filling it with the closure
     variables in the beginning.  (The overwritten part should just
     contain placeholder values.) */
  Lisp_Object proto_constvec = AREF (protofun, COMPILED_CONSTANTS);
  ptrdiff_t constsize = ASIZE (proto_constvec);
  ptrdiff_t nvars = nargs - 1;
  if (nvars > constsize)
    error ("Closure vars do not fit in constvec");
  Lisp_Object constvec = make_vector (constsize);
  memcpy (XVECTOR (constvec)->contents, args + 1, nvars * word_size);
  memcpy (XVECTOR (constvec)->contents + nvars,
	  XVECTOR (proto_constvec)->contents + nvars,
	  (constsize - nvars) * word_size);

  /* Return a copy of the prototype function with the new constant vector. */
  ptrdiff_t protosize = PVSIZE (protofun);
  struct Lisp_Vector *vec = static_vector_allocator (protosize, false);
  vec->header = XVECTOR (protofun)->header;
  memcpy (vec->contents, XVECTOR (protofun)->contents, protosize * word_size);
  vec->contents[COMPILED_CONSTANTS] = constvec;
  return make_lisp_ptr (vec, Lisp_Vectorlike);
}

struct symbol_block
{
  /* Data first, to preserve alignment.  */
  struct Lisp_Symbol symbols[BLOCK_NSYMBOLS];
  struct symbol_block *next;
};

/* Current symbol block and index of first unused Lisp_Symbol
   structure in it.  */

static struct symbol_block *symbol_block;
static int symbol_block_index = BLOCK_NSYMBOLS;
/* Pointer to the first symbol_block that contains pinned symbols.
   Tests for 24.4 showed that at dump-time, Emacs contains about 15K symbols,
   10K of which are pinned (and all but 250 of them are interned in obarray),
   whereas a "typical session" has in the order of 30K symbols.
   symbol_block_pinned lets mark_pinned_symbols scan only 15K symbols rather
   than 30K to find the 10K symbols we need to mark.  */
static struct symbol_block *symbol_block_pinned;
static struct Lisp_Symbol *symbol_free_list;

static void
set_symbol_name (Lisp_Object sym, Lisp_Object name)
{
  XSYMBOL (sym)->u.s.name = name;
}

void
init_symbol (Lisp_Object val, Lisp_Object name)
{
  struct Lisp_Symbol *p = XSYMBOL (val);
  set_symbol_name (val, name);
  set_symbol_plist (val, Qnil);
  p->u.s.redirect = SYMBOL_PLAINVAL;
  SET_SYMBOL_VAL (p, Qunbound);
  set_symbol_function (val, Qnil);
  set_symbol_next (val, NULL);
  p->u.s.gcmarkbit = false;
  p->u.s.interned = SYMBOL_UNINTERNED;
  p->u.s.trapped_write = SYMBOL_UNTRAPPED_WRITE;
  p->u.s.declared_special = false;
  p->u.s.pinned = false;
}

DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
       doc: /* Return an uninterned, unbound symbol whose name is NAME. */)
  (Lisp_Object name)
{
  Lisp_Object val;

  CHECK_STRING (name);

  if (symbol_free_list)
    {
      XSETSYMBOL (val, symbol_free_list);
      symbol_free_list = symbol_free_list->u.s.next;
    }
  else
    {
      if (symbol_block_index == BLOCK_NSYMBOLS)
	{
	  struct symbol_block *new
	    = lisp_malloc (sizeof *new, false, MEM_TYPE_SYMBOL);
	  new->next = symbol_block;
	  symbol_block = new;
	  symbol_block_index = 0;
	}
      XSETSYMBOL (val, &symbol_block->symbols[symbol_block_index]);
      symbol_block_index++;
    }

  init_symbol (val, name);
  bytes_since_gc += sizeof (struct Lisp_Symbol);
  symbols_consed++;
  return val;
}

Lisp_Object
make_misc_ptr (void *a)
{
  struct Lisp_Misc_Ptr *p = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Misc_Ptr,
							 PVEC_MISC_PTR);
  p->pointer = a;
  return make_lisp_ptr (p, Lisp_Vectorlike);
}

/* Return a new overlay with specified START, END and PLIST.  */

Lisp_Object
build_overlay (Lisp_Object start, Lisp_Object end, Lisp_Object plist)
{
  struct Lisp_Overlay *p = ALLOCATE_PSEUDOVECTOR (struct Lisp_Overlay, plist,
						  PVEC_OVERLAY);
  Lisp_Object overlay = make_lisp_ptr (p, Lisp_Vectorlike);
  OVERLAY_START (overlay) = start;
  OVERLAY_END (overlay) = end;
  set_overlay_plist (overlay, plist);
  p->next = NULL;
  return overlay;
}

DEFUN ("make-marker", Fmake_marker, Smake_marker, 0, 0, 0,
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

/* Return a newly allocated marker which points into BUF
   at character position CHARPOS and byte position BYTEPOS.  */

Lisp_Object
build_marker (struct buffer *buf, ptrdiff_t charpos, ptrdiff_t bytepos)
{
  /* No dead buffers here.  */
  eassert (BUFFER_LIVE_P (buf));

  /* Every character is at least one byte.  */
  eassert (charpos <= bytepos);

  struct Lisp_Marker *m = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Marker,
						       PVEC_MARKER);
  m->buffer = buf;
  m->charpos = charpos;
  m->bytepos = bytepos;
  m->insertion_type = 0;
  m->need_adjustment = 0;
  m->next = BUF_MARKERS (buf);
  BUF_MARKERS (buf) = m;
  return make_lisp_ptr (m, Lisp_Vectorlike);
}


/* Return a newly created vector or string with specified arguments as
   elements.  If all the arguments are characters that can fit
   in a string of events, make a string; otherwise, make a vector.

   Allows any number of arguments, including zero.  */

Lisp_Object
make_event_array (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;

  for (i = 0; i < nargs; i++)
    /* The things that fit in a string
       are characters that are in 0...127,
       after discarding the meta bit and all the bits above it.  */
    if (!FIXNUMP (args[i])
	|| (XFIXNUM (args[i]) & ~(-CHAR_META)) >= 0200)
      return Fvector (nargs, args);

  /* Since the loop exited, we know that all the things in it are
     characters, so we can make a string.  */
  {
    Lisp_Object result;

    result = Fmake_string (make_fixnum (nargs), make_fixnum (0), Qnil);
    for (i = 0; i < nargs; i++)
      {
	SSET (result, i, XFIXNUM (args[i]));
	/* Move the meta bit to the right place for a string char.  */
	if (XFIXNUM (args[i]) & CHAR_META)
	  SSET (result, i, SREF (result, i) | 0x80);
      }

    return result;
  }
}

#ifdef HAVE_MODULES
/* Create a new module user ptr object.  */
Lisp_Object
make_user_ptr (void (*finalizer) (void *), void *p)
{
  struct Lisp_User_Ptr *uptr
    = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_User_Ptr, PVEC_USER_PTR);
  uptr->finalizer = finalizer;
  uptr->p = p;
  return make_lisp_ptr (uptr, Lisp_Vectorlike);
}
#endif

static void
init_finalizer_list (struct Lisp_Finalizer *head)
{
  head->prev = head->next = head;
}

/* Insert FINALIZER before ELEMENT.  */

static void
finalizer_insert (struct Lisp_Finalizer *element,
                  struct Lisp_Finalizer *finalizer)
{
  eassert (finalizer->prev == NULL);
  eassert (finalizer->next == NULL);
  finalizer->next = element;
  finalizer->prev = element->prev;
  finalizer->prev->next = finalizer;
  element->prev = finalizer;
}

static void
unchain_finalizer (struct Lisp_Finalizer *finalizer)
{
  if (finalizer->prev != NULL)
    {
      eassert (finalizer->next != NULL);
      finalizer->prev->next = finalizer->next;
      finalizer->next->prev = finalizer->prev;
      finalizer->prev = finalizer->next = NULL;
    }
}

static void
mark_finalizer_list (struct Lisp_Finalizer *head)
{
  for (struct Lisp_Finalizer *finalizer = head->next;
       finalizer != head;
       finalizer = finalizer->next)
    {
      set_vectorlike_marked (&finalizer->header);
      mark_object (&finalizer->function);
    }
}

/* Move doomed finalizers to list DEST from list SRC.  A doomed
   finalizer is one that is not GC-reachable and whose
   finalizer->function is non-nil.  */

static void
queue_doomed_finalizers (struct Lisp_Finalizer *dest,
                         struct Lisp_Finalizer *src)
{
  for (struct Lisp_Finalizer *current = src->next,
	 *next = current->next;
       current != src;
       current = next, next = current->next)
    {
      if (! vectorlike_marked_p (&current->header)
          && ! NILP (current->function))
        {
          unchain_finalizer (current);
          finalizer_insert (dest, current);
        }
    }
}

static Lisp_Object
run_finalizer_handler (Lisp_Object args)
{
  add_to_log ("finalizer failed: %S", args);
  return Qnil;
}

static void
run_finalizer_function (Lisp_Object function)
{
  specpdl_ref count = SPECPDL_INDEX ();
#ifdef HAVE_PDUMPER
  ++number_finalizers_run;
#endif

  specbind (Qinhibit_quit, Qt);
  internal_condition_case_1 (call0, function, Qt, run_finalizer_handler);
  unbind_to (count, Qnil);
}

static void
run_finalizers (struct Lisp_Finalizer *finalizers)
{
  struct Lisp_Finalizer *finalizer;
  Lisp_Object function;

  while (finalizers->next != finalizers)
    {
      finalizer = finalizers->next;
      unchain_finalizer (finalizer);
      function = finalizer->function;
      if (! NILP (function))
	{
	  finalizer->function = Qnil;
	  run_finalizer_function (function);
	}
    }
}

DEFUN ("make-finalizer", Fmake_finalizer, Smake_finalizer, 1, 1, 0,
       doc: /* Wrap FUNCTION in a finalizer (similar to destructor).
FUNCTION is called in an end-run around gc once its finalizer object
becomes unreachable or only reachable from other finalizers.  */)
  (Lisp_Object function)
{
  CHECK_TYPE (FUNCTIONP (function), Qfunctionp, function);
  struct Lisp_Finalizer *finalizer
    = ALLOCATE_PSEUDOVECTOR (struct Lisp_Finalizer, function, PVEC_FINALIZER);
  finalizer->function = function;
  finalizer->prev = finalizer->next = NULL;
  finalizer_insert (&finalizers, finalizer);
  return make_lisp_ptr (finalizer, Lisp_Vectorlike);
}


/* With the rare exception of functions implementing block-based
   allocation of various types, you should not directly test or set GC
   mark bits on objects.  Some objects might live in special memory
   regions (e.g., a dump image) and might store their mark bits
   elsewhere.  */

static bool
vector_marked_p (const struct Lisp_Vector *v)
{
  bool ret;
  eassert (! mgc_xpntr_p (v));
  if (pdumper_object_p (v))
    {
      /* Checking cold for bool vectors saves faulting in vector header.  */
      if (pdumper_cold_object_p (v))
        {
          eassert (PVTYPE (v) == PVEC_BOOL_VECTOR);
          ret = true;
        }
      else
	ret = pdumper_marked_p (v);
    }
  else
    ret = XVECTOR_MARKED_P (v);
  return ret;
}

static void
set_vector_marked (struct Lisp_Vector *v)
{
  eassert (! vector_marked_p (v));
  if (pdumper_object_p (v))
    {
      eassert (PVTYPE (v) != PVEC_BOOL_VECTOR);
      pdumper_set_marked (v);
    }
  else
    XMARK_VECTOR (v);
}

static bool
vectorlike_marked_p (const union vectorlike_header *header)
{
  return vector_marked_p ((const struct Lisp_Vector *) header);
}

static void
set_vectorlike_marked (union vectorlike_header *header)
{
  if (! vectorlike_marked_p (header))
    set_vector_marked ((struct Lisp_Vector *) header);
}

static bool
cons_marked_p (const struct Lisp_Cons *c)
{
  return pdumper_object_p (c)
    ? pdumper_marked_p (c)
    : XCONS_MARKED_P (c);
}

static void
set_cons_marked (struct Lisp_Cons *c)
{
  if (pdumper_object_p (c))
    pdumper_set_marked (c);
  else
    XMARK_CONS (c);
}

static bool
string_marked_p (const struct Lisp_String *s)
{
  bool ret;
  eassert (! mgc_xpntr_p (s));
  if (pdumper_object_p (s))
    ret = pdumper_marked_p (s);
  else
    ret = XSTRING_MARKED_P (s);
  return ret;
}

static void
set_string_marked (struct Lisp_String *s)
{
  eassert (! string_marked_p (s));
  if (pdumper_object_p (s))
    pdumper_set_marked (s);
  else
    XMARK_STRING (s);
}

static bool
symbol_marked_p (const struct Lisp_Symbol *s)
{
  return pdumper_object_p (s)
    ? pdumper_marked_p (s)
    : s->u.s.gcmarkbit;
}

static void
set_symbol_marked (struct Lisp_Symbol *s)
{
  if (pdumper_object_p (s))
    pdumper_set_marked (s);
  else
    s->u.s.gcmarkbit = true;
}

static bool
interval_marked_p (INTERVAL i)
{
  return pdumper_object_p (i)
    ? pdumper_marked_p (i)
    : i->gcmarkbit;
}

static void
set_interval_marked (INTERVAL i)
{
  if (pdumper_object_p (i))
    pdumper_set_marked (i);
  else
    i->gcmarkbit = true;
}

void
memory_full (size_t nbytes)
{
  const size_t enough = (1 << 14);

  if (! initialized)
    fatal ("memory exhausted");

  Vmemory_full = Qt;
  if (nbytes > enough)
    {
      void *p = malloc (enough);
      if (p)
	{
	  Vmemory_full = Qnil;
	  free (p);
	}
    }

  xsignal (Qnil, Vmemory_signal_data);
}

static void
mem_init (void)
{
  mem_z.left = mem_z.right = MEM_NIL;
  mem_z.parent = NULL;
  mem_z.color = MEM_BLACK;
  mem_z.start = mem_z.end = NULL;
  mem_root = MEM_NIL;
}

/* Return mem_node containing START or failing that, MEM_NIL.  */

struct mem_node *
mem_find (void *start)
{
  struct mem_node *p;

  if (start < min_heap_address || start > max_heap_address)
    return MEM_NIL;

  /* Make the search always successful to speed up the loop below.  */
  mem_z.start = start;
  mem_z.end = (char *) start + 1;

  p = mem_root;
  while (start < p->start || start >= p->end)
    p = start < p->start ? p->left : p->right;
  return p;
}

/* Insert node representing mem block of TYPE spanning START and END.
   Return the inserted node.  */

static struct mem_node *
mem_insert (void *start, void *end, enum mem_type type)
{
  struct mem_node *c, *parent, *x;

  if (min_heap_address == NULL || start < min_heap_address)
    min_heap_address = start;
  if (max_heap_address == NULL || end > max_heap_address)
    max_heap_address = end;

  /* See where in the tree a node for START belongs.  In this
     particular application, it shouldn't happen that a node is already
     present.  For debugging purposes, let's check that.  */
  c = mem_root;
  parent = NULL;

  while (c != MEM_NIL)
    {
      parent = c;
      c = start < c->start ? c->left : c->right;
    }

  /* Create a new node.  */
  x = xmalloc (sizeof *x);
  x->start = start;
  x->end = end;
  x->type = type;
  x->parent = parent;
  x->left = x->right = MEM_NIL;
  x->color = MEM_RED;

  /* Insert it as child of PARENT or install it as root.  */
  if (parent)
    {
      if (start < parent->start)
	parent->left = x;
      else
	parent->right = x;
    }
  else
    mem_root = x;

  /* Re-establish red-black tree properties.  */
  mem_insert_fixup (x);

  return x;
}

/* Insert node X, then rebalance red-black tree.  X is always red.  */

static void
mem_insert_fixup (struct mem_node *x)
{
  while (x != mem_root && x->parent->color == MEM_RED)
    {
      /* X is red and its parent is red.  This is a violation of
	 red-black tree property #3.  */

      if (x->parent == x->parent->parent->left)
	{
	  /* We're on the left side of our grandparent, and Y is our
	     "uncle".  */
	  struct mem_node *y = x->parent->parent->right;

	  if (y->color == MEM_RED)
	    {
	      /* Uncle and parent are red but should be black because
		 X is red.  Change the colors accordingly and proceed
		 with the grandparent.  */
	      x->parent->color = MEM_BLACK;
	      y->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      x = x->parent->parent;
            }
	  else
	    {
	      /* Parent and uncle have different colors; parent is
		 red, uncle is black.  */
	      if (x == x->parent->right)
		{
		  x = x->parent;
		  mem_rotate_left (x);
                }

	      x->parent->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      mem_rotate_right (x->parent->parent);
            }
        }
      else
	{
	  /* This is the symmetrical case of above.  */
	  struct mem_node *y = x->parent->parent->left;

	  if (y->color == MEM_RED)
	    {
	      x->parent->color = MEM_BLACK;
	      y->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      x = x->parent->parent;
            }
	  else
	    {
	      if (x == x->parent->left)
		{
		  x = x->parent;
		  mem_rotate_right (x);
		}

	      x->parent->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      mem_rotate_left (x->parent->parent);
            }
        }
    }

  /* The root may have been changed to red due to the algorithm.  Set
     it to black so that property #5 is satisfied.  */
  mem_root->color = MEM_BLACK;
}

/*   (x)                   (y)
     / \                   / \
    a   (y)      ===>    (x)  c
        / \              / \
       b   c            a   b  */

static void
mem_rotate_left (struct mem_node *x)
{
  struct mem_node *y;

  /* Turn y's left sub-tree into x's right sub-tree.  */
  y = x->right;
  x->right = y->left;
  if (y->left != MEM_NIL)
    y->left->parent = x;

  /* Y's parent was x's parent.  */
  if (y != MEM_NIL)
    y->parent = x->parent;

  /* Get the parent to point to y instead of x.  */
  if (x->parent)
    {
      if (x == x->parent->left)
	x->parent->left = y;
      else
	x->parent->right = y;
    }
  else
    mem_root = y;

  /* Put x on y's left.  */
  y->left = x;
  if (x != MEM_NIL)
    x->parent = y;
}

/*     (x)                (Y)
       / \                / \
     (y)  c      ===>    a  (x)
     / \                    / \
    a   b                  b   c  */

static void
mem_rotate_right (struct mem_node *x)
{
  struct mem_node *y = x->left;

  x->left = y->right;
  if (y->right != MEM_NIL)
    y->right->parent = x;

  if (y != MEM_NIL)
    y->parent = x->parent;
  if (x->parent)
    {
      if (x == x->parent->right)
	x->parent->right = y;
      else
	x->parent->left = y;
    }
  else
    mem_root = y;

  y->right = x;
  if (x != MEM_NIL)
    x->parent = y;
}

static void
mem_delete (struct mem_node *z)
{
  struct mem_node *x, *y;

  if (!z || z == MEM_NIL)
    return;

  if (z->left == MEM_NIL || z->right == MEM_NIL)
    y = z;
  else
    {
      y = z->right;
      while (y->left != MEM_NIL)
	y = y->left;
    }

  if (y->left != MEM_NIL)
    x = y->left;
  else
    x = y->right;

  x->parent = y->parent;
  if (y->parent)
    {
      if (y == y->parent->left)
	y->parent->left = x;
      else
	y->parent->right = x;
    }
  else
    mem_root = x;

  if (y != z)
    {
      z->start = y->start;
      z->end = y->end;
      z->type = y->type;
    }

  if (y->color == MEM_BLACK)
    mem_delete_fixup (x);

  xfree (y);
}

/* Delete X, then rebalance red-black tree.  */

static void
mem_delete_fixup (struct mem_node *x)
{
  while (x != mem_root && x->color == MEM_BLACK)
    {
      if (x == x->parent->left)
	{
	  struct mem_node *w = x->parent->right;

	  if (w->color == MEM_RED)
	    {
	      w->color = MEM_BLACK;
	      x->parent->color = MEM_RED;
	      mem_rotate_left (x->parent);
	      w = x->parent->right;
            }

	  if (w->left->color == MEM_BLACK && w->right->color == MEM_BLACK)
	    {
	      w->color = MEM_RED;
	      x = x->parent;
            }
	  else
	    {
	      if (w->right->color == MEM_BLACK)
		{
		  w->left->color = MEM_BLACK;
		  w->color = MEM_RED;
		  mem_rotate_right (w);
		  w = x->parent->right;
                }
	      w->color = x->parent->color;
	      x->parent->color = MEM_BLACK;
	      w->right->color = MEM_BLACK;
	      mem_rotate_left (x->parent);
	      x = mem_root;
            }
        }
      else
	{
	  struct mem_node *w = x->parent->left;

	  if (w->color == MEM_RED)
	    {
	      w->color = MEM_BLACK;
	      x->parent->color = MEM_RED;
	      mem_rotate_right (x->parent);
	      w = x->parent->left;
            }

	  if (w->right->color == MEM_BLACK && w->left->color == MEM_BLACK)
	    {
	      w->color = MEM_RED;
	      x = x->parent;
            }
	  else
	    {
	      if (w->left->color == MEM_BLACK)
		{
		  w->right->color = MEM_BLACK;
		  w->color = MEM_RED;
		  mem_rotate_left (w);
		  w = x->parent->left;
                }

	      w->color = x->parent->color;
	      x->parent->color = MEM_BLACK;
	      w->left->color = MEM_BLACK;
	      mem_rotate_right (x->parent);
	      x = mem_root;
            }
        }
    }

  x->color = MEM_BLACK;
}

/* Return P "made whole" as a Lisp_String if P's mem_block M
   corresponds to a Lisp_String data field.  */

static struct Lisp_String *
live_string_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_STRING);
  struct string_block *b = m->start;
  char *cp = p;
  ptrdiff_t distance = cp - (char *) &b->strings[0];

  /* P must point into a Lisp_String structure, and it
     must not be on the free list.  */
  if (0 <= distance && distance < sizeof b->strings)
    {
      ptrdiff_t off = distance % sizeof b->strings[0];
      /* Since compilers can optimize away struct fields, scan all
	 offsets.  See Bug#28213.  */
      if (off == Lisp_String
	  || off == 0
	  || off == offsetof (struct Lisp_String, u.s.size_byte)
	  || off == offsetof (struct Lisp_String, u.s.intervals)
	  || off == offsetof (struct Lisp_String, u.s.data))
	{
	  struct Lisp_String *s = p = cp -= off;
	  if (s->u.s.data)
	    return s;
	}
    }
  return NULL;
}

static bool
live_string_p (struct mem_node *m, void *p)
{
  return live_string_holding (m, p) == p;
}

/* Return P "made whole" as a Lisp_Cons if P's mem_block M
   corresponds to a Lisp_Cons data field.  */

static struct Lisp_Cons *
live_cons_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_CONS);
  struct cons_block *b = m->start;
  char *cp = p;
  ptrdiff_t distance = cp - (char *) &b->conses[0];

  /* P must point into a Lisp_Cons, not be
     one of the unused cells in the current cons block,
     and not be on the free list.  */
  if (0 <= distance && distance < sizeof b->conses
      && (b != cons_block
	  || distance / sizeof b->conses[0] < cons_block_index))
    {
      ptrdiff_t off = distance % sizeof b->conses[0];
      if (off == Lisp_Cons
	  || off == 0
	  || off == offsetof (struct Lisp_Cons, u.s.u.cdr))
	{
	  struct Lisp_Cons *s = p = cp -= off;
	  if (! deadp (s->u.s.car))
	    return s;
	}
    }
  return NULL;
}

static bool
live_cons_p (struct mem_node *m, void *p)
{
  return live_cons_holding (m, p) == p;
}


/* Return P "made whole" as a Lisp_Symbol if P's mem_block M
   corresponds to a Lisp_Symbol data field.  */

static struct Lisp_Symbol *
live_symbol_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_SYMBOL);
  struct symbol_block *b = m->start;
  char *cp = p;
  ptrdiff_t distance = cp - (char *) &b->symbols[0];

  /* P must point into the Lisp_Symbol, not be
     one of the unused cells in the current symbol block,
     and not be on the free list.  */
  if (0 <= distance && distance < sizeof b->symbols
      && (b != symbol_block
	  || distance / sizeof b->symbols[0] < symbol_block_index))
    {
      ptrdiff_t off = distance % sizeof b->symbols[0];
      if (off == Lisp_Symbol

	  /* Plain '|| off == 0' would run afoul of GCC 10.2
	     -Wlogical-op, as Lisp_Symbol happens to be zero.  */
	  || (Lisp_Symbol != 0 && off == 0)

	  || off == offsetof (struct Lisp_Symbol, u.s.name)
	  || off == offsetof (struct Lisp_Symbol, u.s.val)
	  || off == offsetof (struct Lisp_Symbol, u.s.function)
	  || off == offsetof (struct Lisp_Symbol, u.s.plist)
	  || off == offsetof (struct Lisp_Symbol, u.s.next))
	{
	  struct Lisp_Symbol *s = p = cp -= off;
	  if (! deadp (s->u.s.function))
	    return s;
	}
    }
  return NULL;
}

static bool
live_symbol_p (struct mem_node *m, void *p)
{
  return live_symbol_holding (m, p) == p;
}


/* Return P "made whole" as a Lisp_Float if P's mem_block M
   corresponds to a Lisp_Float data field.  */

static struct Lisp_Float *
live_float_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_FLOAT);
  struct float_block *b = m->start;
  char *cp = p;
  ptrdiff_t distance = cp - (char *) &b->floats[0];

  /* P must point to (or be a tagged pointer to) the start of a
     Lisp_Float and not be one of the unused cells in the current
     float block.  */
  if (0 <= distance && distance < sizeof b->floats)
    {
      int off = distance % sizeof b->floats[0];
      if ((off == Lisp_Float || off == 0)
	  && (b != float_block
	      || distance / sizeof b->floats[0] < float_block_index))
	{
	  p = cp - off;
	  return p;
	}
    }
  return NULL;
}

static bool
live_float_p (struct mem_node *m, void *p)
{
  return live_float_holding (m, p) == p;
}

/* Return VECTOR if P points within it, NULL otherwise.  */

static struct Lisp_Vector *
live_vector_pointer (struct Lisp_Vector *vector, void *p)
{
  void *vvector = vector;
  char *cvector = vvector;
  char *cp = p;
  ptrdiff_t distance = cp - cvector;
  return ((distance == Lisp_Vectorlike
	   || distance == 0
	   || (sizeof vector->header <= distance
	       && distance < vector_nbytes (vector)
	       && (! (vector->header.size & PSEUDOVECTOR_FLAG)
		   ? (offsetof (struct Lisp_Vector, contents) <= distance
		      && (((distance - offsetof (struct Lisp_Vector, contents))
			   % word_size)
			  == 0))
		   /* For non-bool-vector pseudovectors, treat any pointer
		      past the header as valid since it's too much of a pain
		      to write special-case code for every pseudovector.  */
		   : (PVTYPE (vector) != PVEC_BOOL_VECTOR
		      || distance == offsetof (struct Lisp_Bool_Vector, size)
		      || (offsetof (struct Lisp_Bool_Vector, data) <= distance
			  && (((distance
				- offsetof (struct Lisp_Bool_Vector, data))
			       % sizeof (bits_word))
			      == 0))))))
	  ? vector : NULL);
}

/* Return M "made whole" as a large Lisp_Vector if P points within it.  */

static struct Lisp_Vector *
live_large_vector_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_VECTORLIKE);
  return live_vector_pointer (large_vector_contents (m->start), p);
}

static bool
live_large_vector_p (struct mem_node *m, void *p)
{
  return live_large_vector_holding (m, p) == p;
}

/* Return M "made whole" as a non-large Lisp_Vector if P points within it.  */

static struct Lisp_Vector *
live_small_vector_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_VBLOCK);
  struct Lisp_Vector *vp = p;
  struct vector_block *block = m->start;
  struct Lisp_Vector *vector = (struct Lisp_Vector *) block->data;

  /* P is in the block's allocation range.  Scan the block
     up to P and see whether P points to the start of some
     vector which is not on a free list.  FIXME: check whether
     some allocation patterns (probably a lot of short vectors)
     may cause a substantial overhead of this loop.  */
  while (VECTOR_IN_BLOCK (vector, block) && vector <= vp)
    {
      struct Lisp_Vector *next = ADVANCE (vector, vector_nbytes (vector));
      if (vp < next && PVTYPE (vector) != PVEC_FREE)
	return live_vector_pointer (vector, vp);
      vector = next;
    }
  return NULL;
}

static bool
live_small_vector_p (struct mem_node *m, void *p)
{
  return live_small_vector_holding (m, p) == p;
}

/* Workhorse of conservative stack scanning.

   If P looks like it points to Lisp data, mark it and return true.  */

static bool
mark_maybe_pointer (void *const * p)
{
  bool ret = false;
  uintptr_t mask = VALMASK & UINTPTR_MAX;
  struct mem_node *m;
  enum Space_Type xpntr_type;
  void *xpntr;

  /* Research Bug#41321 so we can suitably #ifdef treatment of
     Lisp_Symbol pointers being split across non-contiguous registers.
  */
  void *p_sym;
  INT_ADD_WRAPV ((uintptr_t) *p, (uintptr_t) lispsym, (uintptr_t *) &p_sym);

#if USE_VALGRIND
  VALGRIND_MAKE_MEM_DEFINED (p, sizeof (*p));
#endif

  if (pdumper_object_p (*p))
    {
      uintptr_t masked_p = (uintptr_t) *p & mask;
      void *po = (void *) masked_p;
      char *cp = *p;
      char *cpo = po;
      int type = pdumper_find_object_type (po);
      ret = (pdumper_valid_object_type_p (type)
	     // Verify P’s tag, if any, matches pdumper-reported type.
	     && (! USE_LSB_TAG || *p == po || cp - cpo == type));
      if (ret)
	{
	  if (type == Lisp_Symbol)
	    mark_automatic_object (make_lisp_symbol (po));
	  else
	    mark_automatic_object (make_lisp_ptr (po, type));
	}
    }
  else if (pdumper_object_p (p_sym))
    {
      uintptr_t masked_p = (uintptr_t) p_sym & mask;
      void *po = (void *) masked_p;
      char *cp = p_sym;
      char *cpo = po;
      ret = (pdumper_find_object_type (po) == Lisp_Symbol
	     // Verify P’s tag, if any, matches pdumper-reported type.
	     && (! USE_LSB_TAG || p_sym == po || cp - cpo == Lisp_Symbol));
      if (ret)
	mark_automatic_object (make_lisp_symbol (po));
    }
  else if ((xpntr_type = mgc_find_xpntr (*p, &xpntr)) != Space_Type_Max)
    {
      /* analogous logic to set_string_marked() */
      void *forwarded = mgc_fwd_xpntr (xpntr);
      if (! forwarded)
	{
	  ret = true;
	  if ((enum Lisp_Type) xpntr_type < Lisp_Type_Max)
	    mark_automatic_object (make_lisp_ptr (xpntr, (enum Lisp_Type) xpntr_type));
	  else if (xpntr_type == Space_Interval)
	    mark_interval_tree ((INTERVAL *) &xpntr);
	  else
	    emacs_abort ();
	  forwarded = mgc_fwd_xpntr (xpntr);
	}
      eassert (forwarded);
      ptrdiff_t offset;
      INT_SUBTRACT_WRAPV ((uintptr_t) *p, (uintptr_t) xpntr, &offset);
      INT_ADD_WRAPV ((uintptr_t) forwarded, offset, (uintptr_t *) p);
    }
  else if ((m = mem_find (*p)) != MEM_NIL)
    {
      switch (m->type)
	{
	case MEM_TYPE_NON_LISP:
	  break;

	case MEM_TYPE_CONS:
	  {
	    struct Lisp_Cons *h = live_cons_holding (m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_Cons));
		ret = true;
	      }
	  }
	  break;

	case MEM_TYPE_STRING:
	  {
	    struct Lisp_String *h = live_string_holding (m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_String));
		ret = true;
	      }
	  }
	  break;

	case MEM_TYPE_SYMBOL:
	  {
	    struct Lisp_Symbol *h = live_symbol_holding (m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_symbol (h));
		ret = true;
	      }
	  }
	  break;

	case MEM_TYPE_FLOAT:
	  {
	    struct Lisp_Float *h = live_float_holding (m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_Float));
		ret = true;
	      }
	  }
	  break;

	case MEM_TYPE_VECTORLIKE:
	  {
	    struct Lisp_Vector *h = live_large_vector_holding (m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_Vectorlike));
		ret = true;
	      }
	  }
	  break;

	case MEM_TYPE_VBLOCK:
	  {
	    struct Lisp_Vector *h = live_small_vector_holding (m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_Vectorlike));
		ret = true;
	      }
	  }
	  break;

	default:
	  emacs_abort ();
	}
    }
  else if ((m = mem_find (p_sym)) != MEM_NIL && m->type == MEM_TYPE_SYMBOL)
    {
      struct Lisp_Symbol *h = live_symbol_holding (m, p_sym);
      if (h)
	{
	  mark_automatic_object (make_lisp_symbol (h));
	  ret = true;
	}
    }
  return ret;
}

/* Alignment of pointer values.  Use alignof, as it sometimes returns
   a smaller alignment than GCC's __alignof__ and mark_memory might
   miss objects if __alignof__ were used.  */
#define GC_POINTER_ALIGNMENT alignof (void *)

/* Mark live Lisp objects on the C stack.

   There are several system-dependent problems to consider when
   porting this to new architectures:

   Processor Registers

   We have to mark Lisp objects in CPU registers that can hold local
   variables or are used to pass parameters.

   If __builtin_unwind_init is available, it should suffice to save
   registers in flush_stack_call_func().  This presumably is always
   the case for platforms of interest to Commercial Emacs.  We
   preserve the legacy else-branch that calls test_setjmp() to verify
   the sys_jmp_buf saves registers.

   Stack Layout

   Architectures differ in the way their processor stack is organized.
   For example, the stack might look like this

     +----------------+
     |  Lisp_Object   |  size = 4
     +----------------+
     | something else |  size = 2
     +----------------+
     |  Lisp_Object   |  size = 4
     +----------------+
     |	...	      |

   In such a case, not every Lisp_Object will be aligned equally.  To
   find all Lisp_Object on the stack it won't be sufficient to walk
   the stack in steps of 4 bytes.  Instead, two passes will be
   necessary, one starting at the start of the stack, and a second
   pass starting at the start of the stack + 2.  Likewise, if the
   minimal alignment of Lisp_Objects on the stack is 1, four passes
   would be necessary, each one starting with one byte more offset
   from the stack start.  */

void ATTRIBUTE_NO_SANITIZE_ADDRESS
mark_memory (void const *start, void const *end)
{
  char const *pp;

  /* Allow inverted arguments.  */
  if (end < start)
    {
      void const *tem = start;
      start = end;
      end = tem;
    }

  eassert (((uintptr_t) start) % GC_POINTER_ALIGNMENT == 0);

  /* Ours is not a "precise" gc, in which all object references
     are unambiguous and markable.  Here, for example,

       Lisp_Object obj = build_string ("test");
       struct Lisp_String *ptr = XSTRING (obj);
       garbage_collect ();
       fprintf (stderr, "test '%s'\n", ptr->u.s.data);

     the compiler is liable to optimize away OBJ, so our
     "conservative" gc must recognize that PTR references Lisp
     data.  */

  for (pp = start; (void const *) pp < end; pp += GC_POINTER_ALIGNMENT)
    {
      // void *p = *(void *const *) pp;
      mark_maybe_pointer ((void *const *) pp);
    }
}

#ifndef HAVE___BUILTIN_UNWIND_INIT

# ifdef GC_SETJMP_WORKS
static void
test_setjmp (void)
{
}
# else

static bool setjmp_tested_p;
static int longjmps_done;

/* Perform a quick check if it looks like setjmp saves registers in a
   jmp_buf.  Print a message to stderr saying so.  When this test
   succeeds, this is _not_ a proof that setjmp is sufficient for
   conservative stack marking.  Only the sources or a disassembly
   can prove that.  */

static void
test_setjmp (void)
{
  if (setjmp_tested_p)
    return;
  setjmp_tested_p = true;
  char buf[10];
  register int x;
  sys_jmp_buf jbuf;

  /* Arrange for X to be put in a register.  */
  sprintf (buf, "1");
  x = strlen (buf);
  x = 2 * x - 1;

  sys_setjmp (jbuf);
  if (longjmps_done == 1)
    {
      /* Gets here after the sys_longjmp().  */
      if (x != 1)
	/* Didn't restore the register before the setjmp!  */
	emacs_abort ();
    }

  ++longjmps_done;
  x = 2;
  if (longjmps_done == 1)
    sys_longjmp (jbuf, 1);
}
# endif /* ! GC_SETJMP_WORKS */
#endif /* ! HAVE___BUILTIN_UNWIND_INIT */

/* The type of an object near the stack top, whose address can be used
   as a stack scan limit.  */
typedef union
{
  /* Make sure stack_top and m_stack_bottom are properly aligned as GC
     expects.  */
  Lisp_Object o;
  void *p;
#ifndef HAVE___BUILTIN_UNWIND_INIT
  sys_jmp_buf j;
  char c;
#endif
} stacktop_sentry;

/* Yield an address close enough to the top of the stack that the
   garbage collector need not scan above it.  Callers should be
   declared NO_INLINE.  */
#ifdef HAVE___BUILTIN_FRAME_ADDRESS
# define NEAR_STACK_TOP(addr) ((void) (addr), __builtin_frame_address (0))
#else
# define NEAR_STACK_TOP(addr) (addr)
#endif

/* Set *P to the address of the top of the stack.  This must be a
   macro, not a function, so that it is executed in the caller's
   environment.  It is not inside a do-while so that its storage
   survives the macro.  Callers should be declared NO_INLINE.  */
#ifdef HAVE___BUILTIN_UNWIND_INIT
# define SET_STACK_TOP_ADDRESS(p)	\
   stacktop_sentry sentry;		\
   *(p) = NEAR_STACK_TOP (&sentry)
#else
# define SET_STACK_TOP_ADDRESS(p)		\
   stacktop_sentry sentry;			\
   test_setjmp ();				\
   sys_setjmp (sentry.j);			\
   *(p) = NEAR_STACK_TOP (&sentry + (stack_bottom < &sentry.c))
#endif
NO_INLINE /* Ensures registers are spilled. (Bug#41357)  */
void
flush_stack_call_func1 (void (*func) (void *arg), void *arg)
{
  void *end;
  struct thread_state *self = current_thread;
  SET_STACK_TOP_ADDRESS (&end);
  self->stack_top = end;
  func (arg);
  eassert (current_thread == self);
}

/* Determine whether it is safe to access memory at address P.  */
static int
valid_pointer_p (void *p)
{
#ifdef WINDOWSNT
  return w32_valid_pointer_p (p, 16);
#else

  if (ADDRESS_SANITIZER)
    return p ? -1 : 0;

  int fd[2];
  static int under_rr_state;

  if (!under_rr_state)
    under_rr_state = getenv ("RUNNING_UNDER_RR") ? -1 : 1;
  if (under_rr_state < 0)
    return under_rr_state;

  /* Obviously, we cannot just access it (we would SEGV trying), so we
     trick the o/s to tell us whether p is a valid pointer.
     Unfortunately, we cannot use NULL_DEVICE here, as emacs_write may
     not validate p in that case.  */

  if (emacs_pipe (fd) == 0)
    {
      bool valid = emacs_write (fd[1], p, 16) == 16;
      emacs_close (fd[1]);
      emacs_close (fd[0]);
      return valid;
    }

  return -1;
#endif
}

/* Return 2 if OBJ is a killed or special buffer object, 1 if OBJ is a
   valid lisp object, 0 if OBJ is NOT a valid lisp object, or -1 if we
   cannot validate OBJ.  This function can be quite slow, and is used
   only in debugging.  */

int
valid_lisp_object_p (Lisp_Object obj)
{
  if (FIXNUMP (obj))
    return 1;

  void *p = XPNTR (obj);
  if (PURE_P (p) || mgc_xpntr_p (p))
    return 1;

  if (SYMBOLP (obj) && c_symbol_p (p))
    return ((char *) p - (char *) lispsym) % sizeof lispsym[0] == 0;

  if (p == &buffer_slot_defaults || p == &buffer_slot_symbols)
    return 2;

  if (pdumper_object_p (p))
    return pdumper_object_p_precise (p) ? 1 : 0;

  struct mem_node *m = mem_find (p);

  if (m == MEM_NIL)
    {
      int valid = valid_pointer_p (p);
      if (valid <= 0)
	return valid;

      if (SUBRP (obj))
	return 1;

      return 0;
    }

  switch (m->type)
    {
    case MEM_TYPE_NON_LISP:
      return 0;

    case MEM_TYPE_CONS:
      return live_cons_p (m, p);

    case MEM_TYPE_STRING:
      return live_string_p (m, p);

    case MEM_TYPE_SYMBOL:
      return live_symbol_p (m, p);

    case MEM_TYPE_FLOAT:
      return live_float_p (m, p);

    case MEM_TYPE_VECTORLIKE:
      return live_large_vector_p (m, p);

    case MEM_TYPE_VBLOCK:
      return live_small_vector_p (m, p);

    default:
      break;
    }

  return 0;
}

/* Allocate room for SIZE bytes from pure Lisp storage and return a
   pointer to it.  TYPE is the Lisp type for which the memory is
   allocated.  TYPE < 0 means it's not used for a Lisp object,
   and that the result should have an alignment of -TYPE.

   The bytes are initially zero.

   If pure space is exhausted, allocate space from the heap.  This is
   merely an expedient to let Emacs warn that pure space was exhausted
   and that Emacs should be rebuilt with a larger pure space.  */

static void *
pure_alloc (size_t size, int type)
{
  void *result;

 again:
  if (type >= 0)
    {
      /* Allocate space for a Lisp object from the beginning of the free
	 space with taking account of alignment.  */
      result = pointer_align (purebeg + pure_bytes_used_lisp, LISP_ALIGNMENT);
      pure_bytes_used_lisp = ((char *)result - (char *)purebeg) + size;
    }
  else
    {
      /* Allocate space for a non-Lisp object from the end of the free
	 space.  */
      ptrdiff_t unaligned_non_lisp = pure_bytes_used_non_lisp + size;
      char *unaligned = purebeg + pure_size - unaligned_non_lisp;
      int decr = (intptr_t) unaligned & (-1 - type);
      pure_bytes_used_non_lisp = unaligned_non_lisp + decr;
      result = unaligned - decr;
    }
  pure_bytes_used = pure_bytes_used_lisp + pure_bytes_used_non_lisp;

  if (pure_bytes_used <= pure_size)
    return result;

  /* Don't allocate a large amount here,
     because it might get mmap'd and then its address
     might not be usable.  */
  int small_amount = 10000;
  eassert (size <= small_amount - LISP_ALIGNMENT);
  purebeg = xzalloc (small_amount);
  pure_size = small_amount;
  pure_bytes_used_before_overflow += pure_bytes_used - size;
  pure_bytes_used = 0;
  pure_bytes_used_lisp = pure_bytes_used_non_lisp = 0;

  /* Can't GC if pure storage overflowed because we can't determine
     if something is a pure object or not.  */
  gc_inhibited = true;
  goto again;
}

/* Find the byte sequence {DATA[0], ..., DATA[NBYTES-1], '\0'} from
   the non-Lisp data pool of the pure storage, and return its start
   address.  Return NULL if not found.  */

static char *
find_string_data_in_pure (const char *data, ptrdiff_t nbytes)
{
  int i;
  ptrdiff_t skip, bm_skip[256], last_char_skip, infinity, start, start_max;
  const unsigned char *p;
  char *non_lisp_beg;

  if (pure_bytes_used_non_lisp <= nbytes)
    return NULL;

  /* Set up the Boyer-Moore table.  */
  skip = nbytes + 1;
  for (i = 0; i < 256; i++)
    bm_skip[i] = skip;

  p = (const unsigned char *) data;
  while (--skip > 0)
    bm_skip[*p++] = skip;

  last_char_skip = bm_skip['\0'];

  non_lisp_beg = purebeg + pure_size - pure_bytes_used_non_lisp;
  start_max = pure_bytes_used_non_lisp - (nbytes + 1);

  /* See the comments in the function `boyer_moore' (search.c) for the
     use of `infinity'.  */
  infinity = pure_bytes_used_non_lisp + 1;
  bm_skip['\0'] = infinity;

  p = (const unsigned char *) non_lisp_beg + nbytes;
  start = 0;
  do
    {
      /* Check the last character (== '\0').  */
      do
	{
	  start += bm_skip[*(p + start)];
	}
      while (start <= start_max);

      if (start < infinity)
	/* Couldn't find the last character.  */
	return NULL;

      /* No less than `infinity' means we could find the last
	 character at `p[start - infinity]'.  */
      start -= infinity;

      /* Check the remaining characters.  */
      if (memcmp (data, non_lisp_beg + start, nbytes) == 0)
	/* Found.  */
	return non_lisp_beg + start;

      start += last_char_skip;
    }
  while (start <= start_max);

  return NULL;
}


/* Return a string allocated in pure space.  DATA is a buffer holding
   NCHARS characters, and NBYTES bytes of string data.  MULTIBYTE
   means make the result string multibyte.

   Must get an error if pure storage is full, since if it cannot hold
   a large string it may be able to hold conses that point to that
   string; then the string is not protected from gc.  */

Lisp_Object
make_pure_string (const char *data,
		  ptrdiff_t nchars, ptrdiff_t nbytes, bool multibyte)
{
  Lisp_Object string;
  struct Lisp_String *s = pure_alloc (sizeof *s, Lisp_String);
  s->u.s.data = (unsigned char *) find_string_data_in_pure (data, nbytes);
  if (s->u.s.data == NULL)
    {
      s->u.s.data = pure_alloc (nbytes + 1, -1);
      memcpy (s->u.s.data, data, nbytes);
      s->u.s.data[nbytes] = '\0';
    }
  s->u.s.size = nchars;
  s->u.s.size_byte = multibyte ? nbytes : Sdata_Unibyte;
  s->u.s.intervals = NULL;
  XSETSTRING (string, s);
  return string;
}

/* Return a string allocated in pure space.  Do not
   allocate the string data, just point to DATA.  */

Lisp_Object
make_pure_c_string (const char *data, ptrdiff_t nchars)
{
  Lisp_Object string;
  struct Lisp_String *s = pure_alloc (sizeof *s, Lisp_String);
  s->u.s.size = nchars;
  s->u.s.size_byte = Sdata_Pure;
  s->u.s.data = (unsigned char *) data;
  s->u.s.intervals = NULL;
  XSETSTRING (string, s);
  return string;
}

static Lisp_Object purecopy (Lisp_Object obj);

/* Return a cons allocated from pure space.  Give it pure copies
   of CAR as car and CDR as cdr.  */

Lisp_Object
pure_cons (Lisp_Object car, Lisp_Object cdr)
{
  Lisp_Object new;
  struct Lisp_Cons *p = pure_alloc (sizeof *p, Lisp_Cons);
  XSETCONS (new, p);
  XSETCAR (new, purecopy (car));
  XSETCDR (new, purecopy (cdr));
  return new;
}


/* Value is a float object with value NUM allocated from pure space.  */

static Lisp_Object
make_pure_float (double num)
{
  Lisp_Object new;
  struct Lisp_Float *p = pure_alloc (sizeof *p, Lisp_Float);
  XSETFLOAT (new, p);
  XFLOAT_INIT (new, num);
  return new;
}

/* Value is a bignum object with value VALUE allocated from pure
   space.  */

static Lisp_Object
make_pure_bignum (Lisp_Object value)
{
  mpz_t const *n = xbignum_val (value);
  size_t i, nlimbs = mpz_size (*n);
  size_t nbytes = nlimbs * sizeof (mp_limb_t);
  mp_limb_t *pure_limbs;
  mp_size_t new_size;

  struct Lisp_Bignum *b = pure_alloc (sizeof *b, Lisp_Vectorlike);
  XSETPVECTYPESIZE (b, PVEC_BIGNUM, 0, VECSIZE (struct Lisp_Bignum));

  int limb_alignment = alignof (mp_limb_t);
  pure_limbs = pure_alloc (nbytes, - limb_alignment);
  for (i = 0; i < nlimbs; ++i)
    pure_limbs[i] = mpz_getlimbn (*n, i);

  new_size = nlimbs;
  if (mpz_sgn (*n) < 0)
    new_size = -new_size;

  mpz_roinit_n (b->value, pure_limbs, new_size);

  return make_lisp_ptr (b, Lisp_Vectorlike);
}

/* Return a vector with room for LEN Lisp_Objects allocated from
   pure space.  */

static Lisp_Object
make_pure_vector (ptrdiff_t len)
{
  Lisp_Object new;
  size_t size = header_size + len * word_size;
  struct Lisp_Vector *p = pure_alloc (size, Lisp_Vectorlike);
  XSETVECTOR (new, p);
  XVECTOR (new)->header.size = len;
  return new;
}

/* Copy all contents and parameters of TABLE to a new table allocated
   from pure space, return the purified table.  */
static struct Lisp_Hash_Table *
purecopy_hash_table (struct Lisp_Hash_Table *table)
{
  eassert (NILP (table->weak));
  eassert (table->purecopy);

  struct Lisp_Hash_Table *pure = pure_alloc (sizeof *pure, Lisp_Vectorlike);
  struct hash_table_test pure_test = table->test;

  /* Purecopy the hash table test.  */
  pure_test.name = purecopy (table->test.name);
  pure_test.user_hash_function = purecopy (table->test.user_hash_function);
  pure_test.user_cmp_function = purecopy (table->test.user_cmp_function);

  pure->header = table->header;
  pure->weak = purecopy (Qnil);
  pure->hash = purecopy (table->hash);
  pure->next = purecopy (table->next);
  pure->index = purecopy (table->index);
  pure->count = table->count;
  pure->next_free = table->next_free;
  pure->purecopy = table->purecopy;
  eassert (!pure->mutable);
  pure->rehash_threshold = table->rehash_threshold;
  pure->rehash_size = table->rehash_size;
  pure->key_and_value = purecopy (table->key_and_value);
  pure->test = pure_test;

  return pure;
}

DEFUN ("purecopy", Fpurecopy, Spurecopy, 1, 1, 0,
       doc: /* Make a copy of object OBJ in pure storage.
Recursively copies contents of vectors and cons cells.
Does not copy symbols.  Copies strings without text properties.  */)
  (register Lisp_Object obj)
{
  if (NILP (Vloadup_pure_table))
    return obj;
  else if (MARKERP (obj) || OVERLAYP (obj) || SYMBOLP (obj))
    /* Can't purify those.  */
    return obj;
  else
    return purecopy (obj);
}

/* Pinned objects are marked before every GC cycle.  */
static struct pinned_object
{
  Lisp_Object object;
  struct pinned_object *next;
} *pinned_objects;

static Lisp_Object
purecopy (Lisp_Object obj)
{
  if (FIXNUMP (obj)
      || (! SYMBOLP (obj) && PURE_P (XPNTR (obj)))
      || SUBRP (obj))
    return obj;    /* Already pure.  */

  if (STRINGP (obj) && XSTRING (obj)->u.s.intervals)
    message_with_string ("Dropping text-properties while making string `%s' pure",
			 obj, true);

  if (! NILP (Vloadup_pure_table)) /* Hash consing.  */
    {
      Lisp_Object tmp = Fgethash (obj, Vloadup_pure_table, Qnil);
      if (! NILP (tmp))
	return tmp;
    }

  if (CONSP (obj))
    obj = pure_cons (XCAR (obj), XCDR (obj));
  else if (FLOATP (obj))
    obj = make_pure_float (XFLOAT_DATA (obj));
  else if (STRINGP (obj))
    obj = make_pure_string (SSDATA (obj), SCHARS (obj),
			    SBYTES (obj),
			    STRING_MULTIBYTE (obj));
  else if (HASH_TABLE_P (obj))
    {
      struct Lisp_Hash_Table *table = XHASH_TABLE (obj);
      /* Do not purify hash tables which haven't been defined with
         :purecopy as non-nil or are weak - they aren't guaranteed to
         not change.  */
      if (! NILP (table->weak) || !table->purecopy)
        {
          /* Instead, add the hash table to the list of pinned objects,
             so that it will be marked during GC.  */
          struct pinned_object *o = xmalloc (sizeof *o);
          o->object = obj;
          o->next = pinned_objects;
          pinned_objects = o;
          return obj; /* Don't hash cons it.  */
        }

      struct Lisp_Hash_Table *h = purecopy_hash_table (table);
      XSET_HASH_TABLE (obj, h);
    }
  else if (COMPILEDP (obj) || VECTORP (obj) || RECORDP (obj))
    {
      struct Lisp_Vector *objp = XVECTOR (obj);
      ptrdiff_t nbytes = vector_nbytes (objp);
      struct Lisp_Vector *vec = pure_alloc (nbytes, Lisp_Vectorlike);
      register ptrdiff_t i;
      ptrdiff_t size = ASIZE (obj);
      if (size & PSEUDOVECTOR_FLAG)
	size &= PSEUDOVECTOR_SIZE_MASK;
      memcpy (vec, objp, nbytes);
      for (i = 0; i < size; i++)
	vec->contents[i] = purecopy (vec->contents[i]);
      // Byte code strings must be pinned.
      if (COMPILEDP (obj) && size >= 2 && STRINGP (vec->contents[1])
	  && !STRING_MULTIBYTE (vec->contents[1]))
	pin_string (vec->contents[1]);
      XSETVECTOR (obj, vec);
    }
  else if (SYMBOLP (obj))
    {
      if (! XSYMBOL (obj)->u.s.pinned && ! c_symbol_p (XSYMBOL (obj)))
	{ /* We can't purify them, but they appear in many pure objects.
	     Mark them as `pinned' so we know to mark them at every GC cycle.  */
	  XSYMBOL (obj)->u.s.pinned = true;
	  symbol_block_pinned = symbol_block;
	}
      /* Don't hash-cons it.  */
      return obj;
    }
  else if (BIGNUMP (obj))
    obj = make_pure_bignum (obj);
  else
    {
      AUTO_STRING (fmt, "Don't know how to purify: %S");
      Fsignal (Qerror, list1 (CALLN (Fformat, fmt, obj)));
    }

  if (! NILP (Vloadup_pure_table)) /* Hash consing.  */
    Fputhash (obj, obj, Vloadup_pure_table);

  return obj;
}



/* Put an entry in staticvec, pointing at the variable with address
   VARADDRESS.  */

void
staticpro (Lisp_Object const *varaddress)
{
  for (int i = 0; i < staticidx; i++)
    eassert (staticvec[i] != varaddress);
  if (staticidx >= NSTATICS)
    fatal ("NSTATICS too small; try increasing and recompiling Emacs.");
  staticvec[staticidx++] = varaddress;
}

static void
allow_garbage_collection (void)
{
  gc_inhibited = false;
}

specpdl_ref
inhibit_garbage_collection (void)
{
  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_protect_void (allow_garbage_collection);
  gc_inhibited = true;
  return count;
}

/* Calculate total bytes of live objects.  */

static size_t
total_bytes_of_live_objects (void)
{
  return gcstat.total_conses * sizeof (struct Lisp_Cons)
    + gcstat.total_symbols * sizeof (struct Lisp_Symbol)
    + gcstat.total_string_bytes
    + gcstat.total_vector_slots * word_size
    + gcstat.total_floats * sizeof (struct Lisp_Float)
    + gcstat.total_intervals * sizeof (struct interval)
    + gcstat.total_strings * sizeof (struct Lisp_String);
}

#ifdef HAVE_WINDOW_SYSTEM

/* Remove unmarked font-spec and font-entity objects from ENTRY, which is
   (DRIVER-TYPE NUM-FRAMES FONT-CACHE-DATA ...), and return changed entry.  */

static Lisp_Object
compact_font_cache_entry (Lisp_Object entry)
{
  Lisp_Object tail, *prev = &entry;

  for (tail = entry; CONSP (tail); tail = XCDR (tail))
    {
      bool drop = 0;
      Lisp_Object obj = XCAR (tail);

      /* Consider OBJ if it is (font-spec . [font-entity font-entity ...]).  */
      if (CONSP (obj)
	  && GC_FONT_SPEC_P (XCAR (obj))
	  && ! vectorlike_marked_p (&GC_XFONT_SPEC (XCAR (obj))->header)
	  && VECTORP (XCDR (obj)))
	{
	  ptrdiff_t i, size = ASIZE (XCDR (obj));
	  Lisp_Object obj_cdr = XCDR (obj);

	  /* If font-spec is not marked, most likely all font-entities
	     are not marked too.  But we must be sure that nothing is
	     marked within OBJ before we really drop it.  */
	  for (i = 0; i < size; i++)
            {
              Lisp_Object objlist;

              if (vectorlike_marked_p (
                    &GC_XFONT_ENTITY (AREF (obj_cdr, i))->header))
                break;

              objlist = AREF (AREF (obj_cdr, i), FONT_OBJLIST_INDEX);
              for (; CONSP (objlist); objlist = XCDR (objlist))
                {
                  Lisp_Object val = XCAR (objlist);
                  struct font *font = GC_XFONT_OBJECT (val);

                  if (! NILP (AREF (val, FONT_TYPE_INDEX))
                      && vectorlike_marked_p (&font->header))
                    break;
                }
              if (CONSP (objlist))
		{
		  /* Found a marked font, bail out.  */
		  break;
		}
            }

	  if (i == size)
	    {
	      /* No marked fonts were found, so this entire font
		 entity can be dropped.  */
	      drop = 1;
	    }
	}
      if (drop)
	*prev = XCDR (tail);
      else
	prev = xcdr_addr (tail);
    }
  return entry;
}

/* Compact font caches on all terminals and mark
   everything which is still here after compaction.  */

static void
compact_font_caches (void)
{
  struct terminal *t;

  for (t = terminal_list; t; t = t->next_terminal)
    {
      Lisp_Object cache = TERMINAL_FONT_CACHE (t);
      /* Inhibit compacting the caches if the user so wishes.  Some of
	 the users don't mind a larger memory footprint, but do mind
	 slower redisplay.  */
      if (!inhibit_compacting_font_caches
	  && CONSP (cache))
	{
	  Lisp_Object entry;

	  for (entry = XCDR (cache); CONSP (entry); entry = XCDR (entry))
	    XSETCAR (entry, compact_font_cache_entry (XCAR (entry)));
	}
      mark_object (&cache);
    }
}

#else /* not HAVE_WINDOW_SYSTEM */

#define compact_font_caches() (void)(0)

#endif /* HAVE_WINDOW_SYSTEM */

/* Remove (MARKER . DATA) entries with unmarked MARKER
   from buffer undo LIST and return changed list.  */

static Lisp_Object
compact_undo_list (Lisp_Object list)
{
  Lisp_Object tail, *prev = &list;

  for (tail = list; CONSP (tail); tail = XCDR (tail))
    {
      if (CONSP (XCAR (tail))
	  && MARKERP (XCAR (XCAR (tail)))
	  && !vectorlike_marked_p (&XMARKER (XCAR (XCAR (tail)))->header))
	*prev = XCDR (tail);
      else
	prev = xcdr_addr (tail);
    }
  return list;
}

static void
mark_pinned_objects (void)
{
  for (struct pinned_object *pobj = pinned_objects; pobj; pobj = pobj->next)
    mark_object (&pobj->object);
}

static void
mark_pinned_symbols (void)
{
  struct symbol_block *sblk;
  int lim = (symbol_block_pinned == symbol_block
	     ? symbol_block_index : BLOCK_NSYMBOLS);

  for (sblk = symbol_block_pinned; sblk; sblk = sblk->next)
    {
      struct Lisp_Symbol *sym = sblk->symbols, *end = sym + lim;
      for (; sym < end; ++sym)
	if (sym->u.s.pinned)
	  mark_automatic_object (make_lisp_symbol (sym));

      lim = BLOCK_NSYMBOLS;
    }
}

static void
mark_most_objects (void)
{
  struct Lisp_Vector *vbuffer_slot_defaults =
    (struct Lisp_Vector *) &buffer_slot_defaults;
  struct Lisp_Vector *vbuffer_slot_symbols =
    (struct Lisp_Vector *) &buffer_slot_symbols;

  for (int i = 0; i < BUFFER_LISP_SIZE; ++i)
    {
      mark_object (&vbuffer_slot_defaults->contents[i]);
      mark_object (&vbuffer_slot_symbols->contents[i]);
    }

  for (int i = 0; i < ARRAYELTS (lispsym); ++i)
    mark_automatic_object (builtin_lisp_symbol (i));

  // defvar_lisp calls staticpro.
  for (int i = 0; i < staticidx; ++i)
    mark_object ((Lisp_Object *)staticvec[i]);
}

/* List of weak hash tables we found during marking the Lisp heap.
   NULL on entry to garbage_collect and after it returns.  */
static struct Lisp_Hash_Table *weak_hash_tables;

static void
mark_and_sweep_weak_table_contents (void)
{
  /* Mark all keys and values that are in use.  Keep on marking until
     there is no more change.  This is necessary for cases like
     value-weak table A containing an entry X -> Y, where Y is used in a
     key-weak table B, Z -> Y.  If B comes after A in the list of weak
     tables, X -> Y might be removed from A, although when looking at B
     one finds that it shouldn't.  */
  for (bool marked = true; marked; )
    {
      marked = false;
      for (struct Lisp_Hash_Table *h = weak_hash_tables;
	   h != NULL;
	   h = h->next_weak)
        marked |= sweep_weak_table (h, false);
    }

  /* Remove hash table entries that aren't used.  */
  while (weak_hash_tables)
    {
      struct Lisp_Hash_Table *h = weak_hash_tables;
      weak_hash_tables = h->next_weak;
      h->next_weak = NULL;
      sweep_weak_table (h, true);
    }
}

/* The looser of the threshold and percentage constraints prevails.  */
static void
update_bytes_between_gc (void)
{
  intmax_t threshold0 = gc_cons_threshold;
  intmax_t threshold1 = FLOATP (Vgc_cons_percentage)
    ? XFLOAT_DATA (Vgc_cons_percentage) * total_bytes_of_live_objects ()
    : threshold0;
  bytes_between_gc = max (threshold0, threshold1);
}

/* Immediately adjust bytes_between_gc for changes to
   gc-cons-threshold.  */
static Lisp_Object
watch_gc_cons_threshold (Lisp_Object symbol, Lisp_Object newval,
			 Lisp_Object operation, Lisp_Object where)
{
  if (INTEGERP (newval))
    {
      intmax_t threshold;
      if (integer_to_intmax (newval, &threshold))
	{
	  gc_cons_threshold = max (threshold, GC_DEFAULT_THRESHOLD >> 3);
	  update_bytes_between_gc ();
	}
    }
  return Qnil;
}

/* Immediately adjust bytes_between_gc for changes to
   gc-cons-percentage.  */
static Lisp_Object
watch_gc_cons_percentage (Lisp_Object symbol, Lisp_Object newval,
			  Lisp_Object operation, Lisp_Object where)
{
  if (FLOATP (newval))
    {
      Vgc_cons_percentage = newval;
      update_bytes_between_gc ();
    }
  return Qnil;
}

static inline bool mark_stack_empty_p (void);

/* Subroutine of Fgarbage_collect that does most of the work.  */
void
garbage_collect (void)
{
  static struct timespec gc_elapsed = {0, 0};
  Lisp_Object tail, buffer;
  bool message_p = false;
  specpdl_ref count = SPECPDL_INDEX ();
  struct timespec start;

  eassert (weak_hash_tables == NULL);

  if (gc_inhibited || gc_in_progress)
    return;

  gc_in_progress = true;

  eassert (mark_stack_empty_p ());

  /* Show up in profiler.  */
  record_in_backtrace (QAutomatic_GC, 0, 0);

  /* Do this early in case user quits.  */
  FOR_EACH_LIVE_BUFFER (tail, buffer)
    compact_buffer (XBUFFER (buffer));

  size_t tot_before = (profiler_memory_running
		       ? total_bytes_of_live_objects ()
		       : (size_t) -1);

  start = current_timespec ();

  /* Restore what's currently displayed in the echo area.  */
  if (NILP (Vmemory_full))
    {
      message_p = push_message ();
      record_unwind_protect_void (pop_message_unwind);
    }

  if (garbage_collection_messages)
    message1_nolog ("Garbage collecting...");

  block_input ();

  shrink_regexp_cache ();

  mark_most_objects ();
  mark_pinned_objects ();
  mark_pinned_symbols ();
  mark_lread ();
  mark_terminals ();
  mark_kboards ();
  mark_threads ();

#ifdef HAVE_PGTK
  mark_pgtkterm ();
#endif

#ifdef USE_GTK
  xg_mark_data ();
#endif

#ifdef HAVE_HAIKU
  mark_haiku_display ();
#endif

#ifdef HAVE_WINDOW_SYSTEM
  mark_fringe_data ();
#endif

#ifdef HAVE_X_WINDOWS
  mark_xterm ();
#endif

  /* Everything is now marked, except for font caches, undo lists, and
     finalizers.  The first two admit compaction before marking.
     All finalizers, even unmarked ones, need to run after sweep,
     so survive the unmarked ones in doomed_finalizers.  */

  compact_font_caches ();

  FOR_EACH_LIVE_BUFFER (tail, buffer)
    {
      struct buffer *b = XBUFFER (buffer);
      if (! EQ (BVAR (b, undo_list), Qt))
	bset_undo_list (b, compact_undo_list (BVAR (b, undo_list)));
      mark_object (&BVAR (b, undo_list));
    }

  queue_doomed_finalizers (&doomed_finalizers, &finalizers);
  mark_finalizer_list (&doomed_finalizers);

  /* Must happen after all other marking and before gc_sweep.  */
  mark_and_sweep_weak_table_contents ();
  eassert (weak_hash_tables == NULL);

  eassert (mark_stack_empty_p ());

  gc_sweep ();

  unmark_main_thread ();

  bytes_since_gc = 0;

  update_bytes_between_gc ();

  /* Unblock as late as possible since it could signal (Bug#43389).  */
  unblock_input ();

  if (garbage_collection_messages && NILP (Vmemory_full))
    {
      if (message_p || minibuf_level > 0)
	restore_message ();
      else
	message1_nolog ("Garbage collecting...done");
    }

  unbind_to (count, Qnil);

  /* GC is complete: now we can run our finalizer callbacks.  */
  run_finalizers (&doomed_finalizers);

  if (! NILP (Vpost_gc_hook))
    {
      specpdl_ref gc_count = inhibit_garbage_collection ();
      safe_run_hooks (Qpost_gc_hook);
      unbind_to (gc_count, Qnil);
    }

  gc_in_progress = false;
  gc_elapsed = timespec_add (gc_elapsed,
			     timespec_sub (current_timespec (), start));
  Vgc_elapsed = make_float (timespectod (gc_elapsed));
  gcs_done++;

  /* Collect profiling data.  */
  if (tot_before != (size_t) -1)
    {
      size_t tot_after = total_bytes_of_live_objects ();
      if (tot_after < tot_before)
	malloc_probe (min (tot_before - tot_after, SIZE_MAX));
    }
}

DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 0, "",
       doc: /* Reclaim storage for no longer referenced objects.
For further details, see Info node `(elisp)Garbage Collection'.  */)
  (void)
{
  garbage_collect ();
  return Fgc_counts ();
}

DEFUN ("gc-counts", Fgc_counts, Sgc_counts, 0, 0, 0,
       doc: /* Return a list of entries of the form (NAME SIZE USED FREE), where:
- NAME is the Lisp data type, e.g., "conses".
- SIZE is per-object bytes.
- USED is the live count.
- FREE is the free-list count, i.e., reclaimed and redeployable objects.
*/)
  (void)
{
  Lisp_Object total[] = {
    list4 (Qconses, make_fixnum (sizeof (struct Lisp_Cons)),
	   make_int (gcstat.total_conses),
	   make_int (gcstat.total_free_conses)),
    list4 (Qsymbols, make_fixnum (sizeof (struct Lisp_Symbol)),
	   make_int (gcstat.total_symbols),
	   make_int (gcstat.total_free_symbols)),
    list4 (Qstrings, make_fixnum (sizeof (struct Lisp_String)),
	   make_int (gcstat.total_strings),
	   make_int (gcstat.total_free_strings)),
    list3 (Qstring_bytes, make_fixnum (1),
	   make_int (gcstat.total_string_bytes)),
    list3 (Qvectors,
	   make_fixnum (header_size + sizeof (Lisp_Object)),
	   make_int (gcstat.total_vectors)),
    list4 (Qvector_slots, make_fixnum (word_size),
	   make_int (gcstat.total_vector_slots),
	   make_int (gcstat.total_free_vector_slots)),
    list4 (Qfloats, make_fixnum (sizeof (struct Lisp_Float)),
	   make_int (gcstat.total_floats),
	   make_int (gcstat.total_free_floats)),
    list4 (Qintervals, make_fixnum (sizeof (struct interval)),
	   make_int (gcstat.total_intervals),
	   make_int (gcstat.total_free_intervals)),
    list3 (Qbuffers, make_fixnum (sizeof (struct buffer)),
	   make_int (gcstat.total_buffers)),
  };
  return CALLMANY (Flist, total);
}

DEFUN ("garbage-collect-maybe", Fgarbage_collect_maybe,
Sgarbage_collect_maybe, 1, 1, 0,
       doc: /* Call `garbage-collect' if enough allocation happened.
FACTOR determines what "enough" means here:
If FACTOR is a positive number N, it means to run GC if more than
1/Nth of the allocations needed to trigger automatic allocation took
place.
Therefore, as N gets higher, this is more likely to perform a GC.
Returns non-nil if GC happened, and nil otherwise.  */)
  (Lisp_Object factor)
{
  CHECK_FIXNAT (factor);
  EMACS_INT fact = XFIXNAT (factor);

  if (fact >= 1 && bytes_since_gc > bytes_between_gc / fact)
    {
      garbage_collect ();
      return Qt;
    }
  return Qnil;
}

/* Mark Lisp objects in glyph matrix MATRIX.  Currently the
   only interesting objects referenced from glyphs are strings.  */

static void
mark_glyph_matrix (struct glyph_matrix *matrix)
{
  struct glyph_row *row = matrix->rows;
  struct glyph_row *end = row + matrix->nrows;

  for (; row < end; ++row)
    if (row->enabled_p)
      {
	int area;
	for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	  {
	    struct glyph *glyph = row->glyphs[area];
	    struct glyph *end_glyph = glyph + row->used[area];

	    for (; glyph < end_glyph; ++glyph)
	      if (STRINGP (glyph->object)
		  && ! string_marked_p (XSTRING (glyph->object)))
		mark_object (&glyph->object);
	  }
      }
}

/* Whether to remember a few of the last marked values for debugging.  */
#define GC_REMEMBER_LAST_MARKED 0

#if GC_REMEMBER_LAST_MARKED
enum { LAST_MARKED_SIZE = 1 << 9 }; /* Must be a power of 2.  */
Lisp_Object last_marked[LAST_MARKED_SIZE] EXTERNALLY_VISIBLE;
static int last_marked_index;
#endif

/* Whether to enable the mark_object_loop_halt debugging feature.  */
#define GC_CDR_COUNT 0

#if GC_CDR_COUNT
/* For debugging--call abort when we cdr down this many
   links of a list, in mark_object.  In debugging,
   the call to abort will hit a breakpoint.
   Normally this is zero and the check never goes off.  */
ptrdiff_t mark_object_loop_halt EXTERNALLY_VISIBLE;
#endif

static void
mark_vectorlike (union vectorlike_header *header)
{
  struct Lisp_Vector *ptr = (struct Lisp_Vector *) header;
  ptrdiff_t size = ptr->header.size;

  if (size & PSEUDOVECTOR_FLAG)
    size &= PSEUDOVECTOR_SIZE_MASK;

  eassert (! vectorlike_marked_p (header));
  set_vectorlike_marked (header);
  mark_objects (ptr->contents, size);
}

/* Like mark_vectorlike but optimized for char-tables (and
   sub-char-tables) assuming that the contents are mostly integers or
   symbols.  */

static void
mark_char_table (struct Lisp_Vector *ptr, enum pvec_type pvectype)
{
  set_vector_marked (ptr);
  for (int size = ptr->header.size & PSEUDOVECTOR_SIZE_MASK,
	 /* Consult Lisp_Sub_Char_Table layout before changing this.  */
	 i = (pvectype == PVEC_SUB_CHAR_TABLE ? SUB_CHAR_TABLE_OFFSET : 0);
       i < size;
       ++i)
    {
      Lisp_Object *val = &ptr->contents[i];
      if (! FIXNUMP (*val) &&
	  (! SYMBOLP (*val) || ! symbol_marked_p (XSYMBOL (*val))))
	{
	  if (SUB_CHAR_TABLE_P (*val))
	    {
	      if (! vector_marked_p (XVECTOR (*val)))
		mark_char_table (XVECTOR (*val), PVEC_SUB_CHAR_TABLE);
	    }
	  else
	    mark_object (val);
	}
    }
}

/* Mark the chain of overlays starting at PTR.  */

static void
mark_overlay (struct Lisp_Overlay *ptr)
{
  for (; ptr && ! vectorlike_marked_p (&ptr->header); ptr = ptr->next)
    {
      set_vectorlike_marked (&ptr->header);
      /* These two are always markers and can be marked fast.  */
      set_vectorlike_marked (&XMARKER (ptr->start)->header);
      set_vectorlike_marked (&XMARKER (ptr->end)->header);
      mark_object (&ptr->plist);
    }
}

/* Mark Lisp_Objects and special pointers in BUFFER.  */

static void
mark_buffer (struct buffer *buffer)
{
  /* This is handled much like other pseudovectors...  */
  mark_vectorlike (&buffer->header);

  /* ...but there are some buffer-specific things.  */

  mark_interval_tree (&buffer->text->intervals);

  /* For now, we just don't mark the undo_list.  It's done later in
     a special way just before the sweep phase, and after stripping
     some of its elements that are not needed any more.
     Note: this later processing is only done for live buffers, so
     for dead buffers, the undo_list should be nil (set by Fkill_buffer),
     but just to be on the safe side, we mark it here.  */
  if (! BUFFER_LIVE_P (buffer))
      mark_object (&BVAR (buffer, undo_list));

  mark_overlay (buffer->overlays_before);
  mark_overlay (buffer->overlays_after);

  /* If this is an indirect buffer, mark its base buffer.  */
  if (buffer->base_buffer &&
      !vectorlike_marked_p (&buffer->base_buffer->header))
    mark_buffer (buffer->base_buffer);
}

/* Mark Lisp faces in the face cache C.  */

static void
mark_face_cache (struct face_cache *c)
{
  if (c)
    {
      for (int i = 0; i < c->used; i++)
	{
	  struct face *face = FACE_FROM_ID_OR_NULL (c->f, i);

	  if (face)
	    {
	      if (face->font && !vectorlike_marked_p (&face->font->header))
		mark_vectorlike (&face->font->header);

	      mark_objects (face->lface, LFACE_VECTOR_SIZE);
	    }
	}
    }
}

/* Remove killed buffers or items whose car is a killed buffer from
   LIST, and mark other items.  Return changed LIST, which is marked.  */

static Lisp_Object
mark_discard_killed_buffers (Lisp_Object list)
{
  Lisp_Object tail, *prev = &list;

  for (tail = list; CONSP (tail) && ! cons_marked_p (XCONS (tail));
       tail = XCDR (tail))
    {
      Lisp_Object tem = XCAR (tail);
      if (CONSP (tem))
	tem = XCAR (tem);
      if (BUFFERP (tem) && ! BUFFER_LIVE_P (XBUFFER (tem)))
	*prev = XCDR (tail);
      else
	{
	  set_cons_marked (XCONS (tail));
	  mark_automatic_object (XCAR (tail));
	  prev = xcdr_addr (tail);
	}
    }
  mark_automatic_object (tail);
  return list;
}

static void
mark_frame (struct Lisp_Vector *ptr)
{
  struct frame *f = (struct frame *) ptr;
  mark_vectorlike (&ptr->header);
  mark_face_cache (f->face_cache);
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f) && FRAME_OUTPUT_DATA (f))
    {
      struct font *font = FRAME_FONT (f);

      if (font && !vectorlike_marked_p (&font->header))
        mark_vectorlike (&font->header);
    }
#endif
}

static void
mark_window (struct Lisp_Vector *ptr)
{
  struct window *w = (struct window *) ptr;

  mark_vectorlike (&ptr->header);

  /* Marking just window matrices is sufficient since frame matrices
     use the same glyph memory.  */
  if (w->current_matrix)
    {
      mark_glyph_matrix (w->current_matrix);
      mark_glyph_matrix (w->desired_matrix);
    }

  wset_prev_buffers
    (w, mark_discard_killed_buffers (w->prev_buffers));
  wset_next_buffers
    (w, mark_discard_killed_buffers (w->next_buffers));
}

/* Entry of the mark stack.  */
struct mark_entry
{
  ptrdiff_t n;
  Lisp_Object *values;
};

/* This stack is used during marking for traversing data structures without
   using C recursion.  */
struct mark_stack
{
  struct mark_entry *stack;	/* base of stack */
  ptrdiff_t size;		/* allocated size in entries */
  ptrdiff_t sp;			/* current number of entries */
};

static struct mark_stack mark_stk = {NULL, 0, 0};

static inline bool
mark_stack_empty_p (void)
{
  return mark_stk.sp <= 0;
}

/* Pop and return a value from the mark stack (which must be nonempty).  */
static inline Lisp_Object *
mark_stack_pop (void)
{
  Lisp_Object *ret;
  struct mark_entry *entry;
  eassume (! mark_stack_empty_p ());

  entry = &mark_stk.stack[mark_stk.sp - 1];
  eassert (entry->n > 0);
  if (--entry->n == 0)
    --mark_stk.sp;
  ret = entry->values++;
  return ret;
}

static void
grow_mark_stack (void)
{
  struct mark_stack *ms = &mark_stk;
  eassert (ms->sp == ms->size);
  ptrdiff_t min_incr = ms->sp == 0 ? 8192 : 1;
  ms->stack = xpalloc (ms->stack, &ms->size, min_incr, -1, sizeof *ms->stack);
  eassert (ms->sp < ms->size);
}

static inline void
mark_stack_push_n (Lisp_Object *values, ptrdiff_t n)
{
  eassert (n >= 0);
  if (n > 0)
    {
      if (mark_stk.sp >= mark_stk.size)
	grow_mark_stack ();
      mark_stk.stack[mark_stk.sp++] = (struct mark_entry)
	{
	  .n = n,
	  .values = values,
	};
    }
}

static inline void
mark_stack_push (Lisp_Object *value)
{
  return mark_stack_push_n (value, 1);
}

static void
gc_process_string (Lisp_Object *objp)
{
  struct Lisp_String *s = XSTRING (*objp);
  void *forwarded = mgc_fwd_xpntr (s);
  if (forwarded)
    {
      XSETSTRING (*objp, forwarded);
      eassert (! XSTRING_MARKED_P (s));
    }
  else if (mgc_xpntr_p (s))
    {
      /* Do not use string_(set|get)_intervals here.  */
      XSETSTRING (*objp, mgc_flip_xpntr (s, Space_String));
      forwarded = mgc_fwd_xpntr (s);
      if (! forwarded)
	memory_full (SIZE_MAX);
      else
	{
	  struct Lisp_String *s1 = (struct Lisp_String *) forwarded;
	  eassert ((void *) XSTRING (*objp) == (void *) s1);
	  SDATA_OF_LISP_STRING (s1)->string = s1;
	  s1->u.s.intervals = balance_intervals (s1->u.s.intervals);
	  mark_interval_tree (&s1->u.s.intervals);
	}
    }
  else if (! string_marked_p (s))
    {
      set_string_marked (s);
      mark_interval_tree (&s->u.s.intervals);
    }
}

/* Mark the MARK_STK above BASE_SP.

   Until commit 7a8798d, recursively calling mark_object() could
   easily overwhelm the call stack, which MARK_STK deftly circumvents.
   However, we still recursively mark_object() less common Lisp types
   like pseudovectors whose object depths presumably wouldn't trigger
   our pre-7a8798d problems.  */

static void
process_mark_stack (ptrdiff_t base_sp)
{
#if GC_CHECK_MARKED_OBJECTS
  struct mem_node *m = NULL;
#endif
#if GC_CDR_COUNT
  ptrdiff_t cdr_count = 0;
#endif

  eassume (mark_stk.sp >= base_sp && base_sp >= 0);

  while (mark_stk.sp > base_sp)
    {
      Lisp_Object *objp = mark_stack_pop ();

      void *xpntr = XPNTR (*objp);
      if (PURE_P (xpntr))
	continue;

#if GC_REMEMBER_LAST_MARKED
      last_marked[last_marked_index++] = *objp;
      last_marked_index &= LAST_MARKED_SIZE - 1;
#endif

#if GC_CHECK_MARKED_OBJECTS

      /* Under ENABLE_CHECKING, ensure OBJ's xpntr, e.g., struct
         Lisp_Symbol *, points to a block previously registered with
         MEM_ROOT via lisp_malloc().  */
#define CHECK_ALLOCATED()				\
      do {						\
	if (pdumper_object_p (xpntr))			\
	  {						\
	    if (! pdumper_object_p_precise (xpntr))	\
	      emacs_abort ();				\
	    break;					\
	  }						\
	if (mgc_fwd_xpntr (xpntr)			\
	    || mgc_xpntr_p (xpntr))			\
	  break;					\
	m = mem_find (xpntr);				\
	if (m == MEM_NIL)				\
	  emacs_abort ();				\
      } while (0)

      /* Under ENABLE_CHECKING, ensure OBJ's xpntr, e.g., struct
         Lisp_Symbol *, points within a block of the appropriate
         mem_type (a struct Lisp_String should come from a
         MEM_TYPE_STRING block), and is also not a free-list cell, as
         determined by argument function LIVEP.  */
#define CHECK_LIVE(LIVEP, MEM_TYPE)				\
      do {							\
	if (pdumper_object_p (xpntr))				\
	  break;						\
	if (mgc_fwd_xpntr (xpntr)				\
	    || mgc_xpntr_p (xpntr))				\
	  break;						\
	if (! (m->type == MEM_TYPE && LIVEP (m, xpntr)))	\
	  emacs_abort ();					\
      } while (0)

      /* Check both of the above conditions, for non-symbols.  */
#define CHECK_ALLOCATED_AND_LIVE(LIVEP, MEM_TYPE)	\
      do {						\
	CHECK_ALLOCATED ();				\
	CHECK_LIVE (LIVEP, MEM_TYPE);			\
      } while (false)

      /* Check both of the above conditions, for symbols.  */
#define CHECK_ALLOCATED_AND_LIVE_SYMBOL()			\
      do {							\
	if (! c_symbol_p (ptr))					\
	  {							\
	    CHECK_ALLOCATED ();					\
	    CHECK_LIVE (live_symbol_p, MEM_TYPE_SYMBOL);	\
	  }							\
	eassert (valid_lisp_object_p (ptr->u.s.function));	\
      } while (false)

#else /* not GC_CHECK_MARKED_OBJECTS */

#define CHECK_ALLOCATED_AND_LIVE(LIVEP, MEM_TYPE)	((void) 0)
#define CHECK_ALLOCATED_AND_LIVE_SYMBOL()		((void) 0)

#endif /* not GC_CHECK_MARKED_OBJECTS */

      switch (XTYPE (*objp))
	{
	case Lisp_String:
	  CHECK_ALLOCATED_AND_LIVE (live_string_p, MEM_TYPE_STRING);
	  gc_process_string (objp);
	  break;

	case Lisp_Vectorlike:
	  {
	    struct Lisp_Vector *ptr = XVECTOR (*objp);
	    void *forwarded = mgc_fwd_xpntr (ptr);
	    if (forwarded)
	      {
		XSETVECTOR (*objp, forwarded);
		eassert (! XVECTOR_MARKED_P (ptr));
	      }
	    else if (mgc_xpntr_p (ptr))
	      {
		XSETVECTOR (*objp, mgc_flip_xpntr (ptr, Space_Vectorlike));
		ptr = XVECTOR (*objp);
		ptrdiff_t size = ptr->header.size;
		if (size & PSEUDOVECTOR_FLAG)
		  size &= PSEUDOVECTOR_SIZE_MASK;
		mark_stack_push_n (ptr->contents, size);
	      }
	    else if (! vector_marked_p (ptr))
	      {
#ifdef GC_CHECK_MARKED_OBJECTS
		if (! pdumper_object_p (xpntr)
		    && ! SUBRP (*objp)
		    && ! main_thread_p (xpntr))
		  {
		    m = mem_find (xpntr);
		    if (m == MEM_NIL)
		      emacs_abort ();
		    if (m->type == MEM_TYPE_VECTORLIKE)
		      CHECK_LIVE (live_large_vector_p, MEM_TYPE_VECTORLIKE);
		    else
		      CHECK_LIVE (live_small_vector_p, MEM_TYPE_VBLOCK);
		  }
#endif

		switch (PVTYPE (ptr))
		  {
		  case PVEC_BUFFER:
		    mark_buffer ((struct buffer *) ptr);
		    break;

		  case PVEC_FRAME:
		    mark_frame (ptr);
		    break;

		  case PVEC_WINDOW:
		    mark_window (ptr);
		    break;

		  case PVEC_HASH_TABLE:
		    {
		      struct Lisp_Hash_Table *h = (struct Lisp_Hash_Table *)ptr;
		      ptrdiff_t size = ptr->header.size & PSEUDOVECTOR_SIZE_MASK;
		      set_vector_marked (ptr);
		      mark_stack_push_n (ptr->contents, size);
		      mark_stack_push (&h->test.name);
		      mark_stack_push (&h->test.user_hash_function);
		      mark_stack_push (&h->test.user_cmp_function);
		      if (NILP (h->weak))
			mark_stack_push (&h->key_and_value);
		      else
			{
			  /* A weak table marks only the vector, not
			     its contents.  */
			  eassert (h->next_weak == NULL);
			  h->next_weak = weak_hash_tables;
			  weak_hash_tables = h;
			  set_vector_marked (XVECTOR (h->key_and_value));
			}
		      break;
		    }

		  case PVEC_CHAR_TABLE:
		  case PVEC_SUB_CHAR_TABLE:
		    mark_char_table (ptr, PVTYPE (ptr));
		    break;

		  case PVEC_BOOL_VECTOR:
		    /* Can't be dumped bool vector since they're
		       always marked (they're in the old section
		       and don't have mark bits), and we're in a
		       ! vector_marked_p() block */
		    eassert (! vector_marked_p (ptr)
			     && ! pdumper_object_p (ptr));
		    set_vector_marked (ptr);
		    break;

		  case PVEC_OVERLAY:
		    mark_overlay (XOVERLAY (*objp));
		    break;

		  case PVEC_SUBR:
#ifdef HAVE_NATIVE_COMP
		    if (SUBR_NATIVE_COMPILEDP (*objp))
		      {
			set_vector_marked (ptr);
			struct Lisp_Subr *subr = XSUBR (*objp);
			mark_stack_push (&subr->intspec.native);
			mark_stack_push (&subr->command_modes);
			mark_stack_push (&subr->native_comp_u);
			mark_stack_push (&subr->lambda_list);
			mark_stack_push (&subr->type);
		      }
#endif
		    break;

		  case PVEC_FREE:
		    emacs_abort ();

		  default:
		    {
		      /* Same as mark_vectorlike() except stack push
			 versus recursive call to mark_objects().  */
		      ptrdiff_t size = ptr->header.size;
		      if (size & PSEUDOVECTOR_FLAG)
			size &= PSEUDOVECTOR_SIZE_MASK;
		      set_vector_marked (ptr);
		      mark_stack_push_n (ptr->contents, size);
		    }
		    break;
		  }
	      }
	  }
	  break;

	case Lisp_Symbol:
	  {
	    struct Lisp_Symbol *ptr = XSYMBOL (*objp);
	    void *forwarded = mgc_fwd_xpntr (ptr);

	  nextsym:
	    if (forwarded)
	      {
	      case SYMBOL_PLAINVAL:
		mark_stack_push (&ptr->u.s.val.value);
		break;
	      case SYMBOL_VARALIAS:
		{
		  Lisp_Object tem;
		  XSETSYMBOL (tem, SYMBOL_ALIAS (ptr));
		  mark_stack_push (&tem);
		  break;
		}
	      case SYMBOL_LOCALIZED:
		{
		  struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (ptr);
		  /* If the value is set up for a killed buffer
		     restore its global binding.  */
		  if (BUFFERP (blv->where)
		      && ! BUFFER_LIVE_P (XBUFFER (blv->where)))
		    symval_restore_default (ptr);
		  mark_stack_push (&blv->where);
		  mark_stack_push (&blv->valcell);
		  mark_stack_push (&blv->defcell);
		}
		break;
	      case SYMBOL_FORWARDED:
		/* If the value is forwarded to a buffer or keyboard field,
		   these are marked when we see the corresponding object.
		   And if it's forwarded to a C variable, either it's not
		   a Lisp_Object var, or it's staticpro'd already.  */
		break;
	      default:
		emacs_abort ();
		break;
	      }
	    else if (mgc_xpntr_p (ptr))
	      {
		XSETVECTOR (*objp, mgc_flip_xpntr (ptr, Space_Vectorlike));
		ptr = XVECTOR (*objp);
		mark_stack_push (&ptr->u.s.function);
		mark_stack_push (&ptr->u.s.plist);
	      }
	    else if (! symbol_marked_p (ptr))
	      {
		CHECK_ALLOCATED_AND_LIVE_SYMBOL ();
		set_symbol_marked (ptr);
		mark_stack_push (&ptr->u.s.function);
		mark_stack_push (&ptr->u.s.plist);
		switch (ptr->u.s.redirect)
		  {
		  case SYMBOL_PLAINVAL:
		    mark_stack_push (&ptr->u.s.val.value);
		    break;
		  case SYMBOL_VARALIAS:
		    {
		      Lisp_Object tem;
		      XSETSYMBOL (tem, SYMBOL_ALIAS (ptr));
		      mark_stack_push (&tem);
		      break;
		    }
		  case SYMBOL_LOCALIZED:
		    mark_localized_symbol (ptr);
		    break;
		  case SYMBOL_FORWARDED:
		    /* If the value is forwarded to a buffer or keyboard field,
		       these are marked when we see the corresponding object.
		       And if it's forwarded to a C variable, either it's not
		       a Lisp_Object var, or it's staticpro'd already.  */
		    break;
		  default:
		    emacs_abort ();
		    break;
		  }

		if (! PURE_P (XSTRING (ptr->u.s.name)))
		  gc_process_string (&ptr->u.s.name);

	    /* Inner loop to mark next symbol in this bucket, if any.  */
	    xpntr = ptr = ptr->u.s.next;
	    if (ptr)
	      goto nextsym;
	  }
	  break;

	case Lisp_Cons:
	  {
	    struct Lisp_Cons *ptr = XCONS (*objp);
	    if (cons_marked_p (ptr))
	      break;
	    CHECK_ALLOCATED_AND_LIVE (live_cons_p, MEM_TYPE_CONS);
	    set_cons_marked (ptr);

	    /* Put cdr, then car onto stack.  */
	    if (! NILP (ptr->u.s.u.cdr))
	      {
		mark_stack_push (&ptr->u.s.u.cdr);
#if GC_CDR_COUNT
		cdr_count++;
		if (cdr_count == mark_object_loop_halt)
		  emacs_abort ();
#endif
	      }
	    mark_stack_push (&ptr->u.s.car);
	  }
	  break;

	case Lisp_Float:
	  CHECK_ALLOCATED_AND_LIVE (live_float_p, MEM_TYPE_FLOAT);
	  /* Do not mark floats stored in a dump image: these floats are
	     "cold" and do not have mark bits.  */
	  if (pdumper_object_p (XFLOAT (*objp)))
	    eassert (pdumper_cold_object_p (XFLOAT (*objp)));
	  else if (! XFLOAT_MARKED_P (XFLOAT (*objp)))
	    XFLOAT_MARK (XFLOAT (*objp));
	  break;

	case_Lisp_Int:
	  break;

	default:
	  emacs_abort ();
	}
    }

#undef CHECK_LIVE
#undef CHECK_ALLOCATED
#undef CHECK_ALLOCATED_AND_LIVE
}

void
mark_objects (Lisp_Object *objs, ptrdiff_t n)
{
  ptrdiff_t sp = mark_stk.sp;
  mark_stack_push_n (objs, n);
  process_mark_stack (sp);
}

/* Mark the Lisp pointers in the terminal objects.
   Called by Fgarbage_collect.  */

static void
mark_terminals (void)
{
  for (struct terminal *t = terminal_list;
       t != NULL;
       t = t->next_terminal)
    {
      eassert (t->name != NULL);
#ifdef HAVE_WINDOW_SYSTEM
      mark_image_cache (t->image_cache);
#endif /* HAVE_WINDOW_SYSTEM */
      if (! vectorlike_marked_p (&t->header))
	mark_vectorlike (&t->header);
    }
}

/* Value is non-zero if OBJ will survive the current GC because it's
   either marked or does not need to be marked to survive.  */

bool
survives_gc_p (Lisp_Object obj)
{
  bool survives_p;

  switch (XTYPE (obj))
    {
    case_Lisp_Int:
      survives_p = true;
      break;

    case Lisp_Symbol:
      survives_p = symbol_marked_p (XSYMBOL (obj));
      break;

    case Lisp_String:
      survives_p = string_marked_p (XSTRING (obj))
	|| mgc_fwd_xpntr (XSTRING (obj));
      break;

    case Lisp_Vectorlike:
      survives_p =
	(SUBRP (obj) && !SUBR_NATIVE_COMPILEDP (obj)) ||
	vector_marked_p (XVECTOR (obj));
      break;

    case Lisp_Cons:
      survives_p = cons_marked_p (XCONS (obj));
      break;

    case Lisp_Float:
      survives_p =
        XFLOAT_MARKED_P (XFLOAT (obj)) ||
        pdumper_object_p (XFLOAT (obj));
      break;

    default:
      emacs_abort ();
    }

  return survives_p || PURE_P (XPNTR (obj));
}

/* Formerly two functions sweep_conses() and sweep_floats() which
   did the same thing modulo epsilon.

   A probably un-portable and certainly incomprehensible foray into
   void-star-star gymnastics.
*/
static void
sweep_void (void **free_list,
	    int block_index,
	    void **current_block,
	    enum Lisp_Type xtype,
	    size_t block_nitems,
	    ptrdiff_t offset_items,
	    ptrdiff_t offset_chain_from_item,
	    ptrdiff_t offset_next,
	    size_t xsize,
	    size_t *tally_used,
	    size_t *tally_free)
{
  size_t cum_free = 0, cum_used = 0;
  int blk_end = block_index;

  eassume (offset_items == 0);
  *free_list = NULL;

  /* NEXT pointers point from CURRENT_BLOCK into the past.  The first
     iteration processes items through the prevailing BLOCK_INDEX.
     Subsequent iterations process whole blocks of BLOCK_NITEMS items.  */
  for (void **prev = current_block, *blk = *prev;
       blk != NULL;
       blk = *prev, blk_end = block_nitems)
    {
      size_t blk_free = 0;

      /* Currently BLOCK_NITEMS < BITS_PER_BITS_WORD (gcmarkbits needs
	 but one word to describe all items in the block), so the
	 WEND assignment effectively rounds up to 1.  */
      int wend = (blk_end + BITS_PER_BITS_WORD - 1) / BITS_PER_BITS_WORD;
      for (int w = 0; w < wend; ++w)
	for (int start = w * BITS_PER_BITS_WORD, c = start;
	     c < start + min (blk_end - start, BITS_PER_BITS_WORD);
	     ++c)
	  {
	    void *xpntr = (void *) ((uintptr_t) blk + offset_items + c * xsize);
	    bool marked = false;

	    switch (xtype)
	      {
	      case Lisp_Float:
		marked = XFLOAT_MARKED_P (xpntr);
		break;
	      case Lisp_Cons:
		marked = XCONS_MARKED_P (xpntr);
		break;
	      default:
		emacs_abort ();
		break;
	      }

	    if (marked)
	      {
		cum_used++;
		switch (xtype)
		  {
		  case Lisp_Float:
		    XFLOAT_UNMARK (xpntr);
		    break;
		  case Lisp_Cons:
		    XUNMARK_CONS (xpntr);
		    break;
		  default:
		    emacs_abort ();
		    break;
		  }
	      }
	    else
	      {
		blk_free++;

		/* splice RECLAIM into free list. */
		void *reclaim = (void *) ((uintptr_t) blk + offset_items + c * xsize),
		  *reclaim_next = (void *) ((uintptr_t) reclaim + offset_chain_from_item);
		switch (xtype)
		  {
		  case Lisp_Cons:
		    {
		      struct Lisp_Cons *reclaimed_cons
			= (struct Lisp_Cons *) reclaim;
		      *(struct Lisp_Cons **) reclaim_next
			= *(struct Lisp_Cons **) free_list;
		      *(struct Lisp_Cons **) free_list
			= reclaimed_cons;
		      reclaimed_cons->u.s.car = dead_object ();
		    }
		    break;
		  case Lisp_Float:
		    {
		      *(struct Lisp_Float **) reclaim_next
			= *(struct Lisp_Float **) free_list;
		      *(struct Lisp_Float **) free_list
			= (struct Lisp_Float *) reclaim;
		    }
		    break;
		  default:
		    emacs_abort ();
		    break;
		  }
	      }
	  }

      void *block_next = (void *) ((uintptr_t) blk + offset_next);

      /* If BLK contains only free items and we've already seen more
         than two such blocks, then deallocate BLK.  */
      if (blk_free >= block_nitems && cum_free > block_nitems)
        {
	  void *free_next = (void *) ((uintptr_t) blk + offset_items
				      + 0 * xsize + offset_chain_from_item);
	  switch (xtype)
	    {
	    case Lisp_Cons:
	      {
		*(struct Lisp_Cons **) prev
		  = *(struct Lisp_Cons **) block_next;
		*(struct Lisp_Cons **) free_list
		  = *(struct Lisp_Cons **) free_next;
	      }
	      break;
	    case Lisp_Float:
	      {
		*(struct Lisp_Float **) prev
		  = *(struct Lisp_Float **) block_next;
		*(struct Lisp_Float **) free_list
		  = *(struct Lisp_Float **) free_next;
	      }
	      break;
	    default:
	      emacs_abort ();
	      break;
	    }
	  lisp_align_free (blk);
        }
      else
        {
          prev = block_next;
	  cum_free += blk_free;
        }
    }

  *tally_used = cum_used;
  *tally_free = cum_free;
}

static void
sweep_intervals (void)
{
  struct interval_block **iprev = &interval_block;
  size_t cum_free = 0, cum_used = 0;

  interval_free_list = 0;

  for (struct interval_block *iblk; (iblk = *iprev); )
    {
      int blk_free = 0;
      for (int i = 0;
	   /* For first block, process up to prevailing
	      INTERVAL_BLOCK_INDEX.  Subsequent blocks should contain
	      BLOCK_NINTERVALS items. */
	   i < (iblk == interval_block
		? interval_block_index
		: BLOCK_NINTERVALS);
	   i++)
        {
          if (! iblk->intervals[i].gcmarkbit)
            {
              set_interval_parent (&iblk->intervals[i], interval_free_list);
              interval_free_list = &iblk->intervals[i];
              blk_free++;
            }
          else
            {
              cum_used++;
              iblk->intervals[i].gcmarkbit = false;
            }
        }

      /* If BLK contains only free items and we've already seen more
         than two such blocks, then deallocate BLK.  */
      if (blk_free >= BLOCK_NINTERVALS && cum_free > BLOCK_NINTERVALS)
        {
          *iprev = iblk->next;
          /* Unhook from the free list.  */
          interval_free_list = INTERVAL_PARENT (&iblk->intervals[0]);
          lisp_free (iblk);
        }
      else
        {
          cum_free += blk_free;
          iprev = &iblk->next;
        }
    }
  gcstat.total_intervals = cum_used;
  gcstat.total_free_intervals = cum_free;
}

static void
sweep_symbols (void)
{
  struct symbol_block *sblk;
  struct symbol_block **sprev = &symbol_block;
  size_t cum_free = 0, cum_used = ARRAYELTS (lispsym);

  symbol_free_list = NULL;

  for (int i = 0; i < ARRAYELTS (lispsym); i++)
    lispsym[i].u.s.gcmarkbit = false;

  for (sblk = symbol_block; sblk; sblk = *sprev)
    {
      int blk_free = 0;
      struct Lisp_Symbol *sym = sblk->symbols;

      /* First iteration processes up to prevailing
	 SYMBOL_BLOCK_INDEX.  Subsequent iterations traverse whole
	 blocks of BLOCK_NSYMBOLS. */
      struct Lisp_Symbol *end
	= sym + (sblk == symbol_block ? symbol_block_index : BLOCK_NSYMBOLS);

      for (; sym < end; ++sym)
        {
          if (sym->u.s.gcmarkbit)
            {
              ++cum_used;
              sym->u.s.gcmarkbit = false;
              eassert (valid_lisp_object_p (sym->u.s.function));
            }
	  else
            {
              if (sym->u.s.redirect == SYMBOL_LOCALIZED)
		{
                  xfree (SYMBOL_BLV (sym));
                  /* Avoid re-free (bug#29066).  */
                  sym->u.s.redirect = SYMBOL_PLAINVAL;
                }
              sym->u.s.next = symbol_free_list;
              symbol_free_list = sym;
              symbol_free_list->u.s.function = dead_object ();
              ++blk_free;
            }
        }

      /* If BLK contains only free items and we've already seen more
         than two such blocks, then deallocate BLK.  */
      if (blk_free >= BLOCK_NSYMBOLS && cum_free > BLOCK_NSYMBOLS)
        {
          *sprev = sblk->next;
          /* Unhook from the free list.  */
          symbol_free_list = sblk->symbols[0].u.s.next;
          lisp_free (sblk);
        }
      else
        {
          cum_free += blk_free;
          sprev = &sblk->next;
        }
    }
  gcstat.total_symbols = cum_used;
  gcstat.total_free_symbols = cum_free;
}

/* Markers are weak pointers.  Invalidate all markers pointing to the
   swept BUFFER.  */
static void
unchain_dead_markers (struct buffer *buffer)
{
  struct Lisp_Marker *this, **prev = &BUF_MARKERS (buffer);

  while ((this = *prev))
    if (vectorlike_marked_p (&this->header))
      prev = &this->next;
    else
      {
        this->buffer = NULL;
        *prev = this->next;
      }
}

static void
sweep_buffers (void)
{
  Lisp_Object tail, buf;

  gcstat.total_buffers = 0;
  FOR_EACH_LIVE_BUFFER (tail, buf)
    {
      struct buffer *buffer = XBUFFER (buf);
      /* Do not use buffer_(set|get)_intervals here.  */
      buffer->text->intervals = balance_intervals (buffer->text->intervals);
      unchain_dead_markers (buffer);
      gcstat.total_buffers++;
    }
}

static void
gc_sweep (void)
{
  mgc_flip_space ();
  sweep_strings ();
  check_string_bytes (! noninteractive);
  sweep_void ((void **) &cons_free_list, cons_block_index,
	      (void **) &cons_block, Lisp_Cons, BLOCK_NCONS,
	      offsetof (struct cons_block, conses),
	      offsetof (struct Lisp_Cons, u.s.u.chain),
	      offsetof (struct cons_block, next),
	      sizeof (struct Lisp_Cons),
	      &gcstat.total_conses,
	      &gcstat.total_free_conses);
  sweep_void ((void **) &float_free_list, float_block_index,
	      (void **) &float_block, Lisp_Float, BLOCK_NFLOATS,
	      offsetof (struct float_block, floats),
	      offsetof (struct Lisp_Float, u.chain),
	      offsetof (struct float_block, next),
	      sizeof (struct Lisp_Float),
	      &gcstat.total_floats,
	      &gcstat.total_free_floats);
  sweep_intervals ();
  sweep_symbols ();
  sweep_buffers ();
  sweep_vectors ();
  pdumper_clear_marks ();
  check_string_bytes (! noninteractive);
}

DEFUN ("memory-full", Fmemory_full, Smemory_full, 0, 0, 0,
       doc: /* Non-nil means Emacs cannot get much more Lisp memory.  */)
  (void)
{
  return Vmemory_full;
}

DEFUN ("memory-info", Fmemory_info, Smemory_info, 0, 0, 0,
       doc: /* Return a list of (TOTAL-RAM FREE-RAM TOTAL-SWAP FREE-SWAP).
All values are in Kbytes.  If there is no swap space,
last two values are zero.  If the system is not supported
or memory information can't be obtained, return nil.  */)
  (void)
{
#if defined HAVE_LINUX_SYSINFO
  struct sysinfo si;
  uintmax_t units;

  if (sysinfo (&si))
    return Qnil;
#ifdef LINUX_SYSINFO_UNIT
  units = si.mem_unit;
#else
  units = 1;
#endif
  return list4i ((uintmax_t) si.totalram * units / BLOCK_ALIGN,
		 (uintmax_t) si.freeram * units / BLOCK_ALIGN,
		 (uintmax_t) si.totalswap * units / BLOCK_ALIGN,
		 (uintmax_t) si.freeswap * units / BLOCK_ALIGN);
#elif defined WINDOWSNT
  unsigned long long totalram, freeram, totalswap, freeswap;

  if (w32_memory_info (&totalram, &freeram, &totalswap, &freeswap) == 0)
    return list4i ((uintmax_t) totalram / BLOCK_ALIGN,
		   (uintmax_t) freeram / BLOCK_ALIGN,
		   (uintmax_t) totalswap / BLOCK_ALIGN,
		   (uintmax_t) freeswap / BLOCK_ALIGN);
  else
    return Qnil;
#elif defined MSDOS
  unsigned long totalram, freeram, totalswap, freeswap;

  if (dos_memory_info (&totalram, &freeram, &totalswap, &freeswap) == 0)
    return list4i ((uintmax_t) totalram / BLOCK_ALIGN,
		   (uintmax_t) freeram / BLOCK_ALIGN,
		   (uintmax_t) totalswap / BLOCK_ALIGN,
		   (uintmax_t) freeswap / BLOCK_ALIGN);
  else
    return Qnil;
#else /* not HAVE_LINUX_SYSINFO, not WINDOWSNT, not MSDOS */
  /* FIXME: add more systems.  */
  return Qnil;
#endif /* HAVE_LINUX_SYSINFO, not WINDOWSNT, not MSDOS */
}

DEFUN ("memory-use-counts", Fmemory_use_counts, Smemory_use_counts, 0, 0, 0,
       doc: /* Return list of consing tallies.
Elements are
  (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS INTERVALS STRINGS).

Tallies count across the process's lifetime, and only increment.  Note
pseudovectors like frames, windows, buffers (but not their contents),
etc. contribute to VECTOR-CELLS.  */)
  (void)
{
  return list (make_int (cons_cells_consed),
	       make_int (floats_consed),
	       make_int (vector_cells_consed),
	       make_int (symbols_consed),
	       make_int (string_chars_consed),
	       make_int (intervals_consed),
	       make_int (strings_consed));
}

#if defined GNU_LINUX && defined __GLIBC__ && \
  (__GLIBC__ > 2 || __GLIBC_MINOR__ >= 10)
DEFUN ("malloc-info", Fmalloc_info, Smalloc_info, 0, 0, "",
       doc: /* Report malloc information to stderr.
This function outputs to stderr an XML-formatted
description of the current state of the memory-allocation
arenas.  */)
  (void)
{
  if (malloc_info (0, stderr))
    error ("malloc_info failed: %s", emacs_strerror (errno));
  return Qnil;
}
#endif

#ifdef HAVE_MALLOC_TRIM
DEFUN ("malloc-trim", Fmalloc_trim, Smalloc_trim, 0, 1, "",
       doc: /* Release free heap memory to the OS.
This function asks libc to return unused heap memory back to the operating
system.  This function isn't guaranteed to do anything, and is mainly
meant as a debugging tool.

If LEAVE_PADDING is given, ask the system to leave that much unused
space in the heap of the Emacs process.  This should be an integer, and if
not given, it defaults to 0.

This function returns nil if no memory could be returned to the
system, and non-nil if some memory could be returned.  */)
  (Lisp_Object leave_padding)
{
  int pad = 0;

  if (! NILP (leave_padding))
    {
      CHECK_FIXNAT (leave_padding);
      pad = XFIXNUM (leave_padding);
    }

  /* 1 means that memory was released to the system.  */
  if (malloc_trim (pad) == 1)
    return Qt;
  else
    return Qnil;
}
#endif

static bool
symbol_uses_obj (Lisp_Object symbol, Lisp_Object obj)
{
  struct Lisp_Symbol *sym = XSYMBOL (symbol);
  Lisp_Object val = find_symbol_value (symbol);
  return (EQ (val, obj)
	  || EQ (sym->u.s.function, obj)
	  || (! NILP (sym->u.s.function)
	      && COMPILEDP (sym->u.s.function)
	      && EQ (AREF (sym->u.s.function, COMPILED_BYTECODE), obj))
	  || (! NILP (val)
	      && COMPILEDP (val)
	      && EQ (AREF (val, COMPILED_BYTECODE), obj)));
}

/* Find at most FIND_MAX symbols which have OBJ as their value or
   function.  This is used in gdbinit's `xwhichsymbols' command.  */

Lisp_Object
which_symbols (Lisp_Object obj, EMACS_INT find_max)
{
   struct symbol_block *sblk;
   specpdl_ref gc_count = inhibit_garbage_collection ();
   Lisp_Object found = Qnil;

   if (! deadp (obj))
     {
       for (int i = 0; i < ARRAYELTS (lispsym); i++)
	 {
	   Lisp_Object sym = builtin_lisp_symbol (i);
	   if (symbol_uses_obj (sym, obj))
	     {
	       found = Fcons (sym, found);
	       if (--find_max == 0)
		 goto out;
	     }
	 }

       for (sblk = symbol_block; sblk; sblk = sblk->next)
	 {
	   struct Lisp_Symbol *asym = sblk->symbols;
	   int bn;

	   for (bn = 0; bn < BLOCK_NSYMBOLS; bn++, asym++)
	     {
	       if (sblk == symbol_block && bn >= symbol_block_index)
		 break;

	       Lisp_Object sym = make_lisp_symbol (asym);
	       if (symbol_uses_obj (sym, obj))
		 {
		   found = Fcons (sym, found);
		   if (--find_max == 0)
		     goto out;
		 }
	     }
	 }
     }

  out:
   return unbind_to (gc_count, found);
}

#ifdef SUSPICIOUS_OBJECT_CHECKING

static void *
find_suspicious_object_in_range (void *begin, void *end)
{
  char *begin_a = begin;
  char *end_a = end;
  int i;

  for (i = 0; i < ARRAYELTS (suspicious_objects); ++i)
    {
      char *suspicious_object = suspicious_objects[i];
      if (begin_a <= suspicious_object && suspicious_object < end_a)
	return suspicious_object;
    }

  return NULL;
}

static void
note_suspicious_free (void *ptr)
{
  struct suspicious_free_record *rec;

  rec = &suspicious_free_history[suspicious_free_history_index++];
  if (suspicious_free_history_index ==
      ARRAYELTS (suspicious_free_history))
    {
      suspicious_free_history_index = 0;
    }

  memset (rec, 0, sizeof (*rec));
  rec->suspicious_object = ptr;
  backtrace (&rec->backtrace[0], ARRAYELTS (rec->backtrace));
}

static void
detect_suspicious_free (void *ptr)
{
  int i;

  eassert (ptr != NULL);

  for (i = 0; i < ARRAYELTS (suspicious_objects); ++i)
    if (suspicious_objects[i] == ptr)
      {
        note_suspicious_free (ptr);
        suspicious_objects[i] = NULL;
      }
}

#endif /* SUSPICIOUS_OBJECT_CHECKING */

DEFUN ("suspicious-object", Fsuspicious_object, Ssuspicious_object, 1, 1, 0,
       doc: /* Return OBJ, maybe marking it for extra scrutiny.
If Emacs is compiled with suspicious object checking, capture
a stack trace when OBJ is freed in order to help track down
garbage collection bugs.  Otherwise, do nothing and return OBJ.   */)
   (Lisp_Object obj)
{
#ifdef SUSPICIOUS_OBJECT_CHECKING
  /* Right now, we care only about vectors.  */
  if (VECTORLIKEP (obj))
    {
      suspicious_objects[suspicious_object_index++] = XVECTOR (obj);
      if (suspicious_object_index == ARRAYELTS (suspicious_objects))
	suspicious_object_index = 0;
    }
#endif
  return obj;
}

#ifdef ENABLE_CHECKING

bool suppress_checking;

void
die (const char *msg, const char *file, int line)
{
  fprintf (stderr, "\r\n%s:%d: Emacs fatal error: assertion failed: %s\r\n",
	   file, line, msg);
  terminate_due_to_signal (SIGABRT, INT_MAX);
}

#endif /* ENABLE_CHECKING */

#if defined (ENABLE_CHECKING) && USE_STACK_LISP_OBJECTS

/* Stress alloca with inconveniently sized requests and check
   whether all allocated areas may be used for Lisp_Object.  */

static void
verify_alloca (void)
{
  int i;
  enum { ALLOCA_CHECK_MAX = 256 };
  /* Start from size of the smallest Lisp object.  */
  for (i = sizeof (struct Lisp_Cons); i <= ALLOCA_CHECK_MAX; i++)
    {
      void *ptr = alloca (i);
      make_lisp_ptr (ptr, Lisp_Cons);
    }
}

#else /* not ENABLE_CHECKING && USE_STACK_LISP_OBJECTS */

#define verify_alloca() ((void) 0)

#endif /* ENABLE_CHECKING && USE_STACK_LISP_OBJECTS */

static void init_runtime (void);

/* Like all init_*_once(), we should only ever call this in the
   bootstrap.

   Via pdumper_do_now_and_after_load(), the initialization
   of the runtime alloc infra happens in init_runtime().
*/

void
init_alloc_once (void)
{
  gc_inhibited = false;
  gc_cons_threshold = GC_DEFAULT_THRESHOLD;

  PDUMPER_REMEMBER_SCALAR (buffer_slot_defaults.header);
  PDUMPER_REMEMBER_SCALAR (buffer_slot_symbols.header);

  /* Nothing can be malloc'ed until init_runtime().  */
  pdumper_do_now_and_after_load (init_runtime);

  Vloadup_pure_table = CALLN (Fmake_hash_table, QCtest, Qequal, QCsize,
                              make_fixed_natnum (80000));
  update_bytes_between_gc ();
  verify_alloca ();
  init_strings ();
  init_vectors ();
}

static void
init_runtime (void)
{
  purebeg = PUREBEG;
  pure_size = PURESIZE;
  static_string_allocator = &allocate_string;
  static_vector_allocator = &allocate_vector;
  static_interval_allocator = &allocate_interval;
  mem_init ();
  init_finalizer_list (&finalizers);
  init_finalizer_list (&doomed_finalizers);
  mgc_initialize_spaces ();
}

void
syms_of_alloc (void)
{
  static struct Lisp_Objfwd const o_fwd
    = {Lisp_Fwd_Obj, &Vmemory_full};
  Vmemory_full = Qnil;
  defvar_lisp (&o_fwd, "memory-full"); // calls staticpro

  DEFVAR_INT ("gc-cons-threshold", gc_cons_threshold,
	      doc: /* Number of bytes of consing between garbage collections.
Garbage collection can happen automatically once this many bytes have been
allocated since the last garbage collection.  All data types count.

Garbage collection happens automatically only when `eval' is called.

By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.
See also `gc-cons-percentage'.  */);

  DEFVAR_LISP ("gc-cons-percentage", Vgc_cons_percentage,
	       doc: /* Portion of the heap used for allocation.
Garbage collection can happen automatically once this portion of the heap
has been allocated since the last garbage collection.
If this portion is smaller than `gc-cons-threshold', this is ignored.  */);
  Vgc_cons_percentage = make_float (0.1);

  DEFVAR_INT ("pure-bytes-used", pure_bytes_used,
	      doc: /* Number of bytes of shareable Lisp data allocated so far.  */);

  DEFVAR_INT ("cons-cells-consed", cons_cells_consed,
	      doc: /* Number of cons cells that have been consed so far.  */);

  DEFVAR_INT ("floats-consed", floats_consed,
	      doc: /* Number of floats that have been consed so far.  */);

  DEFVAR_INT ("vector-cells-consed", vector_cells_consed,
	      doc: /* Number of vector cells that have been consed so far.  */);

  DEFVAR_INT ("symbols-consed", symbols_consed,
	      doc: /* Number of symbols that have been consed so far.  */);
  symbols_consed += ARRAYELTS (lispsym);

  DEFVAR_INT ("string-chars-consed", string_chars_consed,
	      doc: /* Number of string characters that have been consed so far.  */);

  DEFVAR_INT ("intervals-consed", intervals_consed,
	      doc: /* Number of intervals that have been consed so far.  */);

  DEFVAR_INT ("strings-consed", strings_consed,
	      doc: /* Number of strings that have been consed so far.  */);

  DEFVAR_LISP ("loadup-pure-table", Vloadup_pure_table,
	       doc: /* Allocate objects in pure space during loadup.el.  */);
  Vloadup_pure_table = Qnil;

  DEFVAR_BOOL ("garbage-collection-messages", garbage_collection_messages,
	       doc: /* Non-nil means display messages at start and end of garbage collection.  */);
  garbage_collection_messages = 0;

  DEFVAR_LISP ("post-gc-hook", Vpost_gc_hook,
	       doc: /* Hook run after garbage collection has finished.  */);
  Vpost_gc_hook = Qnil;
  DEFSYM (Qpost_gc_hook, "post-gc-hook");

  DEFVAR_LISP ("memory-signal-data", Vmemory_signal_data,
	       doc: /* Precomputed `signal' argument for memory-full error.  */);
  /* We build this in advance because if we wait until we need it, we might
     not be able to allocate the memory to hold it.  */
  Vmemory_signal_data
    = pure_list (Qerror,
		 build_pure_c_string ("Memory exhausted--use"
				      " M-x save-some-buffers then"
				      " exit and restart Emacs"));

  DEFSYM (Qconses, "conses");
  DEFSYM (Qsymbols, "symbols");
  DEFSYM (Qstrings, "strings");
  DEFSYM (Qvectors, "vectors");
  DEFSYM (Qfloats, "floats");
  DEFSYM (Qintervals, "intervals");
  DEFSYM (Qbuffers, "buffers");
  DEFSYM (Qstring_bytes, "string-bytes");
  DEFSYM (Qvector_slots, "vector-slots");
  DEFSYM (Qheap, "heap");
  DEFSYM (QAutomatic_GC, "Automatic GC");

  DEFSYM (Qgc_cons_percentage, "gc-cons-percentage");
  DEFSYM (Qgc_cons_threshold, "gc-cons-threshold");
  DEFSYM (Qchar_table_extra_slots, "char-table-extra-slots");

  DEFVAR_LISP ("gc-elapsed", Vgc_elapsed,
	       doc: /* Accumulated time elapsed in garbage collections.
The time is in seconds as a floating point value.  */);

  DEFVAR_INT ("gcs-done", gcs_done,
              doc: /* Accumulated number of garbage collections done.  */);
  gcs_done = 0;

  DEFVAR_INT ("integer-width", integer_width,
	      doc: /* Maximum number N of bits in safely-calculated integers.
Integers with absolute values less than 2**N do not signal a range error.
N should be nonnegative.  */);

  defsubr (&Scons);
  defsubr (&Slist);
  defsubr (&Svector);
  defsubr (&Srecord);
  defsubr (&Sbool_vector);
  defsubr (&Smake_byte_code);
  defsubr (&Smake_closure);
  defsubr (&Smake_list);
  defsubr (&Smake_vector);
  defsubr (&Smake_record);
  defsubr (&Smake_string);
  defsubr (&Smake_bool_vector);
  defsubr (&Smake_symbol);
  defsubr (&Smake_marker);
  defsubr (&Smake_finalizer);
  defsubr (&Spurecopy);
  defsubr (&Sgarbage_collect);
  defsubr (&Sgarbage_collect_maybe);
  defsubr (&Sgc_counts);
  defsubr (&Smemory_info);
  defsubr (&Smemory_full);
  defsubr (&Smemory_use_counts);
#if defined GNU_LINUX && defined __GLIBC__ && \
  (__GLIBC__ > 2 || __GLIBC_MINOR__ >= 10)

  defsubr (&Smalloc_info);
#endif
#ifdef HAVE_MALLOC_TRIM
  defsubr (&Smalloc_trim);
#endif
  defsubr (&Ssuspicious_object);

  Lisp_Object watcher;

  static union Aligned_Lisp_Subr Swatch_gc_cons_threshold =
     {{{ PSEUDOVECTOR_FLAG | (PVEC_SUBR << PSEUDOVECTOR_AREA_BITS) },
       { .a4 = watch_gc_cons_threshold },
       4, 4, "watch_gc_cons_threshold", {0}, lisp_h_Qnil}};
  XSETSUBR (watcher, &Swatch_gc_cons_threshold.s);
  Fadd_variable_watcher (Qgc_cons_threshold, watcher);

  static union Aligned_Lisp_Subr Swatch_gc_cons_percentage =
     {{{ PSEUDOVECTOR_FLAG | (PVEC_SUBR << PSEUDOVECTOR_AREA_BITS) },
       { .a4 = watch_gc_cons_percentage },
       4, 4, "watch_gc_cons_percentage", {0}, lisp_h_Qnil}};
  XSETSUBR (watcher, &Swatch_gc_cons_percentage.s);
  Fadd_variable_watcher (Qgc_cons_percentage, watcher);
}

#ifdef HAVE_X_WINDOWS
enum defined_HAVE_X_WINDOWS { defined_HAVE_X_WINDOWS = true };
#else
enum defined_HAVE_X_WINDOWS { defined_HAVE_X_WINDOWS = false };
#endif

#ifdef HAVE_PGTK
enum defined_HAVE_PGTK { defined_HAVE_PGTK = true };
#else
enum defined_HAVE_PGTK { defined_HAVE_PGTK = false };
#endif

/* When compiled with GCC, GDB might say "No enum type named
   pvec_type" if we don't have at least one symbol with that type, and
   then xbacktrace could fail.  Similarly for the other enums and
   their values.  Some non-GCC compilers don't like these constructs.  */
#ifdef __GNUC__
union
{
  enum CHARTAB_SIZE_BITS CHARTAB_SIZE_BITS;
  enum char_table_specials char_table_specials;
  enum char_bits char_bits;
  enum CHECK_LISP_OBJECT_TYPE CHECK_LISP_OBJECT_TYPE;
  enum DEFAULT_HASH_SIZE DEFAULT_HASH_SIZE;
  enum Lisp_Bits Lisp_Bits;
  enum Lisp_Compiled Lisp_Compiled;
  enum maxargs maxargs;
  enum MAX_ALLOCA MAX_ALLOCA;
  enum More_Lisp_Bits More_Lisp_Bits;
  enum pvec_type pvec_type;
  enum defined_HAVE_X_WINDOWS defined_HAVE_X_WINDOWS;
  enum defined_HAVE_PGTK defined_HAVE_PGTK;
} const EXTERNALLY_VISIBLE gdb_make_enums_visible = {0};
#endif	/* __GNUC__ */
