/* Allocator and garbage collector.

Copyright (C) 1985-2024 Free Software Foundation, Inc.

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
#include "mem_node.h"
#include "syssignal.h"

#ifdef HAVE_GCC_TLS
# define mem_root (current_thread->m_mem_root)
# define interval_blocks (current_thread->m_interval_blocks)
# define interval_block_index (current_thread->m_interval_block_index)
# define interval_free_list (current_thread->m_interval_free_list)
# define oldest_sblock (current_thread->m_oldest_sblock)
# define current_sblock (current_thread->m_current_sblock)
# define large_sblocks (current_thread->m_large_sblocks)
# define string_blocks (current_thread->m_string_blocks)
# define string_free_list (current_thread->m_string_free_list)
# define float_blocks (current_thread->m_float_blocks)
# define float_block_index (current_thread->m_float_block_index)
# define float_free_list (current_thread->m_float_free_list)
# define cons_blocks (current_thread->m_cons_blocks)
# define cons_block_index (current_thread->m_cons_block_index)
# define cons_free_list (current_thread->m_cons_free_list)
# define vector_blocks (current_thread->m_vector_blocks)
# define vector_free_lists (current_thread->m_vector_free_lists)
# define most_recent_free_slot (current_thread->m_most_recent_free_slot)
# define large_vectors (current_thread->m_large_vectors)
# define symbol_blocks (current_thread->m_symbol_blocks)
# define symbol_block_index (current_thread->m_symbol_block_index)
# define symbol_free_list (current_thread->m_symbol_free_list)
#else
# define mem_root (main_thread->m_mem_root)
# define interval_blocks (main_thread->m_interval_blocks)
# define interval_block_index (main_thread->m_interval_block_index)
# define interval_free_list (main_thread->m_interval_free_list)
# define oldest_sblock (main_thread->m_oldest_sblock)
# define current_sblock (main_thread->m_current_sblock)
# define large_sblocks (main_thread->m_large_sblocks)
# define string_blocks (main_thread->m_string_blocks)
# define string_free_list (main_thread->m_string_free_list)
# define float_blocks (main_thread->m_float_blocks)
# define float_block_index (main_thread->m_float_block_index)
# define float_free_list (main_thread->m_float_free_list)
# define cons_blocks (main_thread->m_cons_blocks)
# define cons_block_index (main_thread->m_cons_block_index)
# define cons_free_list (main_thread->m_cons_free_list)
# define vector_blocks (main_thread->m_vector_blocks)
# define vector_free_lists (main_thread->m_vector_free_lists)
# define most_recent_free_slot (main_thread->m_most_recent_free_slot)
# define large_vectors (main_thread->m_large_vectors)
# define symbol_blocks (main_thread->m_symbol_blocks)
# define symbol_block_index (main_thread->m_symbol_block_index)
# define symbol_free_list (main_thread->m_symbol_free_list)
#endif

struct Lisp_String *(*static_string_allocator) (void);
struct Lisp_Vector *(*static_vector_allocator) (ptrdiff_t len, bool q_clear);
INTERVAL (*static_interval_allocator) (void);

#ifdef HAVE_GCC_TLS
#include <semaphore.h>
static sem_t sem_main_halted, sem_main_resumed, sem_nonmain_halted, sem_nonmain_resumed;
#endif
Lisp_Object Vmemory_full;
PER_THREAD EMACS_INT bytes_since_gc;
EMACS_INT bytes_between_gc;
static bool gc_inhibited;

/* Last recorded live and free-list counts.  */
PER_THREAD_STATIC struct
{
  size_t total_conses, total_free_conses;
  size_t total_symbols, total_free_symbols;
  size_t total_strings, total_free_strings;
  size_t total_string_bytes;
  size_t total_vectors, total_vector_slots, total_free_vector_slots;
  size_t total_floats, total_free_floats;
  size_t total_intervals, total_free_intervals;
  size_t total_buffers;
  /* Size of the ancillary arrays of live hash-table and obarray objects.
     The objects themselves are not included (counted as vectors above).  */
  size_t total_hash_table_bytes;
} gcstat;

/* Total size of ancillary arrays of all allocated hash-table and obarray
   objects, both dead and alive.  This number is always kept up-to-date.  */
static ptrdiff_t hash_table_allocated_bytes = 0;

enum _GL_ATTRIBUTE_PACKED sdata_type
{
  Sdata_Unibyte = -1,
  Sdata_Pure = -2,
  Sdata_Pinned = -3,
};

#define MALLOC_PROBE(size)			\
  do {						\
    if (profiler_memory_running)		\
      malloc_probe (size);			\
  } while (0)

static void unchain_finalizer (struct Lisp_Finalizer *);
static void mark_terminals (void);
static void gc_sweep (void);
static Lisp_Object make_pure_vector (ptrdiff_t);
static void mark_buffer (struct buffer *);

static void sweep_sdata (struct thread_state *);
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

Lisp_Object const *staticvec[NSTATICS];

/* Index of next unused slot in staticvec.  */

int staticidx;

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
  void *p;
  return posix_memalign (&p, alignment, size) == 0 ? p : 0;
}
/* Verify POSIX invariant ALIGNMENT = (2^x) * sizeof (void *).  */
verify (BLOCK_ALIGN % sizeof (void *) == 0
	&& POWER_OF_2 (BLOCK_ALIGN / sizeof (void *)));
verify (malloc_laligned ()
	|| (LISP_ALIGNMENT % sizeof (void *) == 0
	    && POWER_OF_2 (LISP_ALIGNMENT / sizeof (void *))));
#endif

static inline bool
malloc_laligned (void)
{
  return 0 == alignof (max_align_t) % LISP_ALIGNMENT;
}

/* Return true if malloc-returned P is lisp-aligned, or
   if SIZE doesn't require P to be lisp-aligned.  */

static inline bool
laligned (void *p, size_t size)
{
  return (malloc_laligned ()
	  || (intptr_t) p % LISP_ALIGNMENT == 0
	  || size % LISP_ALIGNMENT != 0);
}

/* Return allocation of at least SIZE bytes, ensuring Lisp alignment.

   Q_CLEAR uses calloc() instead of malloc().
*/

static void *
lmalloc (size_t size, bool q_clear)
{
  /* Callers assume a non-NULL return value, even for zero SIZE.  */
  size_t adjsize = MALLOC_0_IS_NONNULL ? size : max (size, LISP_ALIGNMENT);
  void *p = NULL;

#ifdef USE_ALIGNED_ALLOC
  /* When not malloc_laligned (rare), else-clause risks infloop.  */
  if (!malloc_laligned () && (adjsize % LISP_ALIGNMENT == 0))
    {
      p = aligned_alloc (LISP_ALIGNMENT, adjsize);
      if (q_clear && p && adjsize)
	memclear (p, adjsize);
    }
  else
#endif
    {
      for (;;)
	{
	  p = q_clear ? calloc (1, adjsize) : malloc (adjsize);
	  if (p == NULL || laligned (p, adjsize))
	    break;
	  free (p);
	  adjsize = max (adjsize, adjsize + LISP_ALIGNMENT);
	}
    }
  eassert (p == NULL || laligned (p, adjsize));
  return p;
}

static void *
lrealloc (void *p, size_t size)
{
  /* xrealloc() relies on lrealloc() returning non-NULL even for size
     == 0.  MALLOC_0_IS_NONNULL does not mean REALLOC_0_IS_NONNULL.  */
  size_t adjsize = max (size, LISP_ALIGNMENT);
  void *newp = p;
  for (;;)
    {
      newp = realloc (newp, adjsize);
      if (!adjsize || !newp || laligned (newp, adjsize))
	break;
      adjsize = max (adjsize, adjsize + LISP_ALIGNMENT);
    }
  eassert (!newp || laligned (newp, adjsize));
  return newp;
}

/* Like malloc but check for no memory and block interrupt input.  */

void *
xmalloc (size_t size)
{
  void *val = lmalloc (size, false);
  if (!val)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}

/* Like the above, but zeroes out the memory just allocated.  */

void *
xzalloc (size_t size)
{
  void *val = lmalloc (size, true);
  if (!val)
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
  if (!val)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}

/* Like free() but check pdumper_object_p().  */

void
xfree (void *block)
{
  if (block && !pdumper_object_p (block))
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
  if (ckd_mul (&nbytes, nitems, item_size) || SIZE_MAX < nbytes)
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
  if (ckd_mul (&nbytes, nitems, item_size) || SIZE_MAX < nbytes)
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
  if (ckd_add (&n, n0, n0 >> 1))
    n = PTRDIFF_MAX;
  if (0 <= nitems_max && nitems_max < n)
    n = nitems_max;

  ptrdiff_t adjusted_nbytes
    = ((ckd_mul (&nbytes, n, item_size) || SIZE_MAX < nbytes)
       ? min (PTRDIFF_MAX, SIZE_MAX)
       : nbytes < DEFAULT_MXFAST ? DEFAULT_MXFAST : 0);
  if (adjusted_nbytes)
    {
      n = adjusted_nbytes / item_size;
      nbytes = adjusted_nbytes - adjusted_nbytes % item_size;
    }

  if (!pa)
    *nitems = 0;
  if (n - n0 < nitems_incr_min
      && (ckd_add (&n, n0, nitems_incr_min)
	  || (0 <= nitems_max && nitems_max < n)
	  || ckd_mul (&nbytes, n, item_size)))
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

/* Return a block of SIZE bytes, remembering to free it when
   unwinding.  */

void *
record_xmalloc (size_t size)
{
  void *p = xmalloc (size);
  record_unwind_protect_ptr (xfree, p);
  return p;
}

/* Return a lisp-aligned block of NBYTES, registering it to the
   address-to-block lookup.  */

static void *
lisp_malloc (size_t nbytes, bool q_clear, enum mem_type mtype)
{
  void *val = lmalloc (nbytes, q_clear);
  if (!val)
    memory_full (nbytes);
  else if (mtype != MEM_TYPE_NON_LISP)
    mem_insert (val, (char *) val + nbytes, mtype, &mem_root);
  MALLOC_PROBE (nbytes);
  return val;
}

/* Free BLOCK.  This must be called to free memory allocated with a
   call to lisp_malloc.  */

static void
lisp_free (struct thread_state *thr, void *block)
{
  if (block && !pdumper_object_p (block))
    {
      free (block);
      mem_delete (mem_find (thr, block), &THREAD_FIELD (thr, m_mem_root));
    }
}

// should someone decide to muck with VBLOCK_ALIGN...
verify (VBLOCK_ALIGN % LISP_ALIGNMENT == 0);
verify (VBLOCK_ALIGN <= (1 << PSEUDOVECTOR_SIZE_BITS));

/* An aligned block of memory.  */
struct ablock
{
  union
  {
    char payload[BLOCK_NBYTES];
    struct ablock *next;
  } x;

  /* It's complicated.  See ABLOCK_ABASE and ABASE_SENTINEL.  */
  intptr_t overloaded;
};
verify (sizeof (struct ablock) % BLOCK_ALIGN == 0);

#define ABLOCKS_NBLOCKS (1 << 4)
struct ablocks
{
  struct ablock blocks[ABLOCKS_NBLOCKS];
};
verify (sizeof (struct ablocks) % BLOCK_ALIGN == 0);
verify (offsetof (struct ablocks, blocks) == 0);

/* Sentinel conflates the alignedness bool (at most 1) and the obtuse
   double-counting ablock refcount (at most ABLOCK_NBLOCKS * 2).  */
enum { MAX_SENTINEL = (1 + 2 * ABLOCKS_NBLOCKS) };

/* Each ABLOCK *except the first* stores its parent ABLOCKS's (note
   plural) aligned starting address (so-called abase) in the
   OVERLOADED field.

   So if the OVERLOADED member cannot be a sentinel, it must be the
   desired stored address.

   Otherwise, assume the ABLOCK is the first, and merely return
   itself.

   This was a poor design by a first-year graduate student.
*/
#define ABLOCK_ABASE(ablock)				\
  (((uintptr_t) (ablock)->overloaded) > MAX_SENTINEL	\
   ? (struct ablocks *) (ablock)->overloaded		\
   : (struct ablocks *) (ablock))

/* The special first ablock's OVERLOADED is a sentinel value, not an
   abase address.

   Upon creation of an ABLOCKS (note plural), we assign the value
   0 to the sentinel when the true base differs from the abase, and
   1 otherwise.

   Each time a component ablock of ABLOCKS is requisitioned, we
   increment the sentinel by 2 (thus preserving the even-odd bit
   indicating alignedness).

   The sentinel falling below 2 implies all component ablocks were
   free-listed, and we can harvest its ABLOCKS back to OS.

   Enough RAM existed in 2003 that the first-year graduate student did
   not need to obfuscatedly conflate the refcount with the alignedness
   in a single value.
*/
#define ABASE_SENTINEL(abase) ((abase)->blocks[0].overloaded)

/* The true base is the address malloc returned.  It may not
   be aligned, thus the notion of its corresponding ABASE.  */
#ifdef USE_ALIGNED_ALLOC
/* POSIX systems: don't worry.  */
# define ABASE_TRUE_BASE(abase) (abase)
#else
/* Windows: do worry.  An even-valued sentinel means a nontrivial true
   base which we've stored in the slop between BASE and ABASE.  */
# define ABASE_TRUE_BASE(abase)						\
  (1 & (intptr_t) ABASE_SENTINEL (abase) ? abase : ((void **) (abase))[-1])
#endif

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_ABLOCK(b) \
  __asan_poison_memory_region (&(b)->x, sizeof ((b)->x))
# define ASAN_UNPOISON_ABLOCK(b) \
  __asan_unpoison_memory_region (&(b)->x, sizeof ((b)->x))
#else
# define ASAN_POISON_ABLOCK(b) ((void) 0)
# define ASAN_UNPOISON_ABLOCK(b) ((void) 0)
#endif

/* Allocate an aligned block of NBYTES.  */
static void *
lisp_align_malloc (struct thread_state *thr, size_t nbytes, enum mem_type type)
{
  if (!THREAD_FIELD (thr, m_free_ablocks))
    {
      void *base;
      struct ablocks *abase;
#ifdef USE_ALIGNED_ALLOC
      abase = base = aligned_alloc (BLOCK_ALIGN, sizeof (struct ablocks));
#else
      base = malloc (sizeof (struct ablocks));
      abase = pointer_align (base, BLOCK_ALIGN);
      if (base != abase)
	eassert (abase > base);
#endif
      if (!base) memory_full (sizeof (struct ablocks));

      /* Like string blocks, begin ablock's life on free list.  */
      for (int i = 0; i < ABLOCKS_NBLOCKS; ++i)
	{
	  if (base != abase && i >= ABLOCKS_NBLOCKS - 1)
	    {
	      /* Since ABASE exceeds BASE, the last slot of BLOCKS is
		 off limits.  Stow the true BASE address in the slop
		 between BASE and ABASE. */
	      ((void **) abase)[-1] = base;
	      eassert (ABASE_TRUE_BASE (abase) == base);
	      continue;
	    }
	  else if (i == 0) // special first block store sentinel
	    abase->blocks[i].overloaded = (intptr_t) (base == abase);
	  else
	    abase->blocks[i].overloaded = (intptr_t) abase;
	  abase->blocks[i].x.next = THREAD_FIELD (thr, m_free_ablocks);
	  THREAD_FIELD (thr, m_free_ablocks) = &abase->blocks[i];
	  ASAN_POISON_ABLOCK (&abase->blocks[i]);
	}
      eassert ((uintptr_t) abase % BLOCK_ALIGN == 0);
      eassert (ABLOCK_ABASE (&abase->blocks[3]) == abase); /* 3 is arbitrary */
      eassert (ABLOCK_ABASE (&abase->blocks[0]) == abase);
      eassert (ABASE_SENTINEL (abase) == abase->blocks[0].overloaded);
      eassert (ABASE_TRUE_BASE (abase) == base);
      eassert ((intptr_t) ABASE_SENTINEL (abase) == (base == abase));
    }

  /* Pop retval off free list.  */
  struct ablock *val = THREAD_FIELD (thr, m_free_ablocks);
  ASAN_UNPOISON_ABLOCK (val);
  THREAD_FIELD (thr, m_free_ablocks) = val->x.next;

  struct ablocks *abase = ABLOCK_ABASE (val);
  /* Increment reference count (by 2 to preserve even-odd bit).  */
  ABASE_SENTINEL (abase) = 2 + ABASE_SENTINEL (abase);

  /* Register with block lookup */
  if (type != MEM_TYPE_NON_LISP)
    mem_insert (val, (char *) val + nbytes, type, &THREAD_FIELD (thr, m_mem_root));

  MALLOC_PROBE (nbytes);
  eassert (0 == (uintptr_t) val % BLOCK_ALIGN);
  return val;
}

static void
lisp_align_free (struct thread_state *thr, void *block)
{
  /* Deregister from block lookup.  */
  mem_delete (mem_find (thr, block), &THREAD_FIELD (thr, m_mem_root));

  /* Put on free list.  */
  struct ablock *ablock = block;
  ablock->x.next = THREAD_FIELD (thr, m_free_ablocks);
  THREAD_FIELD (thr, m_free_ablocks) = ablock;
  ASAN_POISON_ABLOCK (ablock);

  /* Decrement reference count (by 2 to preserve even-odd bit).  */
  struct ablocks *abase = ABLOCK_ABASE (ablock);
  ABASE_SENTINEL (abase) = ABASE_SENTINEL (abase) - 2;

  if (ABASE_SENTINEL (abase) < 2)
    { /* all ABASE's ablocks have been free'd.  */
      int ncollapsed = 0;
      const int nblocks = ABLOCKS_NBLOCKS - (ABASE_SENTINEL (abase) ? 0 : 1);
      struct ablock *const abase_end = &abase->blocks[nblocks];

#ifdef HAVE_GCC_TLS
      for (struct thread_state *thr = all_threads;
	   thr != NULL;
	   thr = thr->next_thread)
#endif
	{
	  /* Traverse m_free_ablocks, collapsing ABASE's blocks. */
	  struct ablock **pptr = &THREAD_FIELD (thr, m_free_ablocks);
	  while (*pptr)
	    {
#if GC_ASAN_POISON_OBJECTS
	      /* shouldn't this be in collapsed clause? */
	      __asan_unpoison_memory_region (&(*pptr)->x, sizeof ((*pptr)->x));
#endif
	      if ((struct ablock *) abase <= *pptr && *pptr < abase_end)
		{
		  ++ncollapsed;
		  *pptr = (*pptr)->x.next;
		}
	      else
		pptr = &(*pptr)->x.next;
	    }
	}
      eassert (ncollapsed == nblocks);
#ifdef USE_POSIX_MEMALIGN
      eassert ((uintptr_t) ABASE_TRUE_BASE (abase) % BLOCK_ALIGN == 0);
#endif
      free (ABASE_TRUE_BASE (abase));
    }
}

struct interval_block
{
  /* Data first, to preserve alignment.  */
  struct interval intervals[BLOCK_NINTERVALS];
  struct interval_block *next;
};

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_INTERVAL_BLOCK(b)         \
  __asan_poison_memory_region ((b)->intervals, \
			       sizeof ((b)->intervals))
# define ASAN_UNPOISON_INTERVAL_BLOCK(b)         \
  __asan_unpoison_memory_region ((b)->intervals, \
				 sizeof ((b)->intervals))
# define ASAN_POISON_INTERVAL(i) \
  __asan_poison_memory_region (i, sizeof *(i))
# define ASAN_UNPOISON_INTERVAL(i) \
  __asan_unpoison_memory_region (i, sizeof *(i))
#else
# define ASAN_POISON_INTERVAL_BLOCK(b) ((void) 0)
# define ASAN_UNPOISON_INTERVAL_BLOCK(b) ((void) 0)
# define ASAN_POISON_INTERVAL(i) ((void) 0)
# define ASAN_UNPOISON_INTERVAL(i) ((void) 0)
#endif

/* Return a new interval.  */

static INTERVAL
allocate_interval (void)
{
  INTERVAL val = interval_free_list;
  if (val)
    {
      interval_free_list = INTERVAL_PARENT (val);
    }
  else
    {
      if (interval_block_index == BLOCK_NINTERVALS)
	{
	  struct interval_block *newblk
	    = lisp_malloc (sizeof *newblk, false, MEM_TYPE_NON_LISP);
	  newblk->next = interval_blocks;
	  interval_blocks = newblk;
	  interval_block_index = 0;
	  ASAN_POISON_INTERVAL_BLOCK (newblk);
	}
      val = &interval_blocks->intervals[interval_block_index++];
    }
  ASAN_UNPOISON_INTERVAL (val);
  bytes_since_gc += sizeof (struct interval);
  ++intervals_consed;
  RESET_INTERVAL (val);
  val->gcmarkbit = false;
  return val;
}

static void
mark_interval_tree_functor (INTERVAL *i, void *dummy)
{
  eassert (!interval_marked_p (*i));
  set_interval_marked (*i);
  mark_object (&(*i)->plist);
}

static void
mark_interval_tree_functor_mgc (INTERVAL *i, void *dummy)
{
  eassert (!interval_marked_p (*i));
  if (mgc_xpntr_p (*i) && !mgc_fwd_xpntr (*i))
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
  if (!*i)
    return;
  if (mgc_xpntr_p (*i))
    {
      /* INTERVAL *root = i;
	 for ( ; ! NULL_PARENT (*root); root = &(*root)->up.interval);
	 eassert (mgc_xpntr_p (*root));
	 traverse_intervals_noorder (i, mark_interval_tree_functor_mgc, NULL)
      */
      traverse_intervals_noorder (i, mark_interval_tree_functor_mgc, NULL);
    }
  else if (!interval_marked_p (*i))
    traverse_intervals_noorder (i, mark_interval_tree_functor, NULL);
}

struct sblock
{
  struct sblock *next; /* points in direction of oldest_sblock to
			  current_sblock */

  sdata *data_slot;  /* one-after last populated DATA slot. */

  sdata data[FLEXIBLE_ARRAY_MEMBER];
};

struct string_block
{
  /* Data first, to preserve alignment.  */
  struct Lisp_String strings[BLOCK_NSTRINGS];
  struct string_block *next;
};

#ifdef ENABLE_CHECKING
# define GC_STRING_OVERRUN_COOKIE_SIZE ROUNDUP (4, alignof (sdata))
/* Check for overrun in string data blocks by appending a small
   "cookie" after each allocated string data block, and check for the
   presence of this cookie during GC.  */
static char const string_overrun_cookie[GC_STRING_OVERRUN_COOKIE_SIZE] =
  { '\xde', '\xad', '\xbe', '\xef', /* Perhaps some zeros here.  */ };
#else
# define GC_STRING_OVERRUN_COOKIE_SIZE 0
static char const *string_overrun_cookie = { '\0' };
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

/* A string can neither exceed STRING_BYTES_BOUND, nor be so long that
   the size_t arithmetic in allocate_sdata could overflow.  */
static ptrdiff_t const STRING_BYTES_MAX =
  min (STRING_BYTES_BOUND,
       ((SIZE_MAX
	 - GC_STRING_OVERRUN_COOKIE_SIZE
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

#if GC_ASAN_POISON_OBJECTS
/* Prepare s for denoting a free sdata struct, i.e, poison all bytes
   in the flexible array member, except the first SDATA_OFFSET bytes.
   This is only effective for strings of size n where n > sdata_size(n).
 */
# define ASAN_PREPARE_DEAD_SDATA(s, size)                          \
  do {                                                             \
    __asan_poison_memory_region (s, sdata_size (size));		   \
    __asan_unpoison_memory_region (&(s)->string,		   \
				   sizeof (struct Lisp_String *)); \
    __asan_unpoison_memory_region (&SDATA_NBYTES (s),		   \
				   sizeof SDATA_NBYTES (s));	   \
   } while (false)
/* Prepare s for storing string data for NBYTES bytes.  */
# define ASAN_PREPARE_LIVE_SDATA(s, nbytes) \
  __asan_unpoison_memory_region (s, sdata_size (nbytes))
# define ASAN_POISON_SBLOCK_DATA(b, size) \
  __asan_poison_memory_region ((b)->data, size)
# define ASAN_POISON_STRING_BLOCK(b) \
  __asan_poison_memory_region ((b)->strings, STRING_BLOCK_SIZE)
# define ASAN_UNPOISON_STRING_BLOCK(b) \
  __asan_unpoison_memory_region ((b)->strings, STRING_BLOCK_SIZE)
# define ASAN_POISON_STRING(s) \
  __asan_poison_memory_region (s, sizeof *(s))
# define ASAN_UNPOISON_STRING(s) \
  __asan_unpoison_memory_region (s, sizeof *(s))
#else
# define ASAN_PREPARE_DEAD_SDATA(s, size) ((void) 0)
# define ASAN_PREPARE_LIVE_SDATA(s, nbytes) ((void) 0)
# define ASAN_POISON_SBLOCK_DATA(b, size) ((void) 0)
# define ASAN_POISON_STRING_BLOCK(b) ((void) 0)
# define ASAN_UNPOISON_STRING_BLOCK(b) ((void) 0)
# define ASAN_POISON_STRING(s) ((void) 0)
# define ASAN_UNPOISON_STRING(s) ((void) 0)
#endif

#ifdef ENABLE_CHECKING

PER_THREAD_STATIC int check_string_bytes_count;

void
check_string_bytes (struct Lisp_String *s, ptrdiff_t nbytes)
{
  eassume (PURE_P (s) || pdumper_object_p (s) || ! s->u.s.data
	   || nbytes == SDATA_OF_LISP_STRING (s)->nbytes);
}

/* Validate string_bytes member in B.  */

static void
check_sblock (struct sblock *b)
{
  for (sdata *from = b->data, *end = b->data_slot; from < end; )
    {
      ptrdiff_t nbytes = sdata_size (from->string
				     ? STRING_BYTES (from->string)
				     : from->nbytes);
      from = (sdata *) ((char *) from + nbytes + GC_STRING_OVERRUN_COOKIE_SIZE);
    }
}

static void
check_all_sblocks (struct thread_state *thr)
{
  for (struct sblock *b = THREAD_FIELD (thr, m_large_sblocks); b != NULL; b = b->next)
    {
      struct Lisp_String *s = b->data[0].string;
      if (s)
	STRING_BYTES (s);
    }
  for (struct sblock *b = THREAD_FIELD (thr, m_oldest_sblock); b != NULL; b = b->next)
    check_sblock (b);
}

/* Walk the free list looking for bogus next pointers.
   This may catch buffer overrun from a previous string.  */

static void
check_string_free_list (struct thread_state *thr)
{
  for (struct Lisp_String *s = THREAD_FIELD (thr, m_string_free_list);
       s != NULL;
       s = s->u.next)
    eassume ((uintptr_t) s >= BLOCK_ALIGN);
}

#endif /* ENABLE_CHECKING */

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
	  s->u.next = string_free_list;
	  string_free_list = s;
	}
      ASAN_POISON_STRING_BLOCK (b);
    }

  /* Pop a Lisp_String off the free list.  */
  s = string_free_list;
  string_free_list = s->u.next;
  ASAN_UNPOISON_STRING (s);

  ++strings_consed;
  bytes_since_gc += sizeof *s;

#ifdef ENABLE_CHECKING
  if (check_string_bytes_count > (INT_MAX >> 1))
    check_string_bytes_count = 0; // avoid overflow
  if (++check_string_bytes_count % 200 == 0)
    check_all_sblocks (current_thread);
  else if (current_sblock)
    check_sblock (current_sblock);
#endif

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
  eassert (nbytes >= nchars);
  if (nbytes > STRING_BYTES_MAX)
    error ("Requested %ld bytes exceeds %ld", nbytes, STRING_BYTES_MAX);

  ptrdiff_t sdata_nbytes = sdata_size (nbytes);
  struct sblock *b = current_sblock;
  if (nbytes > LARGE_STRING_THRESH || immovable)
    {
      const size_t size = FLEXSIZEOF (struct sblock, data, sdata_nbytes);
      b = lisp_malloc (size + GC_STRING_OVERRUN_COOKIE_SIZE, false, MEM_TYPE_NON_LISP);
      b->next = large_sblocks;
      large_sblocks = b;
      b->data_slot = b->data;
      ASAN_POISON_SBLOCK_DATA (b, size);
    }
  else if (b == NULL
	   || ((SBLOCK_NBYTES - GC_STRING_OVERRUN_COOKIE_SIZE) <
	       ((char *) b->data_slot - (char *) b + sdata_nbytes)))
    {
      /* Not enough room in the current sblock.  */
      b = lisp_malloc (SBLOCK_NBYTES, false, MEM_TYPE_NON_LISP);
      b->next = NULL;
      b->data_slot = b->data;
      if (current_sblock)
	current_sblock->next = b;
      else
	oldest_sblock = b;
      current_sblock = b;
      ASAN_POISON_SBLOCK_DATA (b, SBLOCK_SIZE);
    }

  ASAN_PREPARE_LIVE_SDATA (data, nbytes);
  b->data_slot->string = s;
  b->data_slot->nbytes = nbytes;
  s->u.s.data = b->data_slot->data;
  s->u.s.size = nchars;
  s->u.s.size_byte = nbytes;
  s->u.s.data[nbytes] = '\0'; /* NBYTES is exclusive of the NUL terminator. */

  /* advance DATA_SLOT */
  char *next_slot = (char *) b->data_slot + sdata_nbytes;
  eassume (memcpy (next_slot, string_overrun_cookie, GC_STRING_OVERRUN_COOKIE_SIZE));
  b->data_slot = (sdata *) (next_slot + GC_STRING_OVERRUN_COOKIE_SIZE);
  eassert ((uintptr_t) b->data_slot % alignof (sdata) == 0);
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
sweep_strings (struct thread_state *thr)
{
  struct string_block *live_blocks = NULL;

  gcstat.total_string_bytes
    = gcstat.total_strings
    = gcstat.total_free_strings
    = 0;
  THREAD_FIELD (thr, m_string_free_list) = NULL;

  for (struct string_block *next, *b = THREAD_FIELD (thr, m_string_blocks);
       b != NULL; b = next)
    {
      struct Lisp_String *restore_free_list = THREAD_FIELD (thr, m_string_free_list);
      int nfree = 0;

      ASAN_UNPOISON_STRING_BLOCK (b);
      next = b->next; /* B might not exist later, so store NEXT now.  */

      for (int i = 0; i < BLOCK_NSTRINGS; ++i)
	{
	  struct Lisp_String *s = b->strings + i;
	  ASAN_UNPOISON_STRING (s);
	  if (s->u.s.data != NULL) /* means S is live but is it marked?  */
	    {
	      if (XSTRING_MARKED_P (s))
		{
		  /* S shall remain in live state.  */
		  XUNMARK_STRING (s);

		  /* Do not use string_(set|get)_intervals here.  */
		  s->u.s.intervals = balance_intervals (s->u.s.intervals);

		  ++gcstat.total_strings;
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
		  s->u.next = THREAD_FIELD (thr, m_string_free_list);
		  THREAD_FIELD (thr, m_string_free_list) = s;
		  ++nfree;
		  ASAN_POISON_STRING (s);
		  ASAN_PREPARE_DEAD_SDATA (data, SDATA_NBYTES (data));
		}
	    }
	  else /* s->u.s.data == NULL */
	    {
	      /* Put just allocated S onto free list.  */
	      s->u.next = THREAD_FIELD (thr, m_string_free_list);
	      THREAD_FIELD (thr, m_string_free_list) = s;
	      ++nfree;
	      ASAN_POISON_STRING (s);
	    }
	} /* for each Lisp_String in block B.  */

      if (/* B contains only free-list entries...  */
	  nfree >= BLOCK_NSTRINGS
	  /* ... and B not among first two such reclaimed blocks.  */
	  && gcstat.total_free_strings > BLOCK_NSTRINGS)
	{
	  /* Harvest B back to OS.  */
	  lisp_free (thr, b);
	  THREAD_FIELD (thr, m_string_free_list) = restore_free_list;
	}
      else
	{
	  gcstat.total_free_strings += nfree;
	  b->next = live_blocks;
	  live_blocks = b;
	}
    } /* for each string block B.  */

  THREAD_FIELD (thr, m_string_blocks) = live_blocks;
  sweep_sdata (thr);

#ifdef ENABLE_CHECKING
  check_string_free_list (thr);
  check_all_sblocks (thr);
#endif
}

static void
sweep_sdata (struct thread_state *thr)
{
  /* Simple sweep of large sblocks.  Side effect: reverses list.  */
  struct sblock *swept_large_sblocks = NULL;
  for (struct sblock *next, *b = THREAD_FIELD (thr, m_large_sblocks);
       b != NULL; b = next)
    {
      next = b->next;

      if (b->data[0].string == NULL)
	lisp_free (thr, b);
      else
	{
	  b->next = swept_large_sblocks;
	  swept_large_sblocks = b;
	}
    }
  THREAD_FIELD (thr, m_large_sblocks) = swept_large_sblocks;

  /* Less simple compaction of non-large sblocks.

     TB is the "to block", or the prevailing memmove destination sblock.
     B is the "from block", or the current memmove source sblock.

     The TO sdata tortoise lags with TB.
     The FROM sdata hare iterates over current B.

     We memmove FROM to TO for all B in the sblock list
     (oldest_sblock...current_sblock), and therein lies the
     compaction.
  */
  struct sblock *tb = THREAD_FIELD (thr, m_oldest_sblock);
  if (tb)
    {
      sdata *end_tb = (sdata *) ((char *) tb + SBLOCK_NBYTES);
      sdata *to = tb->data;

      for (struct sblock *b = tb; b != NULL; b = b->next)
	{
	  eassert ((char *) b->data_slot <= (char *) b + SBLOCK_NBYTES);
	  for (sdata *next_from, *end_from = b->data_slot, *from = b->data;
	       from < end_from;
	       from = next_from)
	    {
	      struct Lisp_String *s = from->string;
	      const ptrdiff_t nbytes = from->nbytes;
	      const ptrdiff_t step = sdata_size (nbytes) + GC_STRING_OVERRUN_COOKIE_SIZE;
	      eassert (!s || !XSTRING_MARKED_P (s));
	      eassert (nbytes <= LARGE_STRING_THRESH);

	      /* Check that the string size recorded in the string is the
		 same as the one recorded in the sdata structure.  */
	      eassume (!s || STRING_BYTES (s) == from->nbytes);

	      /* Compute NEXT_FROM now before FROM can mutate.  */
	      next_from = (sdata *) ((char *) from + step);

	      eassume (!memcmp (string_overrun_cookie,
				 (char *) next_from - GC_STRING_OVERRUN_COOKIE_SIZE,
				 GC_STRING_OVERRUN_COOKIE_SIZE));

	      if (s != NULL) /* a live string to be compacted */
		{
		  sdata *next_to = (sdata *) ((char *) to + step);
		  if (next_to > end_tb)
		    {
		      /* TB is full, proceed with the next sblock.  */
		      tb->data_slot = to;
		      tb = tb->next;
		      end_tb = (sdata *) ((char *) tb + SBLOCK_NBYTES);
		      to = tb->data;
		      next_to = (sdata *) ((char *) to + step);
		    }

		  if (from != to)
		    {
		      eassert (tb != b || to < from);
		      ASAN_PREPARE_LIVE_SDATA (to, nbytes);
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
	  lisp_free (thr, b);
	  b = next;
	}

      tb->data_slot = to;
      tb->next = NULL;
    }
  THREAD_FIELD (thr, m_current_sblock) = tb;
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
  for (i = 0; i < nargs; ++i)
    bool_vector_set (vector, i, !NILP (args[i]));

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
  if (!multibyte)
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
  eassert (STRINGP (string) && !STRING_MULTIBYTE (string));
  struct Lisp_String *s = XSTRING (string);
  ptrdiff_t size = STRING_BYTES (s);
  unsigned char *data = s->u.s.data;

  if (size <= LARGE_STRING_THRESH
      && !PURE_P (data) && !pdumper_object_p (data)
      && s->u.s.size_byte != Sdata_Pinned)
    {
      eassert (s->u.s.size_byte == Sdata_Unibyte);
      eassert (s->u.s.data != NULL);
      sdata *old_sdata = SDATA_OF_LISP_STRING (s);
      allocate_sdata (s, size, size, true);
      memcpy (s->u.s.data, data, size);
      old_sdata->string = NULL;
      eassert (old_sdata->nbytes == size);
      ASAN_PREPARE_DEAD_SDATA (old_sdata, size);
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
  (eassert (!pdumper_object_p (fptr)),                                  \
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
  GETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX (fptr))

#define XFLOAT_MARK(fptr) \
  SETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX (fptr))

#define XFLOAT_UNMARK(fptr) \
  UNSETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX (fptr))

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_FLOAT_BLOCK(fblk)         \
  __asan_poison_memory_region ((fblk)->floats, \
			       sizeof ((fblk)->floats))
# define ASAN_UNPOISON_FLOAT_BLOCK(fblk)         \
  __asan_unpoison_memory_region ((fblk)->floats, \
				 sizeof ((fblk)->floats))
# define ASAN_POISON_FLOAT(p) \
  __asan_poison_memory_region (p, sizeof (struct Lisp_Float))
# define ASAN_UNPOISON_FLOAT(p) \
  __asan_unpoison_memory_region (p, sizeof (struct Lisp_Float))
#else
# define ASAN_POISON_FLOAT_BLOCK(fblk) ((void) 0)
# define ASAN_UNPOISON_FLOAT_BLOCK(fblk) ((void) 0)
# define ASAN_POISON_FLOAT(p) ((void) 0)
# define ASAN_UNPOISON_FLOAT(p) ((void) 0)
#endif

Lisp_Object
make_float (double float_value)
{
  Lisp_Object val;
  if (float_free_list)
    {
      XSETFLOAT (val, float_free_list);
      float_free_list = float_free_list->u.chain;
      ASAN_UNPOISON_FLOAT (float_free_list);
    }
  else
    {
      if (float_block_index == BLOCK_NFLOATS)
	{
	  struct float_block *newblk
	    = lisp_align_malloc (current_thread, sizeof *newblk, MEM_TYPE_FLOAT);
	  newblk->next = float_blocks;
	  memset (newblk->gcmarkbits, 0, sizeof newblk->gcmarkbits);
	  float_blocks = newblk;
	  float_block_index = 0;
	  ASAN_POISON_FLOAT_BLOCK (newblk);
	}
      ASAN_UNPOISON_FLOAT (&float_blocks->floats[float_block_index]);
      XSETFLOAT (val, &float_blocks->floats[float_block_index]);
      ++float_block_index;
    }

  XFLOAT_INIT (val, float_value);
  eassert (!XFLOAT_MARKED_P (XFLOAT (val)));
  bytes_since_gc += sizeof (struct Lisp_Float);
  ++floats_consed;
  return val;
}

#define CONS_BLOCK(fptr) \
  (eassert (!pdumper_object_p (fptr)),                                  \
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
  GETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX (fptr))

#define XMARK_CONS(fptr) \
  SETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX (fptr))

#define XUNMARK_CONS(fptr) \
  UNSETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX (fptr))

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_CONS_BLOCK(b) \
  __asan_poison_memory_region ((b)->conses, sizeof ((b)->conses))
# define ASAN_POISON_CONS(p) \
  __asan_poison_memory_region (p, sizeof (struct Lisp_Cons))
# define ASAN_UNPOISON_CONS(p) \
  __asan_unpoison_memory_region (p, sizeof (struct Lisp_Cons))
#else
# define ASAN_POISON_CONS_BLOCK(b) ((void) 0)
# define ASAN_POISON_CONS(p) ((void) 0)
# define ASAN_UNPOISON_CONS(p) ((void) 0)
#endif

/* Explicitly free a cons cell by putting it on the free-list.  */

void
free_cons (struct Lisp_Cons *ptr)
{
  ptr->u.s.u.chain = cons_free_list;
  ptr->u.s.car = dead_object ();
  cons_free_list = ptr;
  ptrdiff_t nbytes = sizeof *ptr;
  bytes_since_gc -= nbytes;
  ASAN_POISON_CONS (ptr);
}

DEFUN ("cons", Fcons, Scons, 2, 2, 0,
       doc: /* Create a new cons, give it CAR and CDR as components, and return it.  */)
  (Lisp_Object car, Lisp_Object cdr)
{
  register Lisp_Object val;

  if (cons_free_list)
    {
      ASAN_UNPOISON_CONS (cons_free_list);
      XSETCONS (val, cons_free_list);
      cons_free_list = cons_free_list->u.s.u.chain;
    }
  else
    {
      if (cons_block_index == BLOCK_NCONS)
	{
	  struct cons_block *newblk
	    = lisp_align_malloc (current_thread, sizeof *newblk, MEM_TYPE_CONS);
	  memset (newblk->gcmarkbits, 0, sizeof newblk->gcmarkbits);
	  newblk->next = cons_blocks;
	  cons_blocks = newblk;
	  cons_block_index = 0;
	  ASAN_POISON_CONS_BLOCK (newblk);
	}
      ASAN_UNPOISON_CONS (&cons_blocks->conses[cons_block_index]);
      XSETCONS (val, &cons_blocks->conses[cons_block_index]);
      ++cons_block_index;
    }

  XSETCAR (val, car);
  XSETCDR (val, cdr);
  eassert (!XCONS_MARKED_P (XCONS (val)));
  bytes_since_gc += sizeof (struct Lisp_Cons);
  ++cons_cells_consed;

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
  for (ptrdiff_t i = 1; i < count; ++i)
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

/* vector_free_lists[slot] needs to chain Lisp_Vector structs.  Since
   struct Lisp_Vector lacks a next pointer, we cannibalize its
   CONTENTS member.  What then of the Lisp_Vector's actual contents?
   There aren't any since vector_free_lists are reclamations.
*/

static struct Lisp_Vector *
vector_next (struct Lisp_Vector *v)
{
  return XUNTAG (v->contents[0], Lisp_Int0, struct Lisp_Vector);
}

/* The comment for vector_next() explains why we can cavalierly use
   CONTENTS as a poor but profligate man's next-pointer.  */

static void
vector_set_next (struct Lisp_Vector *v, struct Lisp_Vector *p)
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

/*
   Each SLOT (index) in vector_free_lists points to a chain of vectors
   of word-length SLOT+1.

   When a new block of VBLOCK_NBYTES is requisitioned, its free list
   slot gradually percolates downwards as subsequent allocation
   requests consume it piecemeal.  Thus, vector_free_lists slots after
   LARGE_VECTOR_THRESH_WORDS would be sparse (requests beyond the
   threshold eschew vector_free_lists altogether).

   To avoid scanning a sparse range we size VBLOCK_NFREE_LISTS as
   LARGE_VECTOR_THRESH_WORDS, and reserve the final index
   VBLOCK_NFREE_LISTS - 1 as an omnibus slot for free vectors sized
   LARGE_VECTOR_THRESH and greater.
*/

static ptrdiff_t
free_slot (ptrdiff_t nbytes)
{
  eassume (LISP_VECTOR_MIN <= nbytes);
  return min ((nbytes - LISP_VECTOR_MIN) / word_size, VBLOCK_NFREE_LISTS - 1);
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

/* The only vector with 0 slots, allocated from pure space.  */

Lisp_Object zero_vector;

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_VECTOR_CONTENTS(v, bytes) \
  __asan_poison_memory_region ((v)->contents, bytes)
# define ASAN_UNPOISON_VECTOR_CONTENTS(v, bytes) \
  __asan_unpoison_memory_region ((v)->contents, bytes)
# define ASAN_UNPOISON_VECTOR_BLOCK(b) \
  __asan_unpoison_memory_region ((b)->data, sizeof (b)->data)
#else
# define ASAN_POISON_VECTOR_CONTENTS(v, bytes) ((void) 0)
# define ASAN_UNPOISON_VECTOR_CONTENTS(v, bytes) ((void) 0)
# define ASAN_UNPOISON_VECTOR_BLOCK(b) ((void) 0)
#endif

static void
add_vector_free_lists (struct thread_state *thr,
		       struct Lisp_Vector *v,
		       ptrdiff_t nbytes)
{
  eassume (header_size <= nbytes);
  ptrdiff_t nwords = (nbytes - header_size) / word_size;
  XSETPVECTYPESIZE (v, PVEC_FREE, 0, nwords);
  eassert (nbytes % word_size == 0);
  ptrdiff_t slot = free_slot (nbytes);
  THREAD_FIELD (thr, m_most_recent_free_slot) = slot;
  vector_set_next (v, THREAD_FIELD (thr, m_vector_free_lists[slot]));
  THREAD_FIELD (thr, m_vector_free_lists[slot]) = v;
  ASAN_POISON_VECTOR_CONTENTS (v, nbytes - header_size);
}

static struct vector_block *
allocate_vector_block (void)
{
  struct vector_block *newblk = xmalloc (sizeof *newblk);
  mem_insert (newblk->data, newblk->data + VBLOCK_NBYTES,
	      MEM_TYPE_VBLOCK, &mem_root);
  newblk->next = vector_blocks;
  vector_blocks = newblk;
  return newblk;
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

/* Memory footprint in bytes of a pseudovector other than a bool-vector.  */
static ptrdiff_t
pv_nwords (const union vectorlike_header *hdr)
{
  return ((hdr->size & PSEUDOVECTOR_SIZE_MASK)
	  + ((hdr->size & PSEUDOVECTOR_REST_MASK) >> PSEUDOVECTOR_SIZE_BITS));
}

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
      nwords = pv_nwords (hdr);
      break;
    }
  return header_size + (word_size * nwords);
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
  switch (PVTYPE (vector))
    {
    case PVEC_BIGNUM:
      mpz_clear (PSEUDOVEC_STRUCT (vector, Lisp_Bignum)->value);
      break;
    case PVEC_OVERLAY:
      {
	struct Lisp_Overlay *ol = PSEUDOVEC_STRUCT (vector, Lisp_Overlay);
	xfree (ol->interval);
	break;
      }
    case PVEC_FINALIZER:
      unchain_finalizer (PSEUDOVEC_STRUCT (vector, Lisp_Finalizer));
      break;
    case PVEC_FONT:
      if (FONT_OBJECT_MAX == (vector->header.size & PSEUDOVECTOR_SIZE_MASK))
	{
	  struct font *font = PSEUDOVEC_STRUCT (vector, font);
	  struct font_driver const *drv = font->driver;
	  if (drv) /* NULL when Bug#16140.  */
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
      eassert (!PSEUDOVEC_STRUCT (vector, Lisp_Marker)->buffer);
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
    case PVEC_HASH_TABLE:
      {
	struct Lisp_Hash_Table *h = PSEUDOVEC_STRUCT (vector, Lisp_Hash_Table);
	if (h->table_size > 0)
	  {
	    eassert (h->index_bits > 0);
	    xfree (h->index);
	    xfree (h->key_and_value);
	    xfree (h->next);
	    xfree (h->hash);
	    ptrdiff_t bytes = (h->table_size * (2 * sizeof *h->key_and_value
						+ sizeof *h->hash
						+ sizeof *h->next)
			       + hash_table_index_size (h) * sizeof *h->index);
	    hash_table_allocated_bytes -= bytes;
	  }
      }
      break;
    case PVEC_OBARRAY:
      {
	struct Lisp_Obarray *o = PSEUDOVEC_STRUCT (vector, Lisp_Obarray);
	xfree (o->buckets);
	ptrdiff_t bytes = obarray_size (o) * sizeof *o->buckets;
	hash_table_allocated_bytes -= bytes;
      }
      break;
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
	if (lisp_parser->indents_query != NULL)
	  ts_query_delete (lisp_parser->indents_query);
	if (lisp_parser->parser != NULL)
	  ts_parser_delete(lisp_parser->parser);
      }
      break;
    case PVEC_TREE_SITTER_NODE:
      /* currently nothing to clean up.  */
      break;
    case PVEC_TREE_SITTER_CURSOR:
      {
	struct Lisp_Tree_Sitter_Cursor *cursor
	  = PSEUDOVEC_STRUCT (vector, Lisp_Tree_Sitter_Cursor);
	ts_tree_cursor_delete (&cursor->cursor);
      }
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
sweep_vectors (struct thread_state *thr)
{
  gcstat.total_vectors =
    gcstat.total_vector_slots =
    gcstat.total_free_vector_slots = 0;

  memset (THREAD_FIELD (thr, m_vector_free_lists), 0,
	  VBLOCK_NFREE_LISTS * sizeof (struct Lisp_Vector *));
  THREAD_FIELD (thr, m_most_recent_free_slot) = VBLOCK_NFREE_LISTS;

  /* Non-large vectors in M_VECTOR_BLOCKS.  */
  for (struct vector_block *block = THREAD_FIELD (thr, m_vector_blocks),
	 **bprev = &THREAD_FIELD (thr, m_vector_blocks);
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
	  ASAN_UNPOISON_VECTOR_BLOCK (block);
	  if (vector_marked_p (vector))
	    {
	      if (run_vector)
		{
		  eassume (run_bytes && run_bytes % word_size == 0);
		  add_vector_free_lists (thr, run_vector, run_bytes);
		  gcstat.total_free_vector_slots += run_bytes / word_size;
		  run_bytes = 0;
		  run_vector = NULL;
		}
	      XUNMARK_VECTOR (vector);
	      ++gcstat.total_vectors;
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
	  struct mem_node *node = mem_find (thr, block->data);
	  eassert (node != mem_nil);
	  mem_delete (node, &THREAD_FIELD (thr, m_mem_root));
	  *bprev = block->next;
	  xfree (block);
	}
      else
	{
	  bprev = &block->next;
	  if (run_vector)
	    {
	      /* block ended in an unmarked vector */
	      add_vector_free_lists (thr, run_vector, run_bytes);
	      gcstat.total_free_vector_slots += run_bytes / word_size;
	    }
	}
    }

  /* Free floating large vectors.  */
  for (struct large_vector *lv = THREAD_FIELD (thr, m_large_vectors),
	 **lvprev = &THREAD_FIELD (thr, m_large_vectors);
       lv != NULL;
       lv = *lvprev)
    {
      struct Lisp_Vector *vector = large_vector_contents (lv);
      if (XVECTOR_MARKED_P (vector))
	{
	  XUNMARK_VECTOR (vector);
	  ++gcstat.total_vectors;
	  gcstat.total_vector_slots
	    += (vector->header.size & PSEUDOVECTOR_FLAG
		? vector_nbytes (vector) / word_size
		: header_size / word_size + vector->header.size);
	  lvprev = &lv->next;
	}
      else
	{
	  *lvprev = lv->next;
	  lisp_free (thr, lv);
	}
    }

  gcstat.total_hash_table_bytes = hash_table_allocated_bytes;
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

      for (ptrdiff_t exact = free_slot (nbytes),
	     index = max (exact, most_recent_free_slot);
	   index < VBLOCK_NFREE_LISTS; ++index)
	{
	  p = vector_free_lists[index];
	  if (p != NULL)
	    {
	      ptrdiff_t nwords = pv_nwords (&p->header);
	      restbytes = header_size + (nwords * word_size) - nbytes;
	      eassert (restbytes || index == exact);
	      /* Either leave no residual or one big enough to sustain a
		 non-degenerate vector.  A hanging chad of MEM_TYPE_VBLOCK
		 triggers all manner of ENABLE_CHECKING failures.  */
	      if (!restbytes || restbytes >= LISP_VECTOR_MIN)
		{
		  ASAN_UNPOISON_VECTOR_CONTENTS (p, nbytes - header_size);
		  vector_free_lists[index] = vector_next (p);
		  break;
		}
	      p = NULL;
	    }
	}

      if (!p)
	{
	  /* Need new block */
	  p = (struct Lisp_Vector *) allocate_vector_block ()->data;
	  restbytes = VBLOCK_NBYTES - nbytes;
	}

      if (restbytes)
	{
	  /* Tack onto free list corresponding to free_slot(RESTBYTES).  */
	  eassert (restbytes % word_size == 0);
	  eassert (restbytes >= LISP_VECTOR_MIN);
	  add_vector_free_lists (current_thread, ADVANCE (p, nbytes), restbytes);
	}

      if (q_clear)
	memclear (p, nbytes);
    }

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
  eassert (0 <= tag && tag <= PVEC_TAG_MAX);
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
  for (ptrdiff_t i = 1; i < size; ++i)
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
  if (!NILP (init))
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
  if (!((FIXNUMP (args[COMPILED_ARGLIST])
	  || CONSP (args[COMPILED_ARGLIST])
	  || NILP (args[COMPILED_ARGLIST]))
	 && STRINGP (args[COMPILED_BYTECODE])
	 && !STRING_MULTIBYTE (args[COMPILED_BYTECODE])
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

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_SYMBOL_BLOCK(s) \
  __asan_poison_memory_region ((s)->symbols, sizeof ((s)->symbols))
# define ASAN_UNPOISON_SYMBOL_BLOCK(s) \
  __asan_unpoison_memory_region ((s)->symbols, sizeof ((s)->symbols))
# define ASAN_POISON_SYMBOL(sym) \
  __asan_poison_memory_region (sym, sizeof *(sym))
# define ASAN_UNPOISON_SYMBOL(sym) \
  __asan_unpoison_memory_region (sym, sizeof *(sym))

#else
# define ASAN_POISON_SYMBOL_BLOCK(s) ((void) 0)
# define ASAN_UNPOISON_SYMBOL_BLOCK(s) ((void) 0)
# define ASAN_POISON_SYMBOL(sym) ((void) 0)
# define ASAN_UNPOISON_SYMBOL(sym) ((void) 0)
#endif

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
  p->u.s.type = SYMBOL_PLAINVAL;
  SET_SYMBOL_VAL (p, Qunbound);
  set_symbol_function (val, Qnil);
  set_symbol_next (val, NULL);
  p->u.s.gcmarkbit = false;
  p->u.s.interned = SYMBOL_UNINTERNED;
  p->u.s.trapped_write = SYMBOL_UNTRAPPED_WRITE;
  p->u.s.declared_special = false;
  p->u.s.pinned = false;
  p->u.s.buffer_local_only = false;
  p->u.s.buffer_local_default = Qunbound;
  p->u.s.c_variable = (lispfwd) { NULL };
  p->u.s.buffer_local_buffer = Qnil;
}

DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
       doc: /* Return an uninterned, unbound symbol whose name is NAME. */)
  (Lisp_Object name)
{
  Lisp_Object val;

  CHECK_STRING (name);

  if (symbol_free_list)
    {
      ASAN_UNPOISON_SYMBOL (symbol_free_list);
      XSETSYMBOL (val, symbol_free_list);
      symbol_free_list = symbol_free_list->u.s.next;
    }
  else
    {
      if (symbol_block_index == BLOCK_NSYMBOLS)
	{
	  struct symbol_block *new
	    = lisp_malloc (sizeof *new, false, MEM_TYPE_SYMBOL);
	  new->next = symbol_blocks;
	  symbol_blocks = new;
	  symbol_block_index = 0;
	  ASAN_POISON_SYMBOL_BLOCK (new);
	}

      ASAN_UNPOISON_SYMBOL (&symbol_blocks->symbols[symbol_block_index]);
      XSETSYMBOL (val, &symbol_blocks->symbols[symbol_block_index]);
      ++symbol_block_index;
    }

  init_symbol (val, name);
  bytes_since_gc += sizeof (struct Lisp_Symbol);
  ++symbols_consed;
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

/* Return a new (deleted) overlay with PLIST.  */

Lisp_Object
build_overlay (bool front_advance, bool rear_advance,
               Lisp_Object plist)
{
  struct Lisp_Overlay *p = ALLOCATE_PSEUDOVECTOR (struct Lisp_Overlay, plist,
						  PVEC_OVERLAY);
  Lisp_Object overlay = make_lisp_ptr (p, Lisp_Vectorlike);
  struct itree_node *node = xmalloc (sizeof (*node));
  itree_node_init (node, front_advance, rear_advance, overlay);
  p->interval = node;
  p->buffer = NULL;
  set_overlay_plist (overlay, plist);
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

  for (i = 0; i < nargs; ++i)
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
    for (i = 0; i < nargs; ++i)
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
      if (!vectorlike_marked_p (&current->header)
          && !NILP (current->function))
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
  specbind (Qinhibit_quit, Qt);
  internal_condition_case_1 (call0, function, Qt, run_finalizer_handler);
  unbind_to (count, Qnil);
}

static bool
run_finalizers (struct Lisp_Finalizer *finalizers)
{
  struct Lisp_Finalizer *finalizer;
  Lisp_Object function;
  bool finalizer_run = false;

  while (finalizers->next != finalizers)
    {
      finalizer = finalizers->next;
      unchain_finalizer (finalizer);
      function = finalizer->function;
      if (!NILP (function))
	{
	  finalizer_run = true;
	  finalizer->function = Qnil;
	  run_finalizer_function (function);
	}
    }
  return finalizer_run;
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

static bool
vector_marked_p (const struct Lisp_Vector *v)
{
  bool ret;
  eassert (!mgc_xpntr_p (v));
  if (pdumper_object_p (v))
    {
      /* Checking "cold" saves faulting in vector header.  */
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
  eassert (!vector_marked_p (v));
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
  if (!vectorlike_marked_p (header))
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
  eassert (!mgc_xpntr_p (s));
  if (pdumper_object_p (s))
    ret = pdumper_marked_p (s);
  else
    ret = XSTRING_MARKED_P (s);
  return ret;
}

static void
set_string_marked (struct Lisp_String *s)
{
  eassert (!string_marked_p (s));
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

  if (!initialized)
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

  Fthrow (Qtop_level, Qt);
}

/* Return P "made whole" as a Lisp_Cons if P's mem_block M
   corresponds to a Lisp_Cons data field.  */

/* Return P "made whole" as a Lisp_String if P's mem_block M
   corresponds to a Lisp_String data field.  */

static struct Lisp_String *
live_string_holding (struct thread_state *thr, struct mem_node *m, void *p)
{
  struct string_block *b = m->start;
  char *cp = p;
  ptrdiff_t distance = cp - (char *) &b->strings[0];

  (void) thr;
  eassert (m->type == MEM_TYPE_STRING);
#if GC_ASAN_POISON_OBJECTS
  if (__asan_address_is_poisoned (p))
    return NULL;
#endif

  /* P must be on a Lisp_String boundary, and not free-listed.  */
  if (0 <= distance && distance < sizeof b->strings)
    {
      ptrdiff_t off = distance % sizeof b->strings[0];
      if (off == Lisp_String
	   || off == 0
	   /* Since compilers can optimize away struct fields, scan all
	      offsets.  See Bug#28213.  */
	   || off == offsetof (struct Lisp_String, u.s.size_byte)
	   || off == offsetof (struct Lisp_String, u.s.intervals)
	   || off == offsetof (struct Lisp_String, u.s.data))
	{
	  struct Lisp_String *s = p = cp -= off;
#if GC_ASAN_POISON_OBJECTS
	  if (__asan_region_is_poisoned (s, sizeof (*s)))
	    return NULL;
#endif
	  if (s->u.s.data)
	    return s;
	}
    }
  return NULL;
}

static struct Lisp_Cons *
live_cons_holding (struct thread_state *thr, struct mem_node *m, void *p)
{
  struct cons_block *b = m->start;
  char *cp = p;
  ptrdiff_t distance = cp - (char *) &b->conses[0];

  eassert (m->type == MEM_TYPE_CONS);
#if GC_ASAN_POISON_OBJECTS
  if (__asan_address_is_poisoned (p))
    return NULL;
#endif

  /* P must be on a Lisp_Cons boundary, and behind cons_block_index,
     and not free-listed.  */
  if (0 <= distance && distance < sizeof b->conses
      && (b != THREAD_FIELD (thr, m_cons_blocks)
	  || distance / sizeof b->conses[0] < THREAD_FIELD (thr, m_cons_block_index)))
    {
      ptrdiff_t off = distance % sizeof b->conses[0];
      if (off == Lisp_Cons
	  || off == 0
	  || off == offsetof (struct Lisp_Cons, u.s.u.cdr))
	{
	  struct Lisp_Cons *s = p = cp -= off;
#if GC_ASAN_POISON_OBJECTS
	  if (__asan_region_is_poisoned (s, sizeof (*s)))
	    return NULL;
#endif
	  if (!deadp (s->u.s.car))
	    return s;
	}
    }
  return NULL;
}

/* Return P "made whole" as a Lisp_Symbol if P's mem_block M
   corresponds to a Lisp_Symbol data field.  */

static struct Lisp_Symbol *
live_symbol_holding (struct thread_state *thr, struct mem_node *m, void *p)
{
  struct symbol_block *b = m->start;
  char *cp = p;
  ptrdiff_t distance = cp - (char *) &b->symbols[0];

  eassert (m->type == MEM_TYPE_SYMBOL);
#if GC_ASAN_POISON_OBJECTS
  if (__asan_address_is_poisoned (p))
    return NULL;
#endif

  /* P must be on a Lisp_Symbol boundary, and behind
     symbol_block_index, and not free-listed.  */
  if (0 <= distance && distance < sizeof b->symbols
      && (b != THREAD_FIELD (thr, m_symbol_blocks)
	  || distance / sizeof b->symbols[0] < THREAD_FIELD (thr, m_symbol_block_index)))
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
#if GC_ASAN_POISON_OBJECTS
	  if (__asan_region_is_poisoned (s, sizeof (*s)))
	    return NULL;
#endif
	  if (!deadp (s->u.s.function))
	    return s;
	}
    }
  return NULL;
}

/* Return P "made whole" as a Lisp_Float if P's mem_block M
   corresponds to a Lisp_Float data field.  */

static struct Lisp_Float *
live_float_holding (struct thread_state *thr, struct mem_node *m, void *p)
{
  struct float_block *b = m->start;
  char *cp = p;
  ptrdiff_t distance = cp - (char *) &b->floats[0];

  eassert (m->type == MEM_TYPE_FLOAT);
#if GC_ASAN_POISON_OBJECTS
  if (__asan_address_is_poisoned (p))
    return NULL;
#endif

  if (0 <= distance && distance < sizeof b->floats)
    {
      ptrdiff_t off = distance % sizeof b->floats[0];
      /* P be on a Lisp_Float boundary, and behind float_block_index.  */
      if ((off == Lisp_Float || off == 0)
	  && (b != THREAD_FIELD (thr, m_float_blocks) // not the wip block
	      || distance / sizeof b->floats[0] < THREAD_FIELD (thr, m_float_block_index)))
	{
	  struct Lisp_Float *f = (struct Lisp_Float *) (cp - off);
#if GC_ASAN_POISON_OBJECTS
	  if (__asan_region_is_poisoned (f, sizeof (*f)))
	    return NULL;
#endif
	  return f;
	}
    }
  return NULL;
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
	       && (!(vector->header.size & PSEUDOVECTOR_FLAG)
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
live_large_vector_holding (struct thread_state *thr, struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_VECTORLIKE);
  (void) thr;
  return live_vector_pointer (large_vector_contents (m->start), p);
}

/* Return M "made whole" as a non-large Lisp_Vector if P points within it.  */

static struct Lisp_Vector *
live_small_vector_holding (struct thread_state *thr, struct mem_node *m, void *p)
{
  struct Lisp_Vector *vp = p;
  struct vector_block *block = m->start;
  struct Lisp_Vector *vector = (struct Lisp_Vector *) block->data;

  eassert (m->type == MEM_TYPE_VBLOCK);
  (void) thr;

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

/* Workhorse of conservative stack scanning.

   If P looks like it points to Lisp data, mark it and return true.  */

static bool
mark_maybe_pointer (void *const *p)
{
  bool ret = false;
  uintptr_t mask = VALMASK & UINTPTR_MAX;
  struct mem_node *m;
  enum Space_Type xpntr_type;
  void *xpntr, *p_sym;
  struct thread_state *thr = NULL;

  /* Research Bug#41321.  If we didn't special-case Lisp_Symbol
     to subtract off lispsym in make_lisp_ptr(), this hack wouldn't
     be necessary.
  */
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
	     && (!USE_LSB_TAG || *p == po || cp - cpo == type));
      if (ret)
	mark_automatic_object (make_lisp_ptr (po, type));
    }
  else if (pdumper_object_p (p_sym))
    {
      uintptr_t masked_p = (uintptr_t) p_sym & mask;
      void *po = (void *) masked_p;
      char *cp = p_sym;
      char *cpo = po;
      ret = (pdumper_find_object_type (po) == Lisp_Symbol
	     // Verify P’s tag, if any, matches pdumper-reported type.
	     && (!USE_LSB_TAG || p_sym == po || cp - cpo == Lisp_Symbol));
      if (ret)
	mark_automatic_object (make_lisp_ptr (po, Lisp_Symbol));
    }
  else if ((xpntr_type = mgc_find_xpntr (*p, &xpntr)) != Space_Type_Max
	   || (xpntr_type = mgc_find_xpntr (p_sym, &xpntr)) != Space_Type_Max)
    {
      /* analogous logic to set_string_marked() */
      ptrdiff_t offset;
      void *forwarded = mgc_fwd_xpntr (xpntr);
      if (!forwarded)
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
      INT_SUBTRACT_WRAPV ((uintptr_t) *p, (uintptr_t) xpntr, &offset);
      INT_ADD_WRAPV ((uintptr_t) forwarded, offset, (uintptr_t *) p);
    }
  else if ((m = mem_find_which_thread (*p, &thr)) != mem_nil)
    {
      switch (m->type)
	{
	case MEM_TYPE_NON_LISP:
	  break;
	case MEM_TYPE_CONS:
	  {
	    struct Lisp_Cons *h = live_cons_holding (thr, m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_Cons));
		ret = true;
	      }
	  }
	  break;
	case MEM_TYPE_STRING:
	  {
	    struct Lisp_String *h = live_string_holding (thr, m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_String));
		ret = true;
	      }
	  }
	  break;
	case MEM_TYPE_SYMBOL:
	  {
	    struct Lisp_Symbol *h = live_symbol_holding (thr, m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_Symbol));
		ret = true;
	      }
	  }
	  break;
	case MEM_TYPE_FLOAT:
	  {
	    struct Lisp_Float *h = live_float_holding (thr, m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_Float));
		ret = true;
	      }
	  }
	  break;
	case MEM_TYPE_VECTORLIKE:
	  {
	    struct Lisp_Vector *h = live_large_vector_holding (thr, m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_Vectorlike));
		ret = true;
	      }
	  }
	  break;
	case MEM_TYPE_VBLOCK:
	  {
	    struct Lisp_Vector *h = live_small_vector_holding (thr, m, *p);
	    if (h)
	      {
		mark_automatic_object (make_lisp_ptr (h, Lisp_Vectorlike));
		ret = true;
	      }
	  }
	  break;
	default:
	  emacs_abort ();
	  break;
	}
    }
  else if ((m = mem_find_which_thread (p_sym, &thr)) != mem_nil
	   && m->type == MEM_TYPE_SYMBOL)
    {
      struct Lisp_Symbol *h = live_symbol_holding (thr, m, p_sym);
      if (h)
	{
	  mark_automatic_object (make_lisp_ptr (h, Lisp_Symbol));
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

   When porting this to new architectures consider the following.

   Processor Registers

   If __builtin_unwind_init is available, it should suffice to save
   registers in with_flushed_stack().  This presumably is always
   the case for platforms of interest to Commercial Emacs.  We
   preserve the legacy else-branch that calls test_setjmp() to verify
   the sys_jmp_buf saves registers.

   Stack Layout

   Architectures differ in their organization of the stack.  Consider:

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

  /* Ours is not a precise gc, in which all object references
     are unambiguous and markable.  Here, for example,

       Lisp_Object obj = build_string ("test");
       struct Lisp_String *ptr = XSTRING (obj);
       garbage_collect ();
       fprintf (stderr, "test '%s'\n", ptr->u.s.data);

     the compiler is liable to optimize away OBJ, so our
     conservative gc must recognize that PTR references Lisp
     data.  */

  for (pp = start; (void const *) pp < end; pp += GC_POINTER_ALIGNMENT)
    {
      // void *p = *(void *const *) pp;
      mark_maybe_pointer ((void *const *) pp);
    }
}

#ifndef HAVE___BUILTIN_UNWIND_INIT
# ifdef __sparc__
   /* This trick flushes the register windows so that all the state of
      the process is contained in the stack.
      FreeBSD does not have a ta 3 handler, so handle it specially.  */
#  if defined __sparc64__ && defined __FreeBSD__
#   define __builtin_unwind_init() asm ("flushw")
#  else
#   define __builtin_unwind_init() asm ("ta 3")
#  endif
# else
#  define __builtin_unwind_init() ((void) 0)
# endif

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

/* In the absence of built-in stack facilities, the address of the ad
   hoc Lisp_Object O should suffice as top of the stack. */
typedef union
{
  Lisp_Object o;
  void *p;
#ifndef HAVE___BUILTIN_UNWIND_INIT
  sys_jmp_buf j;
  char c;
#endif
} stacktop_sentry;

#ifdef HAVE___BUILTIN_FRAME_ADDRESS
# define STACK_TOP_ADDRESS(addr) ((void) (addr), __builtin_frame_address (0))
#else
# define STACK_TOP_ADDRESS(addr) (addr)
#endif

/* Before calling any FUNC that results in a context switch, ensure
   cpu registers fully flushed to C stack so that mark_memory() has
   complete information.

   FUNC must not run any Lisp code nor allocate any Lisp objects!
*/
NO_INLINE /* Crucial.  Ensures registers are spilled.  */
void
with_flushed_stack (void (*func) (void *arg), void *arg)
{
  stacktop_sentry sentry;
  struct thread_state *self = current_thread;

  /* STACK_TOP_ADDRESS() returns the bp not the sp [1], thus GC would
     miss marking callee-saved registers.  Calling
     __builtin_unwind_init() precludes insidious Bug#41357.

     [1] https://gcc.gnu.org/onlinedocs/gcc/Return-Address.html
  */
  __builtin_unwind_init ();

#ifndef HAVE___BUILTIN_UNWIND_INIT
  test_setjmp ();
  sys_setjmp (sentry.j);
  current_thread->stack_top = STACK_TOP_ADDRESS (&sentry + (stack_bottom < &sentry.c));
#else
  current_thread->stack_top = STACK_TOP_ADDRESS (&sentry);
#endif

  func (arg);
  eassume (current_thread == self);
}

/* Return 2 if OBJ is a killed or special buffer object, 1 if OBJ is a
   valid lisp object, 0 if OBJ is NOT a valid lisp object, or -1 if we
   cannot validate OBJ.  */

int
valid_lisp_object_p (Lisp_Object obj)
{
#ifdef ENABLE_CHECKING
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

  struct thread_state *thr = NULL;
  struct mem_node *m = mem_find_which_thread (p, &thr);
  if (m == mem_nil)
    {
      int valid = -1; /* whether P is a valid pointer */
      eassert (thr == NULL);
#ifdef WINDOWSNT
      valid = w32_valid_pointer_p (p, 16);
#else
      if (ADDRESS_SANITIZER)
	valid = p ? -1 : 0;
      else
	{
	  /* Would use NULL_DEVICE if emacs_write would still validate
	     P in its presence.  */
	  int fd[2];
	  if (emacs_pipe (fd) == 0)
	    {
	      valid = (emacs_write (fd[1], p, 16) == 16 ? 1 : 0);
	      emacs_close (fd[1]);
	      emacs_close (fd[0]);
	    }
	}
#endif /* WINDOWSNT */
      return valid <= 0
	? valid
	/* Strings and conses produced by AUTO_STRING etc. all get here.  */
	: (SUBRP (obj) || STRINGP (obj) || CONSP (obj))
	? 1 : 0;
    }

  eassert (thr != NULL);
  switch (m->type)
    {
    case MEM_TYPE_NON_LISP:
      return 0;
    case MEM_TYPE_CONS:
      return live_cons_holding (thr, m, p) == p;
    case MEM_TYPE_STRING:
      return live_string_holding (thr, m, p) == p;
    case MEM_TYPE_SYMBOL:
      return live_symbol_holding (thr, m, p) == p;
    case MEM_TYPE_FLOAT:
      return live_float_holding (thr, m, p) == p;
    case MEM_TYPE_VECTORLIKE:
      return live_large_vector_holding (thr, m, p) == p;
    case MEM_TYPE_VBLOCK:
      return live_small_vector_holding (thr, m, p) == p;
    default:
      break;
    }
#endif /* ENABLE_CHECKING */
  return 0;
}

/* Like xmalloc, but makes allocation count toward the total consing
   and hash table or obarray usage.
   Return NULL for a zero-sized allocation.  */
void *
hash_table_alloc_bytes (ptrdiff_t nbytes)
{
  if (nbytes == 0)
    return NULL;
  bytes_since_gc += nbytes;
  hash_table_allocated_bytes += nbytes;
  return xmalloc (nbytes);
}

/* Like xfree, but makes allocation count toward the total consing.  */
void
hash_table_free_bytes (void *p, ptrdiff_t nbytes)
{
  bytes_since_gc -= nbytes;
  hash_table_allocated_bytes -= nbytes;
  xfree (p);
}

/* Allocate room for SIZE bytes from pure Lisp storage and return a
   pointer to it.  TYPE is the Lisp type for which the memory is
   allocated.  TYPE < 0 means it's not used for a Lisp object,
   and that the result should have an alignment of -TYPE.

   The bytes are initially zero.

   If pure space is exhausted, allocate space from the heap.  This is
   merely an expedient to let Emacs warn that pure space was exhausted
   and that Emacs should be rebuilt with a larger pure space.  */

EMACS_INT pure[(PURESIZE + sizeof (EMACS_INT) - 1) / sizeof (EMACS_INT)] = {1,};

static void *
pure_alloc (size_t size, int alignment)
{
  /* Initializing nonzero forces into data space, not bss space. */
  static ptrdiff_t lisp_bytes, non_lisp_bytes;
  void *result;

  if (alignment == 0)
    {
      /* Allocate Lisp object from PURE beginning.  */
      result = pointer_align ((char *) pure + lisp_bytes, LISP_ALIGNMENT);
      lisp_bytes = ((char *) result - (char *) pure) + size;
    }
  else
    {
      /* Allocate non-Lisp object from PURE end.  */
      ptrdiff_t new_offs = non_lisp_bytes + size;
      char *addr = (char *) pure + PURESIZE - new_offs;
      int shim = (intptr_t) addr & (-1 + alignment);
      non_lisp_bytes = new_offs + shim;
      result = addr - shim;
    }

  if (PURESIZE <= lisp_bytes + non_lisp_bytes)
    emacs_abort ();
  return result;
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
  static void *pure_nul = NULL;
  Lisp_Object string;
  struct Lisp_String *s = pure_alloc (sizeof *s, 0);
  if (nbytes == 0 && pure_nul != NULL)
    s->u.s.data = pure_nul;
  else
    {
      s->u.s.data = pure_alloc (nbytes + 1, 0);
      memcpy (s->u.s.data, data, nbytes);
      s->u.s.data[nbytes] = '\0';
      if (nbytes == 0)
	pure_nul = s->u.s.data;
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
  struct Lisp_String *s = pure_alloc (sizeof *s, 0);
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
  struct Lisp_Cons *p = pure_alloc (sizeof *p, 0);
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
  struct Lisp_Float *p = pure_alloc (sizeof *p, 0);
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

  struct Lisp_Bignum *b = pure_alloc (sizeof *b, 0);
  XSETPVECTYPESIZE (b, PVEC_BIGNUM, 0, VECSIZE (struct Lisp_Bignum));

  pure_limbs = pure_alloc (nbytes, 0);
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
  struct Lisp_Vector *p = pure_alloc (size, 0);
  XSETVECTOR (new, p);
  XVECTOR (new)->header.size = len;
  return new;
}

/* Copy all contents and parameters of TABLE to a new table allocated
   from pure space, return the purified table.  */
static struct Lisp_Hash_Table *
purecopy_hash_table (struct Lisp_Hash_Table *table)
{
  eassert (table->weakness == Weak_None);
  eassert (table->purecopy);

  struct Lisp_Hash_Table *pure = pure_alloc (sizeof *pure, 0);
  *pure = *table;
  pure->mutable = false;

  if (table->table_size > 0)
    {
      ptrdiff_t hash_bytes = table->table_size * sizeof *table->hash;
      pure->hash = pure_alloc (hash_bytes, (int)sizeof *table->hash);
      memcpy (pure->hash, table->hash, hash_bytes);

      ptrdiff_t next_bytes = table->table_size * sizeof *table->next;
      pure->next = pure_alloc (next_bytes, (int)sizeof *table->next);
      memcpy (pure->next, table->next, next_bytes);

      ptrdiff_t nvalues = table->table_size * 2;
      ptrdiff_t kv_bytes = nvalues * sizeof *table->key_and_value;
      pure->key_and_value = pure_alloc (kv_bytes,
					(int)sizeof *table->key_and_value);
      for (ptrdiff_t i = 0; i < nvalues; i++)
	pure->key_and_value[i] = purecopy (table->key_and_value[i]);

      ptrdiff_t index_bytes = hash_table_index_size (table)
	                      * sizeof *table->index;
      pure->index = pure_alloc (index_bytes, (int)sizeof *table->index);
      memcpy (pure->index, table->index, index_bytes);
    }

  return pure;
}

DEFUN ("purecopy-maybe", Fpurecopy_maybe, Spurecopy_maybe, 1, 1, 0,
       doc: /* Monnier's half-measure to reduce pdump footprint.  */)
  (register Lisp_Object obj)
{
  if (NILP (Vpdumper__pure_pool)
      || MARKERP (obj)
      || OVERLAYP (obj)
      || SYMBOLP (obj))
    return obj; /* Can't purify OBJ.  */
  return purecopy (obj);
}

static struct pinned_object
{
  Lisp_Object object;
  struct pinned_object *next;
} *pinned_objects;

static struct symbol_block *first_block_pinned;

static Lisp_Object
purecopy (Lisp_Object obj)
{
  if (FIXNUMP (obj)
      || (!SYMBOLP (obj) && PURE_P (XPNTR (obj)))
      || SUBRP (obj))
    return obj;    /* Already pure.  */

  if (STRINGP (obj) && XSTRING (obj)->u.s.intervals)
    message_with_string ("Dropping text-properties while making string `%s' pure",
			 obj, true);

  if (!NILP (Vpdumper__pure_pool))
    {
      Lisp_Object pooled = Fgethash (obj, Vpdumper__pure_pool, Qnil);
      if (!NILP (pooled))
	return pooled;
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
      if (table->weakness != Weak_None || !table->purecopy)
        {
          /* Instead, add the hash table to the list of pinned objects,
             so that it will be marked during GC.  */
          struct pinned_object *o = xmalloc (sizeof *o);
          o->object = obj;
          o->next = pinned_objects;
          pinned_objects = o;
          return obj; /* Don't hash cons it.  */
        }

      obj = make_lisp_hash_table (purecopy_hash_table (table));
    }
  else if (COMPILEDP (obj) || VECTORP (obj) || RECORDP (obj))
    {
      struct Lisp_Vector *objp = XVECTOR (obj);
      ptrdiff_t nbytes = vector_nbytes (objp);
      struct Lisp_Vector *vec = pure_alloc (nbytes, 0);
      register ptrdiff_t i;
      ptrdiff_t size = ASIZE (obj);
      if (size & PSEUDOVECTOR_FLAG)
	size &= PSEUDOVECTOR_SIZE_MASK;
      memcpy (vec, objp, nbytes);
      for (i = 0; i < size; ++i)
	vec->contents[i] = purecopy (vec->contents[i]);
      /* Byte code strings must be pinned.  */
      if (COMPILEDP (obj) && size >= 2 && STRINGP (vec->contents[1])
	  && !STRING_MULTIBYTE (vec->contents[1]))
	pin_string (vec->contents[1]);
      XSETVECTOR (obj, vec);
    }
  else if (SYMBOLP (obj))
    {
      if (!XSYMBOL (obj)->u.s.pinned && !c_symbol_p (XSYMBOL (obj)))
	{
	  /* As of 2014, recording first block containing pinned
	     symbols in FIRST_BLOCK_PINNED saves us scanning 15K
	     symbols out of typically 30K.  */
	  XSYMBOL (obj)->u.s.pinned = true;
	  first_block_pinned = symbol_blocks;
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

  if (!NILP (Vpdumper__pure_pool))
    Fputhash (obj, obj, Vpdumper__pure_pool);

  return obj;
}

/* Put an entry in staticvec, pointing at the variable with address
   VARADDRESS.  */

void
staticpro (Lisp_Object const *varaddress)
{
  for (int i = 0; i < staticidx; ++i)
    eassert (staticvec[i] != varaddress);
  if (staticidx >= NSTATICS)
    fatal ("NSTATICS exceeded");
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
    + gcstat.total_strings * sizeof (struct Lisp_String)
    + gcstat.total_hash_table_bytes;
}

#ifdef HAVE_WINDOW_SYSTEM
/* Remove unmarked font-spec and font-entity objects from ENTRY, which is
   (DRIVER-TYPE NUM-FRAMES FONT-CACHE-DATA ...), and return changed entry.  */

static Lisp_Object
compact_font_cache_entry (Lisp_Object entry)
{
  for (Lisp_Object tail = entry, *prev = &entry;
       CONSP (tail);
       tail = XCDR (tail))
    {
      bool drop = false;
      Lisp_Object obj = XCAR (tail);

      /* Consider OBJ if it is (font-spec . [font-entity font-entity ...]).  */
      if (CONSP (obj)
	  && GC_FONT_SPEC_P (XCAR (obj))
	  && !vectorlike_marked_p (&GC_XFONT_SPEC (XCAR (obj))->header)
	  && VECTORP (XCDR (obj)))
	{
	  ptrdiff_t i, size = ASIZE (XCDR (obj));
	  Lisp_Object obj_cdr = XCDR (obj);

	  /* If font-spec is not marked, most likely all font-entities
	     are not marked too.  But we must be sure that nothing is
	     marked within OBJ before we really drop it.  */
	  for (i = 0; i < size; ++i)
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

                  if (!NILP (AREF (val, FONT_TYPE_INDEX))
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
	      drop = true;
	    }
	}
      if (drop)
	*prev = XCDR (tail);
      else
	prev = xcdr_addr (tail);
    }
  return entry;
}

static void
mark_font_caches (void)
{
  for (struct terminal *t = terminal_list; t != NULL; t = t->next_terminal)
    {
      Lisp_Object cache = TERMINAL_FONT_CACHE (t);
      if (!inhibit_compacting_font_caches && CONSP (cache))
	/* compact before marking */
	for (Lisp_Object entry = XCDR (cache); CONSP (entry); entry = XCDR (entry))
	  XSETCAR (entry, compact_font_cache_entry (XCAR (entry)));
      mark_object (&cache);
    }
}
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
  for (struct pinned_object *pobj = pinned_objects;
       pobj != NULL;
       pobj = pobj->next)
    mark_object (&pobj->object);
}

static void
mark_pinned_symbols (void)
{
  for (struct symbol_block *sblk = first_block_pinned;
       sblk != NULL;
       sblk = sblk->next)
    {
      for (struct Lisp_Symbol *sym = sblk->symbols,
	     *end = sym + (first_block_pinned == symbol_blocks
			   ? symbol_block_index : BLOCK_NSYMBOLS);
	   sym < end;
	   ++sym)
	if (sym->u.s.pinned)
	  mark_automatic_object (make_lisp_ptr (sym, Lisp_Symbol));
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

/* Merely a local register.  NULL on gc entry and exit.  */

static struct Lisp_Hash_Table *weak_hash_tables;

/* Consider a value-weak table A containing an entry X -> Y, where Y
   is used in a key-weak table B, Z -> Y.  If A preedes B in the
   list of tables, the likely removal of X -> Y  would break B.
*/

static void
mark_and_sweep_weak_table_contents (void)
{
  /* Keep making marking passes until no-op.  */
  for (bool marked = true; marked; )
    {
      marked = false;
      for (struct Lisp_Hash_Table *h = weak_hash_tables;
	   h != NULL;
	   h = h->next_weak)
	marked |= sweep_weak_table (h, false);
    }

  /* Clear local register WEAK_HASH_TABLES.  */
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
  bytes_between_gc = gc_cons_threshold;
  if (FLOATP (Vgc_cons_percentage))
    bytes_between_gc = max (bytes_between_gc,
			    XFLOAT_DATA (Vgc_cons_percentage) *
			    total_bytes_of_live_objects ());
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
		  && !string_marked_p (XSTRING (glyph->object)))
		mark_object (&glyph->object);
	  }
      }
}

static void
mark_vectorlike (union vectorlike_header *header)
{
  struct Lisp_Vector *ptr = (struct Lisp_Vector *) header;
  ptrdiff_t size = ptr->header.size;

  if (size & PSEUDOVECTOR_FLAG)
    size &= PSEUDOVECTOR_SIZE_MASK;

  eassert (!vectorlike_marked_p (header));
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
      if (!FIXNUMP (*val) &&
	  (!SYMBOLP (*val) || !symbol_marked_p (XSYMBOL (*val))))
	{
	  if (SUB_CHAR_TABLE_P (*val))
	    {
	      if (!vector_marked_p (XVECTOR (*val)))
		mark_char_table (XVECTOR (*val), PVEC_SUB_CHAR_TABLE);
	    }
	  else
	    mark_object (val);
	}
    }
}

/* Mark the chain of overlays starting at PTR.  */

static void
mark_overlay (struct Lisp_Overlay *ov)
{
  /* We don't mark the `itree_node` object, because it is managed manually
     rather than by the GC.  */
  eassert (EQ (ov->interval->data, make_lisp_ptr (ov, Lisp_Vectorlike)));
  set_vectorlike_marked (&ov->header);
  mark_object (&ov->plist);
}

/* Mark the overlay subtree rooted at NODE.  */

static void
mark_overlays (struct itree_node *node)
{
  if (node == NULL)
    return;
  mark_object (&node->data);
  mark_overlays (node->left);
  mark_overlays (node->right);
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
  if (!BUFFER_LIVE_P (buffer))
      mark_object (&BVAR (buffer, undo_list));

  if (!itree_empty_p (buffer->overlays))
    mark_overlays (buffer->overlays->root);

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
      for (int i = 0; i < c->used; ++i)
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

  for (tail = list; CONSP (tail) && !cons_marked_p (XCONS (tail));
       tail = XCDR (tail))
    {
      Lisp_Object tem = XCAR (tail);
      if (CONSP (tem))
	tem = XCAR (tem);
      if (BUFFERP (tem) && !BUFFER_LIVE_P (XBUFFER (tem)))
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

PER_THREAD_STATIC struct mark_stack mark_stk = {NULL, 0, 0};

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
  eassume (!mark_stack_empty_p ());

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

#ifdef HAVE_GCC_TLS
/* Nonmain threads are currently allowed to blow up.  */
void
maybe_garbage_collect (void)
{
  if (main_thread_p (current_thread))
    {
      if (q_garbage_collect ())
	{
	  static sigset_t oldset;
	  int main_halted, nonmain_halted;

	  if (sem_getvalue (&sem_main_halted, &main_halted) != 0
	      || sem_getvalue (&sem_nonmain_halted, &nonmain_halted) != 0)
	    return;

	  if (! main_halted)
	    {
	      block_child_signal (&oldset); // also blocks SIGINT
	      sem_post (&sem_main_halted);
	      sem_init (&sem_nonmain_resumed, 0, 0);
	    }

	  if (nonmain_halted == n_running_threads () - 1) // -1 for main
	    {
	      sem_wait_ (&sem_main_halted, current_thread);
	      garbage_collect ();
	      if (nonmain_halted)
		{
		  sem_init (&sem_nonmain_resumed, 0, nonmain_halted);
		  sem_wait_ (&sem_main_resumed, current_thread);
		}
	      restore_signal_mask (&oldset);
	    }
	  // else wait til next time
	}
    }
  else /* nonmain thread */
    {
      int main_halted;

      if (sem_getvalue (&sem_main_halted, &main_halted) != 0)
	return; /* til next time */

      if (main_halted)
	{
	  int nonmain_halted;

	  // gc began, halt myself
	  if (sem_post (&sem_nonmain_halted) != 0)
	    return; /* til next time */

	  // wait until main gc's
	  sem_wait_ (&sem_nonmain_resumed, current_thread);

	  // one step closer to main's release
	  sem_wait_ (&sem_nonmain_halted, current_thread);

	  if (sem_getvalue (&sem_nonmain_halted, &nonmain_halted) != 0)
	    sem_post (&sem_main_resumed); /* abort abort abort */
	  else if (nonmain_halted == 0)
	    sem_post (&sem_main_resumed); /* home free */
	}
    }
}
#endif /* HAVE_GCC_TLS */

/* Subroutine of Fgarbage_collect that does most of the work.  */

bool
garbage_collect (void)
{
  static struct timespec gc_elapsed = { 0, 0 };
  specpdl_ref count = SPECPDL_INDEX ();

#ifdef HAVE_GCC_TLS
  reap_threads (); // as good a place for this as any
#endif

  if (gc_inhibited)
    return false;

  block_input ();

  /* Show up in profiler.  */
  record_in_backtrace (QAutomatic_GC, 0, 0);

  const size_t tot_before = (profiler_memory_running
			     ? total_bytes_of_live_objects ()
			     : (size_t) -1);

  const struct timespec start = current_timespec ();

  /* Discretionary trimming before marking.  */
  Lisp_Object tail, buffer;
  FOR_EACH_LIVE_BUFFER (tail, buffer)
    compact_buffer (XBUFFER (buffer));
  compact_regexp_cache ();

  eassert (weak_hash_tables == NULL && mark_stack_empty_p ());
  mark_most_objects ();
  mark_pinned_objects ();
  mark_pinned_symbols ();
  mark_lread ();
  mark_terminals ();
  mark_kboards ();
  mark_threads ();
  mark_charset ();
  mark_composite ();
  mark_profiler ();
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
  mark_xselect ();
#endif
#ifdef HAVE_WINDOW_SYSTEM
  mark_font_caches ();
#endif
#ifdef HAVE_NS
  mark_nsterm ();
#endif
  mark_fns ();

  /* Mark the undo lists after compacting away unmarked markers.  */
  FOR_EACH_LIVE_BUFFER (tail, buffer)
    {
      struct buffer *b = XBUFFER (buffer);
      if (!EQ (BVAR (b, undo_list), Qt))
	bset_undo_list (b, compact_undo_list (BVAR (b, undo_list)));
      mark_object (&BVAR (b, undo_list));
    }

  /* Persist any unmarked finalizers since they still need to run
     after sweep.  */
  queue_doomed_finalizers (&doomed_finalizers, &finalizers);
  mark_finalizer_list (&doomed_finalizers);

  /* Must happen after all other marking.  */
  mark_and_sweep_weak_table_contents ();
  eassert (weak_hash_tables == NULL && mark_stack_empty_p ());

  mgc_flip_space ();

  gc_sweep ();

  unmark_main_thread ();

  bytes_since_gc = 0;

  update_bytes_between_gc ();

  /* Unblock as late as possible since it could signal (Bug#43389).  */
  unblock_input ();

  unbind_to (count, Qnil);

  /* GC is complete: now we can run our finalizer callbacks.  */
  bool finalizer_run = run_finalizers (&doomed_finalizers);

  if (main_thread_p (current_thread))
    {
      gc_elapsed = timespec_add (gc_elapsed, timespec_sub (current_timespec (), start));
      Vgc_elapsed = make_float (timespectod (gc_elapsed));
      ++gcs_done;
    }

  /* Collect profiling data.  */
  if (tot_before != (size_t) -1)
    {
      const size_t tot_after = total_bytes_of_live_objects ();
      if (tot_after < tot_before)
	malloc_probe (min (tot_before - tot_after, SIZE_MAX));
    }

  if (!NILP (Vpost_gc_hook))
    {
      specpdl_ref gc_count = inhibit_garbage_collection ();
      safe_run_hooks (Qpost_gc_hook);
      unbind_to (gc_count, Qnil);
    }

  return finalizer_run;
}

static void
gc_process_string (Lisp_Object *objp)
{
  struct Lisp_String *s = XSTRING (*objp);
  void *forwarded = mgc_fwd_xpntr (s);
  if (forwarded)
    {
      XSETSTRING (*objp, forwarded);
      eassert (!XSTRING_MARKED_P (s));
    }
  else if (mgc_xpntr_p (s))
    {
      /* Do not use string_(set|get)_intervals here.  */
      XSETSTRING (*objp, mgc_flip_xpntr (s, Space_String));
      forwarded = mgc_fwd_xpntr (s);
      struct Lisp_String *s1 = (struct Lisp_String *) forwarded;
      eassert ((void *) XSTRING (*objp) == (void *) s1);
      SDATA_OF_LISP_STRING (s1)->string = s1;
      s1->u.s.intervals = balance_intervals (s1->u.s.intervals);
      mark_interval_tree (&s1->u.s.intervals);
    }
  else if (!string_marked_p (s))
    {
      set_string_marked (s);
      mark_interval_tree (&s->u.s.intervals);
    }
}

/* Verify mem type associated with XPNTR, i.e., a struct Lisp_String
   comes from a MEM_TYPE_STRING block, and is not free-listed.  */

static inline bool
check_live (void *xpntr, enum mem_type mtype)
{
#ifdef ENABLE_CHECKING
  if (pdumper_object_p (xpntr))
    {
      eassume (pdumper_object_p_precise (xpntr));
      return true;
    }
  else if (mgc_fwd_xpntr (xpntr) || mgc_xpntr_p (xpntr))
    {
      return true;
    }
  else
    {
      struct thread_state *thr = NULL;
      struct mem_node *m = mem_find_which_thread (xpntr, &thr);
      if (m == mem_nil && thr == NULL)
	return false;
      switch (mtype)
	{
	case MEM_TYPE_CONS:
	  return m->type == mtype
	    && live_cons_holding (thr, m, xpntr) == xpntr;
	  break;
	case MEM_TYPE_SYMBOL:
	  return m->type == mtype
	    && live_symbol_holding (thr, m, xpntr) == xpntr;
	  break;
	case MEM_TYPE_FLOAT:
	  return m->type == mtype
	    && live_float_holding (thr, m, xpntr) == xpntr;
	  break;
	case MEM_TYPE_STRING:
	  return m->type == mtype
	    && live_string_holding (thr, m, xpntr) == xpntr;
	  break;
	case MEM_TYPE_VECTORLIKE:
	  if (m->type == MEM_TYPE_VECTORLIKE)
	    return live_large_vector_holding (thr, m, xpntr) == xpntr;
	  else
	    return live_small_vector_holding (thr, m, xpntr) == xpntr;
	  break;
	default:
	  break;
	}
    }
  return false;
#else
  (void) xpntr;
  (void) mtype;
  return true;
#endif /* defined ENABLE_CHECKING */
}

/* Mark the MARK_STK above BASE_SP.

   Until commit 7a8798d, recursively calling mark_object() could
   easily overwhelm the call stack, which MARK_STK deftly circumvents.
   However, we still recursively mark_object() not-as-common Lisp types
   like pseudovectors whose object depths presumably wouldn't trigger
   our pre-7a8798d problems.  */

static void
process_mark_stack (ptrdiff_t base_sp)
{
  eassume (mark_stk.sp >= base_sp && base_sp >= 0);
  while (mark_stk.sp > base_sp)
    {
      Lisp_Object *objp = mark_stack_pop ();

      void *xpntr = XPNTR (*objp);
      if (PURE_P (xpntr))
	continue;

      switch (XTYPE (*objp))
	{
	case Lisp_String:
	  eassert (check_live (xpntr, MEM_TYPE_STRING));
	  gc_process_string (objp);
	  break;
	case Lisp_Vectorlike:
	  {
	    struct Lisp_Vector *ptr = XVECTOR (*objp);
	    void *forwarded = mgc_fwd_xpntr (ptr);
	    if (forwarded)
	      {
		XSETVECTOR (*objp, forwarded);
		eassert (!XVECTOR_MARKED_P (ptr));
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
	    else if (!vector_marked_p (ptr))
	      {
		if (!PSEUDOVECTORP (*objp, PVEC_SUBR)
		    && !PSEUDOVECTORP (*objp, PVEC_THREAD))
		  eassert (check_live (xpntr, MEM_TYPE_VECTORLIKE));
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
		      set_vector_marked (ptr);
		      if (h->weakness == Weak_None)
			/* The values pushed here may include
			   HASH_UNUSED_ENTRY_KEY, which this function must
			   cope with.  */
			mark_stack_push_n (h->key_and_value, 2 * h->table_size);
		      else
			{
			  /* For weak tables, don't mark the
			    contents --- that's what makes it weak.  */
			  eassert (h->next_weak == NULL);
			  h->next_weak = weak_hash_tables;
			  weak_hash_tables = h;
			}
		    }
		    break;
		  case PVEC_OBARRAY:
		    {
		      struct Lisp_Obarray *o = (struct Lisp_Obarray *)ptr;
		      set_vector_marked (ptr);
		      mark_stack_push_n (o->buckets, obarray_size (o));
		    }
		    break;
		  case PVEC_CHAR_TABLE:
		  case PVEC_SUB_CHAR_TABLE:
		    mark_char_table (ptr, PVTYPE (ptr));
		    break;
		  case PVEC_BOOL_VECTOR:
		    /* Can't be dumped bool vector since they're
		       always marked (they're in the old section
		       and don't have mark bits), and we're in a
		       ! vector_marked_p() block */
		    eassert (!vector_marked_p (ptr)
			     && !pdumper_object_p (ptr));
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
		    break;
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
	    if (mgc_xpntr_p (ptr))
	      {
		void *forwarded = mgc_fwd_xpntr (ptr);
                if (forwarded)
                  {
                    XSETSYMBOL (*objp, forwarded);
                    eassert (!symbol_marked_p (ptr));
                    break; /* !!! */
                  }
                XSETSYMBOL (*objp, mgc_flip_xpntr (ptr, Space_Symbol));
		ptr = XSYMBOL (*objp);
	      }
	    else
	      {
		if (symbol_marked_p (ptr))
		  break; /* !!! */
		else if (!c_symbol_p (ptr))
		  eassert (check_live (xpntr, MEM_TYPE_SYMBOL));
		eassert (valid_lisp_object_p (ptr->u.s.function));
		set_symbol_marked (ptr);
	      }
	    mark_stack_push (&ptr->u.s.function);
	    mark_stack_push (&ptr->u.s.plist);
	    mark_stack_push (&ptr->u.s.buffer_local_default);
	    mark_stack_push (&ptr->u.s.buffer_local_buffer);
	    switch (ptr->u.s.type)
	      {
	      case SYMBOL_PLAINVAL:
		mark_stack_push (&ptr->u.s.val.value);
		break;
	      case SYMBOL_VARALIAS:
		mark_automatic_object (make_lisp_ptr (SYMBOL_ALIAS (ptr),
						      Lisp_Symbol));
		break;
	      case SYMBOL_LOCAL_SOMEWHERE:
	      case SYMBOL_FORWARDED:
		/* Either not a Lisp_Object var, or already staticpro'd.  */
	      case SYMBOL_PER_BUFFER:
	      case SYMBOL_KBOARD:
		/* Marked when we see the corresponding object.  */
		break;
	      default:
		emacs_abort ();
		break;
	      }

	    if (!PURE_P (XSTRING (ptr->u.s.name)))
	      gc_process_string (&ptr->u.s.name);

	    if (ptr->u.s.next)
	      mark_automatic_object (make_lisp_ptr (ptr->u.s.next, Lisp_Symbol));
	  }
	  break;
	case Lisp_Cons:
	  {
	    struct Lisp_Cons *ptr = XCONS (*objp);

	    if (mgc_xpntr_p (ptr))
	      {
		void *forwarded = mgc_fwd_xpntr (ptr);
                if (forwarded)
                  {
                    XSETCONS (*objp, forwarded);
                    eassert (!cons_marked_p (ptr));
                    break; /* !!! */
                  }
                XSETCONS (*objp, mgc_flip_xpntr (ptr, Space_Cons));
		ptr = XCONS (*objp);
	      }
	    else
	      {
		if (cons_marked_p (ptr))
		  break; /* !!! */
		eassert (check_live (xpntr, MEM_TYPE_CONS));
		set_cons_marked (ptr);
	      }

	    /* Put cdr, then car onto stack.  */
	    if (!NILP (ptr->u.s.u.cdr))
	      mark_stack_push (&ptr->u.s.u.cdr);
	    mark_stack_push (&ptr->u.s.car);
	  }
	  break;
	case Lisp_Float:
	  {
	    struct Lisp_Float *ptr = XFLOAT (*objp);
	    if (ptr) /* else HASH_UNUSED_ENTRY_KEY */
	      {
		if (pdumper_object_p (ptr))
		  /* pdumper floats are "cold" and lack mark bits.  */
		  eassert (pdumper_cold_object_p (ptr));
		else if (mgc_xpntr_p (ptr))
		  {
		    void *forwarded = mgc_fwd_xpntr (ptr);
		    if (forwarded)
		      {
			XSETFLOAT (*objp, forwarded);
			eassert (!XFLOAT_MARKED_P (ptr));
			break; /* !!! */
		      }
		    XSETFLOAT (*objp, mgc_flip_xpntr (ptr, Space_Float));
		    /* !!! reset ptr to XFLOAT (*objp) if more to do */
		  }
		else
		  {
		    eassert (check_live (xpntr, MEM_TYPE_FLOAT));
		    if (!XFLOAT_MARKED_P (ptr))
		      XFLOAT_MARK (ptr);
		  }
	      }
	  }
	  break;
	case_Lisp_Int:
	  break;
	default:
	  emacs_abort ();
	}
    }
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
      if (!vectorlike_marked_p (&t->header))
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
      survives_p = string_marked_p (XSTRING (obj));
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
sweep_void (struct thread_state *thr,
	    void **free_list,
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
     iteration processes CURRENT_BLOCK to the prevailing BLOCK_INDEX.
     Subsequent iterations process whole blocks of BLOCK_NITEMS items.  */
  for (void **prev = current_block, *blk = *prev;
       blk != NULL;
       blk = *prev, blk_end = block_nitems)
    {
      size_t blk_free = 0;
      switch (xtype)
        {
        case Lisp_Float:
          ASAN_UNPOISON_FLOAT_BLOCK ((struct Lisp_Float *) *blk);
          break;
        case Lisp_Cons:
          break;
        default:
          emacs_abort ();
          break;
        }

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
		++cum_used;
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
		++blk_free;
		/* prepend RECLAIM to free list. */
		void *reclaim = (void *) ((uintptr_t) blk + offset_items + c * xsize),
		  *reclaim_next = (void *) ((uintptr_t) reclaim + offset_chain_from_item);
		switch (xtype)
		  {
		  case Lisp_Cons:
		    {
		      struct Lisp_Cons *reclaimed_cons
			= (struct Lisp_Cons *) reclaim;
                      ASAN_UNPOISON_CONS (reclaimed_cons);
		      *(struct Lisp_Cons **) reclaim_next
			= *(struct Lisp_Cons **) free_list;
		      *(struct Lisp_Cons **) free_list
			= reclaimed_cons;
		      reclaimed_cons->u.s.car = dead_object ();
                      ASAN_UNPOISON_CONS (reclaimed_cons);
		    }
		    break;
		  case Lisp_Float:
		    {
		      *(struct Lisp_Float **) reclaim_next
			= *(struct Lisp_Float **) free_list;
		      *(struct Lisp_Float **) free_list
			= (struct Lisp_Float *) reclaim;
                      ASAN_POISON_FLOAT (reclaim);
		    }
		    break;
		  default:
		    emacs_abort ();
		    break;
		  }
	      }
	  }

      void *block_next = (void *) ((uintptr_t) blk + offset_next);

      /* If BLK contains only free items and cumulative free items
         would exceed two blocks worth, deallocate BLK.  */
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
	  lisp_align_free (thr, blk);
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
sweep_intervals (struct thread_state *thr)
{
  struct interval_block **iprev = &THREAD_FIELD (thr, m_interval_blocks);
  size_t cum_free = 0, cum_used = 0;

  THREAD_FIELD (thr, m_interval_free_list) = NULL;
  for (struct interval_block *iblk; (iblk = *iprev); )
    {
      int blk_free = 0;
      ASAN_UNPOISON_INTERVAL_BLOCK (iblk);
      for (int i = 0;
	   /* For first block, process up to prevailing
	      INTERVAL_BLOCK_INDEX.  Subsequent blocks should contain
	      BLOCK_NINTERVALS items. */
	   i < (iblk == THREAD_FIELD (thr, m_interval_blocks)
		? THREAD_FIELD (thr, m_interval_block_index)
		: BLOCK_NINTERVALS);
	   ++i)
        {
          if (!iblk->intervals[i].gcmarkbit)
            {
              set_interval_parent (&iblk->intervals[i], THREAD_FIELD (thr, m_interval_free_list));
              interval_free_list = &iblk->intervals[i];
              ++blk_free;
	      ASAN_POISON_INTERVAL (&iblk->intervals[i]);
            }
          else
            {
              ++cum_used;
              iblk->intervals[i].gcmarkbit = false;
            }
        }

      /* If BLK contains only free items and we've already seen more
         than two such blocks, then deallocate BLK.  */
      if (blk_free >= BLOCK_NINTERVALS && cum_free > BLOCK_NINTERVALS)
        {
          *iprev = iblk->next;
          /* Unhook from the free list.  */
	  ASAN_UNPOISON_INTERVAL (&iblk->intervals[0]);
          THREAD_FIELD (thr, m_interval_free_list) = INTERVAL_PARENT (&iblk->intervals[0]);
          lisp_free (thr, iblk);
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
sweep_symbols (struct thread_state *thr)
{
  struct symbol_block *sblk;
  struct symbol_block **sprev = &THREAD_FIELD (thr, m_symbol_blocks);
  size_t cum_free = 0, cum_used = ARRAYELTS (lispsym);

  THREAD_FIELD (thr, m_symbol_free_list) = NULL;
  for (int i = 0; i < ARRAYELTS (lispsym); ++i)
    lispsym[i].u.s.gcmarkbit = false;

  for (sblk = THREAD_FIELD (thr, m_symbol_blocks); sblk != NULL; sblk = *sprev)
    {
      ASAN_UNPOISON_SYMBOL_BLOCK (sblk);
      int blk_free = 0;
      struct Lisp_Symbol *sym = sblk->symbols;

      /* First iteration processes up to prevailing
	 SYMBOL_BLOCK_INDEX.  Subsequent iterations traverse whole
	 blocks of BLOCK_NSYMBOLS. */
      struct Lisp_Symbol *end
	= sym + (sblk == THREAD_FIELD (thr, m_symbol_blocks)
		 ? THREAD_FIELD (thr, m_symbol_block_index)
		 : BLOCK_NSYMBOLS);

      for (; sym < end; ++sym)
        {
          if (sym->u.s.gcmarkbit)
            {
              ++cum_used;
              sym->u.s.gcmarkbit = false;
            }
	  else
            {
              if (sym->u.s.type == SYMBOL_LOCAL_SOMEWHERE)
		{
                  /* Avoid re-free (bug#29066).  */
                  sym->u.s.type = SYMBOL_PLAINVAL;
                }
              sym->u.s.next = THREAD_FIELD (thr, m_symbol_free_list);
	      sym->u.s.function = dead_object ();
              THREAD_FIELD (thr, m_symbol_free_list) = sym;
              ++blk_free;
	      ASAN_POISON_SYMBOL (sym);
            }
        }

      /* If BLK contains only free items and we've already seen more
         than two such blocks, then deallocate BLK.  */
      if (blk_free >= BLOCK_NSYMBOLS && cum_free > BLOCK_NSYMBOLS)
        {
          *sprev = sblk->next;
          /* Unhook from the free list.  */
	  ASAN_UNPOISON_SYMBOL (&sblk->symbols[0]);
          THREAD_FIELD (thr, m_symbol_free_list) = sblk->symbols[0].u.s.next;
          lisp_free (thr, sblk);
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
sweep_buffers (struct thread_state *thr)
{
  Lisp_Object tail, buf;
  (void) thr;

  gcstat.total_buffers = 0;
  FOR_EACH_LIVE_BUFFER (tail, buf)
    {
      struct buffer *buffer = XBUFFER (buf);
      /* Do not use buffer_(set|get)_intervals here.  */
      buffer->text->intervals = balance_intervals (buffer->text->intervals);
      unchain_dead_markers (buffer);
      ++gcstat.total_buffers;
    }
}

static void
gc_sweep (void)
{
#ifdef HAVE_GCC_TLS
  for (struct thread_state *thr = all_threads;
       thr != NULL;
       thr = thr->next_thread)
#else
  struct thread_state *thr = current_thread;
#endif
  {
    sweep_strings (thr);
    sweep_void (thr,
		(void **) &THREAD_FIELD (thr, m_cons_free_list),
		THREAD_FIELD (thr, m_cons_block_index),
		(void **) &THREAD_FIELD (thr, m_cons_blocks),
		Lisp_Cons,
		BLOCK_NCONS,
		offsetof (struct cons_block, conses),
		offsetof (struct Lisp_Cons, u.s.u.chain),
		offsetof (struct cons_block, next),
		sizeof (struct Lisp_Cons),
		&gcstat.total_conses,
		&gcstat.total_free_conses);
    sweep_void (thr,
		(void **) &THREAD_FIELD (thr, m_float_free_list),
		THREAD_FIELD (thr, m_float_block_index),
		(void **) &THREAD_FIELD (thr, m_float_blocks),
		Lisp_Float,
		BLOCK_NFLOATS,
		offsetof (struct float_block, floats),
		offsetof (struct Lisp_Float, u.chain),
		offsetof (struct float_block, next),
		sizeof (struct Lisp_Float),
		&gcstat.total_floats,
		&gcstat.total_free_floats);
    sweep_intervals (thr);
    sweep_symbols (thr);
    sweep_buffers (thr);
    sweep_vectors (thr);
  }
  pdumper_clear_marks ();
}

#ifdef HAVE_GCC_TLS
/* Come home.  */

void
reap_thread_allocations (struct thread_state *thr)
{
  mem_merge_into (&main_thread->m_mem_root, thr->m_mem_root);
  mem_delete_root (&thr->m_mem_root);
  eassume (thr->m_mem_root == mem_nil);

#ifdef ENABLE_CHECKING
#define CHECKSUM_BEFORE(type, next, what)				\
  do {									\
    for (struct type *p = thr->what; p != NULL; p = p->next)		\
      ++len_thr;							\
    for (struct type *p = main_thread->what; p != NULL; p = p->next)	\
      ++len_main;							\
  } while (0);
#define CHECKSUM_AFTER(type, next, what)				\
  do {									\
    for (struct type *p = main_thread->what; p != NULL; p = p->next)	\
      ++len_sum;							\
  } while (0);
#else
#define CHECKSUM_BEFORE(type, next, what)	\
  do {						\
    (void) len_thr;				\
    (void) len_main;				\
    (void) len_sum;				\
  } while (0);
#define CHECKSUM_AFTER(type, next, what)
#endif /* ENABLE_CHECKING */

#define PREPEND_ONTO_MAIN(type, next, what)			\
  do {								\
    int len_thr = 0, len_main = 0, len_sum = 0;			\
    CHECKSUM_BEFORE (type, next, what);				\
    for (struct type *p = thr->what; p != NULL; p = p->next)	\
      {								\
	if (p->next == NULL)					\
	  {							\
	    p->next = main_thread->what;			\
	    main_thread->what = thr->what;			\
	    break;						\
	  }							\
      }								\
    CHECKSUM_AFTER (type, next, what);				\
    eassert (len_sum == len_thr + len_main);			\
  } while (0);

  if (thr->m_free_ablocks)
    {
      PREPEND_ONTO_MAIN (ablock, x.next, m_free_ablocks);
    }

  if (thr->m_large_sblocks)
    {
      PREPEND_ONTO_MAIN (sblock, next, m_large_sblocks);
    }

  if (thr->m_large_vectors)
    {
      PREPEND_ONTO_MAIN (large_vector, next, m_large_vectors);
    }

  if (thr->m_current_sblock)
    {
      thr->m_current_sblock->next = main_thread->m_oldest_sblock;
      main_thread->m_oldest_sblock = thr->m_oldest_sblock;
      if (! main_thread->m_current_sblock)
	main_thread->m_current_sblock = thr->m_current_sblock;
    }

  if (thr->m_string_blocks)
    {
      PREPEND_ONTO_MAIN (string_block, next, m_string_blocks);
    }

  if (thr->m_string_free_list)
    {
      PREPEND_ONTO_MAIN (Lisp_String, u.next, m_string_free_list);
    }

  if (thr->m_interval_blocks)
    {
      PREPEND_ONTO_MAIN (interval_block, next, m_interval_blocks);
      main_thread->m_interval_block_index = thr->m_interval_block_index;
    }

  if (thr->m_interval_free_list)
    {
      for (INTERVAL p = thr->m_interval_free_list; ; p = INTERVAL_PARENT (p))
	if (INTERVAL_PARENT (p) == NULL) {
	  set_interval_parent (p, main_thread->m_interval_free_list);
	  main_thread->m_interval_free_list = thr->m_interval_free_list;
	  break;
	}
    }

  if (thr->m_float_blocks)
    {
      PREPEND_ONTO_MAIN (float_block, next, m_float_blocks);
      main_thread->m_float_block_index = thr->m_float_block_index;
    }

  if (thr->m_float_free_list)
    {
      PREPEND_ONTO_MAIN (Lisp_Float, u.chain, m_float_free_list);
    }

  if (thr->m_cons_blocks)
    {
      PREPEND_ONTO_MAIN (cons_block, next, m_cons_blocks);
      main_thread->m_cons_block_index = thr->m_cons_block_index;
    }

  if (thr->m_cons_free_list)
    {
      PREPEND_ONTO_MAIN (Lisp_Cons, u.s.u.chain, m_cons_free_list);
    }

  if (thr->m_vector_free_lists)
    {
      for (int slot = 0; slot < VBLOCK_NFREE_LISTS; ++slot)
	{
	  for (struct Lisp_Vector *p = thr->m_vector_free_lists[slot];
	       p != NULL;
	       p = vector_next (p))
	    if (vector_next (p) == NULL) {
	      vector_set_next (p, main_thread->m_vector_free_lists[slot]);
	      main_thread->m_vector_free_lists[slot] = p;
	      break;
	    }
	}
    }

  if (thr->m_vector_blocks)
    {
      PREPEND_ONTO_MAIN (vector_block, next, m_vector_blocks);
    }

  if (thr->m_symbol_blocks)
    {
      PREPEND_ONTO_MAIN (symbol_block, next, m_symbol_blocks);
      main_thread->m_symbol_block_index = thr->m_symbol_block_index;
    }

  if (thr->m_symbol_free_list)
    {
      PREPEND_ONTO_MAIN (Lisp_Symbol, u.s.next, m_symbol_free_list);
    }
}
#undef PREPEND_ONTO_MAIN
#undef CHECKSUM_BEFORE
#undef CHECKSUM_AFTER
#endif /* HAVE_GCC_TLS */

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
or memory information can't be obtained, return nil.
If `default-directory' is remote, return memory information of the
respective remote host.  */)
  (void)
{
  Lisp_Object handler
    = Ffind_file_name_handler (BVAR (current_buffer, directory),
			       Qmemory_info);
  if (!NILP (handler))
    return call1 (handler, Qmemory_info);

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

  if (!NILP (leave_padding))
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
  Lisp_Object val = find_symbol_value (sym, NULL);
  return (EQ (val, obj)
	  || EQ (sym->u.s.function, obj)
	  || (!NILP (sym->u.s.function)
	      && COMPILEDP (sym->u.s.function)
	      && EQ (AREF (sym->u.s.function, COMPILED_BYTECODE), obj))
	  || (!NILP (val)
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

   if (!deadp (obj))
     {
       for (int i = 0; i < ARRAYELTS (lispsym); ++i)
	 {
	   Lisp_Object sym = builtin_lisp_symbol (i);
	   if (symbol_uses_obj (sym, obj))
	     {
	       found = Fcons (sym, found);
	       if (--find_max == 0)
		 goto out;
	     }
	 }

       for (sblk = symbol_blocks; sblk != NULL; sblk = sblk->next)
	 {
	   struct Lisp_Symbol *asym = sblk->symbols;
	   for (int bn = 0; bn < BLOCK_NSYMBOLS; ++bn, ++asym)
	     {
	       if (sblk == symbol_blocks && bn >= symbol_block_index)
		 break;

	       Lisp_Object sym = make_lisp_ptr (asym, Lisp_Symbol);
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
  /* Start from smallest Lisp object.  */
  for (int i = sizeof (struct Lisp_Cons); i <= (1 << 8); ++i)
    make_lisp_ptr (alloca (i), Lisp_Cons);
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
  gc_cons_threshold = GC_DEFAULT_THRESHOLD;

  PDUMPER_REMEMBER_SCALAR (buffer_slot_defaults.header);
  PDUMPER_REMEMBER_SCALAR (buffer_slot_symbols.header);

  /* Nothing can be malloc'd until init_runtime().  */
  pdumper_do_now_and_after_load (init_runtime);

  update_bytes_between_gc ();
  verify_alloca ();
  init_strings ();
  init_vectors ();
}

static void
init_runtime (void)
{
  static_string_allocator = &allocate_string;
  static_vector_allocator = &allocate_vector;
  static_interval_allocator = &allocate_interval;
  mem_nil->left = mem_nil->right = mem_nil;
  mem_nil->parent = NULL;
  mem_nil->color = MEM_BLACK;
  mem_nil->start = mem_nil->end = NULL;
  eassume (main_thread_p (current_thread));
  mem_root = mem_nil;
  interval_block_index = BLOCK_NINTERVALS;
  float_block_index = BLOCK_NFLOATS;
  cons_block_index = BLOCK_NCONS;
  symbol_block_index = BLOCK_NSYMBOLS;
  most_recent_free_slot = VBLOCK_NFREE_LISTS;
  vector_free_lists = xzalloc (VBLOCK_NFREE_LISTS * sizeof (struct Lisp_Vector *));
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
prevent garbage collection during a part of the program.  But be
sure to get back to the normal value soon enough, to avoid system-wide
memory pressure, and never use a too-high value for prolonged periods
of time.
See also `gc-cons-percentage'.  */);

  DEFVAR_LISP ("gc-cons-percentage", Vgc_cons_percentage,
	       doc: /* Portion of the heap used for allocation.
Garbage collection can happen automatically once this portion of the heap
has been allocated since the last garbage collection.

By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.  But be
sure to get back to the normal value soon enough, to avoid system-wide
memory pressure, and never use a too-high value for prolonged periods
of time.

If this portion is smaller than `gc-cons-threshold', this is ignored.  */);
  Vgc_cons_percentage = make_float (0.1);

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

  DEFVAR_LISP ("post-gc-hook", Vpost_gc_hook,
	       doc: /* Hook run after garbage collection has finished.  */);
  Vpost_gc_hook = Qnil;
  DEFSYM (Qpost_gc_hook, "post-gc-hook");

  DEFSYM (Qmemory_info, "memory-info");
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
  defsubr (&Spurecopy_maybe);
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
