/* Storage allocation and gc for GNU Emacs Lisp interpreter.

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


/*

This file contains the implementation of the Emacs garbage collector:
it's a mostly-copying, mostly-generational, mostly-concurrent,
mostly-bug-free memory alllocation system written with modern virtual
memory and cache architectures in mind.

Basic organization
==================

Every Lisp [1] object in Emacs is allocated on a
type-specific heap (e.g., gc_cons_heap).  Each heap owns [2] one or
more data blocks from which heap objects are actually allocated.
The basic layout looks like this:

  +-------------------+     +-------------------+
  |  gc_heap_data     | <-- | gc_heap (const)   |
  +-------------------+     +-------------------+
           |
           | gc_heap_data.blocks, gc_heap_data.block_array
           |
           V
  +-------------------+     +-------------------+
  |      Block 0      | <-> |      Block 1      | <-> ...
  +-------------------+     +-------------------+

The garbage collector allocates each block independently (using mmap
if available), and blocks can appear in the process physical address
space in any order.  But _conceptually_, all the blocks in a given
heap form a virtual linear space. (LXX here refers to object number
XX.  The letter "L" refers to "lisp".)

  +-------------------------------------------------------
  | [Block 0      ] [Block 1      ] [Block 2      ] ...
  | [L00][L01][L02] [L03][   ][L04] [   ][L06][   ] ...
  +-----------------------*---------*---------------------
                          ^         ^
                          +---------+-- N.B. note the holes

When we allocate an object, we find the first free region big enough
to hold the object we want to allocate, then carve the new object out
of that free space.  For example, if we allocate a new object in the
above heap, the new layout might look like this:

  +-------------------------------------------------------
  | [Block 0      ] [Block 1      ] [Block 2      ] ...
  | [L00][L01][L02] [L03][L07][L04] [   ][L06][   ] ...
  +-----------------------*-------------------------------
                          ^
                          +-- New object (L07) starts here

Note how we've filled the first available hole with our new object.
If the allocator hadn't been able to find a hole, it would have added
a new block to the heap [3] and carved the new object out of the
beginning of that block.  (Every object allocated by the GC is at most
the size of a single block --- larger objects get allocated outside
the normal garbage collector.)

The allocator's hole search proceeds from a location [3] in the heap's
conceptual linear space called the "allocation tip".  After we
allocate an object, we set the heap's allocation tip to the location
in the heap just after the object we allocated.  If the allocation tip
would run off the right edge of the heap, we allocate [3] a new
physical block, append it to the heap's list of blocks, and set the
allocation tip to start of that new block.  The next allocation
searches for a hole beginning from this point and proceeding to
the right [4].

Allocation proceeds in this manner until Emacs allocates enough bytes
to trigger a garbage collection cycle.  During a garbage collection
cycle, we first scan the Emacs stack and heap to figure out which
objects are in use.  This phase is called "marking" and is discussed
in more detail below.  After marking is complete, we "sweep" the heap,
which mostly involves squishing the still-in-use objects in each heap
onto the left side of the heap's virtual linear object space.

For example, suppose we ran the garbage collection on the heap
represented by the previous diagram and found that objects L01, L03,
L07, and L06 were the only ones still in use.  After compaction, the
heap would look like this:

  +-------------------------------------------------------
  | [Block 0      ] [Block 1      ] [Block 2    ] ...
  | [L01][L03][L07] [L06][        ] [           ]
  +-------------------------------------------------------

Note that all the still-live objects are squished against the left
edge of the heap (having moved between physical blocks as necessary),
that the order of objects in the heap is preserved [5], that block 1
has a hole at the end of the block, and that block 2 contains no
objects at all.  Now, as the last [6] stage of garbage collection, we
can return unused memory to the operating system by deallocating block
2 and, on supported operating systems, discarding the memory at the
end of block 1.

At the conclusion of this garbage collection pass, the new heap heap
looks like this:

  +-----------------------------------
  | [ Block 0     ] [Block 1      ]
  | [L01][L03][L07] [L06][L06][   ]
  +----------------------------*------
                               ^
     New allocation tip  ------+

As an optimzation, we set the allocation tip to the first hole in the
heap instead of rewinding it all the way to the left edge of the
heap's allocation space: this way, the next allocation doesn't have to
uselessly skip over live objects to find the next hole.

The diagrams below will leave out the block labels and instead present
the heap as if it were one contiguous space.  The reader should keep
in mind that the heap is presented as one contiguous space, but it's
actually split into non-contiguous blocks.

Sidebar: address space and physical RAM
=======================================================

Memory is the subject of heroic myth and incorrect folklore, so it's
worth briefly talking about how it works on modern computers with
modern operating systems.  (For a more in-depth discussion, see [6].)
Essentially, what we call "memory" is actually two distinct and
separate resources: address space and physical RAM.  The operating
system manages each separately.

A process's address space is, basically a number line.  It's a set of
names (which are numbers called pointers) that a process can use to
refer to memory.  When a process "allocates" "memory" from the kernel
(e.g., using mmap), the process is really just asking the kernel to
set aside a portion of this space. It's as if the kernel has a big
ruler (one for each process!) with some pieces of tape stuck to
it. When a process asks for memory, the kernel finds a part of that
process's ruler not already covered in tape, puts some tape over it,
and then tells the process "here's the place on the ruler that I just
taped".  Deallocating memory is just removing tape from the ruler.

Memory allocation from the kernel occurs in page-sized chunks.  (Just
imagine that the kernel only has pieces of tape exactly one inch long
and always places these pieces at inch marks on the ruler.)  It's also
fairly expensive to go the kernel for memory: most memory allocation
systems (including Emacs) ask the kernel for large chunks of address
space and subdivide these chunks to satisfy small allocation requests.
Each chunk of address space that a process gets from the kernel is
called a memory mapping.

A process allocating a memory mapping of X bytes --- taping over part
of the ruler --- doesn't actually cause the kernel to go find X bytes
of RAM for that process's use.  The kernel only later finds RAM
corresponding to that memory mapping --- when the process tries to
actually use that memory.  How does that work?

When a process *writes* to a just-allocated location in its address
space, the write instruction traps into the kernel because at this
point the hardware doesn't find a physical RAM page connected to that
part of the address space. The kernel then connects physical RAM to
that part of the process's address space and retries the write, which
succeeds now, because the processes sees the connection between
address space and physical memory.

What happens when a process *reads* from a just-allocated location in
its address space?  Some kernels treat reads just like writes.
But other kernels take advantage of a property of fresh memory
mappings: they contain all zero bits at first.  These kernels wire
each new memory mapping (upon access) to a single "zero page" of
physical memory, only bothering to find a unique per-page physical RAM
for that memory mapping when a process *writes* to that memory.

Now we see that address space and physical RAM are two different
resources.  (There's actually a third memory-related resource, commit
charge, but it's not important here, so let's skip it.  Emacs keeps
its heap fully committed.)

Emacs tries to conserve both address space and RAM, but not with the
same zeal.  On modern (64-bit) computers, address space is a abundant:
pointers are 64 bits wide, giving each process address 16 exbibytes of
address space.  (It's actually a bit less, though still enormous, for
various irrelevant reasons.)  Emacs optimizes for minimizing the
frequency of address space allocation, not for the total amount of
address space used, and it makes this trade-off by using a large Lisp
heap block size: the larger the heap block size, the less often Emacs
has to ask the kernel for additional chunks of address space, but the
more space is unused inside each heap block.

But Emacs *does* care about minimizing the actual amount of RAM that
the kernel needs to set aside for Emacs, so it aggressively returns
RAM (but not address space!) to the operating system kernel during
garbage collection.  In particular, Emacs tells the kernel that
"holes" in the lisp heap don't need their contents preserved, allowing
the kernel to use the RAM that would otherwise back these pages for
other proceses.

If address space is plentiful, why does Emacs bother release it at
all?  Because on some systems (32-bit ones) address space is a much
more limited resource, and being completely profligate with it would
make Emacs worse.  And even on 64-bit systems, there's no fixed
maximum amount of address space that Emacs could reasonably set aside
and stay within.

Emacs also tells the kernel that certain bits of heap metadata (e.g.,
the marked-bit arrays) should be all zero at the end of each garbage
collection pass, allowing the kernel to wire the backing RAM to the
shared zero page.  For example, suppose we have a bitset that's 128kB
long and that we have to scan it for one bits during garbage
collection.  If that whole bitset remains zero, it doesn't cost us
128kB of actual RAM --- that region of address space is just 32
(assuming a 4kB page size) consecutive mappings of the zero page ---
and instead it's basically free.  If we set one bit in this bit array,
we need a unique page of RAM for the page holding the one bit, so that
singular bit set costs us 4kB, not 128kB.  And when we clear that one
bit and return that bit's page to the kernel, we give up that 4kB of
RAM and that 128kB bitset goes back to being free.

This way, the large sparse bitsets that the Emacs garbage collector
uses end up costing a lot less than one might expect.

Memory tree
===========

Conservative stack scanning and pinning
=======================================

Sxhash with a moving garbage collector
======================================

Generational garbage collection
===============================

Concurrent marking
==================

Inlining strategy
=================

FAQ
===

Why both cursors and locators?
------------------------------

Why not store mark bits in objects?
-----------------------------------

Storing mark bits in contiguous arrays outside the objects themselves
lets us more efficiently manipulate the mark bits and more eagerly
release memory to the operating system.

Why is pdumper still separate?
------------------------------

What about systems without virtual memory?
------------------------------------------

What about systems without threads?
-----------------------------------

Footnotes
=========


[1] The garbage collector also manages intervals, which are not
technically Lisp-accessible objects.

[2] Blocks are accessible both on a doubly-linked list and an array
for reasons discussed later.

[3] We raise memory_full if allocating that new block fails.

[4] Garbage collection nerds will recognize this allocation strategy
as first-fit allocation.  For reasons discussed later, in practice,
this allocation scheme ends up being tantamount to bump pointer
allocation in practice.

[5] Preserving order is important for maintaining reference locality.
Scrambling object order during compaction is a common mistake that
lesser garbage collectors make.

[6] As an optimization, we actually return memory to the OS before the
end of the garbage collection pass when possible.

[7] "What every programmer should know about memory":
https://lwn.net/Articles/250967/

*/

#include <config.h>

#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <limits.h>		/* For CHAR_BIT.  */
#include <signal.h>		/* For SIGABRT, SIGDANGER.  */

#ifdef HAVE_PTHREAD
#include <pthread.h>
#endif

#include "lisp.h"
#include "bignum.h"
#include "dispextern.h"
#include "intervals.h"
#include "sheap.h"
#include "sysstdio.h"
#include "systime.h"
#include "character.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "frame.h"
#include "blockinput.h"
#include "pdumper.h"
#include "termhooks.h"		/* For struct terminal.  */
#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

#ifdef HAVE_SQLITE3
#include "sqlite.h"
#endif

#ifdef HAVE_TREE_SITTER
#include "tree-sitter.h"
#endif

#include <flexmember.h>
#include <verify.h>
#include <execinfo.h>           /* For backtrace.  */

#include "bitset.h"
#include "ilist.h"
#include "sysmem.h"
#include "thread.h"
#include "alloc.h"

#ifdef HAVE_LINUX_SYSINFO
#include <sys/sysinfo.h>
#endif

#ifdef MSDOS
#include "dosfns.h"		/* For dos_memory_info.  */
#endif

#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif

#if (defined ENABLE_CHECKING \
     && defined HAVE_VALGRIND_VALGRIND_H && !defined USE_VALGRIND)
# define USE_VALGRIND 1
#endif

#if USE_VALGRIND
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>
#endif

#include <unistd.h>
#include <fcntl.h>
#include <xwidget.h>
#include <xterm.h>

#ifdef USE_GTK
# include "gtkutil.h"
#endif
#ifdef WINDOWSNT
#include "w32.h"
#endif


/* Data type definitions.  */

/* When scanning the C stack for live Lisp objects, Emacs keeps track of
   what memory allocated via lisp_malloc and lisp_align_malloc is intended
   for what purpose.  This enumeration specifies the type of memory.  */
enum mem_type
{
  MEM_TYPE_CONS,
  MEM_TYPE_STRING,
  MEM_TYPE_SYMBOL,
  MEM_TYPE_FLOAT,
  MEM_TYPE_INTERVAL,
  MEM_TYPE_VECTOR,
  /* Large vector allocated as its own memory block.  */
  MEM_TYPE_LARGE_VECTOR,
};

/* A node in the red-black tree describing allocated memory containing
   Lisp data.  Each such block is recorded with its start and end
   address when it is allocated, and removed from the tree when it
   is freed.

   A red-black tree is a balanced binary tree with the following
   properties:

   1. Every node is either red or black.
   2. Every leaf is black.
   3. If a node is red, then both of its children are black.
   4. Every simple path from a node to a descendant leaf contains
   the same number of black nodes.
   5. The root is always black.

   When nodes are inserted into the tree, or deleted from the tree,
   the tree is "fixed" so that these properties are always true.

   A red-black tree with N internal nodes has height at most 2
   log(N+1).  Searches, insertions and deletions are done in O(log N).
   Please see a text book about data structures for a detailed
   description of red-black trees.  Any book worth its salt should
   describe them.  */

struct mem_node
{
  /* Children of this node.  An absent child pointer is NULL.  */
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

/* Computes what it says on the tin.  Must be a big ugly macro because
   we want to evaluate it as a constant expression.  */
#define LOG_2_OF_POWER_OF_2(n)                           \
  (   (n) == ((unsigned long long) 1 << 63) ? 63         \
    : (n) == ((unsigned long long) 1 << 62) ? 62         \
    : (n) == ((unsigned long long) 1 << 61) ? 61         \
    : (n) == ((unsigned long long) 1 << 60) ? 60         \
    : (n) == ((unsigned long long) 1 << 59) ? 59         \
    : (n) == ((unsigned long long) 1 << 58) ? 58         \
    : (n) == ((unsigned long long) 1 << 57) ? 57         \
    : (n) == ((unsigned long long) 1 << 56) ? 56         \
    : (n) == ((unsigned long long) 1 << 55) ? 55         \
    : (n) == ((unsigned long long) 1 << 54) ? 54         \
    : (n) == ((unsigned long long) 1 << 53) ? 53         \
    : (n) == ((unsigned long long) 1 << 52) ? 52         \
    : (n) == ((unsigned long long) 1 << 51) ? 51         \
    : (n) == ((unsigned long long) 1 << 50) ? 50         \
    : (n) == ((unsigned long long) 1 << 49) ? 49         \
    : (n) == ((unsigned long long) 1 << 48) ? 48         \
    : (n) == ((unsigned long long) 1 << 47) ? 47         \
    : (n) == ((unsigned long long) 1 << 46) ? 46         \
    : (n) == ((unsigned long long) 1 << 45) ? 45         \
    : (n) == ((unsigned long long) 1 << 44) ? 44         \
    : (n) == ((unsigned long long) 1 << 43) ? 43         \
    : (n) == ((unsigned long long) 1 << 42) ? 42         \
    : (n) == ((unsigned long long) 1 << 41) ? 41         \
    : (n) == ((unsigned long long) 1 << 40) ? 40         \
    : (n) == ((unsigned long long) 1 << 39) ? 39         \
    : (n) == ((unsigned long long) 1 << 38) ? 38         \
    : (n) == ((unsigned long long) 1 << 37) ? 37         \
    : (n) == ((unsigned long long) 1 << 36) ? 36         \
    : (n) == ((unsigned long long) 1 << 35) ? 35         \
    : (n) == ((unsigned long long) 1 << 34) ? 34         \
    : (n) == ((unsigned long long) 1 << 33) ? 33         \
    : (n) == ((unsigned long long) 1 << 32) ? 32         \
    : (n) == ((unsigned long long) 1 << 31) ? 31         \
    : (n) == ((unsigned long long) 1 << 30) ? 30         \
    : (n) == ((unsigned long long) 1 << 29) ? 29         \
    : (n) == ((unsigned long long) 1 << 28) ? 28         \
    : (n) == ((unsigned long long) 1 << 27) ? 27         \
    : (n) == ((unsigned long long) 1 << 26) ? 26         \
    : (n) == ((unsigned long long) 1 << 25) ? 25         \
    : (n) == ((unsigned long long) 1 << 24) ? 24         \
    : (n) == ((unsigned long long) 1 << 23) ? 23         \
    : (n) == ((unsigned long long) 1 << 22) ? 22         \
    : (n) == ((unsigned long long) 1 << 21) ? 21         \
    : (n) == ((unsigned long long) 1 << 20) ? 20         \
    : (n) == ((unsigned long long) 1 << 19) ? 19         \
    : (n) == ((unsigned long long) 1 << 18) ? 18         \
    : (n) == ((unsigned long long) 1 << 17) ? 17         \
    : (n) == ((unsigned long long) 1 << 16) ? 16         \
    : (n) == ((unsigned long long) 1 << 15) ? 15         \
    : (n) == ((unsigned long long) 1 << 14) ? 14         \
    : (n) == ((unsigned long long) 1 << 13) ? 13         \
    : (n) == ((unsigned long long) 1 << 12) ? 12         \
    : (n) == ((unsigned long long) 1 << 11) ? 11         \
    : (n) == ((unsigned long long) 1 <<  9) ?  9         \
    : (n) == ((unsigned long long) 1 <<  8) ?  8         \
    : (n) == ((unsigned long long) 1 <<  7) ?  7         \
    : (n) == ((unsigned long long) 1 <<  6) ?  6         \
    : (n) == ((unsigned long long) 1 <<  5) ?  5         \
    : (n) == ((unsigned long long) 1 <<  4) ?  4         \
    : (n) == ((unsigned long long) 1 <<  3) ?  3         \
    : (n) == ((unsigned long long) 1 <<  2) ?  2         \
    : (n) == ((unsigned long long) 1 <<  1) ?  1         \
    : (n) == ((unsigned long long) 1 <<  0) ?  0         \
    : -1 /* undefined */)

/* Primary control on GC block size: gc_block_data_nr_bytes is the
   number of bytes in the data section of each heap block.  The number
   of objects we can fit in each block depends on the heap to which
   that block belongs.  All other GC size constants are scaled by this
   parameter.  It must be a power of two.  */
// XXX enum { gc_block_data_nr_bytes = 16 * 1024 * 1024 };
enum { gc_block_data_nr_bytes = 256 * 1024 };
verify (POWER_OF_2 (gc_block_data_nr_bytes));

/* These constants control how often we perform garbage collection.
   They're tied to heap growth.  */
static const double gc_cons_percentage_minor_default = 0.1;
static const double gc_cons_percentage_major_default = 0.3;

enum { gc_block_nr_mark_queue_entries =
  gc_block_data_nr_bytes / sizeof (Lisp_Object),
  gc_block_data_nr_pages = gc_block_data_nr_bytes / EMACS_PAGE_SIZE_MIN,
  memory_full_cons_threshold = gc_block_data_nr_bytes,
};

static const uint32_t gc_block_magic = 0xDEADBEEF;
static const uint32_t large_vector_magic = 0xF00DF4CE;
static const size_t gc_default_threshold = 100000 * word_size;
static const size_t gc_hi_threshold = PTRDIFF_MAX;

/* Amount of spare memory (in bytes) to keep in large reserve block,
   or to see whether this much is available when malloc fails on a
   larger request.  */
static const size_t gc_spare_memory = 1 << 14;

verify (POWER_OF_2 (gc_block_nr_mark_queue_entries));

/* Set to one to make sure aux blocks are allocated on the
   gc_block_data_nr_bytes alignment, possibly speeding up compacting
   collection a bit.  (XXX: benchmark.)  */
enum { gc_aux_is_aligned = 0 };

/* Set to true to use memory protection to make aux blocks
   inaccessible while they're not being used.  */
enum { gc_aux_make_inaccessible = enable_checking };

typedef struct large_vector large_vector;
typedef struct large_vector_meta large_vector_meta;

typedef struct gc_block_meminfo gc_block_meminfo;
typedef struct gc_block gc_block;
typedef struct gc_heap gc_heap;
typedef struct gc_heap_data gc_heap_data;
typedef struct gc_locator gc_locator;
typedef struct gc_cursor gc_cursor;
typedef struct gc_aux gc_aux;
typedef struct gc_field_specifier gc_field_specifier;
typedef struct gc_allocation gc_allocation;
typedef struct gc_vector_allocation gc_vector_allocation;
typedef struct gc_tospace_placer gc_tospace_placer;
typedef struct sdata sdata;
typedef struct gc_igscan gc_igscan;
typedef struct gc_gen_y_bit_iter gc_gen_y_bit_iter;

typedef bool (*gc_heap_enumerator)(void *obj, void *data);

#define BLOCK_SIZE(STRUCT) \
  (sizeof (STRUCT))
#define BLOCK_CALC(STRUCT) \
  gc_block_data_nr_bytes / BLOCK_SIZE(STRUCT)

enum {
  gc_slot_size = GCALIGNMENT,
  gc_heap_cons_block_count = BLOCK_CALC(struct Lisp_Cons),
  gc_heap_float_block_count = BLOCK_CALC(struct Lisp_Float),
  gc_heap_symbol_block_count = BLOCK_CALC(struct Lisp_Symbol),
  gc_heap_interval_block_count = BLOCK_CALC(struct interval),
  gc_heap_string_block_count = BLOCK_CALC(struct Lisp_String),
  gc_heap_vector_block_count =
  gc_block_data_nr_bytes / gc_slot_size,
  large_vector_min_nr_bytes = gc_block_data_nr_bytes + 1,
};

enum {
  gc_heap_cons_slot_size = BLOCK_SIZE(struct Lisp_Cons),
  gc_heap_float_slot_size = BLOCK_SIZE(struct Lisp_Float),
  gc_heap_symbol_slot_size = BLOCK_SIZE(struct Lisp_Symbol),
  gc_heap_interval_slot_size = BLOCK_SIZE(struct interval),
  gc_heap_string_slot_size = BLOCK_SIZE(struct Lisp_String)
};

verify (gc_slot_size >= GCALIGNMENT);
verify (gc_slot_size % GCALIGNMENT == 0);
verify (gc_block_data_nr_bytes % EMACS_PAGE_SIZE_MAX == 0);

/* Standard iterator for the heap.  Always points at a valid object
   slot --- never past the end.  */
struct gc_cursor {
  gc_block *block;
  ptrdiff_t slot_nr;
};

/* gc_locator: an alternative to gc_cursor for specifying a location
   in a heap.  A gc_locator is more compact than a cursor (32 bits
   typically), but requires an additional memory access to get the
   corresponding block.  We use use locators to plan object
   relocations during compaction.

   N.B. locators are invalidated across GCs (because block numbers
   change) but cursors are stable (as long as the underlying block to
   which a cursor points isn't deleted).

   TODO: implement short locators. Until we do, all locators are
   actually the size of a machine word.  */

enum {
  gc_locator_nr_bits_slot_nr =
  LOG_2_OF_POWER_OF_2 (gc_block_data_nr_bytes / gc_slot_size),
  gc_locator_max_nr_slots =
  1 << gc_locator_nr_bits_slot_nr,
  gc_locator_nr_bits_block_nr = sizeof (size_t) * CHAR_BIT -
  gc_locator_nr_bits_slot_nr,
  /* The maximum number of blocks a single heap can have is determined
     by the size of the GC heap.  */
  gc_maximum_block_nr =
  (((size_t) 1 << gc_locator_nr_bits_block_nr)
   - 2 /* -2, not -1, so that gc_locator_invalid (which has all one
          bits) never conflicts with a real locator.  */),
};
verify (gc_locator_max_nr_slots >= gc_block_data_nr_bytes / gc_slot_size);
verify (gc_maximum_block_nr <= PTRDIFF_MAX);
verify (gc_locator_max_nr_slots <= INT32_MAX);

struct gc_locator {
  size_t i;
};

static const gc_locator gc_locator_invalid = { .i = (size_t) -1 };

verify (sizeof (gc_locator) == sizeof (size_t));
verify (gc_locator_nr_bits_slot_nr +
        gc_locator_nr_bits_block_nr == sizeof (size_t) * CHAR_BIT);

enum { gc_block_nr_maps = 1 };

struct gc_block_meminfo {
  struct emacs_memory_map maps[gc_block_nr_maps];
};

#define GC_HEAP_BITS(name, nr_bits)                                     \
  struct {                                                              \
    emacs_bitset_word mark[                                             \
      EMACS_BITSET_NR_NEEDED_WORDS (nr_bits)];                          \
    emacs_bitset_word start[                                            \
      EMACS_BITSET_NR_NEEDED_WORDS (nr_bits)];                          \
    emacs_bitset_word pinned[                                           \
      EMACS_BITSET_NR_NEEDED_WORDS (nr_bits)];                          \
    emacs_bitset_word perma_pinned[                                     \
      EMACS_BITSET_NR_NEEDED_WORDS (nr_bits)];                          \
    gc_locator tospace[nr_bits];                                        \
  } name

#define GC_FIELD_SPECIFIER(field)                       \
  (struct gc_field_specifier) {                         \
    .offset = offsetof (gc_block, field),               \
      .nr_bytes = sizeof (((gc_block *)NULL)->field),   \
  }

#define GC_HEAP_BITS_CONFIG(name)                                       \
  .block_size = EMACS_SIZE_THROUGH_FIELD (gc_block, u2.name),           \
    .mark = GC_FIELD_SPECIFIER (u2.name.mark),                          \
    .start = GC_FIELD_SPECIFIER (u2.name.start),                        \
    .pinned = GC_FIELD_SPECIFIER (u2.name.pinned),                      \
    .perma_pinned = GC_FIELD_SPECIFIER (u2.name.perma_pinned),          \
    .tospace = GC_FIELD_SPECIFIER (u2.name.tospace)

/* One block in the heap.  */
struct gc_block {
  /* Actual Lisp object storage.  */
  union {
    uint8_t data[gc_block_data_nr_bytes];
    Lisp_Object mark_queue[gc_block_nr_mark_queue_entries];
  } u;
  /* Block metadata: on its own page.  */
  struct {
    /* Link of this block on its block list.  */
    emacs_list_link link;
    /* Number of this block within its block list.  */
    size_t block_nr;
    union {
      struct {
        struct {
          size_t nr_reads;
          size_t nr_writes;
        } mark_queue;
      } aux;
      struct {
        struct {
          /* The minimum slot-bitset word number that's part of the
             generation we're collecting.  Set by GC core at the start
             of each cycle in gc_heap_prepare().  We don't want to
             bother with masking off individual bits inside our
             metadata bit words, so we set the generation boundary
             such that it lines up with a boundary between bitset
             words.  Consequently, the young generation ends up being
             slightly larger than necessary, but only by a few
             kilobytes.  */
          ptrdiff_t gen_y_slot_nr;
          /* The tospace we've allocated for this block.  The tospace
             is an aux block.  */
          gc_block *tospace;
          /* The smallest tospace block number of any object in this
             block (when regarded as fromspace).  Set by
             gc_block_plan_sweep(); consumed by
             gc_heap_sweep_compact() to perform opportunistic early
             tospace cleanup.  */
          size_t smallest_tospace_block_nr;
        } per_cycle;
      } heap;
    } u;
    /* Memory tree node for this block.  */
    struct mem_node mem;
    /* Memory mappings for this block.  */
    gc_block_meminfo meminfo;
    /* Used for validating block pointers.  */
    uint32_t magic;
    /* Heap to which this block belongs --- for debugging only, since
      all GC functions should receive the heap as a function
      parameter.  */
    const gc_heap *owning_heap_for_debug;
    /* Track modified pages in this heap for generational GC --- one
       bit per operating system page, with each bit set in
       gc_try_handle_sigsegv.  */
    emacs_bitset_word card_table[
      EMACS_BITSET_NR_NEEDED_WORDS (gc_block_data_nr_pages)];
  } meta;
  union {
    struct {
      char dummy[0];
    } aux;
    GC_HEAP_BITS (cons, gc_heap_cons_block_count);
    GC_HEAP_BITS (float_, gc_heap_float_block_count);
    /* TODO: avoid providing perma-pinned bits for symbols */
    GC_HEAP_BITS (symbol, gc_heap_symbol_block_count);
    GC_HEAP_BITS (interval, gc_heap_interval_block_count);
    GC_HEAP_BITS (string, gc_heap_string_block_count);
    GC_HEAP_BITS (vector, gc_heap_vector_block_count);
  } u2;
};

verify (offsetof (gc_block, meta) % EMACS_PAGE_SIZE_MAX == 0);

enum { gc_aux_block_nr_bytes = EMACS_SIZE_THROUGH_FIELD (gc_block, u2.aux) };

static const size_t gc_block_nr_invalid = -1;

struct gc_field_specifier {
  ptrdiff_t offset;
  ptrdiff_t nr_bytes;
};

/* A lisp heap.  The fields in this object are initialized statically,
   and any decent compiler should be able to constant-propagate them
   to any code reading this structure.  Data that changes at runtime
   goes in gc_heap_data, to which this structure points.  */
struct gc_heap {
  /* Runtime-variable data for this heap.  The data should live in the
     Emacs data section and should be zero initialized.  (Everything
     in the data section is zero initialized by default, so there's no
     need to do anything special to make it so.)  */
  gc_heap_data *data;
  /* Lisp tag for objects in this heap.  */
  enum Lisp_Type lisp_type;
  /* Memory type for blocks in this heap.  */
  enum mem_type mem_type;
  /* Index of the C symbol used to describe this heap.  */
  int heap_symbol_index;
  /* Size of the block needed for this heap.  */
  size_t block_size;
  /* If true, blocks are allocated using aligned memory
     so that we can find the block start from any object's
     address without consulting the global memory tree.  */
  bool aligned_blocks;
  /* If true, objects in this heap are compacted during collection.  */
  bool use_moving_gc;
  /* If true, objects in this heap do not refer to GC-managed objects.   */
  bool no_lisp_data;
  /* If true and use_moving_gc, objects in this heap's fromspace stay
     alive throughout the sweep.  Set this flag to true when we have
     to read an object's contents (e.g., to find its pseudovector
     type) to figure out its tospace location.  */
  bool preserve_fromspace_across_sweeps;
  /* Size of objects in the heap if all objects are the same size;
     if zero, objects have variable size.  */
  size_t homogeneous_object_nr_bytes;
  /* Mark bits for this heap.  */
  gc_field_specifier mark;
  /* Object-start bits for the heap.  */
  gc_field_specifier start;
  /* Object-pinned bits for this heap.  */
  gc_field_specifier pinned;
  /* Object-perma-pinned bits for this heap.  */
  gc_field_specifier perma_pinned;
  /* Locators for moving GC.  */
  gc_field_specifier tospace;
  /* Called on each object P that does not survive GC.  Use to clean
     up resources owned by the object.  If NULL, do not call a
     function on doomed objects.  */
  void (*cleanup)(void *p);
  /* Hook to retrieve the size (in bytes) of an object P on the heap.
     Required if homogeneous_object_nr_bytes is zero.  Must be NULL if
     homogeneous_object_nr_bytes is supplied; must be non-NULL
     otherwise.  */
  size_t (*object_nr_bytes)(const void * p);
  /* Hook to use with intergenerational scanning.  Return true to take
     over scanning duties from the regular loop.  */
  bool (*igscan_hook)(void *obj_ptr, gc_phase phase, gc_igscan *scan);
  /* Heap-specific phase-implementing functions.  GC core calls
     through these in order to generate better specializations and
     stack traces.  */
  void (*block_mark_intergenerational)(gc_block *, void *);
  void (*block_plan_sweep)(gc_block *, void *);
  void (*block_sweep_inplace)(gc_block *, void *);
  void (*block_sweep_compact)(gc_block *, void *);
};

/* Mutable portion of a GC heap.  Referenced from a gc_heap: because
   the gc_heap is constant, there's no additional pointer indirection
   for finding the corresponding gc_heap_data.  */
struct gc_heap_data {
  /* Current allocation tip: we try allocating new objects from this
     point, skipping over any parts of the heap already allocated to
     objects.  */
  gc_cursor allocation_tip;
  /* The point at which we stopped the last garbage collection pass.
     Every object allocated beyond this point is part of the young
     generation, allocated since the last garbage collection.
     Objects that survive a minor GC get promoted into the old
     generation.  Consumed by gc_heap_prepare() to set each block's
     gen_y_slot_nr; consumed by gc_heap_plan_sweep() to
     figure out where to start placing young-generation objects in
     tospace; set by gc_heap_plan_sweep() when it finishes
     figuring out where all the tospace objects for the current cycle
     (all of which are being promoted to old generation) are going to
     live.  */
  gc_cursor last_gc_tospace_tip;
  /* All blocks that belong to this heap.
     TODO: rely entirely on the blocks array. */
  emacs_list_head blocks;
  /* Array of all blocks on this heap.  We keep blocks both on a
     doubly-linked list and in this array.  */
  size_t block_array_capacity;
  size_t block_array_size;
  gc_block **block_array;
  struct {
    struct {
      /* Whether gc_block_prepare() has seen the tospace_tip block.  */
      bool saw_last_gc_tospace_tip;
    } prepare;
    struct {
      /* The next block for which we need to collapse the tospace into
         the fromspace.  Updated by
         gc_heap_flip_next_tospace_block(). */
      size_t next_tospace_flip_block_nr;
      /* Whether gc_block_flip_tospace_to_fromspace has seen the new
         tospace_tip block.  */
      bool saw_new_tospace_tip;
    } sweep;
    struct {
      bool locators_destroyed;
    } debug;
  } per_cycle;
  struct {
    ptrdiff_t nr_objects;
    ptrdiff_t nr_slots;
    ptrdiff_t nr_objects_pinned;
    ptrdiff_t nr_slots_pinned;
  } stats;
};

struct gc_allocation {
  gc_cursor obj_c;
  bool is_zero;
#ifdef ENABLE_CHECKING
  size_t nr_bytes;
#endif
};

struct gc_vector_allocation {
  union {
    struct {
      gc_allocation r;
    } small;
    struct {
      struct Lisp_Vector *v;
      bool is_zero;
    } large;
  } u;
  bool is_large;
};

struct gc_tospace_placer {
  gc_locator next_tospace_locator;
  emacs_bitset_word tospace_start_word;
  emacs_bitset_word *tospace_start_wordp;
  uint8_t *tospace_start;
  uint8_t *tospace;
  uint8_t *tospace_end;
};

/* Pseudovector holding string data.  */
struct sdata {
  /* In the large case, the "other" field of the header is zero.
     There's special code in vectorlike_nbytes() to get the real word
     length from u.large.len in this case.  On a 64-bit machine, we
     use the medium layout for strings of up to 32k bytes.  */
  union vectorlike_header header;
  union {
    struct {
      unsigned char data[1];    /* Actually flexible.  */
    } medium;
    struct {
      ptrdiff_t len;            /* Real number words.  */
      unsigned char data[1];    /* Actually flexible.  */
    } large;
  } u;
} GCALIGNED_STRUCT;

/* Data we access frequently during garbage collection, all colocated
   on the same cache line.

   N.B. order the fields to eliminate padding.

   TODO: look into plumbing this information through GC as function
   parameters instead, at least on machines that aren't
   register-starved.  */
struct gc_hot_per_cycle_data {
  /* Start of the pdumper image.  */
  uintptr_t pdumper_start;
  /* Stack address at which we begin the mark phase of garbage
     collection.  We use this value to check whether we've recursively
     marked too deeply and should enqueue an object in the mark buffer
     instead.  The location of the pdumper image never changes across
     an Emacs invocation, but we still store it in
     gc_hot_per_cycle_data to minimize the number of cache lines we
     need to access during marking.  */
  uintptr_t gc_phase_mark_stack_bottom_la;
  /* Size of the pdumper image.  Store the size (int32_t) instead of
     the end pointer (pointer size) so that we can free up 32 bits of
     the cache line on 64-bit systems.  */
  int32_t pdumper_size;
  /* Whether we're performing a major collection.  */
  bool major;
};

/* Context information for igscan --- scanning old-generation objects
   in the heap that might have pointers to new-generation objects.  */
struct gc_igscan {
  /* Block we're searching.  */
  gc_block *const b;
  /* Don't search beyond this slot.  SLOT_NR below may go beyond this
     limit if an object straddles the slot_limit.  */
  const ptrdiff_t slot_limit;
  /* Current slot number: may be the last slot.  May be at most
     gc_heap_nr_slots_per_block(H).  */
  ptrdiff_t slot_nr;
  /* Start bits of the current word to which slot_nr belongs shifted
     so that the bit corresponding to slot_nr is the LSB.  Unspecified
     if slot_nr >= slot_limit.  */
  emacs_bitset_word start_word;
  /* Cache of the dirty state of the page to which SLOT_NR points.
     Unspecified if slot_nr >= slot_limit.  */
  bool page_is_dirty;
};

/* Keeps track of state for iterating only those bits in block slot
   bitsets that correspond to younger generations.  */
struct gc_gen_y_bit_iter {
  const size_t first_word_nr;
  const size_t word_nr_limit;
  const emacs_bitset_word first_word_gen_y_mask;
  size_t word_nr;
};

/* Metadata for large_vector.  We put this data structure in a
   separate heap-allocated structure (as opposed to putting it inside
   large_vector) so that page-based change tracking in large_vector
   doesn't see GC-interal data changes as potential lisp content
   changes.  */
struct large_vector_meta {
  /* Pointer to the next large_vector_meta structure on the chain
     of all large vector metadata structures.  */
  large_vector_meta *next;
  /* The memory node for the large vector: its start is a pointer to
     the large_vector structure.  */
  struct mem_node mem;
  /* Whether we've marked this large vector on the current GC pass.  */
  bool marked;
  /* Whether this vector has been promoted to the old generation.  */
  bool gen_o;
  /* Per-page modification flags used to track intergenerational
     pointers.  */
  emacs_bitset_word card_table[];
};

/* This internal type is used to maintain the list of large vectors
   which are allocated at their own, e.g. outside of vector blocks.
   This structure contains the actual content of the vector.  */
struct large_vector {
  large_vector_meta meta;
#ifdef ENABLE_CHECKING
  uint32_t magic;
#endif
  union {
    union vectorlike_header header;
    GCALIGNED_UNION_MEMBER;
  } u;
};


/* Prototypes.  */

/* The table of prototypes below lists each static function
   in this file and an inlining approach for each.

   GCFN: always expand into caller
   COLD: separate function, unlikely to be called
   NOIL: standalone, but likely to be called
   NRML: let the compiler decide

   It's important that the GCFN functions are inlined: we use them as
   "templates" that get specialized at each call site to the specific
   heap and phase that each is called to work on.  If we were writing
   C++, we'd just use compile-time specialization to generate
   heap-specific code; here, we rely on compiler inline expansion,
   constant propagation, and dead code elimination to achieve the same
   result.

   However, we don't inline these functions in non-optimized builds:
   without optimization, we get all the code bloat of forced inlining
   and none of the gains from specialization, so in this mode, just
   turn off the inling.  */

#ifdef __OPTIMIZE__
# define GCFN static inline ALWAYS_INLINE
#else
# define GCFN static
#endif
#define COLD static _GL_ATTRIBUTE_COLD NO_INLINE
#define NOIL static NO_INLINE
#define NRML static

/* This boilerplate is necessary for ensuring that the compiler
   doesn't inline calls to heap-specific GC helper functions.
   Generating it with macros isn't ideal, but it does avoid mechanical
   repetition in heap definitions.  */

#define DECLARE_STANDARD_HEAP_FUNCTIONS(heap)                           \
  NOIL void heap##_block_mark_intergenerational (gc_block *, void *);   \
  NOIL void heap##_block_plan_sweep (gc_block *, void *);               \
  NOIL void heap##_block_sweep_inplace (gc_block *, void *);            \
  NOIL void heap##_block_sweep_compact (gc_block *, void *);

#define CONFIG_STANDARD_HEAP_FUNCTIONS(heap)                            \
  .block_mark_intergenerational = heap##_block_mark_intergenerational   \
 ,.block_plan_sweep = heap##_block_plan_sweep                           \
 ,.block_sweep_inplace = heap##_block_sweep_inplace                     \
 ,.block_sweep_compact = heap##_block_sweep_compact

#define DEFINE_STANDARD_HEAP_FUNCTIONS(heap)                            \
  void heap##_block_mark_intergenerational (gc_block *b, void *d)       \
  { gc_block_scan_intergenerational (b, GC_PHASE_MARK, &heap); }        \
  void heap##_block_plan_sweep (gc_block *b, void *d)                   \
  { gc_block_plan_sweep (b, d, &heap); }                                \
  void heap##_block_sweep_inplace (gc_block *b, void *d)                \
  { gc_block_sweep_inplace (b, &heap); }                                \
  void heap##_block_sweep_compact (gc_block *b, void *d)                \
  { gc_block_sweep_compact (b, &heap); }

NRML void garbage_collect (bool);
NRML Lisp_Object gc_make_heap_info (const gc_heap *);
NRML bool survives_gc_p (Lisp_Object) ATTRIBUTE_PURE;
NRML void gc_any_object_pin (Lisp_Object);
NRML void gc_any_object_point_into_tospace (Lisp_Object *);
GCFN void gc_any_object_point_into_tospace_pointer (void *, enum Lisp_Type);
GCFN gc_tospace_placer gc_tospace_placer_init (gc_cursor, const gc_heap *);
GCFN void * gc_tospace_placer_place (gc_tospace_placer *, gc_locator, ptrdiff_t, const gc_heap *);
GCFN void gc_tospace_placer_flush (gc_tospace_placer *, const gc_heap *);
NRML bool gc_aux_add_block (void);
NRML bool gc_aux_adjust (void);
NRML gc_block *gc_aux_pop (void);
NRML void gc_aux_push (gc_block *);
COLD gc_block *gc_block_allocate (const gc_heap *);
GCFN bool gc_block_is_any_bit_set (const emacs_bitset_word *, const gc_heap *);
COLD void gc_block_free (gc_block *, const gc_heap *);
NRML gc_block *gc_block_from_link (emacs_list_link *);
NRML gc_block *gc_block_from_mem_node (const struct mem_node *);
GCFN gc_cursor gc_block_make_cursor (const gc_block *, ptrdiff_t, const gc_heap *);
GCFN emacs_bitset_word *gc_block_mark_bits (const gc_block *, const gc_heap *);
GCFN gc_locator *gc_block_locators (const gc_block *, const gc_heap *);
GCFN void *gc_block_maybe_find_live_object_containing (const gc_block *, const void *, const gc_heap *);
GCFN void gc_block_zero_gen_y_slot_bitset (gc_block *, emacs_bitset_word *, const gc_heap *);
GCFN void gc_block_copy_gen_y_slot_bitset (gc_block *, emacs_bitset_word *, const emacs_bitset_word *, const gc_heap *);
GCFN emacs_bitset_word *gc_block_start_bits (const gc_block *, const gc_heap *);
NRML void gc_block_prepare (gc_block *, void *);
GCFN void gc_block_sweep_compact (gc_block *, const gc_heap *);
GCFN void gc_block_sweep_inplace (gc_block *, const gc_heap *);
GCFN void gc_block_flip_tospace_to_fromspace (gc_block *, const gc_heap *);
GCFN void gc_block_cleanup_after_all_sweeps (gc_block *, void *);
NRML bool gc_block_mark_card_table (gc_block *, void *, const gc_heap *);
NRML void gc_block_write_protect_gen_o (gc_block *, const gc_heap *);
NRML void gc_block_write_unprotect (gc_block *, const gc_heap *);
GCFN ptrdiff_t gc_block_gen_y_slot_nr (gc_block *, const gc_heap *);
GCFN bool gc_block_has_any_gen_y (gc_block *, const gc_heap *);
GCFN gc_block *gc_block_check (const gc_block *);
GCFN void gc_cursor_advance_next_block_allocate_if_needed (gc_cursor *, const gc_heap *);
GCFN bool gc_cursor_advance_next_block (gc_cursor *, const gc_heap *);
GCFN void gc_cursor_advance_nr_slots_allocate_if_needed (gc_cursor *, ptrdiff_t, const gc_heap *);
GCFN bool gc_cursor_is_gen_y (gc_cursor c, const gc_heap *h);
GCFN bool gc_cursor_advance_nr_slots (gc_cursor *, ptrdiff_t, const gc_heap *);
GCFN bool gc_cursor_advance_to_object_start_in_block (gc_cursor *, ptrdiff_t, const gc_heap *);
GCFN bool gc_cursor_advance_to_object_pin_in_block (gc_cursor *, ptrdiff_t, const gc_heap *);
GCFN bool gc_cursor_advance_to_object_start (gc_cursor *, const gc_heap *);
GCFN void gc_cursor_check (gc_cursor, const gc_heap *);
GCFN ptrdiff_t gc_cursor_nr_slots_left_in_block (gc_cursor, const gc_heap *);
GCFN ptrdiff_t gc_cursor_object_nr_slots (gc_cursor, const gc_heap *);
GCFN size_t gc_cursor_object_nr_bytes (gc_cursor, const gc_heap *);
GCFN bool gc_cursor_is_object_marked (gc_cursor, const gc_heap *);
GCFN void gc_cursor_make_object_start_here (gc_cursor, const gc_heap *);
GCFN void gc_allocation_commit (gc_allocation *, const gc_heap *);
GCFN bool gc_cursor_object_starts_here (gc_cursor, const gc_heap *);
GCFN void gc_cursor_set_object_marked (gc_cursor, const gc_heap *);
GCFN void gc_cursor_perma_pin_object (gc_cursor, const gc_heap *);
GCFN void gc_cursor_set_object_pinned (gc_cursor, const gc_heap *);
GCFN void gc_cursor_set_object_tospace_locator (gc_cursor, gc_locator, const gc_heap *);
GCFN gc_locator gc_cursor_get_object_tospace_locator (gc_cursor, const gc_heap *);
GCFN void *gc_cursor_to_object (gc_cursor, const gc_heap *);
GCFN gc_locator gc_cursor_to_locator (gc_cursor, const gc_heap *);
NRML void gc_global_object_limit_decrease (size_t);
NRML void gc_heap_add_block (const gc_heap *);
GCFN gc_allocation gc_heap_allocate (const gc_heap *, size_t);
GCFN gc_allocation gc_heap_allocate_and_zero (const gc_heap *, size_t);
GCFN gc_cursor gc_heap_allocate_tospace (const gc_heap *, gc_cursor *, ptrdiff_t);
GCFN gc_cursor gc_heap_make_start_cursor (const gc_heap *);
GCFN size_t gc_heap_maximum_objects_per_block (const gc_heap *);
GCFN ptrdiff_t gc_heap_maximum_object_nr_slots (const gc_heap *);
GCFN size_t gc_heap_nr_bytes_per_slot (const gc_heap *);
GCFN ptrdiff_t gc_heap_nr_slots_per_block (const gc_heap *);
GCFN size_t gc_heap_nr_slot_bitset_words (const gc_heap *);
GCFN emacs_bitset_word *gc_block_pinned_bits (const gc_block *, const gc_heap *);
GCFN emacs_bitset_word *gc_block_perma_pinned_bits (const gc_block *, const gc_heap *);
GCFN ptrdiff_t gc_heap_object_size_to_nr_slots (const gc_heap *, size_t);
GCFN void gc_heap_flip_next_tospace_block (const gc_heap *);
NOIL void gc_heap_sweep (const gc_heap *);
NRML void gc_heap_finalize_tospace (const gc_heap *);
NRML void gc_heap_for_each_block (const gc_heap *, void (*)(gc_block *, void *), void *);
NOIL void gc_heap_cleanup_after_all_sweeps (const gc_heap *);
NOIL void gc_heap_plan_sweep (const gc_heap *);
NRML Lisp_Object gc_interval_smuggle (INTERVAL);
NRML INTERVAL gc_interval_unsmuggle (Lisp_Object);
NRML bool gc_mark_drain_one (Lisp_Object *);
NRML void gc_mark_drain_queue (void);
NRML void gc_mark_enqueue_1 (Lisp_Object);
NRML void gc_mark_or_enqueue (Lisp_Object);
NRML bool gc_mark_queue_block_try_add (gc_block *, Lisp_Object);
NRML bool gc_mark_queue_block_try_remove (gc_block *, Lisp_Object *);
GCFN bool gc_object_is_marked (const void *, const gc_heap *);
GCFN void gc_object_set_marked (void *, const gc_heap *);
GCFN void gc_object_set_pinned (void *, const gc_heap *);
GCFN void gc_object_perma_pin (void *, const gc_heap *);
GCFN void *gc_object_point_into_tospace (void *, const gc_heap *);
GCFN gc_cursor gc_object_to_cursor (const void *, const gc_heap *);
GCFN void scan_reference (Lisp_Object *, gc_phase);
GCFN void scan_reference_pinned (Lisp_Object, gc_phase);
GCFN void scan_reference_pointer_to_interval (INTERVAL *, gc_phase);
GCFN void scan_reference_interval_pinned (INTERVAL, gc_phase);
GCFN void scan_reference_pointer_to_symbol (struct Lisp_Symbol **, gc_phase);
GCFN gc_cursor gc_locator_to_cursor (gc_locator, const gc_heap *);
GCFN bool gc_locator_invalid_p (gc_locator, const gc_heap *);
GCFN ptrdiff_t gc_locator_slot_nr (gc_locator, const gc_heap *);
GCFN size_t gc_locator_block_nr (gc_locator, const gc_heap *);
GCFN void gc_block_preprocess_perma_pins (gc_block *, const gc_heap *);
GCFN void gc_block_scan_intergenerational (gc_block *, gc_phase, const gc_heap *);
GCFN bool gc_igscan_advance_nr_slots_test_dirty (gc_igscan *, ptrdiff_t, const gc_heap *);
GCFN void gc_igscan_scan_object (gc_igscan *, gc_phase, const gc_heap *);
GCFN void gc_igscan_skip_clean_pages (gc_igscan *, const gc_heap *);
GCFN void gc_igscan_skip_to_object_start (gc_igscan *, const gc_heap *);
GCFN ptrdiff_t gc_igscan_get_next_dirty_slot_nr (const gc_igscan *, const gc_heap *);
GCFN void gc_block_plan_sweep (gc_block *, gc_cursor *, const gc_heap *);
GCFN void scan_reference_pointer_to_vectorlike_1 (void *, union vectorlike_header *, gc_phase);
GCFN void scan_reference_pointer_to_vectorlike_2 (union vectorlike_header **, gc_phase);
NRML void scan_maybe_pointer (void *, gc_phase);
NRML void scan_maybe_object (Lisp_Object, gc_phase);
NRML void scan_memory (void const *, void const *, gc_phase);
NRML Lisp_Object watch_gc_cons_threshold (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
NRML Lisp_Object watch_gc_cons_percentage (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
NRML void scan_object_root_visitor_mark (Lisp_Object *, enum gc_root_type, void *);
NRML void scan_object_root_visitor_sweep (Lisp_Object *, enum gc_root_type, void *);
NRML void scan_roots (gc_phase);
NOIL void gc_phase_prepare (bool);
NOIL void gc_phase_mark (void);
NOIL void gc_phase_plan_sweep (void);
NOIL void gc_phase_sweep (void);
NOIL void gc_phase_cleanup (bool);
NRML void init_alloc_once_for_pdumper (void);
NRML void refill_memory_reserve (void);
GCFN bool gc_pdumper_object_p (const void *) _GL_ATTRIBUTE_CONST;
NRML size_t gc_compute_total_live_nr_bytes (void);
NRML void compact_all_buffers (void);
NRML void recompute_consing_until_gc (void);

NRML void scan_doomed_finalizers (gc_phase);
GCFN void gc_heap_check_field (const gc_heap *, const gc_field_specifier *, size_t, size_t);
GCFN void gc_heap_check_bitset (const gc_heap *, const gc_field_specifier *);
NRML void gc_heap_enumerate (const gc_heap *, gc_heap_enumerator, void *);
NRML void gc_heap_grow_block_array (const gc_heap *);
GCFN bool gc_heap_is_moving_gc_this_cycle (const gc_heap *);
GCFN bool gc_heap_has_separate_tospace (const gc_heap *);
NOIL void gc_heap_prepare (const gc_heap *, bool);
NRML void gc_heap_init (const gc_heap *);
GCFN void scan_object (void *, enum Lisp_Type, gc_phase);
NRML void scan_object_for_marking_ool (void *, enum Lisp_Type);
NRML void scan_object_for_sweeping_ool (void *, enum Lisp_Type);

DECLARE_STANDARD_HEAP_FUNCTIONS (gc_cons_heap);
NRML bool cons_marked_p (const struct Lisp_Cons *);
NRML void set_cons_marked (struct Lisp_Cons *);
NRML void set_cons_pinned (struct Lisp_Cons *);
NRML EMACS_INT cons_identity_hash_code (struct Lisp_Cons *);
NRML Lisp_Object live_cons_holding (const struct mem_node *, const void *);
NOIL void gc_mark_or_enqueue_cons (struct Lisp_Cons *);
NRML bool live_cons_p (const struct mem_node *, const void *);
GCFN void scan_cons (struct Lisp_Cons *, gc_phase);
NRML struct Lisp_Cons *point_cons_into_tospace (struct Lisp_Cons *);
NRML Lisp_Object cons_listn (ptrdiff_t, Lisp_Object, Lisp_Object (*) (Lisp_Object, Lisp_Object), va_list);

DECLARE_STANDARD_HEAP_FUNCTIONS (gc_float_heap);
NRML bool float_marked_p (const struct Lisp_Float *);
NRML void set_float_marked (struct Lisp_Float *);
NRML void set_float_pinned (struct Lisp_Float *);
NRML EMACS_INT float_identity_hash_code (struct Lisp_Float *);
NRML Lisp_Object live_float_holding (const struct mem_node *, const void *);
NRML bool live_float_p (const struct mem_node *, const void *);
NRML struct Lisp_Float *point_float_into_tospace (struct Lisp_Float *);
NOIL void gc_mark_or_enqueue_float (struct Lisp_Float *);

DECLARE_STANDARD_HEAP_FUNCTIONS (gc_symbol_heap);
NRML bool symbol_marked_p (const struct Lisp_Symbol *);
NRML void set_symbol_marked (struct Lisp_Symbol *);
NRML void set_symbol_pinned (struct Lisp_Symbol *);
NRML Lisp_Object live_symbol_holding (const struct mem_node *, const void *);
NRML bool live_symbol_p (const struct mem_node *, const void *);
NRML void symbol_cleanup (void *);
GCFN void scan_symbol (struct Lisp_Symbol *, gc_phase);
GCFN void scan_localized_symbol (struct Lisp_Symbol *, gc_phase);
NRML struct Lisp_Symbol *point_symbol_into_tospace (struct Lisp_Symbol *);
NRML bool symbol_uses_obj (Lisp_Object, Lisp_Object);
NRML bool which_symbols_callback (void *, void *);
NOIL void gc_mark_or_enqueue_symbol (struct Lisp_Symbol *);

DECLARE_STANDARD_HEAP_FUNCTIONS (gc_interval_heap);
NRML bool interval_marked_p (const struct interval *);
NRML void set_interval_marked (INTERVAL);
NRML void set_interval_pinned (INTERVAL);
NRML INTERVAL live_interval_holding (const struct mem_node *, const void *);
GCFN void scan_interval (INTERVAL, gc_phase);
NRML INTERVAL point_interval_into_tospace (INTERVAL);
NOIL void gc_mark_or_enqueue_interval (INTERVAL);

DECLARE_STANDARD_HEAP_FUNCTIONS (gc_string_heap);
NRML bool string_marked_p (const struct Lisp_String *);
NRML void set_string_marked (struct Lisp_String *);
NRML void set_string_pinned (struct Lisp_String *);
NRML bool string_has_sdata (const struct Lisp_String *);
NRML sdata *sdata_from_string (struct Lisp_String *);
NRML unsigned char * string_data_pointer (struct Lisp_String *, sdata *);
NRML EMACS_INT string_identity_hash_code (struct Lisp_String *);
NRML Lisp_Object live_string_holding (const struct mem_node *, const void *);
NRML bool live_string_p (const struct mem_node *, const void *);
GCFN void scan_string (struct Lisp_String *, gc_phase);
NRML Lisp_Object make_clear_string (EMACS_INT, bool);
NRML Lisp_Object make_clear_multibyte_string (EMACS_INT, EMACS_INT, bool);
NRML struct Lisp_String *allocate_string (void);
NRML struct Lisp_String *point_string_into_tospace (struct Lisp_String *);
NRML void init_strings (void);
NRML void allocate_string_data (struct Lisp_String *, EMACS_INT, EMACS_INT, bool);
NOIL void gc_mark_or_enqueue_string (struct Lisp_String *s);
NRML ptrdiff_t sdata_size (ptrdiff_t);

DECLARE_STANDARD_HEAP_FUNCTIONS (gc_vector_heap);
NRML void unchain_dead_markers (struct buffer *buffer);
GCFN void scan_hash_table (struct Lisp_Hash_Table *const, gc_phase);
NRML bool vectorlike_always_pinned_p (const union vectorlike_header *);
NRML void set_vectorlike_marked (union vectorlike_header *);
NRML void set_vectorlike_pinned (union vectorlike_header *);
NRML ptrdiff_t vectorlike_lisp_size (const union vectorlike_header *);
NRML EMACS_INT vector_identity_hash_code (struct Lisp_Vector *);
NRML bool vectorlike_marked_p (const union vectorlike_header *);
NRML ptrdiff_t vectorlike_payload_nr_words (const union vectorlike_header *);
NRML Lisp_Object live_vector_holding (const struct mem_node *, const void *);
NRML bool live_vector_p (const struct mem_node *, const void *);
NOIL void sweep_large_vectors (void);
NRML void large_vector_meta_free (large_vector_meta *);
NRML large_vector_meta * large_vector_meta_allocate (void);
NRML gc_vector_allocation large_vector_allocate (size_t, bool);
NRML gc_vector_allocation small_vector_allocate (size_t, bool);
NRML struct Lisp_Vector *gc_vector_allocation_vector (gc_vector_allocation);
NRML Lisp_Object gc_vector_allocation_lv (gc_vector_allocation);
NRML bool gc_vector_allocation_is_zero (gc_vector_allocation);
NRML Lisp_Object gc_vector_allocation_commit (gc_vector_allocation *);
NRML gc_vector_allocation allocate_vectorlike (ptrdiff_t, bool);
NRML void vector_cleanup (void *);
GCFN void scan_vectorlike (union vectorlike_header *, gc_phase);
NRML void bignum_cleanup (struct Lisp_Bignum *);
NRML void finalizer_cleanup (struct Lisp_Finalizer *);
NRML void font_cleanup (struct font *);
NRML void marker_cleanup (struct Lisp_Marker *);
NRML void user_ptr_cleanup (struct Lisp_User_Ptr *);
GCFN void scan_marker (struct Lisp_Marker *, gc_phase);
GCFN void scan_overlay (struct Lisp_Overlay *, gc_phase);
GCFN void scan_vector_lisp_fields (union vectorlike_header *, gc_phase);
GCFN void scan_pseudovector_empty (union vectorlike_header *, gc_phase);
GCFN void *point_vectorlike_into_tospace (union vectorlike_header *);
GCFN void scan_buffer (struct buffer *, gc_phase);
NRML void init_vectors (void);
GCFN gc_vector_allocation allocate_record (EMACS_INT);
NRML struct Lisp_Vector * vector_from_large_vector (struct large_vector *);
NRML struct large_vector * large_vector_from_vectorlike (const union vectorlike_header *);
NRML struct large_vector * large_vector_from_meta (const large_vector_meta *);
GCFN gc_vector_allocation allocate_pseudovector (int, int, enum pvec_type, bool);
GCFN gc_vector_allocation larger_vecalloc (const struct Lisp_Vector *, ptrdiff_t, ptrdiff_t);
NRML size_t gc_vector_object_nr_bytes (const void *);
NRML bool mark_or_sweep_weak_table (struct Lisp_Hash_Table *, bool);
GCFN void scan_glyph_matrix (struct glyph_matrix *, gc_phase);
GCFN void scan_char_table (struct Lisp_Vector *, enum pvec_type, gc_phase);
GCFN void scan_face_cache (struct face_cache *const, struct frame *const, const gc_phase);
GCFN void scan_frame (struct frame *const, const gc_phase);
NRML void discard_killed_buffers (Lisp_Object *);
GCFN void scan_window (struct window *, gc_phase);
GCFN void scan_terminal_display_info (struct terminal *, gc_phase);
GCFN void scan_terminal (struct terminal *, gc_phase);
NRML void visit_vectorlike_root (struct gc_root_visitor, struct Lisp_Vector *, enum gc_root_type);
NRML void visit_buffer_root (struct gc_root_visitor, struct buffer *, enum gc_root_type);
NOIL void gc_mark_or_enqueue_vectorlike (union vectorlike_header *);
GCFN bool vector_igscan_hook(void *, gc_phase, gc_igscan *);
NRML void sweep_one_large_vector (large_vector_meta *);

NRML void mem_insert (struct mem_node *, void *, void *, enum mem_type);
NRML void mem_insert_fixup (struct mem_node *);
NRML void mem_rotate_left (struct mem_node *);
NRML void mem_rotate_right (struct mem_node *);
NRML void mem_remove (struct mem_node *);
NRML struct mem_node *mem_find (const void *);
NRML struct mem_node *mem_node_from_struct (void *b, ptrdiff_t offset);
NRML struct mem_node *mem_minimum (struct mem_node *);
NRML struct mem_node *mem_maximum (struct mem_node *);
NRML void mem_start_modification (void);
NRML void mem_end_modification (void);

GCFN ATTRIBUTE_NO_SANITIZE_UNDEFINED void *XPNTR (Lisp_Object);
NRML void tally_consing_maybe_garbage_collect (size_t);

NRML void *lmalloc (size_t, bool) ATTRIBUTE_MALLOC_SIZE ((1));
NRML void *lrealloc (void *, size_t);
NRML void *lisp_malloc (size_t, bool, enum mem_type, ptrdiff_t) ATTRIBUTE_MALLOC_SIZE ((1));
NRML void lisp_free (void *, ptrdiff_t);
NRML bool laligned (void *, size_t);
NRML int valid_pointer_p (void *);

NRML Lisp_Object run_finalizer_handler (Lisp_Object);
NRML void run_finalizer_function (Lisp_Object);
NOIL void run_doomed_finalizers (void);
NRML void enqueue_doomed_finalizers (void);

NRML void inhibit_garbage_collection_undo (intmax_t);

NRML bool mark_strong_references_on_reachable_weak_list (Lisp_Object, bool (*)(Lisp_Object));
NRML void clear_weak_references_on_reachable_weak_list (Lisp_Object *);
NRML bool mark_strong_references_of_reachable_weak_hash_tables (void);
NRML bool undo_list_entry_survives_gc_p (Lisp_Object);
NRML bool mark_strong_references_of_reachable_undo_list_entries (void);
NRML bool font_cache_entry_survives_gc_p (Lisp_Object);
NRML bool mark_strong_references_of_reachable_font_cache (Lisp_Object);
NRML bool mark_strong_references_of_reachable_font_caches (void);
NRML bool mark_strong_references_of_reachable_finalizers (void);
NRML bool mark_strong_references_of_reachable_weak_objects (void);
NRML void clear_weak_hash_tables (void);
NRML void clear_weak_undo_list_entries (void);
NRML void clear_weak_font_caches (void);
NRML void clear_weak_marker_chains (void);
NOIL void gc_phase_clear_weak_references (void);

GCFN gc_gen_y_bit_iter gc_gen_y_bit_iter_init (gc_block *, const gc_heap *);
GCFN bool gc_gen_y_bit_iter_at_end (const gc_gen_y_bit_iter *);
GCFN void gc_gen_y_bit_iter_next (gc_gen_y_bit_iter *);
GCFN ptrdiff_t gc_gen_y_bit_iter_slot_nr (const gc_gen_y_bit_iter *);
GCFN emacs_bitset_word gc_gen_y_bit_iter_get (const gc_gen_y_bit_iter *, const emacs_bitset_word *);
GCFN void gc_gen_y_bit_iter_set (const gc_gen_y_bit_iter *, emacs_bitset_word *, emacs_bitset_word);


GCFN void check_obarray_elem(const Lisp_Object tail, size_t i, size_t obsize);

extern Lisp_Object which_symbols (Lisp_Object, EMACS_INT) EXTERNALLY_VISIBLE;


/* Macros.  */

#define scan_reference_pointer_to_vectorlike(x, phase)                  \
  scan_reference_pointer_to_vectorlike_1 ((x), &(*(x))->header, phase)

/* Allocate zeroed pseudovector with no Lisp_Object slots.  */
#define UNSAFE_ALLOCATE_PLAIN_PSEUDOVECTOR_UNINIT(vr, type, tag) \
  (*(vr) = allocate_pseudovector (                               \
    VECSIZE (type), 0, tag, /*clearit=*/false),                  \
   (type *) gc_vector_allocation_vector(*vr))

/* Allocate zerod initialized pseudovector with a prefix of Lisp
   object slots.  */
#define UNSAFE_ALLOCATE_PSEUDOVECTOR_UNINIT(vr, type, field, tag)   \
  ((*vr) = allocate_pseudovector (                                  \
    VECSIZE (type),                                                 \
    PSEUDOVECSIZE (type, field),                                    \
    tag, /*clearit=*/false),                                        \
   (type *) gc_vector_allocation_vector(*vr))

/* The type of an object near the stack top, whose address can be used
   as a stack scan limit.  */
typedef union
{
  /* Align the stack top properly.  Even if !HAVE___BUILTIN_UNWIND_INIT,
     jmp_buf may not be aligned enough on darwin-ppc64.  */
  max_align_t o;
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
   __builtin_unwind_init ();		\
   *(p) = NEAR_STACK_TOP (&sentry)
#else
# define SET_STACK_TOP_ADDRESS(p)		\
   stacktop_sentry sentry;			\
   __builtin_unwind_init ();			\
   test_setjmp ();				\
   sys_setjmp (sentry.j);			\
   *(p) = NEAR_STACK_TOP (&sentry + (stack_bottom < &sentry.c))
#endif

#define ENQUEUE_AND_RETURN_IF_TOO_DEEP(objexpr)                         \
  do {                                                                  \
    stacktop_sentry sentry;                                             \
    const void *const fp = NEAR_STACK_TOP (&sentry);                    \
    const uintptr_t fp_la = (uintptr_t) fp;                             \
    const uintptr_t sb_la =                                             \
      gc_hot.gc_phase_mark_stack_bottom_la;                             \
    eassume (fp_la < sb_la);  /* Stacks grow down everywhere.  */       \
    const uintptr_t stack_size = sb_la - fp_la;                         \
    if (eunlikely (stack_size > gc_mark_maximum_stack))                 \
      {                                                                 \
        gc_mark_enqueue_1 (objexpr);                                    \
        return;                                                         \
      }                                                                 \
  } while (false)

/* Maximum number of elements in a vector.  */
#define VECTOR_ELTS_MAX                                                 \
  ((ptrdiff_t)                                                          \
   min (((PTRDIFF_MAX - EMACS_SIZE_THROUGH_FIELD (                      \
            struct large_vector, u.header))                             \
         / word_size),                                                  \
	MOST_POSITIVE_FIXNUM))


/* Data.  */

/* Hot data. */
static struct gc_hot_per_cycle_data gc_hot;

/* Make sure signal handler doesn't look at an invalid tree.  */
static bool mem_tree_being_modified;

/* Root of the tree describing allocated Lisp memory.  */
static struct mem_node *mem_root;

/* Closed-open interval describing the known bounds of the heap.
   Provides an early out in mem_find.  */
static uintptr_t heap_start, heap_end;

/* Conservative approximation of the number of lisp objects on the
   heap.  Updated whenever we allocate a new block.  */
static size_t gc_nr_lisp_objects_upper_bound = 0;

/* Auxiliary block cache.  */
static emacs_list_head gc_aux_blocks =
  EMACS_LIST_HEAD_INITIALIZER (gc_aux_blocks);
static size_t gc_aux_nr_blocks;

/* Maximum size, in bytes, to which we allow the marking thread's
   stack to grow while doing recursive marking.  If we recurse past
   this point, switch to enqueuing objects to be marked in the mark
   ring buffer.

   The size is measured from the actual start of the thread stack and
   not the stack at the point that gc_phase_mark() is called --- this
   way, we don't overflow the stack if gc_phase_mark() happens to be
   called deep inside some call tree.  */
static const uintptr_t gc_mark_maximum_stack = 2 * 1024*1024;

/* Mark queue.  Each entry in the queue is a GC aux block being used
  as a mark queue ring buffer.  */
static emacs_list_head gc_mark_queue =
  EMACS_LIST_HEAD_INITIALIZER (gc_mark_queue);

static const gc_heap gc_cons_heap;
static const gc_heap gc_float_heap;
static const gc_heap gc_symbol_heap;
static const gc_heap gc_interval_heap;
static const gc_heap gc_string_heap;
static const gc_heap gc_vector_heap;

static const gc_heap *const gc_heaps[] = {
  &gc_cons_heap,
  &gc_float_heap,
  &gc_symbol_heap,
  &gc_interval_heap,
  &gc_string_heap,
  &gc_vector_heap,
};

struct emacs_globals globals;

/* The number of bytes we can allocate until we call maybe_gc(), which
   does further checks and, as the name suggests, may perform a
   garbage collection.  The name of this variable is a historical
   artifact: it applies to all lisp allocation, not just cons
   cells.  */
static ptrdiff_t consing_until_gc = PTRDIFF_MAX;

#ifdef HAVE_PDUMPER
/* Number of finalizers run: used to loop over GC until we stop
   generating garbage.  */
size_t number_finalizers_run;
#endif

/* Current garbage collection phase.  */
static gc_phase current_gc_phase = GC_PHASE_NOT_IN_PROGRESS;

/* Total number of bytes of GC heap used for live objects at the end
   of the last garbage collection, major or minor.  */
static size_t gc_heap_size_at_end_of_last_gc;

/* Total number of bytes of GC heap used for live objects at the end
   of the last major garbage collection cycle.  */
static size_t gc_heap_size_at_end_of_last_major_gc;

/* Run a major collection if the total heap size exceeds
   this number.  Set in recompute_consing_until_gc().  */
static size_t gc_major_collection_threshold;

/* Run a minor collection if the total heap size exceeds
   this number.  Set in recompute_consing_until_gc().  */
static size_t gc_minor_collection_threshold;

/* Points to memory space allocated as "spare", to be freed if we run
   out of memory.  We keep it in one large block.  */
static char *spare_memory;

/* If positive, garbage collection is inhibited.  Otherwise, zero.
   Starts off as one so that we don't attempt any GC until we finish
   initializing the heap.  */
static intptr_t garbage_collection_inhibited = 1;

/* If nonzero, this is a warning delivered by malloc and not yet
   displayed.  */
const char *pending_malloc_warning;

/* Addresses of staticpro'd variables.  Initialize it to a nonzero
   value if we might unexec; otherwise some compilers put it into
   BSS.  */
Lisp_Object *staticvec[NSTATICS]
#ifdef HAVE_UNEXEC
= {&Vloadup_pure_table}
#endif
  ;

/* Index of next unused slot in staticvec.  */
int staticidx;

/* Head of a list of all finalizers that may or may not be reachable.
   Finalizers move from this list to doomed_finalizers when they
   become unreachable. Not a cons list: finalizers chain
   themselves.  */
Lisp_Object finalizers;

/* Head of a list of all finalizers that we must run because we found
   them to be unreachable.  This list must be global, and not a local
   inside garbage_collect, in case we GC again while running
   finalizers.  Not a cons list: finalizers chain themselves.  */
Lisp_Object doomed_finalizers;

/* Singly-linked list of large vectors.  */
static large_vector_meta *large_vectors;

/* The only vector with 0 slots, allocated from pure space.  */
Lisp_Object zero_vector;

/* List of weak hash tables we found during marking the Lisp heap.
   NULL on entry to garbage_collect and after it returns.  */
static struct Lisp_Hash_Table *weak_hash_tables;

/* Captured at start of GC.  */
static bool inhibit_compacting_font_caches_snapshot;


/* Code.

Don't define functions here as 'static': instead, put a prototype at
the top of this file.  This way, we can see at a glance all the
prototypes and attributes for the GC code.  */

/* Extract the pointer hidden within O.  */
void *
XPNTR (const Lisp_Object a)
{
  return (SYMBOLP (a)
	  ? (char *) lispsym + (XLI (a) - LISP_WORD_TAG (Lisp_Symbol))
	  : (char *) XLP (a) - (XLI (a) & ~VALMASK));
}

/* Note that we've allowed NR_BYTES of lisp heap data and maybe
   perform a garbage collection pass.
   */
void
tally_consing_maybe_garbage_collect (const size_t nr_bytes)
{
  eassume (nr_bytes <= PTRDIFF_MAX);
  consing_until_gc -= nr_bytes;
  if (consing_until_gc <= 0)
    maybe_garbage_collect ();
}


/************************************************************************
				Malloc
 ************************************************************************/

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
	 intern ("emergency"));
  pending_malloc_warning = 0;
}

/* Called if we can't allocate relocatable space for a buffer.  */

void
buffer_memory_full (ptrdiff_t nbytes)
{
  /* If buffers use the relocating allocator, no need to free
     spare_memory, because we may have plenty of malloc space left
     that we could get, and if we don't, the malloc that fails will
     itself cause spare_memory to be freed.  If buffers don't use the
     relocating allocator, treat this like any other failing
     malloc.  */

#ifndef REL_ALLOC
  memory_full (nbytes);
#else
  /* This used to call error, but if we've run out of memory, we could
     get infinite recursion trying to build the string.  */
  xsignal (Qnil, Vmemory_signal_data);
#endif
}

/* LISP_ALIGNMENT is the alignment of Lisp objects.  It must be at
   least GCALIGNMENT so that pointers can be tagged.  It also must be
   at least as strict as the alignment of all the C types used to
   implement Lisp objects; since pseudovectors can contain any C type,
   this is max_align_t.  On recent GNU/Linux x86 and x86-64 this can
   often waste up to 8 bytes, since alignof (max_align_t) is 16 but
   typical vectors need only an alignment of 8.  Although shrinking
   the alignment to 8 would save memory, it cost a 20% hit to Emacs
   CPU performance on Fedora 28 x86-64 when compiled with gcc -m32.  */
enum { LISP_ALIGNMENT = alignof (union { max_align_t x;
					 GCALIGNED_UNION_MEMBER }) };
verify (LISP_ALIGNMENT % GCALIGNMENT == 0);

/* True if malloc (N) is known to return storage suitably aligned for
   Lisp objects whenever N is a multiple of LISP_ALIGNMENT.  In
   practice this is true whenever alignof (max_align_t) is also a
   multiple of LISP_ALIGNMENT.  This works even for buggy platforms
   like MinGW circa 2020, where alignof (max_align_t) is 16 even though
   the malloc alignment is only 8, and where Emacs still works because
   it never does anything that requires an alignment of 16.  */
enum { MALLOC_IS_LISP_ALIGNED = alignof (max_align_t) % LISP_ALIGNMENT == 0 };

#define MALLOC_PROBE(size)			\
  do {						\
    if (profiler_memory_running)		\
      malloc_probe (size);			\
  } while (0)


/* Like malloc but check for no memory.  */

void *
xmalloc (size_t size)
{
  void *val;

  val = lmalloc (size, false);
  if (!val && size)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}

/* Like the above, but zeroes out the memory just allocated.  */

void *
xzalloc (size_t size)
{
  void *val;

  val = lmalloc (size, true);
  if (!val && size)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}

/* Like realloc but check for no memory.  */

void *
xrealloc (void *block, size_t size)
{
  void *val;

  /* We must call malloc explicitly when BLOCK is 0, since some
     reallocs don't do this.  */
  if (! block)
    val = lmalloc (size, false);
  else
    val = lrealloc (block, size);

  if (!val && size)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}


/* Like free.  */

void
xfree (void *block)
{
  free (block);
}

/* Other parts of Emacs pass large int values to allocator functions
   expecting ptrdiff_t.  This is portable in practice, but check it to
   be safe.  */
verify (INT_MAX <= PTRDIFF_MAX);


/* Allocate an array of NITEMS items, each of size ITEM_SIZE.
   Signal an error on memory exhaustion.  */

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
   Signal an error on memory exhaustion.  */

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

   If memory exhaustion occurs, set *NITEMS to zero if PA is null, and
   signal an error (i.e., do not return).

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


/* Like strdup, but uses xmalloc.  */

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

void *
lisp_malloc (const size_t nbytes,
             const bool clearit,
             const enum mem_type type,
             const ptrdiff_t mem_node_offset)
{
  void *val;

  val = lmalloc (nbytes, clearit);

#if ! USE_LSB_TAG
  /* If the memory just allocated cannot be addressed thru a Lisp
     object's pointer, and it needs to be,
     that's equivalent to running out of memory.  */
  if (val)
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

/* #ifndef GC_MALLOC_CHECK */
/*   if (val && type != MEM_TYPE_NON_LISP) */
/*     mem_insert (val, (char *) val + nbytes, type); */
/* #endif */

  if (val)
    {
      mem_start_modification ();
      mem_insert (mem_node_from_struct (val, mem_node_offset),
                  val, (char *) val + nbytes, type);
      mem_end_modification ();
    }
  if (!val && nbytes)
    memory_full (nbytes);
  MALLOC_PROBE (nbytes);
  return val;
}

/* Free BLOCK.  This must be called to free memory allocated with a
   call to lisp_malloc.  */

void
lisp_free (void *const block, const ptrdiff_t mem_node_offset)
{
  eassert (!gc_pdumper_object_p (block));
  mem_start_modification ();
  mem_remove (mem_node_from_struct (block, mem_node_offset));
  mem_end_modification ();
  free (block);
}

/* True if a malloc-returned pointer P is suitably aligned for SIZE,
   where Lisp object alignment may be needed if SIZE is a multiple of
   LISP_ALIGNMENT.  */

bool
laligned (void *p, size_t size)
{
  return (MALLOC_IS_LISP_ALIGNED || (intptr_t) p % LISP_ALIGNMENT == 0
	  || size % LISP_ALIGNMENT != 0);
}

/* Like malloc and realloc except that if SIZE is Lisp-aligned, make
   sure the result is too, if necessary by reallocating (typically
   with larger and larger sizes) until the allocator returns a
   Lisp-aligned pointer.  Code that needs to allocate C heap memory
   for a Lisp object should use one of these functions to obtain a
   pointer P; that way, if T is an enum Lisp_Type value and L ==
   make_lisp_ptr (P, T), then XPNTR (L) == P and XTYPE (L) == T.

   On typical modern platforms these functions' loops do not iterate.
   On now-rare (and perhaps nonexistent) platforms, the loops in
   theory could repeat forever.  If an infinite loop is possible on a
   platform, a build would surely loop and the builder can then send
   us a bug report.  Adding a counter to try to detect any such loop
   would complicate the code (and possibly introduce bugs, in code
   that's never really exercised) for little benefit.  */
void *
lmalloc (size_t size, const bool clearit)
{
#ifdef USE_ALIGNED_ALLOC
  if (! MALLOC_IS_LISP_ALIGNED && size % LISP_ALIGNMENT == 0)
    {
      void *p = aligned_alloc (LISP_ALIGNMENT, size);
      if (clearit && p)
	memclear (p, size);
      return p;
    }
#endif

  while (true)
    {
      void *p = clearit ? calloc (1, size) : malloc (size);
      if (laligned (p, size))
	return p;
      free (p);
      size_t bigger = size + LISP_ALIGNMENT;
      if (size < bigger)
	size = bigger;
    }
}

void *
lrealloc (void *p, size_t size)
{
  while (true)
    {
      p = realloc (p, size);
      if (laligned (p, size))
	return p;
      size_t bigger = size + LISP_ALIGNMENT;
      if (size < bigger)
	size = bigger;
    }
}

/* Allocate a new GC block for heap H.  Return NULL on failure.  If H
   is NULL, allocate an aux block.  */
gc_block *
gc_block_allocate (const gc_heap *const h)
{
  const size_t needed_nr_bytes = h ? h->block_size : gc_aux_block_nr_bytes;
  const bool need_alignment =
    (h && h->aligned_blocks) || (!h && gc_aux_is_aligned);
  const size_t alignment = need_alignment
    ? gc_block_data_nr_bytes : gc_slot_size;
  eassume (alignment >= GCALIGNMENT);
  eassume (alignment % GCALIGNMENT == 0);
  eassume (needed_nr_bytes >= EMACS_SIZE_THROUGH_FIELD (gc_block, meta));

  gc_block_meminfo mi;
  memset (&mi, 0, sizeof (mi));
  mi.maps[0].spec.fd = -1;
  mi.maps[0].spec.size = needed_nr_bytes;
  mi.maps[0].spec.protection = EMACS_MEMORY_ACCESS_READWRITE;

  if (!emacs_mmap_contiguous (&mi.maps[0], gc_block_nr_maps, alignment))
    return NULL;

  gc_block *const b = mi.maps[0].mapping;
  b->meta.meminfo = mi;
  b->meta.owning_heap_for_debug = h;
  b->meta.block_nr = gc_block_nr_invalid;
  emacs_list_link_init (&b->meta.link);

  if (enable_checking)
    {
      memset (b->u.data, 0xDB, sizeof (b->u.data));
      b->meta.magic = gc_block_magic;
    }

  return b;
}

bool
gc_block_is_any_bit_set (const emacs_bitset_word *const words,
                         const gc_heap *const h)
{
  const size_t nr_bitset_words = gc_heap_nr_slot_bitset_words (h);
  for (size_t i = 0; i < nr_bitset_words; ++i)
    if (words[i])
      return true;
  return false;
}

/* Make a cursor for block B (which must be in heap H) at SLOT_NR
   within the block.  */
gc_cursor
gc_block_make_cursor (const gc_block *const b,
                      const ptrdiff_t slot_nr,
                      const gc_heap *const h)
{
  eassume (b->meta.owning_heap_for_debug == h);
  eassume (slot_nr >= 0);
  eassume (slot_nr < gc_heap_nr_slots_per_block (h));
  return (gc_cursor){
    .block = (gc_block *) b,
    .slot_nr = slot_nr,
  };
}

void
gc_block_free (gc_block *const b, const gc_heap *const h)
{
  eassume (b->meta.owning_heap_for_debug == h);
  eassume (b->meta.block_nr == gc_block_nr_invalid);
  gc_block_meminfo mi = b->meta.meminfo;
  for (int i = 0; i < gc_block_nr_maps; ++i)
    emacs_mmap_unmap (&mi.maps[i]);
}

/* Return a pointer to the start bits for block B, which must be in
   heap H.  */
emacs_bitset_word *
gc_block_start_bits (const gc_block *const b, const gc_heap *const h)
{
  eassume (h->start.offset);
  return (emacs_bitset_word *) ((uintptr_t) b + h->start.offset);
}

/* In debug builds, check that the block is a pointer to a valid
   block.  */
gc_block *
gc_block_check (const gc_block *const b)
{
  if (enable_checking)
    eassert (b->meta.magic == gc_block_magic);
  return (gc_block *) b;
}

/* Finalize the tospace of the next block in H.  */
void
gc_heap_flip_next_tospace_block (const gc_heap *const h)
{
  eassume (current_gc_phase == GC_PHASE_SWEEP);
  eassume (h->data->per_cycle.sweep.next_tospace_flip_block_nr <
           h->data->block_array_size);
  const size_t block_nr = h->data->per_cycle.sweep.next_tospace_flip_block_nr++;
  gc_block *const b = h->data->block_array[block_nr];
  eassume (b->meta.block_nr == block_nr);
  gc_block_flip_tospace_to_fromspace (b, h);
}

void
gc_block_sweep_inplace (gc_block *const b, const gc_heap *const h)
{
  eassume (current_gc_phase == GC_PHASE_SWEEP);
  gc_block_write_unprotect (b, h);
  gc_block_scan_intergenerational (b, GC_PHASE_SWEEP, h);

  int32_t block_nr_objects = 0;
  int32_t block_nr_slots = 0;

  emacs_bitset_word *restrict const start_bit_words =
    gc_block_start_bits (b, h);
  emacs_bitset_word *restrict const mark_bit_words =
    gc_block_mark_bits (b, h);
  for (gc_gen_y_bit_iter yit = gc_gen_y_bit_iter_init (b, h);
       !gc_gen_y_bit_iter_at_end (&yit);
       gc_gen_y_bit_iter_next (&yit))
    {
      const ptrdiff_t word_slot_nr = gc_gen_y_bit_iter_slot_nr (&yit);
      emacs_bitset_word start_bits =
        gc_gen_y_bit_iter_get (&yit, start_bit_words);
      emacs_bitset_word mark_bits =
        gc_gen_y_bit_iter_get (&yit, mark_bit_words);
      eassume ((mark_bits & ~start_bits) == 0);
      const emacs_bitset_word orig_mark_bits = mark_bits;
      block_nr_objects += emacs_popcount_bw (mark_bits);
      int pos = 0;
      while (start_bits)
        {
          const int lz = emacs_ctz_bw (start_bits);
          pos += lz;
          start_bits >>= lz;
          mark_bits >>= lz;
          eassume (start_bits & 1);
          const gc_cursor c = gc_block_make_cursor (
            b, word_slot_nr + pos, h);
          eassert (gc_cursor_object_starts_here (c, h));
          const bool is_marked = mark_bits & 1;
          eassert (is_marked == gc_cursor_is_object_marked (c, h));
          void *const obj_ptr = gc_cursor_to_object (c, h);
          if (is_marked)
            scan_object (obj_ptr, h->lisp_type, GC_PHASE_SWEEP);
          else
            {
              if (h->cleanup)
                h->cleanup (obj_ptr);
              /* Clear freed objects so we can more reliably detect
                 use-after-free.  */
              if (enable_checking)
                memset (obj_ptr, 0xAB, gc_cursor_object_nr_bytes (c, h));
            }
          /* XXX: optimize with object size */
          pos += 1;
          start_bits >>= 1;
          mark_bits >>= 1;
          if (!h->homogeneous_object_nr_bytes && is_marked)
            block_nr_slots += gc_cursor_object_nr_slots (c, h);
        }
      gc_gen_y_bit_iter_set (&yit, start_bit_words, orig_mark_bits);
    }
  gc_block_zero_gen_y_slot_bitset (b, mark_bit_words, h);
  h->data->stats.nr_objects += block_nr_objects;
  /* If object sizes are all the same, the number of slots used
     is the same as the number of objects found.  */
  h->data->stats.nr_slots += h->homogeneous_object_nr_bytes
    ? block_nr_objects : block_nr_slots;
}

/* Compute the tospace location of each live object in block B, which
   must belong to heap H.  On entry and exit, *TOSPACE_TIP_INOUT is a
   cursor pointing to the next available tospace location in heap H.
   Updates b->meta.u.heap.per_cycle.smallest_tospace_block_nr by side
   effect.  */
void
gc_block_plan_sweep (gc_block *const b,
                     gc_cursor *const tospace_tip_inout,
                     const gc_heap *const h)
{
  eassume (current_gc_phase == GC_PHASE_PLAN_SWEEP);
  eassume (b->meta.u.heap.per_cycle.tospace == NULL);
  eassume (h->use_moving_gc);
  eassert (gc_heap_is_moving_gc_this_cycle (h));
  eassume (b->meta.block_nr != gc_block_nr_invalid);
  eassert (b->meta.u.heap.per_cycle.smallest_tospace_block_nr == SIZE_MAX);

  gc_block_preprocess_perma_pins (b, h);

  int32_t block_nr_objects = 0;
  int32_t block_nr_slots = 0;
  int32_t block_nr_objects_pinned = 0;
  int32_t block_nr_slots_pinned = 0;
  gc_cursor tospace_tip = *tospace_tip_inout;

  /* If this block doesn't have any objects in the young generation,
     it won't write to tospace, so there is no smallest tospace block
     number.  Use the block number itself so that the early sweep
     logic in gc_block_sweep_compact() works.  */
  b->meta.u.heap.per_cycle.smallest_tospace_block_nr =
    gc_block_has_any_gen_y (b, h)
    ? tospace_tip.block->meta.block_nr
    : b->meta.block_nr;

  /* Ordinarily, we don't care about the initial contents of the
     locator array.  We told the kernel at the end of the last GC that
     we don't need to preserve the contents of the locator array, so
     at this point, the array is undefined.  */
  if (enable_checking)
    {
      gc_locator *restrict const locators = gc_block_locators (b, h);
      for (ptrdiff_t i = 0; i < gc_heap_nr_slots_per_block (h); ++i)
        locators[i] = gc_locator_invalid;
    }

  const emacs_bitset_word *restrict const start_bit_words =
    gc_block_start_bits (b, h);
  const emacs_bitset_word *restrict const mark_bit_words =
    gc_block_mark_bits (b, h);
  const emacs_bitset_word *restrict pinned_bit_words =
    gc_block_pinned_bits (b, h);
  for (gc_gen_y_bit_iter yit = gc_gen_y_bit_iter_init (b, h);
       !gc_gen_y_bit_iter_at_end (&yit);
       gc_gen_y_bit_iter_next (&yit))
    {
      const ptrdiff_t word_slot_nr = gc_gen_y_bit_iter_slot_nr (&yit);
      emacs_bitset_word start_bits =
        gc_gen_y_bit_iter_get (&yit, start_bit_words);
      emacs_bitset_word mark_bits =
        gc_gen_y_bit_iter_get (&yit, mark_bit_words);
      emacs_bitset_word pinned_bits =
        gc_gen_y_bit_iter_get (&yit, pinned_bit_words);
      eassume ((mark_bits & ~start_bits) == 0);
      eassume ((pinned_bits & ~mark_bits) == 0);

      block_nr_objects += emacs_popcount_bw (mark_bits);
      block_nr_objects_pinned += emacs_popcount_bw (pinned_bits);

      int pos = 0;
      while (mark_bits)
        {
          const int lz = emacs_ctz_bw (mark_bits);
          pos += lz;
          start_bits >>= lz;
          mark_bits >>= lz;
          pinned_bits >>= lz;
          eassume (start_bits & 1);
          eassume (mark_bits & 1);
          const gc_cursor c = gc_block_make_cursor (b, word_slot_nr + pos, h);
          eassert (gc_cursor_object_starts_here (c, h));
          const ptrdiff_t nr_slots = gc_cursor_object_nr_slots (c, h);
          const bool pinned = pinned_bits & 1;
          const gc_cursor tospace_obj_c = elikely (!pinned)
            ? gc_heap_allocate_tospace (h, &tospace_tip, nr_slots) : c;
          gc_cursor_check (tospace_obj_c, h);
          const gc_locator tospace_locator =
            gc_cursor_to_locator (tospace_obj_c, h);
          gc_cursor_set_object_tospace_locator (c, tospace_locator, h);
          /* XXX: optimize with object size */
          pos += 1;
          start_bits >>= 1;
          mark_bits >>= 1;
          pinned_bits >>= 1;
          if (!h->homogeneous_object_nr_bytes)
            {
              block_nr_slots += nr_slots;
              if (pinned)
                block_nr_slots_pinned += nr_slots;
            }
        }
    }
  *tospace_tip_inout = tospace_tip;
  h->data->stats.nr_objects += block_nr_objects;
  h->data->stats.nr_objects_pinned += block_nr_objects_pinned;
  if (h->homogeneous_object_nr_bytes)
    {
      /* If every object is the same size, the total slot count is
         equal to the total object count.  */
      h->data->stats.nr_slots += block_nr_objects;
      h->data->stats.nr_slots_pinned += block_nr_objects_pinned;
    }
  else
    {
      h->data->stats.nr_slots += block_nr_slots;
      h->data->stats.nr_slots_pinned += block_nr_slots_pinned;
    }

  /* Don't clear the pinned bits here: we're now using
     them as the start bits for the tospace!  */
}

gc_tospace_placer
gc_tospace_placer_init (const gc_cursor c, const gc_heap *const h)
{
  gc_cursor_check (c, h);
  const size_t word_nr = c.slot_nr / emacs_bitset_bits_per_word;
  const gc_locator tospace_locator = gc_cursor_to_locator (c, h);
  if (!c.block->meta.u.heap.per_cycle.tospace)
    c.block->meta.u.heap.per_cycle.tospace = gc_aux_pop ();
  const size_t slot_nr = gc_locator_slot_nr (tospace_locator, h);
  eassume (slot_nr == c.slot_nr);
  const size_t byte_offset = slot_nr * gc_heap_nr_bytes_per_slot (h);
  uint8_t *const tospace_begin =
    &c.block->meta.u.heap.per_cycle.tospace->u.data[0];
  if (gc_aux_is_aligned)
    eassume (((uintptr_t) tospace_begin % gc_block_data_nr_bytes) == 0);
  return (gc_tospace_placer) {
    .next_tospace_locator = tospace_locator,
    .tospace_start_word = gc_block_pinned_bits (c.block, h)[word_nr],
    .tospace_start_wordp = &gc_block_pinned_bits (c.block, h)[word_nr],
    .tospace = tospace_begin + byte_offset,
    .tospace_end = tospace_begin + gc_block_data_nr_bytes,
  };
}

void *
gc_tospace_placer_place (gc_tospace_placer *const p,
                         const gc_locator tospace_locator,
                         const ptrdiff_t nr_slots,
                         const gc_heap *const h)
{
  eassume (nr_slots > 0);
  eassume (!gc_locator_invalid_p (tospace_locator, h));
  if (eunlikely (tospace_locator.i != p->next_tospace_locator.i))
    {
      gc_tospace_placer_flush (p, h);
      *p = gc_tospace_placer_init (
        gc_locator_to_cursor (tospace_locator, h), h);
    }
  eassume (tospace_locator.i == p->next_tospace_locator.i);
  void *const old_tospace = p->tospace;
  const ptrdiff_t old_slot_nr = gc_locator_slot_nr (tospace_locator, h);
  const size_t old_word_nr = old_slot_nr / emacs_bitset_bits_per_word;
  const size_t old_bitno = old_slot_nr % emacs_bitset_bits_per_word;
  const size_t nr_bytes = nr_slots * gc_heap_nr_bytes_per_slot (h);
  const emacs_bitset_word new_start_bit =
    ((emacs_bitset_word) 1) << old_bitno;
  /* N.B. don't assert that the new start bit is not already set: for
     pinned objects, we're re-setting the existing bit!  */
  p->tospace_start_word |= new_start_bit;
  p->tospace += nr_bytes;
  const ptrdiff_t new_slot_nr = old_slot_nr + nr_slots;
  const size_t new_word_nr = new_slot_nr / emacs_bitset_bits_per_word;
  const bool at_block_end = new_slot_nr >= gc_heap_nr_slots_per_block (h);
  eassume (old_word_nr <= new_word_nr);
  if (eunlikely (at_block_end))
    {
      eassume (p->tospace_start_wordp);
      gc_tospace_placer_flush (p, h);
      p->tospace_start_wordp = NULL;
      p->next_tospace_locator = gc_locator_invalid;
    }
  else if (old_word_nr != new_word_nr)
    {
      eassume (p->tospace_start_wordp);
      gc_tospace_placer_flush (p, h);
      p->tospace_start_wordp += (new_word_nr - old_word_nr);
      p->tospace_start_word = *p->tospace_start_wordp;
      p->next_tospace_locator.i += nr_slots;
    }
  else
    p->next_tospace_locator.i += nr_slots;
  return old_tospace;
}

void
gc_tospace_placer_flush (gc_tospace_placer *const p, const gc_heap *const h)
{
  (void) h;
  if (p->tospace_start_wordp)
    *p->tospace_start_wordp = p->tospace_start_word;
}

/* Perform a compacting sweep of block H, which must be in heap H.
   Copy each object in B (regarding B as fromspace) to its designated
   place in tospace (in whichever block that is).  */
void
gc_block_sweep_compact (gc_block *const b, const gc_heap *const h)
{
  eassume (current_gc_phase == GC_PHASE_SWEEP);
  gc_block_write_unprotect (b, h);
  gc_block_scan_intergenerational (b, GC_PHASE_SWEEP, h);

  const size_t block_nr = b->meta.block_nr;
  const size_t smallest_tospace_block_nr =
    b->meta.u.heap.per_cycle.smallest_tospace_block_nr;

  /* Fromspace blocks never write forwards into tospace.  */
  eassume (smallest_tospace_block_nr <= b->meta.block_nr);

  /* If we know we're done with certain tospace blocks, collapse their
     tospace to fromspace early to reduce total memory demand.
     We can't perform this optimization if we have to preserve the
     tospace across the whole GC.  */
  if (!h->preserve_fromspace_across_sweeps)
    while (h->data->per_cycle.sweep.next_tospace_flip_block_nr <
           smallest_tospace_block_nr)
      gc_heap_flip_next_tospace_block (h);

  /* If we're compacting in-place and we have a variable object size,
     we might be copying objects to themselves, forcing us to use
     memmove.  */
  const bool objects_may_overlap =
    !gc_heap_has_separate_tospace (h) && !h->homogeneous_object_nr_bytes;

  gc_tospace_placer placer = {
    .next_tospace_locator = gc_locator_invalid,
  };
  const emacs_bitset_word *restrict const start_bit_words =
    gc_block_start_bits (b, h);
  emacs_bitset_word *restrict const mark_bit_words =
    gc_block_mark_bits (b, h);
  for (gc_gen_y_bit_iter yit = gc_gen_y_bit_iter_init (b, h);
       !gc_gen_y_bit_iter_at_end (&yit);
       gc_gen_y_bit_iter_next (&yit))
    {
      const ptrdiff_t word_slot_nr = gc_gen_y_bit_iter_slot_nr (&yit);
      emacs_bitset_word start_bits =
        gc_gen_y_bit_iter_get (&yit, start_bit_words);
      emacs_bitset_word mark_bits =
        gc_gen_y_bit_iter_get (&yit, mark_bit_words);
      eassume ((mark_bits & ~start_bits) == 0);
      int pos = 0;
      while (start_bits)
        {
          const int lz = emacs_ctz_bw (start_bits);
          pos += lz;
          start_bits >>= lz;
          mark_bits >>= lz;
          eassume (start_bits & 1);
          const gc_cursor c = gc_block_make_cursor (
            b, word_slot_nr + pos, h);
          eassert (gc_cursor_object_starts_here (c, h));
          const bool is_marked = mark_bits & 1;
          eassert (is_marked == gc_cursor_is_object_marked (c, h));
          void *const fromspace_obj_ptr = gc_cursor_to_object (c, h);
          const size_t object_nr_bytes = gc_cursor_object_nr_bytes (c, h);
          if (is_marked)
            {
              /* TODO: in the common case that the object isn't
                 pinned, the tospace locator will just be the
                 predicted location already in the placer.  We could
                 potentially get rid of most of the calls to
                 gc_cursor_get_object_tospace_locator().  */
              const gc_locator tospace_locator =
                gc_cursor_get_object_tospace_locator (c, h);
              const size_t tospace_block_nr =
                gc_locator_block_nr (tospace_locator, h);
              const ptrdiff_t tospace_slot_nr =
                gc_locator_slot_nr (tospace_locator, h);
              eassert (tospace_block_nr < block_nr ||
                       (tospace_block_nr == block_nr
                        && tospace_slot_nr <= c.slot_nr));
              void *const tospace_obj_ptr =
                gc_tospace_placer_place (
                  &placer, tospace_locator,
                  object_nr_bytes / gc_heap_nr_bytes_per_slot (h),
                  h);
              if (gc_heap_has_separate_tospace (h) ||
                  fromspace_obj_ptr != tospace_obj_ptr)
              (objects_may_overlap ? memmove : memcpy)(
                tospace_obj_ptr,
                fromspace_obj_ptr,
                object_nr_bytes);
              scan_object (tospace_obj_ptr, h->lisp_type, GC_PHASE_SWEEP);
            }
          else if (h->cleanup)
            h->cleanup (fromspace_obj_ptr);
          /* XXX: optimize with object size */
          pos += 1;
          start_bits >>= 1;
          mark_bits >>= 1;
        }
    }
  gc_tospace_placer_flush (&placer, h);
  gc_block_zero_gen_y_slot_bitset (b, mark_bit_words, h);
}

void
gc_block_zero_gen_y_slot_bitset (gc_block *const b,
                                 emacs_bitset_word *const bitset_words,
                                 const gc_heap *const h)
{
  ptrdiff_t gen_y_slot_nr = gc_block_gen_y_slot_nr (b, h);
  if (gen_y_slot_nr % emacs_bitset_bits_per_word)
    {
      const size_t first_word_nr =
        gen_y_slot_nr / emacs_bitset_bits_per_word;
      emacs_bitset_word first_word = bitset_words[first_word_nr];
      const size_t first_word_gen_o_nr_bits =
        gen_y_slot_nr % emacs_bitset_bits_per_word;
      const emacs_bitset_word first_word_gen_o_mask =
        (((emacs_bitset_word) 1) << first_word_gen_o_nr_bits) - 1;
      first_word &= first_word_gen_o_mask;
      bitset_words[first_word_nr] = first_word;
      const size_t first_word_gen_y_nr_bits =
        emacs_bitset_bits_per_word - first_word_gen_o_nr_bits;
      gen_y_slot_nr += first_word_gen_y_nr_bits;
    }
  eassume ((gen_y_slot_nr % emacs_bitset_bits_per_word) == 0);
  const size_t gen_y_word_nr = gen_y_slot_nr / emacs_bitset_bits_per_word;
  const size_t nr_bitset_words = gc_heap_nr_slot_bitset_words (h);
  const size_t gen_y_nr_words = nr_bitset_words - gen_y_word_nr;
  const size_t gen_y_nr_bytes = gen_y_nr_words * sizeof (emacs_bitset_word);
  emacs_zero_memory (&bitset_words[gen_y_word_nr], gen_y_nr_bytes);
}

void
gc_block_copy_gen_y_slot_bitset (gc_block *const b,
                                 emacs_bitset_word *const to,
                                 const emacs_bitset_word *const from,
                                 const gc_heap *h)
{
  ptrdiff_t gen_y_slot_nr = gc_block_gen_y_slot_nr (b, h);
  if (gen_y_slot_nr % emacs_bitset_bits_per_word)
    {
      const size_t first_word_gen_o_nr_bits =
        gen_y_slot_nr % emacs_bitset_bits_per_word;
      const emacs_bitset_word first_word_gen_o_mask =
        (((emacs_bitset_word) 1) << first_word_gen_o_nr_bits) - 1;
      const emacs_bitset_word first_word_gen_y_mask =
        ~first_word_gen_o_mask;
      const size_t first_word_nr =
        gen_y_slot_nr / emacs_bitset_bits_per_word;
      const emacs_bitset_word first_word_to = to[first_word_nr];
      const emacs_bitset_word first_word_from = from[first_word_nr];
      eassume ((first_word_from & first_word_gen_o_mask) == 0);
      const emacs_bitset_word first_word_new =
        (first_word_to & first_word_gen_o_mask) |
        (first_word_from & first_word_gen_y_mask);
      to[first_word_nr] = first_word_new;
      const size_t first_word_gen_y_nr_bits =
        emacs_bitset_bits_per_word - first_word_gen_o_nr_bits;
      gen_y_slot_nr += first_word_gen_y_nr_bits;
    }
  eassume ((gen_y_slot_nr % emacs_bitset_bits_per_word) == 0);
  const size_t gen_y_word_nr = gen_y_slot_nr / emacs_bitset_bits_per_word;
  const size_t nr_bitset_words = gc_heap_nr_slot_bitset_words (h);
  const size_t gen_y_nr_words = nr_bitset_words - gen_y_word_nr;
  const size_t gen_y_nr_bytes = gen_y_nr_words * sizeof (emacs_bitset_word);
  memcpy (&to[gen_y_word_nr],
          &from[gen_y_word_nr],
          gen_y_nr_bytes);
}

ptrdiff_t
gc_block_gen_y_slot_nr (gc_block *const b, const gc_heap *const h)
{
  /* We set up the per-block slot number in the prepare phase.  */
  eassume (current_gc_phase > GC_PHASE_PREPARE);
  const ptrdiff_t gen_y_slot_nr = b->meta.u.heap.per_cycle.gen_y_slot_nr;
  eassume (0 <= gen_y_slot_nr);
  eassume (gen_y_slot_nr <= gc_heap_nr_slots_per_block (h));
  return gen_y_slot_nr;
}

bool
gc_block_has_any_gen_y (gc_block *const b, const gc_heap *const h)
{
  return gc_block_gen_y_slot_nr (b, h) != gc_heap_nr_slots_per_block (h);
}

void
gc_block_flip_tospace_to_fromspace (gc_block *const b, const gc_heap *const h)
{
  eassume (h->use_moving_gc);
  if (!gc_heap_is_moving_gc_this_cycle (h))
    return /* Nothing to do.  */;

  /* We've been using the pinned bits as the start bits for the
     tospace.  Now we're making the tospace the primary space, move
     the pinned bits to the start bits.  */
  emacs_bitset_word *restrict const start_bit_words =
    gc_block_start_bits (b, h);
  emacs_bitset_word *restrict const pinned_bit_words =
    gc_block_pinned_bits (b, h);
  const ptrdiff_t gen_y_slot_nr = gc_block_gen_y_slot_nr (b, h);
  const size_t gen_y_byte_offset =
    gen_y_slot_nr * gc_heap_nr_bytes_per_slot (h);
  eassume (gen_y_byte_offset <= gc_block_data_nr_bytes);

  if (gen_y_byte_offset == gc_block_data_nr_bytes)
    {
      /* This block is 100% old generation: the fromspace is already
         valid: we just have to scan its modified parts below.  */
      eassume (b->meta.u.heap.per_cycle.tospace == NULL);
    }
  else if (!b->meta.u.heap.per_cycle.tospace)
    {
      /* This block didn't have any objects placed into it.  It may
         have some live old-generation objects, however.  */

      /* Any pinned object should have been marked.  */
      eassert (!gc_block_is_any_bit_set (pinned_bit_words, h));
      /* XXX: shouldn't have to zero words here: we can just remove
         the whole block.  */
      gc_block_zero_gen_y_slot_bitset (b, start_bit_words, h);
    }
  else
    {
      /* XXX: use VM tricks to avoid a dumb copy.  */
      eassert (gc_heap_has_separate_tospace (h)) /*XXX FIXME */;
      memcpy (&b->u.data[gen_y_byte_offset],
              &b->meta.u.heap.per_cycle.tospace->u.data[gen_y_byte_offset],
              gc_block_data_nr_bytes - gen_y_byte_offset);
      gc_aux_push (b->meta.u.heap.per_cycle.tospace);
      b->meta.u.heap.per_cycle.tospace = NULL;
      gc_block_copy_gen_y_slot_bitset (
        b, start_bit_words, pinned_bit_words, h);
      gc_block_zero_gen_y_slot_bitset (b, pinned_bit_words, h);
    }

  gc_block_write_protect_gen_o (b, h);
}

/* Update the card table for block B and remove any write traps.
   After this call, neither modifications to the heap data of block B
   nor anything else updates the card table.  */
void
gc_block_write_unprotect (gc_block *const b, const gc_heap *const h)
{
  eassume ((current_gc_phase == GC_PHASE_SWEEP) || ( current_gc_phase == GC_PHASE_CLEANUP));
  const ptrdiff_t gen_y_slot_nr = gc_block_gen_y_slot_nr (b, h);
  const size_t gen_y_byte_offset =
    gen_y_slot_nr * gc_heap_nr_bytes_per_slot (h);
  eassume (gen_y_byte_offset <= gc_block_data_nr_bytes);
  const size_t nr_pure_gen_o_pages =
    gen_y_byte_offset / EMACS_PAGE_SIZE_MIN;
  (void) nr_pure_gen_o_pages;
  const size_t nr_bytes =
    //XXX nr_pure_gen_o_pages * EMACS_PAGE_SIZE_MIN;
    gc_block_data_nr_bytes;
  emacs_set_memory_protection (
    &b->u.data[0], nr_bytes, EMACS_MEMORY_ACCESS_READWRITE);
}

/* Clear the card table for block B and start recording page
   modifications.  After the next call to gc_block_write_unprotect(),
   the card table reflects any page modifications between that call
   and this one.  */
void
gc_block_write_protect_gen_o (gc_block *const b, const gc_heap *const h)
{
  emacs_zero_memory (&b->meta.card_table[0], sizeof (b->meta.card_table));
  size_t gen_y_byte_offset;

  if (!h->data->per_cycle.sweep.saw_new_tospace_tip &&
      b != h->data->last_gc_tospace_tip.block)
    {
      eassert (b->meta.block_nr <
               h->data->last_gc_tospace_tip.block->meta.block_nr);
      gen_y_byte_offset = gc_block_data_nr_bytes;
    }
  else if (!h->data->per_cycle.sweep.saw_new_tospace_tip)
    {
      eassert (b == h->data->last_gc_tospace_tip.block);
      h->data->per_cycle.sweep.saw_new_tospace_tip = true;
      gen_y_byte_offset =
        h->data->last_gc_tospace_tip.slot_nr *
        gc_heap_nr_bytes_per_slot (h);
    }
  else
    {
      eassert (b->meta.block_nr >
               h->data->last_gc_tospace_tip.block->meta.block_nr);
      gen_y_byte_offset = 0;
    }

  eassume (gen_y_byte_offset <= gc_block_data_nr_bytes);
  const size_t page_size = EMACS_PAGE_SIZE_MIN;

  /* If the generation boundary falls in the middle of the page, we
     end the write-protection region one page early, but set the
     card-table modified bit for the final page.  This way, we don't
     take a page fault for modification of objects in the new
     generation, but we still scan any old-generation objects that
     share the page.  */
  const size_t nr_pure_gen_o_pages = gen_y_byte_offset / page_size;
  (void) nr_pure_gen_o_pages;
  const size_t nr_bytes =
    // XXX nr_pure_gen_o_pages * page_size;
    gc_block_data_nr_bytes;
  emacs_set_memory_protection (
    &b->u.data[0], nr_bytes, EMACS_MEMORY_ACCESS_READ);
  if (nr_bytes % page_size)
    emacs_bitset_set_bit (&b->meta.card_table[0],
                          ARRAYELTS (b->meta.card_table),
                          nr_bytes / page_size);
}

void
gc_block_cleanup_after_all_sweeps (gc_block *const b, void *const hptr)
{
  const gc_heap *const h = hptr;
  eassume (current_gc_phase == GC_PHASE_CLEANUP);
  /* We no longer need to be able to translate fromspace addresses to
     tospace addresses during this GC cycle.
     XXX: only bother discarding the young generation
     */
  //gc_block_write_unprotect(b, h);
  if (gc_heap_is_moving_gc_this_cycle (h))
    emacs_discard_memory (gc_block_locators (b, h), h->tospace.nr_bytes);

}

bool
gc_block_mark_card_table (gc_block *const b,
                          void *const fault_addr,
                          const gc_heap *const h)
{
  gc_block_check (b);
  eassert (b->meta.owning_heap_for_debug == h);
  const uintptr_t b_la = (uintptr_t) b;
  const uintptr_t fault_addr_la = (uintptr_t) fault_addr;
  eassert (fault_addr_la >= b_la);
  const uintptr_t offset_nr_bytes = fault_addr_la - b_la;
  eassume (offset_nr_bytes < gc_block_data_nr_bytes);
  const ptrdiff_t page_bit_nr = offset_nr_bytes / EMACS_PAGE_SIZE_MIN;
  verify (EMACS_PAGE_SIZE_MIN+0 == EMACS_PAGE_SIZE_MAX+0 /* FIXME */);
  eassert (!emacs_bitset_bit_set_p (&b->meta.card_table[0],
                                    ARRAYELTS (b->meta.card_table),
                                    page_bit_nr));
  emacs_bitset_set_bit (
    &b->meta.card_table[0],
    ARRAYELTS (b->meta.card_table),
    page_bit_nr);
  const uintptr_t page_start_la =
    b_la + page_bit_nr * EMACS_PAGE_SIZE_MIN;
  emacs_set_memory_protection ((void *) page_start_la,
                               EMACS_PAGE_SIZE_MIN,
                               EMACS_MEMORY_ACCESS_READWRITE);
  return true;
}

/* Return the number of words of start bits we use for each block in
   this heap.  */
size_t
gc_heap_nr_slot_bitset_words (const gc_heap *const h)
{
  eassume (h->start.nr_bytes);
  eassume (h->start.nr_bytes % sizeof (emacs_bitset_word) == 0);
  return h->start.nr_bytes / sizeof (emacs_bitset_word);
}

/* Return a pointer to the pinned bits for block B, which must be in
   heap H.  */
emacs_bitset_word *
gc_block_pinned_bits (const gc_block *const b, const gc_heap *const h)
{
  eassume (h->pinned.offset);
  return (emacs_bitset_word *) ((uintptr_t) b + h->pinned.offset);
}

/* Return a pointer to the permanently-pinned bits for block B, which
   must be in heap H.  */
emacs_bitset_word *
gc_block_perma_pinned_bits (const gc_block *const b, const gc_heap *const h)
{
  eassume (h->perma_pinned.offset);
  return (emacs_bitset_word *) ((uintptr_t) b + h->perma_pinned.offset);
}

/* Return a pointer to the mark bits for block B, which must be in
   heap H.  */
emacs_bitset_word *
gc_block_mark_bits (const gc_block *const b, const gc_heap *const h)
{
  eassume (h->mark.offset);
  return (emacs_bitset_word *) ((uintptr_t) b + h->mark.offset);
}

/* Return the object locators for block B, which must be in heap H.  */
gc_locator *
gc_block_locators (const gc_block *b, const gc_heap *h)
{
  eassume (h->tospace.offset);
  eassert (!h->data->per_cycle.debug.locators_destroyed);
  return (gc_locator *) ((uintptr_t) b + h->tospace.offset);
}

/* If P is a pointer to a live object in block B (which must be in
   heap H) return a pointer to the object's start.  If P does not
   point to a live object, return NULL.  P may be an interior
   pointer.  */
void *
gc_block_maybe_find_live_object_containing (const gc_block *const b,
                                            const void *const p,
                                            const gc_heap *const h)
{
  /* Since P may or may not point to a live object and since P may be
     an interior pointer, we need to do more work here than
     gc_object_to_cursor does.  In particular, we need to scan
     backwards from the slot number we find to start of the actual
     object.  */
  const uintptr_t p_la = (uintptr_t) p;
  const uintptr_t b_la = (uintptr_t) &b->u.data;
  if (INT_ADD_OVERFLOW (b_la, gc_block_data_nr_bytes))
    return NULL;
  if (p_la < b_la || p_la >= b_la + gc_block_data_nr_bytes)
    return NULL;
  const ptrdiff_t p_slot_nr =
    (ptrdiff_t) ((p_la - b_la) / gc_heap_nr_bytes_per_slot (h));
  eassume (0 <= p_slot_nr && p_slot_nr < gc_heap_nr_slots_per_block (h));
  /* If this heap tells us its maximum object size, we can
     limit the backward search.  */
  const ptrdiff_t limit =
    max (p_slot_nr - gc_heap_maximum_object_nr_slots (h), -1);
  const ptrdiff_t found_start_slot_nr = emacs_bitset_scan_backward (
    gc_block_start_bits (b, h),
    gc_heap_nr_slot_bitset_words (h),
    p_slot_nr,
    limit);
  eassume (found_start_slot_nr >= limit);
  eassume (found_start_slot_nr <= p_slot_nr);
  if (found_start_slot_nr <= limit)
    return NULL;
  const gc_cursor c = {
    .block = gc_block_check ((gc_block *) b),
    .slot_nr = found_start_slot_nr,
  };
  const ptrdiff_t obj_nr_slots = gc_cursor_object_nr_slots (c, h);
  const ptrdiff_t end_slot_nr = c.slot_nr + obj_nr_slots;
  if (p_slot_nr >= end_slot_nr)
    return NULL;
  return gc_cursor_to_object (c, h);
}

gc_block *
gc_block_from_link (emacs_list_link *ll)
{
  return gc_block_check (econtainer_of (ll, gc_block, meta.link));
}

gc_block *
gc_block_from_mem_node (const struct mem_node *const mem)
{
  return gc_block_check (econtainer_of (mem, gc_block, meta.mem));
}

gc_block *
gc_aux_pop (void)
{
  eassume (gc_aux_nr_blocks > 0);
  gc_aux_nr_blocks -= 1;
  gc_block *const b = gc_block_from_link (
    emacs_list_pop_first (&gc_aux_blocks));
  if (gc_aux_make_inaccessible)
    {
      emacs_set_memory_protection (
        &b->u.data, sizeof (b->u.data),
        EMACS_MEMORY_ACCESS_READWRITE);
      memset (&b->u.data, 0xFA, sizeof (b->u.data));
    }
  return b;
}

void
gc_aux_push (gc_block *b)
{
  eassume (b->meta.owning_heap_for_debug == NULL);
  gc_aux_nr_blocks += 1;
  emacs_list_insert_first (&gc_aux_blocks, &b->meta.link);
  if (gc_aux_make_inaccessible)
    emacs_set_memory_protection (
      &b->u.data, sizeof (b->u.data),
      EMACS_MEMORY_ACCESS_NONE);
}

bool
gc_aux_add_block (void)
{
  gc_block *b = gc_block_allocate (NULL);
  if (!b)
    return false;
  gc_aux_push (b);
  return true;
}

/* Make sure we have enough aux blocks for the worst case GC.
  Return true on success or false on failure.  The only failure is
  allocation failure.  */
bool
gc_aux_adjust (void)
{
  size_t nr_objects_plus_fudge;
  if (INT_ADD_WRAPV (gc_nr_lisp_objects_upper_bound,
                     gc_block_nr_mark_queue_entries,
                     &nr_objects_plus_fudge))
    return false;
  const size_t nr_aux_blocks_needed_for_mark =
    (nr_objects_plus_fudge - 1) / gc_block_nr_mark_queue_entries;
  size_t nr_aux_blocks_needed_for_sweep_per_heap = 0;
  size_t nr_aux_blocks_needed_during_whole_sweep = 0;
  for (int i = 0; i < ARRAYELTS (gc_heaps); ++i)
    {
      const gc_heap *const h = gc_heaps[i];
      const size_t nr_blocks = h->data->block_array_size;
      if (h->use_moving_gc && h->preserve_fromspace_across_sweeps)
        {
          if (INT_ADD_WRAPV (nr_aux_blocks_needed_during_whole_sweep,
                             nr_blocks,
                             &nr_aux_blocks_needed_during_whole_sweep))
            return false;
        }
      else if (h->use_moving_gc)
        nr_aux_blocks_needed_for_sweep_per_heap =
          max (nr_aux_blocks_needed_for_sweep_per_heap, nr_blocks);
    }
  size_t nr_aux_blocks_needed_for_sweep;
  if (INT_ADD_WRAPV (nr_aux_blocks_needed_for_sweep_per_heap,
                     nr_aux_blocks_needed_during_whole_sweep,
                     &nr_aux_blocks_needed_for_sweep))
    return false;
  const size_t nr_aux_blocks_needed =
    max (nr_aux_blocks_needed_for_mark,
         nr_aux_blocks_needed_for_sweep);
  while (gc_aux_nr_blocks < nr_aux_blocks_needed)
    if (!gc_aux_add_block ())
      return false;
  return true;
}

/* Try to increase the limit on allocated Lisp objects by
   DELTA. Return true on sucess; on failure, return false without
   raising.  */
bool
gc_object_limit_try_increase (const size_t delta)
{
  if (INT_ADD_OVERFLOW (gc_nr_lisp_objects_upper_bound, delta))
    return false;
  gc_nr_lisp_objects_upper_bound += delta;
  if (!gc_aux_adjust ())
    {
      gc_nr_lisp_objects_upper_bound -= delta;
      return false;
    }
  return true;
}

void
gc_global_object_limit_decrease (const size_t delta)
{
  eassume (gc_nr_lisp_objects_upper_bound >= delta);
  gc_nr_lisp_objects_upper_bound -= delta;
}

/* Call ENUMERATOR on each live object in heap H.  DATA is an opaque
   pointer given to ENUMERATOR.  ENUMERATOR should return true to
   continue scanning or false to terminate the scan.  */
void
gc_heap_enumerate (const gc_heap *const h,
                   const gc_heap_enumerator enumerator,
                   void *const data)
{
  gc_cursor c = gc_heap_make_start_cursor (h);
  while (gc_cursor_advance_to_object_start (&c, h))
    {
      if (!enumerator (gc_cursor_to_object (c, h), data))
        break;
      const ptrdiff_t sz = gc_cursor_object_nr_slots (c, h);
      if (!gc_cursor_advance_nr_slots (&c, sz, h))
        break;
    }
}

void
gc_heap_check_field (const gc_heap *const h,
                     const gc_field_specifier *const spec,
                     const size_t needed_alignment,
                     const size_t nr_needed_bytes)
{
  eassume (spec->offset >= 0);
  eassume (EMACS_SIZE_THROUGH_FIELD (gc_block, meta)
           <= (size_t) spec->offset);
  eassume (spec->offset + spec->nr_bytes <= h->block_size);
  eassume (spec->offset % needed_alignment == 0);
  eassume (spec->nr_bytes % needed_alignment == 0);
  eassume (spec->nr_bytes >= nr_needed_bytes);
}

void
gc_heap_check_bitset (const gc_heap *const h,
                      const gc_field_specifier *const spec)
{
  const size_t nr_slots = gc_heap_nr_slots_per_block (h);
  const size_t nr_bitset_words =
    (nr_slots + emacs_bitset_bits_per_word - 1) / emacs_bitset_bits_per_word;
  const size_t nr_bytes = nr_bitset_words * sizeof (emacs_bitset_word);
  gc_heap_check_field (h, spec, alignof (emacs_bitset_word), nr_bytes);
}

bool
gc_heap_is_moving_gc_this_cycle (const gc_heap *const h)
{
  return h->use_moving_gc;
}

void
gc_block_prepare (gc_block *const b, void *const hptr)
{
  const gc_heap *const h = hptr;
  const gc_cursor last_gc_tospace_tip = h->data->last_gc_tospace_tip;

  /* The smallest tospace block number is set by
     gc_block_plan_sweep(); invalidate it here so we can
     make sure that function ran.  */
  memset (&b->meta.u.heap.per_cycle,
          0, sizeof (b->meta.u.heap.per_cycle));
  if (enable_checking)
    b->meta.u.heap.per_cycle.smallest_tospace_block_nr = SIZE_MAX;

  /* Set each block's gen_y_slot_nr field to the first
     bitset word number in that block that's part of the new
     generation.  When we're doing a major GC, every slot is part
     of the new generation.  A block without any new-generation
     objects gets a gen_y_slot_nr field greater than any
     valid slot-bitset word number in that block.  */
  if (!h->data->per_cycle.prepare.saw_last_gc_tospace_tip &&
      b != last_gc_tospace_tip.block)
    {
      eassume (b->meta.block_nr <
               last_gc_tospace_tip.block->meta.block_nr);
      b->meta.u.heap.per_cycle.gen_y_slot_nr =
        gc_heap_nr_slots_per_block (h);
    }
  else if (!h->data->per_cycle.prepare.saw_last_gc_tospace_tip)
    {
      h->data->per_cycle.prepare.saw_last_gc_tospace_tip = true;
      const ptrdiff_t slot_nr = last_gc_tospace_tip.slot_nr;
      eassume (0 <= slot_nr && slot_nr < gc_heap_nr_slots_per_block (h));
      b->meta.u.heap.per_cycle.gen_y_slot_nr = slot_nr;
    }
  else
    {
      eassume (b->meta.block_nr >
               last_gc_tospace_tip.block->meta.block_nr);
      eassume (b->meta.u.heap.per_cycle.gen_y_slot_nr == 0);
    }
}

/* Prepare heap H for a garbage collection cycle.  If MAJOR, we're
   preparing for a major garbage collection.  */
void
gc_heap_prepare (const gc_heap *const h, const bool major)
{
  eassume (current_gc_phase == GC_PHASE_PREPARE);
  memset (&h->data->per_cycle, 0, sizeof (h->data->per_cycle));
  /* A major GC is just a GC in which the whole heap is the young
     generation.  */
  if (major)
    {
      h->data->last_gc_tospace_tip = gc_heap_make_start_cursor (h);
      memset (&h->data->stats, 0, sizeof (h->data->stats));
    }
  gc_heap_for_each_block (h, gc_block_prepare, (void *) h);
}

bool
gc_heap_has_separate_tospace (const gc_heap *const h)
{
  return true;  // XXX
}

void
gc_heap_for_each_block (const gc_heap *const h,
                        void (*const block_function)(gc_block *, void *),
                        void *const data)
{
  for (emacs_list_link *link = emacs_list_first (&h->data->blocks);
       link != &h->data->blocks.link;
       link = link->next)
    block_function (gc_block_from_link (link), data);
}

/* Initialize heap H.  Each heap must initialized during
   startup exactly once.  Abort on failure.  */
void
gc_heap_init (const gc_heap *const h)
{
  eassume (h->data);
  eassume (h->block_size >= EMACS_SIZE_THROUGH_FIELD (gc_block, meta));
  eassume (h->homogeneous_object_nr_bytes == 0 ||
           h->homogeneous_object_nr_bytes % GCALIGNMENT == 0);
  eassume (h->homogeneous_object_nr_bytes ||
           h->object_nr_bytes);
  gc_heap_check_bitset (h, &h->start);
  gc_heap_check_bitset (h, &h->mark);
  gc_heap_check_bitset (h, &h->pinned);
  gc_heap_check_bitset (h, &h->perma_pinned);
  gc_heap_check_field (h, &h->tospace, alignof (gc_locator),
                       sizeof (gc_locator) * gc_heap_nr_slots_per_block (h));

  eassume (h->heap_symbol_index);
  eassume (h->block_mark_intergenerational);
  eassume (h->block_plan_sweep);
  eassume (h->block_sweep_inplace);
  eassume (h->block_sweep_compact);

  emacs_list_head_init (&h->data->blocks);
  /* Every heap begins life with at least one block.  */
  gc_heap_add_block (h);
  h->data->allocation_tip = gc_heap_make_start_cursor (h);
  h->data->last_gc_tospace_tip = gc_heap_make_start_cursor (h);
}

/* Make a cursor that points to the first block in heap H.  */
gc_cursor
gc_heap_make_start_cursor (const gc_heap *const h)
{
  return gc_block_make_cursor (
    gc_block_from_link (emacs_list_first (&h->data->blocks)),
    0, h);
}

/* Return whether C (which must point into heap H) is currently
   pointing at an object start.  */
bool
gc_cursor_object_starts_here (const gc_cursor c,
                              const gc_heap *const h)
{
  gc_cursor_check (c, h);
  return emacs_bitset_bit_set_p (
    gc_block_start_bits (c.block, h),
    gc_heap_nr_slot_bitset_words (h),
    c.slot_nr);
}

/* Mark the heap as having a live object at the current position.  */
void
gc_cursor_make_object_start_here (const gc_cursor c,
                                  const gc_heap *const h)
{
  gc_cursor_check (c, h);
  eassert (!gc_cursor_object_starts_here (c, h));
  emacs_bitset_set_bit (
    gc_block_start_bits (c.block, h),
    gc_heap_nr_slot_bitset_words (h),
    c.slot_nr);
}

/* Return whether cursor C, which must be in heap H, points into the
   generation being collected in this collection cycle.  */
bool
gc_cursor_is_gen_y (const gc_cursor c, const gc_heap *const h)
{
  gc_cursor_check (c, h);
  return c.slot_nr >= gc_block_gen_y_slot_nr (c.block, h);
}

/* Return whether the cursor C, which must be in heap H,
   points to a marked object.  */
bool
gc_cursor_is_object_marked (const gc_cursor c, const gc_heap *const h)
{
  if (!gc_cursor_is_gen_y (c, h))
    return true;
  return emacs_bitset_bit_set_p (
    gc_block_mark_bits (c.block, h),
    gc_heap_nr_slot_bitset_words (h),
    c.slot_nr);
}

/* Set the mark flag for the object at cursor C, which must be in heap
   H.  C must point to a live object.  */
void
gc_cursor_set_object_marked (const gc_cursor c, const gc_heap *const h)
{
  gc_cursor_check (c, h);
  eassert (gc_cursor_object_starts_here (c, h));
  eassert (gc_cursor_is_gen_y (c, h));
  emacs_bitset_word *restrict const words =
    gc_block_mark_bits (c.block, h);
  emacs_bitset_set_bit (words, gc_heap_nr_slot_bitset_words (h), c.slot_nr);
}

/* Set the pinned flag for the object at cursor C, which must be in
   heap H.  C must point to a live object that is also marked.
   Objects can be pinned only in the GC mark phase.  */
void
gc_cursor_set_object_pinned (const gc_cursor c, const gc_heap *const h)
{
  gc_cursor_check (c, h);
  eassert (gc_cursor_object_starts_here (c, h));
  eassert (gc_cursor_is_object_marked (c, h));
  eassert (current_gc_phase == GC_PHASE_MARK);
  if (gc_heap_is_moving_gc_this_cycle (h) && gc_cursor_is_gen_y (c, h))
    {
      emacs_bitset_word *restrict const words =
        gc_block_pinned_bits (c.block, h);
      emacs_bitset_set_bit (
        words, gc_heap_nr_slot_bitset_words (h), c.slot_nr);
    }
}

/* Set the perma-pinned flag for the object at cursor C, which must be
   in heap H.  C must point to a live object.  GC must not be in
   progress.  */
void
gc_cursor_perma_pin_object (const gc_cursor c, const gc_heap *const h)
{
  gc_cursor_check (c, h);
  eassert (gc_cursor_object_starts_here (c, h));
  eassert (current_gc_phase == GC_PHASE_NOT_IN_PROGRESS);
  emacs_bitset_word *restrict const words =
    gc_block_perma_pinned_bits (c.block, h);
  emacs_bitset_set_bit (words, gc_heap_nr_slot_bitset_words (h), c.slot_nr);
}

/* Set the tospace location of the fromspace object at fromspace
   cursor C to the tospace location of locator T.  */
void
gc_cursor_set_object_tospace_locator (const gc_cursor c,
                                      const gc_locator t,
                                      const gc_heap *const h)
{
  gc_cursor_check (c, h);
  eassert (gc_cursor_object_starts_here (c, h));
  gc_locator *restrict const locators = gc_block_locators (c.block, h);
  eassert (gc_locator_invalid_p (locators[c.slot_nr], h));
  locators[c.slot_nr] = t;
}

gc_locator
gc_cursor_get_object_tospace_locator (const gc_cursor c,
                                      const gc_heap *const h)
{
  gc_locator tospace_loc = gc_locator_invalid;
  eassume (current_gc_phase == GC_PHASE_SWEEP);
  gc_cursor_check (c, h);
  gc_locator *restrict const locators = gc_block_locators (c.block, h);
  tospace_loc = locators[c.slot_nr];

  eassert(! gc_locator_invalid_p(tospace_loc, h));
  return tospace_loc;
}
/* Return the number of slots that the object at cursor C (which must
   be in heap H) occupies.  */
ptrdiff_t
gc_cursor_object_nr_slots (const gc_cursor c, const gc_heap *const h)
{
  gc_cursor_check (c, h);
  eassert (gc_cursor_object_starts_here (c, h));
  if (h->homogeneous_object_nr_bytes)
    return 1;
  const size_t object_nr_bytes =
    h->object_nr_bytes (gc_cursor_to_object (c, h));
  eassume (object_nr_bytes % gc_slot_size == 0);
  return object_nr_bytes / gc_slot_size;
}

/* Return the number of bytes that the object at cursor C (which must
   be in heap H) occupies.  */
size_t
gc_cursor_object_nr_bytes (const gc_cursor c, const gc_heap *const h)
{
  const ptrdiff_t nr_slots = gc_cursor_object_nr_slots (c, h);
  eassume (nr_slots > 0);
  return nr_slots * gc_heap_nr_bytes_per_slot (h);
}

/* Advance the cursor *C_INOUT (which must point into heap H) to the
   next object start in the cursor's current block.  Return true if we
   found such an object within MAX_DISTANCE of the current position or
   false if we did not.  In the latter case, do not modify
   *C_INOUT.  */
bool
gc_cursor_advance_to_object_start_in_block (gc_cursor *const c_inout,
                                            const ptrdiff_t max_distance,
                                            const gc_heap *const h)
{
  eassume (max_distance >= 0);
  eassume (max_distance <= gc_heap_nr_slots_per_block (h));
  gc_cursor c = *c_inout;
  gc_cursor_check (c, h);
  const ptrdiff_t limit = min (c.slot_nr + max_distance,
                               gc_heap_nr_slots_per_block (h));
  const ptrdiff_t found = emacs_bitset_scan_forward (
    gc_block_start_bits (c.block, h),
    gc_heap_nr_slot_bitset_words (h),
    c.slot_nr, limit);
  if (found == limit)
    return false;
  c.slot_nr = found;
  *c_inout = c;
  return true;
}

/* Advance the cursor *C_INOUT (which must point into heap H) to the
   next object pin in the cursor's current block.  Return true if we
   found such an object within MAX_DISTANCE of the current position or
   false if we did not.  In the latter case, do not modify
   *C_INOUT.  */
bool
gc_cursor_advance_to_object_pin_in_block (gc_cursor *const c_inout,
                                          const ptrdiff_t max_distance,
                                          const gc_heap *const h)
{
  eassume (h->use_moving_gc);
  eassume (max_distance >= 0);
  eassume (max_distance <= gc_heap_nr_slots_per_block (h));
  gc_cursor c = *c_inout;
  gc_cursor_check (c, h);
  const ptrdiff_t limit = min (c.slot_nr + max_distance,
                               gc_heap_nr_slots_per_block (h));
  const ptrdiff_t found = emacs_bitset_scan_forward (
    gc_block_pinned_bits (c.block, h),
    gc_heap_nr_slot_bitset_words (h),
    c.slot_nr, limit);
  if (found == limit)
    return false;
  c.slot_nr = found;
  *c_inout = c;
  return true;
}

/* Advance the cursor *C_INOUT (which must point into heap H) to the
   next object in the heap and return true, or return false (leaving
   *C_INOUT untouched) if no next live object exists.  */
bool
gc_cursor_advance_to_object_start (gc_cursor *const c_inout,
                                   const gc_heap *const h)
{
  const ptrdiff_t dist = gc_heap_nr_slots_per_block (h);
  gc_cursor c = *c_inout;
  do
    if (gc_cursor_advance_to_object_start_in_block (&c, dist, h))
      {
        *c_inout = c;
        return true;
      }
  while (gc_cursor_advance_next_block (&c, h));
  return false;
}

/* Convert a cursor to a plain pointer to the object at its position.  */
void *
gc_cursor_to_object (const gc_cursor c, const gc_heap *const h)
{
  gc_cursor_check (c, h);
  return &c.block->u.data[c.slot_nr * gc_heap_nr_bytes_per_slot (h)];
}

/* Make a locator pointing to the same place as cursor C, which
   must be in heap H.  */
gc_locator
gc_cursor_to_locator (const gc_cursor c, const gc_heap *const h)
{
  gc_cursor_check (c, h);
  const size_t block_nr = c.block->meta.block_nr;
  eassume (block_nr != gc_block_nr_invalid);
  eassume (block_nr <= gc_maximum_block_nr);
  const ptrdiff_t slot_nr = c.slot_nr;
  eassume (0 <= slot_nr && slot_nr < gc_locator_max_nr_slots);
  const gc_locator locator = {
    .i = (block_nr << gc_locator_nr_bits_slot_nr) + slot_nr,
  };
  eassume (gc_locator_block_nr (locator, h) == block_nr);
  eassume (gc_locator_slot_nr (locator, h) == slot_nr);
  return locator;
}

/* Return the number of slots remaining in C's block.  */
ptrdiff_t
gc_cursor_nr_slots_left_in_block (
  const gc_cursor c, const gc_heap *const h)
{
  gc_cursor_check (c, h);
  return gc_heap_nr_slots_per_block (h) - c.slot_nr;
}

size_t
gc_heap_nr_bytes_per_slot (const gc_heap *const h)
{
  return h->homogeneous_object_nr_bytes
    ? h->homogeneous_object_nr_bytes
    : gc_slot_size;
}

ptrdiff_t
gc_heap_nr_slots_per_block (const gc_heap *const h)
{
  const size_t bytes_per_slot = gc_heap_nr_bytes_per_slot (h);
  eassume (bytes_per_slot % GCALIGNMENT == 0);
  eassume (bytes_per_slot >= GCALIGNMENT);
  return (ptrdiff_t) (gc_block_data_nr_bytes / bytes_per_slot);
}

ptrdiff_t
gc_heap_object_size_to_nr_slots (const gc_heap *const h, const size_t nr_bytes)
{
  if (h->homogeneous_object_nr_bytes)
    {
      eassume (nr_bytes == h->homogeneous_object_nr_bytes);
      return 1;
    }
  eassume (nr_bytes % gc_slot_size == 0);
  return (ptrdiff_t) (nr_bytes / gc_slot_size);
}

/* Advance cursor *C_INOUT (which must point into heap H) to the start
   of the next block if such a block exists or return NULL if *C_INOUT
   is at the last block.  */
bool
gc_cursor_advance_next_block (
  gc_cursor *const c_inout, const gc_heap *const h)
{
  gc_cursor c = *c_inout;
  gc_cursor_check (c, h);
  if (emacs_list_last_p (&h->data->blocks, &c.block->meta.link))
    return false;
  c.block = gc_block_from_link (c.block->meta.link.next);
  c.slot_nr = 0;
  *c_inout = c;
  return true;
}

/* Advance cursor *C_INOUT (which must point into heap H) to the start
   of the next block.  If C points to the last block in its heap,
   allocate a new block and make C point to its start.  If allocating
   the new block fails, raise memory_full.  */
void
gc_cursor_advance_next_block_allocate_if_needed (
  gc_cursor *const c_inout, const gc_heap *const h)
{
  if (!gc_cursor_advance_next_block (c_inout, h))
    {
      gc_heap_add_block (h);
      const bool success = gc_cursor_advance_next_block (c_inout, h);
      eassume (success);
    }
}

/* Advance cursor *C_INOUT by the given number of slots.  If this
   advancement would make C run off the end of the heap, allocate new
   blocks so it doesn't.  If allocating a needed block fails, raise
   memory_full.  */
void
gc_cursor_advance_nr_slots_allocate_if_needed (
  gc_cursor *const c_inout,
  ptrdiff_t nr_slots,
  const gc_heap *const h)
{
  eassume (nr_slots >= 0);
  gc_cursor c = *c_inout;
  gc_cursor_check (c, h);
  while (nr_slots >= gc_cursor_nr_slots_left_in_block (c, h))
    {
      nr_slots -= gc_cursor_nr_slots_left_in_block (c, h);
      gc_cursor_advance_next_block_allocate_if_needed (&c, h);
    }
  c.slot_nr += nr_slots;
  *c_inout = c;
}

/* Try to advance cursor *C_INOUT (which must be in heap H) by the
   given number of slots.  If the resulting position is within the
   heap, return true.  Otherwise, leave the output cursor unchanged
   and return false.  */
bool
gc_cursor_advance_nr_slots (gc_cursor *const c_inout,
                            ptrdiff_t nr_slots,
                            const gc_heap *const h)
{
  gc_cursor c = *c_inout;
  gc_cursor_check (c, h);
  while (nr_slots >= gc_cursor_nr_slots_left_in_block (c, h))
    {
      nr_slots -= gc_cursor_nr_slots_left_in_block (c, h);
      if (!gc_cursor_advance_next_block (&c, h))
        return false;
    }
  c.slot_nr += nr_slots;
  *c_inout = c;
  return true;
}

void
gc_cursor_check (const gc_cursor c, const gc_heap *const h)
{
  eassume (c.block->meta.owning_heap_for_debug == h);
  eassume (c.slot_nr >= 0);
  eassume (c.slot_nr < gc_heap_nr_slots_per_block (h));
}

/* Return the maximum number of objects we can store in each block of
   heap H.  */
size_t
gc_heap_maximum_objects_per_block (const gc_heap *const h)
{
  // TODO: we can be less conservative in the non-homogeneous case if
  // we configure a minimum object size.
  return h->homogeneous_object_nr_bytes
    ? gc_block_data_nr_bytes / h->homogeneous_object_nr_bytes
    : gc_block_data_nr_bytes / gc_slot_size;
}

/* Return the maximum size of an object in this heap, in slots.  */
ptrdiff_t
gc_heap_maximum_object_nr_slots (const gc_heap *const h)
{
  return h->homogeneous_object_nr_bytes
    ? 1
    : gc_heap_nr_slots_per_block (h);
}

void
gc_heap_grow_block_array (const gc_heap *const h)
{
  size_t new_capacity;
  if (INT_MULTIPLY_WRAPV (h->data->block_array_capacity, 2, &new_capacity))
    memory_full (SIZE_MAX);
  new_capacity = max (4, new_capacity);
  size_t nr_bytes;
  if (INT_MULTIPLY_WRAPV (new_capacity,
                          sizeof (h->data->block_array[0]),
                          &nr_bytes))
    memory_full (SIZE_MAX);
  h->data->block_array = lrealloc (h->data->block_array, nr_bytes);
  h->data->block_array_capacity = new_capacity;
}

/* Add a newly allocated block to H's list of blocks.
   Raise memory_full if we can't allocate that block.  */
void
gc_heap_add_block (const gc_heap *const h)
{
  eassume (h->data->block_array_size <= gc_maximum_block_nr);
  if (h->data->block_array_size >= gc_maximum_block_nr)
    memory_full (h->block_size);
  if (h->data->block_array_size == h->data->block_array_capacity)
    gc_heap_grow_block_array (h);
  eassume (h->data->block_array_size < h->data->block_array_capacity);

  gc_block *const b = gc_block_allocate (h);
  if (!b)
    memory_full (h->block_size);
  /* Increment block_array_size here so that we take the new block
     into account when computing the number of blocks we need during
     sweep.  */
  const size_t block_nr = h->data->block_array_size++;
  if (!gc_object_limit_try_increase (gc_heap_maximum_objects_per_block (h)))
    {
      h->data->block_array_size -= 1;
      gc_block_free (b, h);
      memory_full (gc_aux_block_nr_bytes);
    }
  emacs_list_insert_last (&h->data->blocks, &b->meta.link);
  mem_start_modification ();
  mem_insert (&b->meta.mem,
              b->u.data,
              b->u.data + gc_block_data_nr_bytes,
              h->mem_type);
  mem_end_modification ();
  b->meta.block_nr = block_nr;
  h->data->block_array[b->meta.block_nr] = b;
}

gc_gen_y_bit_iter
gc_gen_y_bit_iter_init (gc_block *const b, const gc_heap *const h)
{
  eassume (b->meta.owning_heap_for_debug == h);
  const ptrdiff_t gen_y_slot_nr = gc_block_gen_y_slot_nr (b, h);
  const size_t first_word_nr =
    gen_y_slot_nr / emacs_bitset_bits_per_word;
  const size_t first_word_gen_o_nr_bits =
    gen_y_slot_nr % emacs_bitset_bits_per_word;
  const emacs_bitset_word first_word_gen_o_mask =
    (((emacs_bitset_word) 1) << first_word_gen_o_nr_bits) - 1;
  return (gc_gen_y_bit_iter){
    .first_word_nr = first_word_nr,
    .word_nr_limit = gc_heap_nr_slot_bitset_words (h),
    .first_word_gen_y_mask = ~first_word_gen_o_mask,
    .word_nr = first_word_nr,
  };
}

bool
gc_gen_y_bit_iter_at_end (const gc_gen_y_bit_iter *const yit)
{
  eassume (yit->word_nr <= yit->word_nr_limit);
  return yit->word_nr == yit->word_nr_limit;
}

void
gc_gen_y_bit_iter_next (gc_gen_y_bit_iter *const yit)
{
  eassume (!gc_gen_y_bit_iter_at_end (yit));
  yit->word_nr += 1;
}

ptrdiff_t
gc_gen_y_bit_iter_slot_nr (const gc_gen_y_bit_iter *const yit)
{
  return yit->word_nr * emacs_bitset_bits_per_word;
}

emacs_bitset_word
gc_gen_y_bit_iter_get (const gc_gen_y_bit_iter *const yit,
                       const emacs_bitset_word *const words)
{
  eassume (!gc_gen_y_bit_iter_at_end (yit));
  emacs_bitset_word word = words[yit->word_nr];
  if (yit->word_nr == yit->first_word_nr)
    word &= yit->first_word_gen_y_mask;
  return word;
}

void
gc_gen_y_bit_iter_set (const gc_gen_y_bit_iter *const yit,
                       emacs_bitset_word *restrict const words,
                       emacs_bitset_word new_word)
{
  eassume (!gc_gen_y_bit_iter_at_end (yit));
  if (yit->word_nr == yit->first_word_nr)
    {
      const emacs_bitset_word first_word_gen_o_mask =
        ~yit->first_word_gen_y_mask;
      eassume ((new_word & first_word_gen_o_mask) == 0);
      new_word |= (words[yit->word_nr] & first_word_gen_o_mask);
    }
  words[yit->word_nr] = new_word;
}

/* Copy perma-pinned bits to the per-cycle pinned bits.  We set the
   perma-pinned bit for an object when we compute its identity hash
   code so that we can use the object's memory location as its hash.
   This function also clears by side effect perma-pinned bits
   corresponding to objects that don't survive the current GC
   cycle.  */
void
gc_block_preprocess_perma_pins (gc_block *const b, const gc_heap *const h)
{
  eassume (current_gc_phase == GC_PHASE_PLAN_SWEEP);
  if (gc_heap_is_moving_gc_this_cycle (h))
    return;
  const emacs_bitset_word *restrict const start_bit_words =
    gc_block_start_bits (b, h);
  const emacs_bitset_word *restrict const mark_bit_words =
    gc_block_mark_bits (b, h);
  emacs_bitset_word *restrict const pinned_bit_words =
    gc_block_pinned_bits (b, h);
  emacs_bitset_word *restrict const perma_pinned_bit_words =
    gc_block_perma_pinned_bits (b, h);
  for (gc_gen_y_bit_iter yit = gc_gen_y_bit_iter_init (b, h);
       !gc_gen_y_bit_iter_at_end (&yit);
       gc_gen_y_bit_iter_next (&yit))
    {
      const emacs_bitset_word perma_pinned_bits =
        gc_gen_y_bit_iter_get (&yit, perma_pinned_bit_words);
      /* We expect the perma-pin bitset to be very sparse, so fetch
         the other bitset words only when we actually find a
         perma-pinned object.  */
      if (!perma_pinned_bits)
        continue;
      /* A perma-pinned object must be live in the heap (i.e., its
         start bit must be set) but it doesn't necessarily have to be
         marked.  By contrast, all normally-pinned objects must also
         be marked, so when pinning, filter the pin bits to the ones
         corresponding to marked objects and throw out perma-pins for
         objects we know to be dead.  */
      const emacs_bitset_word start_bits =
        gc_gen_y_bit_iter_get (&yit, start_bit_words);
      const emacs_bitset_word mark_bits =
        gc_gen_y_bit_iter_get (&yit, mark_bit_words);
      emacs_bitset_word pinned_bits =
        gc_gen_y_bit_iter_get (&yit, pinned_bit_words);
      eassume ((perma_pinned_bits & ~start_bits) == 0);
      const emacs_bitset_word new_perma_pinned_bits =
        perma_pinned_bits & mark_bits;
      pinned_bits |= new_perma_pinned_bits;
      gc_gen_y_bit_iter_set (&yit, pinned_bit_words, pinned_bits);
      if (new_perma_pinned_bits != perma_pinned_bits)
        gc_gen_y_bit_iter_set (
          &yit, perma_pinned_bit_words, new_perma_pinned_bits);
    }
}

bool
gc_igscan_advance_nr_slots_test_dirty (
  gc_igscan *const scan,
  ptrdiff_t nr_slots_to_move,
  const gc_heap *const h)
{
  /* Basically, we advance bytes and slots independently, using the
     former to check for dirtiness and the latter to check for
     startness.  The current position of the scan is encoded entirely
     in the current slot number, however.  */
  const size_t nr_slots_per_block = gc_heap_nr_slots_per_block (h);
  const ptrdiff_t slot_nr = scan->slot_nr;
  bool at_end = false;
  eassume (slot_nr >= 0);
  eassume (slot_nr <= nr_slots_per_block);
  eassume (nr_slots_to_move >= 0);
  eassume (nr_slots_to_move <= nr_slots_per_block - slot_nr);

  /* Make sure we don't index anything past the end.  */
  if (slot_nr + nr_slots_to_move == nr_slots_per_block)
    {
      eassume (nr_slots_to_move > 0);
      nr_slots_to_move -= 1;
      at_end = true;
      /* Make sure a subsequent call to gc_igscan_skip_to_object_start
         doesn't think we have another object to scan.  */
      scan->start_word = 0;
    }
  eassume (slot_nr + nr_slots_to_move < nr_slots_per_block);

  /* TODO: make the code work with runtime-variable page sizes.  */
  verify (EMACS_PAGE_SIZE_MIN + 0 == EMACS_PAGE_SIZE_MAX + 0);

  const size_t page_size = EMACS_PAGE_SIZE_MIN;
  const size_t byte_offset = slot_nr * gc_heap_nr_bytes_per_slot (h);
  size_t nr_bytes_left_to_move = nr_slots_to_move *
    gc_heap_nr_bytes_per_slot (h);
  size_t nr_bytes_left_in_page = (byte_offset % page_size)
    ? page_size - (byte_offset % page_size)
    : page_size;
  bool page_is_dirty = scan->page_is_dirty;
  size_t page_nr = byte_offset / page_size;
  const size_t start_page_nr = page_nr;
  bool any_byte_dirty = false;
  while (nr_bytes_left_to_move)
    {
      any_byte_dirty |= page_is_dirty;
      const size_t n = min (nr_bytes_left_to_move, nr_bytes_left_in_page);
      nr_bytes_left_in_page -= n;
      if (!nr_bytes_left_in_page)
        {
          page_nr += 1;
          nr_bytes_left_in_page = page_size;
          page_is_dirty = emacs_bitset_bit_set_p (
            &scan->b->meta.card_table[0],
            ARRAYELTS (scan->b->meta.card_table),
            page_nr);
        }
      nr_bytes_left_to_move -= n;
    }
  const size_t bits_left_in_word =
    emacs_bitset_bits_per_word
    - (slot_nr % emacs_bitset_bits_per_word);
  const ptrdiff_t new_slot_nr = slot_nr + nr_slots_to_move;
  if (nr_slots_to_move < bits_left_in_word)
    scan->start_word >>= nr_slots_to_move;
  else
    {
      const size_t new_word_nr = new_slot_nr / emacs_bitset_bits_per_word;
      scan->start_word = gc_block_start_bits (scan->b, h)[new_word_nr];
      scan->start_word >>= new_slot_nr % emacs_bitset_bits_per_word;
    }
  if (start_page_nr != page_nr)
    scan->page_is_dirty = page_is_dirty;

  eassert (((new_slot_nr * gc_heap_nr_bytes_per_slot (h))
            / page_size) == page_nr);
  eassert (scan->page_is_dirty ==
           emacs_bitset_bit_set_p (
             &scan->b->meta.card_table[0],
             ARRAYELTS (scan->b->meta.card_table),
             page_nr));

  scan->slot_nr = new_slot_nr + at_end;
  return any_byte_dirty;
}

void
gc_igscan_scan_object (gc_igscan *const scan,
                       const gc_phase phase,
                       const gc_heap *const h)
{
  const gc_cursor c = gc_block_make_cursor (scan->b, scan->slot_nr, h);
  void *const obj_ptr = gc_cursor_to_object (c, h);
  const ptrdiff_t obj_nr_slots = gc_cursor_object_nr_slots (c, h);

  eassume (scan->slot_nr < gc_heap_nr_slots_per_block (h));
  eassert (gc_cursor_object_starts_here (c, h));

  if (h->igscan_hook &&
      h->igscan_hook (obj_ptr, phase, scan))
    return;
  // in the case of marking (intergenerational) references, we need to scan everything
  // when we sweep, it shoud only take place on dirty pages
  if(gc_igscan_advance_nr_slots_test_dirty (scan, obj_nr_slots, h))
  scan_object (obj_ptr, h->lisp_type, phase);
}

ptrdiff_t
gc_igscan_get_next_dirty_slot_nr (const gc_igscan *const scan,
                                  const gc_heap *const h)
{
  if (scan->page_is_dirty)
    return scan->slot_nr;
  const ptrdiff_t slot_nr = scan->slot_nr;
  const ptrdiff_t slot_limit = scan->slot_limit;
  eassume (0 <= slot_nr);
  eassume (slot_nr <= slot_limit);
  eassume (slot_limit <= gc_heap_nr_slots_per_block (h));
  const size_t slot_limit_byte_offset =
    slot_limit * gc_heap_nr_bytes_per_slot (h);
  eassume (slot_limit_byte_offset <= gc_block_data_nr_bytes);
  verify ((gc_block_data_nr_bytes % EMACS_PAGE_SIZE_MIN) == 0);
  const size_t page_size = EMACS_PAGE_SIZE_MIN;
  const ptrdiff_t page_limit =
    (slot_limit_byte_offset + page_size - 1) / page_size;
  const size_t byte_offset = slot_nr * gc_heap_nr_bytes_per_slot (h);
  const ptrdiff_t page_nr = byte_offset / page_size;
  const ptrdiff_t next_dirty_page =
    emacs_bitset_scan_forward (&scan->b->meta.card_table[0],
                               ARRAYELTS (scan->b->meta.card_table),
                               page_nr, page_limit);
  if (next_dirty_page == page_limit)
    return slot_limit;  /* No dirty pages left.  */
  /* If we're at a dirty page, we should have already returned.  */
  eassume (next_dirty_page > page_nr);
  const size_t new_page_byte_offset = next_dirty_page * page_size;
  eassume (new_page_byte_offset < gc_block_data_nr_bytes);
  void *const new_page_ptr = &scan->b->u.data[new_page_byte_offset];
  const bool objects_can_span_pages =
    h->homogeneous_object_nr_bytes == 0 ||
    (h->homogeneous_object_nr_bytes % page_size) != 0;
  void *obj_start;
  if (objects_can_span_pages &&
      (obj_start = gc_block_maybe_find_live_object_containing (
        scan->b, new_page_ptr, h)) &&
      obj_start != new_page_ptr)
    {
      /* An object is straddling the dirty page boundary.
         Start scanning from the object's start.  */
      const gc_cursor c = gc_object_to_cursor (obj_start, h);
      eassert (gc_cursor_object_starts_here (c, h));
      return c.slot_nr;
    }
  /* No object is straddling the page boundary, but slot boundaries
     don't necessarily line up with page boundaries, so go to the
     first slot that covers the page.  */
  return new_page_byte_offset / gc_heap_nr_bytes_per_slot (h);
}

void
gc_igscan_skip_clean_pages (gc_igscan *const scan,
                            const gc_heap *const h)
{
  const ptrdiff_t slot_nr = scan->slot_nr;
  const ptrdiff_t new_slot_nr = gc_igscan_get_next_dirty_slot_nr (scan, h);
  eassume (0 <= slot_nr);
  eassume (slot_nr <= new_slot_nr);
  eassume (new_slot_nr <= gc_heap_nr_slots_per_block (h));
  eassume (scan->slot_limit <= gc_heap_nr_slots_per_block (h));
  if (new_slot_nr >= scan->slot_limit)
    {
      /* Don't bother maintaining the invariants if we're going to the
         scan boundary --- we'll be terminating the loop anyway.  */
      scan->slot_nr = scan->slot_limit;
    }
  else
    gc_igscan_advance_nr_slots_test_dirty (scan, new_slot_nr - slot_nr, h);
}

void
gc_igscan_skip_to_object_start (gc_igscan *const scan,
                                const gc_heap *const h)
{
  /* If we have non-zero start bits, we can find an object by shifting
     the start bit word.  gc_igscan_advance_nr_slots_test_dirty()
     takes care of tracking whether we've moved to a dirty page.  */
  eassume (scan->slot_nr >= scan->slot_limit ||
           scan->start_word ==
           gc_block_start_bits (scan->b, h)
           [scan->slot_nr / emacs_bitset_bits_per_word]
           >> scan->slot_nr % emacs_bitset_bits_per_word);
  if (scan->start_word)
    {
      const int tz = emacs_ctz_bw (scan->start_word);
      gc_igscan_advance_nr_slots_test_dirty (scan, tz, h);
      return;
    }
  const ptrdiff_t slot_limit = scan->slot_limit;
  /* Advance to the next word boundary, being careful not to do past
     scan_limit.  */
  if (scan->slot_nr % emacs_bitset_bits_per_word)
    {
      const size_t bits_left_in_word =
        emacs_bitset_bits_per_word
        - (scan->slot_nr % emacs_bitset_bits_per_word);
      if (slot_limit - scan->slot_nr <= bits_left_in_word)
        {
          scan->slot_nr = slot_limit;
          return;
        }
      gc_igscan_advance_nr_slots_test_dirty (scan, bits_left_in_word, h);
    }
  eassume ((scan->slot_nr % emacs_bitset_bits_per_word) == 0);
  /* If we don't have an object start immediately available, we have
     to go find one by scanning forward in the bitset.  If we're
     currently on a clean page, however, we can accelerate the bitset
     scan by using the card table to skip a large number of start bit
     words.  */
  gc_igscan_skip_clean_pages (scan, h);
  eassume (0 <= slot_limit && slot_limit <=
           gc_heap_nr_slots_per_block (h));
  while (scan->slot_nr < slot_limit && scan->start_word == 0)
    gc_igscan_advance_nr_slots_test_dirty (
      scan, emacs_bitset_bits_per_word, h);
  if (scan->slot_nr < slot_limit)
    {
      eassume (scan->start_word);
      const int tz = emacs_ctz_bw (scan->start_word);
      gc_igscan_advance_nr_slots_test_dirty (scan, tz, h);
    }
}

/* Scan, all the objects in this block that are part of the old
   generation and that may have been modified since the last garbage
   collection cycle.  */
void
gc_block_scan_intergenerational (gc_block *const b,
                                 const gc_phase phase,
                                 const gc_heap *const h)
{
  eassume (current_gc_phase == phase);
  const ptrdiff_t gen_y_slot_nr = gc_block_gen_y_slot_nr (b, h);
  const bool first_page_is_dirty = \
    emacs_bitset_bit_set_p (&b->meta.card_table[0],
			    ARRAYELTS (b->meta.card_table),
			    0);
  gc_igscan scan_buf = {
    .b = b,
    .slot_limit = gen_y_slot_nr,
    .slot_nr = 0,
    .page_is_dirty = first_page_is_dirty,
    /* If the first page isn't dirty, we're not going to read the
       initial start bits anyway, so we can skip the memory load and
       use zero instead.  gc_igscan_skip_to_object_start() will read a
       different start bit word in this case anyway.  */
    .start_word = gc_block_start_bits (b, h)[0],
  };
  gc_igscan *const scan = &scan_buf;
  for (;;)
    {
      gc_igscan_skip_to_object_start (scan, h);
      if (scan->slot_nr >= gen_y_slot_nr)
        break;
      gc_igscan_scan_object (scan, phase, h);
    }
}

/* Figure out the tospace location of each live object in heap H.
   Return a cursor pointing to the end of the new tospace for this
   heap.  */
void
gc_heap_plan_sweep (const gc_heap *const h)
{
  eassume (current_gc_phase == GC_PHASE_PLAN_SWEEP);

  if (!h->use_moving_gc)
    emacs_abort ();             /* XXX: wtf? */
  if (!gc_heap_is_moving_gc_this_cycle (h))
    emacs_abort ();             /* XXX: use identity mapping???  */

  /* Start allocating tospace at the start of the generation we're
     collecting.  If we're doing a major collection, we've already
     reset last_gc_tospace_tip to the start of the heap.  */
  gc_cursor tospace_tip = h->data->last_gc_tospace_tip;
  gc_heap_for_each_block (h, h->block_plan_sweep, &tospace_tip);

  /* Start allocating after the last object we place into tospace.
     We won't modify anything after tospace_tip, so in theory it'd be
     safe to let the mutator allocate from it while we sweep: but we
     don't let that happen right now.  */
  h->data->allocation_tip = tospace_tip;
  /* The next generation starts immediately after the end of the
     tospace of the old generation.  */
  h->data->last_gc_tospace_tip = tospace_tip;
}

/* Clear unmarked objects in heap H and reset all mark bits.  */
void
gc_heap_sweep (const gc_heap *const h)
{
  eassume (current_gc_phase == GC_PHASE_SWEEP);
  if (gc_heap_is_moving_gc_this_cycle (h))
    gc_heap_for_each_block (h, h->block_sweep_compact, NULL);
  else
    gc_heap_for_each_block (h, h->block_sweep_inplace, NULL);
  /* Unless we have to keep the tospace around for a while, collapse
     any remaining tospace blocks now, before we go on to sweep other
     heaps.  */
  if (!h->preserve_fromspace_across_sweeps)
    gc_heap_finalize_tospace (h);
}

void
gc_heap_finalize_tospace (const gc_heap *const h)
{
  while (h->data->per_cycle.sweep.next_tospace_flip_block_nr <
         h->data->block_array_size)
    gc_heap_flip_next_tospace_block (h);
}

/* Called after we've swept all heaps and scanned all roots.    */
void
gc_heap_cleanup_after_all_sweeps (const gc_heap *const h)
{
  /* At this point, all objects have been moved into tospace and so we
     can release the release the fromspace->tospace locators.  */
  gc_heap_for_each_block (h, gc_block_cleanup_after_all_sweeps, (void *) h);
  if (enable_checking)
    h->data->per_cycle.debug.locators_destroyed = true;
}

/* Allocate an object NR_BYTES long in heap H.  On failure, raise via
   memory_full.  Return a cursor pointing at the location at which we
   should initialize the new object.  The new object is not guaranteed
   to be initialized.  If we happen to know that the object contains
   all zero bits, set the IS_ZERO field in the result structure.
   The caller must commit the allocation by calling
   gc_allocation_commit() on the returned OBJ_C cursor in the result
   structure.  Emacs must not GC between the return from this function
   and the call to gc_allocation_commit(): it we do GC in this
   critical section, Emacs may use the memory returned by this
   function for some other object, since until gc_allocation_commit(),
   Emacs doesn't know that the memory returned by this function
   is in use.  */
gc_allocation
gc_heap_allocate (const gc_heap *const h, const size_t nr_bytes)
{
  const ptrdiff_t nr_needed_slots =
    gc_heap_object_size_to_nr_slots (h, nr_bytes);
  eassume (nr_needed_slots > 0);
  eassume (nr_needed_slots <= gc_heap_nr_slots_per_block (h));
  tally_consing_maybe_garbage_collect (nr_bytes);
  gc_cursor c = h->data->allocation_tip;
  for (;;)
    {
      /* If we can't fit this object in the tail of the current block,
         go to the next block unconditionally.  */
      if (gc_cursor_nr_slots_left_in_block (c, h) < nr_needed_slots)
        gc_cursor_advance_next_block_allocate_if_needed (&c, h);
      /* See whether we can fit an object at the current location: if
         we don't find an object start in the region we need for our
         new object, we can place a new object at the current
         allocation tip.  If we do find an object start, skip to the
         found object's end (allocating a new block if needed) and try
         again.  */
      if (!gc_cursor_advance_to_object_start_in_block(
            &c, nr_needed_slots, h))
        break;
      gc_cursor_advance_nr_slots_allocate_if_needed (
        &c, gc_cursor_object_nr_slots (c, h), h);
    }

  const gc_cursor obj_c = c;
  gc_cursor_advance_nr_slots_allocate_if_needed (&c, nr_needed_slots, h);
  h->data->allocation_tip = c;
  const bool is_zero = false;  // XXX
  const size_t allocated_nr_bytes =
    (size_t) nr_needed_slots * gc_heap_nr_bytes_per_slot (h);
  eassume (allocated_nr_bytes == nr_bytes);
  return (gc_allocation){
    .obj_c = obj_c,
    .is_zero = is_zero,
#ifdef ENABLE_CHECKING
    .nr_bytes = nr_bytes,
#endif
  };
}

void
gc_allocation_commit (gc_allocation *const r, const gc_heap *const h)
{
  gc_cursor_make_object_start_here (r->obj_c, h);
#ifdef ENABLE_CHECKING
  eassert (r->nr_bytes == gc_cursor_object_nr_bytes (r->obj_c, h));
#endif
}

/* Allocate an object NR_BYTES long in heap H.  On failure, raise via
   memory_full.  Return a cursor pointing at the location at which we
   should initialize the new object.  The new object is guaranteed to
   contain all zero bits.  Use this function instead of
   gc_heap_allocate followed by memset to take advantage of situations
   in which we can guarantee that the new object is already zero.  */
gc_allocation
gc_heap_allocate_and_zero (const gc_heap *const h, const size_t nr_bytes)
{
  gc_allocation r = gc_heap_allocate (h, nr_bytes);
  if (!r.is_zero)
    {
      memset (gc_cursor_to_object (r.obj_c, h), 0, nr_bytes);
      r.is_zero = true;
    }
  return r;
}

/* Allocate a tospace location for an object NR_NEEDED_SLOTS long,
   starting the search for a tospace location at *TOSPACE_TIP_INOUT.
   Return a valid cursor pointing to a tospace location for the object
   and set *TOSPACE_TIP_INOUT to the next place to start searching for
   tospace object locations or to the invalid cursor if we exhausted
   the heap.  This function does not fail.  */
gc_cursor
gc_heap_allocate_tospace (const gc_heap *h,
                          gc_cursor *tospace_tip_inout,
                          const ptrdiff_t nr_needed_slots)
{
  eassume (nr_needed_slots > 0);
  eassume (nr_needed_slots <= gc_heap_nr_slots_per_block (h));
  gc_cursor c = *tospace_tip_inout;
  for (;;)
    {
      /* If we can't fit this object in the tail of the current block,
         go to the next block unconditionally.  */
      if (gc_cursor_nr_slots_left_in_block (c, h) < nr_needed_slots &&
          !gc_cursor_advance_next_block (&c, h))
        emacs_unreachable ();  /* Ran out of room.  */
      /* See whether we can fit an object at the current location: the
         only reason we couldn't fit the object here would be that the
         proposed tospace allocation object collides with a pinned
         fromspace object: in this case, skip past the pin.  */
      if (!gc_cursor_advance_to_object_pin_in_block(
            &c, nr_needed_slots, h))
        break /* Found a pin-free hole.  */;
      eassert (gc_cursor_object_starts_here (c, h));
      const ptrdiff_t nr_pinned_slots = gc_cursor_object_nr_slots (c, h);
      if (!gc_cursor_advance_nr_slots (&c, nr_pinned_slots, h))
        emacs_unreachable ();  /* Ran out of room.  */
    }
  const gc_cursor obj_c = c;
  if (!gc_cursor_advance_nr_slots (&c, nr_needed_slots, h))
    {
      /* We can't get here: the allocation cursor always points to
         valid free space, and compacting GC always moves that cursor
         to a place earlier in the heap or leaves it in place --- so
         moving the cursor past the end of the last tospace object
         should always succeed.  In the worst case, every object in
         the heap is live, GC doesn't free any objects at all, and the
         resulting tospace tip is equal to the allocation tip before
         the GC.  */
      emacs_unreachable ();
    }
  *tospace_tip_inout = c;
  return obj_c;
}

gc_cursor
gc_locator_to_cursor (const gc_locator locator, const gc_heap *const h)
{
  eassert (!gc_locator_invalid_p (locator, h));
  const gc_cursor c = {
    .block = h->data->block_array[gc_locator_block_nr (locator, h)],
    .slot_nr = gc_locator_slot_nr (locator, h),
  };
  gc_cursor_check (c, h);
  return c;
}

bool
gc_locator_invalid_p (const gc_locator locator, const gc_heap *const h)
{
  (void) h;
  return locator.i == gc_locator_invalid.i;
}

size_t
gc_locator_block_nr (const gc_locator locator, const gc_heap *const h)
{
  (void) h;
  return locator.i >> gc_locator_nr_bits_slot_nr;
}

ptrdiff_t
gc_locator_slot_nr (const gc_locator locator, const gc_heap *const h)
{
  (void) h;
  const size_t mask = (((size_t) 1) << gc_locator_nr_bits_slot_nr) - 1;
  const ptrdiff_t slot_nr = locator.i & mask;
  eassume (slot_nr >= 0);
  return slot_nr;
}

size_t
gc_compute_total_live_nr_bytes (void)
{
  size_t live_nr_bytes = 0;
  for (int i = 0; i < ARRAYELTS (gc_heaps); ++i)
    {
      const size_t nr_slots = gc_heaps[i]->data->stats.nr_slots;
      size_t heap_nr_bytes;
      if (INT_ADD_WRAPV (nr_slots,
                         gc_heap_nr_bytes_per_slot (gc_heaps[i]),
                         &heap_nr_bytes))
        emacs_unreachable ();
      if (INT_ADD_WRAPV (live_nr_bytes, heap_nr_bytes, &live_nr_bytes))
        emacs_unreachable ();
    }
  return live_nr_bytes;
}


/***********************************************************************
			 Interval Allocation
 ***********************************************************************/

static gc_heap_data gc_interval_heap_data;
static const gc_heap gc_interval_heap = {
  .data = &gc_interval_heap_data,
  .heap_symbol_index = iQintervals,
  .mem_type = MEM_TYPE_INTERVAL,
  .lisp_type = Lisp_Int0 /* sic */,
  .aligned_blocks = true,
  .use_moving_gc = true,
  .homogeneous_object_nr_bytes = gc_heap_interval_slot_size,
  GC_HEAP_BITS_CONFIG (interval),
  CONFIG_STANDARD_HEAP_FUNCTIONS (gc_interval_heap),
};

DEFINE_STANDARD_HEAP_FUNCTIONS (gc_interval_heap);

void
gc_mark_or_enqueue_interval (const INTERVAL i)
{
  if (interval_marked_p (i))
    return;  /* Already on mark queue or marked.  */
  set_interval_marked (i);
  ENQUEUE_AND_RETURN_IF_TOO_DEEP (gc_interval_smuggle (i));
  scan_interval (i, GC_PHASE_MARK);
}

bool
interval_marked_p (const struct interval *const i)
{
  return gc_pdumper_object_p (i)
    ? pdumper_marked_p (i)
    : gc_object_is_marked (i, &gc_interval_heap);
}

void
set_interval_marked (const INTERVAL i)
{
  if (gc_pdumper_object_p (i))
    pdumper_set_marked (i);
  else
    gc_object_set_marked (i, &gc_interval_heap);
}

void
set_interval_pinned (const INTERVAL i)
{
  if (!gc_pdumper_object_p (i))
    gc_object_set_pinned (i, &gc_interval_heap);
}

INTERVAL
make_interval (void)
{
  gc_allocation r =
    gc_heap_allocate_and_zero (&gc_interval_heap, gc_heap_interval_slot_size);
  const INTERVAL i = gc_cursor_to_object (r.obj_c, &gc_interval_heap);
  gc_allocation_commit (&r, &gc_interval_heap);
  return i;
}

/* If P is a pointer into a live Lisp interval object on the heap, return
   the object.  Otherwise, return NULL.  M is a pointer to the
   mem_block for P.  */
INTERVAL
live_interval_holding (const struct mem_node *const m, const void *const p)
{
  if (!m || m->type != MEM_TYPE_INTERVAL)
    return NULL;
  gc_block *const b = gc_block_from_mem_node (m);
  return gc_block_maybe_find_live_object_containing (
    b, p, &gc_interval_heap);
}

void
scan_interval (const INTERVAL i, const gc_phase phase)
{
  scan_reference_pointer_to_interval (&i->left, phase);
  scan_reference_pointer_to_interval (&i->right, phase);
  if (i->up_obj)
    scan_reference (&i->up.obj, phase);
  else
    scan_reference_pointer_to_interval (&i->up.interval, phase);
  scan_reference (&i->plist, phase);
}

INTERVAL
point_interval_into_tospace (const INTERVAL i)
{
  if (gc_pdumper_object_p (i))
    return i;
  return gc_object_point_into_tospace (i, &gc_interval_heap);
}


/***********************************************************************
			  String Allocation
 ***********************************************************************/

/* Lisp strings are struct Lisp_String objects (four-words long)
   allocated on the string heap.  These objects contain string
   metadata --- size (in bytes), size (in characters), a pointer to
   the interval tree, and string data.  Actual string data lives in
   one of a few different places:

  1) small strings are placed directly in the struct Lisp_String
     in the small fork of the union;

  2) medium-sized strings (strings with lengths too large for
     embedding in Lisp_String but lengths small enough to be described
     directly in the pseudovector header) are allocated as
     PVEC_STRING_DATA vectors;

  3) large strings (strings with lengths too large to encode in the
     pseudovector header) are also stored as PVEC_STRING_DATA vectors,
     but with zero "other" size; the actual large string size is in
     the ptrdiff_t following the pseudovector header, and the string
     bytes follow; and

  4) strings formed from string literals by Emacs C code are stored
     directly in the Emacs rodata section and the Lisp_String data
     field points there; small C strings are, however, stored directly
     in the Lisp_String for cache efficiency reasons.

  Depending on the configuration of the vector heap, PVEC_STRING_DATA
  vectors larger than a certain size may be allocated in standalone
  memory blocks.
 */

static gc_heap_data gc_string_heap_data;
static const gc_heap gc_string_heap = {
  .data = &gc_string_heap_data,
  .heap_symbol_index = iQstrings,
  .mem_type = MEM_TYPE_STRING,
  .lisp_type = Lisp_String,
  .aligned_blocks = true,
  .use_moving_gc = true,
  .homogeneous_object_nr_bytes = gc_heap_string_slot_size,
  GC_HEAP_BITS_CONFIG (string),
  CONFIG_STANDARD_HEAP_FUNCTIONS (gc_string_heap),
};

DEFINE_STANDARD_HEAP_FUNCTIONS (gc_string_heap);

void
gc_mark_or_enqueue_string (struct Lisp_String *const s)
{
  if (string_marked_p (s))
    return;  /* Already on mark queue or marked.  */
  set_string_marked (s);
  ENQUEUE_AND_RETURN_IF_TOO_DEEP (make_lisp_ptr (s, Lisp_String));
  scan_string (s, GC_PHASE_MARK);
}

/* Return the size of an sdata structure large enough to hold N bytes
   of string data.  This counts the sdata structure, the N bytes, a
   terminating NUL byte, and alignment padding.  */
ptrdiff_t
sdata_size (const ptrdiff_t n)
{
  eassume (n > 0);
  eassume (n <= string_max_nr_bytes_large);
  const ptrdiff_t data_offset = n > string_max_nr_bytes_medium
    ? offsetof (struct sdata, u.large.data)
    : offsetof (struct sdata, u.medium.data);
  /* Add one for terminating NUL.  */
  const ptrdiff_t unaligned_size = data_offset + n + 1;
  return ROUNDUP (unaligned_size, gc_slot_size);
}

struct Lisp_String *
allocate_string (void)
{
  gc_allocation r =
    gc_heap_allocate_and_zero (&gc_string_heap, gc_heap_string_slot_size);
  gc_allocation_commit (&r, &gc_string_heap);
  struct Lisp_String *const s =
    gc_cursor_to_object (r.obj_c, &gc_string_heap);;
  eassume (!stack_string_p (s));
  return s;
}

bool
string_has_sdata (const struct Lisp_String *const s)
{
  return !STRING_INTERNAL_P (s) && !s->u.s.u.external.is_c_string;
}

sdata *
sdata_from_string (struct Lisp_String *const s)
{
  eassume (string_has_sdata (s));
  return STRING_BYTES (s) > string_max_nr_bytes_medium
    ? econtainer_of (STRING_DATA (s), sdata, u.large.data)
    : econtainer_of (STRING_DATA (s), sdata, u.medium.data);
}

unsigned char *
string_data_pointer (struct Lisp_String *s, sdata *const v)
{
  eassume (string_has_sdata (s));
  return STRING_BYTES (s) > string_max_nr_bytes_medium
    ? &v->u.large.data[0]
    : &v->u.medium.data[0];
}

void
scan_string (struct Lisp_String *const s, const gc_phase phase)
{
  eassume (phase == GC_PHASE_MARK || phase == GC_PHASE_SWEEP);
  if (string_has_sdata (s) && STRING_DATA (s))
    {
      /* The string data field is a direct pointer to the character
         data and not a pointer to the pseudovector object header, so
         we have to mark it specially.  */
      sdata *const v = sdata_from_string (s);
      if (phase == GC_PHASE_MARK)
        gc_mark_or_enqueue_vectorlike (&v->header);
      else if (phase == GC_PHASE_SWEEP)
        {
          sdata *const v_tospace =
            point_vectorlike_into_tospace (&v->header);
          if (v != v_tospace)
            s->u.s.u.external.data = string_data_pointer (s, v_tospace);
        }
    }
  scan_reference_pointer_to_interval (&s->u.s.intervals, phase);
  /* TODO: just keep intervals always balanced? */
  eassume (phase == current_gc_phase);
  if (false /* XXX!!!! */ && phase == GC_PHASE_SWEEP && s->u.s.intervals)
    s->u.s.intervals = balance_intervals (s->u.s.intervals);
}

struct Lisp_String *
point_string_into_tospace (struct Lisp_String *const s)
{
  if (gc_pdumper_object_p (s))
    return s;
  return gc_object_point_into_tospace (s, &gc_string_heap);
}

/* Initialize string allocation.  Called from init_alloc_once.  */
void
init_strings (void)
{
  struct Lisp_String *s = allocate_string ();
  *s = (struct Lisp_String){
    .u.s.u.internal.is_internal = true,
  };
  empty_unibyte_string = make_lisp_ptr (s, Lisp_String);
  staticpro (&empty_unibyte_string);

  s = allocate_string ();
  *s = (struct Lisp_String){
    .u.s.u.internal.is_internal = true,
  };
  empty_multibyte_string = make_lisp_ptr (s, Lisp_String);
  staticpro (&empty_multibyte_string);
}

/* Set up Lisp_String S for holding NCHARS characters, NBYTES bytes,
   plus a NUL byte at the end.  Allocate an sdata structure DATA for
   S, and set S->u.s.data to SDATA->u.data.  Store a NUL byte at the
   end of S->u.s.data.  Set S->u.s.size to NCHARS and S->u.s.size_byte
   to NBYTES.  Free S->u.s.data if it was initially non-null.

   If CLEARIT, also clear the other bytes of S->u.s.data.  */
void
allocate_string_data (struct Lisp_String *s,
		      const EMACS_INT nchars,
                      const EMACS_INT nbytes,
                      const bool clearit)
{
  eassume (nchars > 0);
  eassume (nbytes >= nchars);

  if (STRING_BYTES_MAX < nbytes)
    string_overflow ();

  /* needed_nr_bytes and needed_nr_words include the terminating NUL.  */
  const ptrdiff_t needed_nr_bytes = sdata_size (nbytes);
  const ptrdiff_t needed_nr_words = (needed_nr_bytes + word_size) / word_size;
  /* It's safe to leave the string's payload uninitialized: GC
     doesn't scan it.  */
  gc_vector_allocation vr = allocate_vectorlike (needed_nr_words, clearit);
  struct Lisp_Vector *const v = gc_vector_allocation_vector (vr);
  sdata *const sd = (sdata *) v;
  ptrdiff_t plain_vec_nbytes;
  if (enable_checking)
    plain_vec_nbytes = vectorlike_nbytes (&sd->header);
  unsigned char *data;
  if (nbytes > string_max_nr_bytes_medium)
    {
      data = &sd->u.large.data[0];
      sd->u.large.len = needed_nr_words;
      XSETPVECTYPESIZE (sd, PVEC_STRING_DATA, 0, 0);
    }
  else
    {
      data = &sd->u.medium.data[0];
      XSETPVECTYPESIZE (sd, PVEC_STRING_DATA, 0, needed_nr_words);
    }
  if (enable_checking)
    eassert (vectorlike_nbytes (&sd->header) == plain_vec_nbytes);

  s->u.s.u.external.is_internal = false;
  s->u.s.u.external.is_c_string = false;
  s->u.s.u.external.size = nchars;
  s->u.s.u.external.size_byte = nbytes;
  s->u.s.u.external.data = data;
  s->u.s.u.external.data[nbytes] = '\0';
  eassume (s->u.s.u.external.size == nchars);
  eassume (s->u.s.u.external.size_byte == nbytes);
  gc_vector_allocation_commit (&vr);
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
  struct Lisp_String *const xs = XSTRING (string);
  sdata *const old_sdata = string_has_sdata (xs)
    ? sdata_from_string (xs) : NULL;
  const ptrdiff_t nchars = SCHARS (string);
  const ptrdiff_t nbytes = SBYTES (string);
  const ptrdiff_t new_nbytes = nbytes + (new_clen - clen);
  unsigned char *const data = SDATA (string);
  unsigned char *new_charaddr;

  if (STRING_INTERNAL_P (xs) && new_nbytes <= string_max_nr_bytes_small)
    {
      /* New size fits in the small string buffer.  */
      xs->u.s.u.internal.size_byte = new_nbytes;
      eassume (xs->u.s.u.internal.size_byte == new_nbytes);
      new_charaddr = data + cidx_byte;
      memmove (new_charaddr + new_clen, new_charaddr + clen,
	       nbytes - (cidx_byte + (clen - 1)));
    }
  else if (old_sdata && sdata_size (nbytes) == sdata_size (new_nbytes))
    {
      /* No need to reallocate, as the size change falls within the
	 alignment slop.  */
      eassume (!STRING_INTERNAL_P (xs));
      xs->u.s.u.external.size_byte = new_nbytes;
      new_charaddr = data + cidx_byte;
      memmove (new_charaddr + new_clen, new_charaddr + clen,
	       nbytes - (cidx_byte + (clen - 1)));
    }
  else
    {
      allocate_string_data (xs, nchars, new_nbytes, false);
      unsigned char *const new_data = SDATA (string);
      new_charaddr = new_data + cidx_byte;
      memcpy (new_charaddr + new_clen, data + cidx_byte + clen,
	      nbytes - (cidx_byte + clen));
      memcpy (new_data, data, cidx_byte);
    }

  clear_string_char_byte_cache ();
  return new_charaddr;
}

void
string_overflow (void)
{
  error ("Maximum string size exceeded");
}

DEFUN ("make-string", Fmake_string, Smake_string, 2, 3, 0,
       doc: /* Return a newly created string of length LENGTH, with INIT in each element.
LENGTH must be an integer.
INIT must be an integer that represents a character.
If optional argument MULTIBYTE is non-nil, the result will be
a multibyte string even if INIT is an ASCII character.  */)
  (Lisp_Object length, Lisp_Object init, Lisp_Object multibyte)
{
  Lisp_Object val;
  EMACS_INT nbytes;

  CHECK_FIXNAT (length);
  CHECK_CHARACTER (init);

  int c = XFIXNAT (init);
  bool clearit = !c;

  if (ASCII_CHAR_P (c) && NILP (multibyte))
    {
      nbytes = XFIXNUM (length);
      val = make_clear_string (nbytes, clearit);
      if (nbytes && !clearit)
	{
	  memset (SDATA (val), c, nbytes);
	  SDATA (val)[nbytes] = 0;
	}
    }
  else
    {
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      ptrdiff_t len = CHAR_STRING (c, str);
      EMACS_INT string_len = XFIXNUM (length);

      if (INT_MULTIPLY_WRAPV (len, string_len, &nbytes))
	string_overflow ();
      val = make_clear_multibyte_string (string_len, nbytes, clearit);
      if (!clearit)
	{
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
    }

  return val;
}

/* Return a string allocated in pure space.  Do not
   allocate the string data, just point to DATA.  */
Lisp_Object
make_c_string (const char *data, ptrdiff_t nchars)
{
  eassume (nchars >= 0);
  eassume (nchars <= STRING_BYTES_MAX);
  struct Lisp_String *s = allocate_string ();
  if (nchars > string_max_nr_bytes_small)
    *s = (struct Lisp_String){
      .u.s.u.external = {
        .is_internal = false,
        .is_c_string = true,
        .size = nchars,
        .size_byte = nchars,
        .data = (unsigned char *) data,
      }};
  else
    {
      *s = (struct Lisp_String){
        .u.s.u.internal = {
          .is_internal = true,
          .size = nchars,
          .size_byte = nchars,
        }};
      /* Already NUL-terminated.  */
      memcpy (STRING_DATA (s), data, nchars);
    }
  return make_lisp_ptr (s, Lisp_String);
}

bool
string_marked_p (const struct Lisp_String *s)
{
  return gc_pdumper_object_p (s)
    ? pdumper_marked_p (s)
    : eunlikely (stack_string_p (s))
    ? true /* conservative GC scans contents */
    : gc_object_is_marked (s, &gc_string_heap);
}

void
set_string_marked (struct Lisp_String *s)
{
  if (gc_pdumper_object_p (s))
    pdumper_set_marked (s);
  else
    {
      eassume (!stack_string_p (s));
      gc_object_set_marked (s, &gc_string_heap);
    }
}

void
set_string_pinned (struct Lisp_String *s)
{
  if (!gc_pdumper_object_p (s) && !stack_string_p (s))
    gc_object_set_pinned (s, &gc_string_heap);
}

EMACS_INT
string_identity_hash_code (struct Lisp_String *s)
{
  if (!gc_pdumper_object_p (s) && !stack_string_p (s))
    gc_object_perma_pin (s, &gc_string_heap);
  return (uintptr_t) s / alignof (struct Lisp_String);
}


/* Bool vectors  */

/* Fill A with 1 bits if INIT is non-nil, and with 0 bits otherwise.
   Return A.  */

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

/* Make a unibyte string from LENGTH bytes at CONTENTS.  */

Lisp_Object
make_unibyte_string (const char *contents, ptrdiff_t length)
{
  register Lisp_Object val;
  val = make_uninit_string (length);
  memcpy (SDATA (val), contents, length);
  return val;
}


/* Make a multibyte string from NCHARS characters occupying NBYTES
   bytes at CONTENTS.  */
Lisp_Object
make_multibyte_string (const char *contents,
		       ptrdiff_t nchars, ptrdiff_t nbytes)
{
  register Lisp_Object val;
  val = make_uninit_multibyte_string (nchars, nbytes);
  memcpy (SDATA (val), contents, nbytes);
  return val;
}

/* Make a string from NCHARS characters occupying NBYTES bytes at
   CONTENTS.  It is a multibyte string if NBYTES != NCHARS.  */
Lisp_Object
make_string_from_bytes (const char *contents,
			ptrdiff_t nchars, ptrdiff_t nbytes)
{
  Lisp_Object val = make_uninit_multibyte_string (nchars, nbytes);
  memcpy (SDATA (val), contents, nbytes);
  if (SBYTES (val) == SCHARS (val))
    STRING_SET_UNIBYTE (val);
  return val;
}

/* Make a string from NCHARS characters occupying NBYTES bytes at
   CONTENTS.  The argument MULTIBYTE controls whether to label the
   string as multibyte.  If NCHARS is negative, it counts the number of
   characters by itself.  */
Lisp_Object
make_specified_string (const char *contents,
		       ptrdiff_t nchars, ptrdiff_t nbytes, bool multibyte)
{
  Lisp_Object val;

  if (nchars < 0)
    {
      if (multibyte)
	nchars = multibyte_chars_in_text ((const unsigned char *) contents,
					  nbytes);
      else
	nchars = nbytes;
    }
  val = make_uninit_multibyte_string (nchars, nbytes);
  memcpy (SDATA (val), contents, nbytes);
  if (!multibyte)
    STRING_SET_UNIBYTE (val);
  return val;
}

/* Return a unibyte Lisp_String set up to hold LENGTH characters
   occupying LENGTH bytes.  If CLEARIT, clear its contents to null
   bytes; otherwise, the contents are uninitialized.  */
Lisp_Object
make_clear_string (EMACS_INT length, bool clearit)
{
  Lisp_Object val;

  if (!length)
    return empty_unibyte_string;
  val = make_clear_multibyte_string (length, length, clearit);
  STRING_SET_UNIBYTE (val);
  return val;
}

/* Return a unibyte Lisp_String set up to hold LENGTH characters
   occupying LENGTH bytes.  */
Lisp_Object
make_uninit_string (EMACS_INT length)
{
  return make_clear_string (length, false);
}


/* Return a multibyte Lisp_String set up to hold NCHARS characters
   which occupy NBYTES bytes.  If CLEARIT, clear its contents to null
   bytes; otherwise, the contents are uninitialized.  */
Lisp_Object
make_clear_multibyte_string (EMACS_INT nchars, EMACS_INT nbytes, bool clearit)
{
  eassert (nchars >= 0);
  eassume (nchars <= nbytes);
  if (!nbytes)
    return empty_multibyte_string;

  struct Lisp_String *const s = allocate_string ();
  if (nbytes <= string_max_nr_bytes_small)
    {
      s->u.s.u.internal.is_internal = true;
      s->u.s.u.internal.size_byte = nbytes;
      s->u.s.u.internal.size = nchars;
      /* Data already zero.  */
    }
  else
    allocate_string_data (s, nchars, nbytes, clearit);
  return make_lisp_ptr (s, Lisp_String);
}

/* Return a multibyte Lisp_String set up to hold NCHARS characters
   which occupy NBYTES bytes.  */
Lisp_Object
make_uninit_multibyte_string (EMACS_INT nchars, EMACS_INT nbytes)
{
  return make_clear_multibyte_string (nchars, nbytes, false);
}

/* Print arguments to BUF according to a FORMAT, then return
   a Lisp_String initialized with the data from BUF.  */
Lisp_Object
make_formatted_string (char *buf, const char *format, ...)
{
  /* XXX: FIXME: this function sucks. Just vsprintf directly into the
     new string.  */
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

  if (!(size > LARGE_STRING_BYTES
	|| PURE_P (data) || pdumper_object_p (data)
	|| s->u.s.size_byte == -3))
    {
      eassert (s->u.s.size_byte == -1);
      sdata *old_sdata = SDATA_OF_STRING (s);
      allocate_string_data (s, size, size, false, true);
      memcpy (s->u.s.data, data, size);
      old_sdata->string = NULL;
      SDATA_NBYTES (old_sdata) = size;
    }
  s->u.s.size_byte = -3;
}


static gc_heap_data gc_float_heap_data;
static const gc_heap gc_float_heap = {
  .data = &gc_float_heap_data,
  .heap_symbol_index = iQfloats,
  .mem_type = MEM_TYPE_FLOAT,
  .lisp_type = Lisp_Float,
  .aligned_blocks = true,
  .use_moving_gc = true,
  .no_lisp_data = true,
  .homogeneous_object_nr_bytes = gc_heap_float_slot_size,
  GC_HEAP_BITS_CONFIG (float_),
  CONFIG_STANDARD_HEAP_FUNCTIONS (gc_float_heap),
};

DEFINE_STANDARD_HEAP_FUNCTIONS (gc_float_heap);

void
gc_mark_or_enqueue_float (struct Lisp_Float *const f)
{
  if (float_marked_p (f))
    return;  /* Already on mark queue or marked.  */
  set_float_marked (f);
  /* Nothing to scan.  */
}

/* Return a new float object with value FLOAT_VALUE.  */

Lisp_Object
make_float (const double float_value)
{
  gc_allocation r =
    gc_heap_allocate (&gc_float_heap, gc_heap_float_slot_size);
  struct Lisp_Float *const f = gc_cursor_to_object (r.obj_c, &gc_float_heap);
  f->data = float_value;
  gc_allocation_commit (&r, &gc_float_heap);
  return make_lisp_ptr (f, Lisp_Float);
}

bool
float_marked_p (const struct Lisp_Float *f)
{
  return gc_pdumper_object_p (f)
    ? true  /* pdumper floats are cold and always marked */
    : gc_object_is_marked (f, &gc_float_heap);
}

void
set_float_marked (struct Lisp_Float *f)
{
  eassert (!gc_pdumper_object_p (f));
  gc_object_set_marked (f, &gc_float_heap);
}

void
set_float_pinned (struct Lisp_Float *f)
{
  if (!gc_pdumper_object_p (f))
    gc_object_set_pinned (f, &gc_float_heap);
}

EMACS_INT
float_identity_hash_code (struct Lisp_Float *f)
{
  if (!gc_pdumper_object_p (f))
    gc_object_perma_pin (f, &gc_float_heap);
  return (uintptr_t) f / alignof (struct Lisp_Float);
}

/* If P is a pointer into a live Lisp float object on the heap, return
   the object.  Otherwise, return nil.  M is a pointer to the
   mem_block for P.  */
Lisp_Object
live_float_holding (const struct mem_node *const m, const void *const p)
{
  if (!m || m->type != MEM_TYPE_FLOAT)
    return Qnil;
  gc_block *const b = gc_block_from_mem_node (m);
  struct Lisp_Float *const f =
    gc_block_maybe_find_live_object_containing (b, p, &gc_float_heap);
  return f ? make_lisp_ptr (f, Lisp_Float) : Qnil;
}

bool
live_float_p (const struct mem_node *const m, const void *const p)
{
  return !NILP (live_float_holding (m, p));
}

struct Lisp_Float *
point_float_into_tospace (struct Lisp_Float *const f)
{
  if (gc_pdumper_object_p (f))
    return f;
  return gc_object_point_into_tospace (f, &gc_float_heap);
}


/***********************************************************************
			   Cons Allocation
 ***********************************************************************/

static gc_heap_data gc_cons_heap_data;
static const gc_heap gc_cons_heap = {
  .data = &gc_cons_heap_data,
  .heap_symbol_index = iQcons,
  .mem_type = MEM_TYPE_CONS,
  .lisp_type = Lisp_Cons,
  .aligned_blocks = true,
  .use_moving_gc = true,
  .homogeneous_object_nr_bytes = gc_heap_cons_slot_size,
  GC_HEAP_BITS_CONFIG (cons),
  CONFIG_STANDARD_HEAP_FUNCTIONS (gc_cons_heap),
};

DEFINE_STANDARD_HEAP_FUNCTIONS (gc_cons_heap);

/* Mark and maybe scan a cons.  */
void
gc_mark_or_enqueue_cons (struct Lisp_Cons *c)
{
  bool checked_stack_depth = false;
  do {
    if (cons_marked_p (c))
      return;  /* Already on mark queue or marked.  */
    set_cons_marked (c);
    if (!checked_stack_depth)
      {
	checked_stack_depth = true;
	ENQUEUE_AND_RETURN_IF_TOO_DEEP (make_lisp_ptr (c, Lisp_Cons));
      }
    scan_cons (c, GC_PHASE_MARK);
    /* Try looping over the CDR of the cons if it's also a cons ----
       this way, we avoid some recursion.  Note that we don't have to
       check the stack depth again.  */
    if (CONSP(c->u.s.cdr))
      c = XCONS (c->u.s.cdr);
    else
      break;
  } while (c);
}

DEFUN ("cons", Fcons, Scons, 2, 2, 0,
       doc: /* Create a new cons, give it CAR and CDR as components, and return it.  */)
  (Lisp_Object car, Lisp_Object cdr)
{
  gc_allocation r = gc_heap_allocate (&gc_cons_heap, gc_heap_cons_slot_size);
  struct Lisp_Cons *const cons =
    gc_cursor_to_object (r.obj_c, &gc_cons_heap);
  eassume (!stack_cons_p (cons));
  cons->u.s.car = car;
  cons->u.s.cdr = cdr;
  gc_allocation_commit (&r, &gc_cons_heap);
  return make_lisp_ptr (cons, Lisp_Cons);
}

bool
cons_marked_p (const struct Lisp_Cons *c)
{
  return gc_pdumper_object_p (c)
    ? pdumper_marked_p (c)
    : eunlikely (stack_cons_p (c))
    ? true  /* conservative GC scans stack cons contents */
    : gc_object_is_marked (c, &gc_cons_heap);
}

void
set_cons_marked (struct Lisp_Cons *c)
{
  if (gc_pdumper_object_p (c))
    pdumper_set_marked (c);
  else
    {
      eassume (!stack_cons_p (c));
      gc_object_set_marked (c, &gc_cons_heap);
    }
}

void
set_cons_pinned (struct Lisp_Cons *c)
{
  if (!gc_pdumper_object_p (c) && !stack_cons_p (c))
    gc_object_set_pinned (c, &gc_cons_heap);
}

EMACS_INT
cons_identity_hash_code (struct Lisp_Cons *c)
{
  if (!gc_pdumper_object_p (c) && !stack_cons_p (c))
    gc_object_perma_pin (c, &gc_cons_heap);
  return (uintptr_t) c / alignof (struct Lisp_Cons);
}

/* If P is a pointer into a live Lisp cons object on the heap, return
   the object.  Otherwise, return nil.  M is a pointer to the
   mem_block for P.  */
Lisp_Object
live_cons_holding (const struct mem_node *const m, const void *const p)
{
  if (!m || m->type != MEM_TYPE_CONS)
    return Qnil;
  gc_block *const b = gc_block_from_mem_node (m);
  struct Lisp_Cons *const cons =
    gc_block_maybe_find_live_object_containing (b, p, &gc_cons_heap);
  return cons ? make_lisp_ptr (cons, Lisp_Cons) : Qnil;
}

bool
live_cons_p (const struct mem_node *const m, const void *const p)
{
  return !NILP (live_cons_holding (m, p));
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
Lisp_Object
cons_listn (const ptrdiff_t count, const Lisp_Object arg,
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

struct Lisp_Cons *
point_cons_into_tospace (struct Lisp_Cons *const f)
{
  if (gc_pdumper_object_p (f))
    return f;
  return gc_object_point_into_tospace (f, &gc_cons_heap);
}


/***********************************************************************
			   Vector Allocation
 ***********************************************************************/

static gc_heap_data gc_vector_heap_data;
static const gc_heap gc_vector_heap = {
  .data = &gc_vector_heap_data,
  .heap_symbol_index = iQvectors,
  .mem_type = MEM_TYPE_VECTOR,
  .lisp_type = Lisp_Vectorlike,
  .aligned_blocks = true,
  .use_moving_gc = true,
  .preserve_fromspace_across_sweeps = true,
  .object_nr_bytes = gc_vector_object_nr_bytes,
  .cleanup = vector_cleanup,
  .igscan_hook = vector_igscan_hook,
  GC_HEAP_BITS_CONFIG (vector),
  CONFIG_STANDARD_HEAP_FUNCTIONS (gc_vector_heap),
};

DEFINE_STANDARD_HEAP_FUNCTIONS (gc_vector_heap);

void
gc_mark_or_enqueue_vectorlike (union vectorlike_header *const v)
{
  if (vectorlike_marked_p (v))
    return;  /* Already on mark queue or marked.  */
  set_vectorlike_marked (v);
  ENQUEUE_AND_RETURN_IF_TOO_DEEP (make_lisp_ptr (v, Lisp_Vectorlike));
  scan_vectorlike (v, GC_PHASE_MARK);
}

/* Provide special support for scanning parts of large vectors that
   have changed.  Without this function, we'd scan the whole vector if
   any page containing the vector were marked as dirty --- that's
   wasteful.  */
bool
vector_igscan_hook(void *const obj_ptr,
                   const gc_phase phase,
                   gc_igscan *const scan)
{
  struct Lisp_Vector *const v = obj_ptr;
  if (PVTYPE (&v->header) != PVEC_NORMAL_VECTOR)
    return false;  /* Do the normal thing.  */
  const gc_heap *const h = &gc_vector_heap;
  eassume ((header_size % gc_heap_nr_bytes_per_slot (h)) == 0);
  const ptrdiff_t header_nr_slots =
    header_size / gc_heap_nr_bytes_per_slot (h);
  eassume (sizeof (v->contents[0]) == gc_heap_nr_bytes_per_slot (h));
  gc_igscan_advance_nr_slots_test_dirty (scan, header_nr_slots, h);
  const ptrdiff_t vecsize = v->header.size;
  eassume (vecsize >= 0);
  for (ptrdiff_t i = 0; i < vecsize; ++i)
    if (gc_igscan_advance_nr_slots_test_dirty (scan, 1, h))
      scan_reference (&v->contents[i], phase);
  return true;  /* We handled this object.  */
}

struct Lisp_Vector *
vector_from_large_vector (struct large_vector *p)
{
  return (struct Lisp_Vector *) &p->u.header;
}

struct large_vector *
large_vector_from_vectorlike (const union vectorlike_header *const v)
{
  struct large_vector *const lv =
    econtainer_of (v, struct large_vector, u.header);
#ifdef ENABLE_CHECKING
  eassert (lv->magic == large_vector_magic);
#else
  (void) large_vector_magic;
#endif
  return lv;
}

void
init_vectors (void)
{
  zero_vector = make_nil_vector (0);
  staticpro (&zero_vector);
}

ptrdiff_t
vectorlike_payload_nr_words (const union vectorlike_header *const hdr)
{
  const ptrdiff_t size = hdr->size;
  if (!(size & PSEUDOVECTOR_FLAG))
    return size;
  if (PSEUDOVECTOR_TYPEP (hdr, PVEC_BOOL_VECTOR))
    {
      struct Lisp_Bool_Vector *bv = (struct Lisp_Bool_Vector *) hdr;
      ptrdiff_t word_bytes = (bool_vector_words (bv->size)
                              * sizeof (bits_word));
      ptrdiff_t boolvec_bytes = bool_header_size + word_bytes;
      verify (header_size <= bool_header_size);
      return (boolvec_bytes - header_size + word_size - 1) / word_size;
    }
  if (PSEUDOVECTOR_TYPEP (hdr, PVEC_STRING_DATA))
    {
      eassume ((size & PSEUDOVECTOR_SIZE_MASK) == 0);
      const ptrdiff_t nr_words =
        (size & PSEUDOVECTOR_REST_MASK) >> PSEUDOVECTOR_SIZE_BITS;
      if (nr_words == 0)
        {
          const sdata *const sd = (const sdata *) hdr;
          return sd->u.large.len;
        }
    }
  return ((size & PSEUDOVECTOR_SIZE_MASK)
          + ((size & PSEUDOVECTOR_REST_MASK)
             >> PSEUDOVECTOR_SIZE_BITS));
}

/* Return the memory footprint of V in bytes.  */
ptrdiff_t
vectorlike_nbytes (const union vectorlike_header *hdr)
{
  const ptrdiff_t nr_words = vectorlike_payload_nr_words (hdr);
  eassume (nr_words <= VECTOR_ELTS_MAX);
  return header_size + word_size * nr_words;
}

void
bignum_cleanup (struct Lisp_Bignum *const p)
{
  mpz_clear (p->value);
}

void
finalizer_cleanup (struct Lisp_Finalizer *const finalizer)
{
  eassert (NILP (finalizer->function));
  eassert (NILP (finalizer->next));
}

void
font_cleanup (struct font *const font)
{
  /* Different object types use PVEC_FONT for some reason; we care
     only about actual struct font objects.  */
  if ((font->header.size & PSEUDOVECTOR_SIZE_MASK) != FONT_OBJECT_MAX)
    return;

  struct font_driver const *drv = font->driver;

  /* The font driver might sometimes be NULL, e.g. if Emacs was
     interrupted before it had time to set it up.  */
  if (drv)
    {
      /* Attempt to catch subtle bugs like Bug#16140.  */
      eassert (valid_font_driver (drv));
      drv->close_font (font);
    }
}

void
marker_cleanup (struct Lisp_Marker *const marker)
{
  /* clear_weak_marker_chains() should have unchained this marker from
     its buffer already.  */
  eassume (!marker->buffer);
}

void
user_ptr_cleanup (struct Lisp_User_Ptr *const uptr)
{
  if (uptr->finalizer)
    uptr->finalizer (uptr->p);
}

void
vector_cleanup (void *const p)
{
  switch (PSEUDOVECTOR_TYPE ((struct Lisp_Vector *) p))
    {
    case PVEC_BOOL_VECTOR:
      return;
    case PVEC_BUFFER:
      buffer_cleanup (p);
      return;
    case PVEC_CHAR_TABLE:
    case PVEC_COMPILED:
    case PVEC_FRAME:
    case PVEC_HASH_TABLE:
    case PVEC_MISC_PTR:
    case PVEC_NORMAL_VECTOR:
    case PVEC_OTHER:
    case PVEC_OVERLAY:
    case PVEC_PROCESS:
    case PVEC_RECORD:
    case PVEC_SUBR:
    case PVEC_SUB_CHAR_TABLE:
    case PVEC_TERMINAL:
    case PVEC_WINDOW:
    case PVEC_WINDOW_CONFIGURATION:
    case PVEC_XWIDGET:
    case PVEC_XWIDGET_VIEW:
    case PVEC_STRING_DATA:
      return;
    case PVEC_BIGNUM:
      bignum_cleanup (p);
      return;
    case PVEC_FINALIZER:
      finalizer_cleanup (p);
      return;
    case PVEC_FONT:
      font_cleanup (p);
      return;
    case PVEC_THREAD:
      finalize_one_thread (p);
      return;
    case PVEC_NATIVE_COMP_UNIT:
      //TODO: finalizenative comp
      return;
    case PVEC_MUTEX:
      finalize_one_mutex (p);
      return;
    case PVEC_CONDVAR:
      finalize_one_condvar (p);
      return;
    case PVEC_MARKER:
      marker_cleanup (p);
      return;
    case PVEC_USER_PTR:
      user_ptr_cleanup (p);
      return;
    case PVEC_MODULE_FUNCTION:
      module_finalize_function (p);
      return;
    }
  emacs_unreachable ();
}

large_vector *
large_vector_from_meta (const large_vector_meta *const lvm)
{
  return lvm->mem.start;
}

void
sweep_one_large_vector (large_vector_meta *const lvm)
{
  large_vector *const lv = large_vector_from_meta (lvm);
  bool normal_scan = true;
  if (!lvm->gen_o)
    {
      /* Note that this vector has survived a GC.  Leave the mark flag
         set: we'll clear it at the start of the next major GC.
         If the next GC is a minor one, the old generation's vectors
         being pre-marked is a feature, not a bug.  */
      lvm->gen_o = true;
    }
  else
    {
      /* This vector is part of the old generation.  Scan only the
         parts of the vector that the card table tells us have been
         modified.  */
      eassume (!gc_hot.major);
    }

  if (normal_scan)
    scan_vectorlike (&vector_from_large_vector (lv)->header, GC_PHASE_SWEEP);
}

void
sweep_large_vectors (void)
{
  large_vector_meta *lvm, **lvmprev = &large_vectors;
  for (lvm = large_vectors; lvm; lvm = *lvmprev)
    if (lvm->marked)
      {
        sweep_one_large_vector (lvm);
        lvmprev = &lvm->next;
      }
    else
      {
        *lvmprev = lvm->next;
        //large_vector_meta_free (lvm);
      }
}

/* Allocate a large vector; raise on failure. Place the allocated
  vector to the list of large vectors.  */
gc_vector_allocation
large_vector_allocate (const size_t nbytes, const bool clearit)
{
  eassume (nbytes >= large_vector_min_nr_bytes);
  const size_t large_vector_offset =
    offsetof (struct large_vector, u.header);
  eassert (!INT_ADD_OVERFLOW (nbytes, large_vector_offset));
  const size_t total_nr_bytes = nbytes + large_vector_offset;
  tally_consing_maybe_garbage_collect (total_nr_bytes);
  struct large_vector *const lv = lisp_malloc (
    total_nr_bytes,
    clearit, MEM_TYPE_LARGE_VECTOR,
    offsetof (struct large_vector_meta, mem));
  if (!gc_object_limit_try_increase (1))
    {
      lisp_free (lv, offsetof (struct large_vector_meta, mem));
      memory_full (gc_aux_block_nr_bytes);
    }
#ifdef ENABLE_CHECKING
  lv->magic = large_vector_magic;
#endif
  if (!clearit)
    lv->meta.marked = false;
  lv->meta.next = large_vectors;
  large_vectors = lv;
  return (gc_vector_allocation){
    .u.large = {
      .is_zero = clearit,
      .v = vector_from_large_vector (lv),
    },
    .is_large = true,
  };
}

void
large_vector_free (struct large_vector *const lv)
{
  eassume (lv);
#ifdef ENABLE_CHECKING
  eassume (lv->magic == large_vector_magic);
  lv->magic = 0;
#endif
  gc_global_object_limit_decrease (1);
  lisp_free (lv, offsetof (struct large_vector_meta, mem));
}

gc_vector_allocation
small_vector_allocate (const size_t nbytes, const bool clearit)
{
  eassume (nbytes < large_vector_min_nr_bytes);
  verify (NIL_IS_ZERO);
  gc_allocation r = clearit
    ? gc_heap_allocate_and_zero (&gc_vector_heap, nbytes)
    : gc_heap_allocate (&gc_vector_heap, nbytes);
  eassert (!clearit || r.is_zero);
  return (gc_vector_allocation){
    .u.small.r = r,
    .is_large = false,
  };
}

bool
gc_vector_allocation_is_zero (const gc_vector_allocation vr)
{
  return vr.is_large ? vr.u.large.is_zero : vr.u.small.r.is_zero;
}

struct Lisp_Vector *
gc_vector_allocation_vector (const gc_vector_allocation vr)
{
  return vr.is_large
    ? vr.u.large.v
    : gc_cursor_to_object (vr.u.small.r.obj_c, &gc_vector_heap);
}

Lisp_Object
gc_vector_allocation_lv (const gc_vector_allocation vr)
{
  return make_lisp_ptr (gc_vector_allocation_vector (vr),
                        Lisp_Vectorlike);
}

Lisp_Object
gc_vector_allocation_commit (gc_vector_allocation *const vr)
{
  if (!vr->is_large)
    gc_allocation_commit (&vr->u.small.r, &gc_vector_heap);
  return gc_vector_allocation_lv (*vr);
}

/* The lowest-level vector allocation routine.  LEN must be positive
   and at most VECTOR_ELTS_MAX.  Return a vector allocation result
   structure.  The caller must commit the allocation using
   gc_vector_allocation_commit() to inform the GC that object
   creation is complete, and Emacs must not GC before this call.  (See
   the note in gc_heap_allocate()).  */
gc_vector_allocation
allocate_vectorlike (const ptrdiff_t len, const bool clearit)
{
  eassume (0 <= len && len <= VECTOR_ELTS_MAX);
  const size_t nbytes = header_size + len * word_size;
  gc_vector_allocation rv = nbytes < large_vector_min_nr_bytes
    ? small_vector_allocate (nbytes, clearit)
    : large_vector_allocate (nbytes, clearit);
  gc_vector_allocation_vector (rv)->header.size = len;
  eassert (!clearit || gc_vector_allocation_is_zero (rv));
  return rv;
}

gc_vector_allocation
larger_vecalloc (const struct Lisp_Vector *const vec,
                 const ptrdiff_t incr_min,
                 const ptrdiff_t nitems_max)
{
  const ptrdiff_t C_language_max =
    min (PTRDIFF_MAX, SIZE_MAX) / sizeof vec->contents[0];
  const ptrdiff_t n_max =
    (0 <= nitems_max && nitems_max < C_language_max
     ? nitems_max : C_language_max);
  eassert (0 < incr_min && -1 <= nitems_max);
  const ptrdiff_t old_size = vec->header.size;
  const ptrdiff_t incr_max = n_max - old_size;
  const ptrdiff_t incr = max (incr_min, min (old_size >> 1, incr_max));
  if (incr_max < incr)
    memory_full (SIZE_MAX);
  const ptrdiff_t new_size = old_size + incr;
  gc_vector_allocation vr =
    allocate_vectorlike (new_size, /*clearit=*/false);
  memcpy (gc_vector_allocation_vector (vr)->contents,
          vec->contents,
          old_size * sizeof vec->contents[0]);
  return vr;
}

Lisp_Object
larger_vector (const Lisp_Object vec,
               const ptrdiff_t incr_min,
               const ptrdiff_t nitems_max)
{
  const ptrdiff_t old_size = ASIZE (vec);
  gc_vector_allocation vr =
    larger_vecalloc (XVECTOR (vec), incr_min, nitems_max);
  struct Lisp_Vector *const v =
    gc_vector_allocation_vector (vr);
  const ptrdiff_t new_size = vectorlike_lisp_size (&v->header);
  memclear (v->contents + old_size, (new_size - old_size) * word_size);
  return gc_vector_allocation_commit (&vr);
}

/* Allocate a vector with LEN nil slots.  */
Lisp_Object
make_nil_vector (ptrdiff_t len)
{
  verify (NIL_IS_ZERO);
  gc_vector_allocation vr = allocate_vectorlike (len, /*clearit=*/true);
  eassume (gc_vector_allocation_is_zero (vr));
  return gc_vector_allocation_commit (&vr);
}

gc_vector_allocation
allocate_pseudovector (const int memlen,
                       const int lisplen,
                       const enum pvec_type tag,
                       bool clearit)
{
  /* Catch bogus values.  */
  enum { size_max = (1 << PSEUDOVECTOR_SIZE_BITS) - 1 };
  enum { rest_max = (1 << PSEUDOVECTOR_REST_BITS) - 1 };
  verify (size_max + rest_max <= VECTOR_ELTS_MAX);
  eassume (0 <= tag && tag <= PVEC_FONT);
  eassume (0 <= lisplen && lisplen <= memlen);
  eassume (lisplen <= size_max);
  eassume (memlen <= size_max + rest_max);
  gc_vector_allocation vr = allocate_vectorlike (memlen, clearit);
  /* Only the first LISPLEN slots will be traced normally by the GC.  */
  XSETPVECTYPESIZE (gc_vector_allocation_vector (vr),
                    tag, lisplen, memlen - lisplen);
  return vr;
}

void *
allocate_pseudovector_and_zero (int memlen,
                                int lisplen,
                                enum pvec_type tag)
{
  gc_vector_allocation vr =
    allocate_pseudovector (memlen, lisplen, tag, /*clearit=*/true);
  eassume (gc_vector_allocation_is_zero (vr));
  gc_vector_allocation_commit (&vr);
  return gc_vector_allocation_vector (vr);
}

/* Allocate a new empty buffer object, but do _not_ put it on the list
   of all buffers.  We do that only once the buffer is fully
   initialized.  */
struct buffer *
allocate_buffer (void)
{
  struct buffer *const b
    = ALLOCATE_PSEUDOVECTOR_AND_ZERO (
      struct buffer, cursor_in_non_selected_windows_, PVEC_BUFFER);
  if (enable_checking)
    {
      const ptrdiff_t old_header = b->header.size;
      BUFFER_PVEC_INIT (b);
      eassume (old_header == b->header.size);
    }
  return b;
}

/* Allocate a record with COUNT slots.  COUNT must be positive, and
   includes the type slot.  */
gc_vector_allocation
allocate_record (const EMACS_INT count)
{
  if (count > PSEUDOVECTOR_SIZE_MASK)
    error ("Attempt to allocate a record of %"pI"d slots; max is %d",
	   count, PSEUDOVECTOR_SIZE_MASK);
  return allocate_pseudovector (
    count, count, PVEC_RECORD, /*clearit=*/false);
}

DEFUN ("make-record", Fmake_record, Smake_record, 3, 3, 0,
       doc: /* Create a new record.
TYPE is its type as returned by `type-of'; it should be either a
symbol or a type descriptor.  SLOTS is the number of non-type slots,
each initialized to INIT.  */)
  (Lisp_Object type, Lisp_Object slots, Lisp_Object init)
{
  CHECK_FIXNAT (slots);
  const EMACS_INT size = XFIXNAT (slots) + 1;
  gc_vector_allocation vr = allocate_record (size);
  struct Lisp_Vector *const v = gc_vector_allocation_vector (vr);
  v->contents[0] = type;
  for (ptrdiff_t i = 1; i < size; i++)
    v->contents[i] = init;
  return gc_vector_allocation_commit (&vr);
}

DEFUN ("record", Frecord, Srecord, 1, MANY, 0,
       doc: /* Create a new record.
TYPE is its type as returned by `type-of'; it should be either a
symbol or a type descriptor.  SLOTS is used to initialize the record
slots with shallow copies of the arguments.
usage: (record TYPE &rest SLOTS) */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  gc_vector_allocation vr = allocate_record (nargs);
  memcpy (gc_vector_allocation_vector (vr)->contents,
          args, nargs * sizeof *args);
  return gc_vector_allocation_commit (&vr);
}


DEFUN ("make-vector", Fmake_vector, Smake_vector, 2, 2, 0,
       doc: /* Return a newly created vector of length LENGTH, with each element being INIT.
See also the function `vector'.  */)
  (Lisp_Object length, Lisp_Object init)
{
  CHECK_TYPE (FIXNATP (length) && XFIXNAT (length) <= PTRDIFF_MAX,
	      Qwholenump, length);
  return make_vector (XFIXNAT (length), init);
}

/* Return a new vector of length LENGTH with each element being INIT.  */
Lisp_Object
make_vector (ptrdiff_t length, Lisp_Object init)
{
  if (NILP (init))
    return make_nil_vector (length);
  gc_vector_allocation vr =
    allocate_vectorlike (length, /*clearit=*/false);
  struct Lisp_Vector *const v = gc_vector_allocation_vector (vr);
  for (ptrdiff_t i = 0; i < length; i++)
    v->contents[i] = init;
  return gc_vector_allocation_commit (&vr);
}

DEFUN ("vector", Fvector, Svector, 0, MANY, 0,
       doc: /* Return a newly created vector with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (vector &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  gc_vector_allocation vr =
    allocate_vectorlike (nargs, /*clearit=*/false);
  struct Lisp_Vector *const v = gc_vector_allocation_vector (vr);
  memcpy (v->contents, args, nargs * sizeof *args);
  return gc_vector_allocation_commit (&vr);
}

DEFUN ("make-byte-code", Fmake_byte_code, Smake_byte_code, 4, MANY, 0,
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
  /* We used to purecopy everything here, if purify-flag was set.  This worked
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
  Lisp_Object constvec = make_nil_vector (constsize);
  memcpy (XVECTOR (constvec)->contents, args + 1, nvars * word_size);
  memcpy (XVECTOR (constvec)->contents + nvars,
	  XVECTOR (proto_constvec)->contents + nvars,
	  (constsize - nvars) * word_size);

  /* Return a copy of the prototype function with the new constant vector. */
  ptrdiff_t protosize = PVSIZE (protofun);
  //struct Lisp_Vector *v
  gc_vector_allocation vr = allocate_vectorlike (protosize, false);
  struct Lisp_Vector *const v = gc_vector_allocation_vector (vr);

  v->header = XVECTOR (protofun)->header;
  memcpy (v->contents, XVECTOR (protofun)->contents, protosize * word_size);
  v->contents[COMPILED_CONSTANTS] = constvec;
  return make_lisp_ptr (v, Lisp_Vectorlike);
}


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
make_uninit_bool_vector (const EMACS_INT nbits)
{
  const EMACS_INT words = bool_vector_words (nbits);
  const EMACS_INT word_bytes = words * sizeof (bits_word);
  const EMACS_INT needed_elements =
    ((bool_header_size - header_size + word_bytes
      + word_size - 1)
     / word_size);
  if (PTRDIFF_MAX < needed_elements)
    memory_full (SIZE_MAX);

  /* It's safe to leave the bool vector's contents uninitialized: GC
     doesn't scan them.  */
  gc_vector_allocation vr =
    allocate_vectorlike (needed_elements, /*clearit=*/false);
  struct Lisp_Vector *const v = gc_vector_allocation_vector (vr);
  XSETPVECTYPESIZE (v, PVEC_BOOL_VECTOR, 0, 0);
  struct Lisp_Bool_Vector *const p = (struct Lisp_Bool_Vector *) v;
  p->size = nbits;
  /* Clear padding at the end.  */
  if (words)
    p->data[words - 1] = 0;
  return gc_vector_allocation_commit (&vr);
}

DEFUN ("make-bool-vector", Fmake_bool_vector, Smake_bool_vector, 2, 2, 0,
       doc: /* Return a new bool-vector of length LENGTH, using INIT for each element.
LENGTH must be a number.  INIT matters only in whether it is t or nil.  */)
  (Lisp_Object length, Lisp_Object init)
{
  Lisp_Object val;

  CHECK_FIXNAT (length);
  val = make_uninit_bool_vector (XFIXNAT (length));
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

  vector = make_uninit_bool_vector (nargs);
  for (i = 0; i < nargs; i++)
    bool_vector_set (vector, i, !NILP (args[i]));

  return vector;
}

/* If P is a pointer to a live vector-like object, return the object.
   Otherwise, return nil.
   M is a pointer to the mem_block for P.  */
Lisp_Object
live_vector_holding (const struct mem_node *m, const void *p)
{
  if (m->type == MEM_TYPE_VECTOR)
    {
      gc_block *const b = gc_block_from_mem_node (m);
      struct Lisp_String *const v =
        gc_block_maybe_find_live_object_containing (b, p, &gc_vector_heap);
      return v ? make_lisp_ptr (v, Lisp_Vectorlike) : Qnil;
    }
  if (m->type == MEM_TYPE_LARGE_VECTOR)
    {
      /* Ugh: just change the memory bounds to the data portion of the
         large vector block.  */
      const uintptr_t mem_start_la = (uintptr_t) m->start;
      const uintptr_t mem_end_la = (uintptr_t) m->end;
      const uintptr_t p_la = (uintptr_t) p;

      /* If this range check didn't hold, we wouldn't have gotten here
         in the first place.  */
      eassume (mem_start_la <= p_la && p_la < mem_end_la);
      const uintptr_t large_vector_offset =
        offsetof (struct large_vector, u.header);
      if (p_la - mem_start_la < large_vector_offset)
        return Qnil;
      /* XXX: adjust for LV mem_tree */
      const struct Lisp_Vector *const v =
        (struct Lisp_Vector *)(mem_start_la + large_vector_offset);
      const struct large_vector *const lv =
        large_vector_from_vectorlike (&v->header);
      eassert ((char *) lv == (char *) mem_start_la);
      return make_lisp_ptr (v, Lisp_Vectorlike);
    }
  return Qnil;
}

bool
live_vector_p (const struct mem_node *const m, const void *p)
{
  return !NILP (live_vector_holding (m, p));
}

size_t
gc_vector_object_nr_bytes (const void *const vptr)
{
  return vectorlike_nbytes (vptr);
}


/***********************************************************************
			   Symbol Allocation
 ***********************************************************************/

static gc_heap_data gc_symbol_heap_data;
static const gc_heap gc_symbol_heap = {
  .data = &gc_symbol_heap_data,
  .heap_symbol_index = iQsymbols,
  .mem_type = MEM_TYPE_SYMBOL,
  .lisp_type = Lisp_Symbol,
  .aligned_blocks = true,
  .use_moving_gc = true,
  .homogeneous_object_nr_bytes = gc_heap_symbol_slot_size,
  .cleanup = symbol_cleanup,
  GC_HEAP_BITS_CONFIG (symbol),
  CONFIG_STANDARD_HEAP_FUNCTIONS (gc_symbol_heap),
};

DEFINE_STANDARD_HEAP_FUNCTIONS (gc_symbol_heap);

/* Mark and maybe scan a symbol.  */
void
gc_mark_or_enqueue_symbol (struct Lisp_Symbol *s)
{
  bool checked_stack_depth = false;
  do {
    if (symbol_marked_p (s))
      return;  /* Already on mark queue or marked.  */
    set_symbol_marked (s);
    if (!checked_stack_depth)
      {
	checked_stack_depth = true;
	ENQUEUE_AND_RETURN_IF_TOO_DEEP (make_lisp_symbol (s));
      }
    scan_symbol (s, GC_PHASE_MARK);
    /* Try marking the rest of the symbol chain.  We don't need to check
       the stack depth again.  */
    s = s->u.s.next;
  } while(s);
    /* if (s->u.s.next) */
    /*   { */
    /* 	goto again; */
    /*   } */
}

void
init_symbol (Lisp_Object val, Lisp_Object name)
{
  struct Lisp_Symbol *p = XSYMBOL (val);
  p->u.s.name = name;
  set_symbol_plist (val, Qnil);
  p->u.s.f.redirect = SYMBOL_PLAINVAL;
  SET_SYMBOL_VAL (p, Qunbound);
  set_symbol_function (val, Qnil);
  set_symbol_next (val, NULL);
  p->u.s.f.interned = SYMBOL_UNINTERNED;
  p->u.s.f.trapped_write = SYMBOL_UNTRAPPED_WRITE;
  p->u.s.f.declared_special = false;
  p->u.s.f.identity_hash_code = (uintptr_t) p / alignof (struct Lisp_Symbol);
}

DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
       doc: /* Return a newly allocated uninterned symbol whose name is NAME.
Its value is void, and its function definition and property list are nil.  */)
  (Lisp_Object name)
{
  CHECK_STRING (name);
  gc_allocation r =
    gc_heap_allocate (&gc_symbol_heap, gc_heap_symbol_slot_size);
  struct Lisp_Symbol *const s =
    gc_cursor_to_object (r.obj_c, &gc_symbol_heap);
  const Lisp_Object val = make_lisp_symbol (s);
  init_symbol (val, name);
  gc_allocation_commit (&r, &gc_symbol_heap);
  return val;
}

bool
symbol_marked_p (const struct Lisp_Symbol *s)
{
  return gc_pdumper_object_p (s)
    ? pdumper_marked_p (s)
    : c_symbol_p (s)
    ? true /* C symbols are always marked */
    : gc_object_is_marked (s, &gc_symbol_heap);
}

void
set_symbol_marked (struct Lisp_Symbol *s)
{
  if (gc_pdumper_object_p (s))
    pdumper_set_marked (s);
  else if (c_symbol_p (s))
    emacs_unreachable ();
  else
    gc_object_set_marked (s, &gc_symbol_heap);
}

struct Lisp_Symbol *
point_symbol_into_tospace (struct Lisp_Symbol *const s)
{
  if (gc_pdumper_object_p (s) || c_symbol_p (s))
    return s;
  return gc_object_point_into_tospace (s, &gc_symbol_heap);
}

void
set_symbol_pinned (struct Lisp_Symbol *s)
{
  if (!gc_pdumper_object_p (s) && !c_symbol_p (s))
    gc_object_set_pinned (s, &gc_symbol_heap);
}

/* If P is a pointer into a live Lisp symbol object on the heap,
   return the object.  Otherwise, return nil.  M is a pointer to the
   mem_block for P.  */
Lisp_Object
live_symbol_holding (const struct mem_node *const m, const void *const p)
{
  if (!m || m->type != MEM_TYPE_SYMBOL)
    return Qnil;
  gc_block *const b = gc_block_from_mem_node (m);
  struct Lisp_Symbol *const s =
    gc_block_maybe_find_live_object_containing (b, p, &gc_symbol_heap);
  return s ? make_lisp_symbol (s) : Qnil;
}

bool
live_symbol_p (const struct mem_node *const m, const void *const p)
{
  return !NILP (live_symbol_holding (m, p));
}

void
symbol_cleanup (void *const p)
{
  struct Lisp_Symbol *const sym = p;
  if (sym->u.s.f.redirect == SYMBOL_LOCALIZED)
    xfree (SYMBOL_BLV (sym));
}

void
scan_localized_symbol (struct Lisp_Symbol *const ptr,
                       const gc_phase phase)
{
  struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (ptr);
  /* If the value is set up for a killed buffer restore its global binding.  */
  eassume (phase == current_gc_phase);
  if (phase == GC_PHASE_MARK &&
      (BUFFERP (blv->where) && !BUFFER_LIVE_P (XBUFFER (blv->where))))
    swap_in_global_binding (ptr);
  scan_reference (&blv->where, phase);
  scan_reference (&blv->valcell, phase);
  scan_reference (&blv->defcell, phase);
}

void
scan_symbol (struct Lisp_Symbol *const ptr, const gc_phase phase)
{
  scan_reference (&ptr->u.s.function, phase);
  scan_reference (&ptr->u.s.plist, phase);
  scan_reference (&ptr->u.s.name, phase);
  scan_reference_pointer_to_symbol (&ptr->u.s.next, phase);

  switch (ptr->u.s.f.redirect)
    {
    case SYMBOL_PLAINVAL:
      scan_reference (SYMBOL_VALP (ptr), phase);
      return;
    case SYMBOL_VARALIAS:
      scan_reference_pointer_to_symbol (SYMBOL_ALIASP (ptr), phase);
      return;
    case SYMBOL_LOCALIZED:
      scan_localized_symbol (ptr, phase);
      return;
    case SYMBOL_FORWARDED:
      /* If the value is forwarded to a buffer or keyboard field,
         these are marked when we see the corresponding object.
         And if it's forwarded to a C variable, either it's not a
         Lisp_Object var, or it's staticpro'd already.  We don't need
         to scan the forward field for compacting GC either, since all
         of the objects to which a forwarded symbol can point are
         immobile.  */
      return;
    }
  emacs_unreachable ();
}




Lisp_Object
make_misc_ptr (void *a)
{
  gc_vector_allocation vr;
  struct Lisp_Misc_Ptr *const p =
    UNSAFE_ALLOCATE_PLAIN_PSEUDOVECTOR_UNINIT (
      &vr, struct Lisp_Misc_Ptr, PVEC_MISC_PTR);
  p->pointer = a;
  return gc_vector_allocation_commit (&vr);
}

/* Return a new overlay with specified START, END and PLIST.  */

Lisp_Object
build_overlay (Lisp_Object start, Lisp_Object end, Lisp_Object plist)
{
  gc_vector_allocation vr;
  struct Lisp_Overlay *const p =
    UNSAFE_ALLOCATE_PSEUDOVECTOR_UNINIT (
      &vr, struct Lisp_Overlay, plist, PVEC_OVERLAY);
  p->start = start;
  p->end = end;
  p->plist = plist;
  p->next = NULL;
  return gc_vector_allocation_commit (&vr);
}

DEFUN ("make-marker", Fmake_marker, Smake_marker, 0, 0, 0,
       doc: /* Return a newly allocated marker which does not point at any place.  */)
  (void)
{
  struct Lisp_Marker *const p =
    ALLOCATE_PLAIN_PSEUDOVECTOR_AND_ZERO (struct Lisp_Marker, PVEC_MARKER);
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

  gc_vector_allocation vr;
  struct Lisp_Marker *m = UNSAFE_ALLOCATE_PLAIN_PSEUDOVECTOR_UNINIT (
    &vr, struct Lisp_Marker, PVEC_MARKER);
  m->buffer = buf;
  m->charpos = charpos;
  m->bytepos = bytepos;
  m->insertion_type = 0;
  m->need_adjustment = 0;
  m->next = BUF_MARKERS (buf);
  BUF_MARKERS (buf) = m;
  return gc_vector_allocation_commit (&vr);
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
    = ALLOCATE_PLAIN_PSEUDOVECTOR_AND_ZERO (
      struct Lisp_User_Ptr, PVEC_USER_PTR);
  uptr->finalizer = finalizer;
  uptr->p = p;
  return make_lisp_ptr (uptr, Lisp_Vectorlike);
}
#endif

Lisp_Object
run_finalizer_handler (const Lisp_Object args)
{
  add_to_log ("finalizer failed: %S", args);
  return Qnil;
}

void
run_finalizer_function (const Lisp_Object function)
{
  specpdl_ref count = SPECPDL_INDEX ();
#ifdef HAVE_PDUMPER
  ++number_finalizers_run;
#endif

  specbind (Qinhibit_quit, Qt);
  internal_condition_case_1 (call0, function, Qt, run_finalizer_handler);
  unbind_to (count, Qnil);
}

void
run_doomed_finalizers (void)
{
  while (!NILP (doomed_finalizers))
    {
      struct Lisp_Finalizer *finalizer = XFINALIZER (doomed_finalizers);
      const Lisp_Object function = finalizer->function;
      eassert (!NILP (function));
      finalizer->function = Qnil;
      doomed_finalizers = finalizer->next;
      run_finalizer_function (function);
    }
}

DEFUN ("make-finalizer", Fmake_finalizer, Smake_finalizer, 1, 1, 0,
       doc: /* Make a finalizer that will run FUNCTION.
FUNCTION will be called after garbage collection when the returned
finalizer object becomes unreachable.  If the finalizer object is
reachable only through references from finalizer objects, it does not
count as reachable for the purpose of deciding whether to run
FUNCTION.  FUNCTION will be run exactly once per finalizer object.  */)
  (Lisp_Object function)
{
  struct Lisp_Finalizer *finalizer =
    ALLOCATE_PSEUDOVECTOR_AND_ZERO (
      struct Lisp_Finalizer, function, PVEC_FINALIZER);
  finalizer->function = function;
  const Lisp_Object fl = make_lisp_ptr (finalizer, Lisp_Vectorlike);
  if (!NILP (function))  /* nil function means not on a list */
    {
      finalizer->next = finalizers;
      finalizers = fl;
    }
  return fl;
}

/* With the rare exception of functions implementing block-based
   allocation of various types, you should not directly test or set GC
   mark bits on objects.  Some objects might live in special memory
   regions (e.g., a dump image) and might store their mark bits
   elsewhere.  */

bool
vectorlike_marked_p (const union vectorlike_header *const v)
{
  if (gc_pdumper_object_p (v))
    {
      /* Look at cold_start first so that we don't have to fault in
         the vector header just to tell that it's a bool vector.  */
      if (pdumper_cold_object_p (v))
        {
          eassert (PSEUDOVECTOR_TYPEP (v, PVEC_BOOL_VECTOR) ||
                   PSEUDOVECTOR_TYPEP (v, PVEC_STRING_DATA));
          return true;
        }
      return pdumper_marked_p (v);
    }
  if (PVTYPE (v) == PVEC_SUBR)
    return true;
  if (PVTYPE (v) == PVEC_THREAD && main_thread_p (v))
    return true;
  if (vectorlike_nbytes (v) >= large_vector_min_nr_bytes)
    return large_vector_from_vectorlike (v)->meta.marked;
  return gc_object_is_marked (v, &gc_vector_heap);
}

void
set_vectorlike_marked (union vectorlike_header *const v)
{
  eassert (!PSEUDOVECTOR_TYPEP (v, PVEC_SUBR));
  if (gc_pdumper_object_p (v))
    {
      eassert (!PSEUDOVECTOR_TYPEP (v, PVEC_BOOL_VECTOR));
      pdumper_set_marked (v);
    }
  else if (vectorlike_nbytes (v) >= large_vector_min_nr_bytes)
    large_vector_from_vectorlike (v)->meta.marked = true;
  else
    gc_object_set_marked (v, &gc_vector_heap);
}

bool
vectorlike_always_pinned_p (const union vectorlike_header *const v)
{
  /* FIXME: we shouldn't have to read the heap here.  */
  return gc_pdumper_object_p (v) ||
    PSEUDOVECTOR_TYPEP (v, PVEC_SUBR) ||
    vectorlike_nbytes (v) >= large_vector_min_nr_bytes ||
    (PSEUDOVECTOR_TYPEP (v, PVEC_THREAD) && main_thread_p (v));
}

void
set_vectorlike_pinned (union vectorlike_header *const v)
{
  if (!vectorlike_always_pinned_p (v))
    gc_object_set_pinned (v, &gc_vector_heap);
}

EMACS_INT
vector_identity_hash_code (struct Lisp_Vector *const v)
{
  if (!vectorlike_always_pinned_p (&v->header))
    gc_object_perma_pin (v, &gc_vector_heap);
  return (uintptr_t) v / alignof (struct Lisp_Vector);
}

void *
point_vectorlike_into_tospace (union vectorlike_header *const v)
{
  if (vectorlike_always_pinned_p (v))
    return v;
  return gc_object_point_into_tospace (v, &gc_vector_heap);
}

/* Called if malloc (NBYTES) returns zero.  If NBYTES == SIZE_MAX,
   there may have been size_t overflow so that malloc was never
   called, or perhaps malloc was invoked successfully but the
   resulting pointer had problems fitting into a tagged EMACS_INT.  In
   either case this counts as memory being full even though malloc did
   not fail.  */

void
memory_full (size_t nbytes)
{
  if (! initialized)
    fatal ("memory exhausted");

  /* Do not go into hysterics merely because a large request failed.  */
  bool enough_free_memory = false;
  if (gc_spare_memory < nbytes)
    {
      void *p;

      p = malloc (gc_spare_memory);
      if (p)
	{
	  free (p);
	  enough_free_memory = true;
	}
    }

  if (! enough_free_memory)
    {
      Vmemory_full = Qt;
      /* The first time we get here, free the spare memory.  */
      free (spare_memory);
      spare_memory = NULL;
    }

  xsignal (Qnil, Vmemory_signal_data);
}

/* If we released our reserve (due to running out of memory),
   and we have a fair amount free once again,
   try to set aside another reserve in case we run out once more.

   This is called when a relocatable block is freed in ralloc.c,
   and also directly from this file, in case we're not using ralloc.c.  */

void
refill_memory_reserve (void)
{
  if (!spare_memory)
    spare_memory = malloc (gc_spare_memory);
}

/* Conservative C stack marking requires a method to identify possibly
   live Lisp objects given a pointer value.  We do this by keeping
   track of blocks of Lisp data that are allocated in a red-black tree
   (see also the comment of mem_node which is the type of nodes in
   that tree).  Function lisp_malloc adds information for an allocated
   block to the red-black tree with calls to mem_insert, and function
   lisp_free removes it with mem_remove.  Functions live_string_p etc
   call mem_find to lookup information about a given pointer in the
   tree, and use that to determine if the pointer points into a Lisp
   object or not.  */

void
mem_start_modification (void)
{
  eassume (!mem_tree_being_modified);
  mem_tree_being_modified = true;
}

void
mem_end_modification (void)
{
  eassume (mem_tree_being_modified);
  mem_tree_being_modified = false;
}

/* Value is a pointer to the mem_node containing START.  Value is
   NULL if there is no node in the tree containing START.  */
struct mem_node *
mem_find (const void *const start)
{
  eassume (!mem_tree_being_modified);
  struct mem_node *p;

  if ((uintptr_t) start < heap_start || (uintptr_t) start >= heap_end)
    return NULL;

  p = mem_root;
  while (p && (start < p->start || start >= p->end))
    p = start < p->start ? p->left : p->right;
  return p;
}


/* Insert a new node into the tree for a block of memory with start
   address START, end address END, and type TYPE.  Value is a
   pointer to the node that was inserted.  */

void
mem_insert (struct mem_node *mem, void *start, void *end, enum mem_type type)
{
  eassume (mem_tree_being_modified);

  struct mem_node *c, *parent, *x;

  /* See where in the tree a node for START belongs.  In this
     particular application, it shouldn't happen that a node is already
     present.  For debugging purposes, let's check that.  */
  c = mem_root;
  parent = NULL;

  while (c != NULL)
    {
      parent = c;
      c = start < c->start ? c->left : c->right;
    }

  /* Create a new node.  */
  x = mem;
  x->start = start;
  x->end = end;
  x->type = type;
  x->parent = parent;
  x->left = x->right = NULL;
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

  if (heap_start == 0 || (uintptr_t) start < heap_start)
    heap_start = (uintptr_t) start;
  if (heap_end == 0 || (uintptr_t) end > heap_end)
    heap_end = (uintptr_t) end;
}


/* Re-establish the red-black properties of the tree, and thereby
   balance the tree, after node X has been inserted; X is always red.  */

void
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

	  if (y && y->color == MEM_RED)
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

	  if (y && y->color == MEM_RED)
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

void
mem_rotate_left (struct mem_node *x)
{
  struct mem_node *y;

  /* Turn y's left sub-tree into x's right sub-tree.  */
  y = x->right;
  x->right = y->left;
  if (y->left != NULL)
    y->left->parent = x;

  /* Y's parent was x's parent.  */
  if (y != NULL)
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
  if (x != NULL)
    x->parent = y;
}


/*     (x)                (Y)
       / \                / \
     (y)  c      ===>    a  (x)
     / \                    / \
    a   b                  b   c  */

void
mem_rotate_right (struct mem_node *x)
{
  struct mem_node *y = x->left;

  x->left = y->right;
  if (y->right != NULL)
    y->right->parent = x;

  if (y != NULL)
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
  if (x != NULL)
    x->parent = y;
}

struct mem_node *
mem_minimum (struct mem_node *x)
{
  while (x->left) x = x->left;
  return x;
}

struct mem_node *
mem_maximum (struct mem_node *x)
{
  while (x->right) x = x->right;
  return x;
}

/* Delete node Z from the tree.  Z must not be NULL.  */
void
mem_remove (struct mem_node *const z)
{
  eassume (mem_tree_being_modified);
  eassume (z);

  /* Code is from libstdc++ 9's tree.cc, lightly translated to Emacs.
     We use the libstdc++ code instead of the textbook RB tree removal
     approach because that textbook code sometimes retains node Z
     after removal, and we can't do that because our nodes are
     intrusive.  This code is extremely hairy, so we try to be very
     faithful to the libstdc++ original.  */
  struct mem_node* y = z;
  struct mem_node* x = NULL;
  struct mem_node* x_parent = NULL;

  if (y->left == NULL)     // z has at most one non-null child. y == z.
    x = y->right;     // x might be null.
  else
    if (y->right == NULL)  // z has exactly one non-null child. y == z.
      x = y->left;    // x is not null.
    else
      {
        // z has two non-null children.  Set y to
        y = y->right;   //   z's successor.  x might be null.
        while (y->left != NULL)
          y = y->left;
        x = y->right;
      }
  if (y != z)
    {
      // relink y in place of z.  y is z's successor
      z->left->parent = y;
      y->left = z->left;
      if (y != z->right)
        {
          x_parent = y->parent;
          if (x) x->parent = y->parent;
          y->parent->left = x;   // y must be a child of left
          y->right = z->right;
          z->right->parent = y;
        }
      else
        x_parent = y;
      if (mem_root == z)
        mem_root = y;
      else if (z->parent->left == z)
        z->parent->left = y;
      else
        z->parent->right = y;
      y->parent = z->parent;
      {
        int color = y->color;
        y->color = z->color;
        z->color = color;
      }
      y = z;
      // y now points to node to be actually deleted
    }
  else
    {                        // y == z
      x_parent = y->parent;
      if (x)
        x->parent = y->parent;
      if (mem_root == z)
        mem_root = x;
      else
        if (z->parent->left == z)
          z->parent->left = x;
        else
          z->parent->right = x;
      if (heap_start == (uintptr_t) z->start)
        {
          if (z->right == NULL)        // z->left must be null also
            heap_start = z->parent ? (uintptr_t) z->parent->start : 0;
          else
            heap_start = (uintptr_t) mem_minimum (x)->start;
        }
      if (heap_end == (uintptr_t) z->end)
        {
          if (z->left == NULL)         // z->right must be null also
            heap_end = z->parent ? (uintptr_t) z->parent->end : 0;
          else                      // x == z->left
            heap_end = (uintptr_t) mem_maximum (x)->end;
        }
    }
  if (y->color != MEM_RED)
    {
      while (x != mem_root && (x == NULL || x->color == MEM_BLACK))
        if (x == x_parent->left)
          {
            struct mem_node* w = x_parent->right;
            if (w->color == MEM_RED)
              {
                w->color = MEM_BLACK;
                x_parent->color = MEM_RED;
                mem_rotate_left (x_parent);
                w = x_parent->right;
              }
            if ((w->left == NULL ||
                 w->left->color == MEM_BLACK) &&
                (w->right == NULL ||
                 w->right->color == MEM_BLACK))
              {
                w->color = MEM_RED;
                x = x_parent;
                x_parent = x_parent->parent;
              }
            else
              {
                if (w->right == NULL
                    || w->right->color == MEM_BLACK)
                  {
                    w->left->color = MEM_BLACK;
                    w->color = MEM_RED;
                    mem_rotate_right(w);
                    w = x_parent->right;
                  }
                w->color = x_parent->color;
                x_parent->color = MEM_BLACK;
                if (w->right)
                  w->right->color = MEM_BLACK;
                mem_rotate_left(x_parent);
                break;
              }
          }
        else
          {
            // same as above, with right <-> left.
            struct mem_node* w = x_parent->left;
            if (w->color == MEM_RED)
              {
                w->color = MEM_BLACK;
                x_parent->color = MEM_RED;
                mem_rotate_right(x_parent);
                w = x_parent->left;
              }
            if ((w->right == NULL ||
                 w->right->color == MEM_BLACK) &&
                (w->left == NULL ||
                 w->left->color == MEM_BLACK))
              {
                w->color = MEM_RED;
                x = x_parent;
                x_parent = x_parent->parent;
              }
            else
              {
                if (w->left == NULL || w->left->color == MEM_BLACK)
                  {
                    w->right->color = MEM_BLACK;
                    w->color = MEM_RED;
                    mem_rotate_left(w);
                    w = x_parent->left;
                  }
                w->color = x_parent->color;
                x_parent->color = MEM_BLACK;
                if (w->left)
                  w->left->color = MEM_BLACK;
                mem_rotate_right(x_parent);
                break;
              }
          }
      if (x) x->color = MEM_BLACK;
    }
}

struct mem_node *
mem_node_from_struct (void *const b, const ptrdiff_t offset)
{
  return (struct mem_node *)((char *)b + offset);
}

/* If P is a pointer into a live Lisp string object on the heap,
   return the object.  Otherwise, return nil.  M is a pointer to the
   mem_block for P.  */
Lisp_Object
live_string_holding (const struct mem_node *m, const void *p)
{
  if (!m || m->type != MEM_TYPE_STRING)
    return Qnil;
  gc_block *const b = gc_block_from_mem_node (m);
  struct Lisp_String *const s =
    gc_block_maybe_find_live_object_containing (b, p, &gc_string_heap);
  return s ? make_lisp_ptr (s, Lisp_String) : Qnil;
}

bool
live_string_p (const struct mem_node *const m, const void *const p)
{
  return !NILP (live_string_holding (m, p));
}

/* Return VECTOR if P points within it, NULL otherwise.  */

void
scan_maybe_object (const Lisp_Object obj, const gc_phase phase)
{
#if USE_VALGRIND
  VALGRIND_MAKE_MEM_DEFINED (&obj, sizeof (obj));
#endif

  /* Mark P if it's an identifiable pdumper object, i.e., P falls
     within the dump file address range, and aligns with a reloc
     instance.

     FIXME: This code assumes that every reachable pdumper object
     is addressed either by a pointer to the object start, or by
     the same pointer with an LSB-style tag.  This assumption
     fails if a pdumper object is reachable only via machine
     addresses of non-initial object components.  Although such
     addressing is rare in machine code generated by C compilers
     from Emacs source code, it can occur in some cases.  To fix
     this problem, the pdumper code should grok non-initial
     addresses, as the non-pdumper code does.  */
  if (pdumper_object_p (p))
    {
      uintptr_t mask = VALMASK & UINTPTR_MAX;
      uintptr_t masked_p = (uintptr_t) p & mask;
      void *po = (void *) masked_p;
      char *cp = p;
      char *cpo = po;
      /* Don't use pdumper_object_p_precise here! It doesn't check the
         tag bits. OBJ here might be complete garbage, so we need to
         verify both the pointer and the tag.  */
      int type = pdumper_find_object_type (po);
      if (pdumper_valid_object_type_p (type)
	  && (! USE_LSB_TAG || p == po || cp - cpo == type))
	{
	  if (type == Lisp_Symbol)
	    mark_object (make_lisp_symbol (po));
	  else if (! symbol_only)
	    mark_object (make_lisp_ptr (po, type));
	}
      return;
    }

  struct mem_node *const m = mem_find (po);

  if (m != NULL)
    {
      bool mark_p = false;

      switch (XTYPE (obj))
	{
	case Lisp_String:
	  mark_p = EQ (obj, live_string_holding (m, po));
	  break;

	case Lisp_Cons:
	  mark_p = EQ (obj, live_cons_holding (m, po));
	  break;

	case Lisp_Symbol:
	  mark_p = EQ (obj, live_symbol_holding (m, po));
	  break;

	case Lisp_Float:
	  mark_p = EQ (obj, live_float_holding (m, po));
	  break;

	case Lisp_Vectorlike:
	  mark_p = (EQ (obj, live_vector_holding (m, po)));
	  break;

        case Lisp_Type_Unused0:
        case Lisp_Int0:
        case Lisp_Int1:
          break;
	}

      if (mark_p)
	scan_reference_pinned (obj, phase);
    }
}

void
xscan_maybe_objects (Lisp_Object const *array,
                     const ptrdiff_t nelts,
                     const gc_phase phase)
{
  if (phase != GC_PHASE_MARK)
    return;
  eassume (current_gc_phase == phase);
  for (Lisp_Object const *const lim = array + nelts; array < lim; array++)
    scan_maybe_object (*array, GC_PHASE_MARK);
}

/* If P points to Lisp data, mark that as live if it isn't already
   marked.  */
void
scan_maybe_pointer (void *const p, const gc_phase phase)
{
  if (phase != GC_PHASE_MARK)
    return;

  eassume (current_gc_phase == phase);
#ifdef USE_VALGRIND
  VALGRIND_MAKE_MEM_DEFINED (&p, sizeof (p));
#endif

  if (gc_pdumper_object_p (p))
    {
      /* XXX: fix pdumper interior pointer stuff */
      int type = pdumper_find_object_type (p);
      if (!pdumper_valid_object_type_p (type))
        {
          /* False alarm.  */
        }
      else if (type == Lisp_Int0)
        {
          /* See dump_interval_tree() in pdumper.c.  */
          scan_reference_interval_pinned (p, GC_PHASE_MARK);
        }
      else if (type == Lisp_Symbol)
        scan_reference_pinned (make_lisp_symbol (p), GC_PHASE_MARK);
      else
        scan_reference_pinned (make_lisp_ptr (p, type), GC_PHASE_MARK);
      /* See scan_maybe_object for why we can confidently return.  */
      return;
    }

  const struct mem_node *m = mem_find (p);
  if (m != NULL)
    {
      Lisp_Object obj = Qnil;

      switch (m->type)
	{
	case MEM_TYPE_CONS:
	  obj = live_cons_holding (m, p);
	  break;

	case MEM_TYPE_STRING:
	  obj = live_string_holding (m, p);
	  break;

	case MEM_TYPE_SYMBOL:
	  obj = live_symbol_holding (m, p);
	  break;

	case MEM_TYPE_FLOAT:
          obj = live_float_holding (m, p);
	  break;

	case MEM_TYPE_LARGE_VECTOR:
	case MEM_TYPE_VECTOR:
	  obj = live_vector_holding (m, p);
	  break;

        case MEM_TYPE_INTERVAL:
          scan_reference_interval_pinned (
            live_interval_holding (m, p), GC_PHASE_MARK);
          break;
	}

      if (!NILP (obj))
	scan_reference_pinned (obj, GC_PHASE_MARK);
    }
}


/* Alignment of pointer values.  Use alignof, as it sometimes returns
   a smaller alignment than GCC's __alignof__ and mark_memory might
   miss objects if __alignof__ were used.  */
#define GC_POINTER_ALIGNMENT alignof (void *)

/* Mark Lisp objects referenced from the address range START..END
   or END..START.  */

void ATTRIBUTE_NO_SANITIZE_ADDRESS
scan_memory (void const *start, void const *end, const gc_phase phase)
{
  if (phase != GC_PHASE_MARK)
    return;
  eassume (current_gc_phase == phase);

  char const *pp;

  /* Make START the pointer to the start of the memory region,
     if it isn't already.  */
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
      char *p = *(char *const *) pp;
      scan_maybe_pointer (p, phase);

      /* Unmask any struct Lisp_Symbol pointer that make_lisp_symbol
	 previously disguised by adding the address of 'lispsym'.
	 On a host with 32-bit pointers and 64-bit Lisp_Objects,
	 a Lisp_Object might be split into registers saved into
	 non-adjacent words and P might be the low-order word's value.  */
      p += (intptr_t) lispsym;
      scan_maybe_pointer (*(void *const *) pp, GC_PHASE_MARK);

      verify (alignof (Lisp_Object) % GC_POINTER_ALIGNMENT == 0);
      if (alignof (Lisp_Object) == GC_POINTER_ALIGNMENT
	  || (uintptr_t) pp % alignof (Lisp_Object) == 0)
	scan_maybe_object (*(Lisp_Object const *) pp, GC_PHASE_MARK);
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

#  define SETJMP_WILL_LIKELY_WORK "\
\n\
Emacs garbage collector has been changed to use conservative stack\n\
marking.  Emacs has determined that the method it uses to do the\n\
marking will likely work on your system, but this isn't sure.\n\
\n\
If you are a system-programmer, or can get the help of a local wizard\n\
who is, please take a look at the function mark_c_stack in alloc.c, and\n\
verify that the methods used are appropriate for your system.\n\
\n\
Please mail the result to <emacs-devel@gnu.org>.\n\
"

#  define SETJMP_WILL_NOT_WORK "\
\n\
Emacs garbage collector has been changed to use conservative stack\n\
marking.  Emacs has determined that the default method it uses to do the\n\
marking will not work on your system.  We will need a system-dependent\n\
solution for your system.\n\
\n\
Please take a look at the function mark_c_stack in alloc.c, and\n\
try to find a way to make it work on your system.\n\
\n\
Note that you may get false negatives, depending on the compiler.\n\
In particular, you need to use -O with GCC for this test.\n\
\n\
Please mail the result to <emacs-devel@gnu.org>.\n\
"


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
      /* Came here after the longjmp at the end of the function.

         If x == 1, the longjmp has restored the register to its
         value before the setjmp, and we can hope that setjmp
         saves all such registers in the jmp_buf, although that
	 isn't sure.

         For other values of X, either something really strange is
         taking place, or the setjmp just didn't save the register.  */

      if (x == 1)
	fputs (SETJMP_WILL_LIKELY_WORK, stderr);
      else
	{
	  fputs (SETJMP_WILL_NOT_WORK, stderr);
	  exit (1);
	}
    }

  ++longjmps_done;
  x = 2;
  if (longjmps_done == 1)
    sys_longjmp (jbuf, 1);
}
# endif /* ! GC_SETJMP_WORKS */
#endif /* ! HAVE___BUILTIN_UNWIND_INIT */

/* Force callee-saved registers and register windows onto the stack.
   Use the platform-defined __builtin_unwind_init if available,
   obviating the need for machine dependent methods.  */
#ifndef HAVE___BUILTIN_UNWIND_INIT
# ifdef __sparc__
   /* This trick flushes the register windows so that all the state of
      the process is contained in the stack.
      FreeBSD does not have a ta 3 handler, so handle it specially.
      FIXME: Code in the Boehm GC suggests flushing (with 'flushrs') is
      needed on ia64 too.  See mach_dep.c, where it also says inline
      assembler doesn't work with relevant proprietary compilers.  */
#  if defined __sparc64__ && defined __FreeBSD__
#   define __builtin_unwind_init() asm ("flushw")
#  else
#   define __builtin_unwind_init() asm ("ta 3")
#  endif
# else
#  define __builtin_unwind_init() ((void) 0)
# endif
#endif

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

   The stack might look like this

     +----------------+
     |  Lisp_Object   |  size = 4
     +----------------+
     | something else |  size = 2
     +----------------+
     |  Lisp_Object   |  size = 4
     +----------------+
     |	...	      |

   where not every Lisp_Object is aligned equally.  Walking the stack
   in 4-byte steps would immediately miss the second Lisp_Object.  We
   instead issue two passes, one starting at the stack base, and the
   other starting at the base + 2.  Similarly, if the minimal
   alignment of Lisp_Objects were 1, four passes would be
   required.

   We assume the stack is a contiguous in memory.   */
void
xscan_stack (char const *bottom, char const *end, const gc_phase phase)
{
  /* This assumes that the stack is a contiguous region in memory.  If
     that's not the case, something has to be done here to iterate
     over the stack segments.  */
  scan_memory (bottom, end, phase);
}

/* flush_stack_call_func is the trampoline function that flushes
   registers to the stack, and then calls FUNC on ARG.

   Must be called before releasing global interpreter lock (e.g.,
   thread-yield).  This lets the garbage collector easily find roots
   in registers on threads that are not actively running Lisp.

   It is invalid to run any Lisp code or to allocate any GC memory
   from FUNC.

   Must respect calling convention.  First push callee-saved registers in
   flush_stack_call_func, then call flush_stack_call_func1 where now ebp
   would include the pushed-to addresses.  Note NO_INLINE ensures registers
   are spilled.  (Bug#41357)  */

NO_INLINE void
flush_stack_call_func/* 1 */ (void (*func) (void *arg), void *arg)
{
  void *end;
  struct thread_state *self = current_thread;
  SET_STACK_TOP_ADDRESS (&end);
  self->stack_top = end;
  func (arg);
  eassert (current_thread == self);
}

/* Determine whether it is safe to access memory at address P.  */
int
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

  if (SYMBOLP (obj) && c_symbol_p (p))
    return ((char *) p - (char *) lispsym) % sizeof lispsym[0] == 0;

  if (p == &buffer_defaults || p == &buffer_local_symbols)
    return 2;

  if (gc_pdumper_object_p (p))
    return pdumper_object_p_precise (p) ? 1 : 0;

  struct mem_node *m = mem_find (p);

  if (m == NULL)
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
    case MEM_TYPE_CONS:
      return live_cons_p (m, p);

    case MEM_TYPE_STRING:
      return live_string_p (m, p);

    case MEM_TYPE_SYMBOL:
      return live_symbol_p (m, p);

    case MEM_TYPE_FLOAT:
      return live_float_p (m, p);

    case MEM_TYPE_LARGE_VECTOR:
    case MEM_TYPE_VECTOR:
      return live_vector_p (m, p);

    default:
      break;
    }

  return 0;
}

DEFUN ("purecopy", Fpurecopy, Spurecopy, 1, 1, 0,
       doc: /* Return OBJ.  Deprecated.  */)
  (register Lisp_Object obj)
{
  return obj;
}


/***********************************************************************
			  Protection from GC
 ***********************************************************************/

/* Put an entry in staticvec, pointing at the variable with address
   VARADDRESS.  */

void
staticpro (Lisp_Object *varaddress)
{
  for (int i = 0; i < staticidx; i++)
    eassert (staticvec[i] != varaddress);
  if (staticidx >= NSTATICS)
    fatal ("NSTATICS too small; try increasing and recompiling Emacs.");
  staticvec[staticidx++] = varaddress;
}


/***********************************************************************
			  Protection from GC
 ***********************************************************************/

void
inhibit_garbage_collection_undo (const intmax_t old_consing_until_gc)
{
  const ptrdiff_t since_inhibit = gc_hi_threshold - consing_until_gc;
  if (INT_SUBTRACT_WRAPV (old_consing_until_gc,
                          since_inhibit,
                          &consing_until_gc))
    consing_until_gc = -1;
  garbage_collection_inhibited--;
}

/* Temporarily inhibit garbage collection using a specpdl entry.  */
ptrdiff_t
inhibit_garbage_collection (void)
{
  /* Bump garbage_collection_inhibited first so that we don't
     recurse forever below if it happens to be time to GC.  */
  garbage_collection_inhibited++;
  const ptrdiff_t count = SPECPDL_INDEX ();
  record_unwind_protect_intmax (
    inhibit_garbage_collection_undo, consing_until_gc);
  consing_until_gc = gc_hi_threshold;
  return count;
}

void
visit_vectorlike_root (const struct gc_root_visitor visitor,
                       struct Lisp_Vector *const ptr,
                       const enum gc_root_type type)
{
  const ptrdiff_t size = vectorlike_lisp_size (&ptr->header);
  for (ptrdiff_t i = 0; i < size; i++)
    visitor.visit (&ptr->contents[i], type, visitor.data);
}

void
visit_buffer_root (const struct gc_root_visitor visitor,
                   struct buffer *const buffer,
                   const enum gc_root_type type)
{
  /* Metadata buffers don't have constructs that real buffers have.  */
  eassert (buffer->base_buffer == NULL
	   && buffer->overlays_before == NULL
	   && buffer->overlays_after == NULL);

  /* Visit the buffer-locals.  */
  visit_vectorlike_root (visitor, (struct Lisp_Vector *) buffer, type);
}

/* Visit GC roots stored in the Emacs data section.  Used by both core
   GC and by the portable dumping code.

   We mark dynamic GC roots which pdumper doesn't care about directly
   in garbage_collect.  */
void
visit_static_gc_roots (const struct gc_root_visitor visitor)
{
  visit_buffer_root (visitor,
                     &buffer_defaults,
                     GC_ROOT_BUFFER_LOCAL_DEFAULT);
  visit_buffer_root (visitor,
                     &buffer_local_symbols,
                     GC_ROOT_BUFFER_LOCAL_NAME);

  for (int i = 0; i < ARRAYELTS (lispsym); i++)
    {
      Lisp_Object sptr = builtin_lisp_symbol (i);
      visitor.visit (&sptr, GC_ROOT_C_SYMBOL, visitor.data);
    }

  for (int i = 0; i < staticidx; i++)
    visitor.visit (staticvec[i], GC_ROOT_STATICPRO, visitor.data);
}

/* Watch changes to gc-cons-threshold.  */
Lisp_Object
watch_gc_cons_threshold (Lisp_Object symbol, Lisp_Object newval,
			 Lisp_Object operation, Lisp_Object where)
{
  maybe_garbage_collect ();
  return Qnil;
}

/* Watch changes to gc-cons-percentage.  */
Lisp_Object
watch_gc_cons_percentage (Lisp_Object symbol, Lisp_Object newval,
			  Lisp_Object operation, Lisp_Object where)
{
  maybe_garbage_collect ();
  return Qnil;
}

void
scan_object_root_visitor_mark (Lisp_Object *root_ptr,
                               enum gc_root_type type,
                               void *data)
{
  (void) data;
  if (type != GC_ROOT_C_SYMBOL)
    scan_reference (root_ptr, GC_PHASE_MARK);
  else
    scan_object_for_marking_ool (XPNTR (*root_ptr), XTYPE (*root_ptr));
}

static inline bool mark_stack_empty_p (void);

/* Subroutine of Fgarbage_collect that does most of the work.  */
void
scan_object_root_visitor_sweep (Lisp_Object *root_ptr,
                                enum gc_root_type type,
                                void *data)
{
  (void) data;
  if (type != GC_ROOT_C_SYMBOL)
    scan_reference (root_ptr, GC_PHASE_SWEEP);
  else
    scan_object_for_sweeping_ool (XPNTR (*root_ptr), XTYPE (*root_ptr));
}

/* Called from pdumper to walk over objects in the pdumper image that
   might point to runtime heap objects.

   (TODO: right now, pdumper conservatively approximates this set by
   calling sweep_pdumper_object on every object in the pdumper image
   that's marked.  We really should be more selective, especially in
   the generational case.)  */
void
sweep_pdumper_object (void *const obj, const enum Lisp_Type type)
{
  eassume (current_gc_phase == GC_PHASE_SWEEP);
  eassert (gc_pdumper_object_p (obj));
  eassert (pdumper_object_p_precise (obj));
  scan_object_for_sweeping_ool (obj, type);
}

void
scan_roots (const gc_phase phase)
{
  eassume (phase == GC_PHASE_MARK || phase == GC_PHASE_SWEEP);
  struct gc_root_visitor visitor = {.visit = (
      phase == GC_PHASE_MARK
      ? scan_object_root_visitor_mark
      : scan_object_root_visitor_sweep),
  };
  visit_static_gc_roots (visitor);

  scan_reference_pointer_to_vectorlike (&terminal_list, phase);
  scan_reference_pointer_to_vectorlike (&initial_terminal, phase);
  scan_kboards (phase);
  scan_thread_roots (phase);
  scan_doomed_finalizers (phase);

  scan_dispnew_roots (phase);
  scan_marker_roots (phase);
  scan_xdisp_roots (phase);
  scan_syntax_roots (phase);
  scan_process_roots (phase);

#ifdef HAVE_NTGUI
  scan_reference_pointer_to_vectorlike (&w32_system_caret_window, phase);
#endif

#ifdef USE_GTK
  xg_scan_data (phase);
#endif

#ifdef HAVE_WINDOW_SYSTEM
  scan_fringe_data (phase);
#endif

#ifdef HAVE_MODULES
  //  scan_modules (NULL, phase);
#endif
}

bool
mark_strong_references_on_reachable_weak_list (
  Lisp_Object tail,
  bool (*const entry_survives_gc_p)(Lisp_Object car))
{
  bool marked = false;
  for (; CONSP (tail); tail = XCDR (tail))
    if (entry_survives_gc_p (XCAR (tail)) && !survives_gc_p (XCAR (tail)))
      {
        gc_mark_or_enqueue (XCAR (tail));
        marked = true;
      }
  if (!survives_gc_p (tail))
    {
      gc_mark_or_enqueue (tail);
      marked = true;
    }
  return marked;
}

void
clear_weak_references_on_reachable_weak_list (Lisp_Object *prevp)
{
  while (CONSP (*prevp))
    {
      if (survives_gc_p (XCAR (*prevp)))
        {
          eassert (survives_gc_p (XCAR (*prevp)));
          /* We'd like to assert that this cons doesn't already
             survive GC, but we can't because it could always have
             gotten marked from conservative GC.  */
          if (!cons_marked_p (XCONS (*prevp)))
            set_cons_marked (XCONS (*prevp));
          prevp = xcdr_addr (*prevp);
        }
      else
        *prevp = XCDR (*prevp);
    }
  eassert (survives_gc_p (*prevp));
}

/* Mark or sweep weak hash table H depending on the value of MARK.
  In mark mode (MARK is true) scan (using scan_reference) all the
  strongly-referenced parts of weak hash table entries and return
  whether we found any freshly-referenced weak entries.  In sweep mode
  (MARK is false) remove unreferenced weak entries from the hash table
  and return false.
  */
bool
mark_or_sweep_weak_table (struct Lisp_Hash_Table *const h, const bool mark)
{
  ptrdiff_t n = ASIZE (h->index);
  bool marked = false;

  for (ptrdiff_t bucket = 0; bucket < n; ++bucket)
    {
      /* Follow collision chain, removing entries that don't survive
         this garbage collection.  It's okay if hash_rehash_needed_p
         (h) is true, since we're operating entirely on the cached
         hash values. */
      ptrdiff_t prev = -1;
      ptrdiff_t next;
      for (ptrdiff_t i = HASH_INDEX (h, bucket); 0 <= i; i = next)
        {
	  bool key_known_to_survive_p = survives_gc_p (HASH_KEY (h, i));
	  bool value_known_to_survive_p = survives_gc_p (HASH_VALUE (h, i));
	  bool remove_p;

	  if (EQ (h->weak, Qkey))
	    remove_p = !key_known_to_survive_p;
	  else if (EQ (h->weak, Qvalue))
	    remove_p = !value_known_to_survive_p;
	  else if (EQ (h->weak, Qkey_or_value))
	    remove_p = !(key_known_to_survive_p || value_known_to_survive_p);
	  else if (EQ (h->weak, Qkey_and_value))
	    remove_p = !(key_known_to_survive_p && value_known_to_survive_p);
	  else
	    emacs_unreachable ();

	  next = HASH_NEXT (h, i);

	  if (!mark)
	    {
              eassert (!remove_p
                       == (key_known_to_survive_p && value_known_to_survive_p));
	      if (remove_p)
		{
		  /* Take out of collision chain.  */
		  if (prev < 0)
		    set_hash_index_slot (h, bucket, next);
		  else
		    set_hash_next_slot (h, prev, next);

		  /* Add to free list.  */
		  set_hash_next_slot (h, i, h->next_free);
		  h->next_free = i;

		  /* Clear key, value, and hash.  */
		  set_hash_key_slot (h, i, Qunbound);
		  set_hash_value_slot (h, i, Qnil);
                  if (!NILP (h->hash))
                    set_hash_hash_slot (h, i, Qnil);

                  eassert (h->count != 0);
                  h->count += h->count > 0 ? -1 : 1;
                }
	      else
		{
		  prev = i;
		}
	    }
	  else
	    {
	      if (!remove_p)
		{
		  /* Make sure key and value survive.  */
		  if (!key_known_to_survive_p)
		    {
		      scan_reference (HASH_KEYP (h, i), GC_PHASE_MARK);
                      marked = true;
		    }

		  if (!value_known_to_survive_p)
		    {
		      scan_reference (HASH_VALUEP (h, i), GC_PHASE_MARK);
                      marked = true;
		    }
		}
	    }
	}
    }

  return marked;
}

bool
mark_strong_references_of_reachable_weak_hash_tables (void)
{
  bool marked = false;
  struct Lisp_Hash_Table *h;
  for (h = weak_hash_tables; h; h = h->next_weak)
    marked |= mark_or_sweep_weak_table (h, /*mark=*/true);
  return marked;
}

bool
undo_list_entry_survives_gc_p (const Lisp_Object entry)
{
  /* Remove (MARKER . DATA) entries with unmarked MARKER.  */
  return !CONSP (entry) ||
    !MARKERP (XCAR (entry)) ||
    survives_gc_p (XCAR (entry));
}

bool
mark_strong_references_of_reachable_undo_list_entries (void)
{
  bool marked = false;
  Lisp_Object tail, buffer;
  FOR_EACH_LIVE_BUFFER (tail, buffer)
    {
      const Lisp_Object undo_list = BVAR (XBUFFER (buffer), undo_list);
      if (!EQ (undo_list, Qt))
        marked |= mark_strong_references_on_reachable_weak_list (
          undo_list, undo_list_entry_survives_gc_p);
    }
  return marked;
}

bool
font_cache_entry_survives_gc_p (const Lisp_Object obj)
{
  bool drop = false;

  /* Consider OBJ if it is (font-spec . [font-entity font-entity ...]).  */
  if (CONSP (obj) && FONT_SPEC_P (XCAR (obj))
      && !vectorlike_marked_p (&XFONT_SPEC (XCAR (obj))->header)
      && (VECTORLIKEP (XCDR (obj)) && !VECTORP (XCDR (obj))))
    {
      ptrdiff_t i;
      const ptrdiff_t size = PVSIZE (XCDR (obj));
      const Lisp_Object obj_cdr = XCDR (obj);

      /* If font-spec is not marked, most likely all font-entities
         are not marked too.  But we must be sure that nothing is
         marked within OBJ before we really drop it.  */
      for (i = 0; i < size; i++)
        {
          Lisp_Object objlist;

          if (vectorlike_marked_p (
                &XFONT_ENTITY (AREF (obj_cdr, i))->header))
            break;

          objlist = AREF (AREF (obj_cdr, i), FONT_OBJLIST_INDEX);
          for (; CONSP (objlist); objlist = XCDR (objlist))
            {
              Lisp_Object val = XCAR (objlist);
              struct font *font = XFONT_OBJECT (val);

              if (!NILP (AREF (val, FONT_TYPE_INDEX))
                  && vectorlike_marked_p(&font->header))
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

  return !drop;
}

bool
mark_strong_references_of_reachable_font_cache (const Lisp_Object cache)
{
  return mark_strong_references_on_reachable_weak_list (
    cache, font_cache_entry_survives_gc_p);
}

bool
mark_strong_references_of_reachable_font_caches (void)
{
  bool marked = false;
  struct terminal *t;
  for (t = terminal_list; t; t = t->next_terminal)
    {
      Lisp_Object *cachep = TERMINAL_FONT_CACHEP (t);
      /* Inhibit compacting the caches if the user so wishes.  Some of
	 the users don't mind a larger memory footprint, but do mind
	 slower redisplay.  */
      if (!cachep)
        continue;
      if (!inhibit_compacting_font_caches_snapshot)
        while (CONSP (*cachep))
          {
            marked |= mark_strong_references_of_reachable_font_cache (
              XCAR (*cachep));
            cachep = xcdr_addr (*cachep);
          }
      if (!survives_gc_p (*cachep))
        {
          marked = true;
          gc_mark_or_enqueue (*cachep);
        }
    }
  return marked;
}

bool
mark_strong_references_of_reachable_finalizers (void)
{
  bool marked = false;
  Lisp_Object fin = finalizers;
  while (!NILP (fin))
    {
      struct Lisp_Finalizer *const finalizer = XFINALIZER (fin);
      if (survives_gc_p (fin) && !survives_gc_p (finalizer->function))
        {
          marked = true;
          gc_mark_or_enqueue (finalizer->function);
        }
      fin = finalizer->next;
    }
  return marked;
}

/* Mark strongly-held references of reachable weakly-referenced
   objects and return whether we found anything new to mark.  We keep
   calling this function until we find nothing new to mark: this way,
   we handle references from weak objects making other weak objects
   reachable.  */
bool
mark_strong_references_of_reachable_weak_objects (void)
{
  eassert (current_gc_phase == GC_PHASE_MARK);
  bool marked = false;
  marked |= mark_strong_references_of_reachable_weak_hash_tables ();
  marked |= mark_strong_references_of_reachable_undo_list_entries ();
  marked |= mark_strong_references_of_reachable_font_caches ();
  marked |= mark_strong_references_of_reachable_finalizers ();
  return marked;
}

void
clear_weak_hash_tables (void)
{
  while (weak_hash_tables)
    {
      /* XXX: use weak hash table array instead of mutating the object
         itself!  */
      struct Lisp_Hash_Table *const h = weak_hash_tables;
      weak_hash_tables = weak_hash_tables->next_weak;
      h->next_weak = NULL;
      mark_or_sweep_weak_table (h, /*mark=*/false);
    }
}

void
clear_weak_undo_list_entries (void)
{
  static struct timespec gc_elapsed = {0, 0};
  Lisp_Object tail, buffer;
  FOR_EACH_LIVE_BUFFER (tail, buffer)
    clear_weak_references_on_reachable_weak_list (
      &BVAR (XBUFFER (buffer), undo_list));
}

void
clear_weak_font_caches (void)
{
  struct terminal *t;
  for (t = terminal_list; t; t = t->next_terminal)
    {
      Lisp_Object *cachep = TERMINAL_FONT_CACHEP (t);
      if (!cachep)
        continue;
      while (CONSP (*cachep))
        {
          clear_weak_references_on_reachable_weak_list (xcar_addr (*cachep));
          /* Don't assert that the cons isn't already marked: conservative
             GC can mark anything.  */
          if (!cons_marked_p (XCONS (*cachep)))
            set_cons_marked (XCONS (*cachep));
          cachep = xcdr_addr (*cachep);
        }
      eassert (survives_gc_p (*cachep));
    }
}

void
scan_doomed_finalizers (const gc_phase phase)
{
  Lisp_Object *prevp = &doomed_finalizers;
  scan_reference (prevp, phase);
  while (!NILP (*prevp))
    {
      scan_reference (&XFINALIZER (*prevp)->function, phase);
      prevp = &XFINALIZER (*prevp)->next;
    }
}

void
enqueue_doomed_finalizers (void)
{
  Lisp_Object *prevp = &finalizers;
  while (!NILP (*prevp))
    {
      struct Lisp_Finalizer *finalizer = XFINALIZER (*prevp);
      eassert (!NILP (finalizer->function));
      if (survives_gc_p (*prevp))
        prevp = &finalizer->next;
      else
        {
          doomed_finalizers = *prevp;
          *prevp = finalizer->next;
          finalizer->next = doomed_finalizers;
        }
    }
}

void
clear_weak_marker_chains (void)
{
  Lisp_Object tail, buf;
  FOR_EACH_LIVE_BUFFER (tail, buf)
    unchain_dead_markers (XBUFFER (buf));
}

/* Remove references to now-known-to-be-unreachable weakly-referenced
   objects.  */
void
gc_phase_clear_weak_references (void)
{
  clear_weak_hash_tables ();
  clear_weak_undo_list_entries ();
  clear_weak_font_caches ();
  clear_weak_marker_chains ();
  /* Weak finalizers are handled before the mark phase ends.  */
}

bool
gc_is_in_progress (void)
{
  return current_gc_phase != GC_PHASE_NOT_IN_PROGRESS;
}

/* Return whether the object PTR is in the pdumper image range.
   This function produces the same result as pdumper_object_p(), but
   consults the GC cycle hot cache area.  */
bool
gc_pdumper_object_p (const void *const ptr)
{
  const uintptr_t la = (uintptr_t) ptr;
  const bool result =
    gc_hot.pdumper_start <= la &&
    (la - gc_hot.pdumper_start <
     gc_hot.pdumper_size);
  eassert (result == pdumper_object_p (ptr));
  return result;
}

void
gc_phase_prepare (const bool major)
{
  eassume (current_gc_phase == GC_PHASE_PREPARE);
  /* Snapshot things that may change across GC.  */
  inhibit_compacting_font_caches_snapshot = inhibit_compacting_font_caches;
  /* Regenerate hot-phase data.  */
  memset (&gc_hot, 0, sizeof (gc_hot));
  /* Cache pdumper's dump location information.  */
  gc_hot.pdumper_start = (uintptr_t) dump_public.start;
  /* Cache the major-collection flag.  */
  gc_hot.major = major;
  eassume ((uintptr_t) dump_public.end
           - gc_hot.pdumper_start <= INT32_MAX);
  gc_hot.pdumper_size = (uintptr_t) dump_public.end - gc_hot.pdumper_start;
  /* Cache so we don't have to constantly consult current_thread.  */
  gc_hot.gc_phase_mark_stack_bottom_la = (uintptr_t) stack_bottom;
  /* Reset all the variables that we want to regenerate for each
     collection cycle.  */
  for (int i = 0; i < ARRAYELTS (gc_heaps); ++i)
    gc_heap_prepare (gc_heaps[i], major);
}

/* Mark all reachable objects.  */
void
gc_phase_mark (void)
{
  scan_roots (GC_PHASE_MARK);
  for (int i = 0; i < ARRAYELTS (gc_heaps); ++i)
    gc_heap_for_each_block (gc_heaps[i],
                            gc_heaps[i]->block_mark_intergenerational,
                            NULL);

  /* Mark everything.  We need to iterate until a fixed point so that
     we capture mutual references between weakly-referenced objects
     --- consier a value-weak table A containing an entry X -> Y,
     where Y is used in a key-weak table B, Z -> Y.  If B comes after
     A in the list of weak tables, X -> Y might be removed from A,
     although when looking at B one finds that it shouldn't.  */
  do
    gc_mark_drain_queue ();
  while (mark_strong_references_of_reachable_weak_objects ());

  /* We move any finalizer not marked at this point to the
     doomed_finalizers list; after we do that, we have to actually
     mark the doomed_finalizers list so that its entries actually do
     survive this particular GC.  */
  enqueue_doomed_finalizers ();
  scan_doomed_finalizers (GC_PHASE_MARK);
  gc_mark_drain_queue ();
}

void
gc_phase_plan_sweep (void)
{
  /* Figure out where each object will live in tospace.  Remember the
     last non-pinned object we placed in tospace.  */
  for (int i = 0; i < ARRAYELTS (gc_heaps); ++i)
    gc_heap_plan_sweep (gc_heaps[i]);
}

void
gc_phase_sweep (void)
{
  for (int i = 0; i < ARRAYELTS (gc_heaps); ++i)
    gc_heap_sweep (gc_heaps[i]);
  scan_doomed_finalizers (GC_PHASE_SWEEP);
  sweep_large_vectors ();

  scan_roots (GC_PHASE_SWEEP);
  pdumper_sweep ();

  /* A heap might keep its fromspace around until after all sweeps are
     done --- we need to do that when we need to read an object on
     that heap to figure out whether it's on the GC heap or stored
     somewhere else, e.g., inside the Emacs image.  Now we can finish
     the work.  */
  for (int i = 0; i < ARRAYELTS (gc_heaps); ++i)
    gc_heap_finalize_tospace (gc_heaps[i]);
}

void
gc_phase_cleanup (const bool major)
{
  eassume (current_gc_phase == GC_PHASE_CLEANUP);
  /* Restore heap invariants after tospace move.  */
  for (int i = 0; i < ARRAYELTS (gc_heaps); ++i)
    gc_heap_cleanup_after_all_sweeps (gc_heaps[i]);
  gc_heap_size_at_end_of_last_gc = gc_compute_total_live_nr_bytes ();
  if (major)
    gc_heap_size_at_end_of_last_major_gc = gc_heap_size_at_end_of_last_gc;
  recompute_consing_until_gc ();
}

void
compact_all_buffers (void)
{
  Lisp_Object tail, buffer;
  /* Don't keep undo information around forever.
     Do this early on, so it is no problem if the user quits.  */
  FOR_EACH_LIVE_BUFFER (tail, buffer)
    compact_buffer (XBUFFER (buffer));
}

bool
gc_try_handle_sigsegv (void *const fault_address)
{
  if (mem_tree_being_modified)
    return false;
  struct mem_node *mem = mem_find (fault_address);
  if (mem == NULL)
    return false;
  switch (mem->type)
    {
    case MEM_TYPE_CONS:
      return gc_block_mark_card_table (
        mem->start, fault_address, &gc_cons_heap);
    case MEM_TYPE_STRING:
      return gc_block_mark_card_table (
        mem->start, fault_address, &gc_string_heap);
    case MEM_TYPE_SYMBOL:
      return gc_block_mark_card_table (
        mem->start, fault_address, &gc_symbol_heap);
    case MEM_TYPE_FLOAT:
      return gc_block_mark_card_table (
        mem->start, fault_address, &gc_float_heap);
    case MEM_TYPE_VECTOR:
      return gc_block_mark_card_table (
        mem->start, fault_address, &gc_vector_heap);
    case MEM_TYPE_INTERVAL:
      return gc_block_mark_card_table (
        mem->start, fault_address, &gc_interval_heap);
    case MEM_TYPE_LARGE_VECTOR:
      emacs_abort ();  // XXX FIXME
    }
  emacs_unreachable ();
}

static inline void check_obarray_elem(const Lisp_Object tail, size_t i, size_t obsize)
{
  const char *sname = SSDATA (SYMBOL_NAME (tail));
  const size_t sname_len = SBYTES (SYMBOL_NAME (tail));
  eassert (strlen (sname) == sname_len);
  const size_t hash = hash_string (sname, sname_len) % obsize;
  eassert (hash == i);
}

static void
XXX_check_obarray (void)
{
  if (!enable_checking)
    return;
  const Lisp_Object obarray = Vobarray;
  const size_t obsize = ASIZE (obarray);
  register Lisp_Object tail = {0};

  for (size_t i = 0; i < obsize; ++i)
    {
      tail = AREF (obarray, i);
      if (SYMBOLP (tail))
	while (1)
	  {
	    check_obarray_elem(tail, i, obsize);
	    if (XSYMBOL (tail)->u.s.next == 0)
	      break;
	    XSETSYMBOL(tail, XSYMBOL(tail)->u.s.next);
	  }
    }
}

void
garbage_collect (const bool major)
{
  /* TODO: make GC infallible */

  eassert (weak_hash_tables == NULL);

  if (gc_inhibited || gc_in_progress)
    return;

  gc_in_progress = true;

  eassert (mark_stack_empty_p ());

  /* Record this function, so it appears on the profiler's backtraces.  */
  record_in_backtrace (QAutomatic_GC, 0, 0);
  compact_all_buffers ();

  const size_t tot_before = gc_heap_size_at_end_of_last_gc;

  const struct timespec start = current_timespec ();

  /* In case user calls debug_print during GC,
     don't let that cause a recursive GC.  */
  consing_until_gc = gc_hi_threshold;

  /* Save what's currently displayed in the echo area.  Don't do that
     if we are GC'ing because we've run out of memory, since
     push_message will cons, and we might have no memory for that.  */
  bool message_p = false;
  if (NILP (Vmemory_full))
    {
      message_p = push_message ();
      record_unwind_protect_void (pop_message_unwind);
    }

  if (garbage_collection_messages)
    message1_nolog ("Garbage collecting...");

  block_input ();

  shrink_regexp_cache ();

  XXX_check_obarray ();

  eassume (current_gc_phase == GC_PHASE_NOT_IN_PROGRESS);
  current_gc_phase = GC_PHASE_PREPARE;
  gc_phase_prepare (major);

  current_gc_phase = GC_PHASE_MARK;
  gc_phase_mark ();
  gc_phase_clear_weak_references ();
  /* N.B. the bitset and locator arrays corresponding to the part of
     the heap before the start of the current generation are at this
     point untouched by the mark phase and zero or unspecified,
     respectively.  */

  current_gc_phase = GC_PHASE_PLAN_SWEEP;
  gc_phase_plan_sweep ();

  current_gc_phase = GC_PHASE_SWEEP;
  gc_phase_sweep ();

  current_gc_phase = GC_PHASE_CLEANUP;
  gc_phase_cleanup (major);

  // visit_static_gc_roots (visitor);
  mark_pinned_objects ();
  mark_pinned_symbols ();
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
      mark_object (BVAR (b, undo_list));
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
  current_gc_phase = GC_PHASE_NOT_IN_PROGRESS;
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
  run_doomed_finalizers ();

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

  XXX_check_obarray ();

  /* Collect profiling data.  */
  if (profiler_memory_running &&
      gc_heap_size_at_end_of_last_gc < tot_before)
    {
      size_t shrinkage =
        tot_before - gc_heap_size_at_end_of_last_gc;
      if (shrinkage > INTPTR_MAX)
        shrinkage = INTPTR_MAX;
      malloc_probe (-(ptrdiff_t) shrinkage);
    }
}

/* Update consing_until_gc and the major and minor garbage collection
   thresholds.  */
void
recompute_consing_until_gc (void)
{
  double major_percentage = FLOATP (Vgc_cons_percentage_major)
    ? XFLOAT_DATA (Vgc_cons_percentage_major)
    : gc_cons_percentage_major_default;
  const double max_major_percentage =
    (double) gc_heap_size_at_end_of_last_major_gc /
    (double) SIZE_MAX;
  major_percentage = min (major_percentage, max_major_percentage);
  major_percentage = max (major_percentage, 0.0);
  const size_t major_growth =
    gc_heap_size_at_end_of_last_major_gc * major_percentage;
  size_t new_major_threshold;
  if (INT_ADD_WRAPV (gc_heap_size_at_end_of_last_major_gc,
                     major_growth,
                     &new_major_threshold) ||
      new_major_threshold > gc_hi_threshold)
    new_major_threshold = gc_hi_threshold;

  double minor_percentage = FLOATP (Vgc_cons_percentage)
    ? XFLOAT_DATA (Vgc_cons_percentage)
    : gc_cons_percentage_minor_default;
  const double max_minor_percentage =
    (double) gc_heap_size_at_end_of_last_gc /
    (double) SIZE_MAX;
  minor_percentage = min (minor_percentage, max_minor_percentage);
  minor_percentage = max (minor_percentage, 0.0);
  const size_t min_minor_growth =
    (gc_cons_threshold > SIZE_MAX)
    ? SIZE_MAX
    : gc_cons_threshold > 0
    ? gc_cons_threshold
    : 0;
  const size_t minor_growth =
    max (min_minor_growth,
         (size_t) gc_heap_size_at_end_of_last_gc * minor_percentage);
  size_t new_minor_threshold;
  if (INT_ADD_WRAPV (gc_heap_size_at_end_of_last_gc,
                     minor_growth,
                     &new_minor_threshold) ||
      new_minor_threshold > gc_hi_threshold)
    new_minor_threshold = gc_hi_threshold;
  if (new_major_threshold < new_minor_threshold)
    new_major_threshold = new_minor_threshold;

  gc_major_collection_threshold = new_major_threshold;
  gc_minor_collection_threshold = new_minor_threshold;
  eassume (new_minor_threshold <= PTRDIFF_MAX);
  consing_until_gc = new_minor_threshold;
}

/* It may be time to collect garbage.  Recalculate consing_until_gc,
   since it might depend on current usage, and do the garbage
   collection if the recalculation says so.  */
void
maybe_garbage_collect (void)
{
  const ptrdiff_t nr_bytes_allocated_since_last_gc =
    gc_minor_collection_threshold - consing_until_gc;
  eassume (nr_bytes_allocated_since_last_gc >= 0);
  eassume (!INT_ADD_OVERFLOW (gc_heap_size_at_end_of_last_major_gc,
                              (size_t) nr_bytes_allocated_since_last_gc));
  const size_t total_nr_bytes_in_use =
    gc_heap_size_at_end_of_last_gc + nr_bytes_allocated_since_last_gc;
  eassume (gc_heap_size_at_end_of_last_gc >=
           gc_heap_size_at_end_of_last_major_gc);
  if (total_nr_bytes_in_use >= gc_major_collection_threshold)
    garbage_collect (/*major=*/true);
  else if (total_nr_bytes_in_use >= gc_minor_collection_threshold)
    garbage_collect (false);
  else
    recompute_consing_until_gc ();
}

Lisp_Object
gc_make_heap_info (const gc_heap *const h)
{
  return list5 (
    make_lisp_symbol (&lispsym[h->heap_symbol_index]),
    make_uint (h->data->stats.nr_objects),
    make_uint (h->data->stats.nr_slots
               * gc_heap_nr_bytes_per_slot (h)),
    make_uint (h->data->stats.nr_objects_pinned),
    make_uint (h->data->stats.nr_slots_pinned
               * gc_heap_nr_bytes_per_slot (h)));

}

DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 1, "",
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
If MINOR is non-nil, do a minor garbage collection instead of a major one.
If there was overflow in pure space, and Emacs was dumped
using the 'unexec' method, `garbage-collect' returns nil, because
real GC can't be done.
See Info node `(elisp)Garbage Collection'.  */)
  (Lisp_Object minor)
{
  if (gc_inhibited)
    return Qnil;
  garbage_collect (/*major=*/!NILP (minor));

  Lisp_Object info = Qnil;
  for (int i = 0; i < ARRAYELTS (gc_heaps); ++i)
    info = Fcons (gc_make_heap_info (gc_heaps[i]), info);
  return info;
}

/* DEFUN ("garbage-collect-maybe", Fgarbage_collect_maybe, */
/* Sgarbage_collect_maybe, 1, 1, 0, */
/*        doc: /\* Call `garbage-collect' if enough allocation happened. */
/* FACTOR determines what "enough" means here: */
/* If FACTOR is a positive number N, it means to run GC if more than */
/* 1/Nth of the allocations needed to trigger automatic allocation took */
/* place. */
/* Therefore, as N gets higher, this is more likely to perform a GC. */
/* Returns non-nil if GC happened, and nil otherwise.  *\/) */
/*   (Lisp_Object factor) */
/* { */
/*   CHECK_FIXNAT (factor); */
/*   EMACS_INT fact = XFIXNAT (factor); */

/*   EMACS_INT since_gc = gc_threshold - consing_until_gc; */
/*   if (fact >= 1 && since_gc > gc_threshold / fact) */
/*     { */
/*       garbage_collect (true); */
/*       return Qt; */
/*     } */
/*   else */
/*     return Qnil; */
/* } */

/* Mark Lisp objects in glyph matrix MATRIX.  Currently the
   only interesting objects referenced from glyphs are strings.  */
void
scan_glyph_matrix (struct glyph_matrix *const matrix, const gc_phase phase)
{
  scan_reference_pointer_to_vectorlike (&matrix->buffer, phase);

  struct glyph_row *row = matrix->rows;
  struct glyph_row *const end = row + matrix->nrows;

  for (; row < end; ++row)
    if (row->enabled_p)
      for (int area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
        {
          struct glyph *glyph = row->glyphs[area];
          struct glyph *end_glyph = glyph + row->used[area];

          for (; glyph < end_glyph; ++glyph)
            if (STRINGP (glyph->object)
                && !string_marked_p (XSTRING (glyph->object)))
              scan_reference (&glyph->object, phase);
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

void
scan_vector_lisp_fields (union vectorlike_header *header, const gc_phase phase)
{
  struct Lisp_Vector *ptr = (struct Lisp_Vector *) header;
  const ptrdiff_t size = vectorlike_lisp_size (&ptr->header);
  ptrdiff_t i;

  /* Note that this size is not the memory-footprint size, but only
     the number of Lisp_Object fields that we should trace.
     The distinction is used e.g. by Lisp_Process which places extra
     non-Lisp_Object fields at the end of the structure...  */
  for (i = 0; i < size; i++) /* ...and then mark its elements.  */
    scan_reference (&ptr->contents[i], phase);
}

/* Scan a pseudovector with no lisp slots.  Does nothing excep assert
   that we have no lisp slots.  */
void
scan_pseudovector_empty (union vectorlike_header *header, const gc_phase phase)
{
  eassume (header->size & PSEUDOVECTOR_FLAG);
  eassume ((header->size & PSEUDOVECTOR_SIZE_MASK) == 0);
  (void) phase;
}

ptrdiff_t
vectorlike_lisp_size (const union vectorlike_header *const header)
{
  return (header->size & PSEUDOVECTOR_FLAG)
    ? header->size & PSEUDOVECTOR_SIZE_MASK
    : header->size;
}

void
xscan_vector_lisp_fields (union vectorlike_header *const header,
                          const gc_phase phase)
{
  return scan_vector_lisp_fields (header, phase);
}


/* Like scan_vector_lisp_fields but optimized for char-tables (and
   sub-char-tables) assuming that the contents are mostly integers or
   symbols.  */
void
scan_char_table (struct Lisp_Vector *const ptr,
                 const enum pvec_type pvectype,
                 const gc_phase phase)
{
  int size = ptr->header.size & PSEUDOVECTOR_SIZE_MASK;
  /* Consult the Lisp_Sub_Char_Table layout before changing this.  */
  int i, idx = (pvectype == PVEC_SUB_CHAR_TABLE ? SUB_CHAR_TABLE_OFFSET : 0);
  for (i = idx; i < size; i++)
    scan_reference (&ptr->contents[i], phase);
}

void
scan_overlay (struct Lisp_Overlay *const ptr, const gc_phase phase)
{
  scan_reference (&ptr->start, phase);
  scan_reference (&ptr->end, phase);
  scan_reference (&ptr->plist, phase);
  scan_reference_pointer_to_vectorlike (&ptr->next, phase);
}

/* Mark Lisp_Objects and special pointers in BUFFER.  */
void
scan_buffer (struct buffer *const buffer, const gc_phase phase)
{
  scan_vector_lisp_fields (&buffer->header, phase);
  scan_reference_pointer_to_vectorlike (&buffer->overlays_before, phase);
  scan_reference_pointer_to_vectorlike (&buffer->overlays_after, phase);
  scan_reference_pointer_to_vectorlike (&buffer->base_buffer, phase);

  /* For now, we just don't mark the undo_list.  It's done later in a
     special way just before the sweep phase, and after stripping some
     of its elements that are not needed any more.  Note: this later
     processing is only done for live buffers, so for dead buffers, we
     mark the undo list normally.  The undo list of a dead buffer is
     normally just Qt (set by Fkill_buffer), but it's still
     technically a lisp reference, so let's mark it anyway.  We also
     scan the reference normally when in a non-marking phase of GC
     because the special behavior above applies only to marking.  */
  if (phase == GC_PHASE_SWEEP || !BUFFER_LIVE_P (buffer))
    scan_reference (&BVAR (buffer, undo_list), phase);

  if (!buffer->base_buffer && buffer->text)
    {
      scan_reference_pointer_to_interval (&buffer->text->intervals, phase);
      if (false /*XXXX!!!!!! */ && phase == GC_PHASE_MARK)
        buffer->text->intervals =
          balance_intervals (buffer->text->intervals);
      /* The marker list is weak and is marked during weak analysis.  */
      if (phase == GC_PHASE_SWEEP)
        scan_reference_pointer_to_vectorlike (&buffer->text->markers, phase);
    }
}

/* Mark Lisp faces in the face cache C.  */
void
scan_face_cache (struct face_cache *const c,
                 struct frame *const f,
                 const gc_phase phase)
{
  if (!c)
    return;
  for (int i = 0; i < c->used; ++i)
    {
      struct face *const face = FACE_FROM_ID_OR_NULL (f, i);
      if (face)
        {
          scan_reference_pointer_to_vectorlike (&face->font, phase);
          for (int j = 0; j < LFACE_VECTOR_SIZE; ++j)
            scan_reference (&face->lface[j], phase);
        }
    }
}

void
scan_frame (struct frame *const f, const gc_phase phase)
{
  scan_vector_lisp_fields (&f->header, phase);
  scan_face_cache (f->face_cache, f, phase);
  scan_reference_pointer_to_vectorlike (&f->terminal, phase);
#ifdef HAVE_WINDOW_SYSTEM
  /* We use FRAME_OUTPUT_DATA_NOCHECK() because this is the singular
     place where we're allowed to violate the invariant that
     FRAME_OUTPUT_DATA(f)->frame == f; FRAME_OUTPUT_DATA() checks this
     invariant, so we'd die if we used it here.  */
  if (FRAME_WINDOW_P (f) && FRAME_OUTPUT_DATA_NOCHECK (f))
    {
      if (FRAME_FONT_NOCHECK (f))
        scan_reference_pointer_to_vectorlike (
          &FRAME_FONT_NOCHECK (f), phase);
      scan_reference_pointer_to_vectorlike (
        &FRAME_OUTPUT_DATA_NOCHECK (f)->frame, phase);
    }
#endif
#ifdef USE_GTK
  xg_scan_frame (f, phase);
#endif
}

void
discard_killed_buffers (Lisp_Object *const buffer_list)
{
  Lisp_Object *prev = buffer_list;
  while (CONSP (*prev))
    {
      const Lisp_Object item = XCAR (*prev);
      if (BUFFERP (item) && !BUFFER_LIVE_P (XBUFFER (item)))
        *prev = XCDR (*prev);
      else
        prev = &XCONS (*prev)->u.s.cdr;
    }
}

/* Entry of the mark stack.  */
struct mark_entry
void
scan_window (struct window *const w, const gc_phase phase)
{
  scan_vector_lisp_fields (&w->header, phase);

  /* Scan glyph matrices, if any.  Marking window matrices is
     sufficient because frame matrices use the same glyph memory.  */
  if (w->current_matrix)
    scan_glyph_matrix (w->current_matrix, phase);
  if (w->desired_matrix)
    scan_glyph_matrix (w->desired_matrix, phase);

  eassume (phase == current_gc_phase);
  if (phase == GC_PHASE_MARK)
    {
      /* Filter out killed buffers from both buffer lists in attempt
         to help GC to reclaim killed buffers faster.  We can do it
         elsewhere for live windows, but this is the best place to do
         it for dead windows.  */
      discard_killed_buffers (&w->prev_buffers);
      discard_killed_buffers (&w->next_buffers);
    }
  scan_reference (&w->prev_buffers, phase);
  scan_reference (&w->next_buffers, phase);
}

ptrdiff_t
hash_index_size (const struct Lisp_Hash_Table *const h,
                 const ptrdiff_t size)
{
  const double threshold = h->rehash_threshold;
  const double index_float = size / threshold;
  const ptrdiff_t index_size = (index_float < (double) VECTOR_ELTS_MAX + 1
                                ? next_almost_prime (index_float)
                                : VECTOR_ELTS_MAX + 1);
  if (VECTOR_ELTS_MAX < index_size)
    error ("Hash table too large");
  return index_size;
}

/* Resize hash table H if it's too full.  If H cannot be resized
   because it's already too large, throw an error.  This function is
   in alloc.c, not fns.c, because we want to use GC-internal
   uninitialized allocation routines.  */
void
maybe_resize_hash_table (struct Lisp_Hash_Table *h)
{
  if (h->next_free >= 0)
    return;
  ptrdiff_t old_size = HASH_TABLE_SIZE (h);
  EMACS_INT new_size;
  double rehash_size = h->rehash_size;

  if (rehash_size < 0)
    new_size = old_size - rehash_size;
  else
    {
      double float_new_size = old_size * (rehash_size + 1);
      if (float_new_size < (double) EMACS_INT_MAX)
        new_size = float_new_size;
      else
        new_size = EMACS_INT_MAX;
    }
  if (PTRDIFF_MAX < new_size)
    new_size = PTRDIFF_MAX;
  if (new_size <= old_size)
    new_size = old_size + 1;

  /* Allocate all the new vectors before updating *H, to
     avoid problems if memory is exhausted.  larger_vecalloc
     finishes computing the size of the replacement vectors.  */
  Lisp_Object next;
  ptrdiff_t next_size;
  {
    gc_vector_allocation vr =
      larger_vecalloc (XVECTOR (h->next), new_size - old_size, new_size);
    next = gc_vector_allocation_lv (vr);
    next_size = ASIZE (next);
    for (ptrdiff_t i = old_size; i < next_size - 1; i++)
      ASET (next, i, make_fixnum (i + 1));
    ASET (next, next_size - 1, make_fixnum (-1));
    gc_vector_allocation_commit (&vr);
  }

  /* Build the new&larger key_and_value vector, making sure the new
     fields are initialized to `unbound`.  */
  Lisp_Object key_and_value;
  {
    gc_vector_allocation vr =
      larger_vecalloc (XVECTOR (h->key_and_value),
                       2 * (next_size - old_size),
                       2 * next_size);
    key_and_value = gc_vector_allocation_lv (vr);
    for (ptrdiff_t i = 2 * old_size; i < 2 * next_size; i++)
      ASET (key_and_value, i, Qunbound);
    gc_vector_allocation_commit (&vr);
  }

  Lisp_Object hash = larger_vector (h->hash, next_size - old_size,
                                    next_size);
  ptrdiff_t index_size = hash_index_size (h, next_size);
  h->index = make_vector (index_size, make_fixnum (-1));
  h->key_and_value = key_and_value;
  h->hash = hash;
  h->next = next;
  h->next_free = old_size;

  /* Rehash.  */
  for (ptrdiff_t i = 0; i < old_size; i++)
    if (!NILP (HASH_HASH (h, i)))
      {
        EMACS_UINT hash_code = XUFIXNUM (HASH_HASH (h, i));
        ptrdiff_t start_of_bucket = hash_code % ASIZE (h->index);
        set_hash_next_slot (h, i, HASH_INDEX (h, start_of_bucket));
        set_hash_index_slot (h, start_of_bucket, i);
      }

  if (enable_checking &&
      HASH_TABLE_P (Vpurify_flag) &&
      XHASH_TABLE (Vpurify_flag) == h)
    message ("Growing hash table to: %"pD"d", next_size);
}

void
scan_hash_table (struct Lisp_Hash_Table *const h, const gc_phase phase)
{
  scan_vector_lisp_fields (&h->header, phase);
  scan_reference (&h->test.name, phase);
  scan_reference (&h->test.user_hash_function, phase);
  scan_reference (&h->test.user_cmp_function, phase);
  /* If hash table is not weak, mark all keys and values.  For weak
     tables, mark only the vector and not its contents --- that's what
     makes it weak.  */
  if (NILP (h->weak))
    scan_reference (&h->key_and_value, phase);
  else
    {
      if (phase == GC_PHASE_MARK)
        {
          eassert (h->next_weak == NULL);
          h->next_weak = weak_hash_tables;
          weak_hash_tables = h;
          /* We want the key_and_value vector to survive GC but don't
             want to scan its contents in the normal way, so directly
             set it the state without actually scanning it.
             We inspect the contents of the table in
             mark_strong_references_of_reachable_weak_objects.  Don't
             assert that the vector isn't already marked: conservative
             GC is allowed to mark anything.  */
          if (!vectorlike_marked_p (&XVECTOR (h->key_and_value)->header))
            set_vectorlike_marked (&XVECTOR (h->key_and_value)->header);
        }
      else
        scan_reference (&h->key_and_value, phase);
    }
  /* Array of values: pop them left to right, which seems to be slightly
     faster than right to left.  */
  e->n--;
  if (e->n == 0)
    --mark_stk.sp;		/* last value consumed */
  return (++e->u.values)[-1];
}

void
scan_cons (struct Lisp_Cons *const ptr, const gc_phase phase)
{
  scan_reference (&ptr->u.s.car, phase);
  scan_reference (&ptr->u.s.cdr, phase);
}

void
scan_terminal_display_info (struct terminal *const t, const gc_phase phase)
{
  switch (t->type)
    {
    case output_initial:
      return;
    case output_termcap:
      scan_terminal_display_info_tty (t->display_info.tty, phase);
      return;
    case output_x_window:
#ifdef HAVE_X_WINDOWS
      scan_terminal_display_info_x (t->display_info.x, phase);
      return;
#else
      emacs_unreachable ();
#endif
    case output_msdos_raw:
#ifdef DOS_NT
      scan_terminal_display_info_tty (t->display_info.tty, phase);
      return;
#else
      emacs_unreachable ();
#endif
    case output_w32:
#ifdef HAVE_NTGUI
      scan_terminal_display_info_w32 (t->display_info.w32, phase);
      return;
#else
      emacs_unreachable ();
#endif
    case output_ns:
#ifdef HAVE_NS
      scan_terminal_display_info_ns (t->display_info.ns, phase);
      return;
#else
      emacs_unreachable ();
#endif
    }
  emacs_unreachable ();
}

void
scan_terminal (struct terminal *const t, const gc_phase phase)
{
  scan_vector_lisp_fields (&t->header, phase);
  scan_reference_pointer_to_vectorlike (&t->next_terminal, phase);
  scan_terminal_display_info (t, phase);
  if (phase == GC_PHASE_SWEEP && TERMINAL_FONT_CACHEP (t))
    scan_reference (TERMINAL_FONT_CACHEP (t), phase);
#ifdef HAVE_WINDOW_SYSTEM
  scan_image_cache (t->image_cache, phase);
#endif /* HAVE_WINDOW_SYSTEM */
}

void
scan_vectorlike (union vectorlike_header *const ptr, const gc_phase phase)
{
  const enum pvec_type pvectype =
    PSEUDOVECTOR_TYPE ((struct Lisp_Vector *) ptr);

  /* All pseudovectors get their lisp slots scanned.  */

  /* Some pseudovectors need special treatment.  */
  switch (pvectype)
    {
    case PVEC_NORMAL_VECTOR:
      scan_vector_lisp_fields (ptr, phase);
      return;

    case PVEC_MARKER:
      scan_marker ((struct Lisp_Marker *) ptr, phase);
      return;

    case PVEC_OVERLAY:
      scan_overlay ((struct Lisp_Overlay *) ptr, phase);
      return;

    case PVEC_FINALIZER:
      scan_vector_lisp_fields (ptr, phase);
      return;

    case PVEC_MISC_PTR:
    case PVEC_USER_PTR:
    case PVEC_BIGNUM:
    case PVEC_STRING_DATA:
      scan_pseudovector_empty (ptr, phase);
      return;

    case PVEC_PROCESS:
      scan_vector_lisp_fields (ptr, phase);
      return;

    case PVEC_FRAME:
      scan_frame ((struct frame *) ptr, phase);
      return;

    case PVEC_WINDOW:
      scan_window ((struct window *) ptr, phase);
      return;

    case PVEC_BOOL_VECTOR:
      return;

    case PVEC_BUFFER:
      scan_buffer ((struct buffer *) ptr, phase);
      return;

    case PVEC_HASH_TABLE:
      scan_hash_table ((struct Lisp_Hash_Table *) ptr, phase);
      return;

    case PVEC_TERMINAL:
      scan_terminal ((struct terminal *) ptr, phase);
      return;

    case PVEC_WINDOW_CONFIGURATION:
      scan_vector_lisp_fields (ptr, phase);
      return;

    case PVEC_SUBR:
      return;

    case PVEC_OTHER:
    case PVEC_XWIDGET:
    case PVEC_XWIDGET_VIEW:
      scan_vector_lisp_fields (ptr, phase);
      return;

    case PVEC_THREAD:
      /* The main thread lives in the data section, not the heap. It has no
         mark bit.  We scan it as a root.  */
      eassume (!main_thread_p (ptr));
      scan_thread ((struct thread_state *) ptr, phase);
      return;

    case PVEC_MUTEX:
    case PVEC_CONDVAR:
    case PVEC_MODULE_FUNCTION:
    case PVEC_COMPILED:
      scan_vector_lisp_fields (ptr, phase);
      return;

    case PVEC_CHAR_TABLE:
    case PVEC_SUB_CHAR_TABLE:
      /* Can't use scan_vector_lisp_fields here due to non-zero offset.  */
      scan_char_table ((struct Lisp_Vector *) ptr, pvectype, phase);
      return;

    case PVEC_RECORD:
    case PVEC_FONT:
      scan_vector_lisp_fields (ptr, phase);
      return;
    }
  emacs_unreachable ();
}

void
scan_object (void *const obj,
             const enum Lisp_Type type,
             const gc_phase phase)
{
  switch (type)
    {
    case Lisp_String:
      scan_string (obj, phase);
      return;

    case Lisp_Vectorlike:
      scan_vectorlike (obj, phase);
      return;

    case Lisp_Symbol:
      scan_symbol (obj, phase);
      return;

    case Lisp_Cons:
      scan_cons (obj, phase);
      return;

    case Lisp_Int0:
      scan_interval (obj, phase);
      return;

    case Lisp_Int1:
      emacs_unreachable ();

    case Lisp_Float:
      return;  /* Nothing to scan.  */

    case Lisp_Type_Unused0:
      emacs_unreachable ();
    }
  emacs_unreachable ();
}

void
scan_object_for_marking_ool (void *const obj, const enum Lisp_Type type)
{
  scan_object (obj, type, GC_PHASE_MARK);
}

void
scan_object_for_sweeping_ool (void *const obj, const enum Lisp_Type type)
{
  scan_object (obj, type, GC_PHASE_SWEEP);
}

void
scan_reference (Lisp_Object *const refp, const gc_phase phase)
{
  eassume (phase == current_gc_phase);
  eassume (phase == GC_PHASE_MARK || phase == GC_PHASE_SWEEP);
  if (phase == GC_PHASE_MARK)
    gc_mark_or_enqueue (*refp);
  if (phase == GC_PHASE_SWEEP)
    gc_any_object_point_into_tospace (refp);
}

void
xscan_reference (Lisp_Object *const refp, const gc_phase phase)
{
  scan_reference (refp, phase);
}

void
scan_reference_pinned (const Lisp_Object ref, const gc_phase phase)
{
  eassume (phase == current_gc_phase);
  eassume (phase == GC_PHASE_MARK || phase == GC_PHASE_SWEEP);
  if (phase == GC_PHASE_MARK)
    {
      gc_mark_or_enqueue (ref);
      gc_any_object_pin (ref);
    }
}

void
xscan_reference_pinned (const Lisp_Object ref, const gc_phase phase)
{
  scan_reference_pinned (ref, phase);
}

void
scan_reference_interval_pinned (const INTERVAL i, const gc_phase phase)
{
  eassume (phase == current_gc_phase);
  eassume (phase == GC_PHASE_MARK || phase == GC_PHASE_SWEEP);
  if (i && phase == GC_PHASE_MARK)
    {
      gc_mark_or_enqueue_interval (i);
      set_interval_pinned (i);
    }
}

void
scan_reference_pointer_to_interval (INTERVAL *const ip, const gc_phase phase)
{
  eassume (phase == current_gc_phase);
  eassume (phase == GC_PHASE_MARK || phase == GC_PHASE_SWEEP);
  if (!*ip)
    return;
  if (phase == GC_PHASE_MARK)
    gc_mark_or_enqueue_interval (*ip);
  if (phase == GC_PHASE_SWEEP)
    *ip = point_interval_into_tospace (*ip);
}

void
scan_reference_pointer_to_symbol (struct Lisp_Symbol **const s,
                                  const gc_phase phase)
{
  eassume (phase == current_gc_phase);
  eassume (phase == GC_PHASE_MARK || phase == GC_PHASE_SWEEP);

  if (!*s)
    return;
  if (phase == GC_PHASE_MARK)
    gc_mark_or_enqueue_symbol (*s);
  if (phase == GC_PHASE_SWEEP)
    gc_any_object_point_into_tospace_pointer (s, Lisp_Symbol);
}

void
scan_reference_pointer_to_vectorlike_1 (
  void *const ptr, union vectorlike_header *const hdr, const gc_phase phase)
{
  (void) hdr;
  scan_reference_pointer_to_vectorlike_2 (ptr, phase);
}

void
scan_reference_pointer_to_vectorlike_2 (union vectorlike_header **const hptr,
                                        const gc_phase phase)
{
  eassume (phase == current_gc_phase);
  eassume (phase == GC_PHASE_MARK || phase == GC_PHASE_SWEEP);

  if (!*hptr)
    return;

  if (phase == GC_PHASE_MARK)
    gc_mark_or_enqueue_vectorlike (*hptr);
  if (phase == GC_PHASE_SWEEP)
    gc_any_object_point_into_tospace_pointer (hptr, Lisp_Vectorlike);
}

void
xscan_reference_pointer_to_vectorlike_2 (union vectorlike_header **const hptr,
                                         const gc_phase phase)
{
  scan_reference_pointer_to_vectorlike_2 (hptr, phase);
}

Lisp_Object
gc_interval_smuggle (const INTERVAL i)
{
  /* We never need to actually mark integers, so use the int tag to
     indicate that we really have an interval.  */
  const Lisp_Object obj = make_lisp_ptr (i, Lisp_Int0);
  eassert (gc_interval_unsmuggle (obj) == i);
  return obj;
}

INTERVAL
gc_interval_unsmuggle (const Lisp_Object obj)
{
  eassume (XTYPE (obj) == Lisp_Int0);
  return XUNTAG (obj, Lisp_Int0, struct interval);
}

bool
survives_gc_p (const Lisp_Object obj)
{
  switch (XTYPE (obj))
    {
    case Lisp_Symbol:
      return symbol_marked_p (XSYMBOL (obj));
    case Lisp_Type_Unused0:
      emacs_unreachable ();
    case Lisp_Int0:
    case Lisp_Int1:
      return true;
    case Lisp_String:
      return string_marked_p (XSTRING (obj));
    case Lisp_Vectorlike:
      return vectorlike_marked_p (&XVECTOR (obj)->header);
    case Lisp_Cons:
      return cons_marked_p (XCONS (obj));
    case Lisp_Float:
      return float_marked_p (XFLOAT (obj));
    }

  emacs_unreachable ();
}

EMACS_INT
gc_any_object_identity_hash_code (const Lisp_Object obj)
{
  switch (XTYPE (obj))
    {
    case Lisp_Symbol:
      return symbol_identity_hash_code (XSYMBOL (obj));
    case Lisp_Type_Unused0:
      emacs_unreachable ();
    case Lisp_Int0:
    case Lisp_Int1:
      return XUFIXNUM_RAW (obj);
    case Lisp_String:
      return string_identity_hash_code (XSTRING (obj));
    case Lisp_Vectorlike:
      return vector_identity_hash_code (XVECTOR (obj));
    case Lisp_Cons:
      return cons_identity_hash_code (XCONS (obj));
    case Lisp_Float:
      return float_identity_hash_code (XFLOAT (obj));
    }
  emacs_unreachable ();
}

void
gc_any_object_pin (const Lisp_Object obj)
{
  switch (XTYPE (obj))
    {
    case Lisp_Symbol:
      set_symbol_pinned (XSYMBOL (obj));
      return;
    case Lisp_Type_Unused0:
    case Lisp_Int0:
    case Lisp_Int1:
      emacs_unreachable ();
    case Lisp_String:
      set_string_pinned (XSTRING (obj));
      return;
    case Lisp_Vectorlike:
      /* survives_p = */
      /* 	(SUBRP (obj) && !SUBR_NATIVE_COMPILEDP (obj)) || */
      /* 	vector_marked_p (XVECTOR (obj)); */
      /* break; */
      set_vectorlike_pinned (&XVECTOR (obj)->header);
      return;
    case Lisp_Cons:
      set_cons_pinned (XCONS (obj));
      return;
    case Lisp_Float:
      set_float_pinned (XFLOAT (obj));
      return;
    }
  emacs_unreachable ();
}

/* Make the object reference at *OBJP point into tospace.  */
void
gc_any_object_point_into_tospace (Lisp_Object *const objp)
{
  /* Compiler will optimize out the writes for mark-sweep heaps.  */
  eassume (current_gc_phase == GC_PHASE_SWEEP);
  switch (XTYPE (*objp))
    {
    case Lisp_Symbol:
      *objp = make_lisp_symbol (
        point_symbol_into_tospace (XSYMBOL (*objp)));
      return;
    case Lisp_Type_Unused0:
      emacs_unreachable ();
    case Lisp_Int0:
    case Lisp_Int1:
      return;
    case Lisp_String:
      *objp = make_lisp_ptr (
        point_string_into_tospace (XSTRING (*objp)), Lisp_String);
      return;
    case Lisp_Vectorlike:
      *objp = make_lisp_ptr (
        point_vectorlike_into_tospace (&XVECTOR (*objp)->header),
        Lisp_Vectorlike);
      return;
    case Lisp_Cons:
      *objp = make_lisp_ptr (
        point_cons_into_tospace (XCONS (*objp)), Lisp_Cons);
      return;
    case Lisp_Float:
      *objp = make_lisp_ptr (
        point_float_into_tospace (XFLOAT (*objp)), Lisp_Float);
      return;
    }
  emacs_unreachable ();
}

void
gc_any_object_point_into_tospace_pointer (void *const ptrpx,
                                          const enum Lisp_Type type)
{
  /* Compilerswill optimize out the writes for mark-sweep heaps.  */
  eassume (current_gc_phase == GC_PHASE_SWEEP);
  void **const ptrp = ptrpx;
  void *const ptr = *ptrp;
  switch (type)
    {
    case Lisp_Symbol:
      *ptrp = point_symbol_into_tospace (ptr);
      return;
    case Lisp_Type_Unused0:
    case Lisp_Int0:
    case Lisp_Int1:
      emacs_unreachable ();
    case Lisp_String:
      *ptrp = point_string_into_tospace (ptr);
      return;
    case Lisp_Vectorlike:
      *ptrp = point_vectorlike_into_tospace (ptr);
      return;
    case Lisp_Cons:
      *ptrp = point_cons_into_tospace (ptr);
      return;
    case Lisp_Float:
      *ptrp = point_float_into_tospace (ptr);
      return;
    }
  emacs_unreachable ();
}

bool
gc_mark_queue_block_try_add (gc_block *const b, const Lisp_Object obj)
{
  eassume (b->meta.u.aux.mark_queue.nr_reads <=
           b->meta.u.aux.mark_queue.nr_writes);
  const size_t size = b->meta.u.aux.mark_queue.nr_writes -
    b->meta.u.aux.mark_queue.nr_reads;
  eassume (size <= gc_block_nr_mark_queue_entries);
  const size_t free = gc_block_nr_mark_queue_entries - size;
  if (free == 0)
    return false;
  const size_t slot = b->meta.u.aux.mark_queue.nr_writes %
    gc_block_nr_mark_queue_entries;
  b->u.mark_queue[slot] = obj;
  b->meta.u.aux.mark_queue.nr_writes += 1;
  return true;
}

bool
gc_mark_queue_block_try_remove (gc_block *const b,
                                Lisp_Object *const out)
{
  eassume (b->meta.u.aux.mark_queue.nr_reads <=
           b->meta.u.aux.mark_queue.nr_writes);
  const size_t size = b->meta.u.aux.mark_queue.nr_writes -
    b->meta.u.aux.mark_queue.nr_reads;
  eassume (size <= gc_block_nr_mark_queue_entries);
  if (size == 0)
    return false;
  const size_t slot = b->meta.u.aux.mark_queue.nr_reads %
    gc_block_nr_mark_queue_entries;
  *out = b->u.mark_queue[slot];
  b->meta.u.aux.mark_queue.nr_reads += 1;
  return true;
}

void
gc_mark_enqueue_1 (const Lisp_Object obj)
{
  for (emacs_list_link *bl = gc_mark_queue.link.prev;
       bl != &gc_mark_queue.link;
       bl = bl->prev)
    {
      if (gc_mark_queue_block_try_add (gc_block_from_link (bl), obj))
        return;
    }
  gc_block *const b = gc_aux_pop ();
  memset (&b->meta.u.aux.mark_queue, 0, sizeof (b->meta.u.aux.mark_queue));
  emacs_list_insert_last (&gc_mark_queue, &b->meta.link);
  const bool success = gc_mark_queue_block_try_add (b, obj);
  eassume (success);
}

void
gc_mark_or_enqueue (Lisp_Object obj)
{
  switch (XTYPE (obj))
    {
    case Lisp_Symbol:
      gc_mark_or_enqueue_symbol (XSYMBOL (obj));
      return;
    case Lisp_Type_Unused0:
      emacs_unreachable ();
    case Lisp_Int0:
    case Lisp_Int1:
      return;
    case Lisp_String:
      gc_mark_or_enqueue_string (XSTRING (obj));
      return;
    case Lisp_Vectorlike:
      gc_mark_or_enqueue_vectorlike (&XVECTOR (obj)->header);
      return;
    case Lisp_Cons:
      gc_mark_or_enqueue_cons (XCONS (obj));
      return;
    case Lisp_Float:
      gc_mark_or_enqueue_float (XFLOAT (obj));
      return;
    }
  emacs_unreachable ();
}

bool
gc_mark_drain_one (Lisp_Object *const out)
{
  emacs_list_link *bl = gc_mark_queue.link.next;
  while (bl != &gc_mark_queue.link)
    {
      gc_block *const b = gc_block_from_link (bl);
      if (gc_mark_queue_block_try_remove (b, out))
        return true;
      bl = bl->next;
      emacs_list_remove (&b->meta.link);
      gc_aux_push (b);
    }
  return false;
}

NO_INLINE
void
gc_mark_drain_queue (void)
{
  Lisp_Object obj;
  while (gc_mark_drain_one (&obj))
    {
      const enum Lisp_Type type = XTYPE (obj);
      void *const ptr = (type == Lisp_Int0)
        ? gc_interval_unsmuggle (obj)
        : XPNTR (obj);
      scan_object_for_marking_ool (ptr, type);
    }
}

bool
gc_object_is_marked (const void *const obj, const gc_heap *const h)
{
  return gc_cursor_is_object_marked (gc_object_to_cursor (obj, h), h);
}

void
gc_object_set_marked (void *const obj, const gc_heap *const h)
{
  gc_cursor_set_object_marked (gc_object_to_cursor (obj, h), h);
}

void
gc_object_set_pinned (void *const obj, const gc_heap *const h)
{
  gc_cursor_set_object_pinned (gc_object_to_cursor (obj, h), h);
}

void
gc_object_perma_pin (void *const obj, const gc_heap *const h)
{
  gc_cursor_perma_pin_object (gc_object_to_cursor (obj, h), h);
}

void *
gc_object_point_into_tospace (void *const obj, const gc_heap *h)
{
  eassert (!gc_pdumper_object_p (obj));
  if (!h->use_moving_gc)
    return obj;
  const gc_cursor c = gc_object_to_cursor (obj, h);
  if (!gc_cursor_is_gen_y (c, h))
    return gc_cursor_to_object (c, h);
  const gc_locator tospace = gc_cursor_get_object_tospace_locator (c, h);
  const gc_cursor new_c = gc_locator_to_cursor (tospace, h);

  return gc_cursor_to_object (new_c, h);
}

/* Return a cursor pointing to the given object, which must exist in
   the given heap H.  OBJ must not be an interior pointer.  */
gc_cursor
gc_object_to_cursor (const void *const obj, const gc_heap *const h)
{
  eassert (!gc_pdumper_object_p (obj));
  eassert (!main_thread_p (obj));
  const uintptr_t obj_la = (uintptr_t) obj;
  const uintptr_t block_la = h->aligned_blocks
    ? (obj_la / gc_block_data_nr_bytes) * gc_block_data_nr_bytes
    : (uintptr_t) gc_block_from_mem_node (mem_find (obj));
  eassume (block_la % GCALIGNMENT == 0);
  eassume (obj_la >= block_la);
  const uintptr_t offset = obj_la - block_la;
  eassume (offset % GCALIGNMENT == 0);
  eassume (offset < gc_block_data_nr_bytes);
  eassume (h->homogeneous_object_nr_bytes == 0 ||
           offset % h->homogeneous_object_nr_bytes == 0);
  const gc_cursor c = {
    .block = gc_block_check ((gc_block *) block_la),
    .slot_nr = offset / gc_heap_nr_bytes_per_slot (h),
  };
  gc_cursor_check (c, h);
  return c;
}



/* Remove BUFFER's markers that are due to be swept.  This is needed since
   we treat BUF_MARKERS and markers's `next' field as weak pointers.  */
void
unchain_dead_markers (struct buffer *buffer)
{
  if (buffer->base_buffer)
    return;  /* The base buffer owns its text.  */
  struct Lisp_Marker *this, **prev = &BUF_MARKERS (buffer);

  while ((this = *prev))
    {
      eassert (PSEUDOVECTOR_TYPEP (&this->header, PVEC_MARKER));
      if (vectorlike_marked_p (&this->header))
        prev = &this->next;
      else
        {
          this->buffer = NULL;
          *prev = this->next;
        }
    }
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
  return list4i ((uintmax_t) si.totalram * units / 1024,
		 (uintmax_t) si.freeram * units / 1024,
		 (uintmax_t) si.totalswap * units / 1024,
		 (uintmax_t) si.freeswap * units / 1024);
#elif defined WINDOWSNT
  unsigned long long totalram, freeram, totalswap, freeswap;

  if (w32_memory_info (&totalram, &freeram, &totalswap, &freeswap) == 0)
    return list4i ((uintmax_t) totalram / 1024,
		   (uintmax_t) freeram / 1024,
		   (uintmax_t) totalswap / 1024,
		   (uintmax_t) freeswap / 1024);
  else
    return Qnil;
#elif defined MSDOS
  unsigned long totalram, freeram, totalswap, freeswap;

  if (dos_memory_info (&totalram, &freeram, &totalswap, &freeswap) == 0)
    return list4i ((uintmax_t) totalram / 1024,
		   (uintmax_t) freeram / 1024,
		   (uintmax_t) totalswap / 1024,
		   (uintmax_t) freeswap / 1024);
  else
    return Qnil;
#else /* not HAVE_LINUX_SYSINFO, not WINDOWSNT, not MSDOS */
  /* FIXME: add more systems.  */
  return Qnil;
#endif /* HAVE_LINUX_SYSINFO, not WINDOWSNT, not MSDOS */
}

/* Debugging aids.  */

DEFUN ("memory-use-counts", Fmemory_use_counts, Smemory_use_counts, 0, 0, 0,
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

bool
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

struct which_symbols_data {
  Lisp_Object obj;
  Lisp_Object found;
  EMACS_INT find_max;
  EMACS_INT found_so_far;
};

bool
which_symbols_callback (void *const obj, void *const data_p)
{
  struct which_symbols_data *const data = data_p;
  if (data->found_so_far >= data->find_max)
    return false;
  const Lisp_Object sym = make_lisp_symbol (obj);
  if (symbol_uses_obj (sym, data->obj))
    {
      data->found = Fcons (sym, data->found);
      data->found_so_far += 1;
    }
  return true;
}

Lisp_Object
which_symbols (const Lisp_Object obj, const EMACS_INT find_max)
{
  const ptrdiff_t gc_count = inhibit_garbage_collection ();
  struct which_symbols_data data = {
    .obj = obj,
    .found = Qnil,
    .find_max = find_max,
    .found_so_far = 0,
  };
  for (int i = 0; i < ARRAYELTS (lispsym); i++)
    if (!which_symbols_callback (XSYMBOL (builtin_lisp_symbol (i)), &data))
      break;
  gc_heap_enumerate (&gc_symbol_heap, which_symbols_callback, &data);
  return unbind_to (gc_count, data.found);
}

DEFUN ("suspicious-object", Fsuspicious_object, Ssuspicious_object, 1, 1, 0,
       doc: /* Return OBJ, maybe marking it for extra scrutiny.
If Emacs is compiled with suspicious object checking, capture
a stack trace when OBJ is freed in order to help track down
garbage collection bugs.  Otherwise, do nothing and return OBJ.   */)
   (Lisp_Object obj)
{
  return obj;                   /* XXXX: declare obsolete */
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

NO_INLINE static void
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

/* Initialization.  */

void
init_alloc_once (void)
{
  gc_cons_threshold = gc_default_threshold;
  /* Even though Qt's contents are not set up, its address is known.  */
  Vpurify_flag = Qt;

  PDUMPER_REMEMBER_SCALAR (buffer_defaults.header);
  PDUMPER_REMEMBER_SCALAR (buffer_local_symbols.header);

  /* Call init_alloc_once_for_pdumper now so we run init early.
     Keep in mind that when we reload from a dump, we'll run _only_
     init_alloc_once_for_pdumper and not init_alloc_once at all.  */
  pdumper_do_now_and_after_load (init_alloc_once_for_pdumper);

  Vloadup_pure_table = CALLN (Fmake_hash_table, QCtest, Qequal, QCsize,
                              make_fixed_natnum (80000));
  update_bytes_between_gc ();

  verify_alloca ();

  init_strings ();
  init_vectors ();
}

void
init_alloc_once_for_pdumper (void)
{
  verify (NIL_IS_ZERO);

  gc_heap_init (&gc_float_heap);
  gc_heap_init (&gc_cons_heap);
  gc_heap_init (&gc_symbol_heap);
  gc_heap_init (&gc_interval_heap);
  gc_heap_init (&gc_string_heap);
  gc_heap_init (&gc_vector_heap);

  refill_memory_reserve ();

  /* Make sure we have enough mark queue depth for all the built-in
     objects. */
  const size_t nr_builtin_symbols = sizeof (lispsym) / sizeof (lispsym[0]);
  if (!gc_object_limit_try_increase (nr_builtin_symbols + EMACS_NUM_SUBRS))
    emacs_abort ();

  /* Now that we've initialized the heap, we can allow GC.

     XXX: allow GC later, after all initialization done
     */
  recompute_consing_until_gc ();
  eassume (garbage_collection_inhibited >= 0);
  garbage_collection_inhibited -= 1;
}

void
syms_of_alloc (void)
{
  static struct Lisp_Objfwd const o_fwd
    = {Lisp_Fwd_Obj, &Vmemory_full};
  staticpro (&Vmemory_full);
  Vmemory_full = Qnil;
  defvar_lisp (&o_fwd, "memory-full");

  DEFVAR_INT ("gc-cons-threshold", gc_cons_threshold,
	      doc: /* Number of bytes of consing between garbage collections.
Garbage collection can happen automatically once this many bytes have been
allocated since the last garbage collection.  All data types count.

Garbage collection happens automatically only when `eval' is called.

By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.
See also `gc-cons-percentage'.  */);
  DEFVAR_LISP ("gc-cons-percentage-major",
               Vgc_cons_percentage_major,
               doc: /* Heap growth that triggers a major collection.
If the heap has grown by more than this amount since the last major
collection, run another major collection pass.  If this value
is less than or equal to `gc-cons-percentage', every collection
is a major collection.  */);
  Vgc_cons_percentage_major = make_float (gc_cons_percentage_major_default);

  DEFVAR_LISP ("gc-cons-percentage", Vgc_cons_percentage,
	       doc: /* Portion of the heap used for allocation.
Garbage collection can happen automatically once this portion of the heap
has been allocated since the last garbage collection.
If this portion is smaller than `gc-cons-threshold', this is ignored.  */);
  Vgc_cons_percentage = make_float (gc_cons_percentage_minor_default);

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
    = list (Qerror,
            build_c_string ("Memory exhausted--use"
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
  //  defsubr (&Sgarbage_collect_maybe);
  defsubr (&Smemory_info);
  defsubr (&Smemory_full);
  defsubr (&Smemory_use_counts);
#if defined GNU_LINUX && defined __GLIBC__ && \
  (__GLIBC__ > 2 || __GLIBC_MINOR__ >= 10)

  defsubr (&Smalloc_info);
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
