/* Copyright (C) 2018-2024 Free Software Foundation, Inc.

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
   A primordial run of ../admin/pdump.el initializes emacs from scratch
   then immediately dumps the heap of Lisp objects.  Subsequent emacs
   invocations then memory map this dump for fast startup.

   A dump file is precisely coupled to the Emacs binary that produced
   it, so gotchas of alignment and endianness should not arise.

   Relocations adjust pointers within the dump to account for the new
   process's address space.

   Things may make more sense if you prepend "on_load" to all the
   reloc_* function names.
*/

#include <config.h>

#include <fcntl.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/types.h>

#include "blockinput.h"
#include "buffer.h"
#include "charset.h"
#include "coding.h"
#include "frame.h"
#include "intervals.h"
#include "pdumper.h"
#include "window.h"
#include "sysstdio.h"
#include "thread.h"
#include "bignum.h"

#ifdef CHECK_STRUCTS
# include "dmpstruct.h"
#endif

#if GNUC_PREREQ (4, 7, 0)
# pragma GCC diagnostic error "-Wshadow"
#endif

/* Require an architecture in which pointers, ptrdiff_t and intptr_t
   are the same size and have the same layout, and where bytes have
   eight bits --- that is, a general-purpose computer made after 1990.
   Also require Lisp_Object to be at least as wide as pointers.  */
verify (sizeof (ptrdiff_t) == sizeof (void *));
verify (sizeof (intptr_t) == sizeof (ptrdiff_t));
verify (sizeof (void (*) (void)) == sizeof (void *));
verify (sizeof (ptrdiff_t) <= sizeof (Lisp_Object));
verify (sizeof (ptrdiff_t) <= sizeof (EMACS_INT));

struct pdumper_info pdumper_info;
pdumper_hook dump_hooks[24];
int nr_dump_hooks;
const char dump_magic[16] = {
  'D', 'U', 'M', 'P', 'E', 'D',
  'G', 'N', 'U',
  'E', 'M', 'A', 'C', 'S'
};

static struct
{
  void *mem;
  int sz;
} remembered_data[32];
static int nr_remembered_data = 0;

static void ATTRIBUTE_FORMAT_PRINTF (1, 2)
dump_trace (const char *fmt, ...)
{
  if (0)
    {
      va_list args;
      va_start (args, fmt);
      vfprintf (stderr, fmt, args);
      va_end (args);
    }
}

static inline dump_off
DUMP_OFF (ptrdiff_t value)
{
  eassert (DUMP_OFF_MIN <= value);
  eassert (value <= DUMP_OFF_MAX);
  return (dump_off) value;
}

#define DUMP_OFFSETOF(type, member) (DUMP_OFF (offsetof (type, member)))

/* Double-ended singly linked list.  */
struct tailq
{
  Lisp_Object head;
  Lisp_Object tail;
  intptr_t length;
};

/* Queue of objects to dump.  */
struct queue
{
  /* Objects with no link weights at all.  Kept in dump order.  */
  struct tailq zero_weight_objects;
  /* Objects with simple link weight: just one entry of type
     WEIGHT_NORMAL.  Score in this special case is non-decreasing as
     position increases, so we can avoid the need to rescan a big list
     for each object by storing these objects in order.  */
  struct tailq one_weight_normal_objects;
  /* Likewise, for objects with one WEIGHT_STRONG weight.  */
  struct tailq one_weight_strong_objects;
  /* List of objects with complex link weights --- i.e., not one of
     the above cases.  Order is irrelevant, since we scan the whole
     list every time.  Relatively few objects end up here.  */
  struct tailq fancy_weight_objects;
  /* Hash table of link weights: maps an object to a list of zero or
     more (BASIS . WEIGHT) pairs.  As a special case, an object with
     zero weight is marked by Qt in the hash table --- this way, we
     can distinguish objects we've seen but that have no weight from
     ones that we haven't seen at all.  */
  Lisp_Object link_weights;
  /* Hash table mapping object to a sequence number --- used to
     resolve ties.  */
  Lisp_Object sequence_numbers;
  dump_off next_sequence_number;
};

enum cold_op
  {
    COLD_OP_OBJECT,
    COLD_OP_STRING,
    COLD_OP_CHARSET,
    COLD_OP_BUFFER,
    COLD_OP_BIGNUM,
    COLD_OP_NATIVE_SUBR,
  };

/* This structure controls what operations we perform inside
   dump_object.  */
struct dump_flags
{
  /* Actually write object contents to the dump.  Without this flag
     set, we still scan objects and enqueue pointed-to objects; making
     this flag false is useful when we want to process an object's
     referents normally, but dump an object itself separately,
     later.  */
  bool_bf dump_object_contents : 1;
  /* Record object starts. We turn this flag off when writing to the
     discardable section so that we don't trick conservative GC into
     thinking we have objects there.  Ignored (we never record object
     starts) if dump_object_contents is false.  */
  bool_bf record_object_starts : 1;
  /* Pack objects tighter than GC memory alignment would normally
     require.  Useful for objects copied into the Emacs image instead
     of used directly from the loaded dump.
  */
  bool_bf pack_objects : 1;
  /* Sometimes we dump objects that we've already scanned for outbound
     references to other objects.  These objects should not cause new
     objects to enter the object dumping queue.  This flag causes Emacs
     to assert that no new objects are enqueued while dumping.  */
  bool_bf assert_already_seen : 1;
  /* Punt on unstable hash tables: defer them to ctx->deferred_hash_tables.  */
  bool_bf defer_hash_tables : 1;
  /* Punt on symbols: defer them to ctx->deferred_symbols.  */
  bool_bf defer_symbols : 1;
  /* Punt on cold objects: defer them to ctx->cold_queue.  */
  bool_bf defer_cold_objects : 1;
  /* Punt on copied objects: defer them to ctx->copied_queue.  */
  bool_bf defer_copied_objects : 1;
};

/* Information we use while we dump.  Note that we're not the garbage
   collector and can operate under looser constraints: specifically,
   we allocate memory during the dumping process.  */
struct dump_context
{
  /* Header we'll write to the dump file when done.  */
  struct dump_header header;
  /* Data that will be written to the dump file.  */
  void *buf;
  dump_off buf_size;
  dump_off max_offset;

  Lisp_Object restore_pure_pool;
  Lisp_Object restore_post_gc_hook;
  Lisp_Object restore_process_environment;

#ifdef REL_ALLOC
  bool blocked_ralloc;
#endif

  /* File descriptor for dumpfile; < 0 if closed.  */
  int fd;
  /* Name of dump file --- used for error reporting.  */
  Lisp_Object filename;
  /* Current offset in dump file.  */
  dump_off offset;

  /* Starting offset of current object.  */
  dump_off obj_offset;

  /* Flags currently in effect for dumping.  */
  struct dump_flags flags;

  dump_off end_heap;

  /* Hash mapping objects we've already dumped to their offsets.  */
  Lisp_Object objects_dumped;

  /* Queue of objects to dump.  */
  struct queue queue;

  /* Deferred object lists.  */
  Lisp_Object deferred_hash_tables;
  Lisp_Object deferred_symbols;

  /* Fixups in the dump file.  */
  Lisp_Object fixups;

  /* Hash table of staticpro values: avoids double relocations.  */
  Lisp_Object staticpro_table;

  /* Hash table mapping symbols to their pre-copy-queue fwd structures
     (which we dump immediately before the start of the discardable
     section). */
  Lisp_Object symbol_aux;
  Lisp_Object symbol_cvar;

  /* Queue of copied objects for special treatment.  */
  Lisp_Object copied_queue;
  /* Queue of cold objects to dump.  */
  Lisp_Object cold_queue;

  /* Relocations in the dump.  */
  Lisp_Object dump_relocs[RELOC_NUM_PHASES];

  /* Object starts.  */
  Lisp_Object object_starts;

  /* Relocations in Emacs.  */
  Lisp_Object emacs_relocs;

  /* Hash table mapping bignums to their _data_ blobs, which we store
     in the cold section.  The actual Lisp_Bignum objects are normal
     heap objects.  */
  Lisp_Object bignum_data;

  /* List of hash tables that have been dumped.  */
  Lisp_Object hash_tables;

  dump_off nr_hot_relocations;
  dump_off nr_discardable_relocations;
};

/* Fictitious, non-positive offsets indicate an object is still
   queued and hasn't been dumped.  */
enum special_dump_offset
  {
   IS_RUNTIME_MAGIC = -6,
   ON_COPIED_QUEUE = -5,
   ON_HASH_TABLE_QUEUE = -4,
   ON_SYMBOL_QUEUE = -3,
   ON_COLD_QUEUE = -2,
   ON_NORMAL_QUEUE = -1,
   NOT_SEEN = 0,
  };

/* Weights for score scores for object non-locality.  */

struct link_weight
{
  /* Wrapped in a struct to break unwanted implicit conversion.  */
  int value;
};

static struct link_weight const
  WEIGHT_NONE = { .value = 0 },
  WEIGHT_NORMAL = { .value = 1000 },
  WEIGHT_STRONG = { .value = 1200 };


/* Dump file creation */

static void dump_grow_buffer (struct dump_context *ctx)
{
  ctx->buf = xrealloc (ctx->buf, ctx->buf_size = (ctx->buf_size ?
						  (ctx->buf_size * 2)
						  : 8 * 1024 * 1024));
}

static dump_off dump_object (struct dump_context *ctx, Lisp_Object object);

/* Like the Lisp function `push'.  Return NEWELT.  */
static Lisp_Object
push (Lisp_Object *where, Lisp_Object newelt)
{
  *where = Fcons (newelt, *where);
  return newelt;
}

/* Like the Lisp function `pop'.  */
static Lisp_Object
pop (Lisp_Object *where)
{
  Lisp_Object ret = XCAR (*where);
  *where = XCDR (*where);
  return ret;
}

static void remember_cold_op (struct dump_context *ctx,
			      enum cold_op op,
			      Lisp_Object arg);

static AVOID
error_unsupported_dump_object (struct dump_context *ctx,
                               Lisp_Object object,
			       const char *msg)
{
  error ("unsupported object type in dump: %s", msg);
}

static bool
builtin_symbol_p (Lisp_Object object)
{
  return SYMBOLP (object) && builtin_lisp_symbol_p (XSYMBOL (object));
}

/* Return whether OBJECT is bit-invariant across a dump.  Note that some
   self-representing objects still need to be dumped!
*/
static bool
self_representing_p (Lisp_Object object)
{
  return FIXNUMP (object) || builtin_symbol_p (object);
}

/* Inverse of INT_TO_INTEGER.  */
static intmax_t
INTEGER_TO_INT (Lisp_Object value)
{
  intmax_t n;
  bool ok = integer_to_intmax (value, &n);
  eassert (ok);
  return n;
}

static void
write_bytes (struct dump_context *ctx, const void *buf, dump_off nbyte)
{
  eassert (nbyte == 0 || buf != NULL);
  eassert (ctx->obj_offset == 0);
  eassert (ctx->flags.dump_object_contents);
  while (ctx->offset + nbyte > ctx->buf_size)
    dump_grow_buffer (ctx);
  memcpy ((char *)ctx->buf + ctx->offset, buf, nbyte);
  ctx->offset += nbyte;
}

static Lisp_Object
make_eq_hash_table (void)
{
  return CALLN (Fmake_hash_table, QCtest, Qeq);
}

static void
tailq_init (struct tailq *tailq)
{
  tailq->head = tailq->tail = Qnil;
  tailq->length = 0;
}

static intptr_t
tailq_length (const struct tailq *tailq)
{
  return tailq->length;
}

static void
tailq_prepend (struct tailq *tailq, Lisp_Object value)
{
  Lisp_Object link = Fcons (value, tailq->head);
  tailq->head = link;
  if (NILP (tailq->tail))
    tailq->tail = link;
  ++tailq->length;
}

static bool
tailq_empty_p (struct tailq *tailq)
{
  return NILP (tailq->head);
}

static Lisp_Object
tailq_peek (struct tailq *tailq)
{
  eassert (!tailq_empty_p (tailq));
  return XCAR (tailq->head);
}

static Lisp_Object
tailq_pop (struct tailq *tailq)
{
  eassert (!tailq_empty_p (tailq));
  eassert (tailq->length > 0);
  tailq->length -= 1;
  Lisp_Object value = XCAR (tailq->head);
  tailq->head = XCDR (tailq->head);
  if (NILP (tailq->head))
    tailq->tail = Qnil;
  return value;
}

static void
seek (struct dump_context *ctx, dump_off offset)
{
  ctx->max_offset = max (ctx->max_offset, ctx->offset);
  eassert (ctx->obj_offset == 0);
  ctx->offset = offset;
}

static void
write_bytes_zero (struct dump_context *ctx, dump_off nbytes)
{
  while (nbytes > 0)
    {
      uintmax_t zero = 0;
      dump_off to_write = sizeof (zero);
      if (to_write > nbytes)
        to_write = nbytes;
      write_bytes (ctx, &zero, to_write);
      nbytes -= to_write;
    }
}

static void
align_output (struct dump_context *ctx, int alignment)
{
  if (ctx->offset % alignment != 0)
    write_bytes_zero (ctx, alignment - (ctx->offset % alignment));
}

static dump_off
start_object (struct dump_context *ctx, void *out, dump_off outsz)
{
  eassert (ctx->obj_offset == 0); /* zero since one obj at a time */
  int alignment = ctx->flags.pack_objects ? 1 : DUMP_ALIGNMENT;
  if (ctx->flags.dump_object_contents)
    align_output (ctx, alignment);
  ctx->obj_offset = ctx->offset;
  memset (out, 0, outsz);
  return ctx->offset;
}

static dump_off
finish_object (struct dump_context *ctx, const void *out, dump_off sz)
{
  dump_off offset = ctx->obj_offset;
  eassert (offset > 0);
  eassert (offset == ctx->offset); /* No intervening writes.  */
  ctx->obj_offset = 0;
  if (ctx->flags.dump_object_contents)
    write_bytes (ctx, out, sz);
  return offset;
}

/* Return OBJECT's offset, or one of special_dump_offset.  */
static dump_off
recall_object (struct dump_context *ctx, Lisp_Object object)
{
  Lisp_Object dumped = ctx->objects_dumped;
  return INTEGER_TO_INT (Fgethash (object, dumped, make_fixnum (NOT_SEEN)));
}

static void
remember_object (struct dump_context *ctx, Lisp_Object object, dump_off offset)
{
  Fputhash (object, INT_TO_INTEGER (offset), ctx->objects_dumped);
}

/* If LV lives outside alloc.c blocks, return its xpntr and NULL
   otherwise.
*/
static void *
emacs_ptr (Lisp_Object lv)
{
  if (SUBRP (lv) && !NATIVE_COMP_FUNCTIONP (lv))
    return XSUBR (lv);
  if (builtin_symbol_p (lv))
    return XSYMBOL (lv);
  if (XTYPE (lv) == Lisp_Vectorlike
      && PVTYPE (XVECTOR (lv)) == PVEC_THREAD)
    return XTHREAD (lv);
  return NULL;
}

static void
queue_init (struct queue *queue)
{
  tailq_init (&queue->zero_weight_objects);
  tailq_init (&queue->one_weight_normal_objects);
  tailq_init (&queue->one_weight_strong_objects);
  tailq_init (&queue->fancy_weight_objects);
  queue->link_weights = make_eq_hash_table ();
  queue->sequence_numbers = make_eq_hash_table ();
  queue->next_sequence_number = 1;
}

static bool
queue_empty_p (struct queue *queue)
{
  ptrdiff_t count = XHASH_TABLE (queue->sequence_numbers)->count;
  bool is_empty = count == 0;
  eassert (count == XFIXNAT (Fhash_table_count (queue->link_weights)));
  if (!is_empty)
    {
      eassert (!tailq_empty_p (&queue->zero_weight_objects)
	       || !tailq_empty_p (&queue->one_weight_normal_objects)
	       || !tailq_empty_p (&queue->one_weight_strong_objects)
	       || !tailq_empty_p (&queue->fancy_weight_objects));
    }
  else
    {
      /* If we're empty, we can still have a few stragglers on one of
         the above queues.  */
    }

  return is_empty;
}

static void
queue_push_weight (Lisp_Object *weight_list,
		   dump_off basis,
		   struct link_weight weight)
{
  if (EQ (*weight_list, Qt))
    *weight_list = Qnil;
  push (weight_list, Fcons (INT_TO_INTEGER (basis),
			    INT_TO_INTEGER (weight.value)));
}

static void
queue_enqueue (struct queue *queue,
	       Lisp_Object object,
	       dump_off basis,
	       struct link_weight weight)
{
  Lisp_Object weights = Fgethash (object, queue->link_weights, Qnil);
  Lisp_Object orig_weights = weights;
  /* N.B. want to find the last item of a given weight in each queue
     due to prepend use.  */
  bool use_single_queues = true;
  if (NILP (weights))
    {
      /* Object is new.  */
      EMACS_UINT uobj = XLI (object);
      dump_trace ("new object %0*"pI"x weight=%d\n",
		  (EMACS_INT_WIDTH + 3) / 4, uobj, weight.value);
      if (weight.value == WEIGHT_NONE.value)
        {
          eassert (weight.value == 0);
          tailq_prepend (&queue->zero_weight_objects, object);
          weights = Qt;
        }
      else if (!use_single_queues)
        {
          tailq_prepend (&queue->fancy_weight_objects, object);
          queue_push_weight (&weights, basis, weight);
        }
      else if (weight.value == WEIGHT_NORMAL.value)
        {
          tailq_prepend (&queue->one_weight_normal_objects, object);
          queue_push_weight (&weights, basis, weight);
        }
      else if (weight.value == WEIGHT_STRONG.value)
        {
          tailq_prepend (&queue->one_weight_strong_objects, object);
          queue_push_weight (&weights, basis, weight);
        }
      else
        {
          emacs_abort ();
        }

      Fputhash (object,
                INT_TO_INTEGER(queue->next_sequence_number++),
                queue->sequence_numbers);
    }
  else
    {
      /* Object was already on the queue.  It's okay for an object to
         be on multiple queues so long as we maintain order
         invariants: attempting to dump an object multiple times is
         harmless, and most of the time, an object is only referenced
         once before being dumped, making this code path uncommon.  */
      if (weight.value != WEIGHT_NONE.value)
        {
          if (EQ (weights, Qt))
            {
              /* Object previously had a zero weight.  Once we
                 incorporate the link weight attached to this call,
                 the object will have a single weight.  Put the object
                 on the appropriate single-weight queue.  */
              weights = Qnil;
	      struct tailq *tailq;
              if (!use_single_queues)
		tailq = &queue->fancy_weight_objects;
              else if (weight.value == WEIGHT_NORMAL.value)
		tailq = &queue->one_weight_normal_objects;
              else if (weight.value == WEIGHT_STRONG.value)
		tailq = &queue->one_weight_strong_objects;
              else
                emacs_abort ();
	      tailq_prepend (tailq, object);
            }
          else if (use_single_queues && NILP (XCDR (weights)))
            tailq_prepend (&queue->fancy_weight_objects, object);
          queue_push_weight (&weights, basis, weight);
        }
    }

  if (!EQ (weights, orig_weights))
    Fputhash (object, weights, queue->link_weights);
}

static float
calc_link_score (dump_off basis, dump_off link_basis, dump_off link_weight)
{
  float distance = (float)(basis - link_basis);
  eassert (distance >= 0);
  float link_score = powf (distance, -0.2f);
  return powf (link_score, (float) link_weight / 1000.0f);
}

/* Compute the score for a queued object.

   OBJECT is the object to query, which must currently be queued for
   dumping.  BASIS is the offset at which we would be
   dumping the object; score is computed relative to BASIS and the
   various BASIS values supplied to dump_add_link_weight --- the
   further an object is from its referrers, the greater the
   score.  */
static float
queue_compute_score (struct queue *queue,
		     Lisp_Object object,
		     dump_off basis)
{
  float score = 0;
  Lisp_Object object_link_weights =
    Fgethash (object, queue->link_weights, Qnil);
  if (EQ (object_link_weights, Qt))
    object_link_weights = Qnil;
  while (!NILP (object_link_weights))
    {
      Lisp_Object basis_weight_pair = pop (&object_link_weights);
      dump_off link_basis = INTEGER_TO_INT (XCAR (basis_weight_pair));
      dump_off link_weight = INTEGER_TO_INT (XCDR (basis_weight_pair));
      score += calc_link_score (basis, link_basis, link_weight);
    }
  return score;
}

/* Scan the fancy part of the dump queue.

   BASIS is the position at which to evaluate the score function,
   usually ctx->offset.

   If we have at least one entry in the queue, return the pointer (in
   the singly-linked list) to the cons containing the object via
   *OUT_HIGHEST_SCORE_CONS_PTR and return its score.

   If the queue is empty, set *OUT_HIGHEST_SCORE_CONS_PTR to NULL
   and return negative infinity.  */
static float
queue_scan_fancy (struct queue *queue,
		  dump_off basis,
		  Lisp_Object **out_highest_score_cons_ptr)
{
  Lisp_Object *cons_ptr = &queue->fancy_weight_objects.head;
  Lisp_Object *highest_score_cons_ptr = NULL;
  float highest_score = -INFINITY;
  bool first = true;

  while (!NILP (*cons_ptr))
    {
      Lisp_Object queued_object = XCAR (*cons_ptr);
      float score = queue_compute_score (queue, queued_object, basis);
      if (first || score >= highest_score)
        {
          highest_score_cons_ptr = cons_ptr;
          highest_score = score;
          if (first)
            first = false;
        }
      cons_ptr = &XCONS (*cons_ptr)->u.s.u.cdr;
    }

  *out_highest_score_cons_ptr = highest_score_cons_ptr;
  return highest_score;
}

/* Return the sequence number of OBJECT.

   Return -1 if object doesn't have a sequence number.  This situation
   can occur when we've double-queued an object.  If this happens, we
   discard the errant object and try again.  */
static dump_off
queue_sequence (struct queue *queue, Lisp_Object object)
{
  Lisp_Object n = Fgethash (object, queue->sequence_numbers, Qnil);
  return NILP (n) ? -1 : INTEGER_TO_INT (n);
}

/* Find score and sequence at head of a one-weight object queue.

   Transparently discard stale objects from head of queue.  BASIS
   is the baseness for score computation.

   We organize these queues so that score is strictly decreasing, so
   examining the head is sufficient.  */
static void
queue_find_score_of_one_weight_queue (struct queue *queue,
				      dump_off basis,
				      struct tailq *one_weight_queue,
				      float *out_score,
				      int *out_sequence)
{
  /* Transparently discard stale objects from the head of this queue.  */
  do
    {
      if (tailq_empty_p (one_weight_queue))
        {
          *out_score = -INFINITY;
          *out_sequence = 0;
        }
      else
        {
          Lisp_Object head = tailq_peek (one_weight_queue);
          *out_sequence = queue_sequence (queue, head);
          if (*out_sequence < 0)
            tailq_pop (one_weight_queue);
          else
            *out_score =
              queue_compute_score (queue, head, basis);
        }
    }
  while (*out_sequence < 0);
}

/* Pop the next object to dump from the dump queue.

   BASIS is the dump offset at which to evaluate score.

   The object returned is the queued object with the greatest score;
   by side effect, the object is removed from the dump queue.
   The dump queue must not be empty.  */
static Lisp_Object
queue_dequeue (struct queue *queue, dump_off basis)
{
  eassert (EQ (Fhash_table_count (queue->sequence_numbers),
               Fhash_table_count (queue->link_weights)));

  eassert (XFIXNUM (Fhash_table_count (queue->sequence_numbers))
	   <= (tailq_length (&queue->fancy_weight_objects)
	       + tailq_length (&queue->zero_weight_objects)
	       + tailq_length (&queue->one_weight_normal_objects)
	       + tailq_length (&queue->one_weight_strong_objects)));

  dump_trace
    (("queue_dequeue basis=%"PRIdDUMP_OFF" fancy=%"PRIdPTR
      " zero=%"PRIdPTR" normal=%"PRIdPTR" strong=%"PRIdPTR" hash=%td\n"),
     basis,
     tailq_length (&queue->fancy_weight_objects),
     tailq_length (&queue->zero_weight_objects),
     tailq_length (&queue->one_weight_normal_objects),
     tailq_length (&queue->one_weight_strong_objects),
     (ptrdiff_t) XHASH_TABLE (queue->link_weights)->count);

  static const int nr_candidates = 3;
  struct candidate
  {
    float score;
    dump_off sequence;
  } candidates[nr_candidates];

  Lisp_Object *fancy_cons = NULL;
  candidates[0].sequence = 0;
  do
    {
      if (candidates[0].sequence < 0)
        *fancy_cons = XCDR (*fancy_cons);  /* Discard stale object.  */
      candidates[0].score = queue_scan_fancy (queue, basis, &fancy_cons);
      candidates[0].sequence =
        candidates[0].score > -INFINITY
        ? queue_sequence (queue, XCAR (*fancy_cons))
        : 0;
    }
  while (candidates[0].sequence < 0);

  queue_find_score_of_one_weight_queue
    (queue, basis,
     &queue->one_weight_normal_objects,
     &candidates[1].score,
     &candidates[1].sequence);

  queue_find_score_of_one_weight_queue
    (queue, basis,
     &queue->one_weight_strong_objects,
     &candidates[2].score,
     &candidates[2].sequence);

  int best = -1;
  for (int i = 0; i < nr_candidates; ++i)
    {
      eassert (candidates[i].sequence >= 0);
      if (candidates[i].score > -INFINITY
	  && (best < 0
	      || candidates[i].score > candidates[best].score
	      || (candidates[i].score == candidates[best].score
		  && candidates[i].sequence < candidates[best].sequence)))
        best = i;
    }

  Lisp_Object result;
  const char *src;
  if (best < 0)
    {
      src = "zero";
      result = tailq_pop (&queue->zero_weight_objects);
    }
  else if (best == 0)
    {
      src = "fancy";
      result = tailq_pop (&queue->fancy_weight_objects);
    }
  else if (best == 1)
    {
      src = "normal";
      result = tailq_pop (&queue->one_weight_normal_objects);
    }
  else if (best == 2)
    {
      src = "strong";
      result = tailq_pop (&queue->one_weight_strong_objects);
    }
  else
    emacs_abort ();

  EMACS_UINT uresult = XLI (result);
  dump_trace ("  result score=%f src=%s object=%0*"pI"x\n",
              best < 0 ? -1.0 : (double) candidates[best].score,
	      src, (EMACS_INT_WIDTH + 3) / 4, uresult);
  {
    Lisp_Object weights = Fgethash (result, queue->link_weights, Qnil);
    while (!NILP (weights) && CONSP (weights))
      {
        Lisp_Object basis_weight_pair = pop (&weights);
        dump_off link_basis =
          INTEGER_TO_INT (XCAR (basis_weight_pair));
        dump_off link_weight =
          INTEGER_TO_INT (XCDR (basis_weight_pair));
	dump_trace
	  ("    link_basis=%d distance=%d weight=%d contrib=%f\n",
	   link_basis,
	   basis - link_basis,
	   link_weight,
	   (double) calc_link_score (basis, link_basis, link_weight));
      }
  }

  Fremhash (result, queue->link_weights);
  Fremhash (result, queue->sequence_numbers);
  return result;
}

static void
enqueue_object (struct dump_context *ctx,
		Lisp_Object object,
		struct link_weight weight)
{
  /* Fixnums are bit-invariant, and don't need dumping.  */
  if (!FIXNUMP (object))
    {
      dump_off state = recall_object (ctx, object);
      bool already_dumped_object = state > NOT_SEEN;
      eassert (!ctx->flags.assert_already_seen || already_dumped_object);
      if (!already_dumped_object)
        {
          if (state == NOT_SEEN)
            {
              state = ON_NORMAL_QUEUE;
              remember_object (ctx, object, state);
            }
          /* Note that we call queue_enqueue even if the object
             is already on the normal queue: multiple enqueue calls
             can increase the object's weight.  */
          if (state == ON_NORMAL_QUEUE)
            queue_enqueue (&ctx->queue, object, ctx->offset, weight);
        }
    }
}

static void
remember_cold_op (struct dump_context *ctx, enum cold_op op, Lisp_Object arg)
{
  if (ctx->flags.dump_object_contents)
    push (&ctx->cold_queue, Fcons (make_fixnum (op), arg));
}

/* On load:
   uintptr_t value = read_word (reloc.offset);
   value += emacs_basis ();
   write_word (reloc.offset, value);
*/

static void
reloc_emacs_ptr (struct dump_context *ctx, dump_off offset)
{
  if (ctx->flags.dump_object_contents)
    push (&ctx->dump_relocs[EARLY_RELOCS],
	  list2 (make_fixnum (RELOC_EMACS_PTR),
		 INT_TO_INTEGER (offset)));
}

/* On load:
   uintptr_t value = pdumper_info.addr_beg + read_word (reloc.offset);
   write_lv (reloc.offset, make_lisp_ptr ((void *) value, lisp_type));
*/

static void
reloc_dump_lv (struct dump_context *ctx, dump_off offset,
	       enum Lisp_Type type)
{
  if (ctx->flags.dump_object_contents)
    {
      int reloc_type;
      switch (type)
	{
	case Lisp_Symbol:
	case Lisp_String:
	case Lisp_Vectorlike:
	case Lisp_Cons:
	case Lisp_Float:
	  reloc_type = RELOC_DUMP_LV + type;
	  break;
	default:
	  emacs_abort ();
	  break;
	}

      push (&ctx->dump_relocs[EARLY_RELOCS],
	    list2 (make_fixnum (reloc_type),
		   INT_TO_INTEGER (offset)));
    }
}

/* On load:
   uintptr_t value = read_word (reloc.offset);
   value += pdumper_info.addr_beg;
   write_word (reloc.offset, value);
*/

static void
reloc_dump_ptr (struct dump_context *ctx, dump_off offset)
{
  if (ctx->flags.dump_object_contents)
    push (&ctx->dump_relocs[EARLY_RELOCS],
	  list2 (make_fixnum (RELOC_DUMP_PTR),
		 INT_TO_INTEGER (offset)));
}

/* On load:
   uintptr_t value = emacs_basis () + read_word (reloc.offset);
   write_lv (reloc.offset, make_lisp_ptr ((void *) value, lisp_type));
*/

static void
reloc_emacs_lv (struct dump_context *ctx, dump_off offset,
		enum Lisp_Type type)
{
  if (ctx->flags.dump_object_contents)
    push (&ctx->dump_relocs[EARLY_RELOCS],
	  list2 (make_fixnum (RELOC_EMACS_LV + type),
		 INT_TO_INTEGER (offset)));
}

uintptr_t
emacs_basis (void)
{
  return (uintptr_t) &Vpdumper__pure_pool;
}

static dump_off
emacs_offset (const void *emacs_ptr)
{
  return DUMP_OFF ((intptr_t) emacs_ptr - (intptr_t) emacs_basis ());
}

/* On load:
   memcpy (emacs_ptr_at (reloc.offset),
           (char *) pdumper_info.addr_beg + reloc.ptr.offset,
           reloc.length);
*/
static void
reloc_copy_from_dump (struct dump_context *ctx, dump_off offset,
		      void *emacs_ptr, dump_off length)
{
  if (ctx->flags.dump_object_contents && length)
    {
      eassert (offset >= 0);
      push (&ctx->emacs_relocs,
	    list4 (make_fixnum (RELOC_COPY_FROM_DUMP),
		   INT_TO_INTEGER (emacs_offset (emacs_ptr)),
		   INT_TO_INTEGER (offset),
		   INT_TO_INTEGER (length)));
    }
}

/* On load:
   memcpy (emacs_ptr_at (reloc.offset), &reloc.ptr.immediate, reloc.length);
*/

static void
reloc_immediate (struct dump_context *ctx, const void *emacs_ptr,
		 const void *value_ptr, dump_off size)
{
  if (ctx->flags.dump_object_contents)
    {
      intmax_t value = 0;
      eassert (size <= sizeof (value));
      memcpy (&value, value_ptr, size);
      push (&ctx->emacs_relocs,
	    list4 (make_fixnum (RELOC_IMMEDIATE),
		   INT_TO_INTEGER (emacs_offset (emacs_ptr)),
		   INT_TO_INTEGER (value),
		   INT_TO_INTEGER (size)));
    }
}

#define DEFINE_EMACS_IMMEDIATE_FN(fnname, type)                         \
  static void                                                           \
  fnname (struct dump_context *ctx,                                     \
          const type *emacs_ptr,                                        \
          type value)                                                   \
  {                                                                     \
    reloc_immediate (ctx, emacs_ptr, &value, sizeof (value));		\
  }

DEFINE_EMACS_IMMEDIATE_FN (reloc_immediate_lv, Lisp_Object)
DEFINE_EMACS_IMMEDIATE_FN (reloc_immediate_ptrdiff_t, ptrdiff_t)
DEFINE_EMACS_IMMEDIATE_FN (reloc_immediate_intmax_t, intmax_t)
DEFINE_EMACS_IMMEDIATE_FN (reloc_immediate_int, int)
DEFINE_EMACS_IMMEDIATE_FN (reloc_immediate_bool, bool)
  ;

/* On load:
   pval = reloc.ptr.offset + pdumper_info.addr_beg;
   memcpy (emacs_ptr_at (reloc.offset), &pval, sizeof (pval));
*/

static void
reloc_to_dump_ptr (struct dump_context *ctx,
		   const void *emacs_ptr, dump_off offset)
{
  if (ctx->flags.dump_object_contents)
    push (&ctx->emacs_relocs,
	  list3 (make_fixnum (RELOC_DUMP_PTR),
		 INT_TO_INTEGER (emacs_offset (emacs_ptr)),
		 INT_TO_INTEGER (offset)));
}

/* On load:
   void *obj_ptr = reloc.type == RELOC_DUMP_LV
     ? (char *) pdumper_info.addr_beg + reloc.ptr.offset
     : emacs_ptr_at (reloc.ptr.offset);
   lv = make_lisp_ptr (obj_ptr, reloc.length);
   memcpy (emacs_ptr_at (reloc.offset), &lv, sizeof (lv));
*/

static void
reloc_to_lv (struct dump_context *ctx, Lisp_Object const *obj)
{
  if (self_representing_p (*obj))
    reloc_immediate_lv (ctx, obj, *obj);
  else
    {
      if (ctx->flags.dump_object_contents)
	push (&ctx->emacs_relocs, list3 (make_fixnum (emacs_ptr (*obj)
						      ? RELOC_EMACS_LV
						      : RELOC_DUMP_LV),
					 INT_TO_INTEGER (emacs_offset (obj)),
					 *obj));
      enqueue_object (ctx, *obj, WEIGHT_NONE);
    }
}

/* On load:
   pval = reloc.ptr.offset + emacs_basis ();
   memcpy (emacs_ptr_at (reloc.offset), &pval, sizeof (pval));
*/

static void
reloc_to_emacs_ptr (struct dump_context *ctx, void *emacs_ptr,
		    void const *target_emacs_ptr)
{
  if (ctx->flags.dump_object_contents)
    {
      push (&ctx->emacs_relocs,
	    list3 (make_fixnum (RELOC_EMACS_PTR),
		   INT_TO_INTEGER (emacs_offset (emacs_ptr)),
		   INT_TO_INTEGER (emacs_offset (target_emacs_ptr))));
    }
}

enum dump_fixup_type
  {
    FIXUP_LISP_OBJECT,
    FIXUP_LISP_OBJECT_RAW,
    FIXUP_PTR_DUMP_RAW,
    FIXUP_BIGNUM_DATA,
  };

/* Plan to have pointer-sized value at OFFSET get fixed to NEW_OFFSET.  */
static void
remember_fixup_ptr (struct dump_context *ctx,
		    dump_off offset,
		    dump_off new_offset)
{
  if (ctx->flags.dump_object_contents)
    {
      /* We should not be generating relocations into the
	 to-be-copied-into-Emacs dump region.  */
      eassert (ctx->header.discardable_start == 0
	       || new_offset < ctx->header.discardable_start
	       || (ctx->header.cold_start != 0
		   && new_offset >= ctx->header.cold_start));
      push (&ctx->fixups,
	    list3 (make_fixnum (FIXUP_PTR_DUMP_RAW),
		   INT_TO_INTEGER (offset),
		   INT_TO_INTEGER (new_offset)));
    }
}

static void
dump_roots (struct dump_context *ctx)
{
  const struct Lisp_Vector *vbuffer_slot_defaults =
    (struct Lisp_Vector *) &buffer_slot_defaults;
  const struct Lisp_Vector *vbuffer_slot_symbols =
    (struct Lisp_Vector *) &buffer_slot_symbols;

  for (int i = 0; i < BUFFER_LISP_SIZE; ++i)
    {
      reloc_to_lv (ctx, vbuffer_slot_defaults->contents + i);
      reloc_to_lv (ctx, vbuffer_slot_symbols->contents + i);
    }

  for (int i = 0; i < ARRAYELTS (lispsym); ++i)
    enqueue_object (ctx, builtin_lisp_symbol (i), WEIGHT_NONE);

  for (int i = 0; i < staticidx; ++i)
    {
      Fputhash (INT_TO_INTEGER (emacs_offset (staticvec[i])),
		Qt, ctx->staticpro_table);
      reloc_to_lv (ctx, staticvec[i]);
    }
}

enum { PDUMPER_MAX_OBJECT_SIZE = 1 << 11 };

static dump_off
field_relpos (const void *in_start, const void *in_field)
{
  ptrdiff_t in_start_val = (ptrdiff_t) in_start;
  ptrdiff_t in_field_val = (ptrdiff_t) in_field;
  eassert (in_start_val <= in_field_val);
  ptrdiff_t relpos = in_field_val - in_start_val;
  /* The following assertion attempts to detect bugs whereby IN_START
     and IN_FIELD don't point to the same object/structure, on the
     assumption that a too-large difference between them is
     suspicious.  As of Apr 2019 the largest object we dump -- 'struct
     buffer' -- is slightly smaller than 1KB, and we want to leave
     some margin for future extensions.  If the assertion below is
     ever violated, make sure the two pointers indeed point into the
     same object, and if so, enlarge the value of PDUMPER_MAX_OBJECT_SIZE.  */
  eassert (relpos < PDUMPER_MAX_OBJECT_SIZE);
  return DUMP_OFF (relpos);
}

static void
cpyptr (void *out, const void *in)
{
  memcpy (out, in, sizeof (void *));
}

/* Convenience macro for regular assignment.  */
#define DUMP_FIELD_COPY(out, in, name) \
  ((out)->name = (in)->name)

static void
write_field_lisp_common (struct dump_context *ctx,
			 void *out_field,
			 Lisp_Object value,
			 struct link_weight weight)
{
  const intptr_t out_value = (intptr_t) 0xDEADF00D;
  enqueue_object (ctx, value, weight);
  memcpy (out_field, &out_value, sizeof (out_value));
}

/* Write a Lisp_Object xpntr to the field of *OUT corresponding to the
   same notional *IN_FIELD.

   CTX is the dump context.  OUT points to the dumped object.  IN_START
   and IN_FIELD are the starting address and target field of the current
   Emacs object.  TYPE is the output Lisp type.
*/
static void
write_field_lisp_xpntr (struct dump_context *ctx,
			void *out,
			const void *in_start,
			const void *in_field,
			const enum Lisp_Type ptr_type,
			struct link_weight weight)
{
  eassert (ctx->obj_offset > 0);
  Lisp_Object value;
  dump_off relpos = field_relpos (in_start, in_field);
  void *ptrval;
  cpyptr (&ptrval, in_field);
  if (ptrval == NULL)
    return; /* !!! */
  switch (ptr_type)
    {
    case Lisp_Symbol:
    case Lisp_String:
    case Lisp_Vectorlike:
    case Lisp_Cons:
    case Lisp_Float:
      value = make_lisp_ptr (ptrval, ptr_type);
      break;
    default:
      emacs_abort ();
      break;
    }

  /* We don't know about the target object yet, so add a fixup.
     When we process the fixup, we'll have dumped the target
     object.  */
  if (ctx->flags.dump_object_contents)
    {
      dump_off out_field_offset = ctx->obj_offset + relpos;
      push (&ctx->fixups, list3 (make_fixnum (FIXUP_LISP_OBJECT_RAW),
				 INT_TO_INTEGER (out_field_offset),
				 value));
    }
  write_field_lisp_common (ctx, (char *) out + relpos, value, weight);
}

/* Write a Lisp_Object to the field of *OUT corresponding to the same
   notional *IN_FIELD.

   CTX is the dump context.  OUT points to the dumped object.  IN_START
   and IN_FIELD are the starting address and target field of the current
   Emacs object.  TYPE is the output Lisp type.  If IN_FIELD already
   points to a Lisp_Object, TYPE is not applicable and set to NULL.
*/
static void
write_field_lisp_object (struct dump_context *ctx,
			 void *out,
			 const void *in_start,
			 const void *in_field,
			 struct link_weight weight)
{
  eassert (ctx->obj_offset > 0);
  Lisp_Object value;
  dump_off relpos = field_relpos (in_start, in_field);
  memcpy (&value, in_field, sizeof (value));
  if (self_representing_p (value))
    {
      memcpy ((char *) out + relpos, &value, sizeof (value));
      return; /* !!! */
    }

  /* We don't know about the target object yet, so add a fixup.
     When we process the fixup, we'll have dumped the target
     object.  */
  if (ctx->flags.dump_object_contents)
    {
      dump_off out_field_offset = ctx->obj_offset + relpos;
      push (&ctx->fixups, list3 (make_fixnum (FIXUP_LISP_OBJECT),
				 INT_TO_INTEGER (out_field_offset),
				 value));
    }
  write_field_lisp_common (ctx, (char *) out + relpos, value, weight);
}

/* Point dumped object field to contents at TARGET_OFFSET.  */
static void
write_field_dump_ptr (struct dump_context *ctx,
		      void *out,
		      const void *in_start,
		      const void *in_field,
		      dump_off target_offset)
{
  eassert (ctx->obj_offset > 0);
  if (ctx->flags.dump_object_contents)
    {
      dump_off relpos = field_relpos (in_start, in_field);
      reloc_dump_ptr (ctx, ctx->obj_offset + relpos);
      intptr_t outval = target_offset;
      memcpy ((char *) out + relpos, &outval, sizeof (outval));
    }
}

/* Point dumped object field to verbatim address within emacs.

   CTX is the dump context.  OUT points to the dumped object.  IN_START
   and IN_FIELD are the starting address and target field of the current
   Emacs object.
*/
static void
write_field_emacs_ptr (struct dump_context *ctx,
		       void *out,
		       const void *in_start,
		       const void *in_field)
{
  eassert (ctx->obj_offset > 0);
  if (ctx->flags.dump_object_contents)
    {
      dump_off relpos = field_relpos (in_start, in_field);
      void *abs_emacs_ptr;
      cpyptr (&abs_emacs_ptr, in_field);
      intptr_t rel_emacs_ptr = 0;
      if (abs_emacs_ptr)
	{
	  rel_emacs_ptr = emacs_offset ((void *) abs_emacs_ptr);
	  reloc_emacs_ptr (ctx, ctx->obj_offset + relpos);
	}
      cpyptr ((char *) out + relpos, &rel_emacs_ptr);
    }
}

static void
start_object_pseudovector (struct dump_context *ctx,
			    union vectorlike_header *out_hdr,
			    const union vectorlike_header *in_hdr)
{
  eassert (in_hdr->size & PSEUDOVECTOR_FLAG);
  start_object (ctx, out_hdr, DUMP_OFF (vectorlike_nbytes (in_hdr)));
  *out_hdr = *in_hdr;
}

/* Need a macro for alloca.  */
#define START_DUMP_PVEC(ctx, hdr, type, out)                  \
  const union vectorlike_header *_in_hdr = (hdr);             \
  type *out = alloca (vectorlike_nbytes (_in_hdr));           \
  start_object_pseudovector (ctx, &out->header, _in_hdr)

static dump_off
finish_dump_pvec (struct dump_context *ctx,
                  union vectorlike_header *out_hdr)
{
  return finish_object (ctx, out_hdr, vectorlike_nbytes (out_hdr));
}

static void
dump_pseudovector (struct dump_context *ctx,
		   union vectorlike_header *out_hdr,
		   const union vectorlike_header *in_hdr)
{
  const struct Lisp_Vector *in = (const struct Lisp_Vector *) in_hdr;
  struct Lisp_Vector *out = (struct Lisp_Vector *) out_hdr;
  ptrdiff_t size = in->header.size;
  eassert (size & PSEUDOVECTOR_FLAG);
  size &= PSEUDOVECTOR_SIZE_MASK;
  for (ptrdiff_t i = 0; i < size; ++i)
    write_field_lisp_object (ctx, out, in, &in->contents[i], WEIGHT_STRONG);
}

static dump_off
dump_cons (struct dump_context *ctx, const struct Lisp_Cons *cons)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Cons_00EEE63F67)
# error "Lisp_Cons changed. See CHECK_STRUCTS comment in config.h."
#endif
  struct Lisp_Cons out;
  start_object (ctx, &out, sizeof (out));
  write_field_lisp_object (ctx, &out, cons, &cons->u.s.car, WEIGHT_STRONG);
  write_field_lisp_object (ctx, &out, cons, &cons->u.s.u.cdr, WEIGHT_NORMAL);
  return finish_object (ctx, &out, sizeof (out));
}

static dump_off
dump_interval_tree (struct dump_context *ctx,
                    INTERVAL tree,
                    dump_off parent_offset)
{
#if CHECK_STRUCTS && !defined (HASH_interval_1B38941C37)
# error "interval changed. See CHECK_STRUCTS comment in config.h."
#endif
  /* TODO: output tree breadth-first?  */
  struct interval out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, tree, total_length);
  DUMP_FIELD_COPY (&out, tree, position);
  if (!tree->up_obj)
    {
      eassert (parent_offset != 0);
      write_field_dump_ptr (ctx, &out, tree, &tree->up.interval, parent_offset);
    }
  else
    write_field_lisp_object (ctx, &out, tree, &tree->up.obj, WEIGHT_STRONG);
  DUMP_FIELD_COPY (&out, tree, up_obj);
  eassert (tree->gcmarkbit == 0);
  DUMP_FIELD_COPY (&out, tree, write_protect);
  DUMP_FIELD_COPY (&out, tree, visible);
  DUMP_FIELD_COPY (&out, tree, front_sticky);
  DUMP_FIELD_COPY (&out, tree, rear_sticky);
  write_field_lisp_object (ctx, &out, tree, &tree->plist, WEIGHT_STRONG);
  dump_off offset = finish_object (ctx, &out, sizeof (out));
  if (tree->left)
      remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct interval, left),
			  dump_interval_tree (ctx, tree->left, offset));
  if (tree->right)
      remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct interval, right),
			  dump_interval_tree (ctx, tree->right, offset));
  return offset;
}

static dump_off
dump_string (struct dump_context *ctx, const struct Lisp_String *string)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_String_03B2DF1C8E)
# error "Lisp_String changed. See CHECK_STRUCTS comment in config.h."
#endif
  /* If we have text properties, write them _after_ the string so that
     at runtime, the prefetcher and cache will DTRT. (We access the
     string before its properties.).

     There's special code to dump string data contiguously later on.
     we seldom write to string data and never relocate it, so lumping
     it together at the end of the dump saves on COW faults.

     If, however, the string's size_byte field is -2, the string data
     is actually a pointer to Emacs data segment, so we can do even
     better by emitting a relocation instead of bothering to copy the
     string data.  */
  struct Lisp_String out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, string, u.s.size);
  DUMP_FIELD_COPY (&out, string, u.s.size_byte);
  if (string->u.s.size_byte == -2)
    /* String literal in Emacs rodata.  */
    write_field_emacs_ptr (ctx, &out, string, &string->u.s.data);
  else
    remember_cold_op (ctx, COLD_OP_STRING,
		      make_lisp_ptr ((void *) string, Lisp_String));

  dump_off offset = finish_object (ctx, &out, sizeof (out));
  if (string->u.s.intervals)
    remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct Lisp_String, u.s.intervals),
			dump_interval_tree (ctx, string->u.s.intervals, 0));

  return offset;
}

static dump_off
dump_marker (struct dump_context *ctx, const struct Lisp_Marker *marker)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Marker_642DBAF866)
# error "Lisp_Marker changed. See CHECK_STRUCTS comment in config.h."
#endif

  START_DUMP_PVEC (ctx, &marker->header, struct Lisp_Marker, out);
  dump_pseudovector (ctx, &out->header, &marker->header);
  DUMP_FIELD_COPY (out, marker, need_adjustment);
  DUMP_FIELD_COPY (out, marker, insertion_type);
  if (marker->buffer)
    {
      write_field_lisp_xpntr (ctx, out, marker, &marker->buffer,
			      Lisp_Vectorlike, WEIGHT_NORMAL);
      write_field_lisp_xpntr (ctx, out, marker, &marker->next,
			      Lisp_Vectorlike, WEIGHT_STRONG);
      DUMP_FIELD_COPY (out, marker, charpos);
      DUMP_FIELD_COPY (out, marker, bytepos);
    }
  return finish_dump_pvec (ctx, &out->header);
}

static dump_off
dump_interval_node (struct dump_context *ctx, struct itree_node *node,
                    dump_off parent_offset)
{
#if CHECK_STRUCTS && !defined (HASH_itree_node_50DE304F13)
# error "itree_node changed. See CHECK_STRUCTS comment in config.h."
#endif
  struct itree_node out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, node, begin);
  DUMP_FIELD_COPY (&out, node, end);
  DUMP_FIELD_COPY (&out, node, limit);
  DUMP_FIELD_COPY (&out, node, offset);
  DUMP_FIELD_COPY (&out, node, otick);
  write_field_lisp_object (ctx, &out, node, &node->data, WEIGHT_STRONG);
  DUMP_FIELD_COPY (&out, node, red);
  DUMP_FIELD_COPY (&out, node, rear_advance);
  DUMP_FIELD_COPY (&out, node, front_advance);
  dump_off offset = finish_object (ctx, &out, sizeof (out));
  if (node->parent)
    remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct itree_node, parent),
			dump_interval_node (ctx, node->parent, offset));
  if (node->left)
    remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct itree_node, left),
			dump_interval_node (ctx, node->left, offset));
  if (node->right)
    remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct itree_node, right),
			dump_interval_node (ctx, node->right, offset));
  return offset;
}

static dump_off
dump_overlay (struct dump_context *ctx, const struct Lisp_Overlay *overlay)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Overlay_5F9D7E02FC)
# error "Lisp_Overlay changed. See CHECK_STRUCTS comment in config.h."
#endif
  START_DUMP_PVEC (ctx, &overlay->header, struct Lisp_Overlay, out);
  dump_pseudovector (ctx, &out->header, &overlay->header);
  dump_off offset = finish_dump_pvec (ctx, &out->header);
  remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct Lisp_Overlay, interval),
		      dump_interval_node (ctx, overlay->interval, offset));
  return offset;
}

static void
dump_field_finalizer_ref (struct dump_context *ctx,
                          void *out,
                          const struct Lisp_Finalizer *finalizer,
                          struct Lisp_Finalizer *const *field)
{
  if (*field == &finalizers || *field == &doomed_finalizers)
    write_field_emacs_ptr (ctx, out, finalizer, field);
  else
    write_field_lisp_xpntr (ctx, out, finalizer, field, Lisp_Vectorlike, WEIGHT_NORMAL);
}

static dump_off
dump_finalizer (struct dump_context *ctx,
                const struct Lisp_Finalizer *finalizer)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Finalizer_D58E647CB8)
# error "Lisp_Finalizer changed. See CHECK_STRUCTS comment in config.h."
#endif
  START_DUMP_PVEC (ctx, &finalizer->header, struct Lisp_Finalizer, out);
  /* Manually dump finalizer->function instead of using
     dump_pseudovector, so we can give it a low weight.  */
  write_field_lisp_object (ctx, &out, finalizer, &finalizer->function, WEIGHT_NONE);
  dump_field_finalizer_ref (ctx, &out, finalizer, &finalizer->prev);
  dump_field_finalizer_ref (ctx, &out, finalizer, &finalizer->next);
  return finish_dump_pvec (ctx, &out->header);
}

static dump_off
dump_bignum (struct dump_context *ctx, Lisp_Object object)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Bignum_661945DE2B)
# error "Lisp_Bignum changed. See CHECK_STRUCTS comment in config.h."
#endif
  const struct Lisp_Bignum *bignum = XBIGNUM (object);
  START_DUMP_PVEC (ctx, &bignum->header, struct Lisp_Bignum, out);
  verify (sizeof (out->value) >= sizeof (struct bignum_reload_info));
  dump_off bignum_offset = finish_dump_pvec (ctx, &out->header);
  if (ctx->flags.dump_object_contents)
    {
      /* Export the bignum into a blob in the cold section.  */
      remember_cold_op (ctx, COLD_OP_BIGNUM, object);

      /* Write the offset of that exported blob here.  */
      dump_off value_offset = bignum_offset
	+ DUMP_OFFSETOF (struct Lisp_Bignum, value);
      push (&ctx->fixups,
	    list3 (make_fixnum (FIXUP_BIGNUM_DATA),
		   INT_TO_INTEGER (value_offset),
		   object));

      /* When we load the dump, slurp the data blob and turn it into a
         real bignum.  Attach the relocation to the start of the
         Lisp_Bignum instead of the actual mpz field so that the
         relocation offset is aligned.  The relocation-application
         code knows to actually advance past the header.  */
      push (&ctx->dump_relocs[EARLY_RELOCS],
	    list2 (make_fixnum (RELOC_BIGNUM),
		   INT_TO_INTEGER (bignum_offset)));
    }

  return bignum_offset;
}

static dump_off
dump_float (struct dump_context *ctx, const struct Lisp_Float *lfloat)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Float_7E7D284C02)
# error "Lisp_Float changed. See CHECK_STRUCTS comment in config.h."
#endif
  eassert (ctx->header.cold_start);
  struct Lisp_Float out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, lfloat, u.data);
  return finish_object (ctx, &out, sizeof (out));
}

static dump_off
dump_fwd_int (struct dump_context *ctx, const struct Lisp_Intfwd *intfwd)
{
#if CHECK_STRUCTS && !defined HASH_Lisp_Intfwd_4D887A7387
# error "Lisp_Intfwd changed. See CHECK_STRUCTS comment in config.h."
#endif
  reloc_immediate_intmax_t (ctx, intfwd->intvar, *intfwd->intvar);
  struct Lisp_Intfwd out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, intfwd, type);
  write_field_emacs_ptr (ctx, &out, intfwd, &intfwd->intvar);
  return finish_object (ctx, &out, sizeof (out));
}

static dump_off
dump_fwd_bool (struct dump_context *ctx, const struct Lisp_Boolfwd *boolfwd)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Boolfwd_0EA1C7ADCC)
# error "Lisp_Boolfwd changed. See CHECK_STRUCTS comment in config.h."
#endif
  reloc_immediate_bool (ctx, boolfwd->boolvar, *boolfwd->boolvar);
  struct Lisp_Boolfwd out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, boolfwd, type);
  write_field_emacs_ptr (ctx, &out, boolfwd, &boolfwd->boolvar);
  return finish_object (ctx, &out, sizeof (out));
}

static dump_off
dump_fwd_obj (struct dump_context *ctx, const struct Lisp_Objfwd *objfwd)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Objfwd_45D3E513DC)
# error "Lisp_Objfwd changed. See CHECK_STRUCTS comment in config.h."
#endif
  if (NILP (Fgethash (INT_TO_INTEGER (emacs_offset (objfwd->objvar)),
                      ctx->staticpro_table,
                      Qnil)))
    reloc_to_lv (ctx, objfwd->objvar);
  struct Lisp_Objfwd out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, objfwd, type);
  write_field_emacs_ptr (ctx, &out, objfwd, &objfwd->objvar);
  return finish_object (ctx, &out, sizeof (out));
}

static dump_off
dump_fwd_buffer_obj (struct dump_context *ctx,
                     const struct Lisp_Buffer_Objfwd *buffer_objfwd)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Buffer_Objfwd_611EBD13FF)
# error "Lisp_Buffer_Objfwd changed. See CHECK_STRUCTS comment in config.h."
#endif
  struct Lisp_Buffer_Objfwd out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, buffer_objfwd, type);
  DUMP_FIELD_COPY (&out, buffer_objfwd, offset);
  write_field_lisp_object (ctx, &out, buffer_objfwd, &buffer_objfwd->predicate,
			   WEIGHT_NORMAL);
  return finish_object (ctx, &out, sizeof (out));
}

static dump_off
dump_fwd_kboard_obj (struct dump_context *ctx,
                     const struct Lisp_Kboard_Objfwd *kboard_objfwd)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Kboard_Objfwd_CAA7E71069)
# error "Lisp_Intfwd changed. See CHECK_STRUCTS comment in config.h."
#endif
  struct Lisp_Kboard_Objfwd out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, kboard_objfwd, type);
  DUMP_FIELD_COPY (&out, kboard_objfwd, offset);
  return finish_object (ctx, &out, sizeof (out));
}

static dump_off
dump_fwd (struct dump_context *ctx, lispfwd fwd)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Fwd_Type_9CBA6EE55E)
# error "Lisp_Fwd_Type changed. See CHECK_STRUCTS comment in config.h."
#endif
  void const *p = fwd.fwdptr;
  dump_off offset;

  switch (XFWDTYPE (fwd))
    {
    case Lisp_Fwd_Int:
      offset = dump_fwd_int (ctx, p);
      break;
    case Lisp_Fwd_Bool:
      offset = dump_fwd_bool (ctx, p);
      break;
    case Lisp_Fwd_Obj:
      offset = dump_fwd_obj (ctx, p);
      break;
    case Lisp_Fwd_Buffer_Obj:
      offset = dump_fwd_buffer_obj (ctx, p);
      break;
    case Lisp_Fwd_Kboard_Obj:
      offset = dump_fwd_kboard_obj (ctx, p);
      break;
    default:
      emacs_abort ();
    }

  return offset;
}

static dump_off
recall_symbol_aux (struct dump_context *ctx, Lisp_Object symbol)
{
  Lisp_Object symbol_aux = ctx->symbol_aux;
  if (NILP (symbol_aux))
    return 0;
  return INTEGER_TO_INT (Fgethash (symbol, symbol_aux, make_fixnum (0)));
}

static dump_off
recall_symbol_cvar (struct dump_context *ctx, Lisp_Object symbol)
{
  Lisp_Object symbol_cvar = ctx->symbol_cvar;
  if (NILP (symbol_cvar))
    return 0;
  return INTEGER_TO_INT (Fgethash (symbol, symbol_cvar, make_fixnum (0)));
}

static void
remember_symbol_aux (struct dump_context *ctx,
		     Lisp_Object symbol,
		     dump_off offset)
{
  Fputhash (symbol, INT_TO_INTEGER (offset), ctx->symbol_aux);
}

static void
remember_symbol_cvar (struct dump_context *ctx,
		      Lisp_Object symbol,
		      dump_off offset)
{
  Fputhash (symbol, INT_TO_INTEGER (offset), ctx->symbol_cvar);
}

static void
dump_pre_dump_symbol (struct dump_context *ctx, struct Lisp_Symbol *symbol)
{
  Lisp_Object symbol_lv = make_lisp_ptr (symbol, Lisp_Symbol);
  eassert (!recall_symbol_aux (ctx, symbol_lv));
  eassert (!recall_symbol_cvar (ctx, symbol_lv));
  switch (symbol->u.s.type)
    {
    case SYMBOL_KBOARD:
    case SYMBOL_PER_BUFFER:
    case SYMBOL_FORWARDED:
      remember_symbol_aux (ctx, symbol_lv, dump_fwd (ctx, symbol->u.s.val.fwd));
      break;
    default:
      break;
    }

  if (symbol->u.s.c_variable.fwdptr)
    remember_symbol_cvar (ctx, symbol_lv, dump_fwd (ctx, symbol->u.s.c_variable));
}

static dump_off
dump_symbol (struct dump_context *ctx,
             Lisp_Object object,
             dump_off offset)
{
#if CHECK_STRUCTS && !defined HASH_Lisp_Symbol_61B174C9F4
# error "Lisp_Symbol changed. See CHECK_STRUCTS comment in config.h."
#endif
#if CHECK_STRUCTS && !defined (HASH_symbol_redirect_EA72E4BFF5)
# error "symbol_redirect changed. See CHECK_STRUCTS comment in config.h."
#endif

  if (ctx->flags.defer_symbols)
    {
      if (offset != ON_SYMBOL_QUEUE)
        {
	  eassert (offset == ON_NORMAL_QUEUE
		   || offset == NOT_SEEN);
          struct dump_flags old_flags = ctx->flags;
          ctx->flags.dump_object_contents = false;
          ctx->flags.defer_symbols = false;
          dump_object (ctx, object);
          ctx->flags = old_flags;
          offset = ON_SYMBOL_QUEUE;
          remember_object (ctx, object, offset);
          push (&ctx->deferred_symbols, object);
        }
      return offset; /* !!! */
    }

  struct Lisp_Symbol *symbol = XSYMBOL (object);
  struct Lisp_Symbol out;
  start_object (ctx, &out, sizeof (out));
  eassert (symbol->u.s.gcmarkbit == 0);
  DUMP_FIELD_COPY (&out, symbol, u.s.type);
  DUMP_FIELD_COPY (&out, symbol, u.s.trapped_write);
  DUMP_FIELD_COPY (&out, symbol, u.s.interned);
  DUMP_FIELD_COPY (&out, symbol, u.s.declared_special);
  DUMP_FIELD_COPY (&out, symbol, u.s.pinned);
  DUMP_FIELD_COPY (&out, symbol, u.s.buffer_local_only);
  write_field_lisp_object (ctx, &out, symbol, &symbol->u.s.name, WEIGHT_STRONG);
  switch (symbol->u.s.type)
    {
    case SYMBOL_PLAINVAL:
      write_field_lisp_object (ctx, &out, symbol, &symbol->u.s.val.value,
			       WEIGHT_NORMAL);
      break;
    case SYMBOL_VARALIAS:
      write_field_lisp_xpntr (ctx, &out, symbol, &symbol->u.s.val.alias,
			      Lisp_Symbol, WEIGHT_NORMAL);
      break;
    case SYMBOL_KBOARD:
    case SYMBOL_PER_BUFFER:
    case SYMBOL_FORWARDED:
    case SYMBOL_LOCAL_SOMEWHERE:
      break;
    default:
      emacs_abort ();
      break;
    }

  write_field_lisp_object (ctx, &out, symbol, &symbol->u.s.function, WEIGHT_NORMAL);
  write_field_lisp_object (ctx, &out, symbol, &symbol->u.s.plist, WEIGHT_NORMAL);
  write_field_lisp_object (ctx, &out, symbol, &symbol->u.s.buffer_local_default,
			   WEIGHT_NORMAL);
  write_field_lisp_object (ctx, &out, symbol, &symbol->u.s.buffer_local_buffer,
			   WEIGHT_NORMAL);
  write_field_lisp_xpntr (ctx, &out, symbol, &symbol->u.s.next, Lisp_Symbol,
			  WEIGHT_STRONG);
  offset = finish_object (ctx, &out, sizeof (out));

  switch (symbol->u.s.type)
    {
    case SYMBOL_KBOARD:
    case SYMBOL_PER_BUFFER:
    case SYMBOL_FORWARDED:
      {
	dump_off aux_offset = recall_symbol_aux (ctx, make_lisp_ptr (symbol, Lisp_Symbol));
	remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct Lisp_Symbol, u.s.val.fwd),
			    aux_offset
			    ? aux_offset
			    : dump_fwd (ctx, symbol->u.s.val.fwd));
      }
      break;
    default:
      break;
    }

  if (symbol->u.s.c_variable.fwdptr)
    {
      dump_off cvar_offset = recall_symbol_cvar (ctx, make_lisp_ptr (symbol, Lisp_Symbol));
      remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct Lisp_Symbol, u.s.c_variable),
			  cvar_offset
			  ? cvar_offset
			  : dump_fwd (ctx, symbol->u.s.c_variable));
    }

  return offset;
}

static dump_off
dump_vectorlike_generic (struct dump_context *ctx,
			 const union vectorlike_header *header)
{
#if CHECK_STRUCTS && !defined (HASH_vectorlike_header_785E52047B)
# error "vectorlike_header changed. See CHECK_STRUCTS comment in config.h."
#endif
  const struct Lisp_Vector *v = (const struct Lisp_Vector *) header;
  ptrdiff_t size = header->size;
  enum pvec_type pvectype = PVTYPE (v);
  dump_off offset;

  if (size & PSEUDOVECTOR_FLAG)
    {
      /* Assert that the pseudovector contains only Lisp values ---
         but see the PVEC_SUB_CHAR_TABLE special case below.  We allow
         one extra word of non-lisp data when Lisp_Object is shorter
         than GCALIGN (e.g., on 32-bit builds) to account for
         GCALIGN-enforcing struct padding.  We can't distinguish
         between padding and some undumpable data member this way, but
         we'll count on sizeof(Lisp_Object) >= GCALIGN builds to catch
         this class of problem.
         */
      eassert ((size & PSEUDOVECTOR_REST_MASK) >> PSEUDOVECTOR_REST_BITS
	       <= (sizeof (Lisp_Object) < GCALIGNMENT));
      size &= PSEUDOVECTOR_SIZE_MASK;
    }

  align_output (ctx, DUMP_ALIGNMENT);
  dump_off prefix_start_offset = ctx->offset;

  dump_off skip;
  if (pvectype == PVEC_SUB_CHAR_TABLE)
    {
      /* PVEC_SUB_CHAR_TABLE has a special case because it's a
         variable-length vector (unlike other pseudovectors, which is
         why we handle it here) and has its non-Lisp data _before_ the
         variable-length Lisp part.  */
      const struct Lisp_Sub_Char_Table *sct =
        (const struct Lisp_Sub_Char_Table *) header;
      struct Lisp_Sub_Char_Table out;
      /* Don't use sizeof(out), since that incorporates unwanted
         padding.  Instead, use the size through the last non-Lisp
         field.  */
      size_t sz = (char *)&out.min_char + sizeof (out.min_char) - (char *)&out;
      eassert (sz < DUMP_OFF_MAX);
      start_object (ctx, &out, DUMP_OFF (sz));
      DUMP_FIELD_COPY (&out, sct, header.size);
      DUMP_FIELD_COPY (&out, sct, depth);
      DUMP_FIELD_COPY (&out, sct, min_char);
      offset = finish_object (ctx, &out, DUMP_OFF (sz));
      skip = SUB_CHAR_TABLE_OFFSET;
    }
  else
    {
      union vectorlike_header out;
      start_object (ctx, &out, sizeof (out));
      DUMP_FIELD_COPY (&out, header, size);
      offset = finish_object (ctx, &out, sizeof (out));
      skip = 0;
    }

  /* We may have written a non-Lisp vector prefix above.  If we have,
     pad to the lisp content start with zero, and make sure we didn't
     scribble beyond that start.  */
  dump_off prefix_size = ctx->offset - prefix_start_offset;
  eassert (prefix_size > 0);
  dump_off skip_start = DUMP_OFF ((char *) &v->contents[skip] - (char *) v);
  eassert (skip_start >= prefix_size);
  write_bytes_zero (ctx, skip_start - prefix_size);

  /* start_object isn't what records conservative-GC object
     starts --- dump_object_1 does --- so the hack below of using
     start_object for each vector word doesn't cause GC problems
     at runtime.  */
  struct dump_flags old_flags = ctx->flags;
  ctx->flags.pack_objects = true;
  for (dump_off i = skip; i < size; ++i)
    {
      Lisp_Object out;
      const Lisp_Object *vslot = &v->contents[i];
      /* In the wide case, we're always misaligned.  */
#if INTPTR_MAX == EMACS_INT_MAX
      eassert (ctx->offset % sizeof (out) == 0);
#endif
      start_object (ctx, &out, sizeof (out));
      write_field_lisp_object (ctx, &out, vslot, vslot, WEIGHT_STRONG);
      finish_object (ctx, &out, sizeof (out));
    }
  ctx->flags = old_flags;
  align_output (ctx, DUMP_ALIGNMENT);
  return offset;
}

/* Return a vector of KEY, VALUE pairs in the given hash table H.
   No room for growth is included.  */
static Lisp_Object *
hash_table_contents (struct Lisp_Hash_Table *h)
{
  ptrdiff_t size = h->count;
  Lisp_Object *key_and_value = hash_table_alloc_bytes (2 * size
						       * sizeof *key_and_value);
  ptrdiff_t n = 0;

  DOHASH (h, k, v)
    {
      key_and_value[n++] = k;
      key_and_value[n++] = v;
    }

  return key_and_value;
}

static dump_off
dump_hash_table_list (struct dump_context *ctx)
{
  return !NILP (ctx->hash_tables)
    ? dump_object (ctx, CALLN (Fapply, Qvector, ctx->hash_tables))
    : 0;
}

static hash_table_std_test_t
hash_table_std_test (const struct hash_table_test *t)
{
  if (EQ (t->name, Qeq))
    return Test_eq;
  if (EQ (t->name, Qeql))
    return Test_eql;
  if (EQ (t->name, Qequal))
    return Test_equal;
  error ("cannot dump hash tables with user-defined tests");  /* Bug#36769 */
}

/* Compact contents and discard inessential information from a hash table,
   preparing it for dumping.
   See `hash_table_thaw' for the code that restores the object to a usable
   state. */
static void
hash_table_freeze (struct Lisp_Hash_Table *h)
{
  h->key_and_value = hash_table_contents (h);
  h->next = NULL;
  h->hash = NULL;
  h->index = NULL;
  h->table_size = 0;
  h->index_bits = 0;
  h->frozen_test = hash_table_std_test (h->test);
  h->test = NULL;
}

static dump_off
dump_hash_table_contents (struct dump_context *ctx, struct Lisp_Hash_Table *h)
{
  align_output (ctx, DUMP_ALIGNMENT);
  dump_off start_offset = ctx->offset;
  ptrdiff_t n = 2 * h->count;

  struct dump_flags old_flags = ctx->flags;
  ctx->flags.pack_objects = true;

  for (ptrdiff_t i = 0; i < n; i++)
    {
      Lisp_Object out;
      const Lisp_Object *slot = &h->key_and_value[i];
      start_object (ctx, &out, sizeof out);
      write_field_lisp_object (ctx, &out, slot, slot, WEIGHT_STRONG);
      finish_object (ctx, &out, sizeof out);
    }

  ctx->flags = old_flags;
  return start_offset;
}

static dump_off
dump_hash_table (struct dump_context *ctx, Lisp_Object object)
{
#if CHECK_STRUCTS && !defined HASH_Lisp_Hash_Table_0360833954
# error "Lisp_Hash_Table changed. See CHECK_STRUCTS comment in config.h."
#endif
  const struct Lisp_Hash_Table *hash_in = XHASH_TABLE (object);
  struct Lisp_Hash_Table hash_munged = *hash_in;
  struct Lisp_Hash_Table *hash = &hash_munged;

  hash_table_freeze (hash);
  push (&ctx->hash_tables, object);

  START_DUMP_PVEC (ctx, &hash->header, struct Lisp_Hash_Table, out);
  dump_pseudovector (ctx, &out->header, &hash->header);
  DUMP_FIELD_COPY (out, hash, count);
  DUMP_FIELD_COPY (out, hash, weakness);
  DUMP_FIELD_COPY (out, hash, purecopy);
  DUMP_FIELD_COPY (out, hash, mutable);
  DUMP_FIELD_COPY (out, hash, frozen_test);
  eassert (hash->next_weak == NULL);
  dump_off offset = finish_dump_pvec (ctx, &out->header);
  if (hash->key_and_value)
    remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct Lisp_Hash_Table, key_and_value),
			dump_hash_table_contents (ctx, hash));
  return offset;
}

static dump_off
dump_obarray_buckets (struct dump_context *ctx, const struct Lisp_Obarray *o)
{
  align_output (ctx, DUMP_ALIGNMENT);
  dump_off start_offset = ctx->offset;
  ptrdiff_t n = obarray_size (o);

  struct dump_flags old_flags = ctx->flags;
  ctx->flags.pack_objects = true;

  for (ptrdiff_t i = 0; i < n; i++)
    {
      Lisp_Object out;
      const Lisp_Object *slot = &o->buckets[i];
      start_object (ctx, &out, sizeof out);
      write_field_lisp_object (ctx, &out, slot, slot, WEIGHT_STRONG);
      finish_object (ctx, &out, sizeof out);
    }

  ctx->flags = old_flags;
  return start_offset;
}

static dump_off
dump_obarray (struct dump_context *ctx, Lisp_Object object)
{
#if CHECK_STRUCTS && !defined HASH_Lisp_Obarray_D2757E61AD
# error "Lisp_Obarray changed. See CHECK_STRUCTS comment in config.h."
#endif
  const struct Lisp_Obarray *in_oa = XOBARRAY (object);
  struct Lisp_Obarray munged_oa = *in_oa;
  struct Lisp_Obarray *oa = &munged_oa;
  START_DUMP_PVEC (ctx, &oa->header, struct Lisp_Obarray, out);
  dump_pseudovector (ctx, &out->header, &oa->header);
  DUMP_FIELD_COPY (out, oa, count);
  DUMP_FIELD_COPY (out, oa, size_bits);
  dump_off offset = finish_dump_pvec (ctx, &out->header);
  remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct Lisp_Obarray, buckets),
		      dump_obarray_buckets (ctx, oa));
  return offset;
}

static dump_off
dump_buffer (struct dump_context *ctx, const struct buffer *in_buffer)
{
#if CHECK_STRUCTS && !defined HASH_buffer_B02F648B82
# error "buffer changed. See CHECK_STRUCTS comment in config.h."
#endif
  struct buffer munged_buffer = *in_buffer;
  struct buffer *buffer = &munged_buffer;

  /* Clear some buffer state for correctness upon load.  */
  if (buffer->base_buffer == NULL)
    buffer->window_count = 0;
  else
    eassert (buffer->window_count == -1);
  buffer->local_minor_modes_ = Qnil;
  buffer->last_name_ = Qnil;
  buffer->last_selected_window_ = Qnil;
  buffer->display_count_ = make_fixnum (0);
  buffer->clip_changed = 0;
  buffer->last_window_start = -1;
  buffer->point_before_scroll_ = Qnil;

  dump_off base_offset = 0;
  if (buffer->base_buffer)
    {
      eassert (buffer->base_buffer->base_buffer == NULL);
      base_offset = dump_object	(ctx, make_lisp_ptr (buffer->base_buffer, Lisp_Vectorlike));
    }

  eassert ((base_offset == 0 && buffer->text == &in_buffer->own_text)
	   || (base_offset > 0 && buffer->text != &in_buffer->own_text));

  START_DUMP_PVEC (ctx, &buffer->header, struct buffer, out);
  dump_pseudovector (ctx, &out->header, &buffer->header);
  if (base_offset == 0)
    base_offset = ctx->obj_offset;
  eassert (base_offset > 0);
  if (buffer->base_buffer == NULL)
    {
      eassert (base_offset == ctx->obj_offset);

      if (BUFFER_LIVE_P (buffer))
	remember_cold_op (ctx, COLD_OP_BUFFER,
			  make_lisp_ptr ((void *) in_buffer, Lisp_Vectorlike));
      else
        eassert (buffer->own_text.beg == NULL);

      DUMP_FIELD_COPY (out, buffer, own_text.gpt);
      DUMP_FIELD_COPY (out, buffer, own_text.z);
      DUMP_FIELD_COPY (out, buffer, own_text.gpt_byte);
      DUMP_FIELD_COPY (out, buffer, own_text.z_byte);
      DUMP_FIELD_COPY (out, buffer, own_text.gap_size);
      DUMP_FIELD_COPY (out, buffer, own_text.modiff);
      DUMP_FIELD_COPY (out, buffer, own_text.chars_modiff);
      DUMP_FIELD_COPY (out, buffer, own_text.save_modiff);
      DUMP_FIELD_COPY (out, buffer, own_text.overlay_modiff);
      DUMP_FIELD_COPY (out, buffer, own_text.compact);
      DUMP_FIELD_COPY (out, buffer, own_text.beg_unchanged);
      DUMP_FIELD_COPY (out, buffer, own_text.end_unchanged);
      DUMP_FIELD_COPY (out, buffer, own_text.unchanged_modified);
      DUMP_FIELD_COPY (out, buffer, own_text.overlay_unchanged_modified);
      write_field_lisp_xpntr (ctx, out, buffer, &buffer->own_text.markers,
			      Lisp_Vectorlike, WEIGHT_NORMAL);
      DUMP_FIELD_COPY (out, buffer, own_text.inhibit_shrinking);
      DUMP_FIELD_COPY (out, buffer, own_text.redisplay);
      DUMP_FIELD_COPY (out, buffer, own_text.monospace);
    }

  eassert (ctx->obj_offset > 0);
  remember_fixup_ptr (ctx, ctx->obj_offset + DUMP_OFFSETOF (struct buffer, text),
		      base_offset + DUMP_OFFSETOF (struct buffer, own_text));

  DUMP_FIELD_COPY (out, buffer, pt);
  DUMP_FIELD_COPY (out, buffer, pt_byte);
  DUMP_FIELD_COPY (out, buffer, begv);
  DUMP_FIELD_COPY (out, buffer, begv_byte);
  DUMP_FIELD_COPY (out, buffer, zv);
  DUMP_FIELD_COPY (out, buffer, zv_byte);

  if (buffer->base_buffer)
    {
      eassert (ctx->obj_offset != base_offset);
      write_field_dump_ptr (ctx, out, buffer, &buffer->base_buffer, base_offset);
    }

  DUMP_FIELD_COPY (out, buffer, indirections);
  DUMP_FIELD_COPY (out, buffer, window_count);

  memcpy (out->local_flags,
          &buffer->local_flags,
          sizeof (out->local_flags));
  DUMP_FIELD_COPY (out, buffer, modtime);
  DUMP_FIELD_COPY (out, buffer, modtime_size);
  DUMP_FIELD_COPY (out, buffer, auto_save_modified);
  DUMP_FIELD_COPY (out, buffer, display_error_modiff);
  DUMP_FIELD_COPY (out, buffer, auto_save_failure_time);
  DUMP_FIELD_COPY (out, buffer, last_window_start);

  /* Not worth serializing these caches.  TODO: really? */
  out->newline_cache = NULL;
  out->width_run_cache = NULL;
  out->bidi_paragraph_cache = NULL;

  DUMP_FIELD_COPY (out, buffer, prevent_redisplay_optimizations_p);
  DUMP_FIELD_COPY (out, buffer, clip_changed);
  DUMP_FIELD_COPY (out, buffer, inhibit_buffer_hooks);

  if (!itree_empty_p (buffer->overlays))
    {
      /* We haven't implemented the code to dump overlays.  */
      error ("dumping overlays is not yet implemented");
    }
  else
    out->overlays = NULL;

  write_field_lisp_object (ctx, out, buffer, &buffer->undo_list_, WEIGHT_STRONG);
  dump_off offset = finish_dump_pvec (ctx, &out->header);
  if (!buffer->base_buffer && buffer->own_text.intervals)
    remember_fixup_ptr (ctx, offset + DUMP_OFFSETOF (struct buffer, own_text.intervals),
			dump_interval_tree (ctx, buffer->own_text.intervals, 0));

  return offset;
}

static dump_off
dump_bool_vector (struct dump_context *ctx, const struct Lisp_Vector *v)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Vector_3091289B35)
# error "Lisp_Vector changed. See CHECK_STRUCTS comment in config.h."
#endif
  /* No relocation needed, so we don't need start_object.  */
  align_output (ctx, DUMP_ALIGNMENT);
  eassert (ctx->offset >= ctx->header.cold_start);
  dump_off offset = ctx->offset;
  ptrdiff_t nbytes = vector_nbytes ((struct Lisp_Vector *) v);
  if (nbytes > DUMP_OFF_MAX)
    error ("vector too large");
  write_bytes (ctx, v, DUMP_OFF (nbytes));
  return offset;
}

static dump_off
dump_subr (struct dump_context *ctx, const struct Lisp_Subr *subr)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Subr_20B7443AD7)
# error "Lisp_Subr changed. See CHECK_STRUCTS comment in config.h."
#endif
  struct Lisp_Subr out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, subr, header.size);
#ifdef HAVE_NATIVE_COMP
  bool non_primitive = !NILP (subr->native_comp_u);
#else
  bool non_primitive = false;
#endif
  if (non_primitive)
    out.function.a0 = NULL;
  else
    write_field_emacs_ptr (ctx, &out, subr, &subr->function.a0);
  DUMP_FIELD_COPY (&out, subr, min_args);
  DUMP_FIELD_COPY (&out, subr, max_args);
  if (non_primitive)
    {
      remember_cold_op (ctx, COLD_OP_NATIVE_SUBR,
			make_lisp_ptr ((void *) subr, Lisp_Vectorlike));
      write_field_lisp_object (ctx, &out, subr, &subr->intspec.native,
			       WEIGHT_NORMAL);
      write_field_lisp_object (ctx, &out, subr, &subr->command_modes,
			       WEIGHT_NORMAL);
    }
  else
    {
      write_field_emacs_ptr (ctx, &out, subr, &subr->symbol_name);
      write_field_emacs_ptr (ctx, &out, subr, &subr->intspec.string);
      write_field_emacs_ptr (ctx, &out, subr, &subr->command_modes);
    }
  DUMP_FIELD_COPY (&out, subr, doc);
#ifdef HAVE_NATIVE_COMP
  write_field_lisp_object (ctx, &out, subr, &subr->native_comp_u, WEIGHT_NORMAL);
  write_field_lisp_object (ctx, &out, subr, &subr->lambda_list, WEIGHT_NORMAL);
  write_field_lisp_object (ctx, &out, subr, &subr->type, WEIGHT_NORMAL);
#endif
  dump_off subr_off = finish_object (ctx, &out, sizeof (out));
  if (non_primitive && ctx->flags.dump_object_contents)
    /* Must follow compilation units in NATIVE_COMP_RELOCS. */
    push (&ctx->dump_relocs[LATE_RELOCS],
	  list2 (make_fixnum (RELOC_NATIVE_SUBR),
		 INT_TO_INTEGER (subr_off)));
  return subr_off;
}

#ifdef HAVE_NATIVE_COMP
static dump_off
dump_native_comp_unit (struct dump_context *ctx,
		       struct Lisp_Native_Comp_Unit *comp_u)
{
  /* Have function documentation always lazy loaded to optimize load-time.  */
  comp_u->data_fdoc_v = Qnil;
  START_DUMP_PVEC (ctx, &comp_u->header, struct Lisp_Native_Comp_Unit, out);
  dump_pseudovector (ctx, &out->header, &comp_u->header);
  out->handle = NULL;

  dump_off comp_u_off = finish_dump_pvec (ctx, &out->header);
  if (ctx->flags.dump_object_contents)
    /* Do real elf after EARLY_RELOCS. */
    push (&ctx->dump_relocs[NATIVE_COMP_RELOCS],
	  list2 (make_fixnum (RELOC_NATIVE_COMP_UNIT),
		 INT_TO_INTEGER (comp_u_off)));
  return comp_u_off;
}
#endif

static void
fill_pseudovec (union vectorlike_header *header, Lisp_Object item)
{
  struct Lisp_Vector *v = (struct Lisp_Vector *) header;
  eassert (v->header.size & PSEUDOVECTOR_FLAG);
  ptrdiff_t size = v->header.size & PSEUDOVECTOR_SIZE_MASK;
  for (ptrdiff_t idx = 0; idx < size; idx++)
    v->contents[idx] = item;
}

static dump_off
dump_nulled_pseudovec (struct dump_context *ctx,
                       const union vectorlike_header *in)
{
  START_DUMP_PVEC (ctx, in, struct Lisp_Vector, out);
  fill_pseudovec (&out->header, Qnil);
  return finish_dump_pvec (ctx, &out->header);
}

static dump_off
dump_vectorlike (struct dump_context *ctx,
                 Lisp_Object lv,
                 dump_off offset)
{
#if CHECK_STRUCTS && !defined HASH_pvec_type_19F6CF5169
# error "pvec_type changed. See CHECK_STRUCTS comment in config.h."
#endif
  const struct Lisp_Vector *v = XVECTOR (lv);
  enum pvec_type ptype = PVTYPE (v);
  switch (ptype)
    {
    case PVEC_FONT:
      /* There are three kinds of font objects that all use PVEC_FONT,
         distinguished by their size.  Font specs and entities are
         harmless data carriers that we can dump like other Lisp
         objects.  Fonts themselves are window-system-specific and
         need to be recreated on each startup.  */
      if ((v->header.size & PSEUDOVECTOR_SIZE_MASK) != FONT_SPEC_MAX
	  && (v->header.size & PSEUDOVECTOR_SIZE_MASK) != FONT_ENTITY_MAX)
        error_unsupported_dump_object(ctx, lv, "font");
      FALLTHROUGH;
    case PVEC_NORMAL_VECTOR:
    case PVEC_CLOSURE:
    case PVEC_CHAR_TABLE:
    case PVEC_SUB_CHAR_TABLE:
    case PVEC_RECORD:
      return dump_vectorlike_generic (ctx, &v->header);
    case PVEC_BOOL_VECTOR:
      return dump_bool_vector(ctx, v);
    case PVEC_HASH_TABLE:
      return dump_hash_table (ctx, lv);
    case PVEC_OBARRAY:
      return dump_obarray (ctx, lv);
    case PVEC_BUFFER:
      return dump_buffer (ctx, XBUFFER (lv));
    case PVEC_SUBR:
      return dump_subr (ctx, XSUBR (lv));
    case PVEC_FRAME:
    case PVEC_WINDOW:
    case PVEC_PROCESS:
    case PVEC_TERMINAL:
      return dump_nulled_pseudovec (ctx, &v->header);
    case PVEC_MARKER:
      return dump_marker (ctx, XMARKER (lv));
    case PVEC_OVERLAY:
      return dump_overlay (ctx, XOVERLAY (lv));
    case PVEC_FINALIZER:
      return dump_finalizer (ctx, XFINALIZER (lv));
    case PVEC_BIGNUM:
      return dump_bignum (ctx, lv);
    case PVEC_NATIVE_COMP_UNIT:
#ifdef HAVE_NATIVE_COMP
      return dump_native_comp_unit (ctx, XNATIVE_COMP_UNIT (lv));
#endif
      break;
    case PVEC_THREAD:
      eassert (emacs_ptr (lv));
      return IS_RUNTIME_MAGIC;
      break;
    case PVEC_WINDOW_CONFIGURATION:
    case PVEC_OTHER:
    case PVEC_XWIDGET:
    case PVEC_XWIDGET_VIEW:
    case PVEC_MISC_PTR:
    case PVEC_USER_PTR:
    case PVEC_MUTEX:
    case PVEC_CONDVAR:
    case PVEC_SQLITE:
    case PVEC_MODULE_FUNCTION:
    case PVEC_FREE:
    case PVEC_TREE_SITTER:
    case PVEC_TREE_SITTER_NODE:
    case PVEC_TREE_SITTER_CURSOR:
      break;
    }
  char msg[60];
  snprintf (msg, sizeof msg, "pseudovector type %u", ptype);
  error_unsupported_dump_object (ctx, lv, msg);
}

/* Add an object to the dump.

   CTX is the dump context; OBJECT is the object to add.  Normally,
   return a positive OFFSET, in bytes from the start of the dump, where
   we wrote the object.  If deferring, return one of
   special_dump_offset.

   Some self-representing objects are immediate values rather than
   tagged Lisp_Objects and are not dumped.
*/
static dump_off
dump_object (struct dump_context *ctx, Lisp_Object object)
{
#if CHECK_STRUCTS && !defined (HASH_Lisp_Type_45F0582FD7)
# error "Lisp_Type changed. See CHECK_STRUCTS comment in config.h."
#endif
  eassert (!EQ (object, dead_object ()));

  dump_off offset = recall_object (ctx, object);
  if (offset > 0)
    return offset;  /* Object already dumped.  */

  bool cold = BOOL_VECTOR_P (object) || FLOATP (object);
  if (cold && ctx->flags.defer_cold_objects)
    {
      if (offset != ON_COLD_QUEUE)
        {
	  eassert (offset == ON_NORMAL_QUEUE
		   || offset == NOT_SEEN);
          offset = ON_COLD_QUEUE;
          remember_object (ctx, object, offset);
          remember_cold_op (ctx, COLD_OP_OBJECT, object);
        }
      return offset;
    }

  if (emacs_ptr (object) && ctx->flags.defer_copied_objects)
    {
      if (offset != ON_COPIED_QUEUE)
        {
	  eassert (offset == ON_NORMAL_QUEUE || offset == NOT_SEEN);
          /* Recursively enqueue object's referents.  */
          struct dump_flags restore_flags = ctx->flags;
          ctx->flags.dump_object_contents = false;
          ctx->flags.defer_copied_objects = false;
          dump_object (ctx, object);
          ctx->flags = restore_flags;

          offset = ON_COPIED_QUEUE;
          remember_object (ctx, object, offset);
          push (&ctx->copied_queue, object);
        }
      return offset;
    }

  /* Object needs to be dumped.  */
  switch (XTYPE (object))
    {
    case Lisp_Symbol:
      offset = dump_symbol (ctx, object, offset);
      break;
    case Lisp_String:
      offset = dump_string (ctx, XSTRING (object));
      break;
    case Lisp_Vectorlike:
      offset = dump_vectorlike (ctx, object, offset);
      break;
    case Lisp_Cons:
      offset = dump_cons (ctx, XCONS (object));
      break;
    case Lisp_Float:
      offset = dump_float (ctx, XFLOAT (object));
      break;
    default:
      /* self representing, e.g., Lisp_Int, ought not be dumped */
      emacs_abort ();
      break;
    }

  /* offset can be < 0 if deferred.  */
  if (ctx->flags.dump_object_contents && offset > 0)
    {
      eassert (offset % DUMP_ALIGNMENT == 0);
      remember_object (ctx, object, offset);
      if (ctx->flags.record_object_starts)
        {
          eassert (!ctx->flags.pack_objects);
          push (&ctx->object_starts, list2 (INT_TO_INTEGER (XTYPE (object)),
					    INT_TO_INTEGER (offset)));
        }
    }

  return offset;
}

static dump_off
dump_charset (struct dump_context *ctx, int cs_i)
{
#if CHECK_STRUCTS && !defined (HASH_charset_E31F4B5D96)
# error "charset changed. See CHECK_STRUCTS comment in config.h."
#endif
  align_output (ctx, alignof (struct charset));
  const struct charset *cs = charset_table + cs_i;
  struct charset out;
  start_object (ctx, &out, sizeof (out));
  DUMP_FIELD_COPY (&out, cs, id);
  write_field_lisp_object (ctx, &out, cs, &cs->attributes, WEIGHT_NORMAL);
  DUMP_FIELD_COPY (&out, cs, dimension);
  memcpy (out.code_space, &cs->code_space, sizeof (cs->code_space));
  DUMP_FIELD_COPY (&out, cs, code_linear_p);
  DUMP_FIELD_COPY (&out, cs, iso_chars_96);
  DUMP_FIELD_COPY (&out, cs, ascii_compatible_p);
  DUMP_FIELD_COPY (&out, cs, supplementary_p);
  DUMP_FIELD_COPY (&out, cs, compact_codes_p);
  DUMP_FIELD_COPY (&out, cs, unified_p);
  DUMP_FIELD_COPY (&out, cs, iso_final);
  DUMP_FIELD_COPY (&out, cs, iso_revision);
  DUMP_FIELD_COPY (&out, cs, emacs_mule_id);
  DUMP_FIELD_COPY (&out, cs, method);
  DUMP_FIELD_COPY (&out, cs, min_code);
  DUMP_FIELD_COPY (&out, cs, max_code);
  DUMP_FIELD_COPY (&out, cs, char_index_offset);
  DUMP_FIELD_COPY (&out, cs, min_char);
  DUMP_FIELD_COPY (&out, cs, max_char);
  DUMP_FIELD_COPY (&out, cs, invalid_code);
  memcpy (out.fast_map, &cs->fast_map, sizeof (cs->fast_map));
  DUMP_FIELD_COPY (&out, cs, code_offset);
  dump_off offset = finish_object (ctx, &out, sizeof (out));
  if (cs_i < charset_table_used && cs->code_space_mask)
    remember_cold_op (ctx, COLD_OP_CHARSET,
		      Fcons (INT_TO_INTEGER (cs_i), INT_TO_INTEGER (offset)));
  return offset;
}

static dump_off
dump_charset_table (struct dump_context *ctx)
{
  struct dump_flags old_flags = ctx->flags;
  ctx->flags.pack_objects = true;
  align_output (ctx, DUMP_ALIGNMENT);
  const dump_off ret = ctx->offset;
  /* Dump entire table, not just the used slots, otherwise table will be
     smaller than charset_table_size upon load.  */
  for (int i = 0; i < charset_table_size; ++i)
    dump_charset (ctx, i);
  reloc_to_dump_ptr (ctx, &charset_table, ret);
  ctx->flags = old_flags;
  return ret;
}

static void
dump_finalizer_list_head_ptr (struct dump_context *ctx,
                              struct Lisp_Finalizer **ptr)
{
  struct Lisp_Finalizer *value = *ptr;
  if (value != &finalizers && value != &doomed_finalizers)
    reloc_to_dump_ptr (ctx, ptr, dump_object
		       (ctx, make_lisp_ptr (value, Lisp_Vectorlike)));
}

static void
dump_metadata_for_pdumper (struct dump_context *ctx)
{
  for (int i = 0; i < nr_dump_hooks; ++i)
    reloc_to_emacs_ptr (ctx, &dump_hooks[i], (void const *) dump_hooks[i]);
  reloc_immediate_int (ctx, &nr_dump_hooks, nr_dump_hooks);

  for (int i = 0; i < nr_remembered_data; ++i)
    {
      reloc_to_emacs_ptr (ctx, &remembered_data[i].mem, remembered_data[i].mem);
      reloc_immediate_int (ctx, &remembered_data[i].sz, remembered_data[i].sz);
    }
  reloc_immediate_int (ctx, &nr_remembered_data, nr_remembered_data);
}

/* Sort the list of copied objects in CTX.  */
static void
dump_sort_copied_objects (struct dump_context *ctx)
{
  /* Sort the objects into the order in which they'll appear in the
     Emacs: this way, on startup, we'll do both the IO from the dump
     file and the copy into Emacs in-order, where prefetch will be
     most effective.  */
  ctx->copied_queue =
    CALLN (Fsort, Fnreverse (ctx->copied_queue),
           Qdump_emacs_portable__sort_predicate_copied);
}

/* Dump parts of copied objects we need at runtime.  */
static void
dump_hot_parts_of_discardable_objects (struct dump_context *ctx)
{
  Lisp_Object copied_queue = ctx->copied_queue;
  while (!NILP (copied_queue))
    {
      Lisp_Object copied = pop (&copied_queue);
      if (SYMBOLP (copied))
        {
          eassert (builtin_symbol_p (copied));
          dump_pre_dump_symbol (ctx, XSYMBOL (copied));
        }
    }
}

static void
drain_copied_objects (struct dump_context *ctx)
{
  Lisp_Object copied_queue = ctx->copied_queue;
  ctx->copied_queue = Qnil;

  struct dump_flags old_flags = ctx->flags;

  /* We should have already fully scanned these objects, so assert
     that we're not adding more entries to the dump queue.  */
  ctx->flags.assert_already_seen = true;

  /* Now we want to actually dump the copied objects, not just record
     them.  */
  ctx->flags.defer_copied_objects = false;

  /* Objects that we memcpy into Emacs shouldn't get object-start
     records (which conservative GC looks at): we usually discard this
     memory after we're finished memcpying, and even if we don't, the
     "real" objects in this section all live in the Emacs image, not
     in the dump.  */
  ctx->flags.record_object_starts = false;

  /* Dump the objects and generate a copy relocation for each.  Don't
     bother trying to reduce the number of copy relocations we
     generate: we'll merge adjacent copy relocations upon output.
     The overall result is that to the greatest extent possible while
     maintaining strictly increasing address order, we copy into Emacs
     in nice big chunks.  */
  while (!NILP (copied_queue))
    {
      Lisp_Object copied = pop (&copied_queue);
      void *optr = emacs_ptr (copied);
      eassert (optr != NULL);
      /* N.B. start_offset is beyond any padding we insert.  */
      dump_off start_offset = dump_object (ctx, copied);
      if (start_offset != IS_RUNTIME_MAGIC)
        {
          dump_off size = ctx->offset - start_offset;
          reloc_copy_from_dump (ctx, start_offset, optr, size);
        }
    }

  ctx->flags = old_flags;
}

static void
dump_cold_string (struct dump_context *ctx, Lisp_Object string)
{
  /* Dump string contents.  */
  dump_off string_offset = recall_object (ctx, string);
  eassert (string_offset > 0);
  if (SBYTES (string) > DUMP_OFF_MAX - 1)
    error ("string too large");
  dump_off total_size = DUMP_OFF (SBYTES (string) + 1);
  eassert (total_size > 0);
  remember_fixup_ptr (ctx, string_offset + DUMP_OFFSETOF (struct Lisp_String, u.s.data),
		      ctx->offset);
  write_bytes (ctx, XSTRING (string)->u.s.data, total_size);
}

static void
dump_cold_charset (struct dump_context *ctx, Lisp_Object data)
{
  /* Dump charset lookup tables.  */
  int cs_i = XFIXNUM (XCAR (data));
  dump_off cs_offset = INTEGER_TO_INT (XCDR (data));
  remember_fixup_ptr (ctx, cs_offset + DUMP_OFFSETOF (struct charset, code_space_mask),
		      ctx->offset);
  struct charset *cs = charset_table + cs_i;
  write_bytes (ctx, cs->code_space_mask, 256);
}

static void
dump_cold_buffer (struct dump_context *ctx, Lisp_Object data)
{
  /* Dump buffer text.  */
  dump_off buffer_offset = recall_object (ctx, data);
  eassert (buffer_offset > 0);
  struct buffer *b = XBUFFER (data);
  eassert (b->text == &b->own_text);
  /* Zero the gap so we don't dump uninitialized bytes.  */
  memset (BUF_GPT_ADDR (b), 0, BUF_GAP_SIZE (b));
  /* See buffer.c for this calculation.  */
  ptrdiff_t nbytes =
    BUF_Z_BYTE (b)
    - BUF_BEG_BYTE (b)
    + BUF_GAP_SIZE (b)
    + 1;
  if (nbytes > DUMP_OFF_MAX)
    error ("buffer too large");
  remember_fixup_ptr (ctx, buffer_offset + DUMP_OFFSETOF (struct buffer, own_text.beg),
		      ctx->offset);
  write_bytes (ctx, b->own_text.beg, DUMP_OFF (nbytes));
}

static void
dump_cold_bignum (struct dump_context *ctx, Lisp_Object object)
{
  mpz_t const *n = xbignum_val (object);
  size_t sz_nlimbs = mpz_size (*n);
  eassert (sz_nlimbs < DUMP_OFF_MAX);
  align_output (ctx, alignof (mp_limb_t));
  dump_off nlimbs = DUMP_OFF (sz_nlimbs);
  Lisp_Object descriptor
    = list2 (INT_TO_INTEGER (ctx->offset),
	     INT_TO_INTEGER (mpz_sgn (*n) < 0 ? -nlimbs : nlimbs));
  Fputhash (object, descriptor, ctx->bignum_data);
  for (mp_size_t i = 0; i < nlimbs; ++i)
    {
      mp_limb_t limb = mpz_getlimbn (*n, i);
      write_bytes (ctx, &limb, sizeof (limb));
    }
}

#ifdef HAVE_NATIVE_COMP
static void
dump_cold_native_subr (struct dump_context *ctx, Lisp_Object subr)
{
  /* Dump subr contents.  */
  dump_off subr_offset = recall_object (ctx, subr);
  eassert (subr_offset > 0);
  remember_fixup_ptr (ctx, subr_offset + DUMP_OFFSETOF (struct Lisp_Subr, symbol_name),
		      ctx->offset);
  const char *symbol_name = XSUBR (subr)->symbol_name;
  write_bytes (ctx, symbol_name, 1 + strlen (symbol_name));

  remember_fixup_ptr (ctx, subr_offset + DUMP_OFFSETOF (struct Lisp_Subr, native_c_name),
		      ctx->offset);
  const char *c_name = XSUBR (subr)->native_c_name;
  write_bytes (ctx, c_name, 1 + strlen (c_name));
}
#endif

static void
drain_cold_data (struct dump_context *ctx)
{
  Lisp_Object cold_queue = Fnreverse (ctx->cold_queue);
  ctx->cold_queue = Qnil;

  struct dump_flags old_flags = ctx->flags;

  /* We should have already scanned all objects to which our cold
     objects refer, so die if an object points to something we haven't
     seen.  */
  ctx->flags.assert_already_seen = true;

  /* Actually dump cold objects instead of deferring them.  */
  ctx->flags.defer_cold_objects = false;

  while (!NILP (cold_queue))
    {
      Lisp_Object item = pop (&cold_queue);
      enum cold_op op = (enum cold_op) XFIXNUM (XCAR (item));
      Lisp_Object data = XCDR (item);
      switch (op)
        {
        case COLD_OP_STRING:
          dump_cold_string (ctx, data);
          break;
        case COLD_OP_CHARSET:
          dump_cold_charset (ctx, data);
          break;
        case COLD_OP_BUFFER:
          dump_cold_buffer (ctx, data);
          break;
        case COLD_OP_OBJECT:
          /* Objects that we can put in the cold section
             must not refer to other objects.  */
          eassert (queue_empty_p (&ctx->queue));
          eassert (ctx->flags.dump_object_contents);
          dump_object (ctx, data);
          eassert (queue_empty_p (&ctx->queue));
          break;
        case COLD_OP_BIGNUM:
          dump_cold_bignum (ctx, data);
          break;
#ifdef HAVE_NATIVE_COMP
	case COLD_OP_NATIVE_SUBR:
	  dump_cold_native_subr (ctx, data);
	  break;
#endif
        default:
          emacs_abort ();
        }
    }

  ctx->flags = old_flags;
}

static void
read_ptr_and_lv (const void *mem,
                     enum Lisp_Type type,
                     void **out_ptr,
                     Lisp_Object *out_lv)
{
  memcpy (out_ptr, mem, sizeof (*out_ptr));
  if (*out_ptr != NULL)
    {
      switch (type)
        {
        case Lisp_Symbol:
        case Lisp_String:
        case Lisp_Vectorlike:
        case Lisp_Cons:
        case Lisp_Float:
          *out_lv = make_lisp_ptr (*out_ptr, type);
          break;
        default:
          emacs_abort ();
        }
    }
}

/* Enqueue for dumping objects referenced by static non-Lisp_Object
   pointers inside Emacs.  */
static void
drain_user_remembered_data_hot (struct dump_context *ctx)
{
  for (int i = 0; i < nr_remembered_data; ++i)
    {
      void *mem = remembered_data[i].mem;
      int sz = remembered_data[i].sz;
      if (sz <= 0)
        {
          enum Lisp_Type type = -sz;
          void *value;
          Lisp_Object lv;
          read_ptr_and_lv (mem, type, &value, &lv);
          if (value != NULL)
	    enqueue_object (ctx, lv, WEIGHT_NONE);
        }
    }
}

/* Dump user-specified non-relocated data.  */
static void
drain_user_remembered_data_cold (struct dump_context *ctx)
{
  for (int i = 0; i < nr_remembered_data; ++i)
    {
      void *mem = remembered_data[i].mem;
      int sz = remembered_data[i].sz;
      if (sz > 0)
        {
          /* Scalar: try to inline the value into the relocation if
             it's small enough; if it's bigger than we can fit in a
             relocation, we have to copy the data into the dump proper
             and emit a copy relocation.  */
          if (sz <= sizeof (intmax_t))
            reloc_immediate (ctx, mem, mem, sz);
          else
            {
              reloc_copy_from_dump (ctx, ctx->offset, mem, sz);
              write_bytes (ctx, mem, sz);
            }
        }
      else
        {
          /* *mem is a raw pointer to a Lisp object of some sort.
             The object to which it points should have already been
             dumped by drain_user_remembered_data_hot.  */
          void *value;
          Lisp_Object lv;
          enum Lisp_Type type = -sz;
          read_ptr_and_lv (mem, type, &value, &lv);
          if (value == NULL)
            /* We can't just ignore NULL: the variable might have
               transitioned from non-NULL to NULL, and we want to
               record this fact.  */
            reloc_immediate_ptrdiff_t (ctx, mem, 0);
          else
            {
              if (emacs_ptr (lv) != NULL)
                {
                  /* Out-of-dump objects like built-in symbols and subrs.  */
		  reloc_to_emacs_ptr (ctx, mem, emacs_ptr (lv));
                }
              else
                {
                  eassert (!self_representing_p (lv));
                  dump_off offset = recall_object (ctx, lv);
                  if (offset <= 0)
                    error ("raw-pointer object not dumped?!");
                  reloc_to_dump_ptr (ctx, mem, offset);
                }
            }
        }
    }
}

static void
unwind_cleanup (void *data)
{
  struct dump_context *ctx = data;
  if (ctx->fd >= 0)
    emacs_close (ctx->fd);
#ifdef REL_ALLOC
  if (ctx->blocked_ralloc)
    r_alloc_inhibit_buffer_relocation (0);
#endif
  Vpdumper__pure_pool = ctx->restore_pure_pool;
  Vpost_gc_hook = ctx->restore_post_gc_hook;
  Vprocess_environment = ctx->restore_process_environment;
}

static struct dump_reloc
destructure_lreloc (Lisp_Object lreloc)
{
  struct dump_reloc reloc;
  reloc.type = (enum reloc_type) XFIXNUM (pop (&lreloc));
  eassert (reloc.type < RELOC_EMACS_LV + Lisp_Type_Max);
  reloc.offset = INTEGER_TO_INT (pop (&lreloc));
  eassert (NILP (lreloc));
  return reloc;
}

static void
emit_dump_reloc (struct dump_context *ctx, Lisp_Object lreloc)
{
  eassert (ctx->flags.pack_objects);
  struct dump_reloc reloc;
  start_object (ctx, &reloc, sizeof (reloc));
  reloc = destructure_lreloc (lreloc);
  finish_object (ctx, &reloc, sizeof (reloc));
  if (reloc.offset < ctx->header.discardable_start)
    ++ctx->nr_hot_relocations;
  else
    ++ctx->nr_discardable_relocations;
}

static Lisp_Object
merge_dump_relocs (struct dump_context *ctx, Lisp_Object lreloc_a, Lisp_Object lreloc_b)
{
#ifdef ENABLE_CHECKING
  /* merely check overlap */
  struct dump_reloc reloc_a = destructure_lreloc (lreloc_a);
  struct dump_reloc reloc_b = destructure_lreloc (lreloc_b);
  eassert (reloc_a.offset < reloc_b.offset);
#else
  (void) ctx;
  (void) lreloc_a;
  (void) lreloc_b;
#endif
  return Qnil;
}

/* Translate a Lisp Emacs-relocation descriptor (a list whose first
   element is one of the EMACS_RELOC_* values, encoded as a fixnum)
   into an emacs_reloc structure value suitable for writing to the
   dump file.
*/
static struct emacs_reloc
destructure_emacs_reloc (struct dump_context *ctx, Lisp_Object lreloc)
{
  struct emacs_reloc reloc = {0};
  int type = XFIXNUM (pop (&lreloc));
  reloc.offset = INTEGER_TO_INT (pop (&lreloc));
  eassert (labs (reloc.offset) <= 60 * 1024 * 1024);
  switch (type)
    {
    case RELOC_COPY_FROM_DUMP:
      reloc.type = type;
      reloc.ptr.offset = INTEGER_TO_INT (pop (&lreloc));
      reloc.length = INTEGER_TO_INT (pop (&lreloc));
      eassert (reloc.ptr.offset < ctx->end_heap);
      break;
    case RELOC_IMMEDIATE:
      reloc.type = type;
      reloc.ptr.immediate = INTEGER_TO_INT (pop (&lreloc));
      reloc.length = INTEGER_TO_INT (pop (&lreloc));
      break;
    case RELOC_EMACS_PTR:
      reloc.type = type;
      reloc.ptr.offset = INTEGER_TO_INT (pop (&lreloc));
      eassert (labs (reloc.ptr.offset) <= 60 * 1024 * 1024);
      break;
    case RELOC_DUMP_PTR:
      reloc.type = type;
      reloc.ptr.offset = INTEGER_TO_INT (pop (&lreloc));
      eassert (reloc.ptr.offset < ctx->end_heap);
      break;
    case RELOC_DUMP_LV:
    case RELOC_EMACS_LV:
      {
	reloc.type = type;
        Lisp_Object target_value = pop (&lreloc);
        eassert (!self_representing_p (target_value)); /* reloc_to_lv fail */

        int tag_type = XTYPE (target_value);
        reloc.length = tag_type;

        if (type == RELOC_EMACS_LV)
	  reloc.ptr.offset = emacs_offset (emacs_ptr (target_value));
        else
          {
	    eassume (ctx); /* Pacify GCC 9.2.1 -O3 -Wnull-dereference.  */
            eassert (!emacs_ptr (target_value));
            reloc.ptr.offset = recall_object (ctx, target_value);
            if (reloc.ptr.offset <= 0)
              {
                Lisp_Object repr = Fprin1_to_string (target_value, Qnil, Qnil);
                error ("relocation target was not dumped: %s", SDATA (repr));
              }
	    eassert (reloc.ptr.offset < ctx->end_heap);
          }
      }
      break;
    default:
      emacs_abort ();
      break;
    }

  /* We should have consumed the whole relocation descriptor.  */
  eassert (NILP (lreloc));
  return reloc;
}

static void
emit_emacs_reloc (struct dump_context *ctx, Lisp_Object lreloc)
{
  eassert (ctx->flags.pack_objects);
  struct emacs_reloc reloc;
  start_object (ctx, &reloc, sizeof (reloc));
  reloc = destructure_emacs_reloc (ctx, lreloc);
  finish_object (ctx, &reloc, sizeof (reloc));
}

/* Combine copied relocations if they're copied from adjacent chunks.  */
static Lisp_Object
merge_emacs_relocs (struct dump_context *ctx, Lisp_Object lreloc_a, Lisp_Object lreloc_b)
{
#ifdef ENABLE_CHECKING
  {
    dump_off off_a = INTEGER_TO_INT (XCAR (XCDR (lreloc_a)));
    dump_off off_b = INTEGER_TO_INT (XCAR (XCDR (lreloc_b)));
    eassert (off_a <= off_b);  /* Catch sort errors.  */
  }
#endif

  if (XFIXNUM (XCAR (lreloc_a)) != RELOC_COPY_FROM_DUMP
      || XFIXNUM (XCAR (lreloc_b)) != RELOC_COPY_FROM_DUMP)
    return Qnil;

  struct emacs_reloc reloc_a = destructure_emacs_reloc (ctx, lreloc_a);
  struct emacs_reloc reloc_b = destructure_emacs_reloc (ctx, lreloc_b);

  eassert (reloc_a.type == RELOC_COPY_FROM_DUMP);
  eassert (reloc_b.type == RELOC_COPY_FROM_DUMP);

  if (reloc_a.offset + reloc_a.length != reloc_b.offset
      || reloc_a.ptr.offset + reloc_a.length != reloc_b.ptr.offset)
    return Qnil;

  dump_off new_length = reloc_a.length + reloc_b.length;
  reloc_a.length = new_length;
  if (reloc_a.length != new_length)
    return Qnil; /* Overflow */

  return list4 (make_fixnum (RELOC_COPY_FROM_DUMP),
                INT_TO_INTEGER (reloc_a.offset),
                INT_TO_INTEGER (reloc_a.ptr.offset),
                INT_TO_INTEGER (reloc_a.length));
}

typedef void (*drain_reloc_handler) (struct dump_context *, Lisp_Object);
typedef Lisp_Object (*drain_reloc_merger) (struct dump_context *, Lisp_Object a, Lisp_Object b);

static void
drain_reloc_list (struct dump_context *ctx,
                  drain_reloc_handler handler,
                  drain_reloc_merger merger,
                  Lisp_Object *reloc_list,
                  struct dump_locator *out_locator)
{
  struct dump_flags restore_flags = ctx->flags;
  ctx->flags.pack_objects = true;
  Lisp_Object relocs = CALLN (Fsort, Fnreverse (*reloc_list),
                              Qdump_emacs_portable__sort_predicate);
  *reloc_list = Qnil;
  align_output (ctx, max (alignof (struct dump_reloc),
			  alignof (struct emacs_reloc)));
  struct dump_locator locator = {0};
  locator.offset = ctx->offset;
  while (!NILP (relocs))
    {
      Lisp_Object merged = pop (&relocs);
      while (!NILP (relocs))
	{
	  Lisp_Object merged_maybe = merger (ctx, merged, XCAR (relocs));
	  if (!NILP (merged_maybe))
	    {
	      merged = merged_maybe;
	      pop (&relocs);
	    }
	  else
	    break;
	}
      handler (ctx, merged);
      ++locator.nr_entries;
    }
  *out_locator = locator;
  ctx->flags = restore_flags;
}

static void
fixup (struct dump_context *ctx)
{
  dump_off saved_offset = ctx->offset;
  Lisp_Object fixups = CALLN (Fsort, Fnreverse (ctx->fixups),
                              Qdump_emacs_portable__sort_predicate);
  ctx->fixups = Qnil;
  for (Lisp_Object fixup
#ifdef ENABLE_CHECKING
, prev_fixup = Qnil
#endif
	 ;
       !NILP (fixups);
#ifdef ENABLE_CHECKING
       prev_fixup = fixup
#endif
)
    {
      fixup = pop (&fixups);
      enum dump_fixup_type type = (enum dump_fixup_type) XFIXNUM (pop (&fixup));
      dump_off dump_fixup_offset = INTEGER_TO_INT (pop (&fixup));
#ifdef ENABLE_CHECKING
      if (!NILP (prev_fixup))
	{
	  dump_off prev_dump_fixup_offset = INTEGER_TO_INT (XCAR (XCDR (prev_fixup)));
	  eassert (dump_fixup_offset - prev_dump_fixup_offset >= sizeof (void *));
	}
#endif
      Lisp_Object arg = pop (&fixup);
      eassert (NILP (fixup));
      seek (ctx, dump_fixup_offset);
      switch (type)
	{
	case FIXUP_LISP_OBJECT:
	  /* Dump wants a Lisp_Object */
	  if (SUBRP (arg) && !NATIVE_COMP_FUNCTIONP (arg))
	    {
	      intptr_t dump_value = emacs_offset (XSUBR (arg));
	      reloc_emacs_lv (ctx, ctx->offset, XTYPE (arg));
	      write_bytes (ctx, &dump_value, sizeof (dump_value));
	    }
	  else if (builtin_symbol_p (arg))
	    {
	      eassert (self_representing_p (arg));
	      write_bytes (ctx, &arg, sizeof (arg));
	    }
	  else
	    {
	      eassert (emacs_ptr (arg) == NULL);
	      intptr_t dump_value = recall_object (ctx, arg);
	      if (dump_value <= 0)
		error ("fixup object not dumped");
	      reloc_dump_lv (ctx, ctx->offset, XTYPE (arg));
	      write_bytes (ctx, &dump_value, sizeof (dump_value));
	    }
	  break;
	case FIXUP_LISP_OBJECT_RAW:
	  /* Dump wants a C pointer */
	  if (SUBRP (arg) && !NATIVE_COMP_FUNCTIONP (arg))
	    {
	      intptr_t dump_value = emacs_offset (XSUBR (arg));
	      reloc_emacs_ptr (ctx, ctx->offset);
	      write_bytes (ctx, &dump_value, sizeof (dump_value));
	    }
	  else if (builtin_symbol_p (arg))
	    {
	      eassert (self_representing_p (arg));
	      intptr_t dump_value = emacs_offset (XSYMBOL (arg));
	      reloc_emacs_ptr (ctx, ctx->offset);
	      write_bytes (ctx, &dump_value, sizeof (dump_value));
	    }
	  else
	    {
	      eassert (emacs_ptr (arg) == NULL);
	      intptr_t dump_value = recall_object (ctx, arg);
	      if (dump_value <= 0)
		error ("fixup object not dumped");
	      reloc_dump_ptr (ctx, ctx->offset);
	      write_bytes (ctx, &dump_value, sizeof (dump_value));
	    }
	  break;
	case FIXUP_PTR_DUMP_RAW:
	  /* Dump knows the exact location it wants, so just
	     believe it.  */
	  {
	    intptr_t dump_value = INTEGER_TO_INT (arg);
	    reloc_dump_ptr (ctx, ctx->offset);
	    write_bytes (ctx, &dump_value, sizeof (dump_value));
	  }
	  break;
	case FIXUP_BIGNUM_DATA:
	  {
	    eassert (BIGNUMP (arg));
	    arg = Fgethash (arg, ctx->bignum_data, Qnil);
	    if (NILP (arg))
	      error ("bignum not dumped");
	    struct bignum_reload_info reload_info = { 0 };
	    reload_info.data_location = INTEGER_TO_INT (pop (&arg));
	    reload_info.nlimbs = INTEGER_TO_INT (pop (&arg));
	    eassert (NILP (arg));
	    write_bytes (ctx, &reload_info, sizeof (reload_info));
	    break;
	  }
	default:
	  emacs_abort ();
	  break;
	}
    }
  seek (ctx, saved_offset);
}

static void
drain_normal_queue (struct dump_context *ctx)
{
  while (!queue_empty_p (&ctx->queue))
    dump_object (ctx, queue_dequeue (&ctx->queue, ctx->offset));
}

static void
drain_deferred_hash_tables (struct dump_context *ctx)
{
  struct dump_flags old_flags = ctx->flags;

  /* Now we want to actually write the hash tables.  */
  ctx->flags.defer_hash_tables = false;

  Lisp_Object deferred_hash_tables = Fnreverse (ctx->deferred_hash_tables);
  ctx->deferred_hash_tables = Qnil;
  while (!NILP (deferred_hash_tables))
    dump_object (ctx, pop (&deferred_hash_tables));
  ctx->flags = old_flags;
}

static void
drain_deferred_symbols (struct dump_context *ctx)
{
  struct dump_flags old_flags = ctx->flags;

  /* Now we want to actually write the symbols.  */
  ctx->flags.defer_symbols = false;

  Lisp_Object deferred_symbols = Fnreverse (ctx->deferred_symbols);
  ctx->deferred_symbols = Qnil;
  while (!NILP (deferred_symbols))
    dump_object (ctx, pop (&deferred_symbols));
  ctx->flags = old_flags;
}

DEFUN ("dump-emacs-portable",
       Fdump_emacs_portable, Sdump_emacs_portable,
       1, 1, 0,
       doc: /* Dump current state of Emacs into dump file FILENAME.  */)
  (Lisp_Object filename)
{
  eassert (initialized);
  if (!noninteractive)
    error ("dump-emacs-portable is a batch operation.");

  /* Clear detritus in memory.  */
  while (garbage_collect ()); /* while a finalizer was run */

  specpdl_ref count = SPECPDL_INDEX ();

  Lisp_Object symbol = intern ("command-line-processed");
  specbind (symbol, Qnil); /* Dump-borne emacs hasn't processed yet.  */

  CHECK_STRING (filename);
  filename = ENCODE_FILE (Fexpand_file_name (filename, Qnil));

  struct dump_context ctx_buf = {0};
  struct dump_context *ctx = &ctx_buf;
  ctx->fd = -1;

  ctx->objects_dumped = make_eq_hash_table ();
  queue_init (&ctx->queue);
  ctx->deferred_hash_tables = Qnil;
  ctx->deferred_symbols = Qnil;

  ctx->fixups = Qnil;
  ctx->staticpro_table = Fmake_hash_table (0, NULL);
  ctx->symbol_aux = Qnil;
  ctx->symbol_cvar = Qnil;
  ctx->copied_queue = Qnil;
  ctx->cold_queue = Qnil;
  for (int i = 0; i < RELOC_NUM_PHASES; ++i)
    ctx->dump_relocs[i] = Qnil;
  ctx->object_starts = Qnil;
  ctx->emacs_relocs = Qnil;
  ctx->bignum_data = make_eq_hash_table ();

  /* Ordinarily, dump_object should remember where it saw objects and
     actually write the object contents to the dump file.  In special
     circumstances below, we temporarily change this default
     behavior.  */
  ctx->flags.dump_object_contents = true;
  ctx->flags.record_object_starts = true;

  /* We want to consolidate certain object types that we know are very likely
     to be modified.  */
  ctx->flags.defer_hash_tables = true;
  /* ctx->flags.defer_symbols = true; XXX  */

  /* These objects go into special sections.  */
  ctx->flags.defer_cold_objects = true;
  ctx->flags.defer_copied_objects = true;

  ctx->filename = filename;

  record_unwind_protect_ptr (unwind_cleanup, ctx);

#ifdef REL_ALLOC
  r_alloc_inhibit_buffer_relocation (1);
  ctx->blocked_ralloc = true;
#endif

  /* Reduce chaos.  */
  block_input ();
  ctx->restore_pure_pool = Vpdumper__pure_pool;
  Vpdumper__pure_pool = Qnil;
  ctx->restore_post_gc_hook = Vpost_gc_hook;
  Vpost_gc_hook = Qnil;
  ctx->restore_process_environment = Vprocess_environment;
  Vprocess_environment = Qnil;

  ctx->fd = emacs_open (SSDATA (filename),
                        O_RDWR | O_TRUNC | O_CREAT, 0666);
  if (ctx->fd < 0)
    report_file_error ("Opening dump output", filename);
  verify (sizeof (ctx->header.magic) == sizeof (dump_magic));
  memcpy (&ctx->header.magic, dump_magic, sizeof (dump_magic));
  ctx->header.magic[0] = '!'; /* Sophomoric hack indicates in-process */

  verify (sizeof (fingerprint) == sizeof (ctx->header.fingerprint));
  for (int i = 0; i < sizeof fingerprint; ++i)
    ctx->header.fingerprint[i] = fingerprint[i];

  const dump_off header_start = ctx->offset;
  pdumper_fingerprint (stderr, "Dumping fingerprint", ctx->header.fingerprint);
  write_bytes (ctx, &ctx->header, sizeof (ctx->header));
  const dump_off header_end = ctx->offset;
  const dump_off hot_start = header_end;

  dump_roots (ctx);
  dump_charset_table (ctx);
  dump_finalizer_list_head_ptr (ctx, &finalizers.prev);
  dump_finalizer_list_head_ptr (ctx, &finalizers.next);
  dump_finalizer_list_head_ptr (ctx, &doomed_finalizers.prev);
  dump_finalizer_list_head_ptr (ctx, &doomed_finalizers.next);
  drain_user_remembered_data_hot (ctx);

  /* We've already remembered all the objects to which GC roots point,
     but we have to manually save the list of GC roots itself.  */
  dump_metadata_for_pdumper (ctx);
  for (int i = 0; i < staticidx; ++i)
    reloc_to_emacs_ptr (ctx, &staticvec[i], staticvec[i]);
  reloc_immediate_int (ctx, &staticidx, staticidx);

  /* Dump until while we keep finding objects to dump.  We add new
     objects to the queue by side effect during dumping.
     We accumulate some types of objects in special lists to get more
     locality for these object types at runtime.  */
  do
    {
      drain_deferred_hash_tables (ctx);
      drain_deferred_symbols (ctx);
      drain_normal_queue (ctx);
    }
  while (!queue_empty_p (&ctx->queue)
	 || !NILP (ctx->deferred_hash_tables)
	 || !NILP (ctx->deferred_symbols));

  ctx->header.hash_list = ctx->offset;
  dump_hash_table_list (ctx);

  /* dump_hash_table_list just adds a new vector to the dump but all
     its content should already have been in the dump, so it doesn't
     add anything to any queue.  */
  eassert (queue_empty_p (&ctx->queue)
	   && NILP (ctx->deferred_hash_tables)
	   && NILP (ctx->deferred_symbols));

  dump_sort_copied_objects (ctx);

  /* While we copy built-in symbols into the Emacs image, these
     built-in structures refer to non-Lisp heap objects that must live
     in the dump; we stick these auxiliary data structures at the end
     of the hot section and use a special hash table to remember them.
     The actual symbol dump will pick them up below.  */
  ctx->symbol_aux = make_eq_hash_table ();
  ctx->symbol_cvar = make_eq_hash_table ();

  dump_hot_parts_of_discardable_objects (ctx);

  /* We don't need to align hot_end: pdumper_load() starts discarding at
     the next page boundary.  */
  const dump_off hot_end = ctx->offset;

  /* The discardable section is memcpy()ed into the emacs data
     segment upon load (and discarded afterwards).  */
  ctx->header.discardable_start = hot_end;
  drain_copied_objects (ctx);
  eassert (queue_empty_p (&ctx->queue));

  const dump_off discardable_end = ctx->offset;
  align_output (ctx, MAX_PAGE_SIZE);
  ctx->header.cold_start = ctx->offset;

  /* The immutable cold section is direct-mapped upon load.  */
  drain_cold_data (ctx);

  /* All lisp objects need to be dumped before this.  */
  drain_user_remembered_data_cold (ctx);

  /* That's it for Lisp data in the heap.  */
  ctx->end_heap = ctx->offset;

  /* Make promised modifications.  */
  fixup (ctx);

  /* Emit relocations in the cold section.  */
  for (int i = 0; i < RELOC_NUM_PHASES; ++i)
    drain_reloc_list (ctx, emit_dump_reloc, merge_dump_relocs,
		      &ctx->dump_relocs[i], &ctx->header.dump_relocs[i]);
  dump_off nr_hot_relocations = ctx->nr_hot_relocations;
  ctx->nr_hot_relocations = 0;
  dump_off nr_discardable_relocations = ctx->nr_discardable_relocations;
  ctx->nr_discardable_relocations = 0;
  drain_reloc_list (ctx, emit_dump_reloc, merge_dump_relocs,
		    &ctx->object_starts, &ctx->header.object_starts);
  drain_reloc_list (ctx, emit_emacs_reloc, merge_emacs_relocs,
		    &ctx->emacs_relocs, &ctx->header.emacs_relocs);

  const dump_off cold_end = ctx->offset;

  eassert (queue_empty_p (&ctx->queue));
  eassert (NILP (ctx->copied_queue));
  eassert (NILP (ctx->cold_queue));
  eassert (NILP (ctx->deferred_symbols));
  eassert (NILP (ctx->deferred_hash_tables));
  eassert (NILP (ctx->fixups));
  for (int i = 0; i < RELOC_NUM_PHASES; ++i)
    eassert (NILP (ctx->dump_relocs[i]));
  eassert (NILP (ctx->emacs_relocs));

  ctx->header.magic[0] = dump_magic[0]; /* Sophomoric hack indicates done */
  seek (ctx, 0);
  write_bytes (ctx, &ctx->header, sizeof (ctx->header));
  if (emacs_write (ctx->fd, ctx->buf, ctx->max_offset) < ctx->max_offset)
    report_file_error ("Could not write to dump file", ctx->filename);
  xfree (ctx->buf);
  ctx->buf = NULL;
  ctx->buf_size = 0;
  ctx->max_offset = 0;

  dump_off header_bytes = header_end - header_start,
    hot_bytes = hot_end - hot_start,
    discardable_bytes = discardable_end - ctx->header.discardable_start,
    cold_bytes = cold_end - ctx->header.cold_start;
  fprintf (stderr,
	   ("Dump complete\n"
	    "Byte counts: header=%"PRIdDUMP_OFF" hot=%"PRIdDUMP_OFF
	    " discardable=%"PRIdDUMP_OFF" cold=%"PRIdDUMP_OFF"\n"
	    "Reloc counts: hot=%"PRIdDUMP_OFF" discardable=%"PRIdDUMP_OFF"\n"),
	   header_bytes, hot_bytes, discardable_bytes, cold_bytes,
           nr_hot_relocations,
           nr_discardable_relocations);

  unblock_input ();
  return unbind_to (count, Qnil);
}

DEFUN ("dump-emacs-portable--sort-predicate",
       Fdump_emacs_portable__sort_predicate,
       Sdump_emacs_portable__sort_predicate,
       2, 2, 0,
       doc: /* Internal relocation sorting function.  */)
     (Lisp_Object a, Lisp_Object b)
{
  dump_off a_offset = INTEGER_TO_INT (XCAR (XCDR (a)));
  dump_off b_offset = INTEGER_TO_INT (XCAR (XCDR (b)));
  return a_offset < b_offset ? Qt : Qnil;
}

DEFUN ("dump-emacs-portable--sort-predicate-copied",
       Fdump_emacs_portable__sort_predicate_copied,
       Sdump_emacs_portable__sort_predicate_copied,
       2, 2, 0,
       doc: /* Internal relocation sorting function.  */)
     (Lisp_Object a, Lisp_Object b)
{
  eassert (emacs_ptr (a));
  eassert (emacs_ptr (b));
  return emacs_ptr (a) < emacs_ptr (b) ? Qt : Qnil;
}

void
pdumper_do_now_and_after_load (pdumper_hook hook)
{
  if (nr_dump_hooks == ARRAYELTS (dump_hooks))
    fatal ("out of dump hooks: make dump_hooks[] bigger");
  dump_hooks[nr_dump_hooks++] = hook;
  hook ();
}

static void
pdumper_remember_user_data (void *mem, int nbytes)
{
  if (nr_remembered_data >= ARRAYELTS (remembered_data))
    fatal ("make remembered_data[] bigger");
  remembered_data[nr_remembered_data].mem = mem;
  remembered_data[nr_remembered_data].sz = nbytes;
  ++nr_remembered_data;
}

void
pdumper_remember_scalar (void *mem, ptrdiff_t nbytes)
{
  eassert (0 <= nbytes && nbytes <= INT_MAX);
  if (nbytes > 0)
    pdumper_remember_user_data (mem, (int) nbytes);
}

/* Remember the pointer at *PTR.  *PTR must be null or point to a Lisp
   object.  TYPE is the rough type of Lisp object to which *PTR
   points.  */
void
pdumper_remember (void *ptr, enum Lisp_Type type)
{
  pdumper_remember_user_data (ptr, -type);
}

void
pdumper_fingerprint (FILE *output, char const *label,
		     unsigned char const xfingerprint[sizeof fingerprint])
{
  enum { hexbuf_size = 2 * sizeof fingerprint };
  char hexbuf[hexbuf_size];
  hexbuf_digest (hexbuf, xfingerprint, sizeof fingerprint);
  fprintf (output, "%s%s%.*s\n", label, *label ? ": " : "",
	   hexbuf_size, hexbuf);
}

/* Return whether the XPNTR is in the cold section of the dump.
   Only bool-vectors and floats should end up there.  */
bool
pdumper_cold_p (const void *xpntr)
{
  bool ret = false;
  if (pdumper_address_p (xpntr))
    {
      dump_off offset = DUMP_OFF ((uintptr_t) xpntr - pdumper_info.addr_beg);
      ret = (offset >= pdumper_info.header.cold_start);
    }
  return ret;
}

/* Return dump entry corresponding to ADDR.  */
const struct dump_start *
pdumper_xpntr_start (const void *addr)
{
  const struct dump_start *ret = NULL;
  if (pdumper_address_p (addr))
    {
      const dump_off offset = DUMP_OFF ((uintptr_t) addr - pdumper_info.addr_beg);
      if (offset % DUMP_ALIGNMENT == 0)
	{
	  struct dump_start *starts
	    = (struct dump_start *) ((char *) pdumper_info.addr_beg
				     + pdumper_info.header.object_starts.offset);
	  ptrdiff_t i_left = 0;
	  ptrdiff_t i_right = pdumper_info.header.object_starts.nr_entries;
	  while (i_left < i_right) /* binary search */
	    {
	      const ptrdiff_t i_mid = i_left + (i_right - i_left) / 2;
	      const struct dump_start *mid = &starts[i_mid];
	      if (offset > mid->offset)
		i_left = i_mid + 1;
	      else
		{
		  ret = mid;
		  i_right = i_mid;
		  if (i_right <= i_left
		      || offset > starts[i_right - 1].offset)
		    break;
		}
	    }
	  if (ret && ret->offset != offset)
	    ret = NULL;
	}
    }
  return ret;
}

/* Return whether OBJ is marked according to the portable dumper.  */
bool
pdumper_marked_p (const void *xpntr)
{
  bool ret = false;
  if (pdumper_address_p (xpntr))
    {
      ptrdiff_t offset = (uintptr_t) xpntr - pdumper_info.addr_beg;
      eassert (offset % DUMP_ALIGNMENT == 0);
      eassert (offset < pdumper_info.header.cold_start);
      eassert (offset < pdumper_info.header.discardable_start);
      ptrdiff_t bitno = offset / DUMP_ALIGNMENT;
      ret = bitset_test (pdumper_info.mark_bits, (bitset_bindex) bitno);
    }
  return ret;
}

void
pdumper_set_marked (const void *xpntr)
{
  eassert (pdumper_address_p (xpntr));
  ptrdiff_t offset = (uintptr_t) xpntr - pdumper_info.addr_beg;
  eassert (offset % DUMP_ALIGNMENT == 0);
  eassert (offset < pdumper_info.header.cold_start);
  eassert (offset < pdumper_info.header.discardable_start);
  ptrdiff_t bitno = offset / DUMP_ALIGNMENT;
  bitset_set (pdumper_info.mark_bits, (bitset_bindex) bitno);
}

void
pdumper_clear_marks (void)
{
  bitset_zero (pdumper_info.mark_bits);
}

ssize_t
read_bytes (int fd, void *buf, size_t bytes_to_read)
{
  /* Cannot use emacs_read since no lisp yet.  */
  size_t bytes_read = 0;
  while (bytes_read < bytes_to_read)
    {
      /* Some platforms accept only int-sized values to read.
         Round this down to a page size (see MAX_RW_COUNT in sysdep.c).  */
      int max_rw_count = INT_MAX >> 18 << 18;
      int chunk_to_read = min (bytes_to_read - bytes_read, max_rw_count);
      ssize_t chunk = read (fd, (char *) buf + bytes_read, chunk_to_read);
      if (chunk < 0)
	{
	  bytes_read = chunk;
	  break;
	}
      else if (chunk == 0)
        break;
      bytes_read += chunk;
    }

  return bytes_read;
}

/* Inverse of `dump-emacs-portable'.

   N.B. We run very early in initialization, so we can't use lisp,
   unwinding, and so on.  */
/* Prepend the Emacs startup directory to filename, if that is
   relative, so that we could later make it absolute correctly.  */
void
pdumper_record_wd (const char *wd)
{
  if (wd && !file_name_absolute_p (pdumper_info.filename))
    {
      char *dfn = xmalloc (strlen (wd) + 1
			   + strlen (pdumper_info.filename) + 1);
      splice_dir_file (dfn, wd, pdumper_info.filename);
      xfree (pdumper_info.filename);
      pdumper_info.filename = dfn;
    }
}

DEFUN ("pdumper-stats", Fpdumper_stats, Spdumper_stats, 0, 0, 0,
       doc: /* Return statistics about portable dumping used by this session.
If this Emacs session was started from a dump file,
the return value is an alist of the form:

  ((dumped-with-pdumper . t) (load-time . TIME) (pdump-file-name . FILE))

where TIME is the time in seconds it took to restore Emacs state
from the dump file, and FILE is the name of the dump file.
Value is nil if this session was not started using a dump file.*/)
     (void)
{
  if (!was_dumped_p ())
    return Qnil;

  Lisp_Object dump_fn;
#ifdef WINDOWSNT
  char dump_fn_utf8[MAX_UTF8_PATH];
  if (filename_from_ansi (pdumper_info.filename, dump_fn_utf8) == 0)
    dump_fn = DECODE_FILE (build_unibyte_string (dump_fn_utf8));
  else
    dump_fn = build_unibyte_string (pdumper_info.filename);
#else
  dump_fn = DECODE_FILE (build_unibyte_string (pdumper_info.filename));
#endif

  dump_fn = Fexpand_file_name (dump_fn, Qnil);

  return list3 (Fcons (Qdumped_with_pdumper, Qt),
		Fcons (Qload_time, make_float (pdumper_info.load_time)),
		Fcons (Qdump_file_name, dump_fn));
}

void
syms_of_pdumper (void)
{
  DEFVAR_LISP ("pdumper--pure-pool", Vpdumper__pure_pool,
	       doc: /* Singularizes objects "purified" during pdump.
As a half-measure towards reducing the pdumped image size, Monnier
arbitrarily chooses certain lisp objects to become singletons in
purespace.  */);
  Vpdumper__pure_pool = Qnil;
  defsubr (&Sdump_emacs_portable);
  defsubr (&Sdump_emacs_portable__sort_predicate);
  defsubr (&Sdump_emacs_portable__sort_predicate_copied);
  DEFSYM (Qdump_emacs_portable__sort_predicate,
          "dump-emacs-portable--sort-predicate");
  DEFSYM (Qdump_emacs_portable__sort_predicate_copied,
          "dump-emacs-portable--sort-predicate-copied");
  DEFSYM (Qdumped_with_pdumper, "dumped-with-pdumper");
  DEFSYM (Qload_time, "load-time");
  DEFSYM (Qdump_file_name, "pdump-file-name");
  DEFSYM (Qafter_pdump_load_hook, "after-pdump-load-hook");
  defsubr (&Spdumper_stats);
}
