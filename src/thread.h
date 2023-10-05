/* Thread definitions
Copyright (C) 2012-2023 Free Software Foundation, Inc.

This file is NOT part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef THREAD_H
#define THREAD_H

#include "regex-emacs.h"

#ifdef WINDOWSNT
#include <sys/socket.h>
#endif

#ifdef MSDOS
#include <time.h>               /* struct rpl_timespec */
#include <signal.h>		/* sigset_t */
#endif

#include "sysselect.h"
#include "systhread.h"

/* Yield an address close enough to the top of the stack that the
   garbage collector need not scan above it.  Callers should be
   declared NO_INLINE.  */
#ifdef HAVE___BUILTIN_FRAME_ADDRESS
# define NEAR_STACK_TOP(addr) ((void) (addr), __builtin_frame_address (0))
#else
# define NEAR_STACK_TOP(addr) (addr)
#endif

INLINE_HEADER_BEGIN

/* Byte-code interpreter thread state.  */
struct bc_thread_state {
  struct bc_frame *fp;
  char *stack;
  char *stack_end;
};

struct ablock;
struct thread_state
{
  union vectorlike_header header;

  /* Buffer where last search was performed, or Qt if done in a
     string, or Qnil if no last search.  */
  Lisp_Object m_last_thing_searched;
#define last_thing_searched (current_thread->m_last_thing_searched)

  Lisp_Object name;
  Lisp_Object function;

  /* Thread-local let-bindings.  Leave global Vobarray alone.  */
  Lisp_Object obarray;

  /* Populated when FUNCTION finished.  */
  Lisp_Object result;

  /* If non-nil, thread has been signaled.  */
  Lisp_Object error_symbol;
  Lisp_Object error_data;

  /* Mutex or condvar waited for.  */
  Lisp_Object event_object;

  /* !!! Adjust ALLOCATE_ZEROED_PSEUDOVECTOR for new Lisp fields.  */

  char const *m_stack_bottom;
#define stack_bottom (current_thread->m_stack_bottom)

  void const *stack_top;

  struct catchtag *m_catchlist;
#define catchlist (current_thread->m_catchlist)

  /* Handlers pushed by Fcondition_case and internal_condition_case.  */
  struct handler *m_handlerlist;
#define handlerlist (current_thread->m_handlerlist)

  struct handler *m_handlerlist_sentinel;
#define handlerlist_sentinel (current_thread->m_handlerlist_sentinel)

  /* Bottom of specpdl, not to be confused with m_stack_bottom which
     references the C stack.  */
  union specbinding *m_specpdl;
#define specpdl (current_thread->m_specpdl)

  /* Sentinel just beyond specpdl capacity for grow_specpdl().  */
  union specbinding *m_specpdl_end;
#define specpdl_end (current_thread->m_specpdl_end)

  /* Top of specpdl.  */
  union specbinding *m_specpdl_ptr;
#define specpdl_ptr (current_thread->m_specpdl_ptr)

  intmax_t m_lisp_eval_depth;
#define lisp_eval_depth (current_thread->m_lisp_eval_depth)

  struct buffer *m_current_buffer;
#define current_buffer (current_thread->m_current_buffer)

  /* Every call to re_search, etc., must pass &search_regs as the regs
     argument unless you can show it is unnecessary (i.e., if re_search
     is certainly going to be called again before region-around-match
     can be called).

     Since the registers are now dynamically allocated, we need to make
     sure not to refer to the Nth register before checking that it has
     been allocated by checking search_regs.num_regs.

     The regex code keeps track of whether it has allocated the search
     buffer using bits in the re_pattern_buffer.  This means that whenever
     you compile a new pattern, it completely forgets whether it has
     allocated any registers, and will allocate new registers the next
     time you call a searching or matching function.  Therefore, we need
     to call re_set_registers after compiling a new pattern or after
     setting the match registers, so that the regex functions will be
     able to free or re-allocate it properly.  */
  struct re_registers m_search_regs;
#define search_regs (current_thread->m_search_regs)

  /* For longjmp to where kbd input is being done.  This is per-thread
     so that if more than one thread calls read_char, they don't
     clobber each other's getcjmp, which will cause
     quit_throw_to_read_char crash due to using a wrong stack.  */
  sys_jmp_buf m_getcjmp;
#define getcjmp (current_thread->m_getcjmp)

  /* The red-black tree for finding the memory block of a Lisp_Object.  */
  struct mem_node *m_mem_root;

  struct ablock *m_free_ablocks;

  struct interval_block *m_interval_blocks;

  int m_interval_block_index;

  INTERVAL m_interval_free_list;

  struct sblock *m_oldest_sblock;

  struct sblock *m_current_sblock;

  struct sblock *m_large_sblocks;

  struct string_block *m_string_blocks;

  struct Lisp_String *m_string_free_list;

  struct float_block *m_float_blocks;

  int m_float_block_index;

  struct Lisp_Float *m_float_free_list;

  struct cons_block *m_cons_blocks;

  int m_cons_block_index;

  struct Lisp_Cons *m_cons_free_list;

  struct vector_block *m_vector_blocks;

  /* See free_slot() for rationale.  */
  struct Lisp_Vector **m_vector_free_lists;

  /* The last free list where we found large enough vector.  Trade
     fragmentation risk for speed by commencing here on subsequent
     searches.  */
  ptrdiff_t m_most_recent_free_slot;

  /* Singly-linked list of large vectors.  */
  struct large_vector *m_large_vectors;

  struct symbol_block *m_symbol_blocks;

  int m_symbol_block_index;

  struct Lisp_Symbol *m_symbol_free_list;

  /* The OS identifier for this thread.  */
  sys_thread_t thread_id;

  /* The condition variable for this thread.  This is associated with
     the global lock.  This thread broadcasts to it when it exits.  */
  sys_cond_t thread_condvar;

  /* This thread might be waiting for some condition.  If so, this
     points to the condition.  If the thread is interrupted, the
     interrupter should broadcast to this condition.  */
  sys_cond_t *wait_condvar;

  /* Thread's name in the locale encoding.  */
  char *thread_name;

  /* Run under auspices of global lock.  */
  bool cooperative;

  /* Threads are kept on a linked list.  */
  struct thread_state *next_thread;

  struct bc_thread_state bc;
} GCALIGNED_STRUCT;

INLINE bool
THREADP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_THREAD);
}

INLINE void
CHECK_THREAD (Lisp_Object x)
{
  CHECK_TYPE (THREADP (x), Qthreadp, x);
}

INLINE struct thread_state *
XTHREAD (Lisp_Object a)
{
  eassert (THREADP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct thread_state);
}

/* The guts of Lisp_Mutex is a lower-level lisp_mutex_t, which is
   a rich man's sys_mutex_t.  See thread.c header comment.  */
typedef struct {
  /* The owning thread, or NULL if unlocked.  */
  struct thread_state *owner;
  /* The lock count.  */
  unsigned int count;
  /* The underlying system condition variable.  */
  sys_cond_t condition;
} lisp_mutex_t;

struct Lisp_Mutex
{
  union vectorlike_header header;
  Lisp_Object name;
  lisp_mutex_t mutex;
} GCALIGNED_STRUCT;

INLINE bool
MUTEXP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_MUTEX);
}

INLINE void
CHECK_MUTEX (Lisp_Object x)
{
  CHECK_TYPE (MUTEXP (x), Qmutexp, x);
}

INLINE struct Lisp_Mutex *
XMUTEX (Lisp_Object a)
{
  eassert (MUTEXP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_Mutex);
}

struct Lisp_CondVar
{
  union vectorlike_header header;
  Lisp_Object mutex;
  Lisp_Object name;
  sys_cond_t cond;
} GCALIGNED_STRUCT;

INLINE bool
CONDVARP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_CONDVAR);
}

INLINE void
CHECK_CONDVAR (Lisp_Object x)
{
  CHECK_TYPE (CONDVARP (x), Qcondition_variable_p, x);
}

INLINE struct Lisp_CondVar *
XCONDVAR (Lisp_Object a)
{
  eassert (CONDVARP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_CondVar);
}

#ifdef HAVE_GCC_TLS
# define PER_THREAD __thread
# define PER_THREAD_STATIC __thread
#else
# define PER_THREAD /* empty */
# define PER_THREAD_STATIC static
#endif

extern PER_THREAD struct thread_state *current_thread;
extern struct thread_state *const main_thread;

extern void finalize_one_thread (struct thread_state *state);
extern void finalize_one_mutex (struct Lisp_Mutex *);
extern void finalize_one_condvar (struct Lisp_CondVar *);

extern void init_threads (void);
extern void syms_of_threads (void);
extern bool main_thread_p (const void *);

typedef int select_func (int, fd_set *, fd_set *, fd_set *,
			 const struct timespec *, const sigset_t *);

int thread_select  (select_func *func, int max_fds, fd_set *rfds,
		    fd_set *wfds, fd_set *efds, struct timespec *timeout,
		    sigset_t *sigmask);

void release_global_lock (void);
void acquire_global_lock (struct thread_state *);
#ifdef HAVE_GCC_TLS
extern void update_allocations_for_thread_death (struct thread_state *);
#endif

INLINE_HEADER_END

#endif /* THREAD_H */
