/* Threading code.
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

/* Unlike pthread_mutex_lock() which deadlocks if the calling thread
   already holds the sys_mutex_t, lisp_mutex_lock() merely increments
   a count of lisp_mutex_t.

   Unlike the one-dimensional sys_mutex_t, lisp_mutex_t cannot be
   considered unlocked until a sequence of lisp_mutex_unlock() calls
   decrement its count to zero.  */

#include <config.h>
#include <setjmp.h>
#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "process.h"
#include "coding.h"
#include "syssignal.h"
#include "pdumper.h"
#include "keyboard.h"

#ifdef HAVE_NS
#include "nsterm.h"
#endif

union aligned_thread_state
{
  struct thread_state s;
  GCALIGNED_UNION_MEMBER
};
verify (GCALIGNED (union aligned_thread_state));

static union aligned_thread_state main_thread
  = {{
      .header.size = PVECHEADERSIZE (PVEC_THREAD,
				     PSEUDOVECSIZE (struct thread_state,
						    event_object),
				     VECSIZE (struct thread_state)),
      .m_last_thing_searched = LISPSYM_INITIALLY (Qnil),
      .name = LISPSYM_INITIALLY (Qnil),
      .function = LISPSYM_INITIALLY (Qnil),
      .obarray = LISPSYM_INITIALLY (Qnil),
      .result = LISPSYM_INITIALLY (Qnil),
      .error_symbol = LISPSYM_INITIALLY (Qnil),
      .error_data = LISPSYM_INITIALLY (Qnil),
      .event_object = LISPSYM_INITIALLY (Qnil),
    }};

#ifdef HAVE_GCC_TLS
__thread struct thread_state *current_thread = &main_thread.s;
struct thread_state *prevailing_thread = &main_thread.s;
#else
struct thread_state *current_thread = &main_thread.s;
#endif

static struct thread_state *all_threads = &main_thread.s;

static sys_mutex_t global_lock;

extern volatile int interrupt_input_blocked;

/* m_specpdl is set when the thread is created and cleared when the
   thread dies.  */
#define thread_live_p(STATE) ((STATE)->m_specpdl != NULL)

static void
clear_thread (void *arg)
{
  struct thread_state *tstate = arg;
  tstate->event_object = Qnil;
  tstate->wait_condvar = NULL;
}

static void
restore_thread (struct thread_state *self)
{
#ifdef HAVE_GCC_TLS
  eassume (current_thread == self);
  if (self->cooperative)
#endif
    {
#ifdef HAVE_GCC_TLS
      struct thread_state *previous_thread = prevailing_thread;
      prevailing_thread = self;
#else
      struct thread_state *previous_thread = current_thread;
      current_thread = self;
#endif
      if (previous_thread != current_thread)
	{
	  /* Tromey specpdl swap under thread-at-a-time conservatism.  */
	  if (previous_thread != NULL)
	    specpdl_unwind (previous_thread->m_specpdl_ptr,
			    previous_thread->m_specpdl_ptr - previous_thread->m_specpdl,
			    SPECPDL_LET);
	  specpdl_rewind (specpdl_ptr, specpdl_ptr - specpdl, SPECPDL_LET);
	  /* Contortion here because set_buffer_internal immediately
	     returns if argument is current_buffer.  */
	  struct buffer *b = current_buffer;
	  current_buffer = NULL;
	  set_buffer_internal (b);
	}
    }

  if (! NILP (current_thread->error_symbol) && handlerlist)
    {
      Lisp_Object sym = current_thread->error_symbol,
	data = current_thread->error_data;

      current_thread->error_symbol = Qnil;
      current_thread->error_data = Qnil;

      Fsignal (sym, data);
    }
}

static void
lisp_mutex_init (lisp_mutex_t *mutex)
{
  mutex->owner = NULL;
  mutex->count = 0;
  sys_cond_init (&mutex->condition);
}

static bool
lisp_mutex_restore_lock (lisp_mutex_t *mutex, struct thread_state *locker,
			 int restore_count)
{
  eassume (locker != NULL);
  if (mutex->owner == locker)
    mutex->count = restore_count;
  else
    {
      locker->wait_condvar = &mutex->condition;
      while (mutex->owner != NULL)
	sys_cond_wait (&mutex->condition, &global_lock);
      clear_thread (locker);
      /* Tromey ignored locker->error_symbol, and so shall we. */
      mutex->owner = locker;
      mutex->count = restore_count;
      /* must be after setting owner lest Fsignal() in
	 restore_thread() derails `with-mutex' (as in
	 threads-test-condvar-wait).  */
      restore_thread (locker);
    }
  return mutex->owner == locker;
}

static bool
lisp_mutex_lock (lisp_mutex_t *mutex, struct thread_state *locker)
{
  eassume (locker != NULL);
  if (mutex->owner == locker)
    mutex->count++;
  else
    {
      locker->wait_condvar = &mutex->condition;
      while (mutex->owner != NULL && NILP (locker->error_symbol))
	sys_cond_wait (&mutex->condition, &global_lock);
      clear_thread (locker);
      restore_thread (locker);
      if (NILP (locker->error_symbol))
	{
	  mutex->owner = locker;
	  mutex->count = 1;
	}
    }
  return mutex->owner == locker;
}

/* Decrement MUTEX's lock count.  If the count becomes zero, broadcast
   to threads waiting to lock MUTEX.  */
static void
lisp_mutex_unlock (lisp_mutex_t *mutex)
{
  if (mutex->owner != current_thread)
    emacs_abort ();

  if (--mutex->count <= 0)
    {
      mutex->owner = NULL;
      sys_cond_broadcast (&mutex->condition);
    }
}

static void
lisp_mutex_destroy (lisp_mutex_t *mutex)
{
  sys_cond_destroy (&mutex->condition);
}

static int
lisp_mutex_owned_p (lisp_mutex_t *mutex)
{
  return mutex->owner == current_thread;
}

DEFUN ("make-mutex", Fmake_mutex, Smake_mutex, 0, 1, 0,
       doc: /* Create a mutex.
A mutex provides a synchronization point for threads.
Only one thread at a time can hold a mutex.  Other threads attempting
to acquire it will block until the mutex is available.

A thread can acquire a mutex any number of times.

NAME, if given, is used as the name of the mutex.  The name is
informational only.  */)
  (Lisp_Object name)
{
  if (!NILP (name))
    CHECK_STRING (name);

  struct Lisp_Mutex *mutex
    = ALLOCATE_ZEROED_PSEUDOVECTOR (struct Lisp_Mutex, name, PVEC_MUTEX);
  mutex->name = name;
  lisp_mutex_init (&mutex->mutex);

  Lisp_Object result;
  XSETMUTEX (result, mutex);
  return result;
}

static void
mutex_lock_callback (void *arg)
{
  struct Lisp_Mutex *mutex = arg;
  lisp_mutex_lock (&mutex->mutex, current_thread);
}

DEFUN ("mutex-lock", Fmutex_lock, Smutex_lock, 1, 1, 0,
       doc: /* Acquire a mutex.
If the current thread already owns MUTEX, increment the lock count and
return.  Otherwise, block until MUTEX is available, or until the
current thread is signaled using `thread-signal'.  Calls to
`mutex-lock' and `mutex-unlock' must be paired.  */)
  (Lisp_Object mutex)
{
  struct Lisp_Mutex *lmutex;
  specpdl_ref count = SPECPDL_INDEX ();

  CHECK_MUTEX (mutex);
  lmutex = XMUTEX (mutex);

  current_thread->event_object = mutex;
  record_unwind_protect_ptr (clear_thread, current_thread);
  with_flushed_stack (mutex_lock_callback, lmutex);
  return unbind_to (count, Qnil);
}

static void
mutex_unlock_callback (void *arg)
{
  struct Lisp_Mutex *mutex = arg;
  lisp_mutex_unlock (&mutex->mutex);
}

DEFUN ("mutex-unlock", Fmutex_unlock, Smutex_unlock, 1, 1, 0,
       doc: /* Release the mutex.
If this thread does not own MUTEX, signal an error.  Otherwise,
decrement the mutex's count.  If the count becomes zero, release
MUTEX.  */)
  (Lisp_Object mutex)
{
  struct Lisp_Mutex *lmutex;

  CHECK_MUTEX (mutex);
  lmutex = XMUTEX (mutex);

  with_flushed_stack (mutex_unlock_callback, lmutex);
  return Qnil;
}

DEFUN ("mutex-name", Fmutex_name, Smutex_name, 1, 1, 0,
       doc: /* Return the name of MUTEX.
If no name was given when MUTEX was created, return nil.  */)
  (Lisp_Object mutex)
{
  struct Lisp_Mutex *lmutex;

  CHECK_MUTEX (mutex);
  lmutex = XMUTEX (mutex);

  return lmutex->name;
}

void
finalize_one_mutex (struct Lisp_Mutex *mutex)
{
  lisp_mutex_destroy (&mutex->mutex);
}

DEFUN ("make-condition-variable",
       Fmake_condition_variable, Smake_condition_variable,
       1, 2, 0,
       doc: /* Make a condition variable associated with MUTEX.
A condition variable provides a way for a thread to sleep while
waiting for a state change.

MUTEX is the mutex associated with this condition variable.  NAME, if
given, is the name of this condition variable.  The name is
informational only.  */)
  (Lisp_Object mutex, Lisp_Object name)
{
  CHECK_MUTEX (mutex);
  if (!NILP (name))
    CHECK_STRING (name);

  struct Lisp_CondVar *condvar
    = ALLOCATE_ZEROED_PSEUDOVECTOR (struct Lisp_CondVar, name, PVEC_CONDVAR);
  condvar->mutex = mutex;
  condvar->name = name;
  sys_cond_init (&condvar->cond);

  Lisp_Object result;
  XSETCONDVAR (result, condvar);
  return result;
}

static void
condition_wait_callback (void *arg)
{
  struct thread_state *self = current_thread; // current_thread changes!
  struct Lisp_CondVar *cvar = arg;
  struct Lisp_Mutex *mutex = XMUTEX (cvar->mutex);
  unsigned int restore_count = mutex->mutex.count;
  Lisp_Object cond;
  XSETCONDVAR (cond, cvar);

  self->event_object = cond;
  self->wait_condvar = &cvar->cond;

  /* suspend locks and all-points bulletin.  */
  mutex->mutex.count = 0;
  mutex->mutex.owner = NULL;
  sys_cond_broadcast (&mutex->mutex.condition);

  sys_cond_wait (&cvar->cond, &global_lock);
  clear_thread (self);
  lisp_mutex_restore_lock (&mutex->mutex, self, restore_count);
}

DEFUN ("condition-wait", Fcondition_wait, Scondition_wait, 1, 1, 0,
       doc: /* Wait for the condition variable COND to be notified.
COND is the condition variable to wait on.

The mutex associated with COND must be held when this is called.  It
is an error if it is not held.

This releases the mutex and waits for COND to be notified or for this
thread to be signaled with `thread-signal'.  When `condition-wait'
returns, COND's mutex will again be locked by this thread.  */)
  (Lisp_Object cond)
{
  struct Lisp_CondVar *cvar;
  struct Lisp_Mutex *mutex;

  CHECK_CONDVAR (cond);
  cvar = XCONDVAR (cond);

  mutex = XMUTEX (cvar->mutex);
  if (! lisp_mutex_owned_p (&mutex->mutex))
    error ("Condition variable's mutex is not held by current thread");

  with_flushed_stack (condition_wait_callback, cvar);

  return Qnil;
}

/* Used to communicate arguments to condition_notify_callback.  */
struct notify_args
{
  struct Lisp_CondVar *cvar;
  int all;
};

static void
condition_notify_callback (void *arg)
{
  struct thread_state *self = current_thread; // current_thread changes!
  struct notify_args *na = arg;
  struct Lisp_Mutex *mutex = XMUTEX (na->cvar->mutex);
  unsigned int restore_count = mutex->mutex.count;

  /* suspend locks and all-points bulletin.  */
  mutex->mutex.count = 0;
  mutex->mutex.owner = NULL;
  sys_cond_broadcast (&mutex->mutex.condition);

  if (na->all)
    sys_cond_broadcast (&na->cvar->cond);
  else
    sys_cond_signal (&na->cvar->cond);
  lisp_mutex_restore_lock (&mutex->mutex, self, restore_count);
}

DEFUN ("condition-notify", Fcondition_notify, Scondition_notify, 1, 2, 0,
       doc: /* Notify COND, a condition variable.
This wakes a thread waiting on COND.
If ALL is non-nil, all waiting threads are awoken.

The mutex associated with COND must be held when this is called.  It
is an error if it is not held.

This releases COND's mutex when notifying COND.  When
`condition-notify' returns, the mutex will again be locked by this
thread.  */)
  (Lisp_Object cond, Lisp_Object all)
{
  struct Lisp_CondVar *cvar;
  struct Lisp_Mutex *mutex;
  struct notify_args args;

  CHECK_CONDVAR (cond);
  cvar = XCONDVAR (cond);

  mutex = XMUTEX (cvar->mutex);
  if (! lisp_mutex_owned_p (&mutex->mutex))
    error ("Condition variable's mutex is not held by current thread");

  args.cvar = cvar;
  args.all = ! NILP (all);
  with_flushed_stack (condition_notify_callback, &args);

  return Qnil;
}

DEFUN ("condition-mutex", Fcondition_mutex, Scondition_mutex, 1, 1, 0,
       doc: /* Return the mutex associated with condition variable COND.  */)
  (Lisp_Object cond)
{
  struct Lisp_CondVar *cvar;

  CHECK_CONDVAR (cond);
  cvar = XCONDVAR (cond);

  return cvar->mutex;
}

DEFUN ("condition-name", Fcondition_name, Scondition_name, 1, 1, 0,
       doc: /* Return the name of condition variable COND.
If no name was given when COND was created, return nil.  */)
  (Lisp_Object cond)
{
  struct Lisp_CondVar *cvar;

  CHECK_CONDVAR (cond);
  cvar = XCONDVAR (cond);

  return cvar->name;
}

void
finalize_one_condvar (struct Lisp_CondVar *condvar)
{
  sys_cond_destroy (&condvar->cond);
}

struct select_args
{
  select_func *func;
  int max_fds;
  fd_set *rfds;
  fd_set *wfds;
  fd_set *efds;
  struct timespec *timeout;
  sigset_t *sigmask;
  int result;
};

static void
internal_select (void *arg)
{
  struct select_args *sa = arg;
  struct thread_state *self = current_thread; // current_thread changes!
  sigset_t oldset;
#ifdef HAVE_GCC_TLS
  if (self->cooperative)
#endif
    {
      block_interrupt_signal (&oldset);
      release_global_lock ();
      restore_signal_mask (&oldset);
    }
  sa->result = (sa->func) (sa->max_fds, sa->rfds, sa->wfds, sa->efds,
			   sa->timeout, sa->sigmask);
#ifdef HAVE_GCC_TLS
  if (self->cooperative)
#endif
    {
      block_interrupt_signal (&oldset);
      acquire_global_lock (self);
      restore_signal_mask (&oldset);
    }
}

int
thread_select (select_func *func, int max_fds, fd_set *rfds,
	       fd_set *wfds, fd_set *efds, struct timespec *timeout,
	       sigset_t *sigmask)
{
  struct select_args sa;

  sa.func = func;
  sa.max_fds = max_fds;
  sa.rfds = rfds;
  sa.wfds = wfds;
  sa.efds = efds;
  sa.timeout = timeout;
  sa.sigmask = sigmask;
  with_flushed_stack (internal_select, &sa);
  return sa.result;
}

static void
mark_one_thread (struct thread_state *thread)
{
  /* Get the stack top now, in case mark_specpdl changes it.  */
  void const *stack_top = thread->stack_top;

  mark_specpdl (thread->m_specpdl, thread->m_specpdl_ptr);

  mark_memory (thread->m_stack_bottom, stack_top);

  for (struct handler *handler = thread->m_handlerlist;
       handler; handler = handler->next)
    {
      mark_object (&handler->tag_or_ch);
      mark_object (&handler->val);
    }

  if (thread->m_current_buffer)
    {
      Lisp_Object tem;
      XSETBUFFER (tem, thread->m_current_buffer);
      mark_object (&tem);
    }

  mark_bytecode (&thread->bc);

  /* No need to mark Lisp_Object members like m_last_thing_searched,
     as mark_threads_callback does that by calling mark_object.  */
}

static void
mark_threads_callback (void *ignore)
{
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next_thread)
    {
      Lisp_Object thread_obj;

      XSETTHREAD (thread_obj, iter);
      mark_object (&thread_obj);
      mark_one_thread (iter);
    }
}

void
mark_threads (void)
{
  with_flushed_stack (mark_threads_callback, NULL);
}

void
unmark_main_thread (void)
{
  main_thread.s.header.size &= ~ARRAY_MARK_FLAG;
}

static void
yield_callback (void *ignore)
{
  struct thread_state *self = current_thread; // current_thread changes!
#ifdef HAVE_GCC_TLS
  if (self->cooperative)
#endif
    {
      release_global_lock ();
      acquire_global_lock (self);
    }
}

void
release_global_lock (void)
{
  sys_mutex_unlock (&global_lock);
  sys_thread_yield (); // mostly no-op
}

void acquire_global_lock (struct thread_state *state)
{
  sys_mutex_lock (&global_lock);
  restore_thread (state);
}

DEFUN ("thread-yield", Fthread_yield, Sthread_yield, 0, 0, 0,
       doc: /* Yield the CPU to another thread.  */)
     (void)
{
  with_flushed_stack (yield_callback, NULL);
  return Qnil;
}

static Lisp_Object
invoke_thread (void)
{
  specpdl_ref count = SPECPDL_INDEX ();
  current_thread->result = Ffuncall (1, &current_thread->function);
  return unbind_to (count, Qnil);
}

static Lisp_Object last_thread_error;

static Lisp_Object
record_thread_error (Lisp_Object error_form)
{
  last_thread_error = error_form;
  return error_form;
}

static void *
run_thread (void *state)
{
#ifdef HAVE_NS
  /* Alan Third anticipating calls to Objective C code.  */
  void *pool = ns_alloc_autorelease_pool ();
#endif

  struct thread_state *self = state;
  union { char c; GCALIGNED_UNION_MEMBER } stack_pos;

#ifdef HAVE_GCC_TLS
  current_thread = self;
#endif

  self->m_stack_bottom = self->stack_top = &stack_pos.c;
  self->thread_id = sys_thread_self ();

  if (self->thread_name)
    sys_thread_set_name (self->thread_name);

#ifdef HAVE_GCC_TLS
  if (self->cooperative)
#endif
    acquire_global_lock (self);

  /* Put a dummy catcher at top-level so that handlerlist is never NULL.
     This is important since handlerlist->nextfree holds the freelist
     which would otherwise leak every time we unwind back to top-level.   */
  handlerlist_sentinel = xzalloc (sizeof (struct handler));
  handlerlist = handlerlist_sentinel->nextfree = handlerlist_sentinel;
  struct handler *c = push_handler (Qunbound, CATCHER);
  eassert (c == handlerlist_sentinel);
  handlerlist_sentinel->nextfree = NULL;
  handlerlist_sentinel->next = NULL;

  internal_condition_case (invoke_thread, Qt, record_thread_error);

  update_processes_for_thread_death (self);

  /* 1- for unreachable dummy entry */
  xfree (self->m_specpdl - 1);
  self->m_specpdl = NULL;
  self->m_specpdl_ptr = NULL;
  self->m_specpdl_end = NULL;

  for (struct handler *h = handlerlist_sentinel; h != NULL; )
    {
      struct handler *next = h->nextfree;
      xfree (h);
      h = next;
    }

  xfree (self->thread_name);
  sys_cond_broadcast (&self->thread_condvar);

#ifdef HAVE_NS
  ns_release_autorelease_pool (pool);
#endif

  /* Unlink SELF from all_threads.  Avoid doing this before
     sys_cond_broadcast() lest GC.  */
  for (struct thread_state **iter = &all_threads;
       *iter != NULL;
       iter = &(*iter)->next_thread)
    {
      if (*iter == self)
	{
	  *iter = (*iter)->next_thread;
	  break;
	}
    }

#ifdef HAVE_GCC_TLS
  if (self->cooperative)
#endif
    release_global_lock ();

  return NULL;
}

static void
free_search_regs (struct re_registers *regs)
{
  if (regs->num_regs != 0)
    {
      xfree (regs->start);
      xfree (regs->end);
    }
}

void
finalize_one_thread (struct thread_state *state)
{
  free_search_regs (&state->m_search_regs);
  sys_cond_destroy (&state->thread_condvar);
  free_bc_thread (&state->bc);
}

DEFUN ("make-thread", Fmake_thread, Smake_thread, 1, 3, 0,
       doc: /* Spawn thread running FUNCTION.
A non-nil NAME string is assigned to the thread.
A non-nil UNCOOPERATIVE halts and catches fire.
*/)
  (Lisp_Object function, Lisp_Object name, Lisp_Object uncooperative)
{
  Lisp_Object result;
  sys_thread_t thr;
  struct thread_state *new_thread;
  const ptrdiff_t size = 50;

  /* Can't start a thread in temacs.  */
  if (! initialized)
    emacs_abort ();

  if (! NILP (name))
    CHECK_STRING (name);

  new_thread = ALLOCATE_ZEROED_PSEUDOVECTOR (struct thread_state, event_object,
					     PVEC_THREAD);
  new_thread->name = name;
  new_thread->function = function;
  new_thread->obarray = initialize_vector (OBARRAY_SIZE / 10, make_fixnum (0));
#ifdef HAVE_GCC_TLS
  new_thread->cooperative = NILP (uncooperative);
#else
  new_thread->cooperative = true;
  if (! NILP (uncooperative))
    error ("No gcc tls support");
#endif
  new_thread->m_current_buffer = current_thread->m_current_buffer;

  /* 1+ for unreachable dummy entry */
  union specbinding *pdlvec = xmalloc ((1 + size) * sizeof (union specbinding));
  new_thread->m_specpdl = pdlvec + 1;  /* Skip the dummy entry.  */
  new_thread->m_specpdl_ptr = new_thread->m_specpdl;
  new_thread->m_specpdl_end = new_thread->m_specpdl + size;

  init_bc_thread (&new_thread->bc);

  sys_cond_init (&new_thread->thread_condvar);

  new_thread->next_thread = all_threads;
  all_threads = new_thread;

  new_thread->thread_name =
    NILP (name) ? NULL : xstrdup (SSDATA (ENCODE_SYSTEM (name)));

  if (! sys_thread_create (&thr, run_thread, new_thread))
    {
      all_threads = all_threads->next_thread; /* restore to original.  */
      error ("Could not start a new thread");
    }

  XSETTHREAD (result, new_thread);
  return result;
}

DEFUN ("current-thread", Fcurrent_thread, Scurrent_thread, 0, 0, 0,
       doc: /* Return the current thread.  */)
  (void)
{
  Lisp_Object result;
  XSETTHREAD (result, current_thread);
  return result;
}

DEFUN ("thread-name", Fthread_name, Sthread_name, 1, 1, 0,
       doc: /* Return the name of the THREAD.
The name is the same object that was passed to `make-thread'.  */)
     (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  return tstate->name;
}

static void
thread_signal_callback (void *arg)
{
  struct thread_state *tstate = arg;
  struct thread_state *self = current_thread; // current_thread changes!
  sys_cond_broadcast (tstate->wait_condvar);
  restore_thread (self);
}

DEFUN ("thread-signal", Fthread_signal, Sthread_signal, 3, 3, 0,
       doc: /* Signal an error in a thread.
This acts like `signal', but arranges for the signal to be raised
in THREAD.  If THREAD is the current thread, acts just like `signal'.
This will interrupt a blocked call to `mutex-lock', `condition-wait',
or `thread-join' in the target thread.
If THREAD is the main thread, just the error message is shown.  */)
  (Lisp_Object thread, Lisp_Object error_symbol, Lisp_Object data)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  if (tstate == current_thread)
    Fsignal (error_symbol, data);

#ifdef THREADS_ENABLED
  if (main_thread_p (tstate))
    {
      /* Construct an event.  */
      struct input_event event;
      EVENT_INIT (event);
      event.kind = THREAD_EVENT;
      event.frame_or_window = Qnil;
      event.arg = list3 (Fcurrent_thread (), error_symbol, data);

      /* Store it into the input event queue.  */
      kbd_buffer_store_event (&event);
    }
  else
#endif
    {
      /* What to do if thread is already signaled?  */
      /* What if error_symbol is Qnil?  */
      tstate->error_symbol = error_symbol;
      tstate->error_data = data;

      if (tstate->wait_condvar)
	with_flushed_stack (thread_signal_callback, tstate);
    }

  return Qnil;
}

DEFUN ("thread-live-p", Fthread_live_p, Sthread_live_p, 1, 1, 0,
       doc: /* Return t if THREAD is alive, or nil if it has exited.  */)
  (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  return thread_live_p (tstate) ? Qt : Qnil;
}

DEFUN ("thread--blocker", Fthread_blocker, Sthread_blocker, 1, 1, 0,
       doc: /* Return the object that THREAD is blocking on.
If THREAD is blocked in `thread-join' on a second thread, return that
thread.
If THREAD is blocked in `mutex-lock', return the mutex.
If THREAD is blocked in `condition-wait', return the condition variable.
Otherwise, if THREAD is not blocked, return nil.  */)
  (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  return tstate->event_object;
}

static void
thread_join_callback (void *arg)
{
  struct thread_state *tstate = arg;
  struct thread_state *self = current_thread; // current_thread changes!
  Lisp_Object thread;

  XSETTHREAD (thread, tstate);
  self->event_object = thread;
  self->wait_condvar = &tstate->thread_condvar;
  while (thread_live_p (tstate) && NILP (self->error_symbol))
    sys_cond_wait (self->wait_condvar, &global_lock);
  clear_thread (self);
  restore_thread (self);
}

DEFUN ("thread-join", Fthread_join, Sthread_join, 1, 1, 0,
       doc: /* Wait for THREAD to exit.
This blocks the current thread until THREAD exits or until the current
thread is signaled.  It returns the result of the THREAD function.  It
is an error for a thread to try to join itself.  */)
  (Lisp_Object thread)
{
  struct thread_state *tstate;
  Lisp_Object error_symbol, error_data;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  if (tstate == current_thread)
    error ("Cannot join current thread");

  error_symbol = tstate->error_symbol;
  error_data = tstate->error_data;

  if (thread_live_p (tstate))
    with_flushed_stack (thread_join_callback, tstate);

  if (! NILP (error_symbol))
    Fsignal (error_symbol, error_data);

  return tstate->result;
}

DEFUN ("all-threads", Fall_threads, Sall_threads, 0, 0, 0,
       doc: /* Return a list of all the live threads.  */)
  (void)
{
  Lisp_Object result = Qnil;
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next_thread)
    {
      if (thread_live_p (iter))
	{
	  Lisp_Object thread;

	  XSETTHREAD (thread, iter);
	  result = Fcons (thread, result);
	}
    }

  return result;
}

DEFUN ("thread-last-error", Fthread_last_error, Sthread_last_error, 0, 1, 0,
       doc: /* Return the last error form recorded by a dying thread.
If CLEANUP is non-nil, remove this error form from history.  */)
     (Lisp_Object cleanup)
{
  Lisp_Object result = last_thread_error;

  if (!NILP (cleanup))
    last_thread_error = Qnil;

  return result;
}

bool
main_thread_p (const void *ptr)
{
  return ptr == &main_thread.s;
}

void
init_threads (void)
{
  sys_cond_init (&main_thread.s.thread_condvar);
  sys_mutex_init (&global_lock);
  sys_mutex_lock (&global_lock);
  if (current_thread != &main_thread.s)
    emacs_abort ();
  main_thread.s.thread_id = sys_thread_self ();
  main_thread.s.cooperative = true;
  init_bc_thread (&main_thread.s.bc);
}

void
syms_of_threads (void)
{
#ifdef THREADS_ENABLED
  defsubr (&Sthread_yield);
  defsubr (&Smake_thread);
  defsubr (&Scurrent_thread);
  defsubr (&Sthread_name);
  defsubr (&Sthread_signal);
  defsubr (&Sthread_live_p);
  defsubr (&Sthread_join);
  defsubr (&Sthread_blocker);
  defsubr (&Sall_threads);
  defsubr (&Smake_mutex);
  defsubr (&Smutex_lock);
  defsubr (&Smutex_unlock);
  defsubr (&Smutex_name);
  defsubr (&Smake_condition_variable);
  defsubr (&Scondition_wait);
  defsubr (&Scondition_notify);
  defsubr (&Scondition_mutex);
  defsubr (&Scondition_name);
  defsubr (&Sthread_last_error);

  staticpro (&last_thread_error);
  last_thread_error = Qnil;

  Fprovide (intern_c_string ("threads"), Qnil);
#endif

  DEFSYM (Qthreadp, "threadp");
  DEFSYM (Qmutexp, "mutexp");
  DEFSYM (Qcondition_variable_p, "condition-variable-p");

  DEFVAR_LISP ("main-thread", Vmain_thread,
    doc: /* The main thread of Emacs.  */);
#ifdef THREADS_ENABLED
  XSETTHREAD (Vmain_thread, &main_thread.s);
#else
  Vmain_thread = Qnil;
#endif
}
