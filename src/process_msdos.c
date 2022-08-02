/* Asynchronous subprocess control for GNU Emacs.

Copyright (C) 1985-1988, 1993-1996, 1998-1999, 2001-2022 Free Software
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


#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>		/* Some typedefs are used in sys/file.h.  */
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <netinet/tcp.h>

#include "lisp.h"

/* Only MS-DOS does not define `subprocesses'.  */
#define PIPECONN_P(p) false
#define PIPECONN1_P(p) false

#ifdef HAVE_SETRLIMIT
# include <sys/resource.h>

/* If NOFILE_LIMIT.rlim_cur is greater than FD_SETSIZE, then
   NOFILE_LIMIT is the initial limit on the number of open files,
   which should be restored in child processes.  */
static struct rlimit nofile_limit;
#endif

#include "systime.h"
#include "systty.h"

#include "window.h"
#include "character.h"
#include "buffer.h"
#include "coding.h"
#include "process.h"
#include "frame.h"
#include "termopts.h"
#include "keyboard.h"
#include "blockinput.h"
#include "atimer.h"
#include "sysselect.h"
#include "syssignal.h"
#include "syswait.h"
#ifdef HAVE_GNUTLS
#include "gnutls.h"
#endif

#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

#ifdef HAVE_GLIB
#include "xgselect.h"
#ifndef WINDOWSNT
#include <glib.h>
#endif
#endif

#if defined HAVE_GETADDRINFO_A || defined HAVE_GNUTLS
/* This is 0.1s in nanoseconds. */
#define ASYNC_RETRY_NSEC 100000000
#endif

#ifdef WINDOWSNT
extern int sys_select (int, fd_set *, fd_set *, fd_set *,
                       const struct timespec *, const sigset_t *);
#endif

/* Work around GCC 4.3.0 bug with strict overflow checking; see
   <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=52904>.
   This bug appears to be fixed in GCC 5.1, so don't work around it there.  */
#if GNUC_PREREQ (4, 3, 0) && ! GNUC_PREREQ (5, 1, 0)
# pragma GCC diagnostic ignored "-Wstrict-overflow"
#endif

/* True if keyboard input is on hold, zero otherwise.  */

static bool kbd_is_on_hold;

/* Nonzero means don't run process sentinels.  This is used
   when exiting.  */
bool inhibit_sentinels;

/* This is referenced in thread.c:run_thread (which is never actually
   called, since threads are not enabled for this configuration.  */
void
update_processes_for_thread_death (Lisp_Object dying_thread)
{
}

/* Defined in msdos.c.  */
extern int sys_select (int, fd_set *, fd_set *, fd_set *,
		       struct timespec *, void *);

/* Implementation of wait_reading_process_output, assuming that there
   are no subprocesses.  Used only by the MS-DOS build.

   Wait for timeout to elapse and/or keyboard input to be available.

   TIME_LIMIT is:
     timeout in seconds
     If negative, gobble data immediately available but don't wait for any.

   NSECS is:
     an additional duration to wait, measured in nanoseconds
     If TIME_LIMIT is zero, then:
       If NSECS == 0, there is no limit.
       If NSECS > 0, the timeout consists of NSECS only.
       If NSECS < 0, gobble data immediately, as if TIME_LIMIT were negative.

   READ_KBD is:
     0 to ignore keyboard input, or
     1 to return when input is available, or
     -1 means caller will actually read the input, so don't throw to
       the quit handler.

   see full version for other parameters. We know that wait_proc will
     always be NULL, since `subprocesses' isn't defined.

   DO_DISPLAY means redisplay should be done to show subprocess
   output that arrives.

   Return -1 signifying we got no output and did not try.  */

int
wait_reading_process_output (intmax_t time_limit, int nsecs, int read_kbd,
			     bool do_display, struct Lisp_Process *wait_proc,
			     int just_wait_proc)
{
  register int nfds;
  struct timespec end_time, timeout;
  enum { MINIMUM = -1, TIMEOUT, FOREVER } wait;

  if (TYPE_MAXIMUM (time_t) < time_limit)
    time_limit = TYPE_MAXIMUM (time_t);

  if (time_limit < 0 || nsecs < 0)
    wait = MINIMUM;
  else if (time_limit > 0 || nsecs > 0)
    {
      wait = TIMEOUT;
      end_time = timespec_add (current_timespec (),
                               make_timespec (time_limit, nsecs));
    }
  else
    wait = FOREVER;

  /* Turn off periodic alarms (in case they are in use)
     and then turn off any other atimers,
     because the select emulator uses alarms.  */
  stop_polling ();
  turn_on_atimers (0);

  while (1)
    {
      bool timeout_reduced_for_timers = false;
      fd_set waitchannels;
      int xerrno;

      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (read_kbd >= 0)
	maybe_quit ();

      /* Compute time from now till when time limit is up.  */
      /* Exit if already run out.  */
      if (wait == TIMEOUT)
	{
	  struct timespec now = current_timespec ();
	  if (timespec_cmp (end_time, now) <= 0)
	    break;
	  timeout = timespec_sub (end_time, now);
	}
      else
	timeout = make_timespec (wait < TIMEOUT ? 0 : 100000, 0);

      /* If our caller will not immediately handle keyboard events,
	 run timer events directly.
	 (Callers that will immediately read keyboard events
	 call timer_delay on their own.)  */
      {
	struct timespec timer_delay;

	do
	  {
	    unsigned old_timers_run = timers_run;
	    timer_delay = timer_check ();
	    if (timers_run != old_timers_run && do_display)
	      /* We must retry, since a timer may have requeued itself
		 and that could alter the time delay.  */
	      redisplay_preserve_echo_area (14);
	    else
	      break;
	  }
	while (!detect_input_pending ());

	/* If there is unread keyboard input, also return.  */
	if (read_kbd != 0
	    && requeued_events_pending_p ())
	  break;

	if (timespec_valid_p (timer_delay))
	  {
	    if (timespec_cmp (timer_delay, timeout) < 0)
	      {
		timeout = timer_delay;
		timeout_reduced_for_timers = true;
	      }
	  }
      }

      /* `set_waiting_for_input' is a Blandyism that claims to have
	 emacs react immediately to C-g and signals.  Passing a
	 writable reference to timeout so that signal handlers can
	 manipulate timeout out-of-band is super obtuse and probably
	 makes no discernible difference.
      */
      if (read_kbd < 0)
	set_waiting_for_input (&timeout);

      /* If a frame has been newly mapped and needs updating,
	 reprocess its display stuff.  */
      if (frame_garbaged && do_display)
	{
	  clear_waiting_for_input ();
	  redisplay_preserve_echo_area (15);
	  if (read_kbd < 0)
	    set_waiting_for_input (&timeout);
	}

      /* Wait till there is something to do.  */
      FD_ZERO (&waitchannels);
      if (read_kbd && detect_input_pending ())
	nfds = 0;
      else
	{
	  if (read_kbd)
	    FD_SET (0, &waitchannels);
	  nfds = pselect (1, &waitchannels, NULL, NULL, &timeout, NULL);
	}

      xerrno = errno;

      clear_waiting_for_input ();

      /*  If we woke up due to SIGWINCH, actually change size now.  */
      do_pending_window_change (0);

      if (wait < FOREVER && nfds == 0 && ! timeout_reduced_for_timers)
	/* We waited the full specified time, so return now.  */
	break;

      if (nfds == -1)
	{
	  /* If the system call was interrupted, then go around the
	     loop again.  */
	  if (xerrno == EINTR)
	    FD_ZERO (&waitchannels);
	  else
	    report_file_errno ("Failed select", Qnil, xerrno);
	}

      /* Check for keyboard input.  */

      if (read_kbd
	  && detect_input_pending_run_timers (do_display))
	{
	  swallow_events (do_display);
	  if (detect_input_pending_run_timers (do_display))
	    break;
	}

      /* If there is unread keyboard input, also return.  */
      if (read_kbd && requeued_events_pending_p ())
	break;
    }

  start_polling ();

  return -1;
}

/* The following functions are needed even if async subprocesses are
   not supported.  Some of them are no-op stubs in that case.  */

#ifdef HAVE_TIMERFD

/* Add FD, which is a descriptor returned by timerfd_create,
   to the set of non-keyboard input descriptors.  */

void
add_timer_wait_descriptor (int fd)
{
  eassert (0 <= fd && fd < FD_SETSIZE);
  add_read_fd (fd, timerfd_callback, NULL);
  fd_callback_info[fd].flags &= ~KEYBOARD_FD;
}

#endif /* HAVE_TIMERFD */

/* If program file NAME starts with /: for quoting a magic
   name, remove that, preserving the multibyteness of NAME.  */

Lisp_Object
remove_slash_colon (Lisp_Object name)
{
  return
    (SREF (name, 0) == '/' && SREF (name, 1) == ':'
     ? (STRING_MULTIBYTE (name)
	? make_multibyte_string (SSDATA (name) + 2,
				 SCHARS (name) - 2,
				 SBYTES (name) - 2)
	: make_unibyte_string (SSDATA (name) + 2, SBYTES (name) - 2))
     : name);
}

/* Add DESC to the set of keyboard input descriptors.  */

void
add_keyboard_wait_descriptor (int desc)
{
}

/* From now on, do not expect DESC to give keyboard input.  */

void
delete_keyboard_wait_descriptor (int desc)
{
}

/* Setup coding systems of PROCESS.  */

void
setup_process_coding_systems (Lisp_Object process)
{
}

DEFUN ("get-buffer-process", Fget_buffer_process, Sget_buffer_process, 1, 1, 0,
       doc: /* Return the (or a) live process associated with BUFFER.
BUFFER may be a buffer or the name of one.
Return nil if all processes associated with BUFFER have been
deleted or killed.  */)
  (register Lisp_Object buffer)
{
  return Qnil;
}

DEFUN ("process-inherit-coding-system-flag",
       Fprocess_inherit_coding_system_flag, Sprocess_inherit_coding_system_flag,
       1, 1, 0,
       doc: /* Return the value of inherit-coding-system flag for PROCESS.
If this flag is t, `buffer-file-coding-system' of the buffer
associated with PROCESS will inherit the coding system used to decode
the process output.  */)
  (register Lisp_Object process)
{
  /* Ignore the argument and return the value of
     inherit-process-coding-system.  */
  return inherit_process_coding_system ? Qt : Qnil;
}

/* Kill all processes associated with `buffer'.
   If `buffer' is nil, kill all processes.  */

void
kill_buffer_processes (Lisp_Object buffer)
{
  /* Since we have no subprocesses, this does nothing.  */
}

DEFUN ("waiting-for-user-input-p", Fwaiting_for_user_input_p,
       Swaiting_for_user_input_p, 0, 0, 0,
       doc: /* Return nil.  */)
  (void)
{
  return Qnil;
}

/* Stop reading input from keyboard sources.  */

void
hold_keyboard_input (void)
{
  kbd_is_on_hold = 1;
}

/* Resume reading input from keyboard sources.  */

void
unhold_keyboard_input (void)
{
  kbd_is_on_hold = 0;
}

/* Return true if keyboard input is on hold, zero otherwise.  */

bool
kbd_on_hold_p (void)
{
  return kbd_is_on_hold;
}


/* Enumeration of and access to system processes a-la ps(1).  */

DEFUN ("list-system-processes", Flist_system_processes, Slist_system_processes,
       0, 0, 0,
       doc: /* Return a list of numerical process IDs of all running processes.
If this functionality is unsupported, return nil.
If `default-directory' is remote, return process IDs of the respective remote host.

See `process-attributes' for getting attributes of a process given its ID.  */)
  (void)
{
  Lisp_Object handler
    = Ffind_file_name_handler (BVAR (current_buffer, directory),
			       Qlist_system_processes);
  if (!NILP (handler))
    return call1 (handler, Qlist_system_processes);

  return list_system_processes ();
}

DEFUN ("process-attributes", Fprocess_attributes,
       Sprocess_attributes, 1, 1, 0,
       doc: /* Return attributes of the process given by its PID, a number.
If `default-directory' is remote, PID is regarded as process
identifier on the respective remote host.

Value is an alist where each element is a cons cell of the form

    (KEY . VALUE)

If this functionality is unsupported, the value is nil.

See `list-system-processes' for getting a list of all process IDs.

The KEYs of the attributes that this function may return are listed
below, together with the type of the associated VALUE (in parentheses).
Not all platforms support all of these attributes; unsupported
attributes will not appear in the returned alist.
Unless explicitly indicated otherwise, numbers can have either
integer or floating point values.

 euid    -- Effective user User ID of the process (number)
 user    -- User name corresponding to euid (string)
 egid    -- Effective user Group ID of the process (number)
 group   -- Group name corresponding to egid (string)
 comm    -- Command name (executable name only) (string)
 state   -- Process state code, such as "S", "R", or "T" (string)
 ppid    -- Parent process ID (number)
 pgrp    -- Process group ID (number)
 sess    -- Session ID, i.e. process ID of session leader (number)
 ttname  -- Controlling tty name (string)
 tpgid   -- ID of foreground process group on the process's tty (number)
 minflt  -- number of minor page faults (number)
 majflt  -- number of major page faults (number)
 cminflt -- cumulative number of minor page faults (number)
 cmajflt -- cumulative number of major page faults (number)
 utime   -- user time used by the process, in `current-time' format
 stime   -- system time used by the process (current-time)
 time    -- sum of utime and stime (current-time)
 cutime  -- user time used by the process and its children (current-time)
 cstime  -- system time used by the process and its children (current-time)
 ctime   -- sum of cutime and cstime (current-time)
 pri     -- priority of the process (number)
 nice    -- nice value of the process (number)
 thcount -- process thread count (number)
 start   -- time the process started (current-time)
 vsize   -- virtual memory size of the process in KB's (number)
 rss     -- resident set size of the process in KB's (number)
 etime   -- elapsed time the process is running (current-time)
 pcpu    -- percents of CPU time used by the process (floating-point number)
 pmem    -- percents of total physical memory used by process's resident set
              (floating-point number)
 args    -- command line which invoked the process (string).  */)
  ( Lisp_Object pid)
{
  Lisp_Object handler
    = Ffind_file_name_handler (BVAR (current_buffer, directory),
			       Qprocess_attributes);
  if (!NILP (handler))
    return call2 (handler, Qprocess_attributes, pid);

  return system_process_attributes (pid);
}

DEFUN ("num-processors", Fnum_processors, Snum_processors, 0, 1, 0,
       doc: /* Return the number of processors, a positive integer.
Each usable thread execution unit counts as a processor.
By default, count the number of available processors,
overridable via the OMP_NUM_THREADS environment variable.
If optional argument QUERY is `current', ignore OMP_NUM_THREADS.
If QUERY is `all', also count processors not available.  */)
  (Lisp_Object query)
{
#ifndef MSDOS
  return make_uint (num_processors (EQ (query, Qall) ? NPROC_ALL
				    : EQ (query, Qcurrent) ? NPROC_CURRENT
				    : NPROC_CURRENT_OVERRIDABLE));
#else
  return make_fixnum (1);
#endif
}

DEFUN ("signal-names", Fsignal_names, Ssignal_names, 0, 0, 0,
       doc: /* Return a list of known signal names on this system.  */)
  (void)
{
#ifndef MSDOS
  int i;
  char name[SIG2STR_MAX];
  Lisp_Object names = Qnil;

  for (i = 0; i <= SIGNUM_BOUND; ++i)
    {
      if (!sig2str (i, name))
	names = Fcons (build_string (name), names);
    }

  return names;
#else
  return Qnil;
#endif
}

/* Limit the number of open files to the value it had at startup.  */

void
restore_nofile_limit (void)
{
#ifdef HAVE_SETRLIMIT
  if (FD_SETSIZE < nofile_limit.rlim_cur)
    setrlimit (RLIMIT_NOFILE, &nofile_limit);
#endif
}

int
open_channel_for_module (Lisp_Object process)
{
  CHECK_PROCESS (process);
  CHECK_TYPE (PIPECONN_P (process), Qpipe_process_p, process);
#ifndef MSDOS
  int fd = dup (XPROCESS (process)->open_fd[SUBPROCESS_STDOUT]);
  if (fd == -1)
    report_file_error ("Cannot duplicate file descriptor", Qnil);
  return fd;
#else
  /* PIPECONN_P returning true shouldn't be possible on MSDOS.  */
  emacs_abort ();
#endif
}


/* This is not called "init_process" because that is the name of a
   Mach system call, so it would cause problems on Darwin systems.  */
void
init_process_emacs (int sockfd)
{
  kbd_is_on_hold = 0;
}

void
syms_of_process (void)
{
  DEFSYM (Qmake_process, "make-process");
  DEFSYM (Qlist_system_processes, "list-system-processes");
  DEFSYM (Qprocess_attributes, "process-attributes");

  DEFSYM (QCname, ":name");
  DEFSYM (QCtype, ":type");

  DEFSYM (Qeuid, "euid");
  DEFSYM (Qegid, "egid");
  DEFSYM (Quser, "user");
  DEFSYM (Qgroup, "group");
  DEFSYM (Qcomm, "comm");
  DEFSYM (Qstate, "state");
  DEFSYM (Qppid, "ppid");
  DEFSYM (Qpgrp, "pgrp");
  DEFSYM (Qsess, "sess");
  DEFSYM (Qttname, "ttname");
  DEFSYM (Qtpgid, "tpgid");
  DEFSYM (Qminflt, "minflt");
  DEFSYM (Qmajflt, "majflt");
  DEFSYM (Qcminflt, "cminflt");
  DEFSYM (Qcmajflt, "cmajflt");
  DEFSYM (Qutime, "utime");
  DEFSYM (Qstime, "stime");
  DEFSYM (Qtime, "time");
  DEFSYM (Qcutime, "cutime");
  DEFSYM (Qcstime, "cstime");
  DEFSYM (Qctime, "ctime");
  DEFSYM (Qpri, "pri");
  DEFSYM (Qnice, "nice");
  DEFSYM (Qthcount, "thcount");
  DEFSYM (Qstart, "start");
  DEFSYM (Qvsize, "vsize");
  DEFSYM (Qrss, "rss");
  DEFSYM (Qetime, "etime");
  DEFSYM (Qpcpu, "pcpu");
  DEFSYM (Qpmem, "pmem");
  DEFSYM (Qargs, "args");
  DEFSYM (Qall, "all");
  DEFSYM (Qcurrent, "current");

  DEFVAR_BOOL ("delete-exited-processes", delete_exited_processes,
	       doc: /* Non-nil means delete processes immediately when they exit.
A value of nil means don't delete them until `list-processes' is run.  */);

  delete_exited_processes = 1;

  defsubr (&Sget_buffer_process);
  defsubr (&Sprocess_inherit_coding_system_flag);
  defsubr (&Slist_system_processes);
  defsubr (&Sprocess_attributes);
  defsubr (&Snum_processors);
  defsubr (&Ssignal_names);
}
