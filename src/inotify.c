/* Inotify support for Emacs

Copyright (C) 2012-2024 Free Software Foundation, Inc.

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

#include "lisp.h"
#include "coding.h"
#include "process.h"
#include "keyboard.h"
#include "termhooks.h"

#include <errno.h>
#include <sys/inotify.h>
#include <sys/ioctl.h>

/* Ignore bits that might be undefined on old GNU/Linux systems.  */

#ifndef IN_EXCL_UNLINK
# define IN_EXCL_UNLINK 0
#endif
#ifndef IN_DONT_FOLLOW
# define IN_DONT_FOLLOW 0
#endif
#ifndef IN_ONLYDIR
# define IN_ONLYDIR 0
#endif

/* Central instance returned by inotify_init(2).  */
static int inotifyfd = -1;

/* Alist of the form

   ((DESCRIPTOR1 . WATCHES1) (DESCRIPTOR2 . WATCHES2) ...)

   DESCRIPTOR is the return value of inotify_add_watch(2).

   WATCHES is a list containing elements of the form

   (IDX FILENAME CALLBACK MASK)

   where IDX is the element's ordinal within WATCHES.  */
static Lisp_Object watches_alist;

static Lisp_Object
mask_to_aspects (uint32_t mask)
{
  Lisp_Object aspects = Qnil;
  if (mask & IN_ACCESS)
    aspects = Fcons (Qaccess, aspects);
  if (mask & IN_ATTRIB)
    aspects = Fcons (Qattrib, aspects);
  if (mask & IN_CLOSE_WRITE)
    aspects = Fcons (Qclose_write, aspects);
  if (mask & IN_CLOSE_NOWRITE)
    aspects = Fcons (Qclose_nowrite, aspects);
  if (mask & IN_CREATE)
    aspects = Fcons (Qcreate, aspects);
  if (mask & IN_DELETE)
    aspects = Fcons (Qdelete, aspects);
  if (mask & IN_DELETE_SELF)
    aspects = Fcons (Qdelete_self, aspects);
  if (mask & IN_MODIFY)
    aspects = Fcons (Qmodify, aspects);
  if (mask & IN_MOVE_SELF)
    aspects = Fcons (Qmove_self, aspects);
  if (mask & IN_MOVED_FROM)
    aspects = Fcons (Qmoved_from, aspects);
  if (mask & IN_MOVED_TO)
    aspects = Fcons (Qmoved_to, aspects);
  if (mask & IN_OPEN)
    aspects = Fcons (Qopen,  aspects);
  if (mask & IN_IGNORED)
    aspects = Fcons (Qignored, aspects);
  if (mask & IN_ISDIR)
    aspects = Fcons (Qisdir, aspects);
  if (mask & IN_Q_OVERFLOW)
    aspects = Fcons (Qq_overflow, aspects);
  if (mask & IN_UNMOUNT)
    aspects = Fcons (Qunmount, aspects);
  return aspects;
}

static uint32_t
symbol_to_inotifymask (Lisp_Object symb)
{
  if (EQ (symb, Qaccess))
    return IN_ACCESS;
  else if (EQ (symb, Qattrib))
    return IN_ATTRIB;
  else if (EQ (symb, Qclose_write))
    return IN_CLOSE_WRITE;
  else if (EQ (symb, Qclose_nowrite))
    return IN_CLOSE_NOWRITE;
  else if (EQ (symb, Qcreate))
    return IN_CREATE;
  else if (EQ (symb, Qdelete))
    return IN_DELETE;
  else if (EQ (symb, Qdelete_self))
    return IN_DELETE_SELF;
  else if (EQ (symb, Qmodify))
    return IN_MODIFY;
  else if (EQ (symb, Qmove_self))
    return IN_MOVE_SELF;
  else if (EQ (symb, Qmoved_from))
    return IN_MOVED_FROM;
  else if (EQ (symb, Qmoved_to))
    return IN_MOVED_TO;
  else if (EQ (symb, Qopen))
    return IN_OPEN;
  else if (EQ (symb, Qmove))
    return IN_MOVE;
  else if (EQ (symb, Qclose))
    return IN_CLOSE;
  else if (EQ (symb, Qdont_follow))
    return IN_DONT_FOLLOW;
  else if (EQ (symb, Qonlydir))
    return IN_ONLYDIR;
  else if (EQ (symb, Qt) || EQ (symb, Qall_events))
    return IN_ALL_EVENTS;
  else
    {
      errno = EINVAL;
      report_file_notify_error ("Unknown aspect", symb);
    }
}

static uint32_t
aspect_to_inotifymask (Lisp_Object aspect)
{
  if (CONSP (aspect) || NILP (aspect))
    {
      Lisp_Object x = aspect;
      uint32_t mask = 0;
      FOR_EACH_TAIL (x)
	mask |= symbol_to_inotifymask (XCAR (x));
      CHECK_LIST_END (x, aspect);
      return mask;
    }
  else
    return symbol_to_inotifymask (aspect);
}

static Lisp_Object
inotifyevent_to_event (Lisp_Object watch, struct inotify_event const *ev)
{
  Lisp_Object name;
  uint32_t mask;
  CONS_TO_INTEGER (Fnth (make_fixnum (3), watch), uint32_t, mask);

  if (!(mask & ev->mask))
    return Qnil;

  if (ev->len > 0)
    {
      name = make_unibyte_string (ev->name, strnlen (ev->name, ev->len));
      name = DECODE_FILE (name);
    }
  else
    name = XCAR (XCDR (watch));

  return list2 (list4 (Fcons (INT_TO_INTEGER (ev->wd), XCAR (watch)),
                       mask_to_aspects (ev->mask),
                       name,
		       INT_TO_INTEGER (ev->cookie)),
		Fnth (make_fixnum (2), watch));
}

/* Static watches_alist is of the form

   ((DESCRIPTOR1 . WATCHES1) (DESCRIPTOR2 . WATCHES2) ...)

   Return a cons (DESCRIPTOR . IDX) where DESCRIPTOR is WD as a Lisp
   fixnum, and IDX is the position within WATCHES the new watch was
   inserted.  */

static Lisp_Object
add_watch (int wd, Lisp_Object filename,
	   uint32_t imask, Lisp_Object callback)
{
  Lisp_Object descriptor = INT_TO_INTEGER (wd);
  Lisp_Object watches = assoc_no_quit (descriptor, watches_alist);
  Lisp_Object mask = INT_TO_INTEGER (imask);

  EMACS_INT idx = 0;
  if (NILP (watches))
    {
      watches = list1 (descriptor);
      watches_alist = Fcons (watches, watches_alist);
    }
  else
    {
      /* Assign IDX to the first unused ordinal in WATCHES.  */
      for (; !NILP (XCDR (watches)); watches = XCDR (watches), idx++)
	if (!EQ (XCAR (XCAR (XCDR (watches))), make_fixnum (idx)))
	  break;
      if (MOST_POSITIVE_FIXNUM < idx)
	emacs_abort ();
    }

  /* Sort-preserving splice of new watch into the gap at WATCHES.  */
  XSETCDR (watches, Fcons (list4 (make_fixnum (idx), filename, callback, mask),
			   XCDR (watches)));

  return Fcons (descriptor, make_fixnum (idx));
}

/* Return cons of watches_alist whose cdr references
   DESCRIPTOR.  This lets the caller remove DESCRIPTOR's entry.
   If DESCRIPTOR is not found, return Qnil.
   IF DESCRIPTOR is at the car of watches_alist, return Qt.
  */

static Lisp_Object
preceding_cons (Lisp_Object descriptor)
{
  Lisp_Object tail, prevtail = Qt;
  for (tail = watches_alist; !NILP (tail); prevtail = tail, tail = XCDR (tail))
    if (equal_no_quit (XCAR (XCAR (tail)), descriptor))
      return prevtail;
  return Qnil;
}

/*  Remove all watches associated with the watch list element after
    PREVTAIL, or after the first element if PREVTAIL is t.  */
static void
remove_descriptor (Lisp_Object prevtail)
{
  Lisp_Object tail = CONSP (prevtail) ? XCDR (prevtail) : watches_alist;

  if (CONSP (prevtail))
    XSETCDR (prevtail, XCDR (tail));
  else
    {
      watches_alist = XCDR (tail);
      if (NILP (watches_alist))
	{
	  delete_read_fd (inotifyfd);
	  emacs_close (inotifyfd);
	  inotifyfd = -1;
	}
    }
}

static void
remove_watch (Lisp_Object descriptor, Lisp_Object idx)
{
  Lisp_Object prevtail = preceding_cons (descriptor);
  if (!NILP (prevtail))
    {
      /* ELT should be (DESCRIPTOR . WATCHES) */
      Lisp_Object elt = XCAR (CONSP (prevtail) ? XCDR (prevtail) : watches_alist);
      for (Lisp_Object prev = elt; !NILP (XCDR (prev)); prev = XCDR (prev))
	if (EQ (idx, XCAR (XCAR (XCDR (prev)))))
	  {
	    XSETCDR (prev, XCDR (XCDR (prev))); /* unsplice */
	    if (NILP (XCDR (elt)))
	      {
		int wd, ret;
		CONS_TO_INTEGER (XCAR (elt), int, wd);
		ret = inotify_rm_watch (inotifyfd, wd);
		remove_descriptor (prevtail);
		if (ret != 0)
		  report_file_notify_error ("Could not rm watch", XCAR (elt));
	      }
	    break;
	  }
    }
}

/* This callback is called when the FD is available for read.  The inotify
   events are read from FD and converted into input_events.  */

static void
inotify_callback (int fd, void *_)
{
  int to_read;
  char *buffer;
  ssize_t n;
  struct input_event event;

  if (ioctl (fd, FIONREAD, &to_read) < 0)
    report_file_notify_error ("Error while retrieving file system events",
			      Qnil);
  USE_SAFE_ALLOCA;
  buffer = SAFE_ALLOCA (to_read);
  errno = -1;
  n = read (fd, buffer, to_read);
  if (errno == EAGAIN)
    n = 0;
  if (n < 0)
    report_file_notify_error ("Error while reading file system events", Qnil);

  EVENT_INIT (event);
  event.kind = FILE_NOTIFY_EVENT;
  for (ssize_t i = 0; i < n; )
    {
      struct inotify_event *ev = (struct inotify_event *) &buffer[i];
      Lisp_Object descriptor = INT_TO_INTEGER (ev->wd);
      Lisp_Object prevtail = preceding_cons (descriptor);

      if (!NILP (prevtail))
        {
	  Lisp_Object tail = CONSP (prevtail) ? XCDR (prevtail) : watches_alist;
	  for (Lisp_Object watches = XCDR (XCAR (tail)); !NILP (watches);
	       watches = XCDR (watches))
            {
              event.arg = inotifyevent_to_event (XCAR (watches), ev);
              if (!NILP (event.arg))
                kbd_buffer_store_event (&event);
            }
          if (ev->mask & IN_IGNORED)
	    /* Watch exogenously removed.  No need to inotify_rm_watch()  */
	    remove_descriptor (prevtail);
        }
      i += sizeof (*ev) + ev->len;
    }
  SAFE_FREE ();
}

DEFUN ("inotify-add-watch", Finotify_add_watch, Sinotify_add_watch, 3, 3, 0,
       doc: /* Wrapper for inotify_add_watch(2).

Return a watch descriptor for FILENAME invoking CALLBACK when an event
satisfying ASPECT occurs.

ASPECT might be one or a list of the following symbols:

access
attrib
close-write
close-nowrite
create
delete
delete-self
modify
move-self
moved-from
moved-to
open
move
close
all-events or t
dont-follow
onlydir

CALLBACK is passed a form representing an event,

\(WATCH-DESCRIPTOR EVENT-ASPECT NAME COOKIE)

In addition to ASPECT symbols, EVENT-ASPECT can also contain one of:

ignored
isdir
q-overflow
unmount

NAME and COOKIE are as described in inotify(7).

The following bit-masks cannot be used because descriptors are shared
across different callers.

IN_EXCL_UNLINK
IN_MASK_ADD
IN_ONESHOT  */)
  (Lisp_Object filename, Lisp_Object aspect, Lisp_Object callback)
{
  Lisp_Object encoded_file_name;
  int wd = -1;
  uint32_t imask = aspect_to_inotifymask (aspect);
  uint32_t mask = imask | IN_MASK_ADD | IN_EXCL_UNLINK;

  CHECK_STRING (filename);

  if (inotifyfd < 0)
    {
      inotifyfd = inotify_init1 (IN_NONBLOCK | IN_CLOEXEC);
      if (inotifyfd < 0)
	report_file_notify_error ("File watching is not available", Qnil);
      watches_alist = Qnil;
      add_read_fd (inotifyfd, &inotify_callback, NULL);
    }

  encoded_file_name = ENCODE_FILE (filename);
  wd = inotify_add_watch (inotifyfd, SSDATA (encoded_file_name), mask);
  if (wd < 0)
    report_file_notify_error ("Could not add watch for file", filename);

  return add_watch (wd, filename, imask, callback);
}

static bool
valid_watch_descriptor (Lisp_Object wd)
{
  return (CONSP (wd)
	  && (RANGED_FIXNUMP (0, XCAR (wd), INT_MAX)
	      || (CONSP (XCAR (wd))
		  && RANGED_FIXNUMP ((MOST_POSITIVE_FIXNUM >> 16) + 1,
				      XCAR (XCAR (wd)), INT_MAX >> 16)
		  && RANGED_FIXNUMP (0, XCDR (XCAR (wd)), (1 << 16) - 1)))
	  && FIXNATP (XCDR (wd)));
}

DEFUN ("inotify-rm-watch", Finotify_rm_watch, Sinotify_rm_watch, 1, 1, 0,
       doc: /* Remove an existing WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `inotify-add-watch'.

See inotify_rm_watch(2) for more information.  */)
  (Lisp_Object watch_descriptor)
{

  Lisp_Object descriptor, id;

  if (!valid_watch_descriptor (watch_descriptor))
    report_file_notify_error ("Invalid descriptor ", watch_descriptor);

  descriptor = XCAR (watch_descriptor);
  id = XCDR (watch_descriptor);
  remove_watch (descriptor, id);

  return Qt;
}

DEFUN ("inotify-valid-p", Finotify_valid_p, Sinotify_valid_p, 1, 1, 0,
       doc: /* Check a watch specified by its WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `inotify-add-watch'.

A watch can become invalid if the file or directory it watches is
deleted, or if the watcher thread exits abnormally for any other
reason.  Removing the watch by calling `inotify-rm-watch' also makes
it invalid.  */)
  (Lisp_Object watch_descriptor)
{
  if (valid_watch_descriptor (watch_descriptor))
    {
      Lisp_Object tail = assoc_no_quit (XCAR (watch_descriptor), watches_alist);
      if (!NILP (tail))
	{
	  Lisp_Object watch = assq_no_quit (XCDR (watch_descriptor), XCDR (tail));
	  if (!NILP (watch))
	    return Qt;
	}
    }
  return Qnil;
}

void
syms_of_inotify (void)
{
  DEFSYM (Qaccess, "access");		/* IN_ACCESS */
  DEFSYM (Qattrib, "attrib");		/* IN_ATTRIB */
  DEFSYM (Qclose_write, "close-write");	/* IN_CLOSE_WRITE */
  DEFSYM (Qclose_nowrite, "close-nowrite");
					/* IN_CLOSE_NOWRITE */
  DEFSYM (Qcreate, "create");		/* IN_CREATE */
  DEFSYM (Qdelete, "delete");		/* IN_DELETE */
  DEFSYM (Qdelete_self, "delete-self");	/* IN_DELETE_SELF */
  DEFSYM (Qmodify, "modify");		/* IN_MODIFY */
  DEFSYM (Qmove_self, "move-self");	/* IN_MOVE_SELF */
  DEFSYM (Qmoved_from, "moved-from");	/* IN_MOVED_FROM */
  DEFSYM (Qmoved_to, "moved-to");	/* IN_MOVED_TO */
  DEFSYM (Qopen, "open");		/* IN_OPEN */

  DEFSYM (Qall_events, "all-events");	/* IN_ALL_EVENTS */
  DEFSYM (Qmove, "move");		/* IN_MOVE */
  DEFSYM (Qclose, "close");		/* IN_CLOSE */

  DEFSYM (Qdont_follow, "dont-follow");	/* IN_DONT_FOLLOW */
  DEFSYM (Qonlydir, "onlydir");		/* IN_ONLYDIR */

  DEFSYM (Qisdir, "isdir");		/* IN_ISDIR */
  DEFSYM (Qq_overflow, "q-overflow");	/* IN_Q_OVERFLOW */
  DEFSYM (Qunmount, "unmount");		/* IN_UNMOUNT */

  defsubr (&Sinotify_add_watch);
  defsubr (&Sinotify_rm_watch);
  defsubr (&Sinotify_valid_p);

  staticpro (&watches_alist);

  Fprovide (intern_c_string ("inotify"), Qnil);
}
