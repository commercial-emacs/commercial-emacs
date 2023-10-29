/* Function for handling the GLib event loop.

Copyright (C) 2009-2023 Free Software Foundation, Inc.

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

#include "xgselect.h"

#ifdef HAVE_GLIB

#include <glib.h>
#include <errno.h>
#include "lisp.h"
#include "blockinput.h"
#include "systime.h"
#include "process.h"

/* xg_select() augments pselect() for Glib.

   1. Glib has its own timers.  If we pselect with a longer timeout
      than Glib's, we'd starve Glib's timers.  So reduce our timeout
      to match Glib's.

   2. Glib has its own descriptors.  Add them to our select pool but
      ensure return value only reflects those descriptors passed as
      arguments. */

int
xg_select (int fds_lim, fd_set *rfds, fd_set *wfds, fd_set *efds,
	   struct timespec *timeout, sigset_t *sigmask)
{
  fd_set all_rfds, all_wfds;
  struct timespec tmo;
  struct timespec *tmop = timeout;

  GMainContext *context = g_main_context_default ();
  bool have_wfds = wfds != NULL;
  GPollFD buf[128];
  GPollFD *gfds = buf;
  const int nbuf = ARRAYELTS (buf);
  int nfds, ngfds, retval, max_fds = fds_lim - 1;
  bool context_acquired = false;
  int tmo_in_millisec;
#ifdef USE_GTK
  if (
# ifndef HAVE_PGTK
  x_gtk_use_native_input &&
# endif
  g_main_context_pending (context))
    {
      /* Under GTK native input or PGTK, a keypress results in two
	 events, the second of which does not express in ALL_RFDS
	 but instead via g_main_context_pending. (Bug#52761) */
      if (rfds) FD_ZERO (rfds);
      if (wfds) FD_ZERO (wfds);
      if (efds) FD_ZERO (efds);
      return 1;
    }
#endif
  if (main_thread_p (current_thread))
    context_acquired = g_main_context_acquire (context);

  if (rfds)
    all_rfds = *rfds;
  else
    FD_ZERO (&all_rfds);
  if (wfds)
    all_wfds = *wfds;
  else
    FD_ZERO (&all_wfds);

  ngfds = ! context_acquired ? -1 : g_main_context_query (context,
							  G_PRIORITY_LOW,
							  &tmo_in_millisec,
							  gfds, nbuf);

  if (ngfds > nbuf)
    {
      /* xnmalloc is thread-safe, SAFE_NALLOCA is not.  */
      gfds = xnmalloc (ngfds, sizeof *gfds);
      ngfds = g_main_context_query (context, G_PRIORITY_LOW, &tmo_in_millisec,
				    gfds, ngfds);
    }

  /* See point (2) in header comment.  */
  for (int i = 0; i < ngfds; ++i)
    {
      if (gfds[i].events & G_IO_IN)
        {
          FD_SET (gfds[i].fd, &all_rfds);
          if (gfds[i].fd > max_fds) max_fds = gfds[i].fd;
        }
      if (gfds[i].events & G_IO_OUT)
        {
          FD_SET (gfds[i].fd, &all_wfds);
          if (gfds[i].fd > max_fds) max_fds = gfds[i].fd;
          have_wfds = true;
        }
    }

  if (gfds != buf)
    xfree (gfds);

  if (ngfds >= 0 && tmo_in_millisec >= 0)
    {
      tmo = make_timespec (tmo_in_millisec / 1000,
			   1000 * 1000 * (tmo_in_millisec % 1000));
      if (! timeout
	  /* See point (1) in header comment.  */
	  || timespec_cmp (tmo, *timeout) < 0)
	tmop = &tmo;
    }

  fds_lim = max_fds + 1;
  bool ready_gfds = false;
  retval = nfds = thread_select (pselect, fds_lim,
				 &all_rfds, have_wfds ? &all_wfds : NULL, efds,
				 tmop, sigmask);
  if (nfds == 0)
    {
      if (rfds) FD_ZERO (rfds);
      if (wfds) FD_ZERO (wfds);
      if (efds) FD_ZERO (efds);
    }
  else if (nfds > 0)
    {
      retval = 0;
      for (int i = 0; i < fds_lim; ++i)
	{
	  if (FD_ISSET (i, &all_rfds))
	    {
	      if (rfds && FD_ISSET (i, rfds)) ++retval;
	      else ready_gfds = true;
	    }
	  else if (rfds)
	    FD_CLR (i, rfds);

	  if (have_wfds && FD_ISSET (i, &all_wfds))
	    {
	      if (wfds && FD_ISSET (i, wfds)) ++retval;
	      else ready_gfds = true;
	    }
	  else if (wfds)
	    FD_CLR (i, wfds);

	  if (efds && FD_ISSET (i, efds))
	    ++retval;
	}
    }

  if (context_acquired
#ifdef USE_GTK
      && retval == 0 /* in which case gtk_main_iteration may not get
			called.  */
#endif
      )
    {
      /* need to dispatch */
      int pselect_errno = errno;
      /* Czekalski: Callbacks containing block/unblock within
	 g_main_context_dispatch() trigger event loop recursion
	 unless we apply this encompassing block/unblock pair
	 (Bug#15801).  */
      block_input ();
      while (g_main_context_pending (context))
	g_main_context_dispatch (context);
      unblock_input ();
      errno = pselect_errno;
    }

  if (retval == 0
      && (ready_gfds || (tmop == &tmo && nfds == 0)))
    {
      /* Emulate pselect by returning error if accommodating glib's
	 descriptors prevented caller's descriptors from polling
	 ready.  */
      retval = -1;
      errno = EINTR;
    }

  if (context_acquired)
    g_main_context_release (context);
  return retval;
}
#endif /* HAVE_GLIB */
