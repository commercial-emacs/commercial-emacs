/* undo handling for GNU Emacs.
   Copyright (C) 1990, 1993-1994, 2000-2024 Free Software Foundation,
   Inc.

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
#include "buffer.h"
#include "keyboard.h"

/* The undo entry (t . modtime) marks the initial change to a previously
   unmodified buffer.  */
void
undo_push_maiden (void)
{
  if (!EQ (BVAR (current_buffer, undo_list), Qt)
      && MODIFF <= SAVE_MODIFF)
    {
      struct buffer *base_buffer = current_buffer->base_buffer != NULL
	? current_buffer->base_buffer
	: current_buffer;
      bset_undo_list (current_buffer,
		      Fcons (Fcons (Qt, buffer_visited_file_modtime (base_buffer)),
			     BVAR (current_buffer, undo_list)));
    }
}

/* Mark POINT_BEFORE_LAST_COMMAND_OR_UNDO for restoration unless it's
   already equal to BEG, the imminent restoration point.  */
static void
undo_push_point (ptrdiff_t beg)
{
  const bool on_boundary = !CONSP (BVAR (current_buffer, undo_list))
    || NILP (CAR (BVAR (current_buffer, undo_list)));

  undo_push_maiden ();

  if (on_boundary
      && point_before_last_command_or_undo != beg
      && buffer_before_last_command_or_undo == current_buffer)
    bset_undo_list (current_buffer,
		    Fcons (make_fixnum (point_before_last_command_or_undo),
			   BVAR (current_buffer, undo_list)));
}

/* Record an insertion that just happened or is about to happen,
   for LENGTH characters at position BEG.
   (It is possible to record an insertion before or after the fact
   because we don't need to record the contents.)  */

void
undo_push_insert (ptrdiff_t beg, ptrdiff_t length)
{
  if (!EQ (BVAR (current_buffer, undo_list), Qt))
    {
      Lisp_Object lbeg, lend;
      undo_push_point (beg);

      /* If this is following another insertion and consecutive with it
	 in the buffer, combine the two.  */
      if (CONSP (BVAR (current_buffer, undo_list)))
	{
	  Lisp_Object elt;
	  elt = XCAR (BVAR (current_buffer, undo_list));
	  if (CONSP (elt)
	      && FIXNUMP (XCAR (elt))
	      && FIXNUMP (XCDR (elt))
	      && XFIXNUM (XCDR (elt)) == beg)
	    {
	      XSETCDR (elt, make_fixnum (beg + length));
	      return;
	    }
	}

      XSETFASTINT (lbeg, beg);
      XSETINT (lend, beg + length);
      bset_undo_list (current_buffer,
		      Fcons (Fcons (lbeg, lend), BVAR (current_buffer, undo_list)));
    }
}

/* Record the fact that markers in the region of FROM, TO are about to
   be adjusted.  This is done only when a marker points within text
   being deleted, because that's the only case where an automatic
   marker adjustment won't be inverted automatically by undoing the
   buffer modification.  */

static void
undo_push_markers (ptrdiff_t from, ptrdiff_t to)
{
  for (struct Lisp_Marker *m = BUF_MARKERS (current_buffer); m; m = m->next)
    {
      ptrdiff_t charpos = m->charpos;
      eassert (charpos <= Z);

      if (from <= charpos && charpos <= to)
        {
          /* insertion_type nil markers will end up at the beginning of
             the re-inserted text after undoing a deletion, and must be
             adjusted to move them to the correct place.

             insertion_type t markers will automatically move forward
             upon re-inserting the deleted text, so we have to arrange
             for them to move backward to the correct position.  */
	  ptrdiff_t adjustment = (m->insertion_type ? to : from) - charpos;
          if (adjustment)
            {
	      Lisp_Object marker = make_lisp_ptr (m, Lisp_Vectorlike);
              bset_undo_list (current_buffer,
			      Fcons (Fcons (marker, make_fixnum (adjustment)),
				     BVAR (current_buffer, undo_list)));
            }
        }
    }
}

/* Record that a deletion is about to take place, of the characters in
   STRING, at location BEG.  Optionally record adjustments for markers
   in the region STRING occupies in the current buffer.  */
void
undo_push_delete (ptrdiff_t beg, Lisp_Object string, bool record_markers)
{
  if (!EQ (BVAR (current_buffer, undo_list), Qt))
    {
      Lisp_Object sbeg;
      undo_push_point (beg);

      if (PT == beg + SCHARS (string))
	XSETINT (sbeg, -beg);
      else
	XSETFASTINT (sbeg, beg);

      /* primitive-undo assumes marker adjustments are recorded
	 immediately before the deletion is recorded.  See bug 16818
	 discussion.  */
      if (record_markers)
	undo_push_markers (beg, beg + SCHARS (string));

      bset_undo_list (current_buffer,
		      Fcons (Fcons (string, sbeg), BVAR (current_buffer, undo_list)));
    }
}

/* Record that a replacement is about to take place,
   for LENGTH characters at location BEG.
   The replacement must not change the number of characters.  */

void
undo_push_insdel (ptrdiff_t beg, ptrdiff_t length)
{
  undo_push_delete (beg, make_buffer_string (beg, beg + length, true), false);
  undo_push_insert (beg, length);
}

/* Record a change in property PROP (whose old value was VAL)
   for LENGTH characters starting at position BEG in BUFFER.  */

void
undo_push_property (ptrdiff_t beg, ptrdiff_t length,
		    Lisp_Object prop, Lisp_Object value,
		    Lisp_Object buffer)
{
  Lisp_Object lbeg, lend, entry;
  struct buffer *buf = XBUFFER (buffer);
  if (!EQ (BVAR (buf, undo_list), Qt))
    {
      undo_push_maiden ();
      XSETINT (lbeg, beg);
      XSETINT (lend, beg + length);
      entry = Fcons (Qnil, Fcons (prop, Fcons (value, Fcons (lbeg, lend))));
      bset_undo_list (current_buffer,
		      Fcons (entry, BVAR (current_buffer, undo_list)));
    }
}

DEFUN ("undo-boundary", Fundo_boundary, Sundo_boundary, 0, 0, 0,
       doc: /* Mark the end of the current undo batch.  */)
  (void)
{
  if (!EQ (BVAR (current_buffer, undo_list), Qt))
    {
      Lisp_Object tem = Fcar (BVAR (current_buffer, undo_list));
      if (!NILP (tem))
	bset_undo_list (current_buffer,
			Fcons (Qnil, BVAR (current_buffer, undo_list)));

      Fset (Qundo_auto__last_boundary_cause, Qexplicit);
      point_before_last_command_or_undo = PT;
      buffer_before_last_command_or_undo = current_buffer;
    }
  return Qnil;
}

/* During gc, trim undo lists to within `undo-limit' bytes.
   An auxiliary `undo-outer-limit-function' accommodates
   a custom trimming algo.  */

void
truncate_undo_list (struct buffer *b)
{
  intmax_t size_so_far = 0;
  Lisp_Object prev = Qnil, list = BVAR (b, undo_list), next = list;

  /* prevent undo-outer-limit-function recursing into gc.  */
  specpdl_ref count = inhibit_garbage_collection ();

  record_unwind_current_buffer ();
  set_buffer_internal (b);

  if (CONSP (next) && NILP (XCAR (next)))
    {
      /* Skip past initial undo boundary, if any.  */
      size_so_far += sizeof (struct Lisp_Cons);
      prev = next;
      next = XCDR (next);
    }

  /* Iterate until first undo boundary */
  while (CONSP (next) && !NILP (XCAR (next)))
    {
      Lisp_Object elt = XCAR (next);
      size_so_far += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size_so_far += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCAR (elt)))
	    size_so_far += (sizeof (struct Lisp_String) - 1
			    + SCHARS (XCAR (elt)));
	}
      prev = next;
      next = XCDR (next);
    }

  /* We're at the first undo boundary.  Apply the special function
     (typically `undo-outer-limit-truncate') if applicable.  */
  if (!NILP (Vundo_outer_limit_function) && INTEGERP (Vundo_outer_limit))
    {
      intmax_t undo_outer_limit;
      if ((integer_to_intmax (Vundo_outer_limit, &undo_outer_limit)
	   ? size_so_far > undo_outer_limit
	   : NILP (Fnatnump (Vundo_outer_limit)))
	  && !NILP (call1 (Vundo_outer_limit_function, make_int (size_so_far))))
	goto out;
    }

  for (Lisp_Object last_boundary = prev; CONSP (next); )
    {
      Lisp_Object elt = XCAR (next);
      if (NILP (elt)) /* undo boundary */
	{
	  if (size_so_far > undo_strong_limit)
	    {
	      /* Silly RMS safety measure.  */
	      XSETCDR (last_boundary, Qnil);
	      break;
	    }
	  else if (size_so_far > undo_limit)
	    {
	      XSETCDR (prev, Qnil);
	      break;
	    }
	  else
	    last_boundary = prev;
	}

      size_so_far += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size_so_far += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCAR (elt)))
	    size_so_far += (sizeof (struct Lisp_String) - 1
			    + SCHARS (XCAR (elt)));
	}
      prev = next;
      next = XCDR (next);
    }

 out:
  unbind_to (count, Qnil);
}

void
syms_of_undo (void)
{
  DEFSYM (Qinhibit_read_only, "inhibit-read-only");
  DEFSYM (Qundo_auto__last_boundary_cause, "undo-auto--last-boundary-cause");
  DEFSYM (Qexplicit, "explicit");

  /* Marker for function call undo list elements.  */
  DEFSYM (Qapply, "apply");

  defsubr (&Sundo_boundary);

  DEFVAR_INT ("undo-limit", undo_limit,
	      doc: /* Keep no more undo information once it exceeds this size.
This limit is applied when garbage collection happens.
When a previous command increases the total undo list size past this
value, the earlier commands that came before it are forgotten.

The size is counted as the number of bytes occupied,
which includes both saved text and other data.  */);
  undo_limit = 160000;

  DEFVAR_INT ("undo-strong-limit", undo_strong_limit,
	      doc: /* Don't keep more than this much size of undo information.
This limit is applied when garbage collection happens.
When a previous command increases the total undo list size past this
value, that command and the earlier commands that came before it are forgotten.
However, the most recent buffer-modifying command's undo info
is never discarded for this reason.

The size is counted as the number of bytes occupied,
which includes both saved text and other data.  */);
  undo_strong_limit = 240000;

  DEFVAR_LISP ("undo-outer-limit", Vundo_outer_limit,
	      doc: /* Outer limit on size of undo information for one command.
At garbage collection time, if the current command has produced
more than this much undo information, it discards the info and displays
a warning.  This is a last-ditch limit to prevent memory overflow.

The size is counted as the number of bytes occupied, which includes
both saved text and other data.  A value of nil means no limit.  In
this case, accumulating one huge undo entry could make Emacs crash as
a result of memory overflow.

In fact, this calls the function which is the value of
`undo-outer-limit-function' with one argument, the size.
The text above describes the behavior of the function
that variable usually specifies.  */);
  Vundo_outer_limit = make_fixnum (24000000);

  DEFVAR_LISP ("undo-outer-limit-function", Vundo_outer_limit_function,
	       doc: /* Function to call when an undo list exceeds `undo-outer-limit'.
This function is called with one argument, the current undo list size
for the most recent command (since the last undo boundary).
If the function returns t, that means truncation has been fully handled.
If it returns nil, the other forms of truncation are done.

Garbage collection is inhibited around the call to this function,
so it must make sure not to do a lot of consing.  */);
  Vundo_outer_limit_function = Qnil;
}
