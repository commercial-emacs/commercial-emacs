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

/* If an undo run rewinds back to the so-called maiden undo entry, then
   `set-buffer-modified-p' to nil provided nothing changed on disk.  */

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

void
undo_push_insert (ptrdiff_t beg, ptrdiff_t length)
{
  if (!EQ (BVAR (current_buffer, undo_list), Qt))
    {
      Lisp_Object lbeg, lend;
      undo_push_maiden ();

      /* If this is following another insertion and consecutive with it
	 in the buffer, combine the two.  */
      if (CONSP (BVAR (current_buffer, undo_list)))
	{
	  Lisp_Object elt = XCAR (BVAR (current_buffer, undo_list));
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

/* Store the marker's charpos at deletion time for proper restoration
   after reinstating deleted text.  For insertions and deletions
   exclusive of the marker, ordinary marker updates suffice.  */

static void
undo_push_markers (ptrdiff_t from, ptrdiff_t to)
{
  for (struct Lisp_Marker *m = BUF_MARKERS (current_buffer); m; m = m->next)
    {
      ptrdiff_t charpos = m->charpos;
      eassert (charpos <= Z);

      if (from <= charpos && charpos <= to)
        {
          /* insertion_type t/f follows/precedes re-inserted text.  */
	  ptrdiff_t offset = (m->insertion_type ? to : from) - charpos;
          if (offset)
            {
	      Lisp_Object marker = make_lisp_ptr (m, Lisp_Vectorlike);
              bset_undo_list (current_buffer,
			      Fcons (Fcons (marker, make_fixnum (offset)),
				     BVAR (current_buffer, undo_list)));
            }
        }
    }
}

void
undo_push_delete (ptrdiff_t beg, Lisp_Object string, bool record_markers)
{
  if (!EQ (BVAR (current_buffer, undo_list), Qt))
    {
      Lisp_Object sbeg;
      undo_push_maiden ();

      if (PT == beg + SCHARS (string))
	XSETINT (sbeg, -beg);
      else
	XSETFASTINT (sbeg, beg);

      if (record_markers)
	undo_push_markers (beg, beg + SCHARS (string));

      bset_undo_list (current_buffer,
		      Fcons (Fcons (string, sbeg), BVAR (current_buffer, undo_list)));
    }
}

/* The replacement must not change the number of characters.  */

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
  struct buffer *buf = XBUFFER (buffer);
  if (!EQ (BVAR (buf, undo_list), Qt))
    {
      Lisp_Object lbeg, lend, entry;
      undo_push_maiden ();
      XSETINT (lbeg, beg);
      XSETINT (lend, beg + length);
      /* (nil PROP VAL BEG . END) undoes a prop change.  */
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
      if (CONSP (BVAR (current_buffer, undo_list))
	  /* Avoid shit like "nil nil" in undo stack.  */
	  && !EQ (UNDO_BOUNDARY, XCAR (BVAR (current_buffer, undo_list))))
	bset_undo_list (current_buffer,
			Fcons (UNDO_BOUNDARY, BVAR (current_buffer, undo_list)));
    }
  return Qnil;
}

/* During gc, ensure undo stack stays within `undo-limit' bytes.  */

void
undo_truncate (struct buffer *b)
{
  intmax_t size = 0;
  Lisp_Object keep = Qnil, kill = BVAR (b, undo_list);

  /* prevent `undo-outer-limit-function' recursing into gc.  */
  specpdl_ref count = inhibit_garbage_collection ();

  record_unwind_current_buffer ();
  set_buffer_internal (b);

  /* Skip to first non-trivial element.  */
  FOR_EACH_TAIL (kill)
    if (!EQ (XCAR (kill), UNDO_BOUNDARY))
      break;

  bool topmost = true;
  FOR_EACH_TAIL (kill)
    {
      Lisp_Object elt = XCAR (kill);
      if (EQ (elt, UNDO_BOUNDARY))
	{
	  if (topmost)
	    {
	      /* `undo-outer-limit' controls the size of the topmost undo.  */
	      topmost = false;
	      if (INTEGERP (Vundo_outer_limit)
		  && !NILP (Vundo_outer_limit_function))
		{
		  intmax_t undo_outer_limit;
		  integer_to_intmax (Vundo_outer_limit, &undo_outer_limit);
		  if (size > undo_outer_limit
		      && !NILP (call1 (Vundo_outer_limit_function, make_int (size))))
		    break; /* !!! */
		}
	    }

	  if (size > undo_limit)
	    {
	      eassume (!NILP (keep));
	      XSETCDR (keep, Qnil);
	      break; /* !!! */
	    }
	}

      size += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCAR (elt)))
	    size += (sizeof (struct Lisp_String) - 1 + SCHARS (XCAR (elt)));
	}
      keep = kill;
    }

  unbind_to (count, Qnil);
}

void
syms_of_undo (void)
{
  DEFSYM (Qinhibit_read_only, "inhibit-read-only");

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
