/* Buffer manipulation primitives for GNU Emacs.

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

#include <config.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "lisp.h"
#include "intervals.h"
#include "process.h"
#include "systime.h"
#include "window.h"
#include "commands.h"
#include "character.h"
#include "buffer.h"
#include "region-cache.h"
#include "indent.h"
#include "blockinput.h"
#include "keymap.h"
#include "frame.h"
#include "xwidget.h"
#include "itree.h"
#include "pdumper.h"

#ifdef WINDOWSNT
#include "w32heap.h"		/* for mmap_* */
#endif

/* Work around GCC bug 109847
   https://gcc.gnu.org/bugzilla/show_bug.cgi?id=109847
   which causes GCC to mistakenly complain about
   AUTO_STRING with "*scratch*".  */
#if GNUC_PREREQ (13, 0, 0)
# pragma GCC diagnostic ignored "-Wanalyzer-out-of-bounds"
#endif

struct buffer buffer_slot_defaults;
struct buffer buffer_slot_map;
struct buffer buffer_slot_symbols;

#define PER_BUFFER_SYMBOL(OFFSET) \
  (*(Lisp_Object *)((OFFSET) + (char *) &buffer_slot_symbols))

/* Maximum length of an overlay vector.  */
#define OVERLAY_COUNT_MAX						\
  ((ptrdiff_t) min (MOST_POSITIVE_FIXNUM,				\
		    min (PTRDIFF_MAX, SIZE_MAX) / word_size))

/* Flags indicating which built-in buffer-local variables are
   permanent locals.  */
static char buffer_permanent_local_flags[MAX_PER_BUFFER_VARS];

/* Number of per-buffer variables used.  */

static int last_per_buffer_idx;

static void call_overlay_mod_hooks (Lisp_Object list, Lisp_Object overlay,
                                    bool after, Lisp_Object arg1,
                                    Lisp_Object arg2, Lisp_Object arg3);
static void reset_buffer_local_variables (struct buffer *, bool);

/* Alist of buffer names to buffers.  Purposely not manipulable
   (i.e., corruptible) from Lisp.  */
Lisp_Object Vbuffer_alist;

static Lisp_Object QSFundamental;	/* A string "Fundamental".  */

static Lisp_Object extant_indirects;

static void alloc_buffer_text (struct buffer *, ptrdiff_t);
static void free_buffer_text (struct buffer *b);
static void copy_overlays (struct buffer *, struct buffer *);
static void modify_overlay (struct buffer *, ptrdiff_t, ptrdiff_t);
static Lisp_Object buffer_lisp_local_values (struct buffer *, bool);
static Lisp_Object buffer_local_variables_1 (struct buffer *buf, int offset, Lisp_Object sym);

static void
CHECK_OVERLAY (Lisp_Object x)
{
  CHECK_TYPE (OVERLAYP (x), Qoverlayp, x);
}

/* Convert the position POS to an EMACS_INT that fits in a fixnum.
   Yield POS's value if POS is already a fixnum, POS's marker position
   if POS is a marker, and MOST_NEGATIVE_FIXNUM or
   MOST_POSITIVE_FIXNUM if POS is a negative or positive bignum.
   Signal an error if POS is not of the proper form.  */

EMACS_INT
fix_position (Lisp_Object pos)
{
  if (FIXNUMP (pos))
    return XFIXNUM (pos);
  if (MARKERP (pos))
    return marker_position (pos);
  CHECK_TYPE (BIGNUMP (pos), Qinteger_or_marker_p, pos);
  return !NILP (Fnatnump (pos)) ? MOST_POSITIVE_FIXNUM : MOST_NEGATIVE_FIXNUM;
}

/* These setters are used only in this file, so they can be private.
   The public setters are inline functions defined in buffer.h.  */
static void
bset_abbrev_mode (struct buffer *b, Lisp_Object val)
{
  b->abbrev_mode_ = val;
}
static void
bset_abbrev_table (struct buffer *b, Lisp_Object val)
{
  b->abbrev_table_ = val;
}
static void
bset_auto_fill_function (struct buffer *b, Lisp_Object val)
{
  b->auto_fill_function_ = val;
}
static void
bset_auto_save_file_format (struct buffer *b, Lisp_Object val)
{
  b->auto_save_file_format_ = val;
}
static void
bset_auto_save_file_name (struct buffer *b, Lisp_Object val)
{
  b->auto_save_file_name_ = val;
}
static void
bset_backed_up (struct buffer *b, Lisp_Object val)
{
  b->backed_up_ = val;
}
static void
bset_begv_marker (struct buffer *b, Lisp_Object val)
{
  b->begv_marker_ = val;
}
static void
bset_bidi_paragraph_start_re (struct buffer *b, Lisp_Object val)
{
  b->bidi_paragraph_start_re_ = val;
}
static void
bset_bidi_paragraph_separate_re (struct buffer *b, Lisp_Object val)
{
  b->bidi_paragraph_separate_re_ = val;
}
static void
bset_buffer_file_coding_system (struct buffer *b, Lisp_Object val)
{
  b->buffer_file_coding_system_ = val;
}
static void
bset_ctl_arrow (struct buffer *b, Lisp_Object val)
{
  b->ctl_arrow_ = val;
}
static void
bset_cursor_in_non_selected_windows (struct buffer *b, Lisp_Object val)
{
  b->cursor_in_non_selected_windows_ = val;
}
static void
bset_cursor_type (struct buffer *b, Lisp_Object val)
{
  b->cursor_type_ = val;
}
static void
bset_display_table (struct buffer *b, Lisp_Object val)
{
  b->display_table_ = val;
}
static void
bset_extra_line_spacing (struct buffer *b, Lisp_Object val)
{
  b->extra_line_spacing_ = val;
}
static void
bset_file_format (struct buffer *b, Lisp_Object val)
{
  b->file_format_ = val;
}
static void
bset_file_truename (struct buffer *b, Lisp_Object val)
{
  b->file_truename_ = val;
}
static void
bset_fringe_cursor_alist (struct buffer *b, Lisp_Object val)
{
  b->fringe_cursor_alist_ = val;
}
static void
bset_fringe_indicator_alist (struct buffer *b, Lisp_Object val)
{
  b->fringe_indicator_alist_ = val;
}
static void
bset_fringes_outside_margins (struct buffer *b, Lisp_Object val)
{
  b->fringes_outside_margins_ = val;
}
static void
bset_header_line_format (struct buffer *b, Lisp_Object val)
{
  b->header_line_format_ = val;
}
static void
bset_tab_line_format (struct buffer *b, Lisp_Object val)
{
  b->tab_line_format_ = val;
}
static void
bset_indicate_buffer_boundaries (struct buffer *b, Lisp_Object val)
{
  b->indicate_buffer_boundaries_ = val;
}
static void
bset_indicate_empty_lines (struct buffer *b, Lisp_Object val)
{
  b->indicate_empty_lines_ = val;
}
static void
bset_invisibility_spec (struct buffer *b, Lisp_Object val)
{
  b->invisibility_spec_ = val;
}
static void
bset_left_fringe_width (struct buffer *b, Lisp_Object val)
{
  b->left_fringe_width_ = val;
}
static void
bset_major_mode (struct buffer *b, Lisp_Object val)
{
  b->major_mode_ = val;
}
static void
bset_local_minor_modes (struct buffer *b, Lisp_Object val)
{
  b->local_minor_modes_ = val;
}
static void
bset_mark (struct buffer *b, Lisp_Object val)
{
  b->mark_ = val;
}
static void
bset_mode_line_format (struct buffer *b, Lisp_Object val)
{
  b->mode_line_format_ = val;
}
static void
bset_mode_name (struct buffer *b, Lisp_Object val)
{
  b->mode_name_ = val;
}
static void
bset_name (struct buffer *b, Lisp_Object val)
{
  b->name_ = val;
}
static void
bset_last_name (struct buffer *b, Lisp_Object val)
{
  b->last_name_ = val;
}
static void
bset_overwrite_mode (struct buffer *b, Lisp_Object val)
{
  b->overwrite_mode_ = val;
}
static void
bset_pt_marker (struct buffer *b, Lisp_Object val)
{
  b->pt_marker_ = val;
}
static void
bset_right_fringe_width (struct buffer *b, Lisp_Object val)
{
  b->right_fringe_width_ = val;
}
static void
bset_save_length (struct buffer *b, Lisp_Object val)
{
  b->save_length_ = val;
}
static void
bset_scroll_bar_width (struct buffer *b, Lisp_Object val)
{
  b->scroll_bar_width_ = val;
}
static void
bset_scroll_bar_height (struct buffer *b, Lisp_Object val)
{
  b->scroll_bar_height_ = val;
}
static void
bset_scroll_down_aggressively (struct buffer *b, Lisp_Object val)
{
  b->scroll_down_aggressively_ = val;
}
static void
bset_scroll_up_aggressively (struct buffer *b, Lisp_Object val)
{
  b->scroll_up_aggressively_ = val;
}
static void
bset_selective_display (struct buffer *b, Lisp_Object val)
{
  b->selective_display_ = val;
}
static void
bset_selective_display_ellipses (struct buffer *b, Lisp_Object val)
{
  b->selective_display_ellipses_ = val;
}
static void
bset_vertical_scroll_bar_type (struct buffer *b, Lisp_Object val)
{
  b->vertical_scroll_bar_type_ = val;
}
static void
bset_horizontal_scroll_bar_type (struct buffer *b, Lisp_Object val)
{
  b->horizontal_scroll_bar_type_ = val;
}
static void
bset_word_wrap (struct buffer *b, Lisp_Object val)
{
  b->word_wrap_ = val;
}
static void
bset_zv_marker (struct buffer *b, Lisp_Object val)
{
  b->zv_marker_ = val;
}

void
nsberror (Lisp_Object spec)
{
  if (STRINGP (spec))
    error ("No buffer named %s", SDATA (spec));
  error ("Invalid buffer argument");
}

DEFUN ("buffer-monospace-p", Fbuffer_monospace_p, Sbuffer_monospace_p, 0, 1, 0,
       doc: /* Return t if BUFFER contains only homogenous text.
By homogenous, we mean only characters of the frame's default font and no
width-varying glyphs such as invisible text or images.  */)
  (Lisp_Object buffer)
{
  if (NILP (buffer))
    buffer = Fcurrent_buffer();
  CHECK_BUFFER (buffer);
  return XBUFFER (buffer)->text->monospace ? Qt : Qnil;
}

DEFUN ("buffer-live-p", Fbuffer_live_p, Sbuffer_live_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a buffer which has not been killed.
Value is nil if OBJECT is not a buffer or if it has been killed.  */)
  (Lisp_Object object)
{
  return ((BUFFERP (object) && BUFFER_LIVE_P (XBUFFER (object)))
	  ? Qt : Qnil);
}

DEFUN ("buffer-list", Fbuffer_list, Sbuffer_list, 0, 1, 0,
       doc: /* Return a list of all live buffers.
If the optional arg FRAME is a frame, return the buffer list in the
proper order for that frame: the buffers shown in FRAME come first,
followed by the rest of the buffers.  */)
  (Lisp_Object frame)
{
  Lisp_Object general;
  general = Fmapcar (Qcdr, Vbuffer_alist);

  if (FRAMEP (frame))
    {
      Lisp_Object framelist, prevlist, tail;

      framelist = Fcopy_sequence (XFRAME (frame)->buffer_list);
      prevlist = Fnreverse (Fcopy_sequence
			    (XFRAME (frame)->buried_buffer_list));

      /* Remove from GENERAL any buffer that duplicates one in
         FRAMELIST or PREVLIST.  */
      tail = framelist;
      while (CONSP (tail))
	{
	  general = Fdelq (XCAR (tail), general);
	  tail = XCDR (tail);
	}
      tail = prevlist;
      while (CONSP (tail))
	{
	  general = Fdelq (XCAR (tail), general);
	  tail = XCDR (tail);
	}

      return CALLN (Fnconc, framelist, general, prevlist);
    }
  else
    return general;
}

/* Like Fassoc, but use Fstring_equal to compare
   (which ignores text properties), and don't ever quit.  */

static Lisp_Object
assoc_ignore_text_properties (Lisp_Object key, Lisp_Object list)
{
  Lisp_Object tail;
  for (tail = list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt = XCAR (tail);
      if (!NILP (Fstring_equal (Fcar (elt), key)))
	return elt;
    }
  return Qnil;
}

DEFUN ("get-buffer", Fget_buffer, Sget_buffer, 1, 1, 0,
       doc: /* Return the buffer named BUFFER-OR-NAME.
BUFFER-OR-NAME must be either a string or a buffer.  If BUFFER-OR-NAME
is a string and there is no buffer with that name, return nil.  If
BUFFER-OR-NAME is a buffer, return it as given.  */)
  (register Lisp_Object buffer_or_name)
{
  if (BUFFERP (buffer_or_name))
    return buffer_or_name;
  CHECK_STRING (buffer_or_name);

  return Fcdr (assoc_ignore_text_properties (buffer_or_name, Vbuffer_alist));
}

DEFUN ("get-file-buffer", Fget_file_buffer, Sget_file_buffer, 1, 1, 0,
       doc: /* Return the buffer visiting file FILENAME (a string).
The buffer's `buffer-file-name' must match exactly the expansion of FILENAME.
If there is no such live buffer, return nil.
See also `find-buffer-visiting'.  */)
  (register Lisp_Object filename)
{
  register Lisp_Object tail, buf, handler;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file name handler.  */
  handler = Ffind_file_name_handler (filename, Qget_file_buffer);
  if (!NILP (handler))
    {
      Lisp_Object handled_buf = call2 (handler, Qget_file_buffer,
				       filename);
      return BUFFERP (handled_buf) ? handled_buf : Qnil;
    }

  FOR_EACH_LIVE_BUFFER (tail, buf)
    {
      if (!STRINGP (BVAR (XBUFFER (buf), filename))) continue;
      if (!NILP (Fstring_equal (BVAR (XBUFFER (buf), filename), filename)))
	return buf;
    }
  return Qnil;
}

DEFUN ("get-truename-buffer", Fget_truename_buffer, Sget_truename_buffer, 1, 1, 0,
       doc: /* Return the buffer with `file-truename' equal to FILENAME (a string).
If there is no such live buffer, return nil.
See also `find-buffer-visiting'.  */)
  (register Lisp_Object filename)
{
  register Lisp_Object tail, buf;

  FOR_EACH_LIVE_BUFFER (tail, buf)
    {
      if (!STRINGP (BVAR (XBUFFER (buf), file_truename))) continue;
      if (!NILP (Fstring_equal (BVAR (XBUFFER (buf), file_truename), filename)))
	return buf;
    }
  return Qnil;
}

DEFUN ("find-buffer", Ffind_buffer, Sfind_buffer, 2, 2, 0,
       doc: /* Return the buffer with buffer-local VARIABLE `equal' to VALUE.
If there is no such live buffer, return nil.
See also `find-buffer-visiting'.  */)
  (Lisp_Object variable, Lisp_Object value)
{
  register Lisp_Object tail, buf;

  FOR_EACH_LIVE_BUFFER (tail, buf)
    {
      if (!NILP (Fequal (value, Fbuffer_local_value (variable, buf))))
	return buf;
    }
  return Qnil;
}

static void
run_buffer_list_update_hook (struct buffer *buf)
{
  eassert (buf);
  if (!buf->inhibit_buffer_hooks)
    {
      Lisp_Object args [] = { Qbuffer_list_update_hook };
      Frun_hooks (1, args);
    }
}

DEFUN ("get-buffer-create", Fget_buffer_create, Sget_buffer_create, 1, 2, 0,
       doc: /* Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.
If BUFFER-OR-NAME is a string and a live buffer with that name exists,
return that buffer.  If no such buffer exists, create a new buffer with
that name and return it.

If BUFFER-OR-NAME starts with a space, the new buffer does not keep undo
information.  If optional argument INHIBIT-BUFFER-HOOKS is non-nil, the
new buffer does not run the hooks `kill-buffer-hook',
`kill-buffer-query-functions', and `buffer-list-update-hook'.  This
avoids slowing down internal or temporary buffers that are never
presented to users or passed on to other applications.

If BUFFER-OR-NAME is a buffer instead of a string, return it as given,
even if it is dead.  The return value is never nil.  */)
  (register Lisp_Object buffer_or_name, Lisp_Object inhibit_buffer_hooks)
{
  register Lisp_Object buffer, name;
  register struct buffer *b;

  buffer = Fget_buffer (buffer_or_name);
  if (!NILP (buffer))
    return buffer;

  if (SCHARS (buffer_or_name) == 0)
    error ("Empty string for buffer name is not allowed");

  b = allocate_buffer ();

  /* An ordinary buffer uses its own struct buffer_text.  */
  b->text = &b->own_text;
  b->base_buffer = NULL;
  /* No one shares the text with us now.  */
  b->indirections = 0;
  /* No one shows us now.  */
  b->window_count = 0;

  memset (&b->local_flags, 0, sizeof (b->local_flags));

  BUF_GAP_SIZE (b) = 20;
  block_input ();
  /* We allocate extra 1-byte at the tail and keep it always '\0' for
     anchoring a search.  */
  alloc_buffer_text (b, BUF_GAP_SIZE (b) + 1);
  unblock_input ();
  if (!BUF_BEG_ADDR (b))
    memory_full (BUF_GAP_SIZE (b) + 1);

  b->pt = BEG;
  b->begv = BEG;
  b->zv = BEG;
  b->pt_byte = BEG_BYTE;
  b->begv_byte = BEG_BYTE;
  b->zv_byte = BEG_BYTE;

  BUF_GPT (b) = BEG;
  BUF_GPT_BYTE (b) = BEG_BYTE;

  BUF_Z (b) = BEG;
  BUF_Z_BYTE (b) = BEG_BYTE;
  BUF_MODIFF (b) = 1;
  BUF_CHARS_MODIFF (b) = 1;
  BUF_OVERLAY_MODIFF (b) = 1;
  BUF_SAVE_MODIFF (b) = 1;
  BUF_COMPACT (b) = 1;
  set_buffer_intervals (b, NULL);
  BUF_UNCHANGED_MODIFIED (b) = 1;
  BUF_OVERLAY_UNCHANGED_MODIFIED (b) = 1;
  BUF_END_UNCHANGED (b) = 0;
  BUF_BEG_UNCHANGED (b) = 0;
  *(BUF_GPT_ADDR (b)) = *(BUF_Z_ADDR (b)) = 0; /* Put an anchor '\0'.  */
  b->text->inhibit_shrinking = false;
  b->text->redisplay = false;
  b->text->monospace = true;

  b->newline_cache = 0;
  b->width_run_cache = 0;
  b->bidi_paragraph_cache = 0;
  bset_width_table (b, Qnil);
  b->prevent_redisplay_optimizations_p = 1;

  /* An ordinary buffer normally doesn't need markers
     to handle BEGV and ZV.  */
  bset_pt_marker (b, Qnil);
  bset_begv_marker (b, Qnil);
  bset_zv_marker (b, Qnil);

  name = Fcopy_sequence (buffer_or_name);
  set_string_intervals (name, NULL);
  bset_name (b, name);
  bset_last_name (b, name);

  b->inhibit_buffer_hooks = !NILP (inhibit_buffer_hooks);
  bset_undo_list (b, SREF (name, 0) != ' ' ? Qnil : Qt);

  reset_buffer (b);
  reset_buffer_local_variables (b, 1);

  bset_mark (b, Fmake_marker ());
  BUF_MARKERS (b) = NULL;

  /* Enter into global register.  */
  XSETBUFFER (buffer, b);
  Vbuffer_alist = nconc2 (Vbuffer_alist, list1 (Fcons (name, buffer)));

  run_buffer_list_update_hook (b);

  return buffer;
}

static void
add_buffer_overlay (struct buffer *b, struct Lisp_Overlay *ov,
                    ptrdiff_t begin, ptrdiff_t end)
{
  eassert (!ov->buffer);
  if (!b->overlays)
    b->overlays = itree_create ();
  ov->buffer = b;
  itree_insert (b->overlays, ov->interval, begin, end);
}

/* Copy overlays of buffer FROM to buffer TO.  */

static void
copy_overlays (struct buffer *from, struct buffer *to)
{
  eassert (to && !to->overlays);
  struct itree_node *node;

  ITREE_FOREACH (node, from->overlays, PTRDIFF_MIN, PTRDIFF_MAX, ASCENDING)
    {
      Lisp_Object ov = node->data;
      Lisp_Object copy = build_overlay (node->front_advance,
					node->rear_advance,
					Fcopy_sequence (OVERLAY_PLIST (ov)),
					node->on_enter,
					node->on_exit);
      add_buffer_overlay (to, XOVERLAY (copy), node->begin, node->end);
    }
}

bool
valid_per_buffer_idx (int idx)
{
  return 0 <= idx && idx < last_per_buffer_idx;
}

/* Clone per-buffer values of buffer FROM.

   Buffer TO gets the same per-buffer values as FROM, with the
   following exceptions: (1) TO's name is left untouched, (2) markers
   are copied and made to refer to TO, and (3) overlay lists are
   copied.  */

static void
clone_per_buffer_values (struct buffer *from, struct buffer *to)
{
  int offset;

  FOR_EACH_PER_BUFFER_OBJECT_AT (offset)
    {
      Lisp_Object obj;

      /* Don't touch the `name' which should be unique for every buffer.  */
      if (offset == PER_BUFFER_VAR_OFFSET (name))
	continue;

      obj = per_buffer_value (from, offset);
      if (MARKERP (obj) && XMARKER (obj)->buffer == from)
	{
	  struct Lisp_Marker *m = XMARKER (obj);

	  obj = build_marker (to, m->charpos, m->bytepos);
	  XMARKER (obj)->insertion_type = m->insertion_type;
	}

      set_per_buffer_value (to, offset, obj);
    }

  memcpy (to->local_flags, from->local_flags, sizeof to->local_flags);

  copy_overlays (from, to);

  /* Get (a copy of) the alist of Lisp-level local variables of FROM
     and install that in TO.  */
  bset_local_val_alist (to, buffer_lisp_local_values (from, 1));
}


/* Store PT, BEGV, and ZV in the markers created for this purpose.  */

static void
record_buffer_markers (struct buffer *b)
{
  if (!NILP (BVAR (b, pt_marker)))
    {
      Lisp_Object buffer;

      eassert (!NILP (BVAR (b, begv_marker)));
      eassert (!NILP (BVAR (b, zv_marker)));

      XSETBUFFER (buffer, b);
      set_marker_both (BVAR (b, pt_marker), buffer, b->pt, b->pt_byte);
      set_marker_both (BVAR (b, begv_marker), buffer, b->begv, b->begv_byte);
      set_marker_both (BVAR (b, zv_marker), buffer, b->zv, b->zv_byte);
    }
}


/* Set PT, BEGV, and ZV to values stored in record_buffer_markers.  */

static void
fetch_buffer_markers (struct buffer *b)
{
  if (!NILP (BVAR (b, pt_marker)))
    {
      Lisp_Object m;

      eassert (!NILP (BVAR (b, begv_marker)));
      eassert (!NILP (BVAR (b, zv_marker)));

      m = BVAR (b, pt_marker);
      SET_BUF_PT_BOTH (b, marker_position (m), marker_byte_position (m));

      m = BVAR (b, begv_marker);
      SET_BUF_BEGV_BOTH (b, marker_position (m), marker_byte_position (m));

      m = BVAR (b, zv_marker);
      SET_BUF_ZV_BOTH (b, marker_position (m), marker_byte_position (m));
    }
}

DEFUN ("make-indirect-buffer", Fmake_indirect_buffer, Smake_indirect_buffer,
       2, 4,
       "bMake indirect buffer (to buffer): \nBName of indirect buffer: ",
       doc: /* Create and return an indirect buffer for buffer BASE-BUFFER, named NAME.
BASE-BUFFER should be a live buffer, or the name of an existing buffer.

NAME should be a string which is not the name of an existing buffer.

Interactively, prompt for BASE-BUFFER (offering the current buffer as
the default), and for NAME (offering as default the name of a recently
used buffer).

Optional argument CLONE non-nil means preserve BASE-BUFFER's state,
such as major and minor modes, in the indirect buffer.
CLONE nil means the indirect buffer's state is reset to default values.

If optional argument INHIBIT-BUFFER-HOOKS is non-nil, the new buffer
does not run the hooks `kill-buffer-hook',
`kill-buffer-query-functions', and `buffer-list-update-hook'.

Interactively, CLONE and INHIBIT-BUFFER-HOOKS are nil.  */)
  (Lisp_Object base_buffer, Lisp_Object name, Lisp_Object clone,
   Lisp_Object inhibit_buffer_hooks)
{
  Lisp_Object buf, tem;
  struct buffer *b;

  CHECK_STRING (name);
  buf = Fget_buffer (name);
  if (!NILP (buf))
    error ("Buffer name `%s' is in use", SDATA (name));

  tem = base_buffer;
  base_buffer = Fget_buffer (base_buffer);
  if (NILP (base_buffer))
    error ("No such buffer: `%s'", SDATA (tem));
  if (!BUFFER_LIVE_P (XBUFFER (base_buffer)))
    error ("Base buffer has been killed");

  if (SCHARS (name) == 0)
    error ("Empty string for buffer name is not allowed");

  b = allocate_buffer ();

  /* Ensure base buffer is not itself indirect.  */
  b->base_buffer = (XBUFFER (base_buffer)->base_buffer
		    ? XBUFFER (base_buffer)->base_buffer
		    : XBUFFER (base_buffer));

  /* Use the base buffer's text object.  */
  b->text = b->base_buffer->text;
  /* We have no own text.  */
  b->indirections = -1;
  /* Notify base buffer that we share the text now.  */
  b->base_buffer->indirections++;
  /* Always -1 for an indirect buffer.  */
  b->window_count = -1;

  memset (&b->local_flags, 0, sizeof (b->local_flags));

  b->pt = b->base_buffer->pt;
  b->begv = b->base_buffer->begv;
  b->zv = b->base_buffer->zv;
  b->pt_byte = b->base_buffer->pt_byte;
  b->begv_byte = b->base_buffer->begv_byte;
  b->zv_byte = b->base_buffer->zv_byte;
  b->inhibit_buffer_hooks = !NILP (inhibit_buffer_hooks);

  b->newline_cache = 0;
  b->width_run_cache = 0;
  b->bidi_paragraph_cache = 0;
  bset_width_table (b, Qnil);

  name = Fcopy_sequence (name);
  set_string_intervals (name, NULL);
  bset_name (b, name);
  bset_last_name (b, name);

  /* An indirect buffer shares undo list of its base (Bug#18180).  */
  bset_undo_list (b, BVAR (b->base_buffer, undo_list));

  reset_buffer (b);
  reset_buffer_local_variables (b, 1);

  /* Put this in the alist of all live buffers.  */
  XSETBUFFER (buf, b);
  Vbuffer_alist = nconc2 (Vbuffer_alist, list1 (Fcons (name, buf)));

  bset_mark (b, Fmake_marker ());

  /* The multibyte status belongs to the base buffer.  */
  bset_enable_multibyte_characters
    (b, BVAR (b->base_buffer, enable_multibyte_characters));

  /* Make sure the base buffer has markers for its narrowing.  */
  if (NILP (BVAR (b->base_buffer, pt_marker)))
    {
      eassert (NILP (BVAR (b->base_buffer, begv_marker)));
      eassert (NILP (BVAR (b->base_buffer, zv_marker)));

      bset_pt_marker (b->base_buffer,
		      build_marker (b->base_buffer, b->base_buffer->pt,
				    b->base_buffer->pt_byte));

      bset_begv_marker (b->base_buffer,
			build_marker (b->base_buffer, b->base_buffer->begv,
				      b->base_buffer->begv_byte));

      bset_zv_marker (b->base_buffer,
		      build_marker (b->base_buffer, b->base_buffer->zv,
				    b->base_buffer->zv_byte));

      XMARKER (BVAR (b->base_buffer, zv_marker))->insertion_type = 1;
    }

  if (NILP (clone))
    {
      /* Give the indirect buffer markers for its narrowing.  */
      bset_pt_marker (b, build_marker (b, b->pt, b->pt_byte));
      bset_begv_marker (b, build_marker (b, b->begv, b->begv_byte));
      bset_zv_marker (b, build_marker (b, b->zv, b->zv_byte));
      XMARKER (BVAR (b, zv_marker))->insertion_type = 1;
    }
  else
    {
      struct buffer *old_b = current_buffer;

      clone_per_buffer_values (b->base_buffer, b);
      bset_filename (b, Qnil);
      bset_file_truename (b, Qnil);
      bset_display_count (b, make_fixnum (0));
      bset_backed_up (b, Qnil);
      bset_local_minor_modes (b, Qnil);
      bset_auto_save_file_name (b, Qnil);
      set_buffer_internal (b);
      Fset (intern ("buffer-save-without-query"), Qnil);
      Fset (intern ("buffer-file-number"), Qnil);
      if (!NILP (Flocal_variable_p (Qbuffer_stale_function, base_buffer)))
	Fkill_local_variable (Qbuffer_stale_function);
      /* Cloned buffers need extra setup, to do things such as deep
	 variable copies for list variables that might be mangled due
	 to destructive operations in the indirect buffer. */
      run_hook (Qclone_indirect_buffer_hook);
      set_buffer_internal (old_b);
    }

  run_buffer_list_update_hook (b);

  return buf;
}

static void
unwind_extant_indirects (void)
{
  extant_indirects = Qnil;
}

static void
restore_undo_list (Lisp_Object arg)
{
  bset_undo_list (XBUFFER (XCAR (arg)), XCDR (arg));
}

static void
restore_proximity (Lisp_Object restore)
{
  struct buffer *buf = XBUFFER (XCAR (restore));
  restore = XCDR (restore);
  buf->proximity->current = XCAR (restore);
  restore = XCDR (restore);
  buf->proximity->preceding = XCAR (restore);
  restore = XCDR (restore);
  buf->proximity->following = XCAR (restore);
}

/* Make read-only newline at (END - 1).  If (END - 1) isn't already a
   newline, insert one and return new END.  */

static ptrdiff_t
mode_overlay_insert_bumpguard (struct buffer *buf, ptrdiff_t end)
{
  Lisp_Object b; XSETBUFFER (b, buf);
  specpdl_ref count = SPECPDL_INDEX ();

  record_unwind_protect (restore_undo_list, Fcons (b, BVAR (buf, undo_list)));
  bset_undo_list (buf, Qt);

  record_unwind_protect (restore_proximity,
			 list4 (b,
				buf->proximity->current,
				buf->proximity->preceding,
				buf->proximity->following));
  if (end == BUF_BEG (buf)
      || !EQ (Fchar_before (make_fixnum (end)), make_fixnum (10)))
    {
      /* Patch in newline if one not already at END.  */
      SET_PT (end++);
      insert_char (10);
    }
  buf->proximity->current = Qnil;
  buf->proximity->preceding = build_marker (current_buffer, end - 1, CHAR_TO_BYTE (end - 1));
  buf->proximity->following = build_marker (current_buffer, end, CHAR_TO_BYTE (end));

  /* Make the demarcating newline read-only.  */
  Fput_text_property (make_fixnum (end - 1), make_fixnum (end),
		      Qrear_nonsticky, list1 (Qread_only), Qnil);
  Fput_text_property (make_fixnum (end - 1), make_fixnum (end),
		      Qread_only, Qt, Qnil);
  unbind_to (count, Qnil);
  return end;
}

/* Remove read-only bumpguard to mode overlay.  */

static void
mode_overlay_delete_bumpguard (struct buffer *buf, const ptrdiff_t pos)
{
  Lisp_Object b; XSETBUFFER (b, buf);
  specpdl_ref count = SPECPDL_INDEX ();

  specbind (Qinhibit_read_only, Qt);
  specbind (Qinhibit_modification_hooks, Qt);
  record_unwind_protect (restore_undo_list, Fcons (b, BVAR (buf, undo_list)));
  bset_undo_list (buf, Qt);

  record_unwind_protect (restore_proximity,
			 list4 (b,
				buf->proximity->current,
				buf->proximity->preceding,
				buf->proximity->following));
  buf->proximity->current = Qnil;
  buf->proximity->preceding = build_marker (current_buffer, pos, CHAR_TO_BYTE (pos));
  buf->proximity->following = build_marker (current_buffer, pos + 1, CHAR_TO_BYTE (pos + 1));
  Fremove_list_of_text_properties (make_fixnum (pos),
				   make_fixnum (pos + 1),
				   list2 (Qrear_nonsticky, Qread_only), b);
  unbind_to (count, Qnil);
}

static void
mode_overlay_switch_to_buffer (Lisp_Object buf)
{
  ptrdiff_t newpt = clip_to_bounds (BUF_BEG (XBUFFER (buf)), PT,
				    BUF_Z (XBUFFER (buf)));
  set_internal (Qtemporary_goal_column,
		find_symbol_value (XSYMBOL (Qtemporary_goal_column), NULL),
		buf, SET_INTERNAL_SET);
  Lisp_Object wstart = !NILP (Fpos_visible_in_window_p (Qnil, Qnil, Qnil))
    ? Fwindow_start (Qnil)
    : Qnil;
  Lisp_Object mark = Qnil;
  if (!NILP (BVAR (current_buffer, mark_active)))
    {
      /* Mark state is a mess between mark, mark-active,
	 transient-mark-mode, mark-even-if-active, etc.  */
      mark = Fcopy_marker (BVAR (current_buffer, mark), Qnil);
      call1 (intern ("set-mark"), Qnil);
    }

  CALLN (Ffuncall, intern ("switch-to-buffer"), buf, Qt);
  SET_PT (newpt);
  if (MARKERP (mark))
    call1 (intern ("set-mark"), Fmarker_position (mark));
  if (!NILP (wstart))
    Fset_window_start (Qnil, wstart, Qnil);

  if (current_buffer->proximity != NULL)
    {
      /* previous_overlay_change scans [BEGV, arg), ergo +1.  */
      const ptrdiff_t previous = previous_overlay_change (PT + 1, true),
	next = next_overlay_change (PT, true);
      current_buffer->proximity->preceding
	= build_marker (XBUFFER (buf), previous, CHAR_TO_BYTE (previous));
      current_buffer->proximity->following =
	(next == ZV && !XBUFFER (buf)->base_buffer)
	? Qnil /* MODE_OVERLAY_BASE_P grows without limit.  */
	: build_marker (XBUFFER (buf), next, CHAR_TO_BYTE (next));
    }
}

/* True if B can be used as 'other-than-BUFFER' buffer.  */

static bool
candidate_buffer (Lisp_Object b, Lisp_Object buffer)
{
  return (BUFFERP (b) && !EQ (b, buffer)
	  && BUFFER_LIVE_P (XBUFFER (b))
	  /* And not within BUFFER's multi lang family */
	  && !(BUFFERP (buffer)
	       && XBUFFER (buffer)->overlays != NULL
	       && XBUFFER (buffer)->overlays == XBUFFER (b)->overlays)
	  /* And not hidden, or if it is, that it's a multi lang buffer */
	  && (!BUFFER_HIDDEN_P (XBUFFER (b))
	      || MODE_OVERLAY_INDIRECT_P (XBUFFER (b))));
}

DEFUN ("mode-overlay--enter-buffer", Fmake_mode_overlay__enter_buffer,
       Smake_mode_overlay__enter_buffer, 2, 2, 0,
       doc: /* Switch to indirect BUF of the just entered overlay OV.  */)
  (Lisp_Object buf, Lisp_Object ov)
{
  if (NILP (Fmemq (ov, current_buffer->proximity->current)))
    current_buffer->proximity->current = Fcons (ov, current_buffer->proximity->current);
  Lisp_Object base = Fbuffer_base_buffer (buf);
  if (!NILP (base))
    Fset_window_prev_buffers (Qnil, call2 (Qassq_delete_all, base,
					   Fwindow_prev_buffers (Qnil)));
  mode_overlay_switch_to_buffer (buf);
  return buf;
}

DEFUN ("mode-overlay--exit-buffer", Fmake_mode_overlay__exit_buffer,
       Smake_mode_overlay__exit_buffer, 2, 2, 0,
       doc: /* Switch to base BUF of the just exited overlay OV.  */)
  (Lisp_Object buf, Lisp_Object ov)
{
  current_buffer->proximity->current = Fdelq (ov, current_buffer->proximity->current);
  mode_overlay_switch_to_buffer (buf);
  return buf;
}

static Lisp_Object
make_mode__overlay_error (Lisp_Object error_val)
{
  struct buffer *base = current_buffer->base_buffer
    ? current_buffer->base_buffer
    : current_buffer;
  if (NILP (extant_indirects))
    {
      base->proximity = NULL;
      call0 (Qdelete_all_mode_overlays);
      /* Since faulty overlay never added, possibly more undoing to do */
    }

  Lisp_Object tail, other;
  FOR_EACH_LIVE_BUFFER (tail, other)
    if (XBUFFER (other)->base_buffer == base
	&& MODE_OVERLAY_INDIRECT_P (XBUFFER (other))
	&& NILP (Fmemq (other, extant_indirects)))
      {
	XBUFFER (other)->proximity = NULL; /* otherwise Fkill_buffer infloops */
	Fkill_buffer (other);
      }
  xsignal1 (XCAR (error_val), XCDR (error_val));
}

static Lisp_Object
make_mode__overlay (Lisp_Object args)
{
  Lisp_Object beg = XCAR (args);
  Lisp_Object end = XCAR (XCDR (args));
  Lisp_Object mode = XCAR (XCDR (XCDR (args)));

  CHECK_FIXNUM_COERCE_MARKER (beg);
  CHECK_FIXNUM_COERCE_MARKER (end);
  CHECK_TYPE (FUNCTIONP (mode), Qfunctionp, mode);

  if (current_buffer->base_buffer)
    xsignal1 (Quser_error, build_string ("Only permitted in base buffer"));

  Lisp_Object extant = Foverlays_in (beg, end);
  FOR_EACH_TAIL (extant)
    {
      if (!NILP (plist_get (OVERLAY_PLIST (XCAR (extant)), Qmode_overlay_p)))
	xsignal1 (Quser_error, build_string ("Overlapping mode overlays"));
    }

  struct buffer *base = current_buffer;

  if (EQ (BVAR (base, major_mode), mode))
    xsignal1 (Quser_error, concat2 (build_string ("Base buffer already "),
				    SYMBOL_NAME (mode)));

  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_protect_excursion ();
  set_buffer_internal (base);
  call0 (Qdeactivate_mark);
  call0 (Qmode_overlay_filter_buffer_substring_function);
  call4 (Qadd_hook, Qwindow_buffer_change_functions,
	 Qmode_overlay_on_switch_to_buffer,
	 Qnil, Qt);
  call4 (Qadd_hook, Qbefore_revert_hook,
	 Qdelete_all_mode_overlays,
	 Qnil, Qt);
  call4 (Qadd_hook, Qchange_major_mode_hook,
	 Qdelete_all_mode_overlays,
	 Qnil, Qt);

  if (!EQ (Qunbound, find_symbol_value
	   (XSYMBOL (Qhl_line_sticky_flag), base)))
    {
      if (FUNCTIONP (Qhl_line_unhighlight))
	call0 (Qhl_line_unhighlight);
      Fmake_local_variable (Qhl_line_sticky_flag);
      set_internal (Qhl_line_sticky_flag, Qnil, Fcurrent_buffer (), SET_INTERNAL_SET);
    }

  if (base->overlays == NULL)
    base->overlays = itree_create ();

  if (base->proximity == NULL)
    base->proximity = (struct proximity *) xmalloc (sizeof *base->proximity);

  /* Reset proximity with crossed boundaries.  */
  base->proximity->current = Qnil;
  base->proximity->preceding = Fpoint_max_marker ();
  base->proximity->following = Fpoint_min_marker ();

  Lisp_Object tail, xcar, buf = Qnil;
  bool mode_called = false;
  FOR_EACH_LIVE_BUFFER (tail, xcar)
    if (XBUFFER (xcar)->base_buffer == base
	&& EQ (BVAR (XBUFFER (xcar), major_mode), mode))
      {
	/* reuse already extant for MODE.  */
	mode_called = true;
	buf = xcar;
	break;
      }

  if (NILP (buf))
    {
      buf = Fmake_indirect_buffer
	(Fcurrent_buffer (),
	 concat2 (concat2 (build_string (" "), BVAR (base, name)),
		  concat3 (build_string ("["), SYMBOL_NAME (mode), build_string ("]"))),
	 Qnil, Qt);
      XBUFFER (buf)->overlays = base->overlays;
      XBUFFER (buf)->proximity = base->proximity;

      specpdl_ref back = SPECPDL_INDEX ();
      record_unwind_protect_excursion ();
      set_buffer_internal (XBUFFER (buf));
      call0 (Qmode_overlay_filter_buffer_substring_function);
      if (!EQ (Qunbound, find_symbol_value
	       (XSYMBOL (Qhl_line_sticky_flag), XBUFFER (buf))))
	{
	  Fmake_local_variable (Qhl_line_sticky_flag);
	  set_internal (Qhl_line_sticky_flag, Qnil, buf, SET_INTERNAL_SET);
	}
      unbind_to (back, Qnil);
    }

  struct Lisp_Subr *sname = &Smake_mode_overlay__enter_buffer.s;
  Lisp_Object callback = intern_c_string (sname->symbol_name);
  Lisp_Object on_enter = CALLN (Ffuncall, intern ("apply-partially"),
				callback,
				buf);
  sname = &Smake_mode_overlay__exit_buffer.s;
  callback = intern_c_string (sname->symbol_name);
  Lisp_Object on_exit = CALLN (Ffuncall, intern ("apply-partially"),
			       callback,
			       Fcurrent_buffer ());
  Lisp_Object ov = build_overlay (false, false, Qnil, on_enter, on_exit);
  ptrdiff_t obeg = clip_to_bounds (BUF_BEG (base), XFIXNUM (beg), BUF_Z (base));
  ptrdiff_t oend = clip_to_bounds (BUF_BEG (base), XFIXNUM (end), BUF_Z (base));

  const ptrdiff_t obeg1 = mode_overlay_insert_bumpguard (base, obeg);
  oend += (obeg1 - obeg);
  obeg = obeg1;
  oend = mode_overlay_insert_bumpguard (base, oend);

  /* Read-only newline is part of overlay */
  add_buffer_overlay (XBUFFER (buf), XOVERLAY (ov), obeg, oend);

  Lisp_Object face_alist = find_symbol_value (XSYMBOL (Qmode_overlay_face_alist), NULL);
  Lisp_Object face = CDR_SAFE (Fassq (mode, face_alist));
  if (CONSP (face))
    face = XCAR (face);
  if (NILP (Finternal_lisp_face_p (face, Qnil)))
    face = Qmode_overlay;
  Foverlay_put (ov, Qface, face);
  Foverlay_put (ov, Qmode_overlay_p, Qt);

  SET_PT (obeg);
  /* sets proximity */
  call1 (on_enter, ov);
  /* will fontify unless noninteractive */
  if (!mode_called)
    call0 (mode);
  /* insurance fontify for noninteractive */
  call2 (Qfont_lock_fontify_region, beg, end);

  /* Because MODE kills all local variables, we do this here.  */
  call4 (Qadd_hook, Qbefore_revert_hook,
	 Qdelete_all_mode_overlays,
	 Qnil, Qt);
  call4 (Qadd_hook, Qchange_major_mode_hook,
	 Qdelete_all_mode_overlays,
	 Qnil, Qt);

  /* restores caller buffer */
  unbind_to (count, Qnil);
  if (current_buffer->proximity) /* MODE might have deleted this */
    if (PT < XFIXNUM (Fmarker_position (current_buffer->proximity->preceding))
	|| (!NILP (current_buffer->proximity->following)
	    && PT >= XFIXNUM (Fmarker_position
			      (current_buffer->proximity->following))))
      call1 (on_exit, ov);
  return ov;
}

DEFUN ("make-mode--overlay", Fmake_mode__overlay, Smake_mode__overlay,
       3, 3, 0,
       doc: /* Unexposed workhorse of `make-mode-overlay'.
The indirect buffer created is distinguished by MODE_OVERLAY_INDIRECT_P.  */)
  (Lisp_Object beg, Lisp_Object end, Lisp_Object mode)
{
  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_protect_void (unwind_extant_indirects);

  Lisp_Object tail, other;
  struct buffer *base = current_buffer->base_buffer
    ? current_buffer->base_buffer
    : current_buffer;

  extant_indirects = Qnil;
  FOR_EACH_LIVE_BUFFER (tail, other)
    if (XBUFFER (other)->base_buffer == base
	&& MODE_OVERLAY_INDIRECT_P (XBUFFER (other)))
      extant_indirects = Fcons (other, extant_indirects);
  return unbind_to (count,
		    internal_condition_case_1
		    (&make_mode__overlay,
		     list3 (beg, end, mode),
		     Qt, &make_mode__overlay_error));
}

DEFUN ("mode-overlay-indirect-p", Fmode_overlay_indirect_p, Smode_overlay_indirect_p, 1, 1, 0,
       doc: /* Return t if BUF corresponds to a mode overlay.  */)
  (Lisp_Object buf)
{
  CHECK_BUFFER (buf);
  return MODE_OVERLAY_INDIRECT_P (XBUFFER (buf)) ? Qt : Qnil;
}

void
delete_all_overlays (struct buffer *b)
{
  if (b->overlays)
    {
      /* The general rule is that the tree cannot be modified from within
	 ITREE_FOREACH, but here we bend this rule a little because we know
	 that the POST_ORDER iterator will not need to look at `node` again.  */
      struct itree_node *node;
      ITREE_FOREACH (node, b->overlays, PTRDIFF_MIN, PTRDIFF_MAX, POST_ORDER)
	{
	  modify_overlay (b, node->begin, node->end);
	  if (MODE_OVERLAY_INDIRECT_P (XOVERLAY (node->data)->buffer))
	    {
	      /* these are shared with B, so douse them */
	      XOVERLAY (node->data)->buffer->overlays = NULL;
	      XOVERLAY (node->data)->buffer->proximity = NULL;
	    }
	  XOVERLAY (node->data)->buffer = NULL;
	  node->parent = NULL;
	  node->left = NULL;
	  node->right = NULL;
	}
      itree_clear (b->overlays);
      itree_destroy (b->overlays);
      b->overlays = NULL;
    }
}

/* Adjust the position of overlays in the current buffer according to
   MULTIBYTE.

   Assume that positions currently correspond to byte positions, if
   MULTIBYTE is true and to character positions if not.
*/

static void
set_overlays_multibyte (bool multibyte)
{
  if (!current_buffer->overlays
      || Z == Z_BYTE
      || MODE_OVERLAY_INDIRECT_P (current_buffer))
    return;

  struct itree_node **nodes = NULL;
  struct itree_tree *tree = current_buffer->overlays;
  const intmax_t size = itree_size (tree);

  /* We can't use `itree_node_set_region` at the same time
     as we iterate over the itree, so we need an auxiliary storage
     to keep the list of nodes.  */
  USE_SAFE_ALLOCA;
  SAFE_NALLOCA (nodes, 1, size);
  {
    struct itree_node *node, **cursor = nodes;
    ITREE_FOREACH (node, tree, PTRDIFF_MIN, PTRDIFF_MAX, ASCENDING)
      *(cursor++) = node;
  }

  for (int i = 0; i < size; ++i, ++nodes)
    {
      struct itree_node * const node = *nodes;

      if (multibyte)
        {
          ptrdiff_t begin = itree_node_begin (tree, node);
          ptrdiff_t end = itree_node_end (tree, node);

          /* This models the behavior of markers.  (The behavior of
             text-intervals differs slightly.) */
          while (begin < Z_BYTE
                 && !CHAR_HEAD_P (FETCH_BYTE (begin)))
            begin++;
          while (end < Z_BYTE
                 && !CHAR_HEAD_P (FETCH_BYTE (end)))
            end++;
          itree_node_set_region (tree, node, BYTE_TO_CHAR (begin),
				 BYTE_TO_CHAR (end));
        }
      else
        {
          itree_node_set_region (tree, node, CHAR_TO_BYTE (node->begin),
				 CHAR_TO_BYTE (node->end));
        }
    }
  SAFE_FREE ();
}

/* Reinitialize all except name, contents, and local variables.  */

void
reset_buffer (register struct buffer *b)
{
  bset_filename (b, Qnil);
  bset_file_truename (b, Qnil);
  bset_directory (b, current_buffer ? BVAR (current_buffer, directory) : Qnil);
  b->modtime = make_timespec (0, UNKNOWN_MODTIME_NSECS);
  b->modtime_size = -1;
  XSETFASTINT (BVAR (b, save_length), 0);
  b->last_window_start = 1;
  /* It is more conservative to start out "changed" than "unchanged".  */
  b->clip_changed = 0;
  b->prevent_redisplay_optimizations_p = 1;
  bset_backed_up (b, Qnil);
  bset_local_minor_modes (b, Qnil);
  BUF_AUTOSAVE_MODIFF (b) = 0;
  b->auto_save_failure_time = 0;
  bset_auto_save_file_name (b, Qnil);
  bset_read_only (b, Qnil);
  b->overlays = NULL;
  b->proximity = NULL;
  bset_mark_active (b, Qnil);
  bset_point_before_scroll (b, Qnil);
  bset_file_format (b, Qnil);
  bset_auto_save_file_format (b, Qt);
  bset_last_selected_window (b, Qnil);
  bset_display_count (b, make_fixnum (0));
  bset_display_time (b, Qnil);
  bset_enable_multibyte_characters
    (b, BVAR (&buffer_slot_defaults, enable_multibyte_characters));
  bset_cursor_type (b, BVAR (&buffer_slot_defaults, cursor_type));
  bset_extra_line_spacing (b, BVAR (&buffer_slot_defaults, extra_line_spacing));

  b->display_error_modiff = 0;
}

/* Reset buffer-locals except permanent-locals.

   If IGNORE_PERM, also reset permanent-locals.
*/

static void
reset_buffer_local_variables (struct buffer *b, bool ignore_perm)
{
  /* Reset the major mode to Fundamental, together with all the
     things that depend on the major mode.
     default-major-mode is handled at a higher level.
     We ignore it here.  */
  bset_major_mode (b, Qfundamental_mode);
  bset_keymap (b, Qnil);
  bset_mode_name (b, QSFundamental);

  /* If the standard case table has been altered and invalidated,
     fix up its insides first.  */
  if (!(CHAR_TABLE_P (XCHAR_TABLE (Vascii_downcase_table)->extras[0])
	 && CHAR_TABLE_P (XCHAR_TABLE (Vascii_downcase_table)->extras[1])
	 && CHAR_TABLE_P (XCHAR_TABLE (Vascii_downcase_table)->extras[2])))
    Fset_standard_case_table (Vascii_downcase_table);

  bset_downcase_table (b, Vascii_downcase_table);
  bset_upcase_table (b, XCHAR_TABLE (Vascii_downcase_table)->extras[0]);
  bset_case_canon_table (b, XCHAR_TABLE (Vascii_downcase_table)->extras[1]);
  bset_case_eqv_table (b, XCHAR_TABLE (Vascii_downcase_table)->extras[2]);
  bset_invisibility_spec (b, Qt);

  Lisp_Object tail = Fcopy_sequence (BVAR (b, local_val_alist));
  FOR_EACH_TAIL_SAFE (tail)
    {
      Lisp_Object pair = XCAR (tail),
	var = XCAR (pair),
	prop_perm = Fget (var, Qpermanent_local);
      eassert (XSYMBOL (var)->u.s.type == SYMBOL_LOCAL_SOMEWHERE);
      if (ignore_perm || NILP (prop_perm)) // kill it
	kill_local_variable_internal (XSYMBOL (var), b);
      else if (EQ (prop_perm, Qpermanent_local_hook))
	{
	  /* Rewrite PAIR's binds.  A negligibly useful but
	     baroque scheme foisted upon us by RMS of
	     course.  */
	  Lisp_Object nbinds = Qnil;
	  for (Lisp_Object tail = XCDR (pair); CONSP (tail); tail = XCDR (tail))
	    {
	      Lisp_Object bind = XCAR (tail);
	      /* Preserve BIND if it's t, or has
		 `permanent-local-hook' property. */
	      if (EQ (bind, Qt)
		  || (SYMBOLP (bind)
		      && !NILP (Fget (bind, Qpermanent_local_hook))))
		nbinds = Fcons (bind, nbinds);
	    }
	  nbinds = Fnreverse (nbinds);
	  if (XSYMBOL (var)->u.s.trapped_write == SYMBOL_TRAPPED_WRITE)
	    notify_variable_watchers (var, nbinds, Qmakunbound,
				      make_lisp_ptr (b, Lisp_Vectorlike));
	  XSETCDR (pair, nbinds);
	}
    }

  eassert (!ignore_perm || NILP (BVAR (b, local_val_alist)));

  /* Douse local_flags bit for killed.  */
  for (int i = 0; i < last_per_buffer_idx; ++i)
    if (ignore_perm || !buffer_permanent_local_flags[i])
      SET_LOCALIZED_SLOT_P (b, i, 0);

  /* Set slot values for killed.  */
  int offset;
  FOR_EACH_PER_BUFFER_OBJECT_AT (offset)
    {
      int idx = PER_BUFFER_IDX (offset);
      if (idx > 0
	  && (ignore_perm || !buffer_permanent_local_flags[idx]))
	set_per_buffer_value (b, offset, per_buffer_default (offset));
    }
}

/* We split this away from generate-new-buffer, because rename-buffer
   and set-visited-file-name ought to be able to use this to really
   rename the buffer properly.  */

DEFUN ("generate-new-buffer-name", Fgenerate_new_buffer_name,
       Sgenerate_new_buffer_name, 1, 2, 0,
       doc: /* Return a string that is the name of no existing buffer based on NAME.
If there is no live buffer named NAME, then return NAME.
Otherwise modify name by appending `<NUMBER>', incrementing NUMBER
\(starting at 2) until an unused name is found, and then return that name.
Optional second argument IGNORE specifies a name that is okay to use (if
it is in the sequence to be tried) even if a buffer with that name exists.

If NAME begins with a space (i.e., a buffer that is not normally
visible to users), then if buffer NAME already exists a random number
is first appended to NAME, to speed up finding a non-existent buffer.  */)
  (Lisp_Object name, Lisp_Object ignore)
{
  Lisp_Object genbase;

  CHECK_STRING (name);

  if ((!NILP (ignore) && !NILP (Fstring_equal (name, ignore)))
      || NILP (Fget_buffer (name)))
    return name;

  if (SREF (name, 0) != ' ') /* See bug#1229.  */
    genbase = name;
  else
    {
      char number[INT_BUFSIZE_BOUND (int)];
      EMACS_INT r = get_random ();
      eassume (0 <= r);
      int i = r % 1000000;
      AUTO_STRING_WITH_LEN (lnumber, number, sprintf (number, "-%d", i));
      genbase = concat2 (name, lnumber);
      if (NILP (Fget_buffer (genbase)))
	return genbase;
    }

  for (ptrdiff_t count = 2; ; count++)
    {
      char number[INT_BUFSIZE_BOUND (ptrdiff_t) + sizeof "<>"];
      AUTO_STRING_WITH_LEN (lnumber, number,
			    sprintf (number, "<%"pD"d>", count));
      Lisp_Object gentemp = concat2 (genbase, lnumber);
      if (!NILP (Fstring_equal (gentemp, ignore))
	  || NILP (Fget_buffer (gentemp)))
	return gentemp;
    }
}


DEFUN ("buffer-name", Fbuffer_name, Sbuffer_name, 0, 1, 0,
       doc: /* Return the name of BUFFER, as a string.
BUFFER defaults to the current buffer.
Return nil if BUFFER has been killed.  */)
  (register Lisp_Object buffer)
{
  return BVAR (!NILP (buffer) ? XBUFFER (buffer) : current_buffer, name);
}

DEFUN ("buffer-last-name", Fbuffer_last_name, Sbuffer_last_name, 0, 1, 0,
       doc: /* Return last name of BUFFER, as a string.
BUFFER defaults to the current buffer.

This is the name BUFFER had before the last time it was renamed or
immediately before it was killed.  */)
  (Lisp_Object buffer)
{
  return BVAR (!NILP (buffer) ? XBUFFER (buffer) : current_buffer, last_name);
}

DEFUN ("buffer-file-name", Fbuffer_file_name, Sbuffer_file_name, 0, 1, 0,
       doc: /* Return name of file BUFFER is visiting, or nil if none.
No argument or nil as argument means use the current buffer.  */)
  (register Lisp_Object buffer)
{
  return BVAR (!NILP (buffer) ? XBUFFER (buffer) : current_buffer, filename);
}

DEFUN ("buffer-base-buffer", Fbuffer_base_buffer, Sbuffer_base_buffer,
       0, 1, 0,
       doc: /* Return the base buffer of indirect buffer BUFFER.
If BUFFER is not indirect, return nil.
BUFFER defaults to the current buffer.  */)
  (register Lisp_Object buffer)
{
  Lisp_Object ret;
  struct buffer *b = BUFFERP (buffer) ? XBUFFER (buffer) : current_buffer;
  return b->base_buffer
    ? (XSETBUFFER (ret, b->base_buffer), ret)
    : Qnil;
}

DEFUN ("buffer-local-value", Fbuffer_local_value,
       Sbuffer_local_value, 2, 2, 0,
       doc: /* Return the value of VARIABLE in BUFFER.
If VARIABLE does not have a buffer-local binding in BUFFER, the value
is the default binding of the variable.  */)
  (Lisp_Object variable, Lisp_Object buffer)
{
  Lisp_Object result;
  CHECK_BUFFER (buffer);
  result = find_symbol_value (XSYMBOL (variable), XBUFFER (buffer));
  return EQ (result, Qunbound)
    ? (xsignal1 (Qvoid_variable, variable), Qnil)
    : result;
}

/* Return an alist of the Lisp-level buffer-local bindings of
   buffer BUF.  That is, don't include the variables maintained
   in special slots in the buffer object.
   If not CLONE, replace elements of the form (VAR . unbound)
   by VAR.  */

static Lisp_Object
buffer_lisp_local_values (struct buffer *buf, bool clone)
{
  Lisp_Object result = Qnil;
  for (Lisp_Object tail = BVAR (buf, local_val_alist);
       CONSP (tail);
       tail = XCDR (tail))
    {
      Lisp_Object val, elt;

      elt = XCAR (tail);

      /* Reference each variable in the alist in buf.
	 If inquiring about the current buffer, this gets the current values,
	 so store them into the alist so the alist is up to date.
	 If inquiring about some other buffer, this swaps out any values
	 for that buffer, making the alist up to date automatically.  */
      val = find_symbol_value (XSYMBOL (XCAR (elt)), NULL);
      /* Use the current buffer value only if buf is the current buffer.  */
      if (buf != current_buffer)
	val = XCDR (elt);

      result = Fcons (!clone && EQ (val, Qunbound)
		      ? XCAR (elt)
		      : Fcons (XCAR (elt), val),
		      result);
    }

  return result;
}


/* If the variable at position index OFFSET in buffer BUF has a
   buffer-local value, return (name . value).  If SYM is non-nil,
   it replaces name.  */

static Lisp_Object
buffer_local_variables_1 (struct buffer *buf, int offset, Lisp_Object sym)
{
  int idx = PER_BUFFER_IDX (offset);
  if ((idx == -1 || LOCALIZED_SLOT_P (buf, idx))
      && SYMBOLP (PER_BUFFER_SYMBOL (offset)))
    {
      sym = NILP (sym) ? PER_BUFFER_SYMBOL (offset) : sym;
      Lisp_Object val = per_buffer_value (buf, offset);
      return EQ (val, Qunbound) ? sym : Fcons (sym, val);
    }
  return Qnil;
}

DEFUN ("buffer-local-variables", Fbuffer_local_variables,
       Sbuffer_local_variables, 0, 1, 0,
       doc: /* Return an alist of variables that are buffer-local in BUFFER.
Most elements look like (SYMBOL . VALUE), describing one variable.
For a symbol that is locally unbound, just the symbol appears in the value.
Note that storing new VALUEs in these elements doesn't change the variables.
No argument or nil as argument means use current buffer as BUFFER.  */)
  (Lisp_Object buffer)
{
  int offset;
  struct buffer *b = BUFFERP (buffer) ? XBUFFER (buffer) : current_buffer;
  Lisp_Object result = buffer_lisp_local_values (b, 0);

  /* Add all slots.  */
  FOR_EACH_PER_BUFFER_OBJECT_AT (offset)
    {
      Lisp_Object tem = buffer_local_variables_1 (b, offset, Qnil);
      if (!NILP (tem))
	result = Fcons (tem, result);
    }

  Lisp_Object tem = buffer_local_variables_1 (b, PER_BUFFER_VAR_OFFSET (undo_list),
					      intern ("buffer-undo-list"));
  if (!NILP (tem))
    result = Fcons (tem, result);

  return result;
}

DEFUN ("buffer-modified-p", Fbuffer_modified_p, Sbuffer_modified_p,
       0, 1, 0,
       doc: /* Return non-nil if BUFFER was modified since last saved.
Returns the symbol \\='autosaved if BUFFER was autosaved since last modified.  */)
  (Lisp_Object buffer)
{
  struct buffer *b = BUFFERP (buffer) ? XBUFFER (buffer) : current_buffer;
  return BUF_SAVE_MODIFF (b) >= BUF_MODIFF (b)
    ? Qnil
    : BUF_AUTOSAVE_MODIFF (b) == BUF_MODIFF (b)
    ? Qautosaved
    : Qt;
}

DEFUN ("force-mode-line-update", Fforce_mode_line_update,
       Sforce_mode_line_update, 0, 1, 0,
       doc: /* Force redisplay of the current buffer's mode line and header line.
With optional non-nil ALL, force redisplay of all mode lines, tab lines and
header lines.  This function also forces recomputation of the
menu bar menus and the frame title.  */)
     (Lisp_Object all)
{
  if (!NILP (all))
    {
      update_mode_lines = 10;
      /* FIXME: This can't be right.  */
      current_buffer->prevent_redisplay_optimizations_p = true;
    }
  else if (buffer_window_count (current_buffer))
    {
      bset_update_mode_line (current_buffer);
      current_buffer->prevent_redisplay_optimizations_p = true;
    }
  return all;
}

DEFUN ("set-buffer-modified-p", Fset_buffer_modified_p, Sset_buffer_modified_p,
       1, 1, 0,
       doc: /* Mark current buffer as modified or unmodified according to FLAG.
A non-nil FLAG means mark the buffer modified.
In addition, this function unconditionally forces redisplay of the
mode lines of the windows that display the current buffer, and also
locks or unlocks the file visited by the buffer, depending on whether
the function's argument is non-nil, but only if both `buffer-file-name'
and `buffer-file-truename' are non-nil.  */)
  (Lisp_Object flag)
{
  Frestore_buffer_modified_p (flag);

  /* Set update_mode_lines only if buffer is displayed in some window.
     Modes like jit-lock preserve a buffer's modified state by
     recording/restoring the state around blocks of code.  Setting
     update_mode_lines makes redisplay consider all windows (on all
     frames).  Stealth fontification of buffers not displayed would
     incur additional redisplay costs if we'd set update_modes_lines
     unconditionally.

     Ideally, I think there should be another mechanism for fontifying
     buffers without "modifying" buffers, or redisplay should be
     smarter about updating the `*' in mode lines.  --gerd  */
  return Fforce_mode_line_update (Qnil);
}

DEFUN ("restore-buffer-modified-p", Frestore_buffer_modified_p,
       Srestore_buffer_modified_p, 1, 1, 0,
       doc: /* Like `set-buffer-modified-p', but doesn't redisplay buffer's mode line.
A nil FLAG means to mark the buffer as unmodified.  A non-nil FLAG
means mark the buffer as modified.  A special value of `autosaved'
will mark the buffer as modified and also as autosaved since it was
last modified.

This function also locks or unlocks the file visited by the buffer,
if both `buffer-file-truename' and `buffer-file-name' are non-nil.

It is not ensured that mode lines will be updated to show the modified
state of the current buffer.  Use with care.  */)
  (Lisp_Object flag)
{

  /* If buffer becoming modified, lock the file.
     If buffer becoming unmodified, unlock the file.  */

  struct buffer *b = current_buffer->base_buffer
    ? current_buffer->base_buffer
    : current_buffer;

  if (!inhibit_modification_hooks)
    {
      Lisp_Object fn = BVAR (b, file_truename);
      /* Test buffer-file-name so that binding it to nil is effective.  */
      if (!NILP (fn) && !NILP (BVAR (b, filename)))
        {
          bool already = SAVE_MODIFF < MODIFF;
          if (!already && !NILP (flag))
	    Flock_file (fn);
          else if (already && NILP (flag))
	    Funlock_file (fn);
        }
    }

  /* Here we have a problem.  SAVE_MODIFF is used here to encode
     buffer-modified-p (as SAVE_MODIFF<MODIFF) as well as
     recent-auto-save-p (as SAVE_MODIFF<auto_save_modified).  So if we
     modify SAVE_MODIFF to affect one, we may affect the other
     as well.
     E.g. if FLAG is nil we need to set SAVE_MODIFF to MODIFF, but
     if SAVE_MODIFF<auto_save_modified that means we risk changing
     recent-auto-save-p from t to nil.
     Vice versa, if FLAG is non-nil and SAVE_MODIFF>=auto_save_modified
     we risk changing recent-auto-save-p from nil to t.  */
  if (NILP (flag))
    /* This unavoidably sets recent-auto-save-p to nil.  */
    SAVE_MODIFF = MODIFF;
  else
    {
      /* If SAVE_MODIFF == auto_save_modified == MODIFF, we can either
	 decrease SAVE_MODIFF and auto_save_modified or increase
	 MODIFF.  */
      if (SAVE_MODIFF >= MODIFF)
	SAVE_MODIFF = modiff_incr (&MODIFF);
      if (EQ (flag, Qautosaved))
	BUF_AUTOSAVE_MODIFF (b) = MODIFF;
    }
  return flag;
}

DEFUN ("buffer-modified-tick", Fbuffer_modified_tick, Sbuffer_modified_tick,
       0, 1, 0,
       doc: /* Return BUFFER's tick counter, incremented for each change in text.
Each buffer has a tick counter which is incremented each time the
text in that buffer is changed.  No argument or nil as argument means
use current buffer as BUFFER.  */)
  (Lisp_Object buffer)
{
  struct buffer *b = BUFFERP (buffer) ? XBUFFER (buffer) : current_buffer;
  return modiff_to_integer (BUF_MODIFF (b));
}

DEFUN ("internal--set-buffer-modified-tick",
       Finternal__set_buffer_modified_tick, Sinternal__set_buffer_modified_tick,
       1, 2, 0,
       doc: /* Set BUFFER's tick counter to TICK.
No argument or nil as argument means use current buffer as BUFFER.  */)
  (Lisp_Object tick, Lisp_Object buffer)
{
  struct buffer *b = BUFFERP (buffer) ? XBUFFER (buffer) : current_buffer;
  CHECK_FIXNUM (tick);
  BUF_MODIFF (b) = XFIXNUM (tick);
  return Qnil;
}

DEFUN ("buffer-chars-modified-tick", Fbuffer_chars_modified_tick,
       Sbuffer_chars_modified_tick, 0, 1, 0,
       doc: /* Return BUFFER's character-change tick counter.
Each buffer has a character-change tick counter, which is set to the
value of the buffer's tick counter (see `buffer-modified-tick'), each
time text in that buffer is inserted or deleted.  By comparing the
values returned by two individual calls of `buffer-chars-modified-tick',
you can tell whether a character change occurred in that buffer in
between these calls.  No argument or nil as argument means use current
buffer as BUFFER.  */)
  (Lisp_Object buffer)
{
  struct buffer *b = BUFFERP (buffer) ? XBUFFER (buffer) : current_buffer;
  return modiff_to_integer (BUF_CHARS_MODIFF (b));
}

DEFUN ("rename-buffer", Frename_buffer, Srename_buffer, 1, 2,
       "(list (read-string \"Rename buffer (to new name): \" \
	      nil 'buffer-name-history (buffer-name (current-buffer))) \
	      current-prefix-arg)",
       doc: /* Change current buffer's name to NEWNAME (a string).
If second arg UNIQUE is nil or omitted, it is an error if a
buffer named NEWNAME already exists.
If UNIQUE is non-nil, come up with a new name using
`generate-new-buffer-name'.
Interactively, you can set UNIQUE with a prefix argument.
We return the name we actually gave the buffer.
This does not change the name of the visited file (if any).  */)
  (register Lisp_Object newname, Lisp_Object unique)
{
  register Lisp_Object tem, buf;
  Lisp_Object oldname = BVAR (current_buffer, name);
  Lisp_Object requestedname = newname;

  CHECK_STRING (newname);

  if (SCHARS (newname) == 0)
    error ("Empty string is invalid as a buffer name");

  tem = Fget_buffer (newname);
  if (!NILP (tem))
    {
      /* Don't short-circuit if UNIQUE is t.  That is a useful way to
	 rename the buffer automatically so you can create another
	 with the original name.  It makes UNIQUE equivalent to
	 (rename-buffer (generate-new-buffer-name NEWNAME)).  */
      if (NILP (unique) && XBUFFER (tem) == current_buffer)
	return BVAR (current_buffer, name);
      if (!NILP (unique))
	newname = Fgenerate_new_buffer_name (newname, oldname);
      else
	error ("Buffer name `%s' is in use", SDATA (newname));
    }

  bset_last_name (current_buffer, oldname);
  bset_name (current_buffer, newname);

  /* Catch redisplay's attention.  Unless we do this, the mode lines for
     any windows displaying current_buffer will stay unchanged.  */
  bset_update_mode_line (current_buffer);

  XSETBUFFER (buf, current_buffer);
  Fsetcar (Frassq (buf, Vbuffer_alist), newname);
  if (NILP (BVAR (current_buffer, filename))
      && !NILP (BVAR (current_buffer, auto_save_file_name)))
    call0 (intern ("rename-auto-save-file"));

  run_buffer_list_update_hook (current_buffer);

  call2 (intern ("uniquify--rename-buffer-advice"),
         requestedname, unique);

  /* Refetch since that last call may have done GC.  */
  return BVAR (current_buffer, name);
}

DEFUN ("other-buffer", Fother_buffer, Sother_buffer, 0, 3, 0,
       doc: /* Return most recently selected buffer other than BUFFER.
Buffers not visible in windows are preferred to visible buffers, unless
optional second argument VISIBLE-OK is non-nil.  If the optional third
argument FRAME specifies a live frame, then use that frame's buffer list
instead of the selected frame's buffer list.

The buffer is found by scanning the selected or specified frame's buffer
list first, followed by the list of all buffers.  If no other buffer
exists, return the buffer "*scratch*".  */)
  (Lisp_Object buffer, Lisp_Object visible_ok, Lisp_Object frame)
{
  struct frame *f = decode_live_frame (frame);
  Lisp_Object tail = f->buffer_list, pred = f->buffer_predicate;
  buffer = !NILP (buffer) ? buffer : Fcurrent_buffer ();
  Lisp_Object buf, notsogood = Qnil;

  /* Consider buffers that have been seen in the frame first.  */
  for (; CONSP (tail); tail = XCDR (tail))
    {
      buf = XCAR (tail);
      if (candidate_buffer (buf, buffer)
	  /* If the frame has a buffer_predicate, disregard buffers that
	     don't fit the predicate.  */
	  && (NILP (pred) || !NILP (call1 (pred, buf))))
	{
	  if (!NILP (visible_ok)
	      || NILP (Fget_buffer_window (buf, Qvisible)))
	    return buf;
	  else if (NILP (notsogood))
	    notsogood = buf;
	}
    }

  /* Consider alist of all buffers next.  */
  FOR_EACH_LIVE_BUFFER (tail, buf)
    {
      if (candidate_buffer (buf, buffer)
	  /* If the frame has a buffer_predicate, disregard buffers that
	     don't fit the predicate.  */
	  && (NILP (pred) || !NILP (call1 (pred, buf))))
	{
	  if (!NILP (visible_ok)
	      || NILP (Fget_buffer_window (buf, Qvisible)))
	    return buf;
	  else if (NILP (notsogood))
	    notsogood = buf;
	}
    }

  if (!NILP (notsogood))
    return notsogood;
  else
    return safe_calln (Qget_scratch_buffer_create);
}

/* The following function is a safe variant of Fother_buffer: It doesn't
   pay attention to any frame-local buffer lists, doesn't care about
   visibility of buffers, and doesn't evaluate any frame predicates.  */

Lisp_Object
other_buffer_safely (Lisp_Object buffer)
{
  Lisp_Object tail, buf;

  FOR_EACH_LIVE_BUFFER (tail, buf)
    if (candidate_buffer (buf, buffer))
      return buf;

  /* This function must return a valid buffer, since it is frequently
     our last line of defense in the face of the expected buffers
     becoming dead under our feet.  safe_call below could return nil
     if recreating *scratch* in Lisp, which does some fancy stuff,
     signals an error in some weird use case.  */
  buf = safe_calln (Qget_scratch_buffer_create);
  if (NILP (buf))
    {
      AUTO_STRING (scratch, "*scratch*");
      buf = Fget_buffer_create (scratch, Qnil);
      Fset_buffer_major_mode (buf);
    }
  return buf;
}

DEFUN ("buffer-enable-undo", Fbuffer_enable_undo, Sbuffer_enable_undo,
       0, 1, "",
       doc: /* Start keeping undo information for buffer BUFFER.
No argument or nil as argument means do this for the current buffer.  */)
  (register Lisp_Object buffer)
{
  Lisp_Object real_buffer;

  if (NILP (buffer))
    XSETBUFFER (real_buffer, current_buffer);
  else
    {
      real_buffer = Fget_buffer (buffer);
      if (NILP (real_buffer))
	nsberror (buffer);
    }

  if (EQ (BVAR (XBUFFER (real_buffer), undo_list), Qt))
    bset_undo_list (XBUFFER (real_buffer), Qnil);

  return Qnil;
}

/* Truncate undo list and shrink the gap of BUFFER.  */

void
compact_buffer (struct buffer *buffer)
{
  BUFFER_CHECK_INDIRECTION (buffer);

  if (BUFFER_LIVE_P (buffer)
      /* not indirect */
      && buffer->base_buffer == NULL
      /* changed since last compaction */
      && BUF_COMPACT (buffer) != BUF_MODIFF (buffer))
    {
      if (!EQ (BVAR(buffer, undo_list), Qt)) /* undo not disabled.  */
	undo_truncate (buffer);

      if (!buffer->text->inhibit_shrinking)
	{
	  /* Shrink if gap exceeds either 10% of buffer size or
	     GAP_BYTES_DFL bytes.  */
	  const ptrdiff_t thresh = clip_to_bounds (GAP_BYTES_MIN,
						   BUF_Z_BYTE (buffer) / 10,
						   GAP_BYTES_DFL);
	  if (BUF_GAP_SIZE (buffer) > thresh)
	    make_gap_1 (buffer, -(BUF_GAP_SIZE (buffer) - thresh));
	}
      BUF_COMPACT (buffer) = BUF_MODIFF (buffer);
    }
}

DEFUN ("kill-buffer", Fkill_buffer, Skill_buffer, 0, 1, "bKill buffer: ",
       doc: /* Kill the buffer specified by BUFFER-OR-NAME.
The argument may be a buffer or the name of an existing buffer.
Argument nil or omitted means kill the current buffer.  Return t if the
buffer is actually killed, nil otherwise.

The functions in `kill-buffer-query-functions' are called with the
buffer to be killed as the current buffer.  If any of them returns nil,
the buffer is not killed.  The hook `kill-buffer-hook' is run before the
buffer is actually killed.  The buffer being killed will be current
while the hook is running.  Functions called by any of these hooks are
supposed to not change the current buffer.  Neither hook is run for
internal or temporary buffers created by `get-buffer-create' or
`generate-new-buffer' with argument INHIBIT-BUFFER-HOOKS non-nil.

Any processes that have this buffer as the `process-buffer' are killed
with SIGHUP.  This function calls `replace-buffer-in-windows' for
cleaning up all windows currently displaying the buffer to be killed. */)
  (Lisp_Object buffer_or_name)
{
  Lisp_Object buffer;
  struct buffer *b;
  Lisp_Object tem;
  struct Lisp_Marker *m;

  if (NILP (buffer_or_name))
    buffer = Fcurrent_buffer ();
  else
    buffer = Fget_buffer (buffer_or_name);
  if (NILP (buffer))
    nsberror (buffer_or_name);

  if (MODE_OVERLAY_INDIRECT_P (XBUFFER (buffer)))
    XSETBUFFER (buffer, XBUFFER (buffer)->base_buffer);

  CHECK_BUFFER (buffer);
  b = XBUFFER (buffer);

  if (!BUFFER_LIVE_P (b))
    return Qnil;

  /* Run hooks with the buffer to be killed as the current buffer.  */
  specpdl_ref count = SPECPDL_INDEX ();
  bool modified;

  record_unwind_protect_excursion ();
  set_buffer_internal (b);

  /* First run the query functions; if any query is answered no,
     don't kill the buffer.  */
  if (!b->inhibit_buffer_hooks)
    {
      tem = CALLN (Frun_hook_with_args_until_failure,
		   Qkill_buffer_query_functions);
      if (NILP (tem))
	return unbind_to (count, Qnil);
    }

  /* Is this a modified buffer that's visiting a file? */
  modified = !NILP (BVAR (b, filename))
    && BUF_MODIFF (b) > BUF_SAVE_MODIFF (b);

  /* Query if the buffer is still modified.  */
  if (INTERACTIVE && modified)
    {
      /* Ask whether to kill the buffer, and exit if the user says
	 "no".  */
      if (NILP (call1 (Qkill_buffer__possibly_save, buffer)))
	return unbind_to (count, Qnil);
      /* Recheck modified.  */
      modified = BUF_MODIFF (b) > BUF_SAVE_MODIFF (b);
    }

  /* Delete the autosave file, if requested. */
  if (modified
      && kill_buffer_delete_auto_save_files
      && delete_auto_save_files
      && !NILP (Frecent_auto_save_p ())
      && STRINGP (BVAR (b, auto_save_file_name))
      && !NILP (Ffile_exists_p (BVAR (b, auto_save_file_name)))
      /* If `auto-save-visited-mode' is on, then we're auto-saving
	 to the visited file -- don't delete it.. */
      && NILP (Fstring_equal (BVAR (b, auto_save_file_name),
			      BVAR (b, filename))))
    {
      tem = do_yes_or_no_p (build_string ("Delete auto-save file? "));
      if (!NILP (tem))
	call0 (intern ("delete-auto-save-file-if-necessary"));
    }

  /* If the hooks have killed the buffer, exit now.  */
  if (!BUFFER_LIVE_P (b))
    return unbind_to (count, Qt);

  /* Then run the hooks.  */
  if (!b->inhibit_buffer_hooks)
    run_hook (Qkill_buffer_hook);
  unbind_to (count, Qnil);

  /* If the hooks have killed the buffer, exit now.  */
  if (!BUFFER_LIVE_P (b))
    return Qt;

  /* We have no more questions to ask.  Verify that it is valid
     to kill the buffer.  This must be done after the questions
     since anything can happen within do_yes_or_no_p.  */

  /* Don't kill the minibuffer now current.  */
  if (EQ (buffer, XWINDOW (minibuf_window)->contents))
    return Qnil;

  /* When we kill an ordinary buffer which shares its buffer text
     with indirect buffer(s), we must kill indirect buffer(s) too.
     We do it at this stage so nothing terrible happens if they
     ask questions or their hooks get errors.  */
  if (!b->base_buffer && b->indirections > 0)
    {
      Lisp_Object tail, other;

      FOR_EACH_LIVE_BUFFER (tail, other)
	{
	  struct buffer *obuf = XBUFFER (other);
	  if (obuf->base_buffer == b)
	    {
	      obuf->proximity = NULL; /* otherwise Fkill_buffer infloops */
	      Fkill_buffer (other);
	      if (BUFFER_LIVE_P (obuf))
		error ("Unable to kill buffer whose indirect buffer `%s' cannot be killed",
		       SDATA (BVAR (obuf, name)));
	    }
	}

      /* Exit if we now have killed the base buffer (Bug#11665).  */
      if (!BUFFER_LIVE_P (b))
	return Qt;
    }

  /* Run replace_buffer_in_windows before making another buffer current
     since set-window-buffer-start-and-point will refuse to make another
     buffer current if the selected window does not show the current
     buffer (bug#10114).  */
  replace_buffer_in_windows (buffer);

  /* For dead windows that have not been collected yet, remove this
     buffer from those windows' lists of previously and next shown
     buffers and remove any 'quit-restore' or 'quit-restore-prev'
     parameters mentioning the buffer.  */
  if (XFIXNUM (BVAR (b, display_count)) > 0)
    window_discard_buffer_from_dead_windows (buffer);

  /* Exit if replacing the buffer in windows has killed our buffer.  */
  if (!BUFFER_LIVE_P (b))
    return Qt;

  /* Make this buffer not be current.  Exit if it is the sole visible
     buffer.  */
  if (b == current_buffer)
    {
      tem = Fother_buffer (buffer, Qnil, Qnil);
      Fset_buffer (tem);
      if (b == current_buffer)
	return Qnil;
    }

  /* If the buffer now current is shown in the minibuffer and our buffer
     is the sole other buffer give up.  */
  XSETBUFFER (tem, current_buffer);
  if (EQ (tem, XWINDOW (minibuf_window)->contents)
      && EQ (buffer, Fother_buffer (buffer, Qnil, Qnil)))
    return Qnil;

  /* Now there is no question: we can kill the buffer.  */

  /* Unlock this buffer's file, if it is locked.  */
  unlock_buffer (b);

  kill_buffer_processes (buffer);
  kill_buffer_xwidgets (buffer);

  /* Killing buffer processes may run sentinels which may have killed
     our buffer.  */
  if (!BUFFER_LIVE_P (b))
    return Qt;

  /* These may run Lisp code and into infinite loops (if someone
     insisted on circular lists) so allow quitting here.  */
  frames_discard_buffer (buffer);

  clear_charpos_cache (b);

  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;
  /* Once the buffer is removed from Vbuffer_alist, its undo_list field is
     not traced by the GC in the same way.  So set it to nil early.  */
  bset_undo_list (b, Qnil);
  /* Remove the buffer from the list of all buffers.  */
  Vbuffer_alist = Fdelq (Frassq (buffer, Vbuffer_alist), Vbuffer_alist);
  /* If replace_buffer_in_windows didn't do its job fix that now.  */
  replace_buffer_in_windows_safely (buffer);
  Vinhibit_quit = tem;

  if (b->base_buffer)
    {
      INTERVAL i;
      /* Unchain all markers that belong to this indirect buffer.
	 Don't unchain the markers that belong to the base buffer
	 or its other indirect buffers.  */
      struct Lisp_Marker **mp = &BUF_MARKERS (b);
      while ((m = *mp))
	{
	  if (m->buffer == b)
	    {
	      m->buffer = NULL;
	      *mp = m->next;
	    }
	  else
	    mp = &m->next;
	}
      /* Intervals should be owned by the base buffer (Bug#16502).  */
      i = buffer_intervals (b);
      if (i)
	{
	  Lisp_Object owner;
	  XSETBUFFER (owner, b->base_buffer);
	  set_interval_object (i, owner);
	}
    }
  else
    {
      /* Unchain all markers of this buffer and its indirect buffers.
	 and leave them pointing nowhere.  */
      for (m = BUF_MARKERS (b); m; )
	{
	  struct Lisp_Marker *next = m->next;
	  m->buffer = 0;
	  m->next = NULL;
	  m = next;
	}
      BUF_MARKERS (b) = NULL;
      set_buffer_intervals (b, NULL);

      /* Perhaps we should explicitly free the interval tree here...  */
    }

  if (b->base_buffer == NULL
      || b->overlays != b->base_buffer->overlays)
    delete_all_overlays (b);

  /* Reset the local variables, so that this buffer's local values
     won't be protected from GC.  They would be protected
     if they happened to remain cached in their symbols.
     This gets rid of them for certain.  */
  reset_buffer_local_variables (b, 1);

  bset_last_name (b, BVAR (b, name));
  bset_name (b, Qnil);

  block_input ();
  if (b->base_buffer)
    {
      /* Notify our base buffer that we don't share the text anymore.  */
      eassert (b->indirections == -1);
      b->base_buffer->indirections--;
      eassert (b->base_buffer->indirections >= 0);
      /* Make sure that we wasn't confused.  */
      eassert (b->window_count == -1);
      b->proximity = NULL; /* base buffer xfree's */
      b->overlays = NULL;  /* base buffer xfree's */
    }
  else
    {
      if (b->proximity != NULL)
	{
	  xfree (b->proximity);
	  b->proximity = NULL;
	}
      /* Make sure that no one shows us.  */
      eassert (b->window_count == 0);
      /* No one shares our buffer text, can free it.  */
      free_buffer_text (b);
    }

  if (b->newline_cache)
    {
      free_region_cache (b->newline_cache);
      b->newline_cache = 0;
    }
  if (b->width_run_cache)
    {
      free_region_cache (b->width_run_cache);
      b->width_run_cache = 0;
    }
  if (b->bidi_paragraph_cache)
    {
      free_region_cache (b->bidi_paragraph_cache);
      b->bidi_paragraph_cache = 0;
    }
  bset_width_table (b, Qnil);
  unblock_input ();

  run_buffer_list_update_hook (b);

  return Qt;
}

/* Move association for BUFFER to the front of buffer (a)lists.  Since
   we do this each time BUFFER is selected visibly, the more recently
   selected buffers are always closer to the front of those lists.  This
   means that other_buffer is more likely to choose a relevant buffer.

   Note that this moves BUFFER to the front of the buffer lists of the
   selected frame even if BUFFER is not shown there.  If BUFFER is not
   shown in the selected frame, consider the present behavior a feature.
   `select-window' gets this right since it shows BUFFER in the selected
   window when calling us.  */

void
record_buffer (Lisp_Object buffer)
{
  Lisp_Object aelt, aelt_cons, tem;
  register struct frame *f = XFRAME (selected_frame);

  CHECK_BUFFER (buffer);

  /* Update Vbuffer_alist (we know that it has an entry for BUFFER).
     Don't allow quitting since this might leave the buffer list in an
     inconsistent state.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;
  aelt = Frassq (buffer, Vbuffer_alist);
  aelt_cons = Fmemq (aelt, Vbuffer_alist);
  Vbuffer_alist = Fdelq (aelt, Vbuffer_alist);
  XSETCDR (aelt_cons, Vbuffer_alist);
  Vbuffer_alist = aelt_cons;
  Vinhibit_quit = tem;

  /* Update buffer list of selected frame.  */
  fset_buffer_list (f, Fcons (buffer, Fdelq (buffer, f->buffer_list)));
  fset_buried_buffer_list (f, Fdelq (buffer, f->buried_buffer_list));

  run_buffer_list_update_hook (XBUFFER (buffer));
}


/* Move BUFFER to the end of the buffer (a)lists.  Do nothing if the
   buffer is killed.  For the selected frame's buffer list this moves
   BUFFER to its end even if it was never shown in that frame.  If
   this happens we have a feature, hence `bury-buffer-internal' should be
   called only when BUFFER was shown in the selected frame.  */

DEFUN ("bury-buffer-internal", Fbury_buffer_internal, Sbury_buffer_internal,
       1, 1, 0,
       doc: /* Move BUFFER to the end of the buffer list.  */)
  (Lisp_Object buffer)
{
  Lisp_Object aelt, aelt_cons, tem;
  register struct frame *f = XFRAME (selected_frame);

  CHECK_BUFFER (buffer);

  /* Update Vbuffer_alist (we know that it has an entry for BUFFER).
     Don't allow quitting since this might leave the buffer list in an
     inconsistent state.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;
  aelt = Frassq (buffer, Vbuffer_alist);
  aelt_cons = Fmemq (aelt, Vbuffer_alist);
  Vbuffer_alist = Fdelq (aelt, Vbuffer_alist);
  XSETCDR (aelt_cons, Qnil);
  Vbuffer_alist = nconc2 (Vbuffer_alist, aelt_cons);
  Vinhibit_quit = tem;

  /* Update buffer lists of selected frame.  */
  fset_buffer_list (f, Fdelq (buffer, f->buffer_list));
  fset_buried_buffer_list
    (f, Fcons (buffer, Fdelq (buffer, f->buried_buffer_list)));

  run_buffer_list_update_hook (XBUFFER (buffer));

  return Qnil;
}

DEFUN ("set-buffer-major-mode", Fset_buffer_major_mode, Sset_buffer_major_mode, 1, 1, 0,
       doc: /* Set an appropriate major mode for BUFFER.
For the *scratch* buffer, use `initial-major-mode', otherwise choose a mode
according to the default value of `major-mode'.
Use this function before selecting the buffer, since it may need to inspect
the current buffer's major mode.  */)
  (Lisp_Object buffer)
{
  Lisp_Object function;

  CHECK_BUFFER (buffer);

  if (!BUFFER_LIVE_P (XBUFFER (buffer)))
    error ("Attempt to set major mode for a dead buffer");

  if (strcmp (SSDATA (BVAR (XBUFFER (buffer), name)), "*scratch*") == 0)
    function = find_symbol_value (XSYMBOL (intern ("initial-major-mode")), NULL);
  else
    {
      function = BVAR (&buffer_slot_defaults, major_mode);
      if (NILP (function)
	  && NILP (Fget (BVAR (current_buffer, major_mode), Qmode_class)))
	function = BVAR (current_buffer, major_mode);
    }

  if (NILP (function)) /* If function is `fundamental-mode', allow it to run
                          so that `run-mode-hooks' and thus
                          `hack-local-variables' get run. */
    return Qnil;

  specpdl_ref count = SPECPDL_INDEX ();

  /* To select a nonfundamental mode,
     select the buffer temporarily and then call the mode function.  */

  record_unwind_current_buffer ();

  Fset_buffer (buffer);
  call0 (function);

  return unbind_to (count, Qnil);
}

DEFUN ("current-buffer", Fcurrent_buffer, Scurrent_buffer, 0, 0, 0,
       doc: /* Return the current buffer as a Lisp object.  */)
  (void)
{
  register Lisp_Object buf;
  XSETBUFFER (buf, current_buffer);
  return buf;
}

void
set_buffer_internal (struct buffer *new_buf)
{
  Lisp_Object tail;
  struct buffer *old_buf = current_buffer;
  BUFFER_CHECK_INDIRECTION (new_buf);

#ifdef USE_MMAP_FOR_BUFFERS
  if (new_buf->text->beg == NULL)
    enlarge_buffer_text (new_buf, 0);
#endif /* USE_MMAP_FOR_BUFFERS */

  if (new_buf == old_buf)
    return;
  current_buffer = new_buf;
  last_known_column_point = -1; /* Invalidate indentation cache.  */

  /* Set base's undo list to child's.  */
  if (old_buf)
    {
      if (old_buf->base_buffer)
	bset_undo_list (old_buf->base_buffer, BVAR (old_buf, undo_list));

      /* Backgrounds old markers.  */
      record_buffer_markers (old_buf);
    }

  /* Set child's undo list to base's.  */
  if (current_buffer->base_buffer)
    bset_undo_list (current_buffer, BVAR (current_buffer->base_buffer, undo_list));

  /* Foregrounds new markers.  */
  fetch_buffer_markers (current_buffer);

  /* Perform context switching blv_update. Afterwards, no C variables
     will reflect OLD_BUF's view.
  */
  if (old_buf)
    {
      tail = BVAR (old_buf, local_val_alist);
      FOR_EACH_TAIL_SAFE (tail)
	{
	  Lisp_Object var = XCAR (XCAR (tail));
	  switch_buffer_local_context (XSYMBOL (var), current_buffer);
	}
    }

  /* Intersection of OLD_BUF and NEW_BUF blv's switched.  Now do rest
     of NEW_BUF's blv's.  */
  tail = BVAR (current_buffer, local_val_alist);
  FOR_EACH_TAIL_SAFE (tail)
    {
      Lisp_Object var = XCAR (XCAR (tail));
      switch_buffer_local_context (XSYMBOL (var), current_buffer);
    }
}

/* Switch to buffer B temporarily for redisplay purposes.
   This avoids certain things that don't need to be done within redisplay.  */

void
set_buffer_temp (struct buffer *b)
{
  register struct buffer *old_buf;

  if (current_buffer == b)
    return;

  old_buf = current_buffer;
  current_buffer = b;

  /* If the old current buffer has markers to record PT, BEGV and ZV
     when it is not current, update them now.  */
  record_buffer_markers (old_buf);

  /* If the new current buffer has markers to record PT, BEGV and ZV
     when it is not current, fetch them now.  */
  fetch_buffer_markers (b);
}

DEFUN ("set-buffer", Fset_buffer, Sset_buffer, 1, 1, 0,
       doc: /* Make buffer BUFFER-OR-NAME current for editing operations.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer.
See also `with-current-buffer' when you want to make a buffer current
temporarily.  This function does not display the buffer, so its effect
ends when the current command terminates.  Use `switch-to-buffer' or
`pop-to-buffer' to switch buffers permanently.
The return value is the buffer made current.  */)
  (register Lisp_Object buffer_or_name)
{
  register Lisp_Object buffer;
  buffer = Fget_buffer (buffer_or_name);
  if (NILP (buffer))
    nsberror (buffer_or_name);
  if (!BUFFER_LIVE_P (XBUFFER (buffer)))
    error ("Selecting deleted buffer");
  set_buffer_internal (XBUFFER (buffer));
  return buffer;
}

void
restore_buffer (Lisp_Object buffer_or_name)
{
  Fset_buffer (buffer_or_name);
}

/* Set the current buffer to BUFFER provided if it is alive.  */

void
set_buffer_if_live (Lisp_Object buffer)
{
  if (BUFFER_LIVE_P (XBUFFER (buffer)))
    set_buffer_internal (XBUFFER (buffer));
}

DEFUN ("barf-if-buffer-read-only", Fbarf_if_buffer_read_only,
				   Sbarf_if_buffer_read_only, 0, 1, 0,
       doc: /* Signal a `buffer-read-only' error if the current buffer is read-only.
If the text under POSITION (which defaults to point) has the
`inhibit-read-only' text property set, the error will not be raised.  */)
  (Lisp_Object position)
{
  if (NILP (position))
    XSETFASTINT (position, PT);
  else
    CHECK_FIXNUM (position);

  if (!NILP (BVAR (current_buffer, read_only))
      && NILP (Vinhibit_read_only)
      && NILP (Fget_text_property (position, Qinhibit_read_only, Qnil)))
    xsignal1 (Qbuffer_read_only, Fcurrent_buffer ());
  return Qnil;
}

DEFUN ("erase-buffer", Ferase_buffer, Serase_buffer, 0, 0, "*",
       doc: /* Delete the entire contents of the current buffer.
Any narrowing restriction in effect (see `narrow-to-region') is removed,
so the buffer is truly empty after this.  */)
  (void)
{
  Fwiden ();

  del_range (BEG, Z);

  current_buffer->last_window_start = 1;
  /* Prevent warnings, or suspension of auto saving, that would happen
     if future size is less than past size.  Use of erase-buffer
     implies that the future text is not really related to the past text.  */
  XSETFASTINT (BVAR (current_buffer, save_length), 0);
  bset_bidi_display_reordering (current_buffer, Qnil);
  return Qnil;
}

void
validate_region (Lisp_Object *b, Lisp_Object *e)
{
  EMACS_INT beg = fix_position (*b), end = fix_position (*e);

  if (end < beg)
    {
      EMACS_INT tem = beg;  beg = end;  end = tem;
    }

  if (!(BEGV <= beg && end <= ZV))
    args_out_of_range_3 (Fcurrent_buffer (), *b, *e);

  *b = make_fixnum (beg);
  *e = make_fixnum (end);
}

/* Advance BYTE_POS up to a character boundary
   and return the adjusted position.  */

ptrdiff_t
advance_to_char_boundary (ptrdiff_t byte_pos)
{
  int c;

  if (byte_pos == BEG)
    /* Beginning of buffer is always a character boundary.  */
    return BEG;

  c = FETCH_BYTE (byte_pos);
  if (!CHAR_HEAD_P (c))
    {
      /* We should advance BYTE_POS only when C is a constituent of a
         multibyte sequence.  */
      ptrdiff_t orig_byte_pos = byte_pos;

      do
	{
	  byte_pos--;
	  c = FETCH_BYTE (byte_pos);
	}
      while (!CHAR_HEAD_P (c) && byte_pos > BEG);
      byte_pos += next_char_len (byte_pos);
      if (byte_pos < orig_byte_pos)
	byte_pos = orig_byte_pos;
      /* If C is a constituent of a multibyte sequence, BYTE_POS was
         surely advance to the correct character boundary.  If C is
         not, BYTE_POS was unchanged.  */
    }

  return byte_pos;
}

static void
swap_buffer_overlays (struct buffer *buffer, struct buffer *other)
{
  struct itree_node *node;

  if (MODE_OVERLAY_INDIRECT_P (buffer))
    return;

  ITREE_FOREACH (node, buffer->overlays, PTRDIFF_MIN, PTRDIFF_MAX, ASCENDING)
    XOVERLAY (node->data)->buffer = other;

  ITREE_FOREACH (node, other->overlays, PTRDIFF_MIN, PTRDIFF_MAX, ASCENDING)
    XOVERLAY (node->data)->buffer = buffer;

  /* Swap the interval trees. */
  void *tmp = buffer->overlays;
  buffer->overlays = other->overlays;
  other->overlays = tmp;
}

DEFUN ("buffer-swap-text", Fbuffer_swap_text, Sbuffer_swap_text,
       1, 1, 0,
       doc: /* Swap the text between current buffer and BUFFER.
Using this function from `save-excursion' might produce surprising
results, see Info node `(elisp)Swapping Text'.  */)
  (Lisp_Object buffer)
{
  struct buffer *other_buffer;
  CHECK_BUFFER (buffer);
  other_buffer = XBUFFER (buffer);

  if (!BUFFER_LIVE_P (other_buffer))
    error ("Cannot swap a dead buffer's text");

  /* Actually, it probably works just fine.
   * if (other_buffer == current_buffer)
   *   error ("Cannot swap a buffer's text with itself"); */

  /* Actually, this may be workable as well, tho probably only if they're
     *both* indirect.  */
  if (other_buffer->base_buffer
      || current_buffer->base_buffer)
    error ("Cannot swap indirect buffers's text");

  { /* This is probably harder to make work.  */
    Lisp_Object tail, other;
    FOR_EACH_LIVE_BUFFER (tail, other)
      if (XBUFFER (other)->base_buffer == other_buffer
	  || XBUFFER (other)->base_buffer == current_buffer)
	error ("One of the buffers to swap has indirect buffers");
  }

#define swapfield(field, type) \
  do {							\
    type tmp##field = other_buffer->field;		\
    other_buffer->field = current_buffer->field;	\
    current_buffer->field = tmp##field;			\
  } while (0)
#define swapfield_(field, type) \
  do {							\
    type tmp##field = BVAR (other_buffer, field);		\
    bset_##field (other_buffer, BVAR (current_buffer, field));	\
    bset_##field (current_buffer, tmp##field);			\
  } while (0)

  swapfield (own_text, struct buffer_text);
  eassert (current_buffer->text == &current_buffer->own_text);
  eassert (other_buffer->text == &other_buffer->own_text);
#ifdef REL_ALLOC
  r_alloc_reset_variable ((void **) &current_buffer->own_text.beg,
			  (void **) &other_buffer->own_text.beg);
  r_alloc_reset_variable ((void **) &other_buffer->own_text.beg,
			  (void **) &current_buffer->own_text.beg);
#endif /* REL_ALLOC */

  swapfield (pt, ptrdiff_t);
  swapfield (pt_byte, ptrdiff_t);
  swapfield (begv, ptrdiff_t);
  swapfield (begv_byte, ptrdiff_t);
  swapfield (zv, ptrdiff_t);
  swapfield (zv_byte, ptrdiff_t);
  eassert (!current_buffer->base_buffer);
  eassert (!other_buffer->base_buffer);
  swapfield (indirections, ptrdiff_t);
  current_buffer->clip_changed = 1;	other_buffer->clip_changed = 1;
  swapfield (newline_cache, struct region_cache *);
  swapfield (width_run_cache, struct region_cache *);
  swapfield (bidi_paragraph_cache, struct region_cache *);
  current_buffer->prevent_redisplay_optimizations_p = 1;
  other_buffer->prevent_redisplay_optimizations_p = 1;
  swapfield_ (undo_list, Lisp_Object);
  swapfield_ (mark, Lisp_Object);
  swapfield_ (mark_active, Lisp_Object); /* Belongs with the `mark'.  */
  swapfield_ (enable_multibyte_characters, Lisp_Object);
  swapfield_ (initial_char_script, Lisp_Object);
  swapfield_ (bidi_display_reordering, Lisp_Object);
  swapfield_ (bidi_paragraph_direction, Lisp_Object);
  swapfield_ (bidi_paragraph_separate_re, Lisp_Object);
  swapfield_ (bidi_paragraph_start_re, Lisp_Object);
  /* FIXME: Not sure what we should do with these *_marker fields.
     Hopefully they're just nil anyway.  */
  swapfield_ (pt_marker, Lisp_Object);
  swapfield_ (begv_marker, Lisp_Object);
  swapfield_ (zv_marker, Lisp_Object);
  bset_point_before_scroll (current_buffer, Qnil);
  bset_point_before_scroll (other_buffer, Qnil);

  modiff_incr (&current_buffer->text->modiff);
  modiff_incr (&other_buffer->text->modiff);
  modiff_incr (&current_buffer->text->chars_modiff);
  modiff_incr (&other_buffer->text->chars_modiff);
  modiff_incr (&current_buffer->text->overlay_modiff);
  modiff_incr (&other_buffer->text->overlay_modiff);
  current_buffer->text->beg_unchanged = current_buffer->text->gpt;
  current_buffer->text->end_unchanged = current_buffer->text->gpt;
  other_buffer->text->beg_unchanged = other_buffer->text->gpt;
  other_buffer->text->end_unchanged = other_buffer->text->gpt;
  swap_buffer_overlays (current_buffer, other_buffer);
  {
    struct Lisp_Marker *m;
    for (m = BUF_MARKERS (current_buffer); m; m = m->next)
      if (m->buffer == other_buffer)
	m->buffer = current_buffer;
      else
	/* Since there's no indirect buffer in sight, markers on
	   BUF_MARKERS(buf) should either be for `buf' or dead.  */
	eassert (!m->buffer);
    for (m = BUF_MARKERS (other_buffer); m; m = m->next)
      if (m->buffer == current_buffer)
	m->buffer = other_buffer;
      else
	/* Since there's no indirect buffer in sight, markers on
	   BUF_MARKERS(buf) should either be for `buf' or dead.  */
	eassert (!m->buffer);
  }
  { /* Some of the C code expects that both window markers of a
       live window points to that window's buffer.  So since we
       just swapped the markers between the two buffers, we need
       to undo the effect of this swap for window markers.  */
    Lisp_Object w = selected_window, ws = Qnil;
    Lisp_Object buf1, buf2;
    XSETBUFFER (buf1, current_buffer); XSETBUFFER (buf2, other_buffer);

    while (NILP (Fmemq (w, ws)))
      {
	ws = Fcons (w, ws);
	if (MARKERP (XWINDOW (w)->pointm)
	    && (EQ (XWINDOW (w)->contents, buf1)
		|| EQ (XWINDOW (w)->contents, buf2)))
	  Fset_marker (XWINDOW (w)->pointm,
		       make_fixnum
		       (BUF_BEGV (XBUFFER (XWINDOW (w)->contents))),
		       XWINDOW (w)->contents);
	/* Blindly copied from pointm part.  */
	if (MARKERP (XWINDOW (w)->old_pointm)
	    && (EQ (XWINDOW (w)->contents, buf1)
		|| EQ (XWINDOW (w)->contents, buf2)))
	  Fset_marker (XWINDOW (w)->old_pointm,
		       make_fixnum
		       (BUF_BEGV (XBUFFER (XWINDOW (w)->contents))),
		       XWINDOW (w)->contents);
	if (MARKERP (XWINDOW (w)->start)
	    && (EQ (XWINDOW (w)->contents, buf1)
		|| EQ (XWINDOW (w)->contents, buf2)))
	  Fset_marker (XWINDOW (w)->start,
		       make_fixnum
		       (XBUFFER (XWINDOW (w)->contents)->last_window_start),
		       XWINDOW (w)->contents);
	w = Fnext_window (w, Qt, Qt);
      }
  }

  if (current_buffer->text->intervals)
    (eassert (EQ (current_buffer->text->intervals->up.obj, buffer)),
     XSETBUFFER (current_buffer->text->intervals->up.obj, current_buffer));
  if (other_buffer->text->intervals)
    (eassert (EQ (other_buffer->text->intervals->up.obj, Fcurrent_buffer ())),
     XSETBUFFER (other_buffer->text->intervals->up.obj, other_buffer));

  return Qnil;
}

DEFUN ("set-buffer-multibyte", Fset_buffer_multibyte, Sset_buffer_multibyte,
       1, 1, 0,
       doc: /* Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
In these cases, the buffer contents remain unchanged as a sequence of
bytes but the contents viewed as characters do change.
If FLAG is `to', this makes the buffer a multibyte buffer by changing
all eight-bit bytes to eight-bit characters.
If the multibyte flag was really changed, undo information of the
current buffer is cleared.  */)
  (Lisp_Object flag)
{
  struct Lisp_Marker *tail, *markers;
  Lisp_Object btail, other;
  ptrdiff_t begv, zv;
  bool narrowed = (BEG != BEGV || Z != ZV);
  bool modified_p = !NILP (Fbuffer_modified_p (Qnil));
  Lisp_Object old_undo = BVAR (current_buffer, undo_list);

  if (current_buffer->base_buffer)
    error ("Cannot do `set-buffer-multibyte' on an indirect buffer");

  /* Do nothing if nothing actually changes.  */
  if (NILP (flag) == NILP (BVAR (current_buffer, enable_multibyte_characters)))
    return flag;

  /* Don't record these buffer changes.  We will put a special undo entry
     instead.  */
  bset_undo_list (current_buffer, Qt);

  /* If the cached position is for this buffer, clear it out.  */
  clear_charpos_cache (current_buffer);

  if (NILP (flag))
    begv = BEGV_BYTE, zv = ZV_BYTE;
  else
    begv = BEGV, zv = ZV;

  if (narrowed)
    error ("Changing multibyteness in a narrowed buffer");

  invalidate_buffer_caches (current_buffer, BEGV, ZV);

  if (NILP (flag))
    {
      ptrdiff_t pos, stop;
      unsigned char *p;

      /* Do this first, so it can use CHAR_TO_BYTE
	 to calculate the old correspondences.  */
      set_intervals_multibyte (false);
      set_overlays_multibyte (false);

      bset_enable_multibyte_characters (current_buffer, Qnil);

      Z = Z_BYTE;
      BEGV = BEGV_BYTE;
      ZV = ZV_BYTE;
      GPT = GPT_BYTE;
      TEMP_SET_PT_BOTH (PT_BYTE, PT_BYTE);


      for (tail = BUF_MARKERS (current_buffer); tail; tail = tail->next)
	tail->charpos = tail->bytepos;

      /* Convert multibyte form of 8-bit characters to unibyte.  */
      pos = BEG;
      stop = GPT;
      p = BEG_ADDR;
      while (1)
	{
	  if (pos == stop)
	    {
	      if (pos == Z)
		break;
	      p = GAP_END_ADDR;
	      stop = Z;
	    }
	  if (ASCII_CHAR_P (*p))
	    p++, pos++;
	  else if (CHAR_BYTE8_HEAD_P (*p))
	    {
	      int bytes, c = string_char_and_length (p, &bytes);
	      /* Delete all bytes for this 8-bit character but the
		 last one, and change the last one to the character
		 code.  */
	      bytes--;
	      del_range_2 (pos, pos, pos + bytes, pos + bytes);
	      p = GAP_END_ADDR;
	      *p++ = c;
	      pos++;
	      if (begv > pos)
		begv -= bytes;
	      if (zv > pos)
		zv -= bytes;
	      stop = Z;
	    }
	  else
	    {
	      int bytes = BYTES_BY_CHAR_HEAD (*p);
	      p += bytes, pos += bytes;
	    }
	}
    }
  else
    {
      ptrdiff_t pt = PT;
      ptrdiff_t pos, stop;
      unsigned char *p, *pend;

      /* Be sure not to have a multibyte sequence striding over the GAP.
	 Ex: We change this: "...abc\302 _GAP_ \241def..."
	     to: "...abc _GAP_ \302\241def..."  */

      if (EQ (flag, Qt)
	  && GPT_BYTE > 1 && GPT_BYTE < Z_BYTE
	  && !CHAR_HEAD_P (*(GAP_END_ADDR)))
	{
	  unsigned char *q = GAP_BEG_ADDR - 1;

	  while (!CHAR_HEAD_P (*q) && q > BEG_ADDR) q--;
	  if (LEADING_CODE_P (*q))
	    {
	      ptrdiff_t new_gpt = GPT_BYTE - (GAP_BEG_ADDR - q);

	      move_gap (new_gpt, new_gpt);
	    }
	}

      /* Make the buffer contents valid as multibyte by converting
	 8-bit characters to multibyte form.  */
      pos = BEG;
      stop = GPT;
      p = BEG_ADDR;
      pend = GAP_BEG_ADDR;
      while (1)
	{
	  int bytes;

	  if (pos == stop)
	    {
	      if (pos == Z)
		break;
	      p = GAP_END_ADDR;
	      pend = Z_ADDR;
	      stop = Z;
	    }

	  if (ASCII_CHAR_P (*p))
	    p++, pos++;
	  else if (EQ (flag, Qt)
		   && 0 < (bytes = multibyte_length (p, pend, true, false)))
	    p += bytes, pos += bytes;
	  else
	    {
	      unsigned char tmp[MAX_MULTIBYTE_LENGTH];
	      int c;

	      c = BYTE8_TO_CHAR (*p);
	      bytes = CHAR_STRING (c, tmp);
	      *p = tmp[0];
	      TEMP_SET_PT_BOTH (pos + 1, pos + 1);
	      bytes--;
	      insert_1_both ((char *) tmp + 1, bytes, bytes, 1, 0, 0);
	      /* Now the gap is after the just inserted data.  */
	      pos = GPT;
	      p = GAP_END_ADDR;
	      if (pos <= begv)
		begv += bytes;
	      if (pos <= zv)
		zv += bytes;
	      if (pos <= pt)
		pt += bytes;
	      pend = Z_ADDR;
	      stop = Z;
	    }
	}

      if (pt != PT)
	TEMP_SET_PT (pt);

      /* Do this first, so that chars_in_text asks the right question.
	 set_intervals_multibyte needs it too.  */
      bset_enable_multibyte_characters (current_buffer, Qt);

      GPT_BYTE = advance_to_char_boundary (GPT_BYTE);
      GPT = chars_in_text (BEG_ADDR, GPT_BYTE - BEG_BYTE) + BEG;

      Z = chars_in_text (GAP_END_ADDR, Z_BYTE - GPT_BYTE) + GPT;

      BEGV_BYTE = advance_to_char_boundary (BEGV_BYTE);
      if (BEGV_BYTE > GPT_BYTE)
	BEGV = chars_in_text (GAP_END_ADDR, BEGV_BYTE - GPT_BYTE) + GPT;
      else
	BEGV = chars_in_text (BEG_ADDR, BEGV_BYTE - BEG_BYTE) + BEG;

      ZV_BYTE = advance_to_char_boundary (ZV_BYTE);
      if (ZV_BYTE > GPT_BYTE)
	ZV = chars_in_text (GAP_END_ADDR, ZV_BYTE - GPT_BYTE) + GPT;
      else
	ZV = chars_in_text (BEG_ADDR, ZV_BYTE - BEG_BYTE) + BEG;

      {
	ptrdiff_t byte = advance_to_char_boundary (PT_BYTE);
	ptrdiff_t position;

	if (byte > GPT_BYTE)
	  position = chars_in_text (GAP_END_ADDR, byte - GPT_BYTE) + GPT;
	else
	  position = chars_in_text (BEG_ADDR, byte - BEG_BYTE) + BEG;
	TEMP_SET_PT_BOTH (position, byte);
      }

      tail = markers = BUF_MARKERS (current_buffer);

      /* This prevents BYTE_TO_CHAR (that is, buf_bytepos_to_charpos) from
	 getting confused by the markers that have not yet been updated.
	 It is also a signal that it should never create a marker.  */
      BUF_MARKERS (current_buffer) = NULL;

      for (; tail; tail = tail->next)
	{
	  tail->bytepos = advance_to_char_boundary (tail->bytepos);
	  tail->charpos = BYTE_TO_CHAR (tail->bytepos);
	}

      /* Make sure no markers were put on the chain
	 while the chain value was incorrect.  */
      if (BUF_MARKERS (current_buffer))
	emacs_abort ();

      BUF_MARKERS (current_buffer) = markers;

      /* Do this last, so it can calculate the new correspondences
	 between chars and bytes.  */
      /* FIXME: Is it worth the trouble, really?  Couldn't we just throw
         away all the text-properties instead of trying to guess how
         to adjust them?  AFAICT the result is not reliable anyway.  */
      set_intervals_multibyte (true);
      set_overlays_multibyte (true);
    }

  if (!EQ (old_undo, Qt))
    {
      /* Represent all the above changes by a special undo entry.  */
      bset_undo_list (current_buffer,
		      Fcons (list3 (Qapply,
				    intern ("set-buffer-multibyte"),
				    NILP (flag) ? Qt : Qnil),
			     old_undo));
    }

  current_buffer->prevent_redisplay_optimizations_p = 1;

  /* If buffer is shown in a window, let redisplay consider other windows.  */
  if (buffer_window_count (current_buffer))
    windows_or_buffers_changed = 10;

  /* Copy this buffer's new multibyte status
     into all of its indirect buffers.  */
  FOR_EACH_LIVE_BUFFER (btail, other)
    {
      struct buffer *o = XBUFFER (other);
      if (o->base_buffer == current_buffer && BUFFER_LIVE_P (o))
	{
	  BVAR (o, enable_multibyte_characters)
	    = BVAR (current_buffer, enable_multibyte_characters);
	  o->prevent_redisplay_optimizations_p = true;
	}
    }

  /* Restore the modifiedness of the buffer.  */
  if (!modified_p && !NILP (Fbuffer_modified_p (Qnil)))
    Fset_buffer_modified_p (Qnil);

  /* Update coding systems of this buffer's process (if any).  */
  {
    Lisp_Object process;

    process = Fget_buffer_process (Fcurrent_buffer ());
    if (PROCESSP (process))
      setup_process_coding_systems (process);
  }

  return flag;
}

DEFUN ("kill-all-local-variables", Fkill_all_local_variables,
       Skill_all_local_variables, 0, 1, 0,
       doc: /* Switch to Fundamental mode by killing current buffer's local variables.
Most local variable bindings are eliminated so that the default values
become effective once more.  Also, the syntax table is set from
`standard-syntax-table', the local keymap is set to nil,
and the abbrev table from `fundamental-mode-abbrev-table'.
This function also forces redisplay of the mode line.

Every function to select a new major mode starts by
calling this function.

As a special exception, local variables whose names have a non-nil
`permanent-local' property are not eliminated by this function.  If
the optional KILL-PERMANENT argument is non-nil, clear out these local
variables, too.

The first thing this function does is run
the normal hook `change-major-mode-hook'.  */)
  (Lisp_Object kill_permanent)
{
  run_hook (Qchange_major_mode_hook);
  reset_buffer_local_variables (current_buffer, !NILP (kill_permanent));
  /* Force mode-line redisplay.  Useful here because all major mode
     commands call this function.  */
  bset_update_mode_line (current_buffer);

  return Qnil;
}

/* Find all the overlays in the current buffer that overlap the range
   [BEG, END).

   If EMPTY is true, include empty overlays in that range and also at
   END, provided END denotes the position at the end of the accessible
   part of the buffer.

   If TRAILING is true, include overlays that begin at END, provided
   END denotes the position at the end of the accessible part of the
   buffer.

   Return the number found, and store them in a vector in *VEC_PTR.
   Store in *LEN_PTR the size allocated for the vector.
   Store in *NEXT_PTR the next position after POS where an overlay starts,
     or ZV if there are no more overlays.
   NEXT_PTR may be 0, meaning don't store that info.

   *VEC_PTR and *LEN_PTR should contain a valid vector and size
   when this function is called.

   If EXTEND, make the vector bigger if necessary.  If not, never
   extend the vector, and store only as many overlays as will fit.
   But still return the total number of overlays.
*/

static ptrdiff_t
overlays_in (ptrdiff_t beg, ptrdiff_t end, bool extend,
	     Lisp_Object **vec_ptr, ptrdiff_t *len_ptr,
	     bool empty, bool trailing,
             ptrdiff_t *next_ptr)
{
  ptrdiff_t idx = 0;
  ptrdiff_t len = *len_ptr;
  ptrdiff_t next = ZV;
  Lisp_Object *vec = *vec_ptr;
  struct itree_node *node;

  /* Extend the search range if overlays beginning at ZV are
     wanted.  */
  ptrdiff_t search_end = ZV;
  if (end >= ZV && (empty || trailing))
    ++search_end;

  ITREE_FOREACH (node, current_buffer->overlays, beg, search_end,
                 ASCENDING)
    {
      if (node->begin > end)
        {
          next = min (next, node->begin);
          break;
        }
      else if (node->begin == end)
        {
          next = node->begin;
          if ((!empty || end < ZV) && beg < end)
            break;
          if (empty && node->begin != node->end)
            continue;
        }

      if (!empty && node->begin == node->end)
        continue;

      if (extend && idx == len)
        {
          vec = xpalloc (vec, len_ptr, 1, OVERLAY_COUNT_MAX,
                         sizeof *vec);
          *vec_ptr = vec;
          len = *len_ptr;
        }
      if (idx < len)
        vec[idx] = node->data;
      /* Keep counting overlays even if we can't return them all.  */
      idx++;
    }
  if (next_ptr)
    *next_ptr = next ? next : ZV;

  return idx;
}

/* Find all non-empty overlays in the current buffer that contain
   position POS.

   See overlays_in for the meaning of the arguments.
  */

ptrdiff_t
overlays_at (ptrdiff_t pos, bool extend,
             Lisp_Object **vec_ptr, ptrdiff_t *len_ptr,
             ptrdiff_t *next_ptr)
{
  return overlays_in (pos, pos + 1, extend, vec_ptr, len_ptr,
		      false, true, next_ptr);
}

ptrdiff_t
next_overlay_change (ptrdiff_t pos, bool callback_p)
{
  ptrdiff_t ret = ZV; // ret gets smaller and smaller
  if (current_buffer->overlays)
    {
      struct itree_iterator iter, *iter_p   /* [pos, ret) */
	= itree_iterator_start (&iter, current_buffer->overlays,
				pos, ret, ITREE_ASCENDING);

      for (struct itree_node *node = itree_iterator_next (iter_p);
	   node != NULL;
	   node = itree_iterator_next (iter_p))
	{
	  if (!callback_p
	      || OVERLAY_ON_ENTER (node->data)
	      || OVERLAY_ON_EXIT (node->data))
	    {
	      if (node->begin > pos)
		{
		  /* Ordinarily first iteration. */
		  eassert (node->begin < ret);
		  ret = node->begin;
		  break;
		}
	      else if (node->begin < node->end && node->end < ret)
		{
		  /* Overlay starts at POS? */
		  ret = node->end;
		  itree_iterator_narrow (iter_p, pos, ret);
		}
	    }
	}
    }
  return ret;
}

ptrdiff_t
previous_overlay_change (ptrdiff_t pos, bool callback_p)
{
  ptrdiff_t ret = BEGV; // ret gets bigger and bigger
  if (current_buffer->overlays)
    {
      struct itree_iterator iter, *iter_p   /* [ret, pos) */
	= itree_iterator_start (&iter, current_buffer->overlays,
				ret, pos, ITREE_DESCENDING);

      /* Since tree is sorted by interval-begin, and we are comparing by
	 interval-end, an exhaustive search is required.  */
      for (struct itree_node *node = itree_iterator_next (iter_p);
	   node != NULL;
	   node = itree_iterator_next (iter_p))
	{
	  if (!callback_p
	      || OVERLAY_ON_ENTER (node->data)
	      || OVERLAY_ON_EXIT (node->data))
	    {
	      ret = (node->end < pos)
		? node->end
		: max (ret, node->begin);
	      itree_iterator_narrow (iter_p, ret, pos);
	    }
	}
    }
  return ret;
}

/* Return true if there exists an overlay with a non-nil
   `mouse-face' property overlapping OVERLAY.  */

bool
mouse_face_overlay_overlaps (Lisp_Object overlay)
{
  ptrdiff_t start = OVERLAY_START (overlay);
  ptrdiff_t end = OVERLAY_END (overlay);
  Lisp_Object tem;
  struct itree_node *node;

  ITREE_FOREACH (node, current_buffer->overlays,
                 start, min (end, ZV) + 1,
                 ASCENDING)
    {
      if (node->begin < end && node->end > start
          && node->begin < node->end
          && !EQ (node->data, overlay)
          && (tem = Foverlay_get (overlay, Qmouse_face),
	      !NILP (tem)))
	return true;
    }
  return false;
}

/* Return the value of the 'display-line-numbers-disable' property at
   EOB, if there's an overlay at ZV with a non-nil value of that property.  */
bool
disable_line_numbers_overlay_at_eob (void)
{
  Lisp_Object tem = Qnil;
  struct itree_node *node;

  ITREE_FOREACH (node, current_buffer->overlays, ZV, ZV, ASCENDING)
    {
      if ((tem = Foverlay_get (node->data, Qdisplay_line_numbers_disable),
	   !NILP (tem)))
	return true;
    }
  return false;
}

/* Fast function to just test if we're at an overlay boundary.

   Returns true if some overlay starts or ends (or both) at POS,
*/
bool
overlay_touches_p (ptrdiff_t pos)
{
  struct itree_node *node;

  /* We need to find overlays ending in pos, as well as empty ones at
     pos. */
  ITREE_FOREACH (node, current_buffer->overlays, pos - 1, pos + 1, DESCENDING)
    if (node->begin == pos || node->end == pos)
      return true;
  return false;
}

int
compare_overlays (const void *v1, const void *v2)
{
  const struct sortvec *s1 = v1;
  const struct sortvec *s2 = v2;
  /* Return 1 if s1 should take precedence, -1 if v2 should take precedence,
     and 0 if they're equal.  */
  if (s1->priority != s2->priority)
    return s1->priority < s2->priority ? -1 : 1;
  /* If the priority is equal, give precedence to the one not covered by the
     other.  If neither covers the other, obey spriority.  */
  else if (s1->beg < s2->beg)
    return (s1->end < s2->end && s1->spriority > s2->spriority ? 1 : -1);
  else if (s1->beg > s2->beg)
    return (s1->end > s2->end && s1->spriority < s2->spriority ? -1 : 1);
  else if (s1->end != s2->end)
    return s2->end < s1->end ? -1 : 1;
  else if (s1->spriority != s2->spriority)
    return (s1->spriority < s2->spriority ? -1 : 1);
  else if (EQ (s1->overlay, s2->overlay))
    return 0;
  else
    /* Avoid the non-determinism of qsort by choosing an arbitrary ordering
       between "equal" overlays.  The result can still change between
       invocations of Emacs, but it won't change in the middle of
       `find_field' (bug#6830).  */
    return XLI (s1->overlay) < XLI (s2->overlay) ? -1 : 1;
}

void
make_sortvec_item (struct sortvec *item, Lisp_Object overlay)
{
  Lisp_Object tem;
  /* This overlay is good and counts: put it into sortvec.  */
  item->overlay = overlay;
  item->beg = OVERLAY_START (overlay);
  item->end = OVERLAY_END (overlay);
  tem = Foverlay_get (overlay, Qpriority);
  if (NILP (tem))
    {
      item->priority = 0;
      item->spriority = 0;
    }
  else if (FIXNUMP (tem))
    {
      item->priority = XFIXNUM (tem);
      item->spriority = 0;
    }
  else if (CONSP (tem))
    {
      Lisp_Object car = XCAR (tem);
      Lisp_Object cdr = XCDR (tem);
      item->priority  = FIXNUMP (car) ? XFIXNUM (car) : 0;
      item->spriority = FIXNUMP (cdr) ? XFIXNUM (cdr) : 0;
    }
}
/* Sort an array of overlays by priority.  The array is modified in place.
   The return value is the new size; this may be smaller than the original
   size if some of the overlays were invalid or were window-specific.  */
ptrdiff_t
sort_overlays (Lisp_Object *overlay_vec, ptrdiff_t noverlays, struct window *w)
{
  ptrdiff_t i, j;
  USE_SAFE_ALLOCA;
  struct sortvec *sortvec;

  SAFE_NALLOCA (sortvec, 1, noverlays);

  /* Put the valid and relevant overlays into sortvec.  */

  for (i = 0, j = 0; i < noverlays; i++)
    {
      Lisp_Object overlay;

      overlay = overlay_vec[i];
      if (OVERLAYP (overlay)
	  && OVERLAY_START (overlay) > 0
	  && OVERLAY_END (overlay) > 0)
	{
          /* If we're interested in a specific window, then ignore
             overlays that are limited to some other window.  */
          if (w && !overlay_matches_window (w, overlay))
            continue;
          make_sortvec_item (sortvec + j, overlay);
	  j++;
	}
    }
  noverlays = j;

  /* Sort the overlays into the proper order: increasing priority.  */

  if (noverlays > 1)
    qsort (sortvec, noverlays, sizeof (struct sortvec), compare_overlays);

  for (i = 0; i < noverlays; i++)
    overlay_vec[i] = sortvec[i].overlay;

  SAFE_FREE ();
  return (noverlays);
}

struct sortstr
{
  Lisp_Object string, string2;
  ptrdiff_t size;
  EMACS_INT priority;
};

struct sortstrlist
{
  struct sortstr *buf;	/* An array that expands as needed; never freed.  */
  ptrdiff_t size;	/* Allocated length of that array.  */
  ptrdiff_t used;	/* How much of the array is currently in use.  */
  ptrdiff_t bytes;	/* Total length of the strings in buf.  */
};

/* Buffers for storing information about the overlays touching a given
   position.  These could be automatic variables in overlay_strings, but
   it's more efficient to hold onto the memory instead of repeatedly
   allocating and freeing it.  */
static struct sortstrlist overlay_heads, overlay_tails;
static unsigned char *overlay_str_buf;

/* Allocated length of overlay_str_buf.  */
static ptrdiff_t overlay_str_len;

/* A comparison function suitable for passing to qsort.  */
static int
cmp_for_strings (const void *as1, const void *as2)
{
  struct sortstr const *s1 = as1;
  struct sortstr const *s2 = as2;
  if (s1->size != s2->size)
    return s2->size < s1->size ? -1 : 1;
  if (s1->priority != s2->priority)
    return s1->priority < s2->priority ? -1 : 1;
  return 0;
}

static void
record_overlay_string (struct sortstrlist *ssl, Lisp_Object str,
		       Lisp_Object str2, Lisp_Object pri, ptrdiff_t size)
{
  ptrdiff_t nbytes;

  if (ssl->used == ssl->size)
    ssl->buf = xpalloc (ssl->buf, &ssl->size, 5, -1, sizeof *ssl->buf);
  ssl->buf[ssl->used].string = str;
  ssl->buf[ssl->used].string2 = str2;
  ssl->buf[ssl->used].size = size;
  ssl->buf[ssl->used].priority = (FIXNUMP (pri) ? XFIXNUM (pri) : 0);
  ssl->used++;

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    nbytes = SCHARS (str);
  else if (!STRING_MULTIBYTE (str))
    nbytes = count_size_as_multibyte (SDATA (str),
				      SBYTES (str));
  else
    nbytes = SBYTES (str);

  if (ckd_add (&nbytes, nbytes, ssl->bytes))
    memory_full (SIZE_MAX);
  ssl->bytes = nbytes;

  if (STRINGP (str2))
    {
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	nbytes = SCHARS (str2);
      else if (!STRING_MULTIBYTE (str2))
	nbytes = count_size_as_multibyte (SDATA (str2),
					  SBYTES (str2));
      else
	nbytes = SBYTES (str2);

      if (ckd_add (&nbytes, nbytes, ssl->bytes))
	memory_full (SIZE_MAX);
      ssl->bytes = nbytes;
    }
}

/* Concatenate the strings associated with overlays that begin or end
   at POS, ignoring overlays that are specific to windows other than W.
   The strings are concatenated in the appropriate order: shorter
   overlays nest inside longer ones, and higher priority inside lower.
   Normally all of the after-strings come first, but zero-sized
   overlays have their after-strings ride along with the
   before-strings because it would look strange to print them
   inside-out.

   Returns the concatenated string's length, and return the pointer to
   that string via PSTR, if that variable is non-NULL.  The storage of
   the concatenated strings may be overwritten by subsequent calls.  */

ptrdiff_t
overlay_strings (ptrdiff_t pos, struct window *w, unsigned char **pstr)
{
  bool multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  struct itree_node *node;

  overlay_heads.used = overlay_heads.bytes = 0;
  overlay_tails.used = overlay_tails.bytes = 0;

  ITREE_FOREACH (node, current_buffer->overlays, pos - 1, pos + 1, DESCENDING)
    {
      Lisp_Object overlay = node->data;
      eassert (OVERLAYP (overlay));

      ptrdiff_t startpos = node->begin;
      ptrdiff_t endpos = node->end;

      if (endpos != pos && startpos != pos)
	continue;
      Lisp_Object window = Foverlay_get (overlay, Qwindow);
      if (WINDOWP (window) && XWINDOW (window) != w)
	continue;
      Lisp_Object str;

      if (startpos == pos
	  && (str = Foverlay_get (overlay, Qbefore_string), STRINGP (str)))
	record_overlay_string (&overlay_heads, str,
			       (startpos == endpos
				? Foverlay_get (overlay, Qafter_string)
				: Qnil),
			       Foverlay_get (overlay, Qpriority),
			       endpos - startpos);
      else if (endpos == pos
	  && (str = Foverlay_get (overlay, Qafter_string), STRINGP (str)))
	record_overlay_string (&overlay_tails, str, Qnil,
			       Foverlay_get (overlay, Qpriority),
			       endpos - startpos);
    }

  if (overlay_tails.used > 1)
    qsort (overlay_tails.buf, overlay_tails.used, sizeof (struct sortstr),
	   cmp_for_strings);
  if (overlay_heads.used > 1)
    qsort (overlay_heads.buf, overlay_heads.used, sizeof (struct sortstr),
	   cmp_for_strings);
  if (overlay_heads.bytes || overlay_tails.bytes)
    {
      Lisp_Object tem;
      unsigned char *p;
      ptrdiff_t total;

      if (ckd_add (&total, overlay_heads.bytes, overlay_tails.bytes))
	memory_full (SIZE_MAX);
      if (total > overlay_str_len)
	overlay_str_buf = xpalloc (overlay_str_buf, &overlay_str_len,
				   total - overlay_str_len, -1, 1);

      p = overlay_str_buf;
      for (ptrdiff_t i = overlay_tails.used; --i >= 0;)
	{
	  ptrdiff_t nbytes;
	  tem = overlay_tails.buf[i].string;
	  nbytes = copy_text (SDATA (tem), p,
			      SBYTES (tem),
			      STRING_MULTIBYTE (tem), multibyte);
	  p += nbytes;
	}
      for (ptrdiff_t i = 0; i < overlay_heads.used; ++i)
	{
	  ptrdiff_t nbytes;
	  tem = overlay_heads.buf[i].string;
	  nbytes = copy_text (SDATA (tem), p,
			      SBYTES (tem),
			      STRING_MULTIBYTE (tem), multibyte);
	  p += nbytes;
	  tem = overlay_heads.buf[i].string2;
	  if (STRINGP (tem))
	    {
	      nbytes = copy_text (SDATA (tem), p,
				  SBYTES (tem),
				  STRING_MULTIBYTE (tem), multibyte);
	      p += nbytes;
	    }
	}
      if (p != overlay_str_buf + total)
	emacs_abort ();
      if (pstr)
	*pstr = overlay_str_buf;
      return total;
    }
  return 0;
}

void
adjust_overlays_for_insert (ptrdiff_t pos, ptrdiff_t length, bool before_markers)
{
  struct buffer *base = current_buffer->base_buffer
    ? current_buffer->base_buffer
    : current_buffer;
  itree_insert_gap (base->overlays, pos, length, before_markers);

  Lisp_Object tail, other;
  FOR_EACH_LIVE_BUFFER (tail, other)
    if (XBUFFER (other)->base_buffer == base
	&& XBUFFER (other)->overlays != base->overlays)
      itree_insert_gap (XBUFFER (other)->overlays, pos, length,
			before_markers);
}

static void
evaporate_overlays (struct itree_tree *overlays, ptrdiff_t pos)
{
  Lisp_Object hit_list = Qnil;
  struct itree_node *node;

  /* Delete zero-sized overlays if Qevaporate.  */
  ITREE_FOREACH (node, overlays, pos, pos, ASCENDING)
    {
      if (node->end == pos && node->begin == pos
          && !NILP (Foverlay_get (node->data, Qevaporate)))
        hit_list = Fcons (node->data, hit_list);
    }

  for (; CONSP (hit_list); hit_list = XCDR (hit_list))
    Fdelete_overlay (XCAR (hit_list));
}

void
adjust_overlays_for_delete (ptrdiff_t pos, ptrdiff_t length)
{
  struct buffer *base = current_buffer->base_buffer
    ? current_buffer->base_buffer
    : current_buffer;
  itree_delete_gap (base->overlays, pos, length);
  evaporate_overlays (base->overlays, pos);

  Lisp_Object tail, other;
  FOR_EACH_LIVE_BUFFER (tail, other)
    if (XBUFFER (other)->base_buffer == base
	&& XBUFFER (other)->overlays != base->overlays)
      {
	itree_delete_gap (XBUFFER (other)->overlays, pos, length);
	evaporate_overlays (XBUFFER (other)->overlays, pos);
      }
}

DEFUN ("overlayp", Foverlayp, Soverlayp, 1, 1, 0,
       doc: /* Return t if OBJECT is an overlay.  */)
  (Lisp_Object object)
{
  return (OVERLAYP (object) ? Qt : Qnil);
}

DEFUN ("make-overlay", Fmake_overlay, Smake_overlay, 2, 7, 0,
       doc: /* Create a new overlay with range BEG to END in BUFFER and return it.
If omitted, BUFFER defaults to the current buffer.
BEG and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
\(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
\(which means the text *is* included in the overlay).  */)
  (Lisp_Object beg, Lisp_Object end, Lisp_Object buffer,
   Lisp_Object front_advance, Lisp_Object rear_advance,
   Lisp_Object on_enter, Lisp_Object on_exit)
{
  Lisp_Object ov;
  struct buffer *b;

  if (NILP (buffer))
    XSETBUFFER (buffer, current_buffer);
  else
    CHECK_BUFFER (buffer);

  b = XBUFFER (buffer);
  if (!BUFFER_LIVE_P (b))
    error ("Attempt to create overlay in a dead buffer");

  if (MARKERP (beg) && !EQ (Fmarker_buffer (beg), buffer))
    signal_error ("Marker points into wrong buffer", beg);
  else if (MARKERP (end) && !EQ (Fmarker_buffer (end), buffer))
    signal_error ("Marker points into wrong buffer", end);
  else if (!NILP (on_enter) && !FUNCTIONP (on_enter))
    signal_error ("Enter callback must be a function", on_enter);
  else if (!NILP (on_exit) && !FUNCTIONP (on_exit))
    signal_error ("Exit callback must be a function", on_exit);

  CHECK_FIXNUM_COERCE_MARKER (beg);
  CHECK_FIXNUM_COERCE_MARKER (end);

  if (!NILP (on_enter))
    CHECK_TYPE (FUNCTIONP (on_enter), Qfunctionp, on_enter);
  if (!NILP (on_exit))
    CHECK_TYPE (FUNCTIONP (on_exit), Qfunctionp, on_exit);

  if (XFIXNUM (beg) > XFIXNUM (end))
    {
      Lisp_Object temp;
      temp = beg; beg = end; end = temp;
    }

  ptrdiff_t obeg = clip_to_bounds (BUF_BEG (b), XFIXNUM (beg), BUF_Z (b));
  ptrdiff_t oend = clip_to_bounds (obeg, XFIXNUM (end), BUF_Z (b));
  ov = build_overlay (!NILP (front_advance), !NILP (rear_advance),
		      Qnil, on_enter, on_exit);
  add_buffer_overlay (b, XOVERLAY (ov), obeg, oend);

  /* No redisplay needed as overlay births with no properties.  */
  return ov;
}

/* Mark a section of BUF as needing redisplay because of overlays changes.  */

static void
modify_overlay (struct buffer *buf, ptrdiff_t start, ptrdiff_t end)
{
  if (start > end)
    {
      ptrdiff_t temp = start;
      start = end;
      end = temp;
    }
  BUF_COMPUTE_UNCHANGED (buf, start, end);
  bset_redisplay (buf);
  modiff_incr (&BUF_OVERLAY_MODIFF (buf));
}

DEFUN ("move-overlay", Fmove_overlay, Smove_overlay, 3, 4, 0,
       doc: /* Move OVERLAY to BUFFER range spanning BEG to END.
A null target BUFFER results in OVERLAY being moved within its
associated buffer, or if it lacks one, to the current buffer.  */)
  (Lisp_Object overlay, Lisp_Object beg, Lisp_Object end, Lisp_Object buffer)
{
  CHECK_OVERLAY (overlay);
  Lisp_Object obuffer = Foverlay_buffer (overlay);
  if (NILP (buffer))
    {
      if (!NILP (obuffer))
	buffer = obuffer;
      else
	XSETBUFFER (buffer, current_buffer);
    }
  CHECK_BUFFER (buffer);

  if (NILP (Fbuffer_live_p (buffer)))
    error ("Attempt to move overlay to a dead buffer");

  if (MARKERP (beg) && !EQ (Fmarker_buffer (beg), buffer))
    signal_error ("Marker points into wrong buffer", beg);
  if (MARKERP (end) && !EQ (Fmarker_buffer (end), buffer))
    signal_error ("Marker points into wrong buffer", end);

  CHECK_FIXNUM_COERCE_MARKER (beg);
  CHECK_FIXNUM_COERCE_MARKER (end);

  if (XFIXNUM (beg) > XFIXNUM (end))
    {
      Lisp_Object temp;
      temp = beg; beg = end; end = temp;
    }

  ptrdiff_t n_beg = clip_to_bounds (BUF_BEG (XBUFFER (buffer)),
				    XFIXNUM (beg), BUF_Z (XBUFFER (buffer)));
  ptrdiff_t n_end = clip_to_bounds (n_beg, XFIXNUM (end), BUF_Z (XBUFFER (buffer)));

  if (EQ (buffer, obuffer))
    {
      /* Move within OVERLAY's buffer.  */
      itree_node_set_region (XBUFFER (buffer)->overlays,
			     XOVERLAY (overlay)->interval, n_beg, n_end);

      ptrdiff_t o_beg = OVERLAY_START (overlay);
      ptrdiff_t o_end = OVERLAY_END (overlay);

      /* Redisplay the area OVERLAY just left, or just enclosed.  */
      if (o_beg == n_beg)
	modify_overlay (XBUFFER (buffer), o_end, n_end);
      else if (o_end == n_end)
	modify_overlay (XBUFFER (buffer), o_beg, n_beg);
      else
	modify_overlay (XBUFFER (buffer), min (o_beg, n_beg), max (o_end, n_end));
    }
  else
    {
      /* Do a thorough redisplay if overlay changed buffers.  */
      Fdelete_overlay (overlay);
      add_buffer_overlay (XBUFFER (buffer), XOVERLAY (overlay), n_beg, n_end);
      modify_overlay (XBUFFER (buffer), n_beg, n_end);
    }

  /* Delete an evaporated overlay.  */
  if (n_beg == n_end && !NILP (Foverlay_get (overlay, Qevaporate)))
    Fdelete_overlay (overlay);

  return overlay;
}

DEFUN ("delete-overlay", Fdelete_overlay, Sdelete_overlay, 1, 1, 0,
       doc: /* Delete the OVERLAY from its buffer.  */)
  (Lisp_Object overlay)
{
  CHECK_OVERLAY (overlay);
  struct buffer *obuffer = OVERLAY_BUFFER (overlay);
  if (obuffer != NULL)
    {
      /* Turn off optimizations if overlay contained before- or
	 after-strings since they could contain newlines.  */
      if (!windows_or_buffers_changed
	  && (!NILP (Foverlay_get (overlay, Qbefore_string))
	      || !NILP (Foverlay_get (overlay, Qafter_string))))
	obuffer->prevent_redisplay_optimizations_p = 1;

      modify_overlay (obuffer, OVERLAY_START (overlay), OVERLAY_END (overlay));
      itree_remove (obuffer->overlays, XOVERLAY (overlay)->interval);
      /* Now kill mode-overlay buffer associated with OBUFFER.  */
      if (MODE_OVERLAY_INDIRECT_P (obuffer) /* don't touch base buffer! */
	  && !NILP (plist_get (OVERLAY_PLIST (overlay), Qmode_overlay_p))
	  && OVERLAY_END (overlay) > BUF_BEG (obuffer))
	{
	  mode_overlay_delete_bumpguard (obuffer, OVERLAY_END (overlay) - 1);
	  mode_overlay_delete_bumpguard (obuffer,
					 OVERLAY_START (overlay) == BUF_BEG (obuffer)
					 ? OVERLAY_START (overlay)
					 : OVERLAY_START (overlay) - 1);

	  if (!NILP (OVERLAY_ON_EXIT (overlay)))
	    call1 (OVERLAY_ON_EXIT (overlay), overlay);

	  /* Kill OBUFFER if no other overlays attached.
	     Erase all proximity tracking if no other mode overlays. */
	  bool kill_one = true; /* kill OBUFFER if no other overlays attached. */
	  bool erase_all = true; /* erase tracking if no more mode overlays. */
	  struct itree_node *node;
	  ITREE_FOREACH (node, obuffer->overlays, PTRDIFF_MIN,
			 PTRDIFF_MAX, POST_ORDER)
	    {
	      if (!EQ (node->data, overlay))
		{
		  /* recall obuffer->overlays is same as base->overlays */
		  if (!NILP (plist_get (OVERLAY_PLIST (node->data), Qmode_overlay_p)))
		    {
		      /* another mode-overlay buffer */
		      erase_all = false;
		      if (XOVERLAY (node->data)->buffer == obuffer)
			{
			  /* another mode overlay to this buffer */
			  kill_one = false;
			  break;
			}
		    }
		}
	    }
	  if (kill_one)
	    {
	      /* null out shared overlays to OBUFFER */
	      struct itree_node *node;
	      ITREE_FOREACH (node, obuffer->overlays, PTRDIFF_MIN,
			     PTRDIFF_MAX, POST_ORDER)
		if (XOVERLAY (node->data)->buffer == obuffer)
		  XOVERLAY (node->data)->buffer = NULL;

	      /* before Fkill_buffer */
	      struct buffer *base = obuffer->base_buffer;

	      /* kill the buffer */
	      obuffer->proximity = NULL; /* otherwise Fkill_buffer kills base */
	      Lisp_Object b; XSETBUFFER (b, obuffer);
	      Fkill_buffer (b);

	      if (erase_all)
		{
		  xfree (base->proximity);
		  base->proximity = NULL;
		}
	    }
	}
      XOVERLAY (overlay)->buffer = NULL;
    }
  return Qnil;
}

DEFUN ("delete-all-overlays", Fdelete_all_overlays, Sdelete_all_overlays, 0, 1, 0,
       doc: /* Delete all overlays of BUFFER.
BUFFER omitted or nil means delete all overlays of the current
buffer.  */)
  (Lisp_Object buffer)
{
  struct buffer *b = BUFFERP (buffer) ? XBUFFER (buffer) : current_buffer;
  delete_all_overlays (b);
  return Qnil;
}

/* Overlay dissection functions.  */

DEFUN ("overlay-start", Foverlay_start, Soverlay_start, 1, 1, 0,
       doc: /* Return the position at which OVERLAY starts.  */)
  (Lisp_Object overlay)
{
  CHECK_OVERLAY (overlay);
  if (!OVERLAY_BUFFER (overlay))
    return Qnil;
  return make_fixnum (OVERLAY_START (overlay));
}

DEFUN ("overlay-end", Foverlay_end, Soverlay_end, 1, 1, 0,
       doc: /* Return the position at which OVERLAY ends.  */)
  (Lisp_Object overlay)
{
  CHECK_OVERLAY (overlay);
  if (!OVERLAY_BUFFER (overlay))
    return Qnil;
  return make_fixnum (OVERLAY_END (overlay));
}

DEFUN ("overlay-buffer", Foverlay_buffer, Soverlay_buffer, 1, 1, 0,
       doc: /* Return the buffer OVERLAY belongs to.
Return nil if OVERLAY has been deleted.  */)
  (Lisp_Object overlay)
{
  Lisp_Object buffer;
  CHECK_OVERLAY (overlay);
  if (!OVERLAY_BUFFER (overlay))
    return Qnil;
  XSETBUFFER (buffer, OVERLAY_BUFFER (overlay));
  return buffer;
}

DEFUN ("overlay-properties", Foverlay_properties, Soverlay_properties, 1, 1, 0,
       doc: /* Return a list of the properties on OVERLAY.
This is a copy of OVERLAY's plist; modifying its conses has no effect on
OVERLAY.  */)
  (Lisp_Object overlay)
{
  CHECK_OVERLAY (overlay);
  return Fcopy_sequence (OVERLAY_PLIST (overlay));
}

DEFUN ("overlay-on-enter", Foverlay_on_enter, Soverlay_on_enter, 1, 1, 0,
       doc: /* Return the on-enter function of OVERLAY.  */)
  (Lisp_Object overlay)
{
  CHECK_OVERLAY (overlay);
  return OVERLAY_ON_ENTER (overlay);
}

DEFUN ("overlay-on-exit", Foverlay_on_exit, Soverlay_on_exit, 1, 1, 0,
       doc: /* Return the on-enter function of OVERLAY.  */)
  (Lisp_Object overlay)
{
  CHECK_OVERLAY (overlay);
  return OVERLAY_ON_EXIT (overlay);
}

DEFUN ("overlays-at", Foverlays_at, Soverlays_at, 1, 2, 0,
       doc: /* Return a list of the overlays that contain the character at POS.
If SORTED is non-nil, then sort them by decreasing priority.

Zero-length overlays that start and stop at POS are not included in
the return value.  Instead use `overlays-in' if those overlays are of
interest.  */)
  (Lisp_Object pos, Lisp_Object sorted)
{
  ptrdiff_t len, noverlays;
  Lisp_Object *overlay_vec;
  Lisp_Object result;

  CHECK_FIXNUM_COERCE_MARKER (pos);

  if (!buffer_has_overlays ())
    return Qnil;

  len = 10;
  /* We can't use alloca here because overlays_at can call xrealloc.  */
  overlay_vec = xmalloc (len * sizeof *overlay_vec);

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.  */
  noverlays = overlays_at (XFIXNUM (pos), true, &overlay_vec, &len, NULL);

  if (!NILP (sorted))
    noverlays = sort_overlays (overlay_vec, noverlays,
			       WINDOWP (sorted) ? XWINDOW (sorted) : NULL);

  /* Make a list of them all.  */
  result = Flist (noverlays, overlay_vec);

  /* The doc string says the list should be in decreasing order of
     priority, so we reverse the list, because sort_overlays sorts in
     the increasing order of priority.  */
  if (!NILP (sorted))
    result = Fnreverse (result);

  xfree (overlay_vec);
  return result;
}

DEFUN ("overlays-in", Foverlays_in, Soverlays_in, 2, 2, 0,
       doc: /* Return a list of the overlays that overlap the region BEG ... END.
Overlap means that at least one character is contained within the overlay
and also contained within the specified region.

Empty overlays are included in the result if they are located at BEG,
between BEG and END, or at END provided END denotes the position at the
end of the accessible part of the buffer.

The resulting list of overlays is in an arbitrary unpredictable order.  */)
  (Lisp_Object beg, Lisp_Object end)
{
  ptrdiff_t len, noverlays;
  Lisp_Object *overlay_vec;
  Lisp_Object result;

  CHECK_FIXNUM_COERCE_MARKER (beg);
  CHECK_FIXNUM_COERCE_MARKER (end);

  if (!buffer_has_overlays ())
    return Qnil;

  len = 10;
  overlay_vec = xmalloc (len * sizeof *overlay_vec);

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.  */
  noverlays = overlays_in (XFIXNUM (beg), XFIXNUM (end), 1, &overlay_vec, &len,
                           true, false, NULL);

  /* Make a list of them all.  */
  result = Flist (noverlays, overlay_vec);

  xfree (overlay_vec);
  return result;
}

DEFUN ("next-overlay-change", Fnext_overlay_change, Snext_overlay_change,
       1, 1, 0,
       doc: /* Return the next position after POS where an overlay starts or ends.
If there are no overlay boundaries from POS to (point-max),
the value is (point-max).  */)
  (Lisp_Object pos)
{
  CHECK_FIXNUM_COERCE_MARKER (pos);

  if (!buffer_has_overlays ())
    return make_fixnum (ZV);

  return make_fixnum (next_overlay_change (XFIXNUM (pos), false));
}

DEFUN ("previous-overlay-change", Fprevious_overlay_change,
       Sprevious_overlay_change, 1, 1, 0,
       doc: /* Return the previous position before POS where an overlay starts or ends.
If there are no overlay boundaries from (point-min) to POS,
the value is (point-min).  */)
  (Lisp_Object pos)
{

  CHECK_FIXNUM_COERCE_MARKER (pos);

  if (!buffer_has_overlays ())
    return make_fixnum (BEGV);

  return make_fixnum (previous_overlay_change (XFIXNUM (pos), false));
}

/* These functions are for debugging overlays.  */

DEFUN ("overlay-lists", Foverlay_lists, Soverlay_lists, 0, 0, 0,
       doc: /* Return a list giving all the overlays of the current buffer.

For backward compatibility, the value is actually a list that
holds another list; the overlays are in the inner list.
The list you get is a copy, so that changing it has no effect.
However, the overlays you get are the real objects that the buffer uses. */)
  (void)
{
  Lisp_Object overlays = Qnil;
  struct itree_node *node;

  ITREE_FOREACH (node, current_buffer->overlays, BEG, Z, DESCENDING)
    overlays = Fcons (node->data, overlays);

  return Fcons (overlays, Qnil);
}

DEFUN ("overlay-recenter", Foverlay_recenter, Soverlay_recenter, 1, 1, 0,
       doc: /* Recenter the overlays of the current buffer around position POS.
That makes overlay lookup faster for positions near POS (but perhaps slower
for positions far away from POS).

Since Emacs 29.1, this function is a no-op, because the implementation
of overlays changed and their lookup is now fast regardless of their
position in the buffer.  In particular, this function no longer affects
the value returned by `overlay-lists'.  */)
  (Lisp_Object pos)
{
  CHECK_FIXNUM_COERCE_MARKER (pos);
  /* Noop */
  return Qnil;
}

DEFUN ("overlay-get", Foverlay_get, Soverlay_get, 2, 2, 0,
       doc: /* Return PROP's associated value in OVERLAY's property list.  */)
  (Lisp_Object overlay, Lisp_Object prop)
{
  CHECK_OVERLAY (overlay);
  return lookup_char_property (XOVERLAY (overlay)->plist, prop, 0);
}

DEFUN ("overlay-put", Foverlay_put, Soverlay_put, 3, 3, 0,
       doc: /* Associate PROP with VALUE in OVERLAY's property list.
Return VALUE.*/)
  (Lisp_Object overlay, Lisp_Object prop, Lisp_Object value)
{
  bool found = false, changed = false;
  CHECK_OVERLAY (overlay);
  for (Lisp_Object tail = XOVERLAY (overlay)->plist;
       CONSP (CDR_SAFE (tail));
       tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), prop))
	{
	  found = true;
	  changed = !EQ (XCAR (XCDR (tail)), value);
	  XSETCAR (XCDR (tail), value);
	  break;
	}
    }

  if (!found)
    {
      changed = !NILP (value);
      set_overlay_plist (overlay,
			 Fcons (prop, Fcons (value, XOVERLAY (overlay)->plist)));
    }

  if (OVERLAY_BUFFER (overlay) != NULL)
    {
      if (changed)
	modify_overlay (OVERLAY_BUFFER (overlay), OVERLAY_START (overlay),
                        OVERLAY_END (overlay));
      if (EQ (prop, Qevaporate)
	  && !NILP (value)
	  && (OVERLAY_START (overlay) == OVERLAY_END (overlay)))
	Fdelete_overlay (overlay);
    }
  return value;
}

/* Subroutine of report_overlay_modification.  */

/* Lisp vector holding overlay hook functions to call.
   Vector elements come in pairs.
   Each even-index element is a list of hook functions.
   The following odd-index element is the overlay they came from.

   Before the buffer change, we fill in this vector
   as we call overlay hook functions.
   After the buffer change, we get the functions to call from this vector.
   This way we always call the same functions before and after the change.  */
static Lisp_Object last_overlay_modification_hooks;

/* Number of elements actually used in last_overlay_modification_hooks.  */
static ptrdiff_t last_overlay_modification_hooks_used;

/* Add one functionlist/overlay pair
   to the end of last_overlay_modification_hooks.  */

static void
add_overlay_mod_hooklist (Lisp_Object functionlist, Lisp_Object overlay)
{
  ptrdiff_t oldsize = ASIZE (last_overlay_modification_hooks);

  if (oldsize - 1 <= last_overlay_modification_hooks_used)
    last_overlay_modification_hooks =
      larger_vector (last_overlay_modification_hooks, 2, -1);
  ASET (last_overlay_modification_hooks, last_overlay_modification_hooks_used,
	functionlist); last_overlay_modification_hooks_used++;
  ASET (last_overlay_modification_hooks, last_overlay_modification_hooks_used,
	overlay);      last_overlay_modification_hooks_used++;
}

/* Run the modification-hooks of overlays that include
   any part of the text in START to END.
   If this change is an insertion, also
   run the insert-before-hooks of overlay starting at END,
   and the insert-after-hooks of overlay ending at START.

   This is called both before and after the modification.
   AFTER is true when we call after the modification.

   ARG1, ARG2, ARG3 are arguments to pass to the hook functions.
   When AFTER is nonzero, they are the start position,
   the position after the inserted new text,
   and the length of deleted or replaced old text.  */

void
report_overlay_modification (Lisp_Object start, Lisp_Object end, bool after,
			     Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  /* True if this change is an insertion.  */
  bool insertion = (after ? XFIXNAT (arg3) == 0 : EQ (start, end));

  /* We used to run the functions as soon as we found them and only register
     them in last_overlay_modification_hooks for the purpose of the `after'
     case.  But running elisp code as we traverse the list of overlays is
     painful because the list can be modified by the elisp code so we had to
     copy at several places.  We now simply do a read-only traversal that
     only collects the functions to run and we run them afterwards.  It's
     simpler, especially since all the code was already there.  -stef  */

  if (!after)
    {
      struct itree_node *node;
      EMACS_INT begin_arg = XFIXNUM (start);
      EMACS_INT end_arg = XFIXNUM (end);
      /* We are being called before a change.
	 Scan the overlays to find the functions to call.  */
      last_overlay_modification_hooks_used = 0;

      if (!current_buffer->overlays)
        return;
      ITREE_FOREACH (node, current_buffer->overlays,
                     begin_arg - (insertion ? 1 : 0),
                     end_arg   + (insertion ? 1 : 0),
                     ASCENDING)
	{
          Lisp_Object overlay = node->data;
	  ptrdiff_t obegin = OVERLAY_START (overlay);
	  ptrdiff_t oend = OVERLAY_END (overlay);

	  if (insertion && (begin_arg == obegin
			    || end_arg == obegin))
	    {
	      Lisp_Object prop = Foverlay_get (overlay, Qinsert_in_front_hooks);
	      if (!NILP (prop))
		add_overlay_mod_hooklist (prop, overlay);
	    }
	  if (insertion && (begin_arg == oend
			    || end_arg == oend))
	    {
	      Lisp_Object prop = Foverlay_get (overlay, Qinsert_behind_hooks);
	      if (!NILP (prop))
		add_overlay_mod_hooklist (prop, overlay);
	    }
	  /* Test for intersecting intervals.  This does the right thing
	     for both insertion and deletion.  */
	  if (end_arg > obegin && begin_arg < oend)
	    {
	      Lisp_Object prop = Foverlay_get (overlay, Qmodification_hooks);
	      if (!NILP (prop))
		add_overlay_mod_hooklist (prop, overlay);
	    }
	}
    }
  {
    /* Call the functions recorded in last_overlay_modification_hooks.
       First copy the vector contents, in case some of these hooks
       do subsequent modification of the buffer.  */
    ptrdiff_t size = last_overlay_modification_hooks_used;
    Lisp_Object *copy;

    USE_SAFE_ALLOCA;
    SAFE_ALLOCA_LISP (copy, size);
    memcpy (copy, XVECTOR (last_overlay_modification_hooks)->contents,
	    size * word_size);

    for (ptrdiff_t i = 0; i < size;)
      {
	Lisp_Object prop_i, overlay_i;
	prop_i = copy[i++];
	overlay_i = copy[i++];
	/* It is possible that the recorded overlay has been deleted
	   (which makes its markers' buffers be nil), or that (due to
	   some bug) it belongs to a different buffer.  Only run this
	   hook if the overlay belongs to the current buffer.  */
	if (OVERLAY_BUFFER (overlay_i) == current_buffer)
	  call_overlay_mod_hooks (prop_i, overlay_i, after, arg1, arg2, arg3);
      }

    SAFE_FREE ();
  }
}

static void
call_overlay_mod_hooks (Lisp_Object list, Lisp_Object overlay, bool after,
			Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  while (CONSP (list))
    {
      if (NILP (arg3))
	call4 (XCAR (list), overlay, after ? Qt : Qnil, arg1, arg2);
      else
	call5 (XCAR (list), overlay, after ? Qt : Qnil, arg1, arg2, arg3);
      list = XCDR (list);
    }
}

/* Note: WINDOWSNT implements this stuff on w32heap.c.  */
#if defined USE_MMAP_FOR_BUFFERS && !defined WINDOWSNT

#include <sys/mman.h>

#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#else
#define MAP_ANON 0
#endif
#endif

#ifndef MAP_FAILED
#define MAP_FAILED ((void *) -1)
#endif

#if MAP_ANON == 0
#include <fcntl.h>
#endif


/* Memory is allocated in regions which are mapped using mmap(2).
   The current implementation lets the system select mapped
   addresses;  we're not using MAP_FIXED in general, except when
   trying to enlarge regions.

   Each mapped region starts with a mmap_region structure, the user
   area starts after that structure, aligned to MEM_ALIGN.

	+-----------------------+
	| struct mmap_info +	|
	| padding		|
	+-----------------------+
	| user data		|
	|			|
	|			|
	+-----------------------+  */

struct mmap_region
{
  /* User-specified size.  */
  size_t nbytes_specified;

  /* Number of bytes mapped */
  size_t nbytes_mapped;

  /* Pointer to the location holding the address of the memory
     allocated with the mmap'd block.  The variable actually points
     after this structure.  */
  void **var;

  /* Next and previous in list of all mmap'd regions.  */
  struct mmap_region *next, *prev;
};

/* Doubly-linked list of mmap'd regions.  */

static struct mmap_region *mmap_regions;

/* File descriptor for mmap.  If we don't have anonymous mapping,
   /dev/zero will be opened on it.  */

static int mmap_fd;

/* Page size on this system.  */

static int mmap_page_size;

/* 1 means mmap has been initialized.  */

static bool mmap_initialized_p;

/* Value is X rounded up to the next multiple of N.  */

#define ROUND(X, N)	(((X) + (N) - 1) / (N) * (N))

/* Size of mmap_region structure plus padding.  */

#define MMAP_REGION_STRUCT_SIZE	\
     ROUND (sizeof (struct mmap_region), MEM_ALIGN)

/* Given a pointer P to the start of the user-visible part of a mapped
   region, return a pointer to the start of the region.  */

#define MMAP_REGION(P) \
     ((struct mmap_region *) ((char *) (P) - MMAP_REGION_STRUCT_SIZE))

/* Given a pointer P to the start of a mapped region, return a pointer
   to the start of the user-visible part of the region.  */

#define MMAP_USER_AREA(P) \
     ((void *) ((char *) (P) + MMAP_REGION_STRUCT_SIZE))

#define MEM_ALIGN	sizeof (double)

/* Predicate returning true if part of the address range [START .. END]
   is currently mapped.  Used to prevent overwriting an existing
   memory mapping.

   Default is to conservatively assume the address range is occupied by
   something else.  This can be overridden by system configuration
   files if system-specific means to determine this exists.  */

#ifndef MMAP_ALLOCATED_P
#define MMAP_ALLOCATED_P(start, end) 1
#endif

/* Perform necessary initializations for the use of mmap.  */

static void
mmap_init (void)
{
#if MAP_ANON == 0
  /* The value of mmap_fd is initially 0 in temacs, and -1
     in a dumped Emacs.  */
  if (mmap_fd <= 0)
    {
      /* No anonymous mmap -- we need the file descriptor.  */
      mmap_fd = emacs_open_noquit ("/dev/zero", O_RDONLY, 0);
      if (mmap_fd == -1)
	fatal ("Cannot open /dev/zero: %s", emacs_strerror (errno));
    }
#endif /* MAP_ANON == 0 */

  if (mmap_initialized_p)
    return;
  mmap_initialized_p = 1;

#if MAP_ANON != 0
  mmap_fd = -1;
#endif

  mmap_page_size = getpagesize ();
}

/* Unmap a region.  P is a pointer to the start of the user-araa of
   the region.  */

static void
mmap_free_1 (struct mmap_region *r)
{
  if (r->next)
    r->next->prev = r->prev;
  if (r->prev)
    r->prev->next = r->next;
  else
    mmap_regions = r->next;

  if (munmap (r, r->nbytes_mapped) == -1)
    fprintf (stderr, "munmap: %s\n", emacs_strerror (errno));
}

/* Enlarge region R by NPAGES pages.  NPAGES < 0 means shrink R.
   Value is true if successful.  */

static bool
mmap_enlarge (struct mmap_region *r, int npages)
{
  char *region_end = (char *) r + r->nbytes_mapped;
  size_t nbytes;
  bool success = 0;

  if (npages < 0)
    {
      /* Unmap pages at the end of the region.  */
      nbytes = - npages * mmap_page_size;
      if (munmap (region_end - nbytes, nbytes) == -1)
	fprintf (stderr, "munmap: %s\n", emacs_strerror (errno));
      else
	{
	  r->nbytes_mapped -= nbytes;
	  success = 1;
	}
    }
  else if (npages > 0)
    {
      nbytes = npages * mmap_page_size;

      /* Try to map additional pages at the end of the region.  We
	 cannot do this if the address range is already occupied by
	 something else because mmap deletes any previous mapping.
	 I'm not sure this is worth doing, let's see.  */
      if (!MMAP_ALLOCATED_P (region_end, region_end + nbytes))
	{
	  void *p;

	  p = mmap (region_end, nbytes, PROT_READ | PROT_WRITE,
		    MAP_ANON | MAP_PRIVATE | MAP_FIXED, mmap_fd, 0);
	  if (p == MAP_FAILED)
	    ; /* fprintf (stderr, "mmap: %s\n", emacs_strerror (errno)); */
	  else if (p != region_end)
	    {
	      /* Kernels are free to choose a different address.  In
		 that case, unmap what we've mapped above; we have
		 no use for it.  */
	      if (munmap (p, nbytes) == -1)
		fprintf (stderr, "munmap: %s\n", emacs_strerror (errno));
	    }
	  else
	    {
	      r->nbytes_mapped += nbytes;
	      success = 1;
	    }
	}
    }

  return success;
}

/* Allocate a block of storage large enough to hold NBYTES bytes of
   data.  A pointer to the data is returned in *VAR.  VAR is thus the
   address of some variable which will use the data area.

   The allocation of 0 bytes is valid.

   If we can't allocate the necessary memory, set *VAR to null, and
   return null.  */

static void *
mmap_alloc (void **var, size_t nbytes)
{
  void *p;
  size_t map;

  mmap_init ();

  map = ROUND (nbytes + MMAP_REGION_STRUCT_SIZE, mmap_page_size);
  p = mmap (NULL, map, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE,
	    mmap_fd, 0);

  if (p == MAP_FAILED)
    {
      if (errno != ENOMEM)
	fprintf (stderr, "mmap: %s\n", emacs_strerror (errno));
      p = NULL;
    }
  else
    {
      struct mmap_region *r = p;

      r->nbytes_specified = nbytes;
      r->nbytes_mapped = map;
      r->var = var;
      r->prev = NULL;
      r->next = mmap_regions;
      if (r->next)
	r->next->prev = r;
      mmap_regions = r;

      p = MMAP_USER_AREA (p);
    }

  return *var = p;
}

/* Free a block of relocatable storage whose data is pointed to by
   PTR.  Store 0 in *PTR to show there's no block allocated.  */

static void
mmap_free (void **var)
{
  mmap_init ();

  if (*var)
    {
      mmap_free_1 (MMAP_REGION (*var));
      *var = NULL;
    }
}

/* Given a pointer at address VAR to data allocated with mmap_alloc,
   resize it to size NBYTES.  Change *VAR to reflect the new block,
   and return this value.  If more memory cannot be allocated, then
   leave *VAR unchanged, and return null.  */

static void *
mmap_realloc (void **var, size_t nbytes)
{
  void *result;

  mmap_init ();

  if (*var == NULL)
    result = mmap_alloc (var, nbytes);
  else if (nbytes == 0)
    {
      mmap_free (var);
      result = mmap_alloc (var, nbytes);
    }
  else
    {
      struct mmap_region *r = MMAP_REGION (*var);
      size_t room = r->nbytes_mapped - MMAP_REGION_STRUCT_SIZE;

      if (room < nbytes)
	{
	  /* Must enlarge.  */
	  void *old_ptr = *var;

	  /* Try to map additional pages at the end of the region.
	     If that fails, allocate a new region,  copy data
	     from the old region, then free it.  */
	  if (mmap_enlarge (r, (ROUND (nbytes - room, mmap_page_size)
				/ mmap_page_size)))
	    {
	      r->nbytes_specified = nbytes;
	      *var = result = old_ptr;
	    }
	  else if (mmap_alloc (var, nbytes))
	    {
	      memcpy (*var, old_ptr, r->nbytes_specified);
	      mmap_free_1 (MMAP_REGION (old_ptr));
	      result = *var;
	      r = MMAP_REGION (result);
	      r->nbytes_specified = nbytes;
	    }
	  else
	    {
	      *var = old_ptr;
	      result = NULL;
	    }
	}
      else if (room - nbytes >= mmap_page_size)
	{
	  /* Shrinking by at least a page.  Let's give some
	     memory back to the system.

	     The extra parens are to make the division happens first,
	     on positive values, so we know it will round towards
	     zero.  */
	  mmap_enlarge (r, - ((room - nbytes) / mmap_page_size));
	  result = *var;
	  r->nbytes_specified = nbytes;
	}
      else
	{
	  /* Leave it alone.  */
	  result = *var;
	  r->nbytes_specified = nbytes;
	}
    }

  return result;
}

#endif /* USE_MMAP_FOR_BUFFERS */

/* Allocate NBYTES bytes for buffer B's text buffer.  */

static void
alloc_buffer_text (struct buffer *b, ptrdiff_t nbytes)
{
  void *p;

  block_input ();
#if defined USE_MMAP_FOR_BUFFERS
  p = mmap_alloc ((void **) &b->text->beg, nbytes);
#elif defined REL_ALLOC
  p = r_alloc ((void **) &b->text->beg, nbytes);
#else
  p = xmalloc (nbytes);
#endif

  if (p == NULL)
    {
      unblock_input ();
      memory_full (nbytes);
    }

  b->text->beg = p;
  unblock_input ();
}

/* Enlarge buffer B's text buffer by DELTA bytes.  DELTA < 0 means
   shrink it.  */

void
enlarge_buffer_text (struct buffer *b, ptrdiff_t delta)
{
  block_input ();
  void *p;
  unsigned char *old_beg = b->text->beg;
  ptrdiff_t old_nbytes =
    BUF_Z_BYTE (b) - BUF_BEG_BYTE (b) + BUF_GAP_SIZE (b) + 1;
  ptrdiff_t new_nbytes = old_nbytes + delta;

  if (pdumper_address_p (old_beg))
    b->text->beg = NULL;
  else
    old_beg = NULL;

#if defined USE_MMAP_FOR_BUFFERS
  p = mmap_realloc ((void **) &b->text->beg, new_nbytes);
#elif defined REL_ALLOC
  p = r_re_alloc ((void **) &b->text->beg, new_nbytes);
#else
  p = xrealloc (b->text->beg, new_nbytes);
#endif
  __lsan_ignore_object (p);

  if (p == NULL)
    {
      if (old_beg)
        b->text->beg = old_beg;
      unblock_input ();
      memory_full (new_nbytes);
    }

  if (old_beg)
    memcpy (p, old_beg, min (old_nbytes, new_nbytes));

  BUF_BEG_ADDR (b) = p;
  unblock_input ();
}

static void
free_buffer_text (struct buffer *b)
{
  block_input ();

  if (!pdumper_address_p (b->text->beg))
    {
#if defined USE_MMAP_FOR_BUFFERS
      mmap_free ((void **) &b->text->beg);
#elif defined REL_ALLOC
      r_alloc_free ((void **) &b->text->beg);
#else
      xfree (b->text->beg);
#endif
    }

  BUF_BEG_ADDR (b) = NULL;
  unblock_input ();
}

void
init_buffer_once (void)
{
  int idx;

  /* Items flagged permanent get an explicit permanent-local property
     added in bindings.el, for clarity.  */
  PDUMPER_REMEMBER_SCALAR (buffer_permanent_local_flags);
  memset (buffer_permanent_local_flags, 0, sizeof buffer_permanent_local_flags);

  /* By virtue of being a struct buffer, buffer_slot_map is a
     natural map of slots to the following values:

     -1, a private slot whose value does not admit user mangling by
     special logic in data.c.

     0, a slot without a user-exposed Lisp variable.

     Within interval [1, MAX_PER_BUFFER_VARS), slot's index in the
     per-buffer `local_flags` array.
  */
  memset (&buffer_slot_map, 0, sizeof buffer_slot_map);
  bset_filename (&buffer_slot_map, make_fixnum (-1));
  bset_directory (&buffer_slot_map, make_fixnum (-1));
  bset_backed_up (&buffer_slot_map, make_fixnum (-1));
  bset_save_length (&buffer_slot_map, make_fixnum (-1));
  bset_auto_save_file_name (&buffer_slot_map, make_fixnum (-1));
  bset_read_only (&buffer_slot_map, make_fixnum (-1));
  bset_major_mode (&buffer_slot_map, make_fixnum (-1));
  bset_local_minor_modes (&buffer_slot_map, make_fixnum (-1));
  bset_mode_name (&buffer_slot_map, make_fixnum (-1));
  bset_undo_list (&buffer_slot_map, make_fixnum (-1));
  bset_mark_active (&buffer_slot_map, make_fixnum (-1));
  bset_point_before_scroll (&buffer_slot_map, make_fixnum (-1));
  bset_file_truename (&buffer_slot_map, make_fixnum (-1));
  bset_invisibility_spec (&buffer_slot_map, make_fixnum (-1));
  bset_last_selected_window (&buffer_slot_map, make_fixnum (-1));
  bset_file_format (&buffer_slot_map, make_fixnum (-1));
  bset_auto_save_file_format (&buffer_slot_map, make_fixnum (-1));
  bset_display_count (&buffer_slot_map, make_fixnum (-1));
  bset_display_time (&buffer_slot_map, make_fixnum (-1));
  bset_enable_multibyte_characters (&buffer_slot_map, make_fixnum (-1));

  /* These used to be stuck at 0 by default, but now that the all-zero value
     means Qnil, we have to initialize them explicitly.  */
  bset_name (&buffer_slot_map, make_fixnum (0));
  bset_last_name (&buffer_slot_map, make_fixnum (0));
  bset_mark (&buffer_slot_map, make_fixnum (0));
  bset_local_val_alist (&buffer_slot_map, make_fixnum (0));
  bset_local_func_alist (&buffer_slot_map, make_fixnum (0));
  bset_keymap (&buffer_slot_map, make_fixnum (0));
  bset_downcase_table (&buffer_slot_map, make_fixnum (0));
  bset_upcase_table (&buffer_slot_map, make_fixnum (0));
  bset_case_canon_table (&buffer_slot_map, make_fixnum (0));
  bset_case_eqv_table (&buffer_slot_map, make_fixnum (0));
  bset_width_table (&buffer_slot_map, make_fixnum (0));
  bset_pt_marker (&buffer_slot_map, make_fixnum (0));
  bset_begv_marker (&buffer_slot_map, make_fixnum (0));
  bset_zv_marker (&buffer_slot_map, make_fixnum (0));

  idx = 1;
  XSETFASTINT (BVAR (&buffer_slot_map, mode_line_format), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, abbrev_mode), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, overwrite_mode), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, auto_fill_function), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, selective_display), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, selective_display_ellipses), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, tab_width), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, truncate_lines), idx);
  /* Make this one a permanent local.  */
  buffer_permanent_local_flags[idx++] = 1;
  XSETFASTINT (BVAR (&buffer_slot_map, word_wrap), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, ctl_arrow), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, fill_column), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, left_margin), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, abbrev_table), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, display_table), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, syntax_table), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, cache_long_scans), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, category_table), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, initial_char_script), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, bidi_display_reordering), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, bidi_paragraph_direction), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, bidi_paragraph_separate_re), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, bidi_paragraph_start_re), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, buffer_file_coding_system), idx);
  /* Make this one a permanent local.  */
  buffer_permanent_local_flags[idx++] = 1;
  XSETFASTINT (BVAR (&buffer_slot_map, left_margin_cols), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, right_margin_cols), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, left_fringe_width), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, right_fringe_width), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, fringes_outside_margins), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, scroll_bar_width), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, scroll_bar_height), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, vertical_scroll_bar_type), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, horizontal_scroll_bar_type), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, indicate_empty_lines), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, indicate_buffer_boundaries), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, fringe_indicator_alist), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, fringe_cursor_alist), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, scroll_up_aggressively), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, scroll_down_aggressively), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, header_line_format), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, tab_line_format), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, cursor_type), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, extra_line_spacing), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_slot_map, cursor_in_non_selected_windows), idx); ++idx;

  /* buffer_slot_map contains no pointers, so it's safe to treat it
     as a blob for pdumper.  */
  PDUMPER_REMEMBER_SCALAR (buffer_slot_map);

  if (idx >= MAX_PER_BUFFER_VARS)
    emacs_abort ();
  last_per_buffer_idx = idx;
  PDUMPER_REMEMBER_SCALAR (last_per_buffer_idx);

  reset_buffer (&buffer_slot_defaults);
  eassert (NILP (BVAR (&buffer_slot_defaults, name)));
  reset_buffer_local_variables (&buffer_slot_defaults, true);
  eassert (NILP (BVAR (&buffer_slot_symbols, name)));
  reset_buffer (&buffer_slot_symbols);
  reset_buffer_local_variables (&buffer_slot_symbols, true);
  buffer_slot_defaults.text = &buffer_slot_defaults.own_text;
  buffer_slot_symbols.text = &buffer_slot_symbols.own_text;
  buffer_slot_defaults.indirections = 0;
  buffer_slot_symbols.indirections = 0;
  buffer_slot_defaults.window_count = 0;
  buffer_slot_symbols.window_count = 0;
  set_buffer_intervals (&buffer_slot_defaults, NULL);
  set_buffer_intervals (&buffer_slot_symbols, NULL);
  bset_name (&buffer_slot_defaults, build_pure_c_string (" *buffer-defaults*"));
  bset_name (&buffer_slot_symbols, build_pure_c_string (" *buffer-local-symbols*"));
  BUFFER_PVEC_INIT (&buffer_slot_defaults);
  BUFFER_PVEC_INIT (&buffer_slot_symbols);

  /* Set up the default values of various buffer slots.
     Must do these before making the first buffer!
     Real setup is done in bindings.el
  */
  bset_mode_line_format (&buffer_slot_defaults, build_pure_c_string ("%-"));
  bset_header_line_format (&buffer_slot_defaults, Qnil);
  bset_tab_line_format (&buffer_slot_defaults, Qnil);
  bset_abbrev_mode (&buffer_slot_defaults, Qnil);
  bset_overwrite_mode (&buffer_slot_defaults, Qnil);
  bset_auto_fill_function (&buffer_slot_defaults, Qnil);
  bset_selective_display (&buffer_slot_defaults, Qnil);
  bset_selective_display_ellipses (&buffer_slot_defaults, Qt);
  bset_abbrev_table (&buffer_slot_defaults, Qnil);
  bset_display_table (&buffer_slot_defaults, Qnil);
  bset_undo_list (&buffer_slot_defaults, Qnil);
  bset_mark_active (&buffer_slot_defaults, Qnil);
  bset_file_format (&buffer_slot_defaults, Qnil);
  bset_auto_save_file_format (&buffer_slot_defaults, Qt);
  buffer_slot_defaults.overlays = NULL;

  XSETFASTINT (BVAR (&buffer_slot_defaults, tab_width), 8);
  bset_truncate_lines (&buffer_slot_defaults, Qnil);
  bset_word_wrap (&buffer_slot_defaults, Qnil);
  bset_ctl_arrow (&buffer_slot_defaults, Qt);
  bset_initial_char_script (&buffer_slot_defaults, Qnil);
  bset_bidi_display_reordering (&buffer_slot_defaults, Qnil);
  bset_bidi_paragraph_direction (&buffer_slot_defaults, Qnil);
  bset_bidi_paragraph_start_re (&buffer_slot_defaults, Qnil);
  bset_bidi_paragraph_separate_re (&buffer_slot_defaults, Qnil);
  bset_cursor_type (&buffer_slot_defaults, Qt);
  bset_extra_line_spacing (&buffer_slot_defaults, Qnil);
  bset_cursor_in_non_selected_windows (&buffer_slot_defaults, Qt);
  bset_enable_multibyte_characters (&buffer_slot_defaults, Qt);
  bset_buffer_file_coding_system (&buffer_slot_defaults, Qnil);
  XSETFASTINT (BVAR (&buffer_slot_defaults, fill_column), 70);
  XSETFASTINT (BVAR (&buffer_slot_defaults, left_margin), 0);
  bset_cache_long_scans (&buffer_slot_defaults, Qt);
  bset_file_truename (&buffer_slot_defaults, Qnil);
  XSETFASTINT (BVAR (&buffer_slot_defaults, display_count), 0);
  XSETFASTINT (BVAR (&buffer_slot_defaults, left_margin_cols), 0);
  XSETFASTINT (BVAR (&buffer_slot_defaults, right_margin_cols), 0);
  bset_left_fringe_width (&buffer_slot_defaults, Qnil);
  bset_right_fringe_width (&buffer_slot_defaults, Qnil);
  bset_fringes_outside_margins (&buffer_slot_defaults, Qnil);
  bset_scroll_bar_width (&buffer_slot_defaults, Qnil);
  bset_scroll_bar_height (&buffer_slot_defaults, Qnil);
  bset_vertical_scroll_bar_type (&buffer_slot_defaults, Qt);
  bset_horizontal_scroll_bar_type (&buffer_slot_defaults, Qt);
  bset_indicate_empty_lines (&buffer_slot_defaults, Qnil);
  bset_indicate_buffer_boundaries (&buffer_slot_defaults, Qnil);
  bset_fringe_indicator_alist (&buffer_slot_defaults, Qnil);
  bset_fringe_cursor_alist (&buffer_slot_defaults, Qnil);
  bset_scroll_up_aggressively (&buffer_slot_defaults, Qnil);
  bset_scroll_down_aggressively (&buffer_slot_defaults, Qnil);
  bset_display_time (&buffer_slot_defaults, Qnil);

  static_assert (sizeof (EMACS_INT) == word_size);

  Vbuffer_alist = Qnil;
  current_buffer = 0;
  pdumper_remember (&current_buffer, Lisp_Vectorlike);

  QSFundamental = build_pure_c_string ("Fundamental");

  DEFSYM (Qfundamental_mode, "fundamental-mode");
  bset_major_mode (&buffer_slot_defaults, Qfundamental_mode);

  DEFSYM (Qmode_class, "mode-class");
  DEFSYM (Qprotected_field, "protected-field");

  DEFSYM (Qpermanent_local, "permanent-local");
  DEFSYM (Qkill_buffer_hook, "kill-buffer-hook");
  Fput (Qkill_buffer_hook, Qpermanent_local, Qt);

  Vprin1_to_string_buffer = Fget_buffer_create (build_pure_c_string (" prin1"), Qt);
  Vbuffer_alist = Qnil;

  Fset_buffer (Fget_buffer_create (build_pure_c_string ("*scratch*"), Qnil));

  inhibit_modification_hooks = false;
}

void
init_buffer (void)
{
  Lisp_Object temp;

  AUTO_STRING (scratch, "*scratch*");
  Fset_buffer (Fget_buffer_create (scratch, Qnil));
  if (NILP (BVAR (&buffer_slot_defaults, enable_multibyte_characters)))
    Fset_buffer_multibyte (Qnil);

  char const *pwd = emacs_wd;

  if (!pwd)
    {
      fprintf (stderr, "Error getting directory: %s\n",
               emacs_strerror (errno));
      bset_directory (current_buffer, Qnil);
    }
  else
    {
      /* Maybe this should really use some standard subroutine
         whose definition is filename syntax dependent.  */
      ptrdiff_t len = strlen (pwd);
      bool add_slash = !IS_DIRECTORY_SEP (pwd[len - 1]);

      /* At this moment, we still don't know how to decode the directory
         name.  So, we keep the bytes in unibyte form so that file I/O
         routines correctly get the original bytes.  */
      Lisp_Object dirname = make_unibyte_string (pwd, len + add_slash);
      if (add_slash)
	SSET (dirname, len, DIRECTORY_SEP);
      bset_directory (current_buffer, dirname);

      /* Add /: to the front of the name
         if it would otherwise be treated as magic.  */
      temp = Ffind_file_name_handler (BVAR (current_buffer, directory), Qt);
      if (!NILP (temp)
          /* If the default dir is just /, TEMP is non-nil
             because of the ange-ftp completion handler.
             However, it is not necessary to turn / into /:/.
             So avoid doing that.  */
          && strcmp ("/", SSDATA (BVAR (current_buffer, directory))))
        {
          AUTO_STRING (slash_colon, "/:");
          bset_directory (current_buffer,
                          concat2 (slash_colon,
                                   BVAR (current_buffer, directory)));
        }
    }

  temp = get_minibuffer (0);
  bset_directory (XBUFFER (temp), BVAR (current_buffer, directory));
}

/* Similar to defvar_lisp but define a variable whose value is the
   Lisp_Object stored in the current buffer.  LNAME is the Lisp-level
   variable name.  VNAME is the name of the buffer slot.  PREDICATE
   is nil for a general Lisp variable.  If PREDICATE is non-nil, then
   only Lisp values that satisfies the PREDICATE are allowed (except
   that nil is allowed too).  DOC is a dummy where you write the doc
   string as a comment.  */

#define DEFVAR_PER_BUFFER(lname, vname, predicate, doc)		\
  do {								\
    static struct Lisp_Buffer_Objfwd bo_fwd;			\
    defvar_per_buffer (&bo_fwd, lname, vname, predicate);	\
  } while (0)

static void
defvar_per_buffer (struct Lisp_Buffer_Objfwd *bo_fwd, const char *namestring,
		   Lisp_Object *address, Lisp_Object predicate)
{
  struct Lisp_Symbol *sym;
  int offset;

  sym = XSYMBOL (intern (namestring));
  offset = (char *)address - (char *)current_buffer;

  bo_fwd->type = Lisp_Fwd_Buffer_Obj;
  bo_fwd->offset = offset;
  bo_fwd->predicate = predicate;
  sym->u.s.declared_special = true;
  sym->u.s.type = SYMBOL_PER_BUFFER;
  SET_SYMBOL_FWD (sym, bo_fwd);
  sym->u.s.c_variable.fwdptr = bo_fwd;
  XSETSYMBOL (PER_BUFFER_SYMBOL (offset), sym);

  if (PER_BUFFER_IDX (offset) == 0)
    /* A DEFVAR_PER_BUFFER for a slot explicitly designated
       outside Lisp-space.  */
    emacs_abort ();
}

#ifdef ITREE_DEBUG
static Lisp_Object
make_lispy_itree_node (const struct itree_node *node)
{
  return listn (12,
                intern (":begin"),
                make_fixnum (node->begin),
                intern (":end"),
                make_fixnum (node->end),
                intern (":limit"),
                make_fixnum (node->limit),
                intern (":offset"),
                make_fixnum (node->offset),
                intern (":rear-advance"),
                node->rear_advance ? Qt : Qnil,
                intern (":front-advance"),
                node->front_advance ? Qt : Qnil);
}

static Lisp_Object
overlay_tree (const struct itree_tree *tree,
              const struct itree_node *node)
{
  if (node == ITREE_NULL)
    return Qnil;
  return list3 (make_lispy_itree_node (node),
                overlay_tree (tree, node->left),
                overlay_tree (tree, node->right));
}

DEFUN ("overlay-tree", Foverlay_tree, Soverlay_tree, 0, 1, 0,
       doc: /* Get the overlay tree for BUFFER.  */)
     (Lisp_Object buffer)
{
  struct buffer *b = BUFFERP (buffer) ? XBUFFER (buffer) : current_buffer;
  if (! b->overlays)
    return Qnil;
  return overlay_tree (b->overlays, b->overlays->root);
}
#endif

/* Initialize the buffer routines.  */
void
syms_of_buffer (void)
{
  staticpro (&last_overlay_modification_hooks);
  last_overlay_modification_hooks = initialize_vector (10, Qnil);

  staticpro (&QSFundamental);
  staticpro (&Vbuffer_alist);
  staticpro (&extant_indirects);

  DEFSYM (Qchoice, "choice");
  DEFSYM (Qleft, "left");
  DEFSYM (Qright, "right");
  DEFSYM (Qrange, "range");

  DEFSYM (Qpermanent_local_hook, "permanent-local-hook");
  DEFSYM (Qoverlayp, "overlayp");
  DEFSYM (Qevaporate, "evaporate");
  DEFSYM (Qmodification_hooks, "modification-hooks");
  DEFSYM (Qinsert_in_front_hooks, "insert-in-front-hooks");
  DEFSYM (Qinsert_behind_hooks, "insert-behind-hooks");
  DEFSYM (Qget_file_buffer, "get-file-buffer");
  DEFSYM (Qpriority, "priority");
  DEFSYM (Qbefore_string, "before-string");
  DEFSYM (Qafter_string, "after-string");
  DEFSYM (Qfirst_change_hook, "first-change-hook");
  DEFSYM (Qbefore_change_functions, "before-change-functions");
  DEFSYM (Qafter_change_functions, "after-change-functions");
  DEFSYM (Qkill_buffer_query_functions, "kill-buffer-query-functions");
  DEFSYM (Qget_scratch_buffer_create, "get-scratch-buffer-create");
  DEFSYM (Qmode_overlay_face_alist, "mode-overlay-face-alist");
  DEFSYM (Qmode_overlay_p, "mode-overlay-p");
  DEFSYM (Qmode_overlay, "mode-overlay");
  DEFSYM (Qmode_overlay_on_switch_to_buffer, "mode-overlay-on-switch-to-buffer");
  DEFSYM (Qmode_overlay_filter_buffer_substring_function,
	  "mode-overlay-filter-buffer-substring-function");
  DEFSYM (Qadd_hook, "add-hook");

  DEFSYM (Qvertical_scroll_bar, "vertical-scroll-bar");
  Fput (Qvertical_scroll_bar, Qchoice, list4 (Qnil, Qt, Qleft, Qright));
  DEFSYM (Qhorizontal_scroll_bar, "horizontal-scroll-bar");

  DEFSYM (Qfraction, "fraction");
  Fput (Qfraction, Qrange, Fcons (make_float (0.0), make_float (1.0)));

  DEFSYM (Qoverwrite_mode, "overwrite-mode");
  Fput (Qoverwrite_mode, Qchoice,
	list3 (Qnil, intern ("overwrite-mode-textual"),
	       Qoverwrite_mode_binary));

  Fput (Qprotected_field, Qerror_conditions,
	pure_list (Qprotected_field, Qerror));
  Fput (Qprotected_field, Qerror_message,
	build_pure_c_string ("Attempt to modify a protected field"));

  DEFSYM (Qclone_indirect_buffer_hook, "clone-indirect-buffer-hook");

  DEFVAR_PER_BUFFER ("tab-line-format",
		     &BVAR (current_buffer, tab_line_format),
		     Qnil,
		     doc: /* Analogous to `mode-line-format', but controls the tab line.
The tab line appears, optionally, at the top of a window;
the mode line appears at the bottom.  */);

  DEFVAR_PER_BUFFER ("header-line-format",
		     &BVAR (current_buffer, header_line_format),
		     Qnil,
		     doc: /* Analogous to `mode-line-format', but controls the header line.
The header line appears, optionally, at the top of a window; the mode
line appears at the bottom.

Also see `header-line-indent-mode' if `display-line-numbers-mode' is
turned on and header-line text should be aligned with buffer text.  */);

  DEFVAR_PER_BUFFER ("mode-line-format", &BVAR (current_buffer, mode_line_format),
		     Qnil,
		     doc: /* Template for displaying mode line for a window's buffer.

The value may be nil, a string, a symbol or a list.

A value of nil means don't display a mode line.

For any symbol other than t or nil, the symbol's value is processed as
 a mode line construct.  As a special exception, if that value is a
 string, the string is processed verbatim, without handling any
 %-constructs (see below).  Also, unless the symbol has a non-nil
 `risky-local-variable' property, all properties in any strings, as
 well as all :eval and :propertize forms in the value, are ignored.

When the value is processed, the window's buffer is temporarily the
current buffer.

A list whose car is a string or list is processed by processing each
 of the list elements recursively, as separate mode line constructs,
 and concatenating the results.

A list of the form `(:eval FORM)' is processed by evaluating FORM and
 using the result as a mode line construct.  Be careful--FORM should
 not load any files, because that can cause an infinite recursion.

A list of the form `(:propertize ELT PROPS...)' is processed by
 processing ELT as the mode line construct, and adding the text
 properties PROPS to the result.

A list whose car is a symbol is processed by examining the symbol's
 value, and, if that value is non-nil, processing the cadr of the list
 recursively; and if that value is nil, processing the caddr of the
 list recursively.

A list whose car is an integer is processed by processing the cadr of
 the list, and padding (if the number is positive) or truncating (if
 negative) to the width specified by that number.

A string is printed verbatim in the mode line except for %-constructs:
  %b -- print buffer name.
  %c -- print the current column number (this makes editing slower).
        Columns are numbered starting from the left margin, and the
        leftmost column is displayed as zero.
        To make the column number update correctly in all cases,
        `column-number-mode' must be non-nil.
  %C -- Like %c, but the leftmost column is displayed as one.
  %e -- print error message about full memory.
  %f -- print visited file name.
  %F -- print frame name.
  %i -- print the size of the buffer.
  %I -- like %i, but use k, M, G, etc., to abbreviate.
  %l -- print the current line number.
  %n -- print Narrow if appropriate.
  %o -- print percent of window travel through buffer, or Top, Bot or All.
  %p -- print percent of buffer above top of window, or Top, Bot or All.
  %P -- print percent of buffer above bottom of window, perhaps plus Top,
        or print Bottom or All.
  %q -- print percent of buffer above both the top and the bottom of the
        window, separated by `-', or `All'.
  %s -- print process status.
  %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
  %Z -- like %z, but including the end-of-line format.
  %& -- print * if the buffer is modified, otherwise hyphen.
  %+ -- print *, % or hyphen (modified, read-only, neither).
  %* -- print %, * or hyphen (read-only, modified, neither).
        For a modified read-only buffer, %+ prints * and %* prints %.
  %@ -- print @ if default-directory is on a remote machine, else hyphen.
  %[ -- print one [ for each recursive editing level.
  %] -- print one ] for each recursive editing level.
  %- -- print enough dashes to fill the mode line.
  %% -- print %.
Decimal digits after the % specify field width to which to pad.  */);

  DEFVAR_PER_BUFFER ("major-mode", &BVAR (current_buffer, major_mode),
		     Qsymbolp,
		     doc: /* Symbol for current buffer's major mode.
The default value (normally `fundamental-mode') affects new buffers.
A value of nil means to use the current buffer's major mode, provided
it is not marked as "special".  */);

  DEFVAR_PER_BUFFER ("local-minor-modes",
		     &BVAR (current_buffer, local_minor_modes),
		     Qnil,
		     doc: /* Minor modes currently active in the current buffer.
This is a list of symbols, or nil if there are no minor modes active.  */);

  DEFVAR_PER_BUFFER ("mode-name", &BVAR (current_buffer, mode_name),
                     Qnil,
		     doc: /* Pretty name of current buffer's major mode.
Usually a string, but can use any of the constructs for `mode-line-format',
which see.
Format with `format-mode-line' to produce a string value.  */);

  DEFVAR_PER_BUFFER ("local-abbrev-table", &BVAR (current_buffer, abbrev_table), Qnil,
		     doc: /* Local (mode-specific) abbrev table of current buffer.  */);

  DEFVAR_PER_BUFFER ("abbrev-mode", &BVAR (current_buffer, abbrev_mode), Qnil,
		     doc: /*  Non-nil if Abbrev mode is enabled.
Use the command `abbrev-mode' to change this variable.  */);

  DEFVAR_PER_BUFFER ("fill-column", &BVAR (current_buffer, fill_column),
		     Qintegerp,
		     doc: /* Column beyond which automatic line-wrapping should happen.
It is used by filling commands, such as `fill-region' and `fill-paragraph',
and by `auto-fill-mode', which see.
See also `current-fill-column'.
Interactively, you can set the buffer local value using \\[set-fill-column].  */);

  DEFVAR_PER_BUFFER ("left-margin", &BVAR (current_buffer, left_margin),
		     Qintegerp,
		     doc: /* Column for the default `indent-line-function' to indent to.
Linefeed indents to this column in Fundamental mode.  */);

  DEFVAR_PER_BUFFER ("tab-width", &BVAR (current_buffer, tab_width),
		     Qintegerp,
		     doc: /* Distance between tab stops (for display of tab characters), in columns.
This controls the width of a TAB character on display.
The value should be a positive integer.
Note that this variable doesn't necessarily affect the size of the
indentation step.  However, if the major mode's indentation facility
inserts one or more TAB characters, this variable will affect the
indentation step as well, even if `indent-tabs-mode' is non-nil.  */);

  DEFVAR_PER_BUFFER ("ctl-arrow", &BVAR (current_buffer, ctl_arrow), Qnil,
		     doc: /* Non-nil means display control chars with uparrow `^'.
A value of nil means use backslash `\\' and octal digits.
This variable does not apply to characters whose display is specified in
the current display table (if there is one; see `standard-display-table').  */);

  DEFVAR_PER_BUFFER ("enable-multibyte-characters",
		     &BVAR (current_buffer, enable_multibyte_characters),
		     Qnil,
		     doc: /* Non-nil means the buffer contents are regarded as multi-byte characters.
Otherwise they are regarded as unibyte.  This affects the display,
file I/O and the behavior of various editing commands.

This variable is buffer-local but you cannot set it directly;
use the function `set-buffer-multibyte' to change a buffer's representation.
To prevent any attempts to set it or make it buffer-local, Emacs will
signal an error in those cases.
See also Info node `(elisp)Text Representations'.  */);
  make_symbol_constant (intern_c_string ("enable-multibyte-characters"));

  DEFVAR_PER_BUFFER ("buffer-file-coding-system",
		     &BVAR (current_buffer, buffer_file_coding_system), Qnil,
		     doc: /* Coding system to be used for encoding the buffer contents on saving.
This variable applies to saving the buffer, and also to `write-region'
and other functions that use `write-region'.
It does not apply to sending output to subprocesses, however.

If this is nil, the buffer is saved without any code conversion
unless some coding system is specified in `file-coding-system-alist'
for the buffer file.

If the text to be saved cannot be encoded as specified by this variable,
an alternative encoding is selected by `select-safe-coding-system', which see.

The variable `coding-system-for-write', if non-nil, overrides this variable.

This variable is never applied to a way of decoding a file while reading it.  */);

  DEFVAR_PER_BUFFER ("initial-char-script",
		     &BVAR (current_buffer, initial_char_script), Qnil,
		     doc: /* Char script of first inserted character.  */);

  DEFVAR_PER_BUFFER ("bidi-display-reordering",
		     &BVAR (current_buffer, bidi_display_reordering), Qnil,
		     doc: /* Non-nil means iterate across bidirectional text in the visual order.  */);

  DEFVAR_PER_BUFFER ("bidi-paragraph-start-re",
		     &BVAR (current_buffer, bidi_paragraph_start_re), Qnil,
		     doc: /* If non-nil, a regexp matching a line that starts OR separates paragraphs.

The value of nil means to use empty lines as lines that start and
separate paragraphs.

When Emacs displays bidirectional text, it by default computes
the base paragraph direction separately for each paragraph.
Setting this variable changes the places where paragraph base
direction is recomputed.

The regexp is always matched after a newline, so it is best to
anchor it by beginning it with a "^".

If you change the value of this variable, be sure to change
the value of `bidi-paragraph-separate-re' accordingly.  For
example, to have a single newline behave as a paragraph separator,
set both these variables to "^".

See also `bidi-paragraph-direction'.  */);

  DEFVAR_PER_BUFFER ("bidi-paragraph-separate-re",
		     &BVAR (current_buffer, bidi_paragraph_separate_re), Qnil,
		     doc: /* If non-nil, a regexp matching a line that separates paragraphs.

The value of nil means to use empty lines as paragraph separators.

When Emacs displays bidirectional text, it by default computes
the base paragraph direction separately for each paragraph.
Setting this variable changes the places where paragraph base
direction is recomputed.

The regexp is always matched after a newline, so it is best to
anchor it by beginning it with a "^".

If you change the value of this variable, be sure to change
the value of `bidi-paragraph-start-re' accordingly.  For
example, to have a single newline behave as a paragraph separator,
set both these variables to "^".

See also `bidi-paragraph-direction'.  */);

  DEFVAR_PER_BUFFER ("bidi-paragraph-direction",
		     &BVAR (current_buffer, bidi_paragraph_direction), Qnil,
		     doc: /* If non-nil, forces directionality of text paragraphs in the buffer.

If this is nil (the default), the direction of each paragraph is
determined by the first strong directional character of its text.
The values of `right-to-left' and `left-to-right' override that.
Any other value is treated as nil.

This variable has no effect unless the buffer's value of
`bidi-display-reordering' is non-nil.  */);

 DEFVAR_PER_BUFFER ("truncate-lines", &BVAR (current_buffer, truncate_lines), Qnil,
		     doc: /* Non-nil means do not display continuation lines.
Instead, give each line of text just one screen line.

Note that this is overridden by the variable
`truncate-partial-width-windows' if that variable is non-nil
and this buffer is not full-frame width.

Minibuffers set this variable to nil.

Don't set this to a non-nil value when `visual-line-mode' is
turned on, as it could produce confusing results.   */);

  DEFVAR_PER_BUFFER ("word-wrap", &BVAR (current_buffer, word_wrap), Qnil,
		     doc: /* Non-nil means to use word-wrapping for continuation lines.
When word-wrapping is on, continuation lines are wrapped at the space
or tab character nearest to the right window edge.
If nil, continuation lines are wrapped at the right screen edge.

This variable has no effect if long lines are truncated (see
`truncate-lines' and `truncate-partial-width-windows').  If you use
word-wrapping, you might want to reduce the value of
`truncate-partial-width-windows', since wrapping can make text readable
in narrower windows.

Instead of setting this variable directly, most users should use
Visual Line mode.  Visual Line mode, when enabled, sets `word-wrap'
to t, and additionally redefines simple editing commands to act on
visual lines rather than logical lines.  See the documentation of
`visual-line-mode'.  */);

  DEFVAR_PER_BUFFER ("default-directory", &BVAR (current_buffer, directory),
		     Qstringp,
		     doc: /* Name of default directory of current buffer.
It should be an absolute directory name; on GNU and Unix systems,
these names start with "/" or "~" and end with "/".
To interactively change the default directory, use the command `cd'. */);

  DEFVAR_PER_BUFFER ("auto-fill-function", &BVAR (current_buffer, auto_fill_function),
		     Qnil,
		     doc: /* Function called (if non-nil) to perform auto-fill.
It is called after self-inserting any character specified in
the `auto-fill-chars' table.
NOTE: This variable is not a hook;
its value may not be a list of functions.  */);

  DEFVAR_PER_BUFFER ("buffer-file-name", &BVAR (current_buffer, filename),
		     Qstringp,
		     doc: /* Name of file visited in current buffer, or nil if not visiting a file.
This should be an absolute file name.  */);

  DEFVAR_PER_BUFFER ("buffer-file-truename", &BVAR (current_buffer, file_truename),
		     Qstringp,
		     doc: /* Abbreviated truename of file visited in current buffer, or nil if none.
The truename of a file is calculated by `file-truename'
and then abbreviated with `abbreviate-file-name'.  */);

  DEFVAR_PER_BUFFER ("buffer-auto-save-file-name",
		     &BVAR (current_buffer, auto_save_file_name),
		     Qstringp,
		     doc: /* Name of file for auto-saving current buffer.
If it is nil, that means don't auto-save this buffer.  */);

  DEFVAR_PER_BUFFER ("buffer-read-only", &BVAR (current_buffer, read_only), Qnil,
		     doc: /* Non-nil if this buffer is read-only.  */);

  DEFVAR_PER_BUFFER ("buffer-backed-up", &BVAR (current_buffer, backed_up), Qnil,
		     doc: /* Non-nil if this buffer's file has been backed up.
Backing up is done before the first time the file is saved.  */);

  DEFVAR_PER_BUFFER ("buffer-saved-size", &BVAR (current_buffer, save_length),
		     Qintegerp,
		     doc: /* Length of current buffer when last read in, saved or auto-saved.
0 initially.
-1 means auto-saving turned off until next real save.

If you set this to -2, that means don't turn off auto-saving in this buffer
if its text size shrinks.   If you use `buffer-swap-text' on a buffer,
you probably should set this to -2 in that buffer.  */);

  DEFVAR_PER_BUFFER ("selective-display", &BVAR (current_buffer, selective_display),
		     Qnil,
		     doc: /* Non-nil enables selective display.

An integer N as value means display only lines
that start with less than N columns of space.

A value of t means that the character ^M makes itself and
all the rest of the line invisible; also, when saving the buffer
in a file, save the ^M as a newline.  This usage is obsolete; use
overlays or text properties instead.  */);

  DEFVAR_PER_BUFFER ("selective-display-ellipses",
		     &BVAR (current_buffer, selective_display_ellipses),
		     Qnil,
		     doc: /* Non-nil means display ... on previous line when a line is invisible.  */);

  DEFVAR_PER_BUFFER ("overwrite-mode", &BVAR (current_buffer, overwrite_mode),
		     Qoverwrite_mode,
		     doc: /* Non-nil if self-insertion should replace existing text.
The value should be one of `overwrite-mode-textual',
`overwrite-mode-binary', or nil.
If it is `overwrite-mode-textual', self-insertion still
inserts at the end of a line, and inserts when point is before a tab,
until the tab is filled in.
If `overwrite-mode-binary', self-insertion replaces newlines and tabs too.  */);

  DEFVAR_PER_BUFFER ("buffer-display-table", &BVAR (current_buffer, display_table),
		     Qnil,
		     doc: /* Display table that controls display of the contents of current buffer.

If this variable is nil, the value of `standard-display-table' is used.
Each window can have its own, overriding display table, see
`set-window-display-table' and `window-display-table'.

The display table is a char-table created with `make-display-table'.
A char-table is an array indexed by character codes.  Normal array
primitives `aref' and `aset' can be used to access elements of a char-table.

Each of the char-table elements control how to display the corresponding
text character: the element at index C in the table says how to display
the character whose code is C.  Each element should be a vector of
characters or nil.  The value nil means display the character in the
default fashion; otherwise, the characters from the vector are delivered
to the screen instead of the original character.

For example, (aset buffer-display-table ?X [?Y]) tells Emacs
to display a capital Y instead of each X character.

In addition, a char-table has six extra slots to control the display of:

  the end of a truncated screen line (extra-slot 0, a single character);
  the end of a continued line (extra-slot 1, a single character);
  the escape character used to display character codes in octal
    (extra-slot 2, a single character);
  the character used as an arrow for control characters (extra-slot 3,
    a single character);
  the decoration indicating the presence of invisible lines (extra-slot 4,
    a vector of characters);
  the character used to draw the border between side-by-side windows
    (extra-slot 5, a single character).

See also the functions `display-table-slot' and `set-display-table-slot'.  */);

  DEFVAR_PER_BUFFER ("left-margin-width", &BVAR (current_buffer, left_margin_cols),
		     Qintegerp,
		     doc: /* Width in columns of left marginal area for display of a buffer.
A value of nil means no marginal area.

Setting this variable does not take effect until a new buffer is displayed
in a window.  To make the change take effect, call `set-window-buffer'.  */);

  DEFVAR_PER_BUFFER ("right-margin-width", &BVAR (current_buffer, right_margin_cols),
		     Qintegerp,
		     doc: /* Width in columns of right marginal area for display of a buffer.
A value of nil means no marginal area.

Setting this variable does not take effect until a new buffer is displayed
in a window.  To make the change take effect, call `set-window-buffer'.  */);

  DEFVAR_PER_BUFFER ("left-fringe-width", &BVAR (current_buffer, left_fringe_width),
		     Qintegerp,
		     doc: /* Width of this buffer's left fringe (in pixels).
A value of 0 means no left fringe is shown in this buffer's window.
A value of nil means to use the left fringe width from the window's frame.

Setting this variable does not take effect until a new buffer is displayed
in a window.  To make the change take effect, call `set-window-buffer'.  */);

  DEFVAR_PER_BUFFER ("right-fringe-width", &BVAR (current_buffer, right_fringe_width),
		     Qintegerp,
		     doc: /* Width of this buffer's right fringe (in pixels).
A value of 0 means no right fringe is shown in this buffer's window.
A value of nil means to use the right fringe width from the window's frame.

Setting this variable does not take effect until a new buffer is displayed
in a window.  To make the change take effect, call `set-window-buffer'.  */);

  DEFVAR_PER_BUFFER ("fringes-outside-margins", &BVAR (current_buffer, fringes_outside_margins),
		     Qnil,
		     doc: /* Non-nil means to display fringes outside display margins.
A value of nil means to display fringes between margins and buffer text.

Setting this variable does not take effect until a new buffer is displayed
in a window.  To make the change take effect, call `set-window-buffer'.  */);

  DEFVAR_PER_BUFFER ("scroll-bar-width", &BVAR (current_buffer, scroll_bar_width),
		     Qintegerp,
		     doc: /* Width of this buffer's vertical scroll bars in pixels.
A value of nil means to use the scroll bar width from the window's frame.  */);

  DEFVAR_PER_BUFFER ("scroll-bar-height", &BVAR (current_buffer, scroll_bar_height),
		     Qintegerp,
		     doc: /* Height of this buffer's horizontal scroll bars in pixels.
A value of nil means to use the scroll bar height from the window's frame.  */);

  DEFVAR_PER_BUFFER ("vertical-scroll-bar", &BVAR (current_buffer, vertical_scroll_bar_type),
		     Qvertical_scroll_bar,
		     doc: /* Position of this buffer's vertical scroll bar.
The value takes effect whenever you tell a window to display this buffer;
for instance, with `set-window-buffer' or when `display-buffer' displays it.

A value of `left' or `right' means put the vertical scroll bar at that side
of the window; a value of nil means don't show any vertical scroll bars.
A value of t (the default) means do whatever the window's frame specifies.  */);

  DEFVAR_PER_BUFFER ("horizontal-scroll-bar", &BVAR (current_buffer, horizontal_scroll_bar_type),
		     Qnil,
		     doc: /* Position of this buffer's horizontal scroll bar.
The value takes effect whenever you tell a window to display this buffer;
for instance, with `set-window-buffer' or when `display-buffer' displays it.

A value of `bottom' means put the horizontal scroll bar at the bottom of
the window; a value of nil means don't show any horizontal scroll bars.
A value of t (the default) means do whatever the window's frame
specifies.  */);

  DEFVAR_PER_BUFFER ("indicate-empty-lines",
		     &BVAR (current_buffer, indicate_empty_lines), Qnil,
		     doc: /* Visually indicate unused ("empty") screen lines after the buffer end.
If non-nil, a bitmap is displayed in the left fringe of a window
on graphical displays for each screen line that doesn't correspond
to any buffer text.  */);

  DEFVAR_PER_BUFFER ("indicate-buffer-boundaries",
		     &BVAR (current_buffer, indicate_buffer_boundaries), Qnil,
		     doc: /* Visually indicate buffer boundaries and scrolling.
If non-nil, the first and last line of the buffer are marked in the fringe
of a window on graphical displays with angle bitmaps, or if the window can be
scrolled, the top and bottom line of the window are marked with up and down
arrow bitmaps.

If value is a symbol `left' or `right', both angle and arrow bitmaps
are displayed in the left or right fringe, resp.  Any other value
that doesn't look like an alist means display the angle bitmaps in
the left fringe but no arrows.

You can exercise more precise control by using an alist as the
value.  Each alist element (INDICATOR . POSITION) specifies
where to show one of the indicators.  INDICATOR is one of `top',
`bottom', `up', `down', or t, which specifies the default position,
and POSITION is one of `left', `right', or nil, meaning do not show
this indicator.

For example, ((top . left) (t . right)) places the top angle bitmap in
left fringe, the bottom angle bitmap in right fringe, and both arrow
bitmaps in right fringe.  To show just the angle bitmaps in the left
fringe, but no arrow bitmaps, use ((top .  left) (bottom . left)).  */);

  DEFVAR_PER_BUFFER ("fringe-indicator-alist",
		     &BVAR (current_buffer, fringe_indicator_alist), Qnil,
		     doc: /* Mapping from logical to physical fringe indicator bitmaps.
The value is an alist where each element (INDICATOR . BITMAPS)
specifies the fringe bitmaps used to display a specific logical
fringe indicator.

INDICATOR specifies the logical indicator type which is one of the
following symbols: `truncation' , `continuation', `overlay-arrow',
`top', `bottom', `top-bottom', `up', `down', empty-line', or `unknown'.

BITMAPS is a list of symbols (LEFT RIGHT [LEFT1 RIGHT1]) which specifies
the actual bitmap shown in the left or right fringe for the logical
indicator.  LEFT and RIGHT are the bitmaps shown in the left and/or
right fringe for the specific indicator.  The LEFT1 or RIGHT1 bitmaps
are used only for the `bottom' and `top-bottom' indicators when the
last (only) line has no final newline.  BITMAPS may also be a single
symbol which is used in both left and right fringes.  */);

  DEFVAR_PER_BUFFER ("fringe-cursor-alist",
		     &BVAR (current_buffer, fringe_cursor_alist), Qnil,
		     doc: /* Mapping from logical to physical fringe cursor bitmaps.
The value is an alist where each element (CURSOR . BITMAP)
specifies the fringe bitmaps used to display a specific logical
cursor type in the fringe.

CURSOR specifies the logical cursor type which is one of the following
symbols: `box' , `hollow', `bar', `hbar', or `hollow-small'.  The last
one is used to show a hollow cursor on narrow lines display lines
where the normal hollow cursor will not fit.

BITMAP is the corresponding fringe bitmap shown for the logical
cursor type.  */);

  DEFVAR_PER_BUFFER ("scroll-up-aggressively",
		     &BVAR (current_buffer, scroll_up_aggressively), Qfraction,
		     doc: /* How far to scroll windows upward.
If you move point off the bottom, the window scrolls automatically.
This variable controls how far it scrolls.  The value nil, the default,
means scroll to center point.  A fraction means scroll to put point
that fraction of the window's height from the bottom of the window.
When the value is 0.0, point goes at the bottom line, which in the
simple case that you moved off with C-f means scrolling just one line.
1.0 means point goes at the top, so that in that simple case, the
window scrolls by a full window height.  Meaningful values are
between 0.0 and 1.0, inclusive.  */);

  DEFVAR_PER_BUFFER ("scroll-down-aggressively",
		     &BVAR (current_buffer, scroll_down_aggressively), Qfraction,
		     doc: /* How far to scroll windows downward.
If you move point off the top, the window scrolls automatically.
This variable controls how far it scrolls.  The value nil, the default,
means scroll to center point.  A fraction means scroll to put point
that fraction of the window's height from the top of the window.
When the value is 0.0, point goes at the top line, which in the
simple case that you moved off with C-b means scrolling just one line.
1.0 means point goes at the bottom, so that in that simple case, the
window scrolls by a full window height.  Meaningful values are
between 0.0 and 1.0, inclusive.  */);

  DEFVAR_LISP ("before-change-functions", Vbefore_change_functions,
	       doc: /* List of functions to call before each text change.
Two arguments are passed to each function: the positions of
the beginning and end of the range of old text to be changed.
\(For an insertion, the beginning and end are at the same place.)
No information is given about the length of the text after the change.

Buffer changes made while executing the `before-change-functions'
don't call any before-change or after-change functions.
That's because `inhibit-modification-hooks' is temporarily set non-nil.

If an unhandled error happens in running these functions,
the variable's value remains nil.  That prevents the error
from happening repeatedly and making Emacs nonfunctional.  */);
  Vbefore_change_functions = Qnil;

  DEFVAR_LISP ("after-change-functions", Vafter_change_functions,
	       doc: /* List of functions to call after each text change.
Three arguments are passed to each function: the positions of
the beginning and end of the range of changed text,
and the length in chars of the pre-change text replaced by that range.
\(For an insertion, the pre-change length is zero;
for a deletion, that length is the number of chars deleted,
and the post-change beginning and end are at the same place.)

Buffer changes made while executing the `after-change-functions'
don't call any before-change or after-change functions.
That's because `inhibit-modification-hooks' is temporarily set non-nil.

If an unhandled error happens in running these functions,
the variable's value remains nil.  That prevents the error
from happening repeatedly and making Emacs nonfunctional.  */);
  Vafter_change_functions = Qnil;

  DEFVAR_LISP ("first-change-hook", Vfirst_change_hook,
	       doc: /* A list of functions to call before changing a buffer which is unmodified.
The functions are run using the `run-hooks' function.  */);
  Vfirst_change_hook = Qnil;

  DEFVAR_PER_BUFFER ("buffer-undo-list", &BVAR (current_buffer, undo_list), Qnil,
		     doc: /* List of undo entries in current buffer.
Recent changes come first; older changes follow newer.

An entry (BEG . END) represents an insertion which begins at
position BEG and ends at position END.

An entry (TEXT . POSITION) represents the deletion of the string TEXT
from (abs POSITION).  If POSITION is positive, point was at the front
of the text being deleted; if negative, point was at the end.

An entry (t . TIMESTAMP), where TIMESTAMP is in the style of
`current-time', indicates that the buffer was previously unmodified;
TIMESTAMP is the visited file's modification time, as of that time.
If the modification time of the most recent save is different, this
entry is obsolete.

An entry (t . 0) means the buffer was previously unmodified but
its time stamp was unknown because it was not associated with a file.
An entry (t . -1) is similar, except that it means the buffer's visited
file did not exist.

An entry (nil PROPERTY VALUE BEG . END) indicates that a text property
was modified between BEG and END.  PROPERTY is the property name,
and VALUE is the old value.

An entry (apply FUN-NAME . ARGS) means undo the change with
\(apply FUN-NAME ARGS).

An entry (apply DELTA BEG END FUN-NAME . ARGS) supports selective undo
in the active region.  BEG and END is the range affected by this entry
and DELTA is the number of characters added or deleted in that range by
this change.

An entry (MARKER . DISTANCE) indicates that the marker MARKER
was adjusted in position by the offset DISTANCE (an integer).

An entry of the form POSITION indicates that point was at the buffer
location given by the integer.  Undoing an entry of this form places
point at POSITION.

Entries with value nil mark undo boundaries.  The undo command treats
the changes between two undo boundaries as a single step to be undone.

If the value of the variable is t, undo information is not recorded.  */);

  DEFVAR_PER_BUFFER ("mark-active", &BVAR (current_buffer, mark_active), Qnil,
		     doc: /* Non-nil means the mark and region are currently active in this buffer.  */);

  DEFVAR_PER_BUFFER ("cache-long-scans", &BVAR (current_buffer, cache_long_scans), Qnil,
		     doc: /* Non-nil means that Emacs should use caches in attempt to speedup buffer scans.

There is no reason to set this to nil except for debugging purposes.

Normally, the line-motion functions work by scanning the buffer for
newlines.  Columnar operations (like `move-to-column' and
`compute-motion') also work by scanning the buffer, summing character
widths as they go.  This works well for ordinary text, but if the
buffer's lines are very long (say, more than 500 characters), these
motion functions will take longer to execute.  Emacs may also take
longer to update the display.

If `cache-long-scans' is non-nil, these motion functions cache the
results of their scans, and consult the cache to avoid rescanning
regions of the buffer until the text is modified.  The caches are most
beneficial when they prevent the most searching---that is, when the
buffer contains long lines and large regions of characters with the
same, fixed screen width.

When `cache-long-scans' is non-nil, processing short lines will
become slightly slower (because of the overhead of consulting the
cache), and the caches will use memory roughly proportional to the
number of newlines and characters whose screen width varies.

Bidirectional editing also requires buffer scans to find paragraph
separators.  If you have large paragraphs or no paragraph separators
at all, these scans may be slow.  If `cache-long-scans' is non-nil,
results of these scans are cached.  This doesn't help too much if
paragraphs are of the reasonable (few thousands of characters) size.

The caches require no explicit maintenance; their accuracy is
maintained internally by the Emacs primitives.  Enabling or disabling
the cache should not affect the behavior of any of the motion
functions; it should only affect their performance.  */);

  DEFVAR_PER_BUFFER ("point-before-scroll", &BVAR (current_buffer, point_before_scroll), Qnil,
		     doc: /* Value of point before the last series of scroll operations, or nil.  */);

  DEFVAR_PER_BUFFER ("buffer-file-format", &BVAR (current_buffer, file_format), Qnil,
		     doc: /* List of formats to use when saving this buffer.
Formats are defined by `format-alist'.  This variable is
set when a file is visited.  */);

  DEFVAR_PER_BUFFER ("buffer-auto-save-file-format",
		     &BVAR (current_buffer, auto_save_file_format), Qnil,
		     doc: /* Format in which to write auto-save files.
Should be a list of symbols naming formats that are defined in `format-alist'.
If it is t, which is the default, auto-save files are written in the
same format as a regular save would use.  */);

  DEFVAR_PER_BUFFER ("buffer-invisibility-spec",
		     &BVAR (current_buffer, invisibility_spec), Qnil,
		     doc: /* Invisibility spec of this buffer.
The default is t, which means that text is invisible if it has a non-nil
`invisible' property.
This variable can also be a list.  The list can have two kinds of elements:
`ATOM' and `(ATOM . ELLIPSIS)'.  A text character is invisible if its
`invisible' property is `ATOM', or has an `invisible' property that is a list
that contains `ATOM'.
If the `(ATOM . ELLIPSIS)' form is used, and `ELLIPSIS' is non-nil, an
ellipsis will be displayed after the invisible characters.
Setting this variable is very fast, much faster than scanning all the text in
the buffer looking for properties to change.  */);

  DEFVAR_PER_BUFFER ("last-selected-window",
		     &BVAR (current_buffer, last_selected_window), Qnil,
		     doc: /* Last window displaying this buffer.
Nil if that window no longer displays this buffer.  */);
  DEFSYM (Qlast_selected_window, "last-selected-window");

  DEFVAR_PER_BUFFER ("buffer-display-count",
		     &BVAR (current_buffer, display_count), Qintegerp,
		     doc: /* A number incremented each time this buffer is displayed in a window.
The function `set-window-buffer' increments it.  */);

  DEFVAR_PER_BUFFER ("buffer-display-time",
		     &BVAR (current_buffer, display_time), Qnil,
		     doc: /* Time stamp updated each time this buffer is displayed in a window.
The function `set-window-buffer' updates this variable
to the value obtained by calling `current-time'.
If the buffer has never been shown in a window, the value is nil.  */);

  DEFVAR_LISP ("transient-mark-mode", Vtransient_mark_mode,
	       doc: /*  Non-nil if Transient Mark mode is enabled.
See the command `transient-mark-mode' for a description of this minor mode.

Non-nil also enables highlighting of the region whenever the mark is active.
The region is highlighted with the `region' face.
The variable `highlight-nonselected-windows' controls whether to highlight
all windows or just the selected window.

Lisp programs may give this variable certain special values:

- The symbol `lambda' enables Transient Mark mode temporarily.
  The mode is disabled again after any subsequent action that would
  normally deactivate the mark (e.g. buffer modification).

- The pair (only . OLDVAL) enables Transient Mark mode
  temporarily.  After any subsequent point motion command that is
  not shift-translated, or any other action that would normally
  deactivate the mark (e.g. buffer modification), the value of
  `transient-mark-mode' is set to OLDVAL.  */);
  Vtransient_mark_mode = Qnil;

  DEFVAR_LISP ("inhibit-read-only", Vinhibit_read_only,
	       doc: /* Non-nil means disregard read-only status of buffers or characters.
A non-nil value that is a list means disregard `buffer-read-only' status,
and disregard a `read-only' text property if the property value is a
member of the list.  Any other non-nil value means disregard `buffer-read-only'
and all `read-only' text properties.  */);
  Vinhibit_read_only = Qnil;

  DEFVAR_PER_BUFFER ("cursor-type", &BVAR (current_buffer, cursor_type), Qnil,
		     doc: /* Cursor to use when this buffer is in the selected window.
Values are interpreted as follows:

  t               use the cursor specified for the frame
  nil             don't display a cursor
  box             display a filled box cursor
  (box . SIZE)    display a filled box cursor, but make it
                  hollow if cursor is under masked image larger than
                  SIZE pixels in either dimension.
  hollow          display a hollow box cursor
  bar             display a vertical bar cursor with default width
  (bar . WIDTH)   display a vertical bar cursor with width WIDTH
  hbar            display a horizontal bar cursor with default height
  (hbar . HEIGHT) display a horizontal bar cursor with height HEIGHT
  ANYTHING ELSE   display a hollow box cursor

WIDTH and HEIGHT can't exceed the frame's canonical character size.

When the buffer is displayed in a non-selected window, the
cursor's appearance is instead controlled by the variable
`cursor-in-non-selected-windows'.  */);

  DEFVAR_PER_BUFFER ("line-spacing",
		     &BVAR (current_buffer, extra_line_spacing), Qnumberp,
		     doc: /* Additional space to put between lines when displaying a buffer.
The space is measured in pixels, and put below lines on graphic displays,
see `display-graphic-p'.
If value is a floating point number, it specifies the spacing relative
to the default frame line height.  A value of nil means add no extra space.  */);

  DEFVAR_PER_BUFFER ("cursor-in-non-selected-windows",
		     &BVAR (current_buffer, cursor_in_non_selected_windows), Qnil,
		     doc: /* Non-nil means show a cursor in non-selected windows.
If nil, only shows a cursor in the selected window.
If t, displays a cursor related to the usual cursor type
\(a solid box becomes hollow, a bar becomes a narrower bar).
You can also specify the cursor type as in the `cursor-type' variable.
Use Custom to set this variable and update the display.  */);

  DEFVAR_LISP ("kill-buffer-query-functions", Vkill_buffer_query_functions,
	       doc: /* List of functions called with no args to query before killing a buffer.
The buffer being killed will be current while the functions are running.
See `kill-buffer'.

If any of them returns nil, the buffer is not killed.  Functions run by
this hook are supposed to not change the current buffer.

This hook is not run for internal or temporary buffers created by
`get-buffer-create' or `generate-new-buffer' with argument
INHIBIT-BUFFER-HOOKS non-nil.  */);
  Vkill_buffer_query_functions = Qnil;

  DEFVAR_LISP ("change-major-mode-hook", Vchange_major_mode_hook,
	       doc: /* Normal hook run before changing the major mode of a buffer.
The function `kill-all-local-variables' runs this before doing anything else.  */);
  Vchange_major_mode_hook = Qnil;
  DEFSYM (Qchange_major_mode_hook, "change-major-mode-hook");

  DEFVAR_LISP ("buffer-list-update-hook", Vbuffer_list_update_hook,
	       doc: /* Hook run when the buffer list changes.
Functions (implicitly) running this hook are `get-buffer-create',
`make-indirect-buffer', `rename-buffer', `kill-buffer', `bury-buffer'
and `select-window'.  This hook is not run for internal or temporary
buffers created by `get-buffer-create' or `generate-new-buffer' with
argument INHIBIT-BUFFER-HOOKS non-nil.

Functions run by this hook should avoid calling `select-window' with a
nil NORECORD argument since it may lead to infinite recursion.  */);
  Vbuffer_list_update_hook = Qnil;
  DEFSYM (Qbuffer_list_update_hook, "buffer-list-update-hook");

  DEFVAR_BOOL ("kill-buffer-delete-auto-save-files",
	       kill_buffer_delete_auto_save_files,
	       doc: /* If non-nil, offer to delete any autosave file when killing a buffer.

If `delete-auto-save-files' is nil, any autosave deletion is inhibited.  */);
  kill_buffer_delete_auto_save_files = 0;

  DEFVAR_BOOL ("delete-auto-save-files", delete_auto_save_files,
	       doc: /* Non-nil means delete auto-save file when a buffer is saved.
This is the default.  If nil, auto-save file deletion is inhibited.  */);
  delete_auto_save_files = 1;

  defsubr (&Sbuffer_monospace_p);

  DEFVAR_LISP ("case-fold-search", Vcase_fold_search,
	       doc: /* Non-nil if searches and matches should ignore case.  */);
  Vcase_fold_search = Qt;
  DEFSYM (Qcase_fold_search, "case-fold-search");
  Fmake_variable_buffer_local (Qcase_fold_search);

  defsubr (&Sbuffer_live_p);
  defsubr (&Sbuffer_list);
  defsubr (&Sget_buffer);
  defsubr (&Sget_file_buffer);
  defsubr (&Sget_truename_buffer);
  defsubr (&Sfind_buffer);
  defsubr (&Sget_buffer_create);
  defsubr (&Smake_indirect_buffer);
  defsubr (&Smake_mode_overlay__enter_buffer);
  defsubr (&Smake_mode_overlay__exit_buffer);
  defsubr (&Smake_mode__overlay);
  defsubr (&Smode_overlay_indirect_p);
  defsubr (&Sgenerate_new_buffer_name);
  defsubr (&Sbuffer_name);
  defsubr (&Sbuffer_last_name);
  defsubr (&Sbuffer_file_name);
  defsubr (&Sbuffer_base_buffer);
  defsubr (&Sbuffer_local_value);
  defsubr (&Sbuffer_local_variables);
  defsubr (&Sbuffer_modified_p);
  defsubr (&Sforce_mode_line_update);
  defsubr (&Sset_buffer_modified_p);
  defsubr (&Sbuffer_modified_tick);
  defsubr (&Sinternal__set_buffer_modified_tick);
  defsubr (&Sbuffer_chars_modified_tick);
  defsubr (&Srename_buffer);
  defsubr (&Sother_buffer);
  defsubr (&Sbuffer_enable_undo);
  defsubr (&Skill_buffer);
  defsubr (&Sbury_buffer_internal);
  defsubr (&Sset_buffer_major_mode);
  defsubr (&Scurrent_buffer);
  defsubr (&Sset_buffer);
  defsubr (&Sbarf_if_buffer_read_only);
  defsubr (&Serase_buffer);
  defsubr (&Sbuffer_swap_text);
  defsubr (&Sset_buffer_multibyte);
  defsubr (&Skill_all_local_variables);

  defsubr (&Soverlayp);
  defsubr (&Smake_overlay);
  defsubr (&Sdelete_overlay);
  defsubr (&Sdelete_all_overlays);
  defsubr (&Smove_overlay);
  defsubr (&Soverlay_start);
  defsubr (&Soverlay_end);
  defsubr (&Soverlay_buffer);
  defsubr (&Soverlay_properties);
  defsubr (&Soverlay_on_enter);
  defsubr (&Soverlay_on_exit);
  defsubr (&Soverlays_at);
  defsubr (&Soverlays_in);
  defsubr (&Snext_overlay_change);
  defsubr (&Sprevious_overlay_change);
  defsubr (&Soverlay_recenter);
  defsubr (&Soverlay_lists);
  defsubr (&Soverlay_get);
  defsubr (&Soverlay_put);
  defsubr (&Srestore_buffer_modified_p);

  DEFSYM (Qautosaved, "autosaved");

#ifdef ITREE_DEBUG
  defsubr (&Soverlay_tree);
#endif

  DEFSYM (Qassq_delete_all, "assq-delete-all");
  DEFSYM (Qkill_buffer__possibly_save, "kill-buffer--possibly-save");
  DEFSYM (Qbefore_revert_hook, "before-revert-hook");
  DEFSYM (Qdelete_all_mode_overlays, "delete-all-mode-overlays");
  DEFSYM (Qfont_lock_extra_managed_props, "font-lock-extra-managed-props");
  DEFSYM (Qfont_lock_fontify_region, "font-lock-fontify-region");
  DEFSYM (Qbuffer_stale_function, "buffer-stale-function");
  DEFSYM (Qignore, "ignore");
  DEFSYM (Qtemporary_goal_column, "temporary-goal-column");
  DEFSYM (Qhl_line_sticky_flag, "hl-line-sticky-flag");
  DEFSYM (Qhl_line_unhighlight, "hl-line-unhighlight");

  Fput (intern_c_string ("erase-buffer"), Qdisabled, Qt);
}
