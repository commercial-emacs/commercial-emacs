/* Buffer insertion/deletion and gap motion for GNU Emacs. -*- coding: utf-8 -*-
   Copyright (C) 1985-1986, 1993-1995, 1997-2024 Free Software
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

#include <intprops.h>

#include "lisp.h"
#include "composite.h"
#include "intervals.h"
#include "character.h"
#include "buffer.h"
#include "window.h"
#include "region-cache.h"
#include "pdumper.h"
#include "dispextern.h"

#ifdef HAVE_TREE_SITTER
#include "tree-sitter.h"
#endif

static void insert_from_string_1 (Lisp_Object, ptrdiff_t, ptrdiff_t, ptrdiff_t,
				  ptrdiff_t, bool, bool);
static void insert_from_buffer_1 (struct buffer *, ptrdiff_t, ptrdiff_t, bool);
static void signal_before_change (ptrdiff_t, ptrdiff_t, ptrdiff_t *);

/* Also used in marker.c to enable expensive marker checks.  */

#ifdef MARKER_DEBUG

static void
check_markers (void)
{
  struct Lisp_Marker *tail;
  bool multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));

  for (tail = BUF_MARKERS (current_buffer); tail; tail = tail->next)
    {
      if (tail->buffer->text != current_buffer->text)
	emacs_abort ();
      if (tail->charpos > Z)
	emacs_abort ();
      if (tail->bytepos > Z_BYTE)
	emacs_abort ();
      if (multibyte && ! CHAR_HEAD_P (FETCH_BYTE (tail->bytepos)))
	emacs_abort ();
    }
}

#else /* not MARKER_DEBUG */

#define check_markers() do { } while (0)

#endif /* MARKER_DEBUG */

/* If the selected window's old pointm is adjacent or covered by the
   region from FROM to TO, unsuspend auto hscroll in that window.  */

static void
adjust_suspend_auto_hscroll (ptrdiff_t from, ptrdiff_t to)
{
  if (WINDOWP (selected_window))
    {
      struct window *w = XWINDOW (selected_window);

      if (BUFFERP (w->contents)
	  && XBUFFER (w->contents) == current_buffer
	  && XMARKER (w->old_pointm)->charpos >= from
	  && XMARKER (w->old_pointm)->charpos <= to)
	w->suspend_auto_hscroll = 0;
    }
}


/* Adjust all markers for a deletion
   whose range in bytes is FROM_BYTE to TO_BYTE.
   The range in charpos is FROM to TO.

   This function assumes that the gap is adjacent to
   or inside of the range being deleted.  */

void
adjust_markers_for_delete (ptrdiff_t from, ptrdiff_t from_byte,
			   ptrdiff_t to, ptrdiff_t to_byte)
{
  struct Lisp_Marker *m;
  ptrdiff_t charpos;

  adjust_suspend_auto_hscroll (from, to);
  for (m = BUF_MARKERS (current_buffer); m; m = m->next)
    {
      charpos = m->charpos;
      eassert (charpos <= Z);

      /* If the marker is after the deletion,
	 relocate by number of chars / bytes deleted.  */
      if (charpos > to)
	{
	  m->charpos -= to - from;
	  m->bytepos -= to_byte - from_byte;
	}
      /* Here's the case where a marker is inside text being deleted.  */
      else if (charpos > from)
	{
	  m->charpos = from;
	  m->bytepos = from_byte;
	}
    }
  adjust_overlays_for_delete (from, to - from);
}


/* Adjust markers for an insertion that stretches from FROM / FROM_BYTE
   to TO / TO_BYTE.  We have to relocate the charpos of every marker
   that points after the insertion (but not their bytepos).

   When a marker points at the insertion point,
   we advance it if either its insertion-type is t
   or BEFORE_MARKERS is true.  */

static void
adjust_markers_for_insert (ptrdiff_t from, ptrdiff_t from_byte,
			   ptrdiff_t to, ptrdiff_t to_byte, bool before_markers)
{
  struct Lisp_Marker *m;
  ptrdiff_t nchars = to - from;
  ptrdiff_t nbytes = to_byte - from_byte;

  adjust_suspend_auto_hscroll (from, to);
  for (m = BUF_MARKERS (current_buffer); m; m = m->next)
    {
      eassert (m->bytepos >= m->charpos
	       && m->bytepos - m->charpos <= Z_BYTE - Z);

      if (m->bytepos == from_byte)
	{
	  if (m->insertion_type || before_markers)
	    {
	      m->bytepos = to_byte;
	      m->charpos = to;
	    }
	}
      else if (m->bytepos > from_byte)
	{
	  m->bytepos += nbytes;
	  m->charpos += nchars;
	}
    }
  adjust_overlays_for_insert (from, to - from, before_markers);
}

/* Adjust point for an insertion of NBYTES bytes, which are NCHARS characters.

   This is used only when the value of point changes due to an insert
   or delete; it does not represent a conceptual change in point as a
   marker.  In particular, point is not crossing any interval
   boundaries, so there's no need to use the usual SET_PT macro.  In
   fact it would be incorrect to do so, because either the old or the
   new value of point is out of sync with the current set of
   intervals.  */

static void
adjust_point (ptrdiff_t nchars, ptrdiff_t nbytes)
{
  SET_BUF_PT_BOTH (current_buffer, PT + nchars, PT_BYTE + nbytes);
  /* In a single-byte buffer, the two positions must be equal.  */
  eassert (PT_BYTE >= PT && PT_BYTE - PT <= ZV_BYTE - ZV);
}

/* Adjust markers for a replacement of a text at FROM (FROM_BYTE) of
   length OLD_CHARS (OLD_BYTES) to a new text of length NEW_CHARS
   (NEW_BYTES).  It is assumed that OLD_CHARS > 0, i.e., this is not
   an insertion.  */

static void
adjust_markers_for_replace (ptrdiff_t from, ptrdiff_t from_byte,
			    ptrdiff_t old_chars, ptrdiff_t old_bytes,
			    ptrdiff_t new_chars, ptrdiff_t new_bytes)
{
  register struct Lisp_Marker *m;
  ptrdiff_t prev_to_byte = from_byte + old_bytes;
  ptrdiff_t diff_chars = new_chars - old_chars;
  ptrdiff_t diff_bytes = new_bytes - old_bytes;

  adjust_suspend_auto_hscroll (from, from + old_chars);

  /* FIXME: When OLD_CHARS is 0, this "replacement" is really just an
     insertion, but the behavior we provide here in that case is that of
     `insert-before-markers` rather than that of `insert`.
     Maybe not a bug, but not a feature either.  */
  for (m = BUF_MARKERS (current_buffer); m; m = m->next)
    {
      if (m->bytepos >= prev_to_byte)
	{
	  m->charpos += diff_chars;
	  m->bytepos += diff_bytes;
	}
      else if (m->bytepos > from_byte)
	{
	  m->charpos = from;
	  m->bytepos = from_byte;
	}
    }

  check_markers ();

  adjust_overlays_for_insert (from + old_chars, new_chars, true);
  if (old_chars)
    adjust_overlays_for_delete (from, old_chars);
}

/* Starting at POS (BYTEPOS), find the byte position corresponding to
   ENDPOS, which could be either before or after POS.  */
static ptrdiff_t
count_bytes (ptrdiff_t pos, ptrdiff_t bytepos, ptrdiff_t endpos)
{
  eassert (BEG_BYTE <= bytepos && bytepos <= Z_BYTE
	   && BEG <= endpos && endpos <= Z);

  if (pos <= endpos)
    for ( ; pos < endpos; pos++)
      bytepos += next_char_len (bytepos);
  else
    for ( ; pos > endpos; pos--)
      bytepos -= prev_char_len (bytepos);

  return bytepos;
}

/* Adjust byte positions of markers when their character positions
   didn't change.  This is used in several places that replace text,
   but keep the character positions of the markers unchanged -- the
   byte positions could still change due to different numbers of bytes
   in the new text.

   FROM (FROM_BYTE) and TO (TO_BYTE) specify the region of text where
   changes have been done.  TO_Z, if non-zero, means all the markers
   whose positions are after TO should also be adjusted.  */
void
adjust_markers_bytepos (ptrdiff_t from, ptrdiff_t from_byte,
			ptrdiff_t to, ptrdiff_t to_byte, int to_z)
{
  register struct Lisp_Marker *m;
  ptrdiff_t beg = from, begbyte = from_byte;

  adjust_suspend_auto_hscroll (from, to);

  if (Z == Z_BYTE || (!to_z && to == to_byte))
    {
      /* Make sure each affected marker's bytepos is equal to
	 its charpos.  */
      for (m = BUF_MARKERS (current_buffer); m; m = m->next)
	{
	  if (m->bytepos > from_byte
	      && (to_z || m->bytepos <= to_byte))
	    m->bytepos = m->charpos;
	}
    }
  else
    {
      for (m = BUF_MARKERS (current_buffer); m; m = m->next)
	{
	  /* Recompute each affected marker's bytepos.  */
	  if (m->bytepos > from_byte
	      && (to_z || m->bytepos <= to_byte))
	    {
	      if (m->charpos < beg
		  && beg - m->charpos > m->charpos - from)
		{
		  beg = from;
		  begbyte = from_byte;
		}
	      m->bytepos = count_bytes (beg, begbyte, m->charpos);
	      beg = m->charpos;
	      begbyte = m->bytepos;
	    }
	}
    }

  /* Make sure cached charpos/bytepos is invalid.  */
  clear_charpos_cache (current_buffer);
}

void
buffer_overflow (void)
{
  error ("Maximum buffer size exceeded");
}

static inline void
anchor_gap (void)
{
  if (GAP_SIZE > 0)
    *GAP_BEG_ADDR = 0;
}

void
move_gap (ptrdiff_t charpos, ptrdiff_t bytepos)
{
  BUF_COMPUTE_UNCHANGED (current_buffer, charpos, GPT);
  if (bytepos < GPT_BYTE)
    {
      /* moving characters up moves gap down */
      const ptrdiff_t nbytes = GPT_BYTE - bytepos;
      memmove (GAP_END_ADDR - nbytes, GAP_BEG_ADDR - nbytes, nbytes);
    }
  else
    {
      /* moving characters down moves gap up */
      const ptrdiff_t nbytes = bytepos - GPT_BYTE;
      memmove (GAP_BEG_ADDR, GAP_END_ADDR, nbytes);
    }
  GPT_BYTE = bytepos;
  GPT = charpos;
  anchor_gap ();
}

/* Make the gap NBYTES_ADDED bytes longer.  */

static void
make_gap_larger (ptrdiff_t nbytes_added)
{
  ptrdiff_t restore_gap_pt;
  ptrdiff_t restore_gap_pt_byte;
  ptrdiff_t restore_gap_size;
  ptrdiff_t current_size = Z_BYTE - BEG_BYTE + GAP_SIZE;

  if (BUF_BYTES_MAX - current_size < nbytes_added)
    buffer_overflow ();

  /* If we have to get more space, get enough to last a while;
     but do not exceed the maximum buffer size.  */
  nbytes_added = min (nbytes_added + GAP_BYTES_DFL,
		      BUF_BYTES_MAX - current_size);

  enlarge_buffer_text (current_buffer, nbytes_added);

  restore_gap_size = GAP_SIZE;
  GAP_SIZE = nbytes_added;
  restore_gap_pt = GPT;
  GPT = Z + restore_gap_size;
  restore_gap_pt_byte = GPT_BYTE;
  GPT_BYTE = Z_BYTE + restore_gap_size;

  /* Move the new gap down to be consecutive with the end of the old one.
     Do not update beg_unchanged end_unchanged! */
  const ptrdiff_t restore_beg_unchanged = current_buffer->text->beg_unchanged,
    restore_end_unchanged = current_buffer->text->end_unchanged;
  move_gap (restore_gap_pt + restore_gap_size, restore_gap_pt_byte + restore_gap_size);
  current_buffer->text->beg_unchanged = restore_beg_unchanged;
  current_buffer->text->end_unchanged = restore_end_unchanged;

  /* Now combine the two into one large gap.  */
  GAP_SIZE += restore_gap_size;
  GPT = restore_gap_pt;
  GPT_BYTE = restore_gap_pt_byte;

  /* Put an anchor.  */
  *Z_ADDR = 0;
}

#if defined USE_MMAP_FOR_BUFFERS || defined REL_ALLOC || defined DOUG_LEA_MALLOC

/* Make the gap NBYTES_REMOVED bytes shorter.  */

static void
make_gap_smaller (ptrdiff_t nbytes_removed)
{
  ptrdiff_t restore_gap_pt;
  ptrdiff_t restore_gap_pt_byte;
  ptrdiff_t restore_Z;
  ptrdiff_t restore_Z_byte;
  ptrdiff_t restore_beg_unchanged;
  ptrdiff_t new_gap_size;

  /* Make sure the gap is at least GAP_BYTES_MIN bytes.  */
  nbytes_removed = min (GAP_SIZE - GAP_BYTES_MIN, nbytes_removed);

  restore_gap_pt = GPT;
  restore_gap_pt_byte = GPT_BYTE;
  new_gap_size = GAP_SIZE - nbytes_removed;
  restore_Z = Z;
  restore_Z_byte = Z_BYTE;
  restore_beg_unchanged = BEG_UNCHANGED;

  /* Pretend that the last unwanted part of the gap is the entire gap,
     and that the first desired part of the gap is part of the buffer
     text.  */
  memset (GAP_BEG_ADDR, 0, new_gap_size);
  GPT += new_gap_size;
  GPT_BYTE += new_gap_size;
  Z += new_gap_size;
  Z_BYTE += new_gap_size;
  GAP_SIZE = nbytes_removed;

  /* Move the unwanted pretend gap to the end of the buffer.  */
  move_gap (Z, Z_BYTE);
  enlarge_buffer_text (current_buffer, -nbytes_removed);

  /* Now restore the desired gap.  */
  GAP_SIZE = new_gap_size;
  GPT = restore_gap_pt;
  GPT_BYTE = restore_gap_pt_byte;
  Z = restore_Z;
  Z_BYTE = restore_Z_byte;
  BEG_UNCHANGED = restore_beg_unchanged;

  /* Put an anchor.  */
  *Z_ADDR = 0;
}

#endif /* USE_MMAP_FOR_BUFFERS || REL_ALLOC || DOUG_LEA_MALLOC */

void
make_gap (ptrdiff_t nbytes_added)
{
  if (nbytes_added >= 0)
    /* With set-buffer-multibyte on a large buffer, we can end up growing the
     * buffer *many* times.  Avoid an O(N^2) behavior by increasing by an
     * amount at least proportional to the size of the buffer.
     * On my test (a 223.9MB zip file on a Thinkpad T61):
     * With /5    =>  24s
     * With /32   =>  25s
     * With /64   =>  26s
     * With /128  =>  28s
     * With /1024 =>  51s
     * With /4096 => 131s
     * With /∞    => gave up after 858s
     * Of course, ideally we should never call set-buffer-multibyte on
     * a non-empty buffer (e.g. use buffer-swap-text instead).
     * We chose /64 because it already brings almost the best performance while
     * limiting the potential wasted memory to 1.5%.  */
    make_gap_larger (max (nbytes_added, (Z - BEG) / 64));
#if defined USE_MMAP_FOR_BUFFERS || defined REL_ALLOC || defined DOUG_LEA_MALLOC
  else
    make_gap_smaller (-nbytes_added);
#endif
}

/* Add NBYTES to B's gap.  It's enough to temporarily
   fake current_buffer and avoid real switch to B.  */

void
make_gap_1 (struct buffer *b, ptrdiff_t nbytes)
{
  struct buffer *oldb = current_buffer;

  current_buffer = b;
  make_gap (nbytes);
  current_buffer = oldb;
}

/* Copy NBYTES bytes of text from FROM_ADDR to TO_ADDR.
   FROM_MULTIBYTE says whether the incoming text is multibyte.
   TO_MULTIBYTE says whether to store the text as multibyte.
   If FROM_MULTIBYTE != TO_MULTIBYTE, we convert.

   Return the number of bytes stored at TO_ADDR.  */

ptrdiff_t
copy_text (const unsigned char *from_addr, unsigned char *to_addr,
	   ptrdiff_t nbytes, bool from_multibyte, bool to_multibyte)
{
  if (from_multibyte == to_multibyte)
    {
      memcpy (to_addr, from_addr, nbytes);
      return nbytes;
    }
  else if (from_multibyte)
    {
      ptrdiff_t nchars = 0;
      ptrdiff_t bytes_left = nbytes;

      while (bytes_left > 0)
	{
	  int thislen, c = string_char_and_length (from_addr, &thislen);
	  if (!ASCII_CHAR_P (c))
	    c &= 0xFF;
	  *to_addr++ = c;
	  from_addr += thislen;
	  bytes_left -= thislen;
	  nchars++;
	}
      return nchars;
    }
  else
    {
      unsigned char *initial_to_addr = to_addr;

      /* Convert single-byte to multibyte.  */
      while (nbytes > 0)
	{
	  int c = *from_addr++;

	  if (!ASCII_CHAR_P (c))
	    {
	      c = BYTE8_TO_CHAR (c);
	      to_addr += CHAR_STRING (c, to_addr);
	      nbytes--;
	    }
	  else
	    /* Special case for speed.  */
	    *to_addr++ = c, nbytes--;
	}
      return to_addr - initial_to_addr;
    }
}

/* Insert a string of specified length before point.
   This function judges multibyteness based on
   enable_multibyte_characters in the current buffer;
   it never converts between single-byte and multibyte.

   DO NOT use this for the contents of a Lisp string or a Lisp buffer!
   prepare_to_modify_buffer could relocate the text.  */

void
insert (const char *string, ptrdiff_t nbytes)
{
  if (nbytes > 0)
    {
      ptrdiff_t len = chars_in_text ((unsigned char *) string, nbytes), opoint;
      insert_1_both (string, len, nbytes, 0, 1, 0);
      opoint = PT - len;
      signal_after_change (opoint, 0, len);
      update_compositions (opoint, PT, CHECK_BORDER);
    }
}

/* Likewise, but inherit text properties from neighboring characters.  */

void
insert_and_inherit (const char *string, ptrdiff_t nbytes)
{
  if (nbytes > 0)
    {
      ptrdiff_t len = chars_in_text ((unsigned char *) string, nbytes), opoint;
      insert_1_both (string, len, nbytes, 1, 1, 0);
      opoint = PT - len;
      signal_after_change (opoint, 0, len);
      update_compositions (opoint, PT, CHECK_BORDER);
    }
}

/* Insert the character C before point.  Do not inherit text properties.  */

void
insert_char (int c)
{
  unsigned char str[MAX_MULTIBYTE_LENGTH];
  int len;

  if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))
    len = CHAR_STRING (c, str);
  else
    {
      len = 1;
      str[0] = c;
    }

  insert ((char *) str, len);
}

/* Insert the null-terminated string S before point.  */

void
insert_string (const char *s)
{
  insert (s, strlen (s));
}

/* Like `insert' except that all markers pointing at the place where
   the insertion happens are adjusted to point after it.
   Don't use this function to insert part of a Lisp string,
   since gc could happen and relocate it.  */

void
insert_before_markers (const char *string, ptrdiff_t nbytes)
{
  if (nbytes > 0)
    {
      ptrdiff_t len = chars_in_text ((unsigned char *) string, nbytes), opoint;
      insert_1_both (string, len, nbytes, 0, 1, 1);
      opoint = PT - len;
      signal_after_change (opoint, 0, len);
      update_compositions (opoint, PT, CHECK_BORDER);
    }
}

/* Likewise, but inherit text properties from neighboring characters.  */

void
insert_before_markers_and_inherit (const char *string,
				   ptrdiff_t nbytes)
{
  if (nbytes > 0)
    {
      ptrdiff_t len = chars_in_text ((unsigned char *) string, nbytes), opoint;
      insert_1_both (string, len, nbytes, 1, 1, 1);
      opoint = PT - len;
      signal_after_change (opoint, 0, len);
      update_compositions (opoint, PT, CHECK_BORDER);
    }
}

#ifdef BYTE_COMBINING_DEBUG

/* See if the bytes before POS/POS_BYTE combine with bytes
   at the start of STRING to form a single character.
   If so, return the number of bytes at the start of STRING
   which combine in this way.  Otherwise, return 0.  */

int
count_combining_before (const unsigned char *string, ptrdiff_t length,
			ptrdiff_t pos, ptrdiff_t pos_byte)
{
  int len, combining_bytes;
  const unsigned char *p;

  if (NILP (current_buffer->enable_multibyte_characters))
    return 0;

  /* At first, we can exclude the following cases:
	(1) STRING[0] can't be a following byte of multibyte sequence.
	(2) POS is the start of the current buffer.
	(3) A character before POS is not a multibyte character.  */
  if (length == 0 || CHAR_HEAD_P (*string)) /* case (1) */
    return 0;
  if (pos_byte == BEG_BYTE)	/* case (2) */
    return 0;
  len = 1;
  p = BYTE_POS_ADDR (pos_byte - 1);
  while (! CHAR_HEAD_P (*p)) p--, len++;
  if (! LEADING_CODE_P (*p)) /* case (3) */
    return 0;

  combining_bytes = BYTES_BY_CHAR_HEAD (*p) - len;
  if (combining_bytes <= 0)
    /* The character preceding POS is, complete and no room for
       combining bytes (combining_bytes == 0), or an independent 8-bit
       character (combining_bytes < 0).  */
    return 0;

  /* We have a combination situation.  Count the bytes at STRING that
     may combine.  */
  p = string + 1;
  while (!CHAR_HEAD_P (*p) && p < string + length)
    p++;

  return min (combining_bytes, p - string);
}

/* See if the bytes after POS/POS_BYTE combine with bytes
   at the end of STRING to form a single character.
   If so, return the number of bytes after POS/POS_BYTE
   which combine in this way.  Otherwise, return 0.  */

int
count_combining_after (const unsigned char *string,
		       ptrdiff_t length, ptrdiff_t pos, ptrdiff_t pos_byte)
{
  ptrdiff_t opos_byte = pos_byte;
  ptrdiff_t i;
  ptrdiff_t bytes;
  unsigned char *bufp;

  if (NILP (current_buffer->enable_multibyte_characters))
    return 0;

  /* At first, we can exclude the following cases:
	(1) The last byte of STRING is an ASCII.
	(2) POS is the last of the current buffer.
	(3) A character at POS can't be a following byte of multibyte
	    character.  */
  if (length > 0 && ASCII_CHAR_P (string[length - 1])) /* case (1) */
    return 0;
  if (pos_byte == Z_BYTE)	/* case (2) */
    return 0;
  bufp = BYTE_POS_ADDR (pos_byte);
  if (CHAR_HEAD_P (*bufp))	/* case (3) */
    return 0;

  i = length - 1;
  while (i >= 0 && ! CHAR_HEAD_P (string[i]))
    {
      i--;
    }
  if (i < 0)
    {
      /* All characters in STRING are not character head.  We must
	 check also preceding bytes at POS.  We are sure that the gap
	 is at POS.  */
      unsigned char *p = BEG_ADDR;
      i = pos_byte - 2;
      while (i >= 0 && ! CHAR_HEAD_P (p[i]))
	i--;
      if (i < 0 || !LEADING_CODE_P (p[i]))
	return 0;

      bytes = BYTES_BY_CHAR_HEAD (p[i]);
      return (bytes <= pos_byte - 1 - i + length
	      ? 0
	      : bytes - (pos_byte - 1 - i + length));
    }
  if (!LEADING_CODE_P (string[i]))
    return 0;

  bytes = BYTES_BY_CHAR_HEAD (string[i]) - (length - i);
  bufp++, pos_byte++;
  while (!CHAR_HEAD_P (*bufp)) bufp++, pos_byte++;

  return min (bytes, pos_byte - opos_byte);
}

#endif

/* Put bidi processing on notice if just inserted C is R2L.  */

static void
detect_bidi (struct buffer *buf, const unsigned char *string, ptrdiff_t nbytes)
{
  if (!NILP (BVAR (buf, enable_multibyte_characters)))
    for (int len, offset = 0;
	 offset < nbytes && NILP (BVAR (buf, bidi_display_reordering));
	 offset += len)
      switch (bidi_get_type (string_char_and_length (&string[offset], &len), NEUTRAL_DIR))
	{
	case STRONG_R:
	case STRONG_AL:
	case RLE:
	case RLO:
	case RLI:
	  bset_bidi_display_reordering (buf, Qt);
	  break;
	default:
	  break;
	}
}

/* Insert a sequence of NCHARS chars which occupy NBYTES bytes
   starting at STRING.  INHERIT non-zero means inherit the text
   properties from neighboring characters; zero means inserted text
   will have no text properties.  PREPARE non-zero means call
   prepare_to_modify_buffer, which checks that the region is not
   read-only, and calls before-change-function and any modification
   properties the text may have.  BEFORE_MARKERS non-zero means adjust
   all markers that point at the insertion place to point after it.  */

void
insert_1_both (const char *string,
	       ptrdiff_t nchars, ptrdiff_t nbytes,
	       bool inherit, bool prepare, bool before_markers)
{
  if (nchars == 0)
    return;

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    nchars = nbytes;

  if (prepare)
    /* Do this before moving and increasing the gap,
       because the before-change hooks might move the gap
       or make it smaller.  */
    prepare_to_modify_buffer (PT, PT, NULL);

  if (PT != GPT)
    move_gap (PT, PT_BYTE);
  if (GAP_SIZE < nbytes)
    make_gap (nbytes - GAP_SIZE);

#ifdef BYTE_COMBINING_DEBUG
  if (count_combining_before (string, nbytes, PT, PT_BYTE)
      || count_combining_after (string, nbytes, PT, PT_BYTE))
    emacs_abort ();
#endif

  /* Record deletion of the surrounding text that combines with
     the insertion.  This, together with recording the insertion,
     will add up to the right stuff in the undo list.  */
  undo_push_insert (PT, nchars);
  modiff_incr (&MODIFF);
  CHARS_MODIFF = MODIFF;

  memcpy (GAP_BEG_ADDR, string, nbytes);

  GAP_SIZE -= nbytes;
  GPT += nchars;
  ZV += nchars;
  Z += nchars;
  GPT_BYTE += nbytes;
  ZV_BYTE += nbytes;
  Z_BYTE += nbytes;
  anchor_gap ();

  eassert (GPT <= GPT_BYTE);

  /* The insert may have been in the unchanged region, so check again.  */
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  detect_bidi (current_buffer, (unsigned char *) string, nbytes);

  adjust_markers_for_insert (PT, PT_BYTE,
			     PT + nchars, PT_BYTE + nbytes,
			     before_markers);

  offset_intervals (current_buffer, PT, nchars);

  if (!inherit && buffer_intervals (current_buffer))
    set_text_properties (make_fixnum (PT), make_fixnum (PT + nchars),
			 Qnil, Qnil, Qnil);

#ifdef HAVE_TREE_SITTER
  tree_sitter_record_change (PT, PT, BUFFER_TO_SITTER (PT), PT + nchars);
#endif

  adjust_point (nchars, nbytes);

  check_markers ();
}

/* Insert the part of the text of STRING, a Lisp object assumed to be
   of type string, consisting of the LENGTH characters (LENGTH_BYTE bytes)
   starting at position POS / POS_BYTE.  If the text of STRING has properties,
   copy them into the buffer.

   It does not work to use `insert' for this, because a GC could happen
   before we copy the stuff into the buffer, and relocate the string
   without insert noticing.  */

void
insert_from_string (Lisp_Object string, ptrdiff_t pos, ptrdiff_t pos_byte,
		    ptrdiff_t length, ptrdiff_t length_byte, bool inherit)
{
  ptrdiff_t opoint = PT;

  if (SCHARS (string) == 0)
    return;

  insert_from_string_1 (string, pos, pos_byte, length, length_byte,
			inherit, 0);
  signal_after_change (opoint, 0, PT - opoint);
  update_compositions (opoint, PT, CHECK_BORDER);
}

/* Like insert_from_string except all markers at the insertion point
   are adjusted to point after it.  */

void
insert_from_string_before_markers (Lisp_Object string,
				   ptrdiff_t pos, ptrdiff_t pos_byte,
				   ptrdiff_t length, ptrdiff_t length_byte,
				   bool inherit)
{
  ptrdiff_t opoint = PT;

  if (SCHARS (string) == 0)
    return;

  insert_from_string_1 (string, pos, pos_byte, length, length_byte,
			inherit, 1);
  signal_after_change (opoint, 0, PT - opoint);
  update_compositions (opoint, PT, CHECK_BORDER);
}

/* Subroutine of the insertion functions above.  */

static void
insert_from_string_1 (Lisp_Object string, ptrdiff_t pos, ptrdiff_t pos_byte,
		      ptrdiff_t nchars, ptrdiff_t nbytes,
		      bool inherit, bool before_markers)
{
  ptrdiff_t outgoing_nbytes = nbytes;
  INTERVAL intervals;

  /* Make OUTGOING_NBYTES describe the text
     as it will be inserted in this buffer.  */

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    outgoing_nbytes = nchars;
  else if (!STRING_MULTIBYTE (string))
    outgoing_nbytes
      = count_size_as_multibyte (SDATA (string) + pos_byte,
				 nbytes);

  /* Do this before moving and increasing the gap,
     because the before-change hooks might move the gap
     or make it smaller.  */
  prepare_to_modify_buffer (PT, PT, NULL);

  if (PT != GPT)
    move_gap (PT, PT_BYTE);
  if (GAP_SIZE < outgoing_nbytes)
    make_gap (outgoing_nbytes - GAP_SIZE);

  /* Copy the string text into the buffer, perhaps converting
     between single-byte and multibyte.  */
  copy_text (SDATA (string) + pos_byte, GAP_BEG_ADDR, nbytes,
	     STRING_MULTIBYTE (string),
	     !NILP (BVAR (current_buffer, enable_multibyte_characters)));

#ifdef BYTE_COMBINING_DEBUG
  /* We have copied text into the gap, but we have not altered
     PT or PT_BYTE yet.  So we can pass PT and PT_BYTE
     to these functions and get the same results as we would
     have got earlier on.  Meanwhile, PT_ADDR does point to
     the text that has been stored by copy_text.  */
  if (count_combining_before (GAP_BEG_ADDR, outgoing_nbytes, PT, PT_BYTE)
      || count_combining_after (GAP_BEG_ADDR, outgoing_nbytes, PT, PT_BYTE))
    emacs_abort ();
#endif

  undo_push_insert (PT, nchars);
  modiff_incr (&MODIFF);
  CHARS_MODIFF = MODIFF;

  GAP_SIZE -= outgoing_nbytes;
  GPT += nchars;
  ZV += nchars;
  Z += nchars;
  GPT_BYTE += outgoing_nbytes;
  ZV_BYTE += outgoing_nbytes;
  Z_BYTE += outgoing_nbytes;
  anchor_gap ();

  eassert (GPT <= GPT_BYTE);

  /* The insert may have been in the unchanged region, so check again.  */
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  if (STRING_MULTIBYTE (string))
    detect_bidi (current_buffer, (unsigned char *) SDATA (string), nbytes);

  adjust_markers_for_insert (PT, PT_BYTE, PT + nchars,
			     PT_BYTE + outgoing_nbytes,
			     before_markers);

  offset_intervals (current_buffer, PT, nchars);

  intervals = string_intervals (string);
  /* Get the intervals for the part of the string we are inserting.  */
  if (nbytes < SBYTES (string))
    intervals = copy_intervals (intervals, pos, nchars);

  /* Insert those intervals.  */
  graft_intervals_into_buffer (intervals, PT, nchars,
			       current_buffer, inherit);

#ifdef HAVE_TREE_SITTER
  tree_sitter_record_change (PT, PT, BUFFER_TO_SITTER (PT), PT + nchars);
#endif

  adjust_point (nchars, outgoing_nbytes);

  check_markers ();
}

/* Insert a sequence of NCHARS chars which occupy NBYTES bytes
   starting at GAP_END_ADDR - NBYTES (if text_at_gap_tail) and at
   GAP_BEG_ADDR (if not text_at_gap_tail).
   Contrary to insert_from_gap, this does not invalidate any cache,
   nor update any markers, nor record any buffer modification information
   of any sort.  */
void
insert_from_gap_1 (ptrdiff_t nchars, ptrdiff_t nbytes, bool text_at_gap_tail)
{
  eassert (NILP (BVAR (current_buffer, enable_multibyte_characters))
           ? nchars == nbytes : nchars <= nbytes);

  GAP_SIZE -= nbytes;
  if (!text_at_gap_tail)
    {
      GPT += nchars;
      GPT_BYTE += nbytes;
    }
  ZV += nchars;
  Z += nchars;
  ZV_BYTE += nbytes;
  Z_BYTE += nbytes;

  /* Ensure multi-byte form ends at gap.  */
  anchor_gap ();
  eassert (GPT <= GPT_BYTE);
}

/* Insert a sequence of NCHARS chars which occupy NBYTES bytes
   starting at GAP_END_ADDR - NBYTES (if text_at_gap_tail) and at
   GAP_BEG_ADDR (if not text_at_gap_tail).  */

void
insert_from_gap (ptrdiff_t nchars, ptrdiff_t nbytes, bool text_at_gap_tail)
{
  ptrdiff_t ins_charpos = GPT, ins_bytepos = GPT_BYTE;

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    nchars = nbytes;

  /* No need to call prepare_to_modify_buffer, since this is called
     from places that replace some region with a different text, so
     prepare_to_modify_buffer was already called by the deletion part
     of this dance.  */
  invalidate_buffer_caches (current_buffer, GPT, GPT);
  undo_push_insert (GPT, nchars);
  modiff_incr (&MODIFF);
  CHARS_MODIFF = MODIFF;

  insert_from_gap_1 (nchars, nbytes, text_at_gap_tail);

  adjust_markers_for_insert (ins_charpos, ins_bytepos,
			     ins_charpos + nchars, ins_bytepos + nbytes, false);

  if (buffer_intervals (current_buffer))
    {
      offset_intervals (current_buffer, ins_charpos, nchars);
      graft_intervals_into_buffer (NULL, ins_charpos, nchars,
				   current_buffer, 0);
    }

#ifdef HAVE_TREE_SITTER
  tree_sitter_record_change (ins_charpos, ins_charpos, BUFFER_TO_SITTER (ins_charpos),
			     ins_charpos + nchars);
#endif

  if (ins_charpos < PT)
    adjust_point (nchars, nbytes);

  check_markers ();
}

/* Insert text from BUF, NCHARS characters starting at CHARPOS, into the
   current buffer.  If the text in BUF has properties, they are absorbed
   into the current buffer.

   It does not work to use `insert' for this, because a malloc could happen
   and relocate BUF's text before the copy happens.  */

void
insert_from_buffer (struct buffer *buf,
		    ptrdiff_t charpos, ptrdiff_t nchars, bool inherit)
{
  ptrdiff_t opoint = PT;

  insert_from_buffer_1 (buf, charpos, nchars, inherit);
  signal_after_change (opoint, 0, PT - opoint);
  update_compositions (opoint, PT, CHECK_BORDER);
}

static void
insert_from_buffer_1 (struct buffer *buf,
		      ptrdiff_t from, ptrdiff_t nchars, bool inherit)
{
  ptrdiff_t chunk, chunk_expanded;
  ptrdiff_t from_byte = buf_charpos_to_bytepos (buf, from);
  ptrdiff_t to_byte = buf_charpos_to_bytepos (buf, from + nchars);
  ptrdiff_t incoming_nbytes = to_byte - from_byte;
  ptrdiff_t outgoing_nbytes = incoming_nbytes;
  INTERVAL intervals;

  if (nchars == 0)
    return;

  /* Make OUTGOING_NBYTES describe the text
     as it will be inserted in this buffer.  */

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    outgoing_nbytes = nchars;
  else if (NILP (BVAR (buf, enable_multibyte_characters)))
    {
      ptrdiff_t outgoing_before_gap = 0;
      ptrdiff_t outgoing_after_gap = 0;

      if (from < BUF_GPT (buf))
	{
	  chunk =  BUF_GPT_BYTE (buf) - from_byte;
	  if (chunk > incoming_nbytes)
	    chunk = incoming_nbytes;
	  outgoing_before_gap
	    = count_size_as_multibyte (BUF_BYTE_ADDRESS (buf, from_byte),
				       chunk);
	}
      else
	chunk = 0;

      if (chunk < incoming_nbytes)
	outgoing_after_gap
	  = count_size_as_multibyte (BUF_BYTE_ADDRESS (buf,
						       from_byte + chunk),
				     incoming_nbytes - chunk);

      outgoing_nbytes = outgoing_before_gap + outgoing_after_gap;
    }

  /* Do this before moving and increasing the gap,
     because the before-change hooks might move the gap
     or make it smaller.  */
  prepare_to_modify_buffer (PT, PT, NULL);

  if (PT != GPT)
    move_gap (PT, PT_BYTE);
  if (GAP_SIZE < outgoing_nbytes)
    make_gap (outgoing_nbytes - GAP_SIZE);

  if (from < BUF_GPT (buf))
    {
      chunk = BUF_GPT_BYTE (buf) - from_byte;
      if (chunk > incoming_nbytes)
	chunk = incoming_nbytes;
      /* Record number of output bytes, so we know where
	 to put the output from the second copy_text.  */
      chunk_expanded
	= copy_text (BUF_BYTE_ADDRESS (buf, from_byte),
		     GAP_BEG_ADDR, chunk,
		     !NILP (BVAR (buf, enable_multibyte_characters)),
		     !NILP (BVAR (current_buffer, enable_multibyte_characters)));
    }
  else
    chunk_expanded = chunk = 0;

  if (chunk < incoming_nbytes)
    copy_text (BUF_BYTE_ADDRESS (buf, from_byte + chunk),
	       GAP_BEG_ADDR + chunk_expanded, incoming_nbytes - chunk,
	       !NILP (BVAR (buf, enable_multibyte_characters)),
	       !NILP (BVAR (current_buffer, enable_multibyte_characters)));

#ifdef BYTE_COMBINING_DEBUG
  /* We have copied text into the gap, but we have not altered
     PT or PT_BYTE yet.  So we can pass PT and PT_BYTE
     to these functions and get the same results as we would
     have got earlier on.  Meanwhile, GAP_BEG_ADDR does point to
     the text that has been stored by copy_text.  */
  if (count_combining_before (GAP_BEG_ADDR, outgoing_nbytes, PT, PT_BYTE)
      || count_combining_after (GAP_BEG_ADDR, outgoing_nbytes, PT, PT_BYTE))
    emacs_abort ();
#endif

  undo_push_insert (PT, nchars);
  modiff_incr (&MODIFF);
  CHARS_MODIFF = MODIFF;

  GAP_SIZE -= outgoing_nbytes;
  GPT += nchars;
  ZV += nchars;
  Z += nchars;
  GPT_BYTE += outgoing_nbytes;
  ZV_BYTE += outgoing_nbytes;
  Z_BYTE += outgoing_nbytes;
  anchor_gap ();

  eassert (GPT <= GPT_BYTE);

  /* The insert may have been in the unchanged region, so check again.  */
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  if (NILP (BVAR (current_buffer, bidi_display_reordering))
      && !NILP (BVAR (buf, bidi_display_reordering)))
    bset_bidi_display_reordering (buf, Qt);

  adjust_markers_for_insert (PT, PT_BYTE, PT + nchars,
			     PT_BYTE + outgoing_nbytes,
			     false);

  offset_intervals (current_buffer, PT, nchars);

  /* Get the intervals for the part of the string we are inserting.  */
  intervals = buffer_intervals (buf);
  if (nchars < BUF_Z (buf) - BUF_BEG (buf))
    {
      if (buf == current_buffer && PT <= from)
	from += nchars;
      intervals = copy_intervals (intervals, from, nchars);
    }

  /* Insert those intervals.  */
  graft_intervals_into_buffer (intervals, PT, nchars, current_buffer, inherit);

#ifdef HAVE_TREE_SITTER
  tree_sitter_record_change (PT, PT, BUFFER_TO_SITTER (PT), PT + nchars);
#endif

  adjust_point (nchars, outgoing_nbytes);
}

/* Record undo information and adjust markers and position keepers for
   a replacement of a text PREV_TEXT at FROM to a new text of LEN
   chars (LEN_BYTE bytes) which resides in the gap just after
   GAP_BEG_ADDR.

   PREV_TEXT nil means the new text was just inserted.  */

static void
adjust_after_replace (ptrdiff_t from, ptrdiff_t from_byte,
		      Lisp_Object prev_text, ptrdiff_t len, ptrdiff_t len_byte)
{
  ptrdiff_t nchars_del = 0, nbytes_del = 0;
#ifdef HAVE_TREE_SITTER
  uint32_t old_end_byte;
#endif

#ifdef BYTE_COMBINING_DEBUG
  if (count_combining_before (GAP_BEG_ADDR, len_byte, from, from_byte)
      || count_combining_after (GAP_BEG_ADDR, len_byte, from, from_byte))
    emacs_abort ();
#endif

  if (STRINGP (prev_text))
    {
      nchars_del = SCHARS (prev_text);
      nbytes_del = SBYTES (prev_text);
    }

#ifdef HAVE_TREE_SITTER
  old_end_byte = BUFFER_TO_SITTER (from + nchars_del);
#endif

  /* Update various buffer positions for the new text.  */
  GAP_SIZE -= len_byte;
  ZV += len; Z += len;
  ZV_BYTE += len_byte; Z_BYTE += len_byte;
  GPT += len; GPT_BYTE += len_byte;
  anchor_gap ();

  if (nchars_del > 0)
    adjust_markers_for_replace (from, from_byte, nchars_del, nbytes_del,
				len, len_byte);
  else
    adjust_markers_for_insert (from, from_byte,
			       from + len, from_byte + len_byte, false);

  if (nchars_del > 0)
    undo_push_delete (from, prev_text, false);
  undo_push_insert (from, len);

  offset_intervals (current_buffer, from, len - nchars_del);

#ifdef HAVE_TREE_SITTER
  tree_sitter_record_change (from, from + nchars_del, old_end_byte, from + len);
#endif
  if (from < PT)
    adjust_point (len - nchars_del, len_byte - nbytes_del);

  /* As byte combining will decrease Z, we must check this again.  */
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  check_markers ();

  modiff_incr (&MODIFF);
  CHARS_MODIFF = MODIFF;
}

/* Record undo information, adjust markers and position keepers for an
   insertion of a text from FROM (FROM_BYTE) to TO (TO_BYTE).  The
   text already exists in the current buffer but character length (TO
   - FROM) may be incorrect, the correct length is NEWLEN.  */

void
adjust_after_insert (ptrdiff_t from, ptrdiff_t from_byte,
		     ptrdiff_t to, ptrdiff_t to_byte, ptrdiff_t newlen)
{
  ptrdiff_t len = to - from, len_byte = to_byte - from_byte;

  if (GPT != to)
    move_gap (to, to_byte);
  GAP_SIZE += len_byte;
  GPT -= len; GPT_BYTE -= len_byte;
  ZV -= len; ZV_BYTE -= len_byte;
  Z -= len; Z_BYTE -= len_byte;
  adjust_after_replace (from, from_byte, Qnil, newlen, len_byte);
}

/* Replace the text from character positions FROM to TO with NEW,
   If PREPARE, call prepare_to_modify_buffer.
   If INHERIT, the newly inserted text should inherit text properties
   from the surrounding non-deleted text.
   If ADJUST_MATCH_DATA, then adjust the match data before calling
   signal_after_change.  */

/* Note that this does not yet handle markers quite right.
   Also it needs to record a single undo-entry that does a replacement
   rather than a separate delete and insert.
   That way, undo will also handle markers properly.

   But if MARKERS is 0, don't relocate markers.  */

void
replace_range (ptrdiff_t from, ptrdiff_t to, Lisp_Object new,
               bool prepare, bool inherit, bool markers,
               bool adjust_match_data, bool inhibit_mod_hooks)
{
  ptrdiff_t inschars = SCHARS (new);
  ptrdiff_t insbytes = SBYTES (new);
  ptrdiff_t from_byte, to_byte;
  ptrdiff_t nbytes_del, nchars_del;
  INTERVAL intervals;
  ptrdiff_t outgoing_insbytes = insbytes;
  Lisp_Object deletion;
#ifdef HAVE_TREE_SITTER
  uint32_t old_end_byte;
#endif

  check_markers ();

  deletion = Qnil;

  if (prepare)
    {
      ptrdiff_t range_length = to - from;
      prepare_to_modify_buffer (from, to, &from);
      to = from + range_length;
    }

  /* Make args be valid.  */
  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

#ifdef HAVE_TREE_SITTER
  old_end_byte = BUFFER_TO_SITTER (to);
#endif

  from_byte = CHAR_TO_BYTE (from);
  to_byte = CHAR_TO_BYTE (to);

  nchars_del = to - from;
  nbytes_del = to_byte - from_byte;

  if (nbytes_del <= 0 && insbytes == 0)
    return;

  /* Make OUTGOING_INSBYTES describe the text
     as it will be inserted in this buffer.  */

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    outgoing_insbytes = inschars;
  else if (!STRING_MULTIBYTE (new))
    outgoing_insbytes
      = count_size_as_multibyte (SDATA (new), insbytes);

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (from > GPT)
    move_gap (from, from_byte);
  if (to < GPT)
    move_gap (to, to_byte);

  /* Even if we don't record for undo, we must keep the original text
     because we may have to recover it because of inappropriate byte
     combining.  */
  if (!EQ (BVAR (current_buffer, undo_list), Qt))
    deletion = make_buffer_string_both (from, from_byte, to, to_byte, 1);

  GAP_SIZE += nbytes_del;
  ZV -= nchars_del;
  Z -= nchars_del;
  ZV_BYTE -= nbytes_del;
  Z_BYTE -= nbytes_del;
  GPT = from;
  GPT_BYTE = from_byte;
  anchor_gap ();

  eassert (GPT <= GPT_BYTE);

  if (GPT - BEG < BEG_UNCHANGED)
    BEG_UNCHANGED = GPT - BEG;
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  if (GAP_SIZE < outgoing_insbytes)
    make_gap (outgoing_insbytes - GAP_SIZE);

  /* Copy the string text into the buffer, perhaps converting
     between single-byte and multibyte.  */
  copy_text (SDATA (new), GAP_BEG_ADDR, insbytes,
	     STRING_MULTIBYTE (new),
	     !NILP (BVAR (current_buffer, enable_multibyte_characters)));

#ifdef BYTE_COMBINING_DEBUG
  /* We have copied text into the gap, but we have not marked
     it as part of the buffer.  So we can use the old FROM and FROM_BYTE
     here, for both the previous text and the following text.
     Meanwhile, GAP_BEG_ADDR does point to
     the text that has been stored by copy_text.  */
  if (count_combining_before (GAP_BEG_ADDR, outgoing_insbytes, from, from_byte)
      || count_combining_after (GAP_BEG_ADDR, outgoing_insbytes, from, from_byte))
    emacs_abort ();
#endif

  /* Record the insertion first, so that when we undo,
     the deletion will be undone first.  Thus, undo
     will insert before deleting, and thus will keep
     the markers before and after this text separate.  */
  if (!NILP (deletion))
    {
      undo_push_insert (from + SCHARS (deletion), inschars);
      undo_push_delete (from, deletion, false);
    }

  GAP_SIZE -= outgoing_insbytes;
  GPT += inschars;
  ZV += inschars;
  Z += inschars;
  GPT_BYTE += outgoing_insbytes;
  ZV_BYTE += outgoing_insbytes;
  Z_BYTE += outgoing_insbytes;
  anchor_gap ();

  eassert (GPT <= GPT_BYTE);

  /* Adjust markers for the deletion and the insertion.  */
  if (markers)
    adjust_markers_for_replace (from, from_byte, nchars_del, nbytes_del,
				inschars, outgoing_insbytes);
  else
    {
      /* The character positions of the markers remain intact, but we
	 still need to update their byte positions, because the
	 deleted and the inserted text might have multibyte sequences
	 which make the original byte positions of the markers
	 invalid.  */
      adjust_markers_bytepos (from, from_byte, from + inschars,
			      from_byte + outgoing_insbytes, true);
    }

  offset_intervals (current_buffer, from, inschars - nchars_del);

  /* Get the intervals for the part of the string we are inserting--
     not including the combined-before bytes.  */
  intervals = string_intervals (new);
  /* Insert those intervals.  */
  graft_intervals_into_buffer (intervals, from, inschars,
			       current_buffer, inherit);

#ifdef HAVE_TREE_SITTER
  tree_sitter_record_change (from, from + nchars_del, old_end_byte, from + inschars);
#endif

  /* Relocate point as if it were a marker.  */
  if (from < PT)
    adjust_point ((from + inschars - min (PT, to)),
		  (from_byte + outgoing_insbytes - min (PT_BYTE, to_byte)));

  check_markers ();

  modiff_incr (&MODIFF);
  CHARS_MODIFF = MODIFF;

  if (adjust_match_data)
    update_search_regs (from, to, from + SCHARS (new));

  if (!inhibit_mod_hooks)
    {
      signal_after_change (from, nchars_del, GPT - from);
      update_compositions (from, GPT, CHECK_BORDER);
    }
}

/* Replace the text from character positions FROM to TO with
   the text in INS of length INSCHARS.
   Keep the text properties that applied to the old characters
   (extending them to all the new chars if there are more new chars).

   Note that this does not yet handle markers quite right.

   If MARKERS, relocate markers.

   Unlike most functions at this level, never call
   prepare_to_modify_buffer and never call signal_after_change.  */

void
replace_range_2 (ptrdiff_t from, ptrdiff_t from_byte,
		 ptrdiff_t to, ptrdiff_t to_byte,
		 const char *ins, ptrdiff_t inschars, ptrdiff_t insbytes,
		 bool markers)
{
  ptrdiff_t nbytes_del, nchars_del;
#ifdef HAVE_TREE_SITTER
  uint32_t old_end_byte = BUFFER_TO_SITTER (to);
#endif

  check_markers ();

  nchars_del = to - from;
  nbytes_del = to_byte - from_byte;

  if (nbytes_del <= 0 && insbytes == 0)
    return;

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (from > GPT)
    move_gap (from, from_byte);
  if (to < GPT)
    move_gap (to, to_byte);

  GAP_SIZE += nbytes_del;
  ZV -= nchars_del;
  Z -= nchars_del;
  ZV_BYTE -= nbytes_del;
  Z_BYTE -= nbytes_del;
  GPT = from;
  GPT_BYTE = from_byte;
  anchor_gap ();

  eassert (GPT <= GPT_BYTE);

  if (GPT - BEG < BEG_UNCHANGED)
    BEG_UNCHANGED = GPT - BEG;
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  if (GAP_SIZE < insbytes)
    make_gap (insbytes - GAP_SIZE);

  /* Copy the replacement text into the buffer.  */
  memcpy (GAP_BEG_ADDR, ins, insbytes);

#ifdef BYTE_COMBINING_DEBUG
  /* We have copied text into the gap, but we have not marked it as
     part of the buffer.  So we can use the old FROM and FROM_BYTE
     here, for both the previous text and the following text.
     Meanwhile, GAP_BEG_ADDR does point to the text that has been
     stored by copy_text.  */
  if (count_combining_before (GAP_BEG_ADDR, insbytes, from, from_byte)
      || count_combining_after (GAP_BEG_ADDR, insbytes, from, from_byte))
    emacs_abort ();
#endif

  GAP_SIZE -= insbytes;
  GPT += inschars;
  ZV += inschars;
  Z += inschars;
  GPT_BYTE += insbytes;
  ZV_BYTE += insbytes;
  Z_BYTE += insbytes;
  anchor_gap ();

  eassert (GPT <= GPT_BYTE);

  /* Adjust markers for the deletion and the insertion.  */
  if (!(nchars_del == 1 && inschars == 1 && nbytes_del == insbytes))
    {
      if (markers)
	adjust_markers_for_replace (from, from_byte, nchars_del, nbytes_del,
				    inschars, insbytes);
      else
	{
	  /* The character positions of the markers remain intact, but
	     we still need to update their byte positions, because the
	     deleted and the inserted text might have multibyte
	     sequences which make the original byte positions of the
	     markers invalid.  */
	  adjust_markers_bytepos (from, from_byte, from + inschars,
				  from_byte + insbytes, true);
	}
    }

  offset_intervals (current_buffer, from, inschars - nchars_del);

  /* Relocate point as if it were a marker.  */
  if (from < PT && (nchars_del != inschars || nbytes_del != insbytes))
    {
      if (PT < to)
	/* PT was within the deleted text.  Move it to FROM.  */
	adjust_point (from - PT, from_byte - PT_BYTE);
      else
	adjust_point (inschars - nchars_del, insbytes - nbytes_del);
    }

#ifdef HAVE_TREE_SITTER
  tree_sitter_record_change (from, from + nchars_del, old_end_byte, from + inschars);
#endif

  check_markers ();

  modiff_incr (&MODIFF);
  CHARS_MODIFF = MODIFF;
}

/* Delete characters in current buffer
   from FROM up to (but not including) TO.
   If TO comes before FROM, we delete nothing.  */

void
del_range (ptrdiff_t from, ptrdiff_t to)
{
  del_range_1 (from, to, 1, 0);
}

struct safe_del_range_context
{
  /* From and to positions.  */
  ptrdiff_t from, to;
};

static Lisp_Object
safe_del_range_1 (void *ptr)
{
  struct safe_del_range_context *context;

  context = ptr;
  del_range (context->from, context->to);
  return Qnil;
}

static Lisp_Object
safe_del_range_2 (enum nonlocal_exit type, Lisp_Object value)
{
  return Qt;
}

/* Like del_range; however, catch all non-local exits.  Value is 0 if
   the buffer contents were really deleted.  Otherwise, it is 1.  */

int
safe_del_range (ptrdiff_t from, ptrdiff_t to)
{
  struct safe_del_range_context context;

  context.from = from;
  context.to = to;

  return !NILP (internal_catch_all (safe_del_range_1,
				    &context,
				    safe_del_range_2));
}

/* Like del_range; PREPARE says whether to call prepare_to_modify_buffer.
   RET_STRING says to return the deleted text. */

Lisp_Object
del_range_1 (ptrdiff_t from, ptrdiff_t to, bool prepare, bool ret_string)
{
  ptrdiff_t from_byte, to_byte;
  Lisp_Object deletion;

  /* Make args be valid */
  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

  if (to <= from)
    return Qnil;

  if (prepare)
    {
      ptrdiff_t range_length = to - from;
      prepare_to_modify_buffer (from, to, &from);
      to = min (ZV, from + range_length);
    }

  from_byte = CHAR_TO_BYTE (from);
  to_byte = CHAR_TO_BYTE (to);

  deletion = del_range_2 (from, from_byte, to, to_byte, ret_string);
  signal_after_change (from, to - from, 0);
  update_compositions (from, from, CHECK_HEAD);
  return deletion;
}

/* Like del_range_1 but args are byte positions, not char positions.  */

void
del_range_byte (ptrdiff_t from_byte, ptrdiff_t to_byte)
{
  ptrdiff_t from, to;

  /* Make args be valid.  */
  if (from_byte < BEGV_BYTE)
    from_byte = BEGV_BYTE;
  if (to_byte > ZV_BYTE)
    to_byte = ZV_BYTE;

  if (to_byte <= from_byte)
    return;

  from = BYTE_TO_CHAR (from_byte);
  to = BYTE_TO_CHAR (to_byte);

  {
    ptrdiff_t old_from = from, old_to = Z - to;
    ptrdiff_t range_length = to - from;
    prepare_to_modify_buffer (from, to, &from);
    to = from + range_length;

    if (old_from != from)
      from_byte = CHAR_TO_BYTE (from);
    if (to > ZV)
      {
	to = ZV;
	to_byte = ZV_BYTE;
      }
    else if (old_to == Z - to)
      to_byte = CHAR_TO_BYTE (to);
  }

  del_range_2 (from, from_byte, to, to_byte, 0);
  signal_after_change (from, to - from, 0);
  update_compositions (from, from, CHECK_HEAD);
}

/* Like del_range_1, but positions are specified both as charpos
   and bytepos.  */

void
del_range_both (ptrdiff_t from, ptrdiff_t from_byte,
		ptrdiff_t to, ptrdiff_t to_byte, bool prepare)
{
  /* Make args be valid */
  if (from_byte < BEGV_BYTE)
    from_byte = BEGV_BYTE;
  if (to_byte > ZV_BYTE)
    to_byte = ZV_BYTE;

  if (to_byte <= from_byte)
    return;

  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

  if (prepare)
    {
      ptrdiff_t old_from = from, old_to = Z - to;
      ptrdiff_t range_length = to - from;
      prepare_to_modify_buffer (from, to, &from);
      to = from + range_length;

      if (old_from != from)
	from_byte = CHAR_TO_BYTE (from);
      if (to > ZV)
	{
	  to = ZV;
	  to_byte = ZV_BYTE;
	}
      else if (old_to == Z - to)
	to_byte = CHAR_TO_BYTE (to);
    }

  del_range_2 (from, from_byte, to, to_byte, 0);
  signal_after_change (from, to - from, 0);
  update_compositions (from, from, CHECK_HEAD);
}

/* Delete a range of text, specified both as character positions
   and byte positions.  FROM and TO are character positions,
   while FROM_BYTE and TO_BYTE are byte positions.
   If RET_STRING, the deleted area is returned as a string.  */

Lisp_Object
del_range_2 (ptrdiff_t from, ptrdiff_t from_byte,
	     ptrdiff_t to, ptrdiff_t to_byte, bool ret_string)
{
  ptrdiff_t nbytes_del, nchars_del;
  Lisp_Object deletion;
#ifdef HAVE_TREE_SITTER
  uint32_t old_end_byte = BUFFER_TO_SITTER (to);
#endif

  check_markers ();

  nchars_del = to - from;
  nbytes_del = to_byte - from_byte;

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (from > GPT)
    move_gap (from, from_byte);
  if (to < GPT)
    move_gap (to, to_byte);

#ifdef BYTE_COMBINING_DEBUG
  if (count_combining_before (BUF_BYTE_ADDRESS (current_buffer, to_byte),
			      Z_BYTE - to_byte, from, from_byte))
    emacs_abort ();
#endif

  if (ret_string || !EQ (BVAR (current_buffer, undo_list), Qt))
    deletion = make_buffer_string_both (from, from_byte, to, to_byte, 1);
  else
    deletion = Qnil;

  /* Record marker adjustments, and text deletion into undo
     history.  */
  undo_push_delete (from, deletion, true);

  /* Relocate all markers pointing into the new, larger gap to point
     at the end of the text before the gap.  */
  adjust_markers_for_delete (from, from_byte, to, to_byte);

  modiff_incr (&MODIFF);
  CHARS_MODIFF = MODIFF;

  /* Relocate point as if it were a marker.  */
  if (from < PT)
    adjust_point (from - min (PT, to),
		  from_byte - min (PT_BYTE, to_byte));

  offset_intervals (current_buffer, from, - nchars_del);

  GAP_SIZE += nbytes_del;
  ZV -= nchars_del;
  Z -= nchars_del;
  ZV_BYTE -= nbytes_del;
  Z_BYTE -= nbytes_del;
  GPT = from;
  GPT_BYTE = from_byte;
  /* Anchor unless decode_coding_object needs to access the original
     gap contents.  */
  if (!current_buffer->text->inhibit_shrinking)
    anchor_gap ();
  eassert (GPT <= GPT_BYTE);

  if (GPT - BEG < BEG_UNCHANGED)
    BEG_UNCHANGED = GPT - BEG;
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  check_markers ();

#ifdef HAVE_TREE_SITTER
  tree_sitter_record_change (from, from + nchars_del, old_end_byte, from);
#endif

  return deletion;
}

/* Call this if you're about to change the text of current buffer
   from character positions START to END.  This checks the read-only
   properties of the region, calls the necessary modification hooks,
   and warns the next redisplay that it should pay attention to that
   area.  */

void
modify_text (ptrdiff_t start, ptrdiff_t end)
{
  prepare_to_modify_buffer (start, end, NULL);

  BUF_COMPUTE_UNCHANGED (current_buffer, start - 1, end);
  undo_push_maiden ();
  modiff_incr (&MODIFF);
  CHARS_MODIFF = MODIFF;

  bset_point_before_scroll (current_buffer, Qnil);
}

/* Check that it is okay to modify the buffer between START and END,
   which are char positions.

   Run the before-change-functions, if any.  If intervals are in use,
   verify that the text to be modified is not read-only, and call
   any modification properties the text may have.

   If PRESERVE_PTR is nonzero, we relocate *PRESERVE_PTR
   by holding its value temporarily in a marker.

   Avoid calling this function while manipulating the gap since its
   execution of arbitrary lisp could GC and compact the buffer.
*/

void
prepare_to_modify_buffer_1 (ptrdiff_t start, ptrdiff_t end,
			    ptrdiff_t *preserve_ptr)
{
  struct buffer *base_buffer;
  Lisp_Object temp;

  XSETFASTINT (temp, start);
  if (!NILP (BVAR (current_buffer, read_only)))
    Fbarf_if_buffer_read_only (temp);

  /* If we're about to modify a buffer the contents of which come from
     a dump file, copy the contents to private storage first so we
     don't take a COW fault on the buffer text and keep it around
     forever.  */
  if (pdumper_address_p (BEG_ADDR))
    enlarge_buffer_text (current_buffer, 0);
  eassert (!pdumper_address_p (BEG_ADDR));

  bset_redisplay (current_buffer);

  if (buffer_intervals (current_buffer))
    {
      if (preserve_ptr)
	{
	  Lisp_Object preserve_marker;
	  preserve_marker = Fcopy_marker (make_fixnum (*preserve_ptr), Qnil);
	  verify_interval_modification (current_buffer, start, end);
	  *preserve_ptr = marker_position (preserve_marker);
	  unchain_marker (XMARKER (preserve_marker));
	}
      else
	verify_interval_modification (current_buffer, start, end);
    }

  if (inhibit_modification_hooks)
    return;

  /* For indirect buffers, use the base buffer.  */
  base_buffer = current_buffer->base_buffer
    ? current_buffer->base_buffer
    : current_buffer;

  if (!NILP (BVAR (base_buffer, file_truename))
      /* Make binding buffer-file-name to nil effective.  */
      && !NILP (BVAR (base_buffer, filename))
      && SAVE_MODIFF >= MODIFF)
    Flock_file (BVAR (base_buffer, file_truename));

  if (!NILP (BVAR (current_buffer, mark_active))
      && XMARKER (BVAR (current_buffer, mark))->buffer
      && NILP (Vsaved_region_selection)
      && (EQ (Vselect_active_regions, Qonly)
	  ? EQ (CAR_SAFE (Vtransient_mark_mode), Qonly)
	  : (!NILP (Vselect_active_regions)
	     && !NILP (Vtransient_mark_mode))))
    /* If `select-active-regions' is non-nil, save the region text.  */
    Vsaved_region_selection
      = call1 (Vregion_extract_function, Qnil);

  signal_before_change (start, end, preserve_ptr);
  Fset (Qdeactivate_mark, Qt);
}

/* Like above, but called when we know that the buffer text
   will be modified and region caches should be invalidated.  */

void
prepare_to_modify_buffer (ptrdiff_t start, ptrdiff_t end,
			  ptrdiff_t *preserve_ptr)
{
  prepare_to_modify_buffer_1 (start, end, preserve_ptr);
  invalidate_buffer_caches (current_buffer, start, end);
}

/* Invalidate the caches maintained by the buffer BUF, if any, for the
   region between buffer positions START and END.  */

void
invalidate_buffer_caches (struct buffer *buf, ptrdiff_t start, ptrdiff_t end)
{
  /* Indirect buffers usually have their caches set to NULL, but we
     need to consider the caches of their base buffer.  */
  if (buf->base_buffer)
    buf = buf->base_buffer;
  /* The bidi_paragraph_cache must be invalidated first, because doing
     so might need to use the newline_cache (via find_newline_no_quit,
     see below).  */
  if (buf->bidi_paragraph_cache)
    {
      if (start > BUF_BEG (buf))
	{
	  /* If we are deleting or replacing characters, we could
	     create a paragraph start, because all of the characters
	     from START to the beginning of START's line are
	     whitespace.  Therefore, we must extend the region to be
	     invalidated up to the newline before START.  Similarly,
	     if we are inserting characters immediately after a
	     newline, we could create a paragraph start if the
	     inserted characters start with a newline.  */
	  ptrdiff_t line_beg = start;
	  ptrdiff_t start_byte = buf_charpos_to_bytepos (buf, start);
	  int prev_char = BUF_FETCH_BYTE (buf, start_byte - 1);

	  if ((start == end) == (prev_char == '\n'))
	    {
	      struct buffer *old = current_buffer;

	      set_buffer_internal (buf);

	      line_beg = find_newline_no_quit (start, start_byte, -1,
					       &start_byte);
	      set_buffer_internal (old);
	    }
	  start = line_beg - (line_beg > BUF_BEG (buf));
	}
      invalidate_region_cache (buf,
			       buf->bidi_paragraph_cache,
			       start - BUF_BEG (buf), BUF_Z (buf) - end);
    }
  if (buf->newline_cache)
    invalidate_region_cache (buf,
                             buf->newline_cache,
                             start - BUF_BEG (buf), BUF_Z (buf) - end);
  if (buf->width_run_cache)
    invalidate_region_cache (buf,
                             buf->width_run_cache,
                             start - BUF_BEG (buf), BUF_Z (buf) - end);
}

/* These macros work with an argument named `preserve_ptr'
   and a local variable named `preserve_marker'.  */

#define PRESERVE_VALUE							\
  if (preserve_ptr && NILP (preserve_marker))				\
    preserve_marker = Fcopy_marker (make_fixnum (*preserve_ptr), Qnil)

#define RESTORE_VALUE						\
  if (!NILP (preserve_marker))					\
    {								\
      *preserve_ptr = marker_position (preserve_marker);	\
      unchain_marker (XMARKER (preserve_marker));		\
    }

#define PRESERVE_START_END			\
  if (NILP (start_marker))			\
    start_marker = Fcopy_marker (start, Qnil);	\
  if (NILP (end_marker))			\
    end_marker = Fcopy_marker (end, Qnil);

#define FETCH_START				\
  (!NILP (start_marker) ? Fmarker_position (start_marker) : start)

#define FETCH_END				\
  (!NILP (end_marker) ? Fmarker_position (end_marker) : end)

/* Set a variable to nil if an error occurred.
   Don't change the variable if there was no error.
   VAL is a cons-cell (VARIABLE . NO-ERROR-FLAG).
   VARIABLE is the variable to maybe set to nil.
   NO-ERROR-FLAG is nil if there was an error,
   anything else meaning no error (so this function does nothing).  */
struct rvoe_arg
{
  Lisp_Object *location;
  bool errorp;
};

static void
reset_var_on_error (void *ptr)
{
  struct rvoe_arg *p = ptr;
  if (p->errorp)
    *p->location = Qnil;
}

/* Signal a change to the buffer immediately before it happens.
   START_INT and END_INT are the bounds of the text to be changed.

   If PRESERVE_PTR is nonzero, we relocate *PRESERVE_PTR
   by holding its value temporarily in a marker.  */

static void
signal_before_change (ptrdiff_t start_int, ptrdiff_t end_int,
		      ptrdiff_t *preserve_ptr)
{
  Lisp_Object start, end;
  Lisp_Object start_marker, end_marker;
  Lisp_Object preserve_marker;
  specpdl_ref count = SPECPDL_INDEX ();
  struct rvoe_arg rvoe_arg;

  start = make_fixnum (start_int);
  end = make_fixnum (end_int);
  preserve_marker = Qnil;
  start_marker = Qnil;
  end_marker = Qnil;

  specbind (Qinhibit_modification_hooks, Qt);

  /* If buffer is unmodified, run a special hook for that case.  The
     check for Vfirst_change_hook is just a minor optimization.  */
  if (SAVE_MODIFF >= MODIFF
      && !NILP (Vfirst_change_hook))
    {
      PRESERVE_VALUE;
      PRESERVE_START_END;
      run_hook (Qfirst_change_hook);
    }

  /* Now run the before-change-functions if any.  */
  if (!NILP (Vbefore_change_functions))
    {
      rvoe_arg.location = &Vbefore_change_functions;
      rvoe_arg.errorp = 1;

      PRESERVE_VALUE;
      PRESERVE_START_END;

      /* Mark before-change-functions to be reset to nil in case of error.  */
      record_unwind_protect_ptr (reset_var_on_error, &rvoe_arg);

      /* Actually run the hook functions.  */
      CALLN (Frun_hook_with_args, Qbefore_change_functions,
	     FETCH_START, FETCH_END);

      /* There was no error: unarm the reset_on_error.  */
      rvoe_arg.errorp = 0;
    }

  if (buffer_has_overlays ())
    {
      PRESERVE_VALUE;
      report_overlay_modification (FETCH_START, FETCH_END, 0,
				   FETCH_START, FETCH_END, Qnil);
    }

  if (!NILP (start_marker))
    detach_marker (start_marker);
  if (!NILP (end_marker))
    detach_marker (end_marker);
  RESTORE_VALUE;

  unbind_to (count, Qnil);
}

/* Signal a change immediately after it happens.  CHARPOS is the
   position at which LENDEL characters were deleted and LENINS characters
   inserted.  */

void
signal_after_change (ptrdiff_t charpos, ptrdiff_t lendel, ptrdiff_t lenins)
{
  specpdl_ref count = SPECPDL_INDEX ();
  struct rvoe_arg rvoe_arg;
  Lisp_Object save_insert_behind_hooks, save_insert_in_from_hooks;

  if (inhibit_modification_hooks)
    return;

  /* Save and restore the insert-*-hooks, because after-change-functions
     could clobber them when manipulating text properties.  */
  save_insert_behind_hooks = interval_insert_behind_hooks;
  save_insert_in_from_hooks = interval_insert_in_front_hooks;

  specbind (Qinhibit_modification_hooks, Qt);

  if (!NILP (Vafter_change_functions))
    {
      rvoe_arg.location = &Vafter_change_functions;
      rvoe_arg.errorp = 1;

      /* Mark after-change-functions to be reset to nil in case of error.  */
      record_unwind_protect_ptr (reset_var_on_error, &rvoe_arg);

      /* Actually run the hook functions.  */
      CALLN (Frun_hook_with_args, Qafter_change_functions,
	     make_fixnum (charpos), make_fixnum (charpos + lenins),
	     make_fixnum (lendel));

      /* There was no error: unarm the reset_on_error.  */
      rvoe_arg.errorp = 0;
    }

  interval_insert_behind_hooks = save_insert_behind_hooks;
  interval_insert_in_front_hooks = save_insert_in_from_hooks;

  if (buffer_has_overlays ())
    report_overlay_modification (make_fixnum (charpos),
				 make_fixnum (charpos + lenins),
				 1,
				 make_fixnum (charpos),
				 make_fixnum (charpos + lenins),
				 make_fixnum (lendel));

  /* After an insertion, call the text properties
     insert-behind-hooks or insert-in-front-hooks.  */
  if (lendel == 0)
    report_interval_modification (make_fixnum (charpos),
				  make_fixnum (charpos + lenins));

  unbind_to (count, Qnil);
}

void
syms_of_insdel (void)
{
  DEFVAR_BOOL ("inhibit-modification-hooks", inhibit_modification_hooks,
	       doc: /* Non-nil means don't run any of the hooks that respond to buffer changes.
This affects `before-change-functions' and `after-change-functions',
as well as hooks attached to text properties and overlays.
Setting this variable non-nil also inhibits file locks and checks
whether files are locked by another Emacs session, as well as
handling of the active region per `select-active-regions'.

See also the info node `(elisp) Change Hooks'.  */);
  inhibit_modification_hooks = false;
  DEFSYM (Qinhibit_modification_hooks, "inhibit-modification-hooks");
}
