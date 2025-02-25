/* Caching facts about regions of the buffer, for optimization.

Copyright (C) 1985-1989, 1993, 1995, 2001-2024 Free Software Foundation,
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
#include <stdio.h>

#include "lisp.h"
#include "buffer.h"
#include "region-cache.h"

/* Record character positions, so-called boundaries, where known-ness
   changes.

   To facilitate inserting boundaries, the cache maintains a gap, just
   like text buffers.

   To help boundary positions float along with insertions and deletions,
   positions before the gap are relative to BUF_BEG (thus >= 0), and
   those after the gap are relative to BUF_Z (thus <= 0).  See
   revalidate_region_cache to see how this helps.  */

struct boundary {
  ptrdiff_t offs;
  int value;
};

struct region_cache {
  /* Sorted charpos of inflection points for value.  */
  struct boundary *boundaries;

  /* boundaries[gap_start .. gap_start + gap_len - 1] is the gap.  */
  ptrdiff_t gap_start, gap_len;

  /* Number of non-gap boundaries.  */
  ptrdiff_t cache_len;

  /* Number of characters at head and tail value to be unchanged since
     last invalidation.  They overlap for unmodified buffers. */
  ptrdiff_t skip_head, skip_tail;

  /* The values of BUF_BEG and BUF_Z when we last revalidated.  Yes,
     buffer_beg is always going to be 1.  */
  ptrdiff_t buffer_beg, buffer_end;
};

/* Return the position of boundary i in cache c.  */
#define BOUNDARY_POS(c, i) \
  ((i) < (c)->gap_start \
   ? (c)->buffer_beg + (c)->boundaries[(i)].offs \
   : (c)->buffer_end + (c)->boundaries[(c)->gap_len + (i)].offs)

/* Return the value for text after boundary i in cache c.  */
#define BOUNDARY_VALUE(c, i) \
  ((i) < (c)->gap_start \
   ? (c)->boundaries[(i)].value \
   : (c)->boundaries[(c)->gap_len + (i)].value)

/* Set the value for text after boundary i in cache c to v.  */
#define SET_BOUNDARY_VALUE(c, i, v) \
  ((i) < (c)->gap_start \
   ? ((c)->boundaries[(i)].value = (v))\
   : ((c)->boundaries[(c)->gap_len + (i)].value = (v)))


/* How many elements to add to the gap when we resize the buffer.  */
#define NEW_CACHE_GAP (40)

static void revalidate_region_cache (struct buffer *buf, struct region_cache *c);


/* Interface: Allocating, initializing, and disposing of region caches.  */

struct region_cache *
new_region_cache (void)
{
  struct region_cache *c = xmalloc (sizeof *c);

  c->gap_start = 0;
  c->gap_len = NEW_CACHE_GAP;
  c->cache_len = 0;
  c->boundaries = xmalloc ((c->gap_len + c->cache_len)
			   * sizeof (*c->boundaries));

  c->skip_head = 0;
  c->skip_tail = 0;
  c->buffer_beg = BEG;
  c->buffer_end = BEG;

  /* Insert the boundary for the buffer start.  */
  c->cache_len++;
  c->gap_len--;
  c->gap_start++;
  c->boundaries[0].offs   = 0;  /* from buffer_beg */
  c->boundaries[0].value = 0;

  return c;
}

void
free_region_cache (struct region_cache *c)
{
  xfree (c->boundaries);
  xfree (c);
}


/* Finding positions in the cache.  */

/* Return the index of the last boundary in cache C at or before POS.
   In other words, return the boundary that specifies the value for
   the region POS..(POS + 1).

   This operation should be logarithmic in the number of cache
   entries.  It would be nice if it took advantage of locality of
   reference, too, by searching entries near the last entry found.  */
static ptrdiff_t
find_cache_boundary (struct region_cache *c, ptrdiff_t pos)
{
  ptrdiff_t low = 0, high = c->cache_len;

  while (low + 1 < high)
    {
      /* mid is always a valid index, because low < high and ">> 1"
         rounds down.  */
      ptrdiff_t mid = (low >> 1) + (high >> 1) + (low & high & 1);
      ptrdiff_t boundary = BOUNDARY_POS (c, mid);

      if (pos < boundary)
        high = mid;
      else
        low = mid;
    }

  /* Some testing.  */
  eassert (!(BOUNDARY_POS (c, low) > pos
	     || (low + 1 < c->cache_len
		 && BOUNDARY_POS (c, low + 1) <= pos)));

  return low;
}

/* Move the gap of cache C to index POS, and make sure it has space
   for at least MIN_SIZE boundaries.  */
static void
move_cache_gap (struct region_cache *c, ptrdiff_t pos, ptrdiff_t min_size)
{
  /* Copy these out of the cache and into registers.  */
  ptrdiff_t gap_start = c->gap_start;
  ptrdiff_t gap_len = c->gap_len;
  ptrdiff_t buffer_beg = c->buffer_beg;
  ptrdiff_t buffer_end = c->buffer_end;

  /* We mustn't ever try to put the gap before the dummy start
     boundary.  That must always be start-relative.  */
  eassert (0 < pos && pos <= c->cache_len);

  /* Need we move the gap right?  */
  while (gap_start < pos)
    {
      /* Copy one boundary from after to before the gap, and
         convert its position to start-relative.  */
      c->boundaries[gap_start].offs
        = (buffer_end
           + c->boundaries[gap_start + gap_len].offs
           - buffer_beg);
      c->boundaries[gap_start].value
        = c->boundaries[gap_start + gap_len].value;
      gap_start++;
    }

  /* To enlarge the gap, we need to re-allocate the boundary array, and
     then shift the area after the gap to the new end.  Since the cost
     is proportional to the amount of stuff after the gap, we do the
     enlargement here, after a right shift but before a left shift,
     when the portion after the gap is smallest.  */
  if (gap_len < min_size)
    {
      ptrdiff_t i, nboundaries = c->cache_len;

      c->boundaries =
	xpalloc (c->boundaries, &nboundaries, min_size - gap_len, -1,
		 sizeof *c->boundaries);

      /* Some systems don't provide a version of the copy routine that
         can be trusted to shift memory upward into an overlapping
         region.  memmove isn't widely available.  */
      min_size = nboundaries - c->cache_len - gap_len;
      for (i = c->cache_len - 1; i >= gap_start; i--)
        {
          c->boundaries[i + min_size].offs   = c->boundaries[i + gap_len].offs;
          c->boundaries[i + min_size].value = c->boundaries[i + gap_len].value;
        }

      gap_len = min_size;
    }

  /* Need we move the gap left?  */
  while (pos < gap_start)
    {
      gap_start--;

      /* Copy one region from before to after the gap, and
         convert its position to end-relative.  */
      c->boundaries[gap_start + gap_len].offs
        = c->boundaries[gap_start].offs + buffer_beg - buffer_end;
      c->boundaries[gap_start + gap_len].value
        = c->boundaries[gap_start].value;
    }

  /* Assign these back into the cache.  */
  c->gap_start = gap_start;
  c->gap_len = gap_len;
}


/* Insert a new boundary in cache C; it will have cache index I,
   and have the specified POS and VALUE.  */
static void
insert_cache_boundary (struct region_cache *c, ptrdiff_t i, ptrdiff_t pos,
		       int value)
{
  /* I must be a valid cache index, and we must never want
     to insert something before the dummy first boundary.  */
  eassert (0 < i && i <= c->cache_len);

  /* We must only be inserting things in order.  */
  eassert ((BOUNDARY_POS (c, i - 1) < pos
	    && (i == c->cache_len
		|| pos < BOUNDARY_POS (c, i))));

  /* The value must be different from the ones around it.  However, we
     temporarily create boundaries that establish the same value as
     the subsequent boundary, so we're not going to flag that case.  */
  eassert (BOUNDARY_VALUE (c, i - 1) != value);

  move_cache_gap (c, i, 1);

  c->boundaries[i].offs = pos - c->buffer_beg;
  c->boundaries[i].value = value;
  c->gap_start++;
  c->gap_len--;
  c->cache_len++;
}


/* Delete the i'th entry from cache C if START <= i < END.  */

static void
delete_cache_boundaries (struct region_cache *c,
			 ptrdiff_t start, ptrdiff_t end)
{
  ptrdiff_t len = end - start;

  /* Gotta be in range.  */
  eassert (0 <= start && end <= c->cache_len);

  /* Gotta be in order.  */
  eassert (start <= end);

  /* Can't delete the dummy entry.  */
  eassert (!(start == 0 && end >= 1));

  /* Minimize gap motion.  If we're deleting nothing, do nothing.  */
  if (len == 0)
    ;
  /* If the gap is before the region to delete, delete from the start
     forward.  */
  else if (c->gap_start <= start)
    {
      move_cache_gap (c, start, 0);
      c->gap_len += len;
    }
  /* If the gap is after the region to delete, delete from the end
     backward.  */
  else if (end <= c->gap_start)
    {
      move_cache_gap (c, end, 0);
      c->gap_start -= len;
      c->gap_len   += len;
    }
  /* If the gap is in the region to delete, just expand it.  */
  else
    {
      c->gap_start = start;
      c->gap_len   += len;
    }

  c->cache_len -= len;
}



/* Set the value for a region.  */

/* Set the value in cache C for the region START..END to VALUE.  */
static void
set_cache_region (struct region_cache *c,
		  ptrdiff_t start, ptrdiff_t end, int value)
{
  eassert (start <= end);
  eassert (c->buffer_beg <= start && end <= c->buffer_end);

  /* Eliminate this case; then we can assume that start and end-1 are
     both the locations of real characters in the buffer.  */
  if (start == end)
    return;

  {
    /* We need to make sure that there are no boundaries in the area
       between start to end; the whole area will have the same value,
       so those boundaries will not be necessary.

       Let start_ix be the cache index of the boundary governing the
       first character of start..end, and let end_ix be the cache
       index of the earliest boundary after the last character in
       start..end.  (This tortured terminology is intended to answer
       all the "< or <=?" sort of questions.)  */
    ptrdiff_t start_ix = find_cache_boundary (c, start);
    ptrdiff_t end_ix   = find_cache_boundary (c, end - 1) + 1;

    /* We must remember the value established by the last boundary
       before end; if that boundary's domain stretches beyond end,
       we'll need to create a new boundary at end, and that boundary
       must have that remembered value.  */
    int value_at_end = BOUNDARY_VALUE (c, end_ix - 1);

    /* Delete all boundaries strictly within start..end; this means
       those whose indices are between start_ix (exclusive) and end_ix
       (exclusive).  */
    delete_cache_boundaries (c, start_ix + 1, end_ix);

    /* Make sure we have the right value established going in to
       start..end from the left, and no unnecessary boundaries.  */
    if (BOUNDARY_POS (c, start_ix) == start)
      {
        /* Is this boundary necessary?  If no, remove it; if yes, set
           its value.  */
        if (start_ix > 0
            && BOUNDARY_VALUE (c, start_ix - 1) == value)
          {
            delete_cache_boundaries (c, start_ix, start_ix + 1);
            start_ix--;
          }
        else
          SET_BOUNDARY_VALUE (c, start_ix, value);
      }
    else
      {
        /* Do we need to add a new boundary here?  */
        if (BOUNDARY_VALUE (c, start_ix) != value)
          {
            insert_cache_boundary (c, start_ix + 1, start, value);
            start_ix++;
          }
      }

    /* This is equivalent to letting end_ix float (like a buffer
       marker does) with the insertions and deletions we may have
       done.  */
    end_ix = start_ix + 1;

    /* Make sure we have the correct value established as we leave
       start..end to the right.  */
    if (end == c->buffer_end)
      /* There is no text after start..end; nothing to do.  */
      ;
    else if (end_ix >= c->cache_len
             || end < BOUNDARY_POS (c, end_ix))
      {
        /* There is no boundary at end, but we may need one.  */
        if (value_at_end != value)
          insert_cache_boundary (c, end_ix, end, value_at_end);
      }
    else
      {
        /* There is a boundary at end; should it be there?  */
        if (value == BOUNDARY_VALUE (c, end_ix))
          delete_cache_boundaries (c, end_ix, end_ix + 1);
      }
  }
}

void
invalidate_region_cache (struct buffer *buf, struct region_cache *c,
			 ptrdiff_t nhead, ptrdiff_t ntail)
{
  /* b9c5136 horseshit mitigator elided.  */
  c->skip_head = min (nhead, c->skip_head);
  c->skip_tail = min (ntail, c->skip_tail);
}


/* If you bookend a buffer with single character insertions, the entire
   cache is invalidated.  If this function is called betwixt them, then
   the cache is preserved.

   Cost is log(n), so don't call willy-nilly. */

static void
revalidate_region_cache (struct buffer *buf, struct region_cache *c)
{
  const ptrdiff_t ch_beg = c->buffer_beg + c->skip_head;
  const ptrdiff_t unch_beg = c->buffer_end - c->skip_tail;

  if (ch_beg > unch_beg)
    {
      /* No changes.  */
      return;
    }
  else if (ch_beg == unch_beg)
    {
      /* Changed region is empty string.  Its boundary should precede
	 the gap.  */
      const ptrdiff_t b = find_cache_boundary (c, ch_beg);
      move_cache_gap (c, b + 1, 0);

      /* Now reflect latest endpoints (buffer_beg should remain 1).  */
      c->buffer_beg = BUF_BEG (buf);
      c->buffer_end = BUF_Z (buf);

      const ptrdiff_t new_ch_beg = c->buffer_beg + c->skip_head;
      const ptrdiff_t new_unch_beg = c->buffer_end - c->skip_tail;

      /* Invalidate the now non-empty changed region.  */
      set_cache_region (c, new_ch_beg, new_unch_beg, 0);
    }
  else
    {
      /* Changed region is non-empty.  Invalidate all the boundaries
	 there.  */
      set_cache_region (c, ch_beg, unch_beg, 0);

      /* Boundary of changed region should precede the gap.  */
      const ptrdiff_t b = find_cache_boundary (c, ch_beg);
      move_cache_gap (c, b + 1, 0);

      /* Now reflect latest endpoints (buffer_beg should remain 1).  */
      c->buffer_beg = BUF_BEG (buf);
      c->buffer_end = BUF_Z (buf);

      /* In the odd case gap straddlers become same.  Tag Heuer
	 makes good watches but can't explain shit (b9c5136).  */
      if (b + 1 < c->cache_len
          && BOUNDARY_POS (c, b) == BOUNDARY_POS (c, b + 1))
        {
          if (b > 0 && BOUNDARY_VALUE (c, b - 1) == BOUNDARY_VALUE (c, b + 1))
	    {
	      /* Remove.  */
	      delete_cache_boundaries (c, b, b + 2);
	    }
          else
            {
              /* Coalesce.  */
              SET_BOUNDARY_VALUE (c, b, BOUNDARY_VALUE (c, b + 1));
              delete_cache_boundaries (c, b + 1, b + 2);
            }
        }
    }

  /* Now the entire cache is valid.  */
  c->skip_head = c->skip_tail = c->buffer_end - c->buffer_beg;
}


/* Assert that the region of BUF between START and END (absolute
   buffer positions) is "value," for the purposes of CACHE (e.g. "has
   no newlines", in the case of the line cache).  */
void
know_region_cache (struct buffer *buf, struct region_cache *c,
		   ptrdiff_t start, ptrdiff_t end)
{
  revalidate_region_cache (buf, c);
  set_cache_region (c, start, end, 1);
}


/* Return the value for the text immediately after POS in BUF if the value
   is value, for the purposes of CACHE, and return zero otherwise.
   If NEXT is non-zero, set *NEXT to the nearest
   position after POS where the knowledge changes.  */
int
region_cache_forward (struct buffer *buf, struct region_cache *c,
		      ptrdiff_t pos, ptrdiff_t *next)
{
  revalidate_region_cache (buf, c);

  ptrdiff_t i = find_cache_boundary (c, pos);
  int i_value = BOUNDARY_VALUE (c, i);

  /* Beyond the end of the buffer is unknown, by definition.  */
  if (pos >= BUF_Z (buf))
    {
      if (next)
	*next = BUF_Z (buf);
      i_value = 0;
    }
  else if (next)
    {
      /* Scan forward from i to find the next differing position.  */
      *next = BUF_Z (buf);
      for (ptrdiff_t j = i + 1; j < c->cache_len; ++j)
	if (BOUNDARY_VALUE (c, j) != i_value)
	  {
	    *next = BOUNDARY_POS (c, j);
	    break;
	  }
    }

  return i_value;
}

/* Return the value for the text immediately before POS in BUF if the
   value is value, for the purposes of CACHE, and return zero
   otherwise.  If NEXT is non-zero, set *NEXT to the nearest
   position before POS where the knowledge changes.  */
int
region_cache_backward (struct buffer *buf, struct region_cache *c,
		       ptrdiff_t pos, ptrdiff_t *next)
{
  revalidate_region_cache (buf, c);

  /* Before the beginning of the buffer is unknown, by
     definition. */
  if (pos <= BUF_BEG (buf))
    {
      if (next) *next = BUF_BEG (buf);
      return 0;
    }

  {
    ptrdiff_t i = find_cache_boundary (c, pos - 1);
    int i_value = BOUNDARY_VALUE (c, i);
    ptrdiff_t j;

    if (next)
      {
        /* Scan backward from i to find the next differing position.  */
        for (j = i - 1; j >= 0; j--)
          if (BOUNDARY_VALUE (c, j) != i_value)
            break;

        if (j >= 0)
          *next = BOUNDARY_POS (c, j + 1);
        else
          *next = BUF_BEG (buf);
      }

    return i_value;
  }
}

#ifdef ENABLE_CHECKING

/* Debugging: pretty-print a cache to the standard error output.  */

void pp_cache (struct region_cache *) EXTERNALLY_VISIBLE;
void
pp_cache (struct region_cache *c)
{
  ptrdiff_t beg_u = c->buffer_beg + c->skip_head;
  ptrdiff_t end_u = c->buffer_end - c->skip_tail;

  fprintf (stderr,
           "basis: %"pD"d..%"pD"d    modified: %"pD"d..%"pD"d\n",
           c->buffer_beg, c->buffer_end,
           beg_u, end_u);

  for (ptrdiff_t i = 0; i < c->cache_len; i++)
    {
      ptrdiff_t pos = BOUNDARY_POS (c, i);

      fprintf (stderr, "%c%c%"pD"d : %d\n",
	       pos < beg_u ? 'v' : pos == beg_u ? '-' : ' ',
	       pos > end_u ? '^' : pos == end_u ? '-' : ' ',
	       pos, BOUNDARY_VALUE (c, i));
    }
}

#endif /* ENABLE_CHECKING */
