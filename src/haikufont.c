/* Font support for Haiku windowing

Copyright (C) 2021 Free Software Foundation, Inc.

This file is part of GNU Emacs.

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
#include "dispextern.h"
#include "composite.h"
#include "blockinput.h"
#include "charset.h"
#include "frame.h"
#include "window.h"
#include "fontset.h"
#include "haikuterm.h"
#include "character.h"
#include "font.h"
#include "termchar.h"
#include "pdumper.h"
#include "haiku_support.h"

#include <math.h>
#include <stdlib.h>

static Lisp_Object font_cache;

#define METRICS_NCOLS_PER_ROW	(128)

enum metrics_status
  {
    METRICS_INVALID = -1,    /* metrics entry is invalid */
  };

#define METRICS_STATUS(metrics)	((metrics)->ascent + (metrics)->descent)
#define METRICS_SET_STATUS(metrics, status) \
  ((metrics)->ascent = 0, (metrics)->descent = (status))

static struct
{
  /* registry name */
  const char *name;
  /* characters to distinguish the charset from the others */
  int uniquifier[6];
  /* additional constraint by language */
  const char *lang;
} em_charset_table[] =
  { { "iso8859-1", { 0x00A0, 0x00A1, 0x00B4, 0x00BC, 0x00D0 } },
    { "iso8859-2", { 0x00A0, 0x010E }},
    { "iso8859-3", { 0x00A0, 0x0108 }},
    { "iso8859-4", { 0x00A0, 0x00AF, 0x0128, 0x0156, 0x02C7 }},
    { "iso8859-5", { 0x00A0, 0x0401 }},
    { "iso8859-6", { 0x00A0, 0x060C }},
    { "iso8859-7", { 0x00A0, 0x0384 }},
    { "iso8859-8", { 0x00A0, 0x05D0 }},
    { "iso8859-9", { 0x00A0, 0x00A1, 0x00BC, 0x011E }},
    { "iso8859-10", { 0x00A0, 0x00D0, 0x0128, 0x2015 }},
    { "iso8859-11", { 0x00A0, 0x0E01 }},
    { "iso8859-13", { 0x00A0, 0x201C }},
    { "iso8859-14", { 0x00A0, 0x0174 }},
    { "iso8859-15", { 0x00A0, 0x00A1, 0x00D0, 0x0152 }},
    { "iso8859-16", { 0x00A0, 0x0218}},
    { "gb2312.1980-0", { 0x4E13 }, "zh-cn"},
    { "big5-0", { 0x9C21 }, "zh-tw" },
    { "jisx0208.1983-0", { 0x4E55 }, "ja"},
    { "ksc5601.1985-0", { 0xAC00 }, "ko"},
    { "cns11643.1992-1", { 0xFE32 }, "zh-tw"},
    { "cns11643.1992-2", { 0x4E33, 0x7934 }},
    { "cns11643.1992-3", { 0x201A9 }},
    { "cns11643.1992-4", { 0x20057 }},
    { "cns11643.1992-5", { 0x20000 }},
    { "cns11643.1992-6", { 0x20003 }},
    { "cns11643.1992-7", { 0x20055 }},
    { "gbk-0", { 0x4E06 }, "zh-cn"},
    { "jisx0212.1990-0", { 0x4E44 }},
    { "jisx0213.2000-1", { 0xFA10 }, "ja"},
    { "jisx0213.2000-2", { 0xFA49 }},
    { "jisx0213.2004-1", { 0x20B9F }},
    { "viscii1.1-1", { 0x1EA0, 0x1EAE, 0x1ED2 }, "vi"},
    { "tis620.2529-1", { 0x0E01 }, "th"},
    { "microsoft-cp1251", { 0x0401, 0x0490 }, "ru"},
    { "koi8-r", { 0x0401, 0x2219 }, "ru"},
    { "mulelao-1", { 0x0E81 }, "lo"},
    { "unicode-sip", { 0x20000 }},
    { "mulearabic-0", { 0x628 }},
    { "mulearabic-1", { 0x628 }},
    { "mulearabic-2", { 0x628 }},
    { NULL }
  };

static void
haikufont_apply_registry (struct haiku_font_pattern *pattern,
			  Lisp_Object registry)
{
  char *str = SSDATA (SYMBOL_NAME (registry));
  USE_SAFE_ALLOCA;
  char *re = SAFE_ALLOCA (SBYTES (SYMBOL_NAME (registry)) * 2 + 1);
  int i, j;

  for (i = j = 0; i < SBYTES (SYMBOL_NAME (registry)); i++, j++)
    {
      if (str[i] == '.')
	re[j++] = '\\';
      else if (str[i] == '*')
	re[j++] = '.';
      re[j] = str[i];
      if (re[j] == '?')
	re[j] = '.';
    }
  re[j] = '\0';
  AUTO_STRING_WITH_LEN (regexp, re, j);
  for (i = 0; em_charset_table[i].name; i++)
    if (fast_c_string_match_ignore_case
	(regexp, em_charset_table[i].name,
	 strlen (em_charset_table[i].name)) >= 0)
      break;
  SAFE_FREE ();
  if (!em_charset_table[i].name)
    return;
  int *uniquifier = em_charset_table[i].uniquifier;
  int l;

  for (l = 0; uniquifier[l]; ++l);

  uint32_t *a = xmalloc (l * sizeof *a);
  for (l = 0; uniquifier[l]; ++l)
    a[l] = uniquifier[l];

  if (pattern->specified & FSPEC_WANTED)
    {
      int old_l = l;
      l += pattern->want_chars_len;
      a = xrealloc (a, l * sizeof *a);
      memcpy (&a[old_l], pattern->wanted_chars, (l - old_l) * sizeof *a);
      xfree (pattern->wanted_chars);
    }
  pattern->specified |= FSPEC_WANTED;
  pattern->want_chars_len = l;
  pattern->wanted_chars = a;

  if (em_charset_table[i].lang)
    {
      if (!strncmp (em_charset_table[i].lang, "zh", 2))
	{
	  pattern->specified |= FSPEC_LANGUAGE;
	  pattern->language = LANGUAGE_CN;
	}
      else if (!strncmp (em_charset_table[i].lang, "ko", 2))
	{
	  pattern->specified |= FSPEC_LANGUAGE;
	  pattern->language = LANGUAGE_KO;
	}
      else if (!strncmp (em_charset_table[i].lang, "ja", 2))
	{
	  pattern->specified |= FSPEC_LANGUAGE;
	  pattern->language = LANGUAGE_JP;
	}
    }

  return;
}

static Lisp_Object
haikufont_get_fallback_entity (void)
{
  Lisp_Object ent = font_make_entity ();
  ASET (ent, FONT_TYPE_INDEX, Qhaiku);
  ASET (ent, FONT_FOUNDRY_INDEX, Qhaiku);
  ASET (ent, FONT_FAMILY_INDEX, Qnil);
  ASET (ent, FONT_ADSTYLE_INDEX, Qnil);
  ASET (ent, FONT_REGISTRY_INDEX, Qutf_8);
  ASET (ent, FONT_SIZE_INDEX, make_fixnum (0));
  ASET (ent, FONT_AVGWIDTH_INDEX, make_fixnum (0));
  ASET (ent, FONT_SPACING_INDEX, make_fixnum (FONT_SPACING_MONO));
  FONT_SET_STYLE (ent, FONT_WIDTH_INDEX, Qnil);
  FONT_SET_STYLE (ent, FONT_WEIGHT_INDEX, Qnil);
  FONT_SET_STYLE (ent, FONT_SLANT_INDEX, Qnil);

  return ent;
}

static Lisp_Object
haikufont_get_cache (struct frame *frame)
{
  return font_cache;
}

static Lisp_Object
haikufont_weight_to_lisp (int weight)
{
  switch (weight)
    {
    case HAIKU_THIN:
      return Qthin;
    case HAIKU_ULTRALIGHT:
      return Qultra_light;
    case HAIKU_EXTRALIGHT:
      return Qextra_light;
    case HAIKU_LIGHT:
      return Qlight;
    case HAIKU_SEMI_LIGHT:
      return Qsemi_light;
    case HAIKU_REGULAR:
      return Qnormal;
    case HAIKU_SEMI_BOLD:
      return Qsemi_bold;
    case HAIKU_BOLD:
      return Qbold;
    case HAIKU_EXTRA_BOLD:
      return Qextra_bold;
    case HAIKU_ULTRA_BOLD:
      return Qultra_bold;
    case HAIKU_BOOK:
      return Qbook;
    case HAIKU_HEAVY:
      return Qheavy;
    case HAIKU_ULTRA_HEAVY:
      return Qultra_heavy;
    case HAIKU_BLACK:
      return Qblack;
    case HAIKU_MEDIUM:
      return Qmedium;
    }
  emacs_abort ();
}

static int
haikufont_lisp_to_weight (Lisp_Object weight)
{
  if (EQ (weight, Qthin))
    return HAIKU_THIN;
  if (EQ (weight, Qultra_light))
    return HAIKU_ULTRALIGHT;
  if (EQ (weight, Qextra_light))
    return HAIKU_EXTRALIGHT;
  if (EQ (weight, Qlight))
    return HAIKU_LIGHT;
  if (EQ (weight, Qsemi_light))
    return HAIKU_SEMI_LIGHT;
  if (EQ (weight, Qnormal))
    return HAIKU_REGULAR;
  if (EQ (weight, Qsemi_bold))
    return HAIKU_SEMI_BOLD;
  if (EQ (weight, Qbold))
    return HAIKU_BOLD;
  if (EQ (weight, Qextra_bold))
    return HAIKU_EXTRA_BOLD;
  if (EQ (weight, Qultra_bold))
    return HAIKU_ULTRA_BOLD;
  if (EQ (weight, Qbook))
    return HAIKU_BOOK;
  if (EQ (weight, Qheavy))
    return HAIKU_HEAVY;
  if (EQ (weight, Qultra_heavy))
    return HAIKU_ULTRA_HEAVY;
  if (EQ (weight, Qblack))
    return HAIKU_BLACK;
  if (EQ (weight, Qmedium))
    return HAIKU_MEDIUM;

  emacs_abort ();
}

static Lisp_Object
haikufont_slant_to_lisp (enum haiku_font_slant slant)
{
  switch (slant)
    {
    case NO_SLANT:
      emacs_abort ();
    case SLANT_ITALIC:
      return Qitalic;
    case SLANT_REGULAR:
      return Qnormal;
    case SLANT_OBLIQUE:
      return Qoblique;
    }
  emacs_abort ();
}

static enum haiku_font_slant
haikufont_lisp_to_slant (Lisp_Object slant)
{
  if (EQ (slant, Qitalic) ||
      EQ (slant, Qreverse_italic))
    return SLANT_ITALIC;
  if (EQ (slant, Qoblique) ||
      EQ (slant, Qreverse_oblique))
    return SLANT_OBLIQUE;
  if (EQ (slant, Qnormal))
    return SLANT_REGULAR;
  emacs_abort ();
}

static Lisp_Object
haikufont_width_to_lisp (enum haiku_font_width width)
{
  switch (width)
    {
    case NO_WIDTH:
      emacs_abort ();
    case ULTRA_CONDENSED:
      return Qultra_condensed;
    case EXTRA_CONDENSED:
      return Qextra_condensed;
    case CONDENSED:
      return Qcondensed;
    case SEMI_CONDENSED:
      return Qsemi_condensed;
    case NORMAL_WIDTH:
      return Qnormal;
    case SEMI_EXPANDED:
      return Qsemi_expanded;
    case EXPANDED:
      return Qexpanded;
    case EXTRA_EXPANDED:
      return Qextra_expanded;
    case ULTRA_EXPANDED:
      return Qultra_expanded;
    }

  emacs_abort ();
}

static enum haiku_font_width
haikufont_lisp_to_width (Lisp_Object lisp)
{
  if (EQ (lisp, Qultra_condensed))
    return ULTRA_CONDENSED;
  if (EQ (lisp, Qextra_condensed))
    return EXTRA_CONDENSED;
  if (EQ (lisp, Qcondensed))
    return CONDENSED;
  if (EQ (lisp, Qsemi_condensed))
    return SEMI_CONDENSED;
  if (EQ (lisp, Qnormal))
    return NORMAL_WIDTH;
  if (EQ (lisp, Qexpanded))
    return EXPANDED;
  if (EQ (lisp, Qextra_expanded))
    return EXTRA_EXPANDED;
  if (EQ (lisp, Qultra_expanded))
    return ULTRA_EXPANDED;
  emacs_abort ();
}

static int
haikufont_maybe_handle_special_family (Lisp_Object family,
				       struct haiku_font_pattern *ptn)
{
  CHECK_SYMBOL (family);

  if (EQ (family, Qmonospace) || EQ (family, Qfixed) ||
      EQ (family, Qdefault))
    {
      BFont_populate_fixed_family (ptn);
      return 1;
    }
  else if (EQ (family, intern ("Sans Serif")))
    {
      BFont_populate_plain_family (ptn);
      return 1;
    }
  return 0;
}

static Lisp_Object
haikufont_pattern_to_entity (struct haiku_font_pattern *ptn)
{
  Lisp_Object ent = font_make_entity ();
  ASET (ent, FONT_TYPE_INDEX, Qhaiku);
  ASET (ent, FONT_FOUNDRY_INDEX, Qhaiku);
  ASET (ent, FONT_FAMILY_INDEX, Qdefault);
  ASET (ent, FONT_ADSTYLE_INDEX, Qnil);
  ASET (ent, FONT_REGISTRY_INDEX, Qutf_8);
  ASET (ent, FONT_SIZE_INDEX, make_fixnum (0));
  ASET (ent, FONT_AVGWIDTH_INDEX, make_fixnum (0));
  ASET (ent, FONT_SPACING_INDEX, make_fixnum (FONT_SPACING_MONO));
  FONT_SET_STYLE (ent, FONT_WIDTH_INDEX, Qnormal);
  FONT_SET_STYLE (ent, FONT_WEIGHT_INDEX, Qnormal);
  FONT_SET_STYLE (ent, FONT_SLANT_INDEX, Qnormal);

  if (ptn->specified & FSPEC_FAMILY)
    ASET (ent, FONT_FAMILY_INDEX, intern (ptn->family));
  else
    ASET (ent, FONT_FAMILY_INDEX, Qdefault);

  if (ptn->specified & FSPEC_STYLE)
    ASET (ent, FONT_ADSTYLE_INDEX, intern (ptn->style));
  else
    {
      if (ptn->specified & FSPEC_WEIGHT)
	FONT_SET_STYLE (ent, FONT_WEIGHT_INDEX,
			haikufont_weight_to_lisp (ptn->weight));
      if (ptn->specified & FSPEC_SLANT)
	FONT_SET_STYLE (ent, FONT_SLANT_INDEX,
			haikufont_slant_to_lisp (ptn->slant));
      if (ptn->specified & FSPEC_WIDTH)
	FONT_SET_STYLE (ent, FONT_WIDTH_INDEX,
			haikufont_width_to_lisp (ptn->width));
    }

  if (ptn->specified & FSPEC_SPACING)
    ASET (ent, FONT_SPACING_INDEX,
	  make_fixnum (ptn->mono_spacing_p ?
		       FONT_SPACING_MONO : FONT_SPACING_PROPORTIONAL));
  return ent;
}

static void
haikufont_spec_or_entity_to_pattern (Lisp_Object ent,
				     int list_p,
				     struct haiku_font_pattern *ptn)
{
  Lisp_Object tem;
  ptn->specified = 0;

  tem = AREF (ent, FONT_ADSTYLE_INDEX);
  if (!NILP (tem))
    {
      ptn->specified |= FSPEC_STYLE;
      strncpy ((char *) &ptn->style,
	       SSDATA (SYMBOL_NAME (tem)),
	       sizeof ptn->style - 1);
    }

  tem = FONT_SLANT_SYMBOLIC (ent);
  if (!NILP (tem))
    {
      ptn->specified |= FSPEC_SLANT;
      ptn->slant = haikufont_lisp_to_slant (tem);
    }

  tem = FONT_WEIGHT_SYMBOLIC (ent);
  if (!NILP (tem))
    {
      ptn->specified |= FSPEC_WEIGHT;
      ptn->weight = haikufont_lisp_to_weight (tem);
    }

  tem = FONT_WIDTH_SYMBOLIC (ent);
  if (!NILP (tem))
    {
      ptn->specified |= FSPEC_WIDTH;
      ptn->width = haikufont_lisp_to_width (tem);
    }

  tem = AREF (ent, FONT_SPACING_INDEX);
  if (FIXNUMP (tem))
    {
      ptn->specified |= FSPEC_SPACING;
      ptn->mono_spacing_p = XFIXNUM (tem) != FONT_SPACING_PROPORTIONAL;
    }

  tem = AREF (ent, FONT_FAMILY_INDEX);
  if (!NILP (tem) &&
      (list_p && !haikufont_maybe_handle_special_family (tem, ptn)))
    {
      ptn->specified |= FSPEC_FAMILY;
      strncpy ((char *) &ptn->family,
	       SSDATA (SYMBOL_NAME (tem)),
	       sizeof ptn->family - 1);
    }

  tem = assq_no_quit (QCscript, AREF (ent, FONT_EXTRA_INDEX));
  if (!NILP (tem))
    {
      tem = assq_no_quit (XCDR (tem), Vscript_representative_chars);

      if (CONSP (tem) && VECTORP (XCDR (tem)))
	{
	  tem = XCDR (tem);

	  int count = 0;

	  for (int j = 0; j < ASIZE (tem); ++j)
	    if (TYPE_RANGED_FIXNUMP (uint32_t, AREF (tem, j)))
	      ++count;

	  if (count)
	    {
	      ptn->specified |= FSPEC_NEED_ONE_OF;
	      ptn->need_one_of_len = count;
	      ptn->need_one_of = xmalloc (count * sizeof *ptn->need_one_of);
	      count = 0;
	      for (int j = 0; j < ASIZE (tem); ++j)
		if (TYPE_RANGED_FIXNUMP (uint32_t, AREF (tem, j)))
		  {
		    ptn->need_one_of[j] = XFIXNAT (AREF (tem, j));
		    ++count;
		  }
	    }
	}
      else if (CONSP (tem) && CONSP (XCDR (tem)))
	{
	  int count = 0;

	  for (Lisp_Object it = XCDR (tem); CONSP (it); it = XCDR (it))
	    if (TYPE_RANGED_FIXNUMP (uint32_t, XCAR (it)))
	      ++count;

	  if (count)
	    {
	      ptn->specified |= FSPEC_WANTED;
	      ptn->want_chars_len = count;
	      ptn->wanted_chars = xmalloc (count * sizeof *ptn->wanted_chars);
	      count = 0;

	      for (tem = XCDR (tem); CONSP (tem); tem = XCDR (tem))
		if (TYPE_RANGED_FIXNUMP (uint32_t, XCAR (tem)))
		  {
		    ptn->wanted_chars[count] = XFIXNAT (XCAR (tem));
		    ++count;
		  }
	    }
	}
    }

  tem = assq_no_quit (QClang, AREF (ent, FONT_EXTRA_INDEX));
  if (CONSP (tem))
    {
      tem = XCDR (tem);
      if (EQ (tem, Qzh))
	{
	  ptn->specified |= FSPEC_LANGUAGE;
	  ptn->language = LANGUAGE_CN;
	}
      else if (EQ (tem, Qko))
	{
	  ptn->specified |= FSPEC_LANGUAGE;
	  ptn->language = LANGUAGE_KO;
	}
      else if (EQ (tem, Qjp))
	{
	  ptn->specified |= FSPEC_LANGUAGE;
	  ptn->language = LANGUAGE_JP;
	}
    }

  tem = AREF (ent, FONT_REGISTRY_INDEX);
  if (SYMBOLP (tem))
    haikufont_apply_registry (ptn, tem);
}

static void
haikufont_done_with_query_pattern (struct haiku_font_pattern *ptn)
{
  if (ptn->specified & FSPEC_WANTED)
    xfree (ptn->wanted_chars);

  if (ptn->specified & FSPEC_NEED_ONE_OF)
    xfree (ptn->need_one_of);
}

static Lisp_Object
haikufont_match (struct frame *f, Lisp_Object font_spec)
{
  block_input ();
  Lisp_Object tem = Qnil;
  struct haiku_font_pattern ptn;
  haikufont_spec_or_entity_to_pattern (font_spec, 0, &ptn);
  ptn.specified &= ~FSPEC_FAMILY;
  struct haiku_font_pattern *found = BFont_find (&ptn);
  haikufont_done_with_query_pattern (&ptn);
  if (found)
    {
      tem = haikufont_pattern_to_entity (found);
      haiku_font_pattern_free (found);
    }
  unblock_input ();
  return !NILP (tem) ? tem : haikufont_get_fallback_entity ();
}

static Lisp_Object
haikufont_list (struct frame *f, Lisp_Object font_spec)
{
  block_input ();
  Lisp_Object lst = Qnil;

  /* Returning irrelevant results on receiving an OTF form will cause
     fontset.c to loop over and over, making displaying some
     characters very slow.  */
  Lisp_Object tem = assq_no_quit (QCotf, AREF (font_spec, FONT_EXTRA_INDEX));
  if (CONSP (tem) && !NILP (XCDR (tem)))
    {
      unblock_input ();
      return Qnil;
    }

  struct haiku_font_pattern ptn;
  haikufont_spec_or_entity_to_pattern (font_spec, 1, &ptn);
  struct haiku_font_pattern *found = BFont_find (&ptn);
  haikufont_done_with_query_pattern (&ptn);
  if (found)
    {
      for (struct haiku_font_pattern *pt = found;
	   pt; pt = pt->next)
	lst = Fcons (haikufont_pattern_to_entity (pt), lst);
      haiku_font_pattern_free (found);
    }
  unblock_input ();
  return lst;
}

static void
haiku_bulk_encode (struct haikufont_info *font_info, int block)
{
  unsigned short *unichars = xmalloc (0x101 * sizeof (*unichars));
  unsigned int i, idx;

  block_input ();

  font_info->glyphs[block] = unichars;
  if (!unichars)
    emacs_abort ();

  for (idx = block << 8, i = 0; i < 0x100; idx++, i++)
    unichars[i] = idx;
  unichars[0x100] = 0;


  /* If the font contains the entire block, just store it.  */
  if (!BFont_have_char_block (font_info->be_font,
			      unichars[0], unichars[0xff]))
    {
      for (int i = 0; i < 0x100; ++i)
	if (!BFont_have_char_p (font_info->be_font, unichars[i]))
	  unichars[i] = 0xFFFF;
    }

  unblock_input ();
}

static unsigned int
haikufont_encode_char (struct font *font, int c)
{
  struct haikufont_info *font_info = (struct haikufont_info *) font;
  unsigned char high = (c & 0xff00) >> 8, low = c & 0x00ff;
  unsigned short g;

  if (c > 0xFFFF)
    return FONT_INVALID_CODE;

  if (!font_info->glyphs[high])
    haiku_bulk_encode (font_info, high);
  g = font_info->glyphs[high][low];
  return g == 0xFFFF ? FONT_INVALID_CODE : g;
}

static Lisp_Object
haikufont_open (struct frame *f, Lisp_Object font_entity, int x)
{
  struct haikufont_info *font_info;
  struct haiku_font_pattern ptn;
  struct font *font;
  void *be_font;
  Lisp_Object font_object;
  Lisp_Object tem;

  block_input ();
  if (x <= 0)
    {
      /* Get pixel size from frame instead.  */
      tem = get_frame_param (f, Qfontsize);
      x = NILP (tem) ? 0 : XFIXNAT (tem);
    }

  haikufont_spec_or_entity_to_pattern (font_entity, 1, &ptn);

  if (BFont_open_pattern (&ptn, &be_font, x))
    {
      haikufont_done_with_query_pattern (&ptn);
      unblock_input ();
      return Qnil;
    }

  haikufont_done_with_query_pattern (&ptn);

  font_object = font_make_object (VECSIZE (struct haikufont_info),
				  font_entity, x);

  ASET (font_object, FONT_TYPE_INDEX, Qhaiku);
  font_info = (struct haikufont_info *) XFONT_OBJECT (font_object);
  font = (struct font *) font_info;

  if (!font)
    {
      unblock_input ();
      return Qnil;
    }

  font_info->be_font = be_font;
  font_info->glyphs = xzalloc (0x100 * sizeof *font_info->glyphs);

  font->pixel_size = 0;
  font->driver = &haikufont_driver;
  font->encoding_charset = -1;
  font->repertory_charset = -1;
  font->default_ascent = 0;
  font->vertical_centering = 0;
  font->baseline_offset = 0;
  font->relative_compose = 0;

  font_info->metrics = NULL;
  font_info->metrics_nrows = 0;

  int px_size, min_width, max_width,
    avg_width, height, space_width, ascent,
    descent, underline_pos, underline_thickness;

  BFont_dat (be_font, &px_size, &min_width,
	     &max_width, &avg_width, &height,
	     &space_width, &ascent, &descent,
	     &underline_pos, &underline_thickness);

  font->pixel_size = px_size;
  font->min_width = min_width;
  font->max_width = max_width;
  font->average_width = avg_width;
  font->height = height;
  font->space_width = space_width;
  font->ascent = ascent;
  font->descent = descent;
  font->default_ascent = ascent;
  font->underline_position = underline_pos;
  font->underline_thickness = underline_thickness;

  font->vertical_centering = 0;
  font->baseline_offset = 0;
  font->relative_compose = 0;

  font->props[FONT_NAME_INDEX] = Ffont_xlfd_name (font_object, Qnil);

  unblock_input ();
  return font_object;
}

static void
haikufont_close (struct font *font)
{
  if (font_data_structures_may_be_ill_formed ())
    return;
  struct haikufont_info *info = (struct haikufont_info *) font;

  block_input ();
  if (info && info->be_font)
    BFont_close (info->be_font);

  for (int i = 0; i < info->metrics_nrows; i++)
    if (info->metrics[i])
      xfree (info->metrics[i]);
  if (info->metrics)
    xfree (info->metrics);
  for (int i = 0; i < 0x100; ++i)
    if (info->glyphs[i])
      xfree (info->glyphs[i]);
  xfree (info->glyphs);
  unblock_input ();
}

static void
haikufont_prepare_face (struct frame *f, struct face *face)
{

}

static void
haikufont_glyph_extents (struct font *font, unsigned code,
			 struct font_metrics *metrics)
{
  struct haikufont_info *info = (struct haikufont_info *) font;

  struct font_metrics *cache;
  int row, col;

  row = code / METRICS_NCOLS_PER_ROW;
  col = code % METRICS_NCOLS_PER_ROW;
  if (row >= info->metrics_nrows)
    {
      info->metrics =
	xrealloc (info->metrics,
		  sizeof (struct font_metrics *) * (row + 1));
      memset (info->metrics + info->metrics_nrows, 0,
	      (sizeof (struct font_metrics *)
	       * (row + 1 - info->metrics_nrows)));
      info->metrics_nrows = row + 1;
    }

  if (info->metrics[row] == NULL)
    {
      struct font_metrics *new;
      int i;

      new = xmalloc (sizeof (struct font_metrics) * METRICS_NCOLS_PER_ROW);
      for (i = 0; i < METRICS_NCOLS_PER_ROW; i++)
	METRICS_SET_STATUS (new + i, METRICS_INVALID);
      info->metrics[row] = new;
    }
  cache = info->metrics[row] + col;

  if (METRICS_STATUS (cache) == METRICS_INVALID)
    {
      unsigned char utf8[MAX_MULTIBYTE_LENGTH];
      memset (utf8, 0, MAX_MULTIBYTE_LENGTH);
      CHAR_STRING (code, utf8);
      int advance, lb, rb;
      BFont_char_bounds (info->be_font, (const char *) utf8, &advance, &lb, &rb);

      cache->lbearing = lb;
      cache->rbearing = rb;
      cache->width = advance;
      cache->ascent = font->ascent;
      cache->descent = font->descent;
    }

  if (metrics)
    *metrics = *cache;
}

static void
haikufont_text_extents (struct font *font, const unsigned int *code,
			int nglyphs, struct font_metrics *metrics)
{
  int totalwidth = 0;
  memset (metrics, 0, sizeof (struct font_metrics));

  block_input ();
  for (int i = 0; i < nglyphs; i++)
    {
      struct font_metrics m;
      haikufont_glyph_extents (font, code[i], &m);
      if (metrics)
	{
	  if (totalwidth + m.lbearing < metrics->lbearing)
	    metrics->lbearing = totalwidth + m.lbearing;
	  if (totalwidth + m.rbearing > metrics->rbearing)
	    metrics->rbearing = totalwidth + m.rbearing;
	  if (m.ascent > metrics->ascent)
	    metrics->ascent = m.ascent;
	  if (m.descent > metrics->descent)
	    metrics->descent = m.descent;
	}
      totalwidth += m.width;
    }

  unblock_input ();

  if (metrics)
    metrics->width = totalwidth;
}

static Lisp_Object
haikufont_shape (Lisp_Object lgstring, Lisp_Object direction)
{
  struct haikufont_info *font =
    (struct haikufont_info *) CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring));
  int *advance, *lb, *rb;
  ptrdiff_t glyph_len, len, i, b_len;
  Lisp_Object tem;
  char *b;
  uint32_t *mb_buf;

  glyph_len = LGSTRING_GLYPH_LEN (lgstring);
  for (i = 0; i < glyph_len; ++i)
    {
      tem = LGSTRING_GLYPH (lgstring, i);

      if (NILP (tem))
	break;
    }

  len = i;

  if (INT_MAX / 2 < len)
    memory_full (SIZE_MAX);

  block_input ();

  b_len = 0;
  b = xmalloc (b_len);
  mb_buf = alloca (len * sizeof *mb_buf);

  for (i = b_len; i < len; ++i)
    {
      uint32_t c = LGLYPH_CHAR (LGSTRING_GLYPH (lgstring, i));
      mb_buf[i] = c;
      unsigned char mb[MAX_MULTIBYTE_LENGTH];
      int slen = CHAR_STRING (c, mb);

      b = xrealloc (b, b_len = (b_len + slen));
      if (len == 1)
	b[b_len - slen] = mb[0];
      else
	memcpy (b + b_len - slen, mb, slen);
    }

  advance = alloca (len * sizeof *advance);
  lb = alloca (len * sizeof *lb);
  rb = alloca (len * sizeof *rb);

  eassert (font->be_font);
  BFont_nchar_bounds (font->be_font, b, advance, lb, rb, len);
  xfree (b);

  for (i = 0; i < len; ++i)
    {
      tem = LGSTRING_GLYPH (lgstring, i);
      if (NILP (tem))
	{
	  tem = LGLYPH_NEW ();
	  LGSTRING_SET_GLYPH (lgstring, i, tem);
	}

      LGLYPH_SET_FROM (tem, i);
      LGLYPH_SET_TO (tem, i);
      LGLYPH_SET_CHAR (tem, mb_buf[i]);
      LGLYPH_SET_CODE (tem, mb_buf[i]);

      LGLYPH_SET_WIDTH (tem, advance[i]);
      LGLYPH_SET_LBEARING (tem, lb[i]);
      LGLYPH_SET_RBEARING (tem, rb[i]);
      LGLYPH_SET_ASCENT (tem, font->font.ascent);
      LGLYPH_SET_DESCENT (tem, font->font.descent);
    }

  unblock_input ();

  return make_fixnum (len);
}

static int
haikufont_draw (struct glyph_string *s, int from, int to,
		int x, int y, bool with_background)
{
  struct frame *f = s->f;
  struct face *face = s->face;
  struct font_info *info = (struct font_info *) s->font;
  unsigned char mb[MAX_MULTIBYTE_LENGTH];
  void *view = FRAME_HAIKU_VIEW (f);

  block_input ();
  prepare_face_for_display (s->f, face);

  BView_draw_lock (view);
  BView_StartClip (view);
  if (with_background)
    {
      int height = FONT_HEIGHT (s->font), ascent = FONT_BASE (s->font);

      /* Font's global height and ascent values might be
	 preposterously large for some fonts.  We fix here the case
	 when those fonts are used for display of glyphless
	 characters, because drawing background with font dimensions
	 in those cases makes the display illegible.  There's only one
	 more call to the draw method with with_background set to
	 true, and that's in x_draw_glyph_string_foreground, when
	 drawing the cursor, where we have no such heuristics
	 available.  FIXME.  */
      if (s->first_glyph->type == GLYPHLESS_GLYPH
	  && (s->first_glyph->u.glyphless.method == GLYPHLESS_DISPLAY_HEX_CODE
	      || s->first_glyph->u.glyphless.method == GLYPHLESS_DISPLAY_ACRONYM))
	height = ascent =
	  s->first_glyph->slice.glyphless.lower_yoff
	  - s->first_glyph->slice.glyphless.upper_yoff;

      BView_SetHighColor (view, s->hl == DRAW_CURSOR ?
			  FRAME_CURSOR_COLOR (s->f).pixel : face->background);

      BView_FillRectangle (view, x, y - ascent, s->width, height);
      s->background_filled_p = 1;
    }

  if (s->left_overhang && s->clip_head && !s->for_overlaps)
    {
      /* XXX: Why is this neccessary? */
      BView_ClipToRect (view, s->clip_head->x, 0,
			FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f));
    }

  if (s->hl == DRAW_CURSOR)
    BView_SetHighColor (view, FRAME_OUTPUT_DATA (s->f)->cursor_fg);
  else
    BView_SetHighColor (view, face->foreground);

  BView_MovePenTo (view, x, y);
  BView_SetFont (view, ((struct haikufont_info *) info)->be_font);

  if (from == to)
    {
      int len = CHAR_STRING (s->char2b[from], mb);
      BView_DrawString (view, (char *) mb, len);
    }
  else
    {
      ptrdiff_t b_len = 0;
      char *b = xmalloc (b_len);

      for (int idx = from; idx < to; ++idx)
	{
	  int len = CHAR_STRING (s->char2b[idx], mb);
	  b = xrealloc (b, b_len = (b_len + len));
	  if (len == 1)
	    b[b_len - len] = mb[0];
	  else
	    memcpy (b + b_len - len, mb, len);
	}

      BView_DrawString (view, b, b_len);
      xfree (b);
    }
  BView_EndClip (view);
  BView_draw_unlock (view);
  unblock_input ();
  return 1;
}

struct font_driver const haikufont_driver =
  {
    .type = LISPSYM_INITIALLY (Qhaiku),
    .case_sensitive = true,
    .get_cache = haikufont_get_cache,
    .list = haikufont_list,
    .match = haikufont_match,
    .draw = haikufont_draw,
    .open_font = haikufont_open,
    .close_font = haikufont_close,
    .prepare_face = haikufont_prepare_face,
    .encode_char = haikufont_encode_char,
    .text_extents = haikufont_text_extents,
    .shape = haikufont_shape
  };

void
syms_of_haikufont (void)
{
  DEFSYM (Qfontsize, "fontsize");
  DEFSYM (Qfixed, "fixed");
  DEFSYM (Qplain, "plain");
  DEFSYM (Qultra_light, "ultra-light");
  DEFSYM (Qthin, "thin");
  DEFSYM (Qreverse_italic, "reverse-italic");
  DEFSYM (Qreverse_oblique, "reverse-oblique");
  DEFSYM (Qmonospace, "monospace");
  DEFSYM (Qultra_condensed, "ultra-condensed");
  DEFSYM (Qextra_condensed, "extra-condensed");
  DEFSYM (Qcondensed, "condensed");
  DEFSYM (Qsemi_condensed, "semi-condensed");
  DEFSYM (Qsemi_expanded, "semi-expanded");
  DEFSYM (Qexpanded, "expanded");
  DEFSYM (Qextra_expanded, "extra-expanded");
  DEFSYM (Qultra_expanded, "ultra-expanded");
  DEFSYM (Qzh, "zh");
  DEFSYM (Qko, "ko");
  DEFSYM (Qjp, "jp");

  font_cache = list (Qnil);
  staticpro (&font_cache);
}
