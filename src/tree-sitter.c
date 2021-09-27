/* Tree-sitter integration for GNU Emacs.

Copyright (C) 2021 Free Software Foundation, Inc.

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
#include "tree-sitter.h"

typedef TSLanguage *(*TSLanguageFunctor) (void);

static TSLanguageFunctor
tree_sitter_language_functor (Lisp_Object progmode)
{
  static Lisp_Object cache =
    make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE,
		     DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
		     Qnil, false);
  struct Lisp_Hash_Table *h = XHASH_TABLE (cache);
  Lisp_Object hash;
  ptrdiff_t i;

  CHECK_SYMBOL (progmode);

  i = hash_lookup (h, progmode, &hash);
  if (i < 0)
    {
      Lisp_Object language =
	Fcdr_safe (Fassq (progmode, Fsymbol_value (Qtree_sitter_mode_alist)));
      if (NILP (language))
	{
	  xsignal2 (Qtree_sitter_language_error,
		    progmode, concat2 (build_string ("No entry in "),
				       Fsymbol_name (Qtree_sitter_mode_alist)));
	}
      else
	{
	  dynlib_handle_ptr handle =
	    dynlib_open (SSDATA (concat3 (build_pure_c_string("bin/"),
					  language,
					  Vmodule_file_suffix)));
	  if (handle == NULL)
	    xsignal2 (Qtree_sitter_language_error,
		      progmode, build_string (dynlib_error ()));
	  else
	    {
	      TSLanguageFunctor fn;
	      dynlib_error ();
	      fn = dynlib_sym (handle,
			       SSDATA (concat2 (build_string ("tree_sitter_"),
						progmode)));
	      if (fn == NULL)
		xsignal2 (Qtree_sitter_language_error,
			  progmode, build_string (dynlib_error ()));
	      else
		i = hash_put (h, symbol, make_misc_ptr (fn), hash);
	    }
	}
    }
  return i >= 0 ? (TSLanguageFunctor) (xmint_pointer (HASH_VALUE (h, i))) : NULL;
}

DEFUN ("tree-sitter-language-p",
       Ftree_sitter_language_p, Stree_sitter_language_p,
       1, 1, 0,
       doc: /* Return t if language corresponding to PROGMODE is loadable.  */)
  (Lisp_Object progmode)
{
  return (tree_sitter_language_functor (progmode) == NULL) ? Qnil : Qt;
}

/* An auxiliary function that saves a few lines of code.  */
static inline void
tree_sitter_tree_edit_1 (TSTree *tree, ptrdiff_t start_byte,
		ptrdiff_t old_end_byte, ptrdiff_t new_end_byte)
{
  TSPoint dummy_point = {0, 0};
  TSInputEdit edit = {(uint32_t) start_byte,
		      (uint32_t) old_end_byte,
		      (uint32_t) new_end_byte,
		      dummy_point, dummy_point, dummy_point};
  tree_sitter_tree_edit (tree, &edit);
}

/* "In applications like text editors, you often need to re-parse a
   file after its source code has changed.  First, you must adjust
   the ranges of nodes to reflect the new state.  Then, call
   `ts_parser_parse` again, passing in the old tree."
   -- tree-sitter doc */
void
tree_sitter_record_change (ptrdiff_t start_byte,
			   ptrdiff_t old_end_byte,
			   ptrdiff_t new_end_byte)
{
  Lisp_Object state = Fbuffer_local_value (Qtree_sitter_buffer_state, Fcurrent_buffer ());
  TSTree *tree = XTREE_SITTER (state)->tree;

  eassert (start_byte <= old_end_byte
	   && start_byte <= new_end_byte);

  if (tree != NULL)
    {
      ptrdiff_t bytes_del = old_end_byte - start_byte,
	bytes_ins = new_end_byte - start_byte,
	visible_beg = XTREE_SITTER (state)->visible_beg,
	visible_end = XTREE_SITTER (state)->visible_end;

      ptrdiff_t affected_start = max (visible_beg, start_byte) - visible_beg;
      ptrdiff_t affected_old_end = min (visible_end, affected_start + bytes_del);
      ptrdiff_t affected_new_end = affected_start + bytes_ins;

      tree_sitter_tree_edit_1 (tree, affected_start, affected_old_end,
			       affected_new_end);
      XTREE_SITTER (state)->visible_end = affected_new_end;
      XTREE_SITTER (state)->need_reparse = true;
    }
}

static void
tree_sitter_ensure_position_synced (Lisp_Object state)
{
  TSTree *tree = XTREE_SITTER (state)->tree;
  struct buffer *buffer = XBUFFER (XTREE_SITTER (state)->buffer);
  /* Before we parse or set ranges, catch up with the narrowing
     situation.  We change visible_beg and visible_end to match
     BUF_BEGV_BYTE and BUF_ZV_BYTE, and inform tree-sitter of the
     change.  */
  ptrdiff_t visible_beg = XTREE_SITTER (state)->visible_beg;
  ptrdiff_t visible_end = XTREE_SITTER (state)->visible_end;
  /* Before re-parse, we want to move the visible range of tree-sitter
     to matched the narrowed range. For example,
     from ________|xxxx|__
     to   |xxxx|__________ */

  /* 1. Make sure visible_beg <= BUF_BEGV_BYTE.  */
  if (visible_beg > BUF_BEGV_BYTE (buffer))
    {
      /* Tree-sitter sees: insert at the beginning. */
      tree_sitter_tree_edit_1 (tree, 0, 0, visible_beg - BUF_BEGV_BYTE (buffer));
      visible_beg = BUF_BEGV_BYTE (buffer);
    }
  /* 2. Make sure visible_end = BUF_ZV_BYTE.  */
  if (visible_end < BUF_ZV_BYTE (buffer))
    {
      /* Tree-sitter sees: insert at the end.  */
      tree_sitter_tree_edit_1 (tree, visible_end - visible_beg,
		      visible_end - visible_beg,
		      BUF_ZV_BYTE (buffer) - visible_beg);
      visible_end = BUF_ZV_BYTE (buffer);
    }
  else if (visible_end > BUF_ZV_BYTE (buffer))
    {
      /* Tree-sitter sees: delete at the end.  */
      tree_sitter_tree_edit_1 (tree, BUF_ZV_BYTE (buffer) - visible_beg,
		      visible_end - visible_beg,
		      BUF_ZV_BYTE (buffer) - visible_beg);
      visible_end = BUF_ZV_BYTE (buffer);
    }
  /* 3. Make sure visible_beg = BUF_BEGV_BYTE.  */
  if (visible_beg < BUF_BEGV_BYTE (buffer))
    {
      /* Tree-sitter sees: delete at the beginning.  */
      tree_sitter_tree_edit_1 (tree, 0, BUF_BEGV_BYTE (buffer) - visible_beg, 0);
      visible_beg = BUF_BEGV_BYTE (buffer);
    }
  eassert (0 <= visible_beg);
  eassert (visible_beg <= visible_end);

  XTREE_SITTER (state)->visible_beg = visible_beg;
  XTREE_SITTER (state)->visible_end = visible_end;
}

static void
tree_sitter_ensure_parsed (Lisp_Object state)
{
  if (! XTREE_SITTER (state)->need_reparse)
    return;
  TSParser *parser = XTREE_SITTER (state)->parser;
  TSTree *old_tree = XTREE_SITTER (state)->tree;
  TSInput input = XTREE_SITTER (state)->input;
  struct buffer *buffer = XBUFFER (XTREE_SITTER (state)->buffer);

  /* Before we parse, catch up with the narrowing situation.  */
  tree_sitter_ensure_position_synced (state);

  TSTree *new_tree = ts_parser_parse(parser, old_tree, input);
  if (new_tree == NULL)
    {
      Lisp_Object buf;
      XSETBUFFER (buf, buffer);
      xsignal1 (Qtree_sitter_error, buf);
    }

  tree_sitter_tree_delete (old_tree);
  XTREE_SITTER (state)->tree = new_tree;
  XTREE_SITTER (state)->need_reparse = false;
}

/* This is the read function provided to tree-sitter to read from a
   buffer.  It reads one character at a time and automatically skips
   the gap.  */
static const char*
tree_sitter_read_buffer (void *parser, uint32_t byte_index,
			 TSPoint position, uint32_t *bytes_read)
{
  struct buffer *buffer =
    XBUFFER (((struct Lisp_Tree_Sitter *) parser)->buffer);
  ptrdiff_t visible_beg = ((struct Lisp_Tree_Sitter *) parser)->visible_beg;
  ptrdiff_t visible_end = ((struct Lisp_Tree_Sitter *) parser)->visible_end;
  ptrdiff_t byte_pos = byte_index + visible_beg;
  /* We will make sure visible_beg = BUF_BEGV_BYTE before re-parse (in
     tree_sitter_ensure_parsed), so byte_pos will never be smaller than
     BUF_BEG_BYTE.  */
  eassert (visible_beg = BUF_BEGV_BYTE (buffer));
  eassert (visible_end = BUF_ZV_BYTE (buffer));

  /* Read one character.  Tree-sitter wants us to set bytes_read to 0
     if it reads to the end of buffer.  It doesn't say what it wants
     for the return value in that case, so we just give it an empty
     string.  */
  char *beg;
  int len;
  /* This function could run from a user command, so it is better to
     do nothing instead of raising an error. (It was a pain in the a**
     to decrypt mega-if-conditions in Emacs source, so I wrote the two
     branches separately.)  */
  if (!BUFFER_LIVE_P (buffer))
    {
      beg = NULL;
      len = 0;
    }
  /* Reached visible end-of-buffer, tell tree-sitter to read no more.  */
  else if (byte_pos >= visible_end)
    {
      beg = NULL;
      len = 0;
    }
  /* Normal case, read a character.  */
  else
    {
      beg = (char *) BUF_BYTE_ADDRESS (buffer, byte_pos);
      len = BYTES_BY_CHAR_HEAD ((int) *beg);
    }
  *bytes_read = (uint32_t) len;
  return beg;
}

DEFUN ("tree-sitter-create",
       Ftree_sitter_create, Stree_sitter_create,
       2, 2, 0,
       doc: /* Create and return a tree-sitter in BUFFER for PROGMODE. */)
  (Lisp_Object buffer, Lisp_Object progmode)
{
  Lisp_Object tree_sitter;
  TSLanguageFunctor fn;

  CHECK_BUFFER (buffer);
  CHECK_SYMBOL (progmode);

  fn = tree_sitter_language_functor (progmode);
  if (fn != NULL)
    {
      TSParser *ts_parser = ts_parser_new ();
      tree_sitter_parser_set_language (ts_parser, fn ());
      tree_sitter = make_tree_sitter (buffer, ts_parser, NULL, language);
    }
  return tree_sitter;
}

/* Checks that the RANGES argument of
   tree-sitter-parser-set-included-ranges is valid.  */
static void
tree_sitter_check_range_argument (Lisp_Object ranges)
{
  EMACS_INT last_point = 1;
  while (!NILP (ranges))
    {
      Lisp_Object range = XCAR (ranges);
      CHECK_FIXNUM (XCAR (range));
      CHECK_FIXNUM (XCDR (range));
      EMACS_INT beg = XFIXNUM (XCAR (range));
      EMACS_INT end = XFIXNUM (XCDR (range));
      /* TODO: Maybe we should check for point-min/max, too?  */
      if (!(last_point <= beg && beg <= end))
	xsignal2 (Qtree_sitter_set_range_error,
		  build_pure_c_string
		  ("RANGE is either overlapping or out-of-order"),
		  range);
      last_point = end;
      ranges = XCDR (ranges);
    }
}

DEFUN ("tree-sitter-set-ranges",
       Ftree_sitter_set_ranges,
       Stree_sitter_set_ranges,
       2, 2, 0,
       doc: /* Limit PARSER to RANGES.

RANGES is a list of (BEG . END), each (BEG . END) confines a range in
which the parser should operate in.  Each range must not overlap, and
each range should come in order.  Signal `tree-sitter-set-range-error'
if the argument is invalid, or something else went wrong.  If RANGES
is nil, set PARSER to parse the whole buffer.  */)
  (Lisp_Object state, Lisp_Object ranges)
{
  CHECK_CONS (ranges);
  tree_sitter_check_range_argument (ranges);

  /* Before we parse, catch up with narrowing/widening.  */
  tree_sitter_ensure_position_synced (state);

  bool success;
  if (NILP (ranges))
    {
      /* If RANGES is nil, make state to parse the whole document.
	 To do that we give tree-sitter a 0 length, the range is a
	 dummy.  */
      TSRange ts_range = { { 0, 0 }, { 0, 0 }, 0, 0 };
      success = tree_sitter_set_ranges
	(XTREE_SITTER (state)->parser, &ts_range , 0);
    }
  else
    {
      /* Set ranges for STATE.  */
      ptrdiff_t len = list_length (ranges);
      TSRange *ts_ranges = malloc (sizeof(TSRange) * len);

      for (int idx=0; !NILP (ranges); idx++, ranges = XCDR (ranges))
	{
	  Lisp_Object range = XCAR (ranges);
	  struct buffer *buffer = XBUFFER (XTREE_SITTER (state)->buffer);

	  EMACS_INT beg_byte = buf_charpos_to_bytepos
	    (buffer, XFIXNUM (XCAR (range)));
	  EMACS_INT end_byte = buf_charpos_to_bytepos
	    (buffer, XFIXNUM (XCDR (range)));
	  /* We don't care about start and end points, put in dummy
	     value.  */
	  TSRange rg = {{0,0}, {0,0},
			(uint32_t) beg_byte - BUF_BEGV_BYTE (buffer),
			(uint32_t) end_byte - BUF_BEGV_BYTE (buffer)};
	  ts_ranges[idx] = rg;
	}
      success = tree_sitter_set_ranges
	(XTREE_SITTER (state)->parser, ts_ranges, (uint32_t) len);
      free (ts_ranges);
    }

  if (! success)
    xsignal1 (Qtree_sitter_set_range_error, ranges);

  XTREE_SITTER (state)->need_reparse = true;
  return Qnil;
}

DEFUN ("tree-sitter-ranges",
       Ftree_sitter_ranges,
       Stree_sitter_ranges,
       1, 1, 0,
       doc: /* Return the ranges set for STATE.
See `tree-sitter-set-ranges'.  If no range is set, return
nil.  */)
  (Lisp_Object state)
{
  uint32_t len;
  const TSRange *ranges = ts_parser_included_ranges (XTREE_SITTER (state)->parser, &len);
  if (len == 0)
    return Qnil;
  struct buffer *buffer = XBUFFER (XTREE_SITTER (state)->buffer);

  Lisp_Object list = Qnil;
  for (int idx=0; idx < len; idx++)
    {
      TSRange range = ranges[idx];
      uint32_t beg_byte = range.start_byte + BUF_BEGV_BYTE (buffer);
      uint32_t end_byte = range.end_byte + BUF_BEGV_BYTE (buffer);

      Lisp_Object lisp_range =
	Fcons (make_fixnum (buf_bytepos_to_charpos (buffer, beg_byte)) ,
	       make_fixnum (buf_bytepos_to_charpos (buffer, end_byte)));
      list = Fcons (lisp_range, list);
    }
  return Freverse (list);
}

DEFUN ("tree-sitter-expand-pattern-1",
       Ftree_sitter_expand_pattern_1,
       Stree_sitter_expand_pattern_1, 1, 1, 0,
       doc: /* Expand PATTERN to its string form.

PATTERN can be

    :anchor
    :?
    :*
    :+
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"

Consult Info node `(elisp)Pattern Matching' form detailed
explanation.  */)
  (Lisp_Object pattern)
{
  if (EQ (pattern, intern_c_string (":anchor")))
    return build_pure_c_string(".");
  if (EQ (pattern, intern_c_string (":?")))
    return build_pure_c_string("?");
  if (EQ (pattern, intern_c_string (":*")))
    return build_pure_c_string("*");
  if (EQ (pattern, intern_c_string (":+")))
    return build_pure_c_string("+");
  Lisp_Object opening_delimeter =
    build_pure_c_string (VECTORP (pattern) ? "[" : "(");
  Lisp_Object closing_delimiter =
    build_pure_c_string (VECTORP (pattern) ? "]" : ")");
  if (VECTORP (pattern) || CONSP (pattern))
    return concat3 (opening_delimeter,
		    Fmapconcat (intern_c_string
				("tree-sitter-expand-pattern-1"),
				pattern,
				build_pure_c_string (" ")),
		    closing_delimiter);
  return CALLN (Fformat, build_pure_c_string("%S"), pattern);
}

DEFUN ("tree-sitter-expand-pattern",
       Ftree_sitter_expand_pattern,
       Stree_sitter_expand_pattern, 1, 1, 0,
       doc: /* Expand PATTERN-LIST to its string form.

A PATTERN in PATTERN-LIST can be

    :anchor
    :?
    :*
    :+
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"

Consult Info node `(elisp)Pattern Matching' form detailed
explanation.  */)
  (Lisp_Object pattern_list)
{
  return Fmapconcat (intern_c_string ("tree-sitter-expand-pattern-1"),
		     pattern_list, build_pure_c_string (" "));
}

static const char*
tree_sitter_query_error_to_string (TSQueryError error)
{
  switch (error)
    {
    case TSQueryErrorNone:
      return "none";
      break;
    case TSQueryErrorSyntax:
      return "syntax";
      break;
    case TSQueryErrorNodeType:
      return "node type";
      break;
    case TSQueryErrorField:
      return "field";
      break;
    case TSQueryErrorCapture:
      return "capture";
      break;
    case TSQueryErrorStructure:
      return "structure";
      break;
    default:
      break;
    }
  return "";
}

DEFUN ("tree-sitter-query-capture",
       Ftree_sitter_query_capture,
       Stree_sitter_query_capture, 2, 4, 0,
       doc: /* Query NODE with PATTERN.

Returns a list of (CAPTURE_NAME . NODE).  CAPTURE_NAME is the name
assigned to the node in PATTERN.  NODE is the captured node.

PATTERN is either a string containing one or more matching patterns,
or a list containing one or more s-expression matching patterns.  See
Info node `(elisp)Parsing' for how to write a query pattern in either
string or s-expression form.

BEG and END, if _both_ non-nil, specifies the range in which the query
is executed.

Raise an tree-sitter-query-error if PATTERN is malformed.  See the
info node for how to read the error message.  */)
  (Lisp_Object node, Lisp_Object pattern,
   Lisp_Object beg, Lisp_Object end)
{
  if (!NILP (beg))
    CHECK_INTEGER (beg);
  if (!NILP (end))
    CHECK_INTEGER (end);

  if (CONSP (pattern))
    pattern = Ftree_sitter_expand_pattern (pattern);
  else
    CHECK_STRING (pattern);

  TSNode ts_node = XTS_NODE (node)->node;
  Lisp_Object lisp_parser = XTS_NODE (node)->parser;
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  const TSLanguage *lang = XTS_PARSER (lisp_parser)->parser->language_symbol;
  char *source = SSDATA (pattern);
  uint32_t error_offset;
  TSQueryError error_type;
  TSQuery *query = ts_query_new (lang, source, strlen (source),
					  &error_offset, &error_type);
  TSQueryCursor *cursor = ts_query_cursor_new ();

  if (query == NULL)
      xsignal1 (Qtree_sitter_query_error, make_fixnum (error_type));

  if (! NILP (beg) && ! NILP (end))
    {
      EMACS_INT beg_byte = XFIXNUM (beg), end_byte = XFIXNUM (end);
      tree_sitter_query_cursor_set_byte_range
	(cursor, (uint32_t) beg_byte - visible_beg,
	 (uint32_t) end_byte - visible_beg);
    }

  tree_sitter_query_cursor_exec (cursor, query, ts_node);
  TSQueryMatch match;

  Lisp_Object result = Qnil;
  while (tree_sitter_query_cursor_next_match (cursor, &match))
    {
      const TSQueryCapture *captures = match.captures;
      for (int idx = 0; idx < match.capture_count; idx++)
	{
	  TSQueryCapture capture;
	  Lisp_Object captured_node;
	  const char *capture_name;
	  Lisp_Object entry;
	  uint32_t capture_name_len;

	  capture = captures[idx];
	  captured_node = make_tree_sitter_node (lisp_parser, capture.node);
	  capture_name = ts_query_capture_name_for_id
	    (query, capture.index, &capture_name_len);
	  entry = Fcons (intern_c_string (capture_name), captured_node);
	  result = Fcons (entry, result);
	}
    }
  tree_sitter_query_delete (query);
  tree_sitter_query_cursor_delete (cursor);
  return Freverse (result);
}

void
syms_of_tree_sitter (void)
{
  DEFSYM (Qtree_sitter_error, "tree-sitter-error");
  DEFSYM (Qtree_sitter_query_error, "tree-sitter-query-error");
  DEFSYM (Qtree_sitter_range_error, "tree-sitter-range-error");
  DEFSYM (Qtree_sitter_language_error, "tree-sitter-language-error");

  define_error (Qtree_sitter_error, "Generic tree-sitter error", Qerror);
  define_error (Qtree_sitter_query_error, "Query pattern is malformed",
		Qtree_sitter_error);
  define_error (Qtree_sitter_range_error, "Ranges unordered or overlapping",
		Qtree_sitter_error);
  define_error (Qtree_sitter_language_error, "Cannot load language",
		Qtree_sitter_error);

  DEFSYM (Qtree_sitter_mode_alist, "tree-sitter-mode-alist");

  DEFSYM (Qtree_sitter_buffer_state, "tree-sitter-buffer-state");
  Fmake_variable_buffer_local (Qtree_sitter_buffer_state);

  defsubr (&Stree_sitter_create);

  defsubr (&Stree_sitter_set_ranges);
  defsubr (&Stree_sitter_ranges);

  defsubr (&Stree_sitter_node_type);
  defsubr (&Stree_sitter_node_start);
  defsubr (&Stree_sitter_node_end);
  defsubr (&Stree_sitter_node_string);
  defsubr (&Stree_sitter_node_parent);
  defsubr (&Stree_sitter_node_child);
  defsubr (&Stree_sitter_node_check);
  defsubr (&Stree_sitter_node_field_name_for_child);
  defsubr (&Stree_sitter_node_child_count);
  defsubr (&Stree_sitter_node_child_by_field_name);
  defsubr (&Stree_sitter_node_next_sibling);
  defsubr (&Stree_sitter_node_prev_sibling);
  defsubr (&Stree_sitter_node_first_child_for_pos);
  defsubr (&Stree_sitter_node_descendant_for_range);
  defsubr (&Stree_sitter_node_eq);
  defsubr (&Stree_sitter_expand_pattern);
  defsubr (&Stree_sitter_expand_pattern_1);
  defsubr (&Stree_sitter_language_p);
  defsubr (&Stree_sitter_query_capture);
}
