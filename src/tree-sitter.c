/* Tree-sitter integration for GNU Emacs.

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
#include "buffer.h"
#include "tree-sitter.h"

/* Commentary

   The Emacs wrapper of tree-sitter does not expose everything the C
   API provides, most notably:

   - It doesn't expose a syntax tree, we put the syntax tree in the
     parser object, and updating the tree is handled in the C level.

   - We don't expose tree cursor either.  I think Lisp is slow enough
     to nullify any performance advantage of using a cursor, though I
     don't have evidence.

   - Because updating the change is handled in the C level as each
     change is made in the buffer, there is no way for Lisp to update
     a node.  But since we can just retrieve a new node, it shouldn't
     be a limitation.

   - I didn't expose setting timeout and cancellation flag for a
     parser, mainly because I don't think they are really necessary
     in Emacs' use cases.

   - Many tree-sitter functions asks for a TSPoint, basically a (row,
     column) location.  Emacs uses a gap buffer and keeps no
     information about row and column position.  According to the
     author of tree-sitter, tree-sitter only asks for (row, column)
     position to carry it around and return back to the user later;
     and the real position used is the byte position.  He also said
     that he _think_ that it will work to use byte position only.
     That's why whenever a TSPoint is asked, we pass a dummy one to
     it.  Judging by the nature of parsing algorithms, I think it is
     safe to use only byte position, and I don't think this will
     change in the future.

     REF: https://github.com/tree-sitter/tree-sitter/issues/445

   tree-sitter.h has some commentary on the two main data structure
   for the parser and node.  ts_ensure_position_synced has some
   commentary on how do we make tree-sitter play well with narrowing
   (tree-sitter parser only sees the visible region, so we need to
   translate positions back and forth).  Most action happens in
   ts_ensure_parsed, ts_read_buffer and ts_record_change.

   A complete correspondence list between tree-sitter functions and
   exposed Lisp functions can be found in the manual (elisp)API
   Correspondence.
 */

/*** Loading language library */

/* Translates a symbol tree-sitter-<lang> to a C name
   tree_sitter_<lang>.  */
void
ts_symbol_to_c_name (char *symbol_name)
{
  for (int idx=0; idx < strlen (symbol_name); idx++)
    {
      if (symbol_name[idx] == '-')
	symbol_name[idx] = '_';
    }
}

bool
ts_find_override_name
(Lisp_Object language_symbol, Lisp_Object *name, Lisp_Object *c_symbol)
{
  Lisp_Object list = Vtree_sitter_load_name_list;
  while (!NILP (list))
    {
      Lisp_Object lang = XCAR (XCAR (list));
      CHECK_SYMBOL (lang);
      if (EQ (lang, language_symbol))
	{
	  *name = Fnth (make_fixnum (1), XCAR (list));
	  CHECK_STRING (*name);
	  *c_symbol = Fnth (make_fixnum (2), XCAR (list));
	  CHECK_STRING (*c_symbol);
	  return true;
	}
      list = XCDR (list);
    }
  return false;
}

/* Load the dynamic library of LANGUAGE_SYMBOL and return the pointer
   to the language definition.  Signals
   Qtree_sitter_load_language_error if something goes wrong.
   Qtree_sitter_load_language_error carries the error message from
   trying to load the library with each extension.

   If SIGNAL is true, signal an error when failed to load LANGUAGE; if
   false, return NULL when failed.  */
TSLanguage*
ts_load_language (Lisp_Object language_symbol, bool signal)
{
  CHECK_SYMBOL (language_symbol);
  Lisp_Object symbol_name = Fsymbol_name (language_symbol);

  /* Figure out the library name and C name.  */
  Lisp_Object lib_base_name =
    (concat2 (build_pure_c_string ("lib"), symbol_name));
  char *c_name = strdup (SSDATA (symbol_name));
  ts_symbol_to_c_name (c_name);

  /* Override the library name and C name, if appropriate.  */
  Lisp_Object override_name;
  Lisp_Object override_c_name;
  bool found_override = ts_find_override_name
    (language_symbol, &override_name, &override_c_name);
  if (found_override)
    {
      lib_base_name = override_name;
      c_name = SSDATA (override_c_name);
    }

  Lisp_Object suffixes = Vdynamic_library_suffixes;
  dynlib_handle_ptr handle;
  char const *error;
  Lisp_Object error_list = Qnil;
  /* Try loading dynamic library with each extension in
     'tree-sitter-load-suffixes'.  Stop when succeed, record error
     message and try the next one when fail.  */
  while (!NILP (suffixes))
    {
      char *library_name =
	SSDATA (concat2 (lib_base_name, XCAR (suffixes)));
      dynlib_error ();
      handle = dynlib_open (library_name);
      error = dynlib_error ();
      if (error == NULL)
	break;
      else
	error_list = Fcons (build_string (error), error_list);
      suffixes = XCDR (suffixes);
    }
  if (error != NULL)
    {
      if (signal)
	xsignal2 (Qtree_sitter_load_language_error,
		  symbol_name, Freverse (error_list));
      else
	return NULL;
    }

  /* Load TSLanguage.  */
  dynlib_error ();
  TSLanguage *(*langfn) ();
  langfn = dynlib_sym (handle, c_name);
  error = dynlib_error ();
  if (error != NULL)
    {
      if (signal)
	xsignal1 (Qtree_sitter_load_language_error,
		  build_string (error));
      else
	return NULL;
    }
  TSLanguage *lang = (*langfn) ();

  /* Check if language version matches tree-sitter version.  */
  TSParser *parser = ts_parser_new ();
  bool success = ts_parser_set_language (parser, lang);
  ts_parser_delete (parser);
  if (!success)
    {
      if (signal)
	xsignal2 (Qtree_sitter_load_language_error,
		  build_pure_c_string ("Language version doesn't match tree-sitter version, language version:"),
		  make_fixnum (ts_language_version (lang)));
      else
	return NULL;
    }
  return lang;
}

DEFUN ("tree-sitter-language-exists-p",
       Ftree_sitter_langauge_exists_p, Stree_sitter_language_exists_p,
       1, 1, 0,
       doc: /* Return non-nil if LANGUAGE exists and is loadable.  */)
  (Lisp_Object language)
{
  if (ts_load_language(language, false) == NULL)
    return Qnil;
  else
    return Qt;
}

/*** Parsing functions */

/* An auxiliary function that saves a few lines of code.  */
static inline void
ts_tree_edit_1 (TSTree *tree, ptrdiff_t start_byte,
		ptrdiff_t old_end_byte, ptrdiff_t new_end_byte)
{
  TSPoint dummy_point = {0, 0};
  TSInputEdit edit = {(uint32_t) start_byte,
		      (uint32_t) old_end_byte,
		      (uint32_t) new_end_byte,
		      dummy_point, dummy_point, dummy_point};
  ts_tree_edit (tree, &edit);
}

/* Update each parser's tree after the user made an edit.  This
function does not parse the buffer and only updates the tree. (So it
should be very fast.)  */
void
ts_record_change (ptrdiff_t start_byte, ptrdiff_t old_end_byte,
		  ptrdiff_t new_end_byte)
{
  Lisp_Object parser_list = Fsymbol_value (Qtree_sitter_parser_list);

  while (!NILP (parser_list))
    {
      Lisp_Object lisp_parser = XCAR (parser_list);
      TSTree *tree = XTS_PARSER (lisp_parser)->tree;
      if (tree != NULL)
	{
	  eassert (start_byte <= old_end_byte);
	  eassert (start_byte <= new_end_byte);
	  /* Think the recorded change as a delete followed by an
	     insert, and think of them as moving unchanged text back
	     and forth.  After all, the whole point of updating the
	     tree is to update the position of unchanged text.  */
	  ptrdiff_t bytes_del = old_end_byte - start_byte;
	  ptrdiff_t bytes_ins = new_end_byte - start_byte;

	  ptrdiff_t visible_beg = XTS_PARSER (lisp_parser)->visible_beg;
	  ptrdiff_t visible_end = XTS_PARSER (lisp_parser)->visible_end;

	  ptrdiff_t affected_start =
	    max (visible_beg, start_byte) - visible_beg;
	  ptrdiff_t affected_old_end =
	    min (visible_end, affected_start + bytes_del);
	  ptrdiff_t affected_new_end =
	    affected_start + bytes_ins;

	  ts_tree_edit_1 (tree, affected_start, affected_old_end,
			  affected_new_end);
	  XTS_PARSER (lisp_parser)->visible_end = affected_new_end;
	  XTS_PARSER (lisp_parser)->need_reparse = true;
	}
      parser_list = XCDR (parser_list);
    }
}

void
ts_ensure_position_synced (Lisp_Object parser)
{
  TSParser *ts_parser = XTS_PARSER (parser)->parser;
  TSTree *tree = XTS_PARSER (parser)->tree;
  struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);
  /* Before we parse or set ranges, catch up with the narrowing
     situation.  We change visible_beg and visible_end to match
     BUF_BEGV_BYTE and BUF_ZV_BYTE, and inform tree-sitter of the
     change.  */
  ptrdiff_t visible_beg = XTS_PARSER (parser)->visible_beg;
  ptrdiff_t visible_end = XTS_PARSER (parser)->visible_end;
  /* Before re-parse, we want to move the visible range of tree-sitter
     to matched the narrowed range. For example,
     from ________|xxxx|__
     to   |xxxx|__________ */

  /* 1. Make sure visible_beg <= BUF_BEGV_BYTE.  */
  if (visible_beg > BUF_BEGV_BYTE (buffer))
    {
      /* Tree-sitter sees: insert at the beginning. */
      ts_tree_edit_1 (tree, 0, 0, visible_beg - BUF_BEGV_BYTE (buffer));
      visible_beg = BUF_BEGV_BYTE (buffer);
    }
  /* 2. Make sure visible_end = BUF_ZV_BYTE.  */
  if (visible_end < BUF_ZV_BYTE (buffer))
    {
      /* Tree-sitter sees: insert at the end.  */
      ts_tree_edit_1 (tree, visible_end - visible_beg,
		      visible_end - visible_beg,
		      BUF_ZV_BYTE (buffer) - visible_beg);
      visible_end = BUF_ZV_BYTE (buffer);
    }
  else if (visible_end > BUF_ZV_BYTE (buffer))
    {
      /* Tree-sitter sees: delete at the end.  */
      ts_tree_edit_1 (tree, BUF_ZV_BYTE (buffer) - visible_beg,
		      visible_end - visible_beg,
		      BUF_ZV_BYTE (buffer) - visible_beg);
      visible_end = BUF_ZV_BYTE (buffer);
    }
  /* 3. Make sure visible_beg = BUF_BEGV_BYTE.  */
  if (visible_beg < BUF_BEGV_BYTE (buffer))
    {
      /* Tree-sitter sees: delete at the beginning.  */
      ts_tree_edit_1 (tree, 0, BUF_BEGV_BYTE (buffer) - visible_beg, 0);
      visible_beg = BUF_BEGV_BYTE (buffer);
    }
  eassert (0 <= visible_beg);
  eassert (visible_beg <= visible_end);

  XTS_PARSER (parser)->visible_beg = visible_beg;
  XTS_PARSER (parser)->visible_end = visible_end;
}

void
ts_check_buffer_size (struct buffer *buffer)
{
  ptrdiff_t buffer_size =
    (BUF_Z (buffer) - BUF_BEG (buffer));
  if (buffer_size > UINT32_MAX)
    xsignal1 (Qtree_sitter_size_error, make_fixnum (buffer_size));
}

/* Parse the buffer.  We don't parse until we have to. When we have
to, we call this function to parse and update the tree.  */
void
ts_ensure_parsed (Lisp_Object parser)
{
  if (!XTS_PARSER (parser)->need_reparse)
    return;
  TSParser *ts_parser = XTS_PARSER (parser)->parser;
  TSTree *tree = XTS_PARSER(parser)->tree;
  TSInput input = XTS_PARSER (parser)->input;
  struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);
  ts_check_buffer_size (buffer);

  /* Before we parse, catch up with the narrowing situation.  */
  ts_ensure_position_synced (parser);

  TSTree *new_tree = ts_parser_parse(ts_parser, tree, input);
  /* This should be very rare (impossible, really): it only happens
     when 1) language is not set (impossible in Emacs because the user
     has to supply a language to create a parser), 2) parse canceled
     due to timeout (impossible because we don't set a timeout), 3)
     parse canceled due to cancellation flag (impossible because we
     don't set the flag).  (See comments for ts_parser_parse in
     tree_sitter/api.h.)  */
  if (new_tree == NULL)
    {
      Lisp_Object buf;
      XSETBUFFER (buf, buffer);
      xsignal1 (Qtree_sitter_parse_error, buf);
    }

  ts_tree_delete (tree);
  XTS_PARSER (parser)->tree = new_tree;
  XTS_PARSER (parser)->need_reparse = false;
}

/* This is the read function provided to tree-sitter to read from a
   buffer.  It reads one character at a time and automatically skips
   the gap.  */
const char*
ts_read_buffer (void *parser, uint32_t byte_index,
		TSPoint position, uint32_t *bytes_read)
{
  struct buffer *buffer =
    XBUFFER (((struct Lisp_TS_Parser *) parser)->buffer);
  ptrdiff_t visible_beg = ((struct Lisp_TS_Parser *) parser)->visible_beg;
  ptrdiff_t visible_end = ((struct Lisp_TS_Parser *) parser)->visible_end;
  ptrdiff_t byte_pos = byte_index + visible_beg;
  /* We will make sure visible_beg = BUF_BEGV_BYTE before re-parse (in
     ts_ensure_parsed), so byte_pos will never be smaller than
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

/*** Functions for parser and node object*/

/* Wrap the parser in a Lisp_Object to be used in the Lisp machine.  */
Lisp_Object
make_ts_parser (Lisp_Object buffer, TSParser *parser,
		TSTree *tree, Lisp_Object language_symbol)
{
  struct Lisp_TS_Parser *lisp_parser
    = ALLOCATE_PSEUDOVECTOR
    (struct Lisp_TS_Parser, parser, PVEC_TS_PARSER);

  lisp_parser->language_symbol = language_symbol;
  lisp_parser->buffer = buffer;
  lisp_parser->parser = parser;
  lisp_parser->tree = tree;
  TSInput input = {lisp_parser, ts_read_buffer, TSInputEncodingUTF8};
  lisp_parser->input = input;
  lisp_parser->need_reparse = true;
  lisp_parser->visible_beg = BUF_BEGV (XBUFFER (buffer));
  lisp_parser->visible_end = BUF_ZV (XBUFFER (buffer));
  return make_lisp_ptr (lisp_parser, Lisp_Vectorlike);
}

/* Wrap the node in a Lisp_Object to be used in the Lisp machine.  */
Lisp_Object
make_ts_node (Lisp_Object parser, TSNode node)
{
  struct Lisp_TS_Node *lisp_node
    = ALLOCATE_PSEUDOVECTOR (struct Lisp_TS_Node, parser, PVEC_TS_NODE);
  lisp_node->parser = parser;
  lisp_node->node = node;
  return make_lisp_ptr (lisp_node, Lisp_Vectorlike);
}

DEFUN ("tree-sitter-parser-p",
       Ftree_sitter_parser_p, Stree_sitter_parser_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a tree-sitter parser.  */)
  (Lisp_Object object)
{
  if (TS_PARSERP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("tree-sitter-node-p",
       Ftree_sitter_node_p, Stree_sitter_node_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a tree-sitter node.  */)
  (Lisp_Object object)
{
  if (TS_NODEP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("tree-sitter-node-parser",
       Ftree_sitter_node_parser, Stree_sitter_node_parser,
       1, 1, 0,
       doc: /* Return the parser to which NODE belongs.  */)
  (Lisp_Object node)
{
  CHECK_TS_NODE (node);
  return XTS_NODE (node)->parser;
}

DEFUN ("tree-sitter-parser-create",
       Ftree_sitter_parser_create, Stree_sitter_parser_create,
       2, 2, 0,
       doc: /* Create and return a parser in BUFFER for LANGUAGE.

The parser is automatically added to BUFFER's
`tree-sitter-parser-list'.  LANGUAGE should be the symbol of a
function provided by a tree-sitter language dynamic module, e.g.,
'tree-sitter-json.  */)
  (Lisp_Object buffer, Lisp_Object language)
{
  CHECK_BUFFER(buffer);
  CHECK_SYMBOL (language);
  ts_check_buffer_size (XBUFFER (buffer));

  /* LANGUAGE is a function that returns a USER_PTR that contains the
     pointer to a TSLanguage struct.  */
  TSParser *parser = ts_parser_new ();
  TSLanguage *lang = ts_load_language (language, true);
  /* We check language version when loading a language, so this should
     always succeed.  */
  ts_parser_set_language (parser, lang);

  Lisp_Object lisp_parser
    = make_ts_parser (buffer, parser, NULL, language);

  struct buffer *old_buffer = current_buffer;
  set_buffer_internal (XBUFFER (buffer));

  Fset (Qtree_sitter_parser_list,
	Fcons (lisp_parser, Fsymbol_value (Qtree_sitter_parser_list)));

  set_buffer_internal (old_buffer);
  return lisp_parser;
}

DEFUN ("tree-sitter-parser-buffer",
       Ftree_sitter_parser_buffer, Stree_sitter_parser_buffer,
       1, 1, 0,
       doc: /* Return the buffer of PARSER.  */)
  (Lisp_Object parser)
{
  CHECK_TS_PARSER (parser);
  Lisp_Object buf;
  XSETBUFFER (buf, XBUFFER (XTS_PARSER (parser)->buffer));
  return buf;
}

DEFUN ("tree-sitter-parser-language",
       Ftree_sitter_parser_language, Stree_sitter_parser_language,
       1, 1, 0,
       doc: /* Return parser's language symbol.
This symbol is the one used to create the parser.  */)
  (Lisp_Object parser)
{
  CHECK_TS_PARSER (parser);
  return XTS_PARSER (parser)->language_symbol;
}

/*** Parser API */

DEFUN ("tree-sitter-parser-root-node",
       Ftree_sitter_parser_root_node, Stree_sitter_parser_root_node,
       1, 1, 0,
       doc: /* Return the root node of PARSER.  */)
  (Lisp_Object parser)
{
  CHECK_TS_PARSER (parser);
  ts_ensure_parsed (parser);
  TSNode root_node = ts_tree_root_node (XTS_PARSER (parser)->tree);
  return make_ts_node (parser, root_node);
}

/* Checks that the RANGES argument of
   tree-sitter-parser-set-included-ranges is valid.  */
void
ts_check_range_argument (Lisp_Object ranges)
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
	xsignal1 (Qtree_sitter_set_range_error, range);
      last_point = end;
      ranges = XCDR (ranges);
    }
}

DEFUN ("tree-sitter-parser-set-included-ranges",
       Ftree_sitter_parser_set_included_ranges,
       Stree_sitter_parser_set_included_ranges,
       2, 2, 0,
       doc: /* Limit PARSER to RANGES.

RANGES is a list of (BEG . END), each (BEG . END) confines a range in
which the parser should operate in.  Each range must not overlap, and
each range should come in order.  Signal `tree-sitter-set-range-error'
if the argument is invalid, or something else went wrong.  If RANGES
is nil, set PARSER to parse the whole buffer.  */)
  (Lisp_Object parser, Lisp_Object ranges)
{
  CHECK_TS_PARSER (parser);
  CHECK_CONS (ranges);
  ts_check_range_argument (ranges);

  /* Before we parse, catch up with narrowing/widening.  */
  ts_ensure_position_synced (parser);

  bool success;
  if (NILP (ranges))
    {
      /* If RANGES is nil, make parser to parse the whole document.
	 To do that we give tree-sitter a 0 length, the range is a
	 dummy.  */
      TSRange ts_range = {0, 0, 0, 0};
      success = ts_parser_set_included_ranges
	(XTS_PARSER (parser)->parser, &ts_range , 0);
    }
  else
    {
      /* Set ranges for PARSER.  */
      ptrdiff_t len = list_length (ranges);
      TSRange *ts_ranges = malloc (sizeof(TSRange) * len);
      struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);

      for (int idx=0; !NILP (ranges); idx++, ranges = XCDR (ranges))
	{
	  Lisp_Object range = XCAR (ranges);
	  struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);

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
      success = ts_parser_set_included_ranges
	(XTS_PARSER (parser)->parser, ts_ranges, (uint32_t) len);
      free (ts_ranges);
    }

  if (!success)
    xsignal2 (Qtree_sitter_set_range_error, ranges,
	      build_pure_c_string
	      ("Something went wrong when setting ranges"));

  XTS_PARSER (parser)->need_reparse = true;
  return Qnil;
}

DEFUN ("tree-sitter-parser-included-ranges",
       Ftree_sitter_parser_included_ranges,
       Stree_sitter_parser_included_ranges,
       1, 1, 0,
       doc: /* Return the ranges set for PARSER.
See `tree-sitter-parser-set-ranges'.  If no range is set, return
nil.  */)
  (Lisp_Object parser)
{
  CHECK_TS_PARSER (parser);
  uint32_t len;
  const TSRange *ranges = ts_parser_included_ranges
    (XTS_PARSER (parser)->parser, &len);
  if (len == 0)
    return Qnil;
  struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);

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

/*** Node API  */

DEFUN ("tree-sitter-node-type",
       Ftree_sitter_node_type, Stree_sitter_node_type, 1, 1, 0,
       doc: /* Return the NODE's type as a string.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  const char *type = ts_node_type (ts_node);
  return build_string (type);
}

DEFUN ("tree-sitter-node-start",
       Ftree_sitter_node_start, Stree_sitter_node_start, 1, 1, 0,
       doc: /* Return the NODE's start position.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  uint32_t start_byte = ts_node_start_byte (ts_node);
  struct buffer *buffer =
    XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t start_pos = buf_bytepos_to_charpos
    (buffer, start_byte + visible_beg);
  return make_fixnum (start_pos);
}

DEFUN ("tree-sitter-node-end",
       Ftree_sitter_node_end, Stree_sitter_node_end, 1, 1, 0,
       doc: /* Return the NODE's end position.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  uint32_t end_byte = ts_node_end_byte (ts_node);
  struct buffer *buffer =
    XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t end_pos = buf_bytepos_to_charpos
    (buffer, end_byte + visible_beg);
  return make_fixnum (end_pos);
}

DEFUN ("tree-sitter-node-string",
       Ftree_sitter_node_string, Stree_sitter_node_string, 1, 1, 0,
       doc: /* Return the string representation of NODE.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  char *string = ts_node_string (ts_node);
  return make_string (string, strlen (string));
}

DEFUN ("tree-sitter-node-parent",
       Ftree_sitter_node_parent, Stree_sitter_node_parent, 1, 1, 0,
       doc: /* Return the immediate parent of NODE.
Return nil if there isn't any.  If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode parent = ts_node_parent (ts_node);

  if (ts_node_is_null (parent))
    return Qnil;

  return make_ts_node (XTS_NODE (node)->parser, parent);
}

DEFUN ("tree-sitter-node-child",
       Ftree_sitter_node_child, Stree_sitter_node_child, 2, 3, 0,
       doc: /* Return the Nth child of NODE.

Return nil if there isn't any.  If NAMED is non-nil, look for named
child only.  NAMED defaults to nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object n, Lisp_Object named)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  CHECK_INTEGER (n);
  EMACS_INT idx = XFIXNUM (n);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode child;
  if (NILP (named))
    child = ts_node_child (ts_node, (uint32_t) idx);
  else
    child = ts_node_named_child (ts_node, (uint32_t) idx);

  if (ts_node_is_null (child))
    return Qnil;

  return make_ts_node (XTS_NODE (node)->parser, child);
}

DEFUN ("tree-sitter-node-check",
       Ftree_sitter_node_check, Stree_sitter_node_check, 2, 2, 0,
       doc: /* Return non-nil if NODE has PROPERTY, nil otherwise.

PROPERTY could be 'named, 'missing, 'extra, 'has-changes, 'has-error.
Named nodes correspond to named rules in the language definition,
whereas "anonymous" nodes correspond to string literals in the
language definition.

Missing nodes are inserted by the parser in order to recover from
certain kinds of syntax errors, i.e., should be there but not there.

Extra nodes represent things like comments, which are not required the
language definition, but can appear anywhere.

A node "has changes" if the buffer changed since the node is
created. (Don't forget the "s" at the end of 'has-changes.)

A node "has error" if itself is a syntax error or contains any syntax
errors.  */)
  (Lisp_Object node, Lisp_Object property)
{
  CHECK_TS_NODE (node);
  CHECK_SYMBOL (property);
  TSNode ts_node = XTS_NODE (node)->node;
  bool result;
  if (EQ (property, Qnamed))
    result = ts_node_is_named (ts_node);
  else if (EQ (property, Qmissing))
    result = ts_node_is_missing (ts_node);
  else if (EQ (property, Qextra))
    result = ts_node_is_extra (ts_node);
  else if (EQ (property, Qhas_error))
    result = ts_node_has_error (ts_node);
  else if (EQ (property, Qhas_changes))
    result = ts_node_has_changes (ts_node);
  else
    // TODO: Is this a good error message?
    signal_error ("Expecting one of four symbols, see docstring",
		  property);
  return result ? Qt : Qnil;
}

DEFUN ("tree-sitter-node-field-name-for-child",
       Ftree_sitter_node_field_name_for_child,
       Stree_sitter_node_field_name_for_child, 2, 2, 0,
       doc: /* Return the field name of the Nth child of NODE.

Return nil if there isn't any child or no field is found.
If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object n)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  CHECK_INTEGER (n);
  EMACS_INT idx = XFIXNUM (n);
  TSNode ts_node = XTS_NODE (node)->node;
  const char *name
    = ts_node_field_name_for_child (ts_node, (uint32_t) idx);

  if (name == NULL)
    return Qnil;

  return make_string (name, strlen (name));
}

DEFUN ("tree-sitter-node-child-count",
       Ftree_sitter_node_child_count,
       Stree_sitter_node_child_count, 1, 2, 0,
       doc: /* Return the number of children of NODE.

If NAMED is non-nil, count named child only.  NAMED defaults to
nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  uint32_t count;
  if (NILP (named))
    count = ts_node_child_count (ts_node);
  else
    count = ts_node_named_child_count (ts_node);
  return make_fixnum (count);
}

DEFUN ("tree-sitter-node-child-by-field-name",
       Ftree_sitter_node_child_by_field_name,
       Stree_sitter_node_child_by_field_name, 2, 2, 0,
       doc: /* Return the child of NODE with FIELD-NAME.
Return nil if there isn't any.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object field_name)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  CHECK_STRING (field_name);
  char *name_str = SSDATA (field_name);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode child
    = ts_node_child_by_field_name (ts_node, name_str, strlen (name_str));

  if (ts_node_is_null(child))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, child);
}

DEFUN ("tree-sitter-node-next-sibling",
       Ftree_sitter_node_next_sibling,
       Stree_sitter_node_next_sibling, 1, 2, 0,
       doc: /* Return the next sibling of NODE.

Return nil if there isn't any.  If NAMED is non-nil, look for named
child only.  NAMED defaults to nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode sibling;
  if (NILP (named))
    sibling = ts_node_next_sibling (ts_node);
  else
    sibling = ts_node_next_named_sibling (ts_node);

  if (ts_node_is_null(sibling))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, sibling);
}

DEFUN ("tree-sitter-node-prev-sibling",
       Ftree_sitter_node_prev_sibling,
       Stree_sitter_node_prev_sibling, 1, 2, 0,
       doc: /* Return the previous sibling of NODE.

Return nil if there isn't any.  If NAMED is non-nil, look for named
child only.  NAMED defaults to nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode sibling;

  if (NILP (named))
    sibling = ts_node_prev_sibling (ts_node);
  else
    sibling = ts_node_prev_named_sibling (ts_node);

  if (ts_node_is_null(sibling))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, sibling);
}

DEFUN ("tree-sitter-node-first-child-for-pos",
       Ftree_sitter_node_first_child_for_pos,
       Stree_sitter_node_first_child_for_pos, 2, 3, 0,
       doc: /* Return the first child of NODE on POS.

Specifically, return the first child that extends beyond POS.  POS is
a position in the buffer.  Return nil if there isn't any.  If NAMED is
non-nil, look for named child only.  NAMED defaults to nil.  Note that
this function returns an immediate child, not the smallest
(grand)child.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object pos, Lisp_Object named)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  CHECK_INTEGER (pos);

  struct buffer *buf =
    XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  ptrdiff_t byte_pos = buf_charpos_to_bytepos(buf, XFIXNUM (pos));

  if (byte_pos < BUF_BEGV_BYTE (buf) || byte_pos > BUF_ZV_BYTE (buf))
    xsignal1 (Qargs_out_of_range, pos);

  TSNode ts_node = XTS_NODE (node)->node;
  TSNode child;
  if (NILP (named))
    child = ts_node_first_child_for_byte (ts_node, byte_pos - visible_beg);
  else
    child = ts_node_first_named_child_for_byte
      (ts_node, byte_pos - visible_beg);

  if (ts_node_is_null(child))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, child);
}

DEFUN ("tree-sitter-node-descendant-for-range",
       Ftree_sitter_node_descendant_for_range,
       Stree_sitter_node_descendant_for_range, 3, 4, 0,
       doc: /* Return the smallest node that covers BEG to END.

The returned node is a descendant of NODE.  POS is a position.  Return
nil if there isn't any.  If NAMED is non-nil, look for named child
only.  NAMED defaults to nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object beg, Lisp_Object end, Lisp_Object named)
{
  if (NILP (node))
    return Qnil;
  CHECK_TS_NODE (node);
  CHECK_INTEGER (beg);
  CHECK_INTEGER (end);

  struct buffer *buf =
    XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  ptrdiff_t byte_beg = buf_charpos_to_bytepos(buf, XFIXNUM (beg));
  ptrdiff_t byte_end = buf_charpos_to_bytepos(buf, XFIXNUM (end));

  /* Checks for BUFFER_BEG <= BEG <= END <= BUFFER_END.  */
  if (!(BUF_BEGV_BYTE (buf) <= byte_beg
	&& byte_beg <= byte_end
	&& byte_end <= BUF_ZV_BYTE (buf)))
    xsignal2 (Qargs_out_of_range, beg, end);

  TSNode ts_node = XTS_NODE (node)->node;
  TSNode child;
  if (NILP (named))
    child = ts_node_descendant_for_byte_range
      (ts_node, byte_beg - visible_beg , byte_end - visible_beg);
  else
    child = ts_node_named_descendant_for_byte_range
      (ts_node, byte_beg - visible_beg, byte_end - visible_beg);

  if (ts_node_is_null(child))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, child);
}

DEFUN ("tree-sitter-node-eq",
       Ftree_sitter_node_eq,
       Stree_sitter_node_eq, 2, 2, 0,
       doc: /* Return non-nil if NODE1 and NODE2 are the same node.
If any one of NODE1 and NODE2 is nil, return nil.  */)
  (Lisp_Object node1, Lisp_Object node2)
{
  if (NILP (node1) || NILP (node2))
    return Qnil;
  CHECK_TS_NODE (node1);
  CHECK_TS_NODE (node2);

  TSNode ts_node_1 = XTS_NODE (node1)->node;
  TSNode ts_node_2 = XTS_NODE (node2)->node;

  bool same_node = ts_node_eq (ts_node_1, ts_node_2);
  return same_node ? Qt : Qnil;
}

/*** Query functions */

char*
ts_query_error_to_string (TSQueryError error)
{
  switch (error)
    {
    case TSQueryErrorNone:
      return "none";
    case TSQueryErrorSyntax:
      return "syntax";
    case TSQueryErrorNodeType:
      return "node type";
    case TSQueryErrorField:
      return "field";
    case TSQueryErrorCapture:
      return "capture";
    case TSQueryErrorStructure:
      return "structure";
    }
}

DEFUN ("tree-sitter-query-capture",
       Ftree_sitter_query_capture,
       Stree_sitter_query_capture, 2, 4, 0,
       doc: /* Query NODE with PATTERN.

Returns a list of (CAPTURE_NAME . NODE).  CAPTURE_NAME is the name
assigned to the node in PATTERN.  NODE is the captured node.

PATTERN is a string containing one or more matching patterns.  See
Info node `(elisp)Parsing' for how to write a query pattern.

BEG and END, if _both_ non-nil, specifies the range in which the query
is executed.

Raise an tree-sitter-query-error if PATTERN is malformed.  See the
info node for how to read the error message.  */)
  (Lisp_Object node, Lisp_Object pattern,
   Lisp_Object beg, Lisp_Object end)
{
  CHECK_TS_NODE (node);
  CHECK_STRING (pattern);
  if (!NILP (beg))
    CHECK_INTEGER (beg);
  if (!NILP (end))
    CHECK_INTEGER (end);

  TSNode ts_node = XTS_NODE (node)->node;
  Lisp_Object lisp_parser = XTS_NODE (node)->parser;
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  const TSLanguage *lang = ts_parser_language
    (XTS_PARSER (lisp_parser)->parser);
  char *source = SSDATA (pattern);


  uint32_t error_offset;
  TSQueryError error_type;
  TSQuery *query = ts_query_new (lang, source, strlen (source),
				 &error_offset, &error_type);
  TSQueryCursor *cursor = ts_query_cursor_new ();

  if (query == NULL)
    {
      xsignal2 (Qtree_sitter_query_error,
		make_fixnum (error_offset),
		build_string (ts_query_error_to_string (error_type)));
    }
  if (!NILP (beg) && !NILP (end))
    {
      EMACS_INT beg_byte = XFIXNUM (beg);
      EMACS_INT end_byte = XFIXNUM (end);
      ts_query_cursor_set_byte_range
	(cursor, (uint32_t) beg_byte - visible_beg,
	 (uint32_t) end_byte - visible_beg);
    }

  ts_query_cursor_exec (cursor, query, ts_node);
  TSQueryMatch match;

  Lisp_Object result = Qnil;
  while (ts_query_cursor_next_match (cursor, &match))
    {
      const TSQueryCapture *captures = match.captures;
      for (int idx=0; idx < match.capture_count; idx++)
	{
	  TSQueryCapture capture;
	  Lisp_Object captured_node;
	  const char *capture_name;
	  Lisp_Object entry;
	  uint32_t capture_name_len;

	  capture = captures[idx];
	  captured_node = make_ts_node(lisp_parser, capture.node);
	  capture_name = ts_query_capture_name_for_id
	    (query, capture.index, &capture_name_len);
	  entry = Fcons (intern_c_string_1
			 (capture_name, capture_name_len),
			 captured_node);
	  result = Fcons (entry, result);
	}
    }
  ts_query_delete (query);
  ts_query_cursor_delete (cursor);
  return Freverse (result);
}

/*** Initialization */

/* Initialize the tree-sitter routines.  */
void
syms_of_tree_sitter (void)
{
  DEFSYM (Qtree_sitter_parser_p, "tree-sitter-parser-p");
  DEFSYM (Qtree_sitter_node_p, "tree-sitter-node-p");
  DEFSYM (Qnamed, "named");
  DEFSYM (Qmissing, "missing");
  DEFSYM (Qextra, "extra");
  DEFSYM (Qhas_changes, "has-changes");
  DEFSYM (Qhas_error, "has-error");

  DEFSYM(Qtree_sitter_error, "tree-sitter-error");
  DEFSYM (Qtree_sitter_query_error, "tree-sitter-query-error");
  DEFSYM (Qtree_sitter_parse_error, "tree-sitter-parse-error");
  DEFSYM (Qtree_sitter_set_range_error, "tree-sitter-set-range-error");
  DEFSYM (Qtree_sitter_size_error, "tree-sitter-size-error");
  DEFSYM (Qtree_sitter_load_language_error,
	  "tree-sitter-load-language-error");

  define_error (Qtree_sitter_error, "Generic tree-sitter error", Qerror);
  define_error (Qtree_sitter_query_error, "Query pattern is malformed",
		Qtree_sitter_error);
  define_error (Qtree_sitter_parse_error, "Parse failed",
		Qtree_sitter_error);
  define_error (Qtree_sitter_set_range_error,
		"RANGES are invalid, they have to be ordered and not overlapping",
		Qtree_sitter_error);
  define_error (Qtree_sitter_size_error, "Buffer too large (> 4GB)",
		Qtree_sitter_error);
  define_error (Qtree_sitter_load_language_error,
		"Cannot load language definition",
		Qtree_sitter_error);

  DEFSYM (Qtree_sitter_parser_list, "tree-sitter-parser-list");
  DEFVAR_LISP ("tree-sitter-parser-list", Vtree_sitter_parser_list,
	       doc: /* A list of tree-sitter parsers.

If you removed a parser from this list, do not put it back in.  Emacs
keeps the parser in this list updated with any change in the buffer.
If removed and put back in, there is no guarantee that the parser is in
sync with the buffer's content.  */);
  Vtree_sitter_parser_list = Qnil;
  Fmake_variable_buffer_local (Qtree_sitter_parser_list);

  DEFVAR_LISP ("tree-sitter-load-name-list",
	       Vtree_sitter_load_name_list,
	       doc:
	       /* An override alist for irregular tree-sitter libraries.

By default, Emacs assumes the dynamic library for tree-sitter-<lang>
is libtree-sitter-<lang>.<ext>, where <ext> is the OS specific
extension for dynamic libraries.  Emacs also assumes that the name of
the C function the library provides is tree_sitter_<lang>. If that is
not the case, add an entry

    (LANGUAGE-SYMBOL LIBRARY-BASE-NAME C-SYMBOL-NAME)

to this alist, where LIBRARY-BASE-NAME is the filename of the dynamic
library without extension, C-SYMBOL-NAME is the C function that
returns a TSLanguage pointer.  */);
  Vtree_sitter_load_name_list = Qnil;

  defsubr (&Stree_sitter_parser_p);
  defsubr (&Stree_sitter_node_p);

  defsubr (&Stree_sitter_node_parser);

  defsubr (&Stree_sitter_parser_create);
  defsubr (&Stree_sitter_parser_buffer);
  defsubr (&Stree_sitter_parser_language);

  defsubr (&Stree_sitter_parser_root_node);
  /* defsubr (&Stree_sitter_parse_string); */

  defsubr (&Stree_sitter_parser_set_included_ranges);
  defsubr (&Stree_sitter_parser_included_ranges);

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

  defsubr (&Stree_sitter_query_capture);
}
