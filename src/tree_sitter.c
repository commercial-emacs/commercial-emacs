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
#include "tree_sitter.h"

/* parser.h defines a macro ADVANCE that conflicts with alloc.c.  */
#include <tree_sitter/parser.h>

/*** Functions related to parser and node object.  */

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

/*** Parsing functions */

static inline void
ts_tree_edit_1 (TSTree *tree, ptrdiff_t start_byte,
		ptrdiff_t old_end_byte, ptrdiff_t new_end_byte)
{
  TSPoint dummy_point = {0, 0};
  TSInputEdit edit = {start_byte, old_end_byte, new_end_byte,
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
  eassert(start_byte <= old_end_byte);
  eassert(start_byte <= new_end_byte);

  Lisp_Object parser_list = Fsymbol_value (Qtree_sitter_parser_list);

  while (!NILP (parser_list))
    {
      Lisp_Object lisp_parser = Fcar (parser_list);
      TSTree *tree = XTS_PARSER (lisp_parser)->tree;
      if (tree != NULL)
	{
	  /* We "clip" the change to between visible_beg and
	     visible_end.  It is okay if visible_end ends up larger
	     than BUF_Z, tree-sitter only access buffer text during
	     re-parse, and we will adjust visible_beg/end before
	     re-parse.  */
	  ptrdiff_t visible_beg = XTS_PARSER (lisp_parser)->visible_beg;
	  ptrdiff_t visible_end = XTS_PARSER (lisp_parser)->visible_end;

	  ptrdiff_t visible_start =
	    max (visible_beg, start_byte) - visible_beg;
	  ptrdiff_t visible_old_end =
	    min (visible_end, old_end_byte) - visible_beg;
	  ptrdiff_t visible_new_end =
	    min (visible_end, new_end_byte) - visible_beg;

	  ts_tree_edit_1 (tree, visible_start, visible_old_end,
			  visible_new_end);
	  XTS_PARSER (lisp_parser)->need_reparse = true;

	  parser_list = Fcdr (parser_list);
	}
    }
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
  struct buffer *buffer = XTS_PARSER (parser)->buffer;

  /* Before we parse, catch up with the narrowing situation.  We
     change visible_beg and visible_end to match BUF_BEGV_BYTE and
     BUF_ZV_BYTE, and inform tree-sitter of the change.  */
  ptrdiff_t visible_beg = XTS_PARSER (parser)->visible_beg;
  ptrdiff_t visible_end = XTS_PARSER (parser)->visible_end;
  /* Before re-parse, we want to move the visible range of tree-sitter
     to matched the narrowed range. For example:
     Move ________|____|__
     to   |____|__________ */

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
  XTS_PARSER (parser)->visible_beg = visible_beg;
  XTS_PARSER (parser)->visible_end = visible_end;

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
      XSETBUFFER(buf, buffer);
      xsignal1 (Qtree_sitter_parse_error, buf);
    }

  ts_tree_delete (tree);
  XTS_PARSER (parser)->tree = new_tree;
  XTS_PARSER (parser)->need_reparse = false;
  TSNode node = ts_tree_root_node (new_tree);
}

/* This is the read function provided to tree-sitter to read from a
   buffer.  It reads one character at a time and automatically skips
   the gap.  */
const char*
ts_read_buffer (void *parser, uint32_t byte_index,
		TSPoint position, uint32_t *bytes_read)
{
  struct buffer *buffer = ((struct Lisp_TS_Parser *) parser)->buffer;
  ptrdiff_t visible_beg = ((struct Lisp_TS_Parser *) parser)->visible_beg;
  ptrdiff_t byte_pos = byte_index + visible_beg;
  /* We will make sure visible_beg >= BUF_BEG_BYTE before re-parse (in
     ts_ensure_parsed), so byte_pos will never be smaller than
     BUF_BEG_BYTE (unless byte_index < 0).  */

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
      beg = "";
      len = 0;
    }
  /* Reached visible end-of-buffer, tell tree-sitter to read no more.  */
  else if (byte_pos >= BUF_ZV_BYTE (buffer))
    {
      beg = "";
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

/*** Creators and accessors for parser and node */

/* Wrap the parser in a Lisp_Object to be used in the Lisp machine.  */
Lisp_Object
make_ts_parser (struct buffer *buffer, TSParser *parser,
		TSTree *tree, Lisp_Object name)
{
  struct Lisp_TS_Parser *lisp_parser
    = ALLOCATE_PSEUDOVECTOR (struct Lisp_TS_Parser, name, PVEC_TS_PARSER);

  lisp_parser->name = name;
  lisp_parser->buffer = buffer;
  lisp_parser->parser = parser;
  lisp_parser->tree = tree;
  TSInput input = {lisp_parser, ts_read_buffer, TSInputEncodingUTF8};
  lisp_parser->input = input;
  lisp_parser->need_reparse = true;
  lisp_parser->visible_beg = BUF_BEGV (buffer);
  lisp_parser->visible_end = BUF_ZV (buffer);
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

DEFUN ("tree-sitter-node-parser",
       Ftree_sitter_node_parser, Stree_sitter_node_parser,
       1, 1, 0,
       doc: /* Return the parser to which NODE belongs.  */)
  (Lisp_Object node)
{
  CHECK_TS_NODE (node);
  return XTS_NODE (node)->parser;
}

DEFUN ("tree-sitter-create-parser",
       Ftree_sitter_create_parser, Stree_sitter_create_parser,
       2, 3, 0,
       doc: /* Create and return a parser in BUFFER for LANGUAGE.

The parser is automatically added to BUFFER's
`tree-sitter-parser-list'.  LANGUAGE should be the language provided
by a tree-sitter language dynamic module.

NAME (a string) is the name assigned to the parser, like the name for
a process.  Unlike process names, not care is taken to make each
parser's name unique.  By default, no name is assigned to the parser;
the only consequence of that is you can't use
`tree-sitter-get-parser' to find the parser by its name.  */)
  (Lisp_Object buffer, Lisp_Object language, Lisp_Object name)
{
  CHECK_BUFFER(buffer);
  if (!NILP (name))
    CHECK_STRING (name);

  /* LANGUAGE is a USER_PTR that contains the pointer to a TSLanguage
     struct.  */
  TSParser *parser = ts_parser_new ();
  TSLanguage *lang = (XUSER_PTR (language)->p);
  ts_parser_set_language (parser, lang);

  Lisp_Object lisp_parser
    = make_ts_parser (XBUFFER(buffer), parser, NULL, name);

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
  XSETBUFFER (buf, XTS_PARSER (parser)->buffer);
  return buf;
}

DEFUN ("tree-sitter-parser-name",
       Ftree_sitter_parser_name, Stree_sitter_parser_name,
       1, 1, 0,
       doc: /* Return parser's name.  */)
  (Lisp_Object parser)
{
  CHECK_TS_PARSER (parser);
  return XTS_PARSER (parser)->name;
}

/*** Parser API */

DEFUN ("tree-sitter-parser-root-node",
       Ftree_sitter_parser_root_node, Stree_sitter_parser_root_node,
       1, 1, 0,
       doc: /* Return the root node of PARSER.  */)
  (Lisp_Object parser)
{
  CHECK_TS_PARSER (parser);
  ts_ensure_parsed(parser);
  TSNode root_node = ts_tree_root_node (XTS_PARSER (parser)->tree);
  return make_ts_node (parser, root_node);
}

DEFUN ("tree-sitter-parse-string",
       Ftree_sitter_parse_string, Stree_sitter_parse_string,
       2, 2, 0,
       doc: /* Parse STRING and return the root node.
LANGUAGE should be the language provided by a tree-sitter language
dynamic module.  */)
  (Lisp_Object string, Lisp_Object language)
{
  CHECK_STRING (string);

  /* LANGUAGE is a USER_PTR that contains the pointer to a TSLanguage
     struct.  */
  TSParser *parser = ts_parser_new ();
  TSLanguage *lang = (XUSER_PTR (language)->p);
  ts_parser_set_language (parser, lang);

  TSTree *tree = ts_parser_parse_string (parser, NULL,
					 SSDATA (string),
					 strlen (SSDATA (string)));

  /* See comment in ts_ensure_parsed for possible reasons for a
     failure.  */
  if (tree == NULL)
    xsignal1 (Qtree_sitter_parse_error, string);

  TSNode root_node = ts_tree_root_node (tree);

  Lisp_Object lisp_parser = make_ts_parser (NULL, parser, tree, Qnil);
  Lisp_Object lisp_node = make_ts_node (lisp_parser, root_node);

  return lisp_node;
}

/*** Node API  */

DEFUN ("tree-sitter-node-type",
       Ftree_sitter_node_type, Stree_sitter_node_type, 1, 1, 0,
       doc: /* Return the NODE's type as a symbol.  */)
  (Lisp_Object node)
{
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  const char *type = ts_node_type(ts_node);
  // TODO: Maybe return a string instead.
  return intern_c_string (type);
}

DEFUN ("tree-sitter-node-start-byte",
       Ftree_sitter_node_start_byte, Stree_sitter_node_start_byte, 1, 1, 0,
       doc: /* Return the NODE's start byte position.  */)
  (Lisp_Object node)
{
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  uint32_t start_byte = ts_node_start_byte(ts_node);
  return make_fixnum(start_byte + 1);
}

DEFUN ("tree-sitter-node-end-byte",
       Ftree_sitter_node_end_byte, Stree_sitter_node_end_byte, 1, 1, 0,
       doc: /* Return the NODE's end byte position.  */)
  (Lisp_Object node)
{
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  uint32_t end_byte = ts_node_end_byte(ts_node);
  return make_fixnum(end_byte + 1);
}

DEFUN ("tree-sitter-node-string",
       Ftree_sitter_node_string, Stree_sitter_node_string, 1, 1, 0,
       doc: /* Return the string representation of NODE.  */)
  (Lisp_Object node)
{
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  char *string = ts_node_string(ts_node);
  return make_string(string, strlen (string));
}

DEFUN ("tree-sitter-node-parent",
       Ftree_sitter_node_parent, Stree_sitter_node_parent, 1, 1, 0,
       doc: /* Return the immediate parent of NODE.
Return nil if there isn't any.  */)
  (Lisp_Object node)
{
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode parent = ts_node_parent (ts_node);

  if (ts_node_is_null(parent))
    return Qnil;

  return make_ts_node (XTS_NODE (node)->parser, parent);
}

DEFUN ("tree-sitter-node-child",
       Ftree_sitter_node_child, Stree_sitter_node_child, 2, 3, 0,
       doc: /* Return the Nth child of NODE.
Return nil if there isn't any.  If NAMED is non-nil, look for named
child only.  NAMED defaults to nil.  */)
  (Lisp_Object node, Lisp_Object n, Lisp_Object named)
{
  CHECK_TS_NODE (node);
  CHECK_INTEGER (n);
  EMACS_INT idx = XFIXNUM (n);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode child;
  if (NILP (named))
    child = ts_node_child (ts_node, (uint32_t) idx);
  else
    child = ts_node_named_child (ts_node, (uint32_t) idx);

  if (ts_node_is_null(child))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, child);
}

DEFUN ("tree-sitter-node-check",
       Ftree_sitter_node_check, Stree_sitter_node_check, 2, 2, 0,
       doc: /* Return non-nil if NODE is in condition COND, nil otherwise.

COND could be 'named, 'missing, 'extra, 'has-changes, 'has-error.
Named nodes correspond to named rules in the grammar, whereas
"anonymous" nodes correspond to string literals in the grammar.

Missing nodes are inserted by the parser in order to recover from
certain kinds of syntax errors, i.e., should be there but not there.

Extra nodes represent things like comments, which are not required the
grammar, but can appear anywhere.

A node "has changes" if the buffer changed since the node is
created. (Don't forget the "s" at the end of 'has-changes.)

A node "has error" if itself is a syntax error or contains any syntax
errors.  */)
  (Lisp_Object node, Lisp_Object cond)
{
  CHECK_TS_NODE (node);
  CHECK_SYMBOL (cond);
  TSNode ts_node = XTS_NODE (node)->node;
  bool result;
  if (EQ (cond, Qnamed))
    result = ts_node_is_named (ts_node);
  else if (EQ (cond, Qmissing))
    result = ts_node_is_missing (ts_node);
  else if (EQ (cond, Qextra))
    result = ts_node_is_extra (ts_node);
  else if (EQ (cond, Qhas_error))
    result = ts_node_has_error (ts_node);
  else if (EQ (cond, Qhas_changes))
    result = ts_node_has_changes (ts_node);
  else
    // TODO: Is this a good error message?
    signal_error ("Expecting one of four symbols, see docstring", cond);
  return result ? Qt : Qnil;
}

DEFUN ("tree-sitter-node-field-name-for-child",
       Ftree_sitter_node_field_name_for_child,
       Stree_sitter_node_field_name_for_child, 2, 2, 0,
       doc: /* Return the field name of the Nth child of NODE.
Return nil if there isn't any child or no field is found.  */)
  (Lisp_Object node, Lisp_Object n)
{
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
nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
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
       doc: /* Return the child of NODE with field name NAME.
Return nil if there isn't any.  */)
  (Lisp_Object node, Lisp_Object name)
{
  CHECK_STRING (name);
  char *name_str = SSDATA (name);
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
child only.  NAMED defaults to nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
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
child only.  NAMED defaults to nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
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

DEFUN ("tree-sitter-node-first-child-for-byte",
       Ftree_sitter_node_first_child_for_byte,
       Stree_sitter_node_first_child_for_byte, 2, 3, 0,
       doc: /* Return the first child of NODE on POS.
Specifically, return the first child that extends beyond POS.  POS is
a byte position in the buffer counting from 1.  Return nil if there
isn't any.  If NAMED is non-nil, look for named child only.  NAMED
defaults to nil.  Note that this function returns an immediate child,
not the smallest (grand)child.  */)
  (Lisp_Object node, Lisp_Object pos, Lisp_Object named)
{
  CHECK_INTEGER (pos);

  struct buffer *buf = XTS_PARSER (XTS_NODE (node)->parser)->buffer;
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  ptrdiff_t byte_pos = XFIXNUM (pos);

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

DEFUN ("tree-sitter-node-descendant-for-byte-range",
       Ftree_sitter_node_descendant_for_byte_range,
       Stree_sitter_node_descendant_for_byte_range, 3, 4, 0,
       doc: /* Return the smallest node that covers BEG to END.
The returned node is a descendant of NODE.  POS is a byte position
counting from 1.  Return nil if there isn't any.  If NAMED is non-nil,
look for named child only.  NAMED defaults to nil.  */)
  (Lisp_Object node, Lisp_Object beg, Lisp_Object end, Lisp_Object named)
{
  CHECK_INTEGER (beg);
  CHECK_INTEGER (end);

  struct buffer *buf = XTS_PARSER (XTS_NODE (node)->parser)->buffer;
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  ptrdiff_t byte_beg = XFIXNUM (beg);
  ptrdiff_t byte_end = XFIXNUM (end);

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

/* Query functions */

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
manual for further explanation for how to write a match pattern.

BEG and END, if _both_ non-nil, specifies the range in which the query
is executed.

Raise an tree-sitter-query-error if PATTERN is malformed.  */)
  (Lisp_Object node, Lisp_Object pattern,
   Lisp_Object beg, Lisp_Object end)
{
  CHECK_TS_NODE (node);
  CHECK_STRING (pattern);

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
      // FIXME: Still crashes, debug when I can get a gdb.
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
  TSQueryCapture capture;
  Lisp_Object result = Qnil;
  Lisp_Object entry;
  Lisp_Object captured_node;
  const char *capture_name;
  uint32_t capture_name_len;
  while (ts_query_cursor_next_match (cursor, &match))
    {
      const TSQueryCapture *captures = match.captures;
      for (int idx=0; idx < match.capture_count; idx++)
	{
	  capture = captures[idx];
	  captured_node = make_ts_node(lisp_parser, capture.node);
	  capture_name = ts_query_capture_name_for_id
	    (query, capture.index, &capture_name_len);
	  entry = Fcons (intern_c_string (capture_name),
			 captured_node);
	  result = Fcons (entry, result);
	}
    }
  ts_query_delete (query);
  ts_query_cursor_delete (cursor);
  return Freverse (result);
}

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
  DEFSYM (Qtree_sitter_parse_error, "tree-sitter-parse-error")
  define_error (Qtree_sitter_error, "Generic tree-sitter error", Qerror);
  define_error (Qtree_sitter_query_error, "Query pattern is malformed",
		Qtree_sitter_error);
  define_error (Qtree_sitter_parse_error, "Parse failed",
		Qtree_sitter_error);


  DEFSYM (Qtree_sitter_parser_list, "tree-sitter-parser-list");
  DEFVAR_LISP ("tree-sitter-parser-list", Vtree_sitter_parser_list,
	       doc: /* A list of tree-sitter parsers.
// TODO: more doc.
If you removed a parser from this list, do not put it back in.  */);
  Vtree_sitter_parser_list = Qnil;
  Fmake_variable_buffer_local (Qtree_sitter_parser_list);

  defsubr (&Stree_sitter_parser_p);
  defsubr (&Stree_sitter_node_p);

  defsubr (&Stree_sitter_node_parser);

  defsubr (&Stree_sitter_create_parser);
  defsubr (&Stree_sitter_parser_buffer);
  defsubr (&Stree_sitter_parser_name);

  defsubr (&Stree_sitter_parser_root_node);
  defsubr (&Stree_sitter_parse_string);

  defsubr (&Stree_sitter_node_type);
  defsubr (&Stree_sitter_node_start_byte);
  defsubr (&Stree_sitter_node_end_byte);
  defsubr (&Stree_sitter_node_string);
  defsubr (&Stree_sitter_node_parent);
  defsubr (&Stree_sitter_node_child);
  defsubr (&Stree_sitter_node_check);
  defsubr (&Stree_sitter_node_field_name_for_child);
  defsubr (&Stree_sitter_node_child_count);
  defsubr (&Stree_sitter_node_child_by_field_name);
  defsubr (&Stree_sitter_node_next_sibling);
  defsubr (&Stree_sitter_node_prev_sibling);
  defsubr (&Stree_sitter_node_first_child_for_byte);
  defsubr (&Stree_sitter_node_descendant_for_byte_range);

  defsubr (&Stree_sitter_query_capture);
}
