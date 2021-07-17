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

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "lisp.h"
#include "buffer.h"
#include "coding.h"
#include "tree_sitter.h"

/* parser.h defines a macro ADVANCE that conflicts with alloc.c.   */
#include <tree_sitter/parser.h>

/* Record the byte position of the end of the (to-be) changed text.
We have to record it now, because by the time we get to after-change
hook, the _byte_ position of the end is lost.  */
void
ts_before_change (ptrdiff_t start_int, ptrdiff_t end_int)
{
  /* Iterate through each parser in 'tree-sitter-parser-list' and
     record the byte position.  There could be better ways to record
     it than storing the same position in every parser, but this is
     the most fool-proof way, and I expect a buffer to have only one
     parser most of the time anyway. */
  ptrdiff_t beg_byte = CHAR_TO_BYTE (start_int);
  ptrdiff_t old_end_byte = CHAR_TO_BYTE (end_int);
  Lisp_Object parser_list = Fsymbol_value (Qtree_sitter_parser_list);
  while (!NILP (parser_list))
    {
      Lisp_Object lisp_parser = Fcar (parser_list);
      XTS_PARSER (lisp_parser)->edit.start_byte = beg_byte;
      XTS_PARSER (lisp_parser)->edit.old_end_byte = old_end_byte;
      parser_list = Fcdr (parser_list);
    }
}

/* Update each parser's tree after the user made an edit.  This
function does not parse the buffer and only updates the tree. (So it
should be very fast.)  */
void
ts_after_change (ptrdiff_t charpos, ptrdiff_t lendel, ptrdiff_t lenins)
{
  ptrdiff_t new_end_byte = CHAR_TO_BYTE (charpos + lenins);
  Lisp_Object parser_list = Fsymbol_value (Qtree_sitter_parser_list);
  while (!NILP (parser_list))
    {
      Lisp_Object lisp_parser = Fcar (parser_list);
      TSTree *tree = XTS_PARSER (lisp_parser)->tree;
      XTS_PARSER (lisp_parser)->edit.new_end_byte = new_end_byte;
      if (tree != NULL)
	  ts_tree_edit (tree, &XTS_PARSER (lisp_parser)->edit);
      parser_list = Fcdr (parser_list);
    }
}

/* Parse the buffer.  We don't parse until we have to. When we have
to, we call this function to parse and update the tree.  */
void
ts_ensure_parsed (Lisp_Object parser)
{
  TSParser *ts_parser = XTS_PARSER (parser)->parser;
  TSTree *tree = XTS_PARSER(parser)->tree;
  TSInput input = XTS_PARSER (parser)->input;
  TSTree *new_tree = ts_parser_parse(ts_parser, tree, input);
  XTS_PARSER (parser)->tree = new_tree;
}

/* This is the read function provided to tree-sitter to read from a
   buffer.  It reads one character at a time and automatically skip
   the gap.  */
const char*
ts_read_buffer (void *buffer, uint32_t byte_index,
		TSPoint position, uint32_t *bytes_read)
{
  if (! BUFFER_LIVE_P ((struct buffer *) buffer))
    error ("BUFFER is not live");

  ptrdiff_t byte_pos = byte_index + 1;

  // FIXME: Add some boundary checks?
  /* I believe we can get away with only setting current-buffer
     and not actually switching to it, like what we did in
     'make_gap_1'.  */
  struct buffer *old_buffer = current_buffer;
  current_buffer = (struct buffer *) buffer;

  /* Read one character.  */
  char *beg;
  int len;
  if (byte_pos >= Z_BYTE)
    {
      beg = "";
      len = 0;
    }
  else
    {
      beg = (char *) BYTE_POS_ADDR (byte_pos);
      len = next_char_len(byte_pos);
    }
  *bytes_read = (uint32_t) len;
  current_buffer = old_buffer;
  return beg;
}

/* Wrap the parser in a Lisp_Object to be used in the Lisp machine.  */
Lisp_Object
make_ts_parser (struct buffer *buffer, TSParser *parser, TSTree *tree)
{
  struct Lisp_TS_Parser *lisp_parser
    = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_TS_Parser, PVEC_TS_PARSER);
  lisp_parser->buffer = buffer;
  lisp_parser->parser = parser;
  lisp_parser->tree = tree;
  TSInput input = {buffer, ts_read_buffer, TSInputEncodingUTF8};
  lisp_parser->input = input;
  TSPoint dummy_point = {0, 0};
  TSInputEdit edit = {0, 0, 0, dummy_point, dummy_point, dummy_point};
  lisp_parser->edit = edit;
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


DEFUN ("tree-sitter-create-parser",
       Ftree_sitter_create_parser, Stree_sitter_create_parser,
       2, 2, 0,
       doc: /* Create and return a parser in BUFFER for LANGUAGE.
The parser is automatically added to BUFFER's
`tree-sitter-parser-list'.  LANGUAGE should be the language provided
by a tree-sitter language dynamic module.  */)
  (Lisp_Object buffer, Lisp_Object language)
{
  CHECK_BUFFER(buffer);

  /* LANGUAGE is a USER_PTR that contains the pointer to a TSLanguage
     struct.  */
  TSParser *parser = ts_parser_new ();
  TSLanguage *lang = (XUSER_PTR (language)->p);
  ts_parser_set_language (parser, lang);

  Lisp_Object lisp_parser
    = make_ts_parser (XBUFFER(buffer), parser, NULL);

  // FIXME: Is this the correct way to set a buffer-local variable?
  struct buffer *old_buffer = current_buffer;
  set_buffer_internal (XBUFFER (buffer));

  Fset (Qtree_sitter_parser_list,
	Fcons (lisp_parser, Fsymbol_value (Qtree_sitter_parser_list)));

  set_buffer_internal (old_buffer);
  return lisp_parser;
}

DEFUN ("tree-sitter-parser-root-node",
       Ftree_sitter_parser_root_node, Stree_sitter_parser_root_node,
       1, 1, 0,
       doc: /* Return the root node of PARSER.  */)
  (Lisp_Object parser)
{
  ts_ensure_parsed(parser);
  TSNode root_node = ts_tree_root_node (XTS_PARSER (parser)->tree);
  return make_ts_node (parser, root_node);
}

DEFUN ("tree-sitter-parse", Ftree_sitter_parse, Stree_sitter_parse,
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

  /* See comment for ts_parser_parse in tree_sitter/api.h
     for possible reasons for a failure.  */
  if (tree == NULL)
    signal_error ("Failed to parse STRING", string);

  TSNode root_node = ts_tree_root_node (tree);

  Lisp_Object lisp_parser = make_ts_parser (NULL, parser, tree);
  Lisp_Object lisp_node = make_ts_node (lisp_parser, root_node);

  return lisp_node;
}

DEFUN ("tree-sitter-node-string",
       Ftree_sitter_node_string, Stree_sitter_node_string, 1, 1, 0,
       doc: /* Return the string representation of NODE.  */)
  (Lisp_Object node)
{
  TSNode ts_node = XTS_NODE (node)->node;
  char *string = ts_node_string(ts_node);
  return make_string(string, strlen (string));
}

DEFUN ("tree-sitter-node-parent",
       Ftree_sitter_node_parent, Stree_sitter_node_parent, 1, 1, 0,
       doc: /* Return the immediate parent of NODE.
Return nil if we couldn't find any.  */)
  (Lisp_Object node)
{
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode parent = ts_node_parent(ts_node);

  if (ts_node_is_null(parent))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, parent);
}

DEFUN ("tree-sitter-node-child",
       Ftree_sitter_node_child, Stree_sitter_node_child, 2, 2, 0,
       doc: /* Return the Nth child of NODE.
Return nil if we couldn't find any.  */)
  (Lisp_Object node, Lisp_Object n)
{
  CHECK_INTEGER (n);
  EMACS_INT idx = XFIXNUM (n);
  TSNode ts_node = XTS_NODE (node)->node;
  // FIXME: Is this cast ok?
  TSNode child = ts_node_child(ts_node, (uint32_t) idx);

  if (ts_node_is_null(child))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, child);
}

/* Initialize the tree-sitter routines.  */
void
syms_of_tree_sitter (void)
{
  DEFSYM (Qtree_sitter_parser_list, "tree-sitter-parser-list");
  DEFVAR_LISP ("ts-parser-list", Vtree_sitter_parser_list,
		     doc: /* A list of tree-sitter parsers.
// TODO: more doc.
If you removed a parser from this list, do not put it back in.  */);
  Vtree_sitter_parser_list = Qnil;
  Fmake_variable_buffer_local (Qtree_sitter_parser_list);


  defsubr (&Stree_sitter_create_parser);
  defsubr (&Stree_sitter_parser_root_node);
  defsubr (&Stree_sitter_parse);
  defsubr (&Stree_sitter_node_string);
  defsubr (&Stree_sitter_node_parent);
  defsubr (&Stree_sitter_node_child);
}
