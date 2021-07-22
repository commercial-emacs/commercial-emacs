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

/* parser.h defines a macro ADVANCE that conflicts with alloc.c.  */
#include <tree_sitter/parser.h>

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

/* Update each parser's tree after the user made an edit.  This
function does not parse the buffer and only updates the tree. (So it
should be very fast.)  */
void
ts_record_change (ptrdiff_t start_byte, ptrdiff_t old_end_byte,
		  ptrdiff_t new_end_byte)
{
  Lisp_Object parser_list = Fsymbol_value (Qtree_sitter_parser_list);
  TSPoint dummy_point = {0, 0};
  TSInputEdit edit = {start_byte, old_end_byte, new_end_byte,
		      dummy_point, dummy_point, dummy_point};
  while (!NILP (parser_list))
    {
      Lisp_Object lisp_parser = Fcar (parser_list);
      TSTree *tree = XTS_PARSER (lisp_parser)->tree;
      if (tree != NULL)
	ts_tree_edit (tree, &edit);
      XTS_PARSER (lisp_parser)->need_reparse = true;
      parser_list = Fcdr (parser_list);
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
  TSTree *new_tree = ts_parser_parse(ts_parser, tree, input);
  ts_tree_delete (tree);
  XTS_PARSER (parser)->tree = new_tree;
  XTS_PARSER (parser)->need_reparse = false;
}

/* This is the read function provided to tree-sitter to read from a
   buffer.  It reads one character at a time and automatically skip
   the gap.  */
const char*
ts_read_buffer (void *buffer, uint32_t byte_index,
		TSPoint position, uint32_t *bytes_read)
{
  if (!BUFFER_LIVE_P ((struct buffer *) buffer))
    error ("BUFFER is not live");

  ptrdiff_t byte_pos = byte_index + 1;

  /* Read one character.  Tree-sitter wants us to set bytes_read to 0
     if it reads to the end of buffer.  It doesn't say what it wants
     for the return value in that case, so we just give it an empty
     string.  */
  char *beg;
  int len;
  // TODO BUF_ZV_BYTE?
  if (byte_pos >= BUF_Z_BYTE ((struct buffer *) buffer))
    {
      beg = "";
      len = 0;
    }
  else
    {
      beg = (char *) BUF_BYTE_ADDRESS (buffer, byte_pos);
      len = next_char_len(byte_pos);
    }
  *bytes_read = (uint32_t) len;

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
  lisp_parser->need_reparse = true;
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
  CHECK_TS_PARSER (parser);
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

/* Below this point are uninteresting mechanical translations of
   tree-sitter API.  */

/* Node functions.  */

DEFUN ("tree-sitter-node-type",
       Ftree_sitter_node_type, Stree_sitter_node_type, 1, 1, 0,
       doc: /* Return the NODE's type as a symbol.  */)
  (Lisp_Object node)
{
  CHECK_TS_NODE (node);
  TSNode ts_node = XTS_NODE (node)->node;
  const char *type = ts_node_type(ts_node);
  return intern_c_string (type);
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

COND could be 'named, 'missing, 'extra, 'has-error.  Named nodes
correspond to named rules in the grammar, whereas "anonymous" nodes
correspond to string literals in the grammar.

Missing nodes are inserted by the parser in order to recover from
certain kinds of syntax errors, i.e., should be there but not there.

Extra nodes represent things like comments, which are not required the
grammar, but can appear anywhere.

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
  else
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

/* Query functions */

/* Initialize the tree-sitter routines.  */
void
syms_of_tree_sitter (void)
{
  DEFSYM (Qtree_sitter_parser_p, "tree-sitter-parser-p");
  DEFSYM (Qtree_sitter_node_p, "tree-sitter-node-p");
  DEFSYM (Qnamed, "named");
  DEFSYM (Qmissing, "missing");
  DEFSYM (Qextra, "extra");
  DEFSYM (Qhas_error, "has-error");

  DEFSYM (Qtree_sitter_parser_list, "tree-sitter-parser-list");
  DEFVAR_LISP ("ts-parser-list", Vtree_sitter_parser_list,
		     doc: /* A list of tree-sitter parsers.
// TODO: more doc.
If you removed a parser from this list, do not put it back in.  */);
  Vtree_sitter_parser_list = Qnil;
  Fmake_variable_buffer_local (Qtree_sitter_parser_list);

  defsubr (&Stree_sitter_parser_p);
  defsubr (&Stree_sitter_node_p);
  defsubr (&Stree_sitter_create_parser);
  defsubr (&Stree_sitter_parser_root_node);
  defsubr (&Stree_sitter_parse);

  defsubr (&Stree_sitter_node_type);
  defsubr (&Stree_sitter_node_string);
  defsubr (&Stree_sitter_node_parent);
  defsubr (&Stree_sitter_node_child);
  defsubr (&Stree_sitter_node_check);
  defsubr (&Stree_sitter_node_field_name_for_child);
  defsubr (&Stree_sitter_node_child_count);
  defsubr (&Stree_sitter_node_child_by_field_name);
  defsubr (&Stree_sitter_node_next_sibling);
  defsubr (&Stree_sitter_node_prev_sibling);
}
