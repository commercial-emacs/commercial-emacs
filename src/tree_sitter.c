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

#include "buffer.h"
#include "coding.h"
#include "tree_sitter.h"

/* parser.h defines a macro ADVANCE that conflicts with alloc.c.   */
#include <tree_sitter/parser.h>

Lisp_Object
make_ts_parser (struct buffer *buffer, TSParser *parser, TSTree *tree)
{
  struct Lisp_TS_Parser *lisp_parser
    = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_TS_Parser, PVEC_TS_PARSER);
  lisp_parser->buffer = buffer;
  lisp_parser->parser = parser;
  lisp_parser->tree = tree;
  // TODO TSInput.
  return make_lisp_ptr (lisp_parser, Lisp_Vectorlike);
}

Lisp_Object
make_ts_node (Lisp_Object parser, TSNode node)
{
  struct Lisp_TS_Node *lisp_node
    = ALLOCATE_PSEUDOVECTOR (struct Lisp_TS_Node, parser, PVEC_TS_NODE);
  lisp_node->parser = parser;
  lisp_node->node = node;
  return make_lisp_ptr (lisp_node, Lisp_Vectorlike);
}


/* Tree-sitter parser.  */

DEFUN ("tree-sitter-parse", Ftree_sitter_parse, Stree_sitter_parse,
       2, 2, 0,
       doc: /* Parse STRING and return a parser object.
LANGUAGE should be the language provided by a tree-sitter language
dynamic module.  */)
  (Lisp_Object string, Lisp_Object language)
{
  CHECK_STRING (string);

  /* LANGUAGE is a USER_PTR that contains the pointer to a
     TSLanguage struct.  */
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
Return nil if couldn't find any.  */)
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
Return nil if couldn't find any.  */)
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
  defsubr (&Stree_sitter_parse);
  defsubr (&Stree_sitter_node_string);
  defsubr (&Stree_sitter_node_parent);
  defsubr (&Stree_sitter_node_child);
}
