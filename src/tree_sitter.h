/* Header file for the tree-sitter integration.

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

#ifndef EMACS_TREE_SITTER_H
#define EMACS_TREE_SITTER_H

#include "lisp.h"

#include <tree_sitter/api.h>

INLINE_HEADER_BEGIN

/* A wrapper for a tree-sitter parser, but also contains a parse tree
   and other goodies for convenience.  */
struct Lisp_TS_Parser
{
  union vectorlike_header header;
  /* A parser's name is just a convenient tag, see docstring for
     'tree-sitter-make-parser', and 'tree-sitter-get-parser'. */
  Lisp_Object name;
  struct buffer *buffer;
  TSParser *parser;
  TSTree *tree;
  TSInput input;
  /* Re-parsing an unchanged buffer is not free for tree-sitter, so we
     only make it re-parse when need_reparse == true.  That usually
     means some change is made in the buffer.  But others could set
     this field to true to force tree-sitter to re-parse.  */
  bool need_reparse;
  /* This two positions record the byte position of the "visible
     region" that tree-sitter sees.  Unlike markers, These two
     positions do not change as the user inserts and deletes text
     around them. Before re-parse, we move these positions to match
     BUF_BEGV_BYTE and BUF_ZV_BYTE.  */
  ptrdiff_t visible_beg;
  ptrdiff_t visible_end;
};

/* A wrapper around a tree-sitter node.  */
struct Lisp_TS_Node
{
  union vectorlike_header header;
  /* This should prevent the gc from collecting the parser before the
     node is done with it.  TSNode contains a pointer to the tree it
     belongs to, and the parser object, when collected by gc, will
     free that tree. */
  Lisp_Object parser;
  TSNode node;
};

INLINE bool
TS_PARSERP (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_TS_PARSER);
}

INLINE struct Lisp_TS_Parser *
XTS_PARSER (Lisp_Object a)
{
  eassert (TS_PARSERP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_TS_Parser);
}

INLINE bool
TS_NODEP (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_TS_NODE);
}

INLINE struct Lisp_TS_Node *
XTS_NODE (Lisp_Object a)
{
  eassert (TS_NODEP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_TS_Node);
}

INLINE void
CHECK_TS_PARSER (Lisp_Object parser)
{
  CHECK_TYPE (TS_PARSERP (parser), Qtree_sitter_parser_p, parser);
}

INLINE void
CHECK_TS_NODE (Lisp_Object node)
{
  CHECK_TYPE (TS_NODEP (node), Qtree_sitter_node_p, node);
}

void
ts_record_change (ptrdiff_t start_byte, ptrdiff_t old_end_byte,
		  ptrdiff_t new_end_byte);

Lisp_Object
make_ts_parser (struct buffer *buffer, TSParser *parser,
		TSTree *tree, Lisp_Object name);

Lisp_Object
make_ts_node (Lisp_Object parser, TSNode node);

extern void syms_of_tree_sitter (void);

INLINE_HEADER_END

#endif /* EMACS_TREE_SITTER_H */
