/* Header file for the tree-sitter integration.

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

#ifndef EMACS_TREE_SITTER_H
#define EMACS_TREE_SITTER_H

#include "lisp.h"

#include <tree_sitter/api.h>

INLINE_HEADER_BEGIN

struct Lisp_TS
{
  union vectorlike_header header;
  /* A symbol represents the language this parser uses.  It should be
   the symbol of the function provided by a language dynamic
   module.  */
  Lisp_Object buffer;
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

void
tree_sitter_record_change (ptrdiff_t start_byte, ptrdiff_t old_end_byte,
			   ptrdiff_t new_end_byte);

Lisp_Object
make_tree_sitter_parser (Lisp_Object buffer, TSParser *parser,
			 TSTree *tree, Lisp_Object language_symbol);

Lisp_Object
make_tree_sitter_node (Lisp_Object parser, TSNode node);

extern void syms_of_tree_sitter (void);

INLINE_HEADER_END

#endif /* EMACS_TREE_SITTER_H */
