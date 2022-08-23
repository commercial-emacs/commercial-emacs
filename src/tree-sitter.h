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
#include "buffer.h"

#include <tree_sitter/api.h>
#include <tree_sitter/highlight.h>

INLINE_HEADER_BEGIN

#define BUFFER_TO_SITTER(pos) ((uint32_t) CHAR_TO_BYTE (pos) - 1)
#define SITTER_TO_BUFFER(byte) (BYTE_TO_CHAR ((EMACS_INT) byte + 1))

struct Lisp_Tree_Sitter_Node
{
  union vectorlike_header header;
  TSNode node;
} GCALIGNED_STRUCT;

struct Lisp_Tree_Sitter_Cursor
{
  union vectorlike_header header;
  TSTreeCursor cursor;
} GCALIGNED_STRUCT;

struct Lisp_Tree_Sitter
{
  union vectorlike_header header;
  Lisp_Object progmode;
  TSParser *parser;
  TSTree *prev_tree;
  TSTree *tree;
  TSHighlighter *highlighter;
  const char **highlight_names;
  char *highlights_query;
  TSQuery *indents_query;
} GCALIGNED_STRUCT;

INLINE bool
TREE_SITTERP (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_TREE_SITTER);
}

INLINE struct Lisp_Tree_Sitter *
XTREE_SITTER (Lisp_Object x)
{
  eassert (TREE_SITTERP (x));
  return XUNTAG (x, Lisp_Vectorlike, struct Lisp_Tree_Sitter);
}

INLINE void
CHECK_TREE_SITTER (Lisp_Object x)
{
  CHECK_TYPE (TREE_SITTERP (x), Qtree_sitterp, x);
}

INLINE bool
TREE_SITTER_NODEP (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_TREE_SITTER_NODE);
}

INLINE bool
TREE_SITTER_CURSORP (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_TREE_SITTER_CURSOR);
}

INLINE struct Lisp_Tree_Sitter_Cursor *
XTREE_SITTER_CURSOR (Lisp_Object x)
{
  eassert (TREE_SITTER_CURSORP (x));
  return XUNTAG (x, Lisp_Vectorlike, struct Lisp_Tree_Sitter_Cursor);
}

INLINE struct Lisp_Tree_Sitter_Node *
XTREE_SITTER_NODE (Lisp_Object x)
{
  eassert (TREE_SITTER_NODEP (x));
  return XUNTAG (x, Lisp_Vectorlike, struct Lisp_Tree_Sitter_Node);
}

INLINE void
CHECK_TREE_SITTER_NODE (Lisp_Object x)
{
  CHECK_TYPE (TREE_SITTER_NODEP (x), Qtree_sitter_nodep, x);
}

INLINE void
CHECK_TREE_SITTER_CURSOR (Lisp_Object x)
{
  CHECK_TYPE (TREE_SITTER_CURSORP (x), Qtree_sitter_cursorp, x);
}

INLINE_HEADER_END

#endif /* EMACS_TREE_SITTER_H */
