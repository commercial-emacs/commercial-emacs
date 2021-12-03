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

#define BUFFER_TO_SITTER(byte) ((uint32_t) CHAR_TO_BYTE (byte) - 1)
#define SITTER_TO_BUFFER(byte) (BYTE_TO_CHAR ((EMACS_INT) byte + 1))

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
};

INLINE bool
TREE_SITTERP (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_TREE_SITTER);
}

INLINE struct Lisp_Tree_Sitter *
XTREE_SITTER (Lisp_Object a)
{
  eassert (TREE_SITTERP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_Tree_Sitter);
}

INLINE void
CHECK_TREE_SITTER (Lisp_Object sitter)
{
  CHECK_TYPE (TREE_SITTERP (sitter), Qtree_sitterp, sitter);
}

INLINE_HEADER_END

#endif /* EMACS_TREE_SITTER_H */
