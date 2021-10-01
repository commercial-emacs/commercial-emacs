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

struct Lisp_Tree_Sitter
{
  union vectorlike_header header;
  Lisp_Object progmode;
  TSParser *parser;
  TSTree *tree;
};

INLINE bool
TREE_SITTER_P (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_TREE_SITTER);
}

INLINE struct Lisp_Tree_Sitter *
XTREE_SITTER (Lisp_Object a)
{
  eassert (TREE_SITTER_P (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_Tree_Sitter);
}

INLINE_HEADER_END

#endif /* EMACS_TREE_SITTER_H */
