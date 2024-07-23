/* Elisp native compiler definitions

Copyright (C) 2019-2024 Free Software Foundation, Inc.

This file is NOT part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef COMP_H
#define COMP_H

#include <dynlib.h>

struct Lisp_Native_Comp_Unit
{
  union vectorlike_header header;
  Lisp_Object file;
  Lisp_Object optimize_qualities;
  /* Guard anonymous lambdas against Garbage Collection and serve
     sanity checks.  */
  Lisp_Object lambda_gc_guard_h;
  /* Hash c_name -> d_reloc_imp index.  */
  Lisp_Object lambda_c_name_idx_h;
  /* Hash doc-idx -> function documentation.  */
  Lisp_Object data_fdoc_v;
  /* Analogous to the constant vector but per compilation unit.  */
  Lisp_Object data_vec;
  /* 'data_impure_vec' must be last (see allocate_native_comp_unit).
     Same as data_vec but for data that cannot be moved to pure space.  */
  Lisp_Object data_impure_vec;
  /* STUFFS WE DO NOT DUMP!!  */
  Lisp_Object *data_imp_relocs;
  bool loaded_once;
  bool load_ongoing;
  dynlib_handle_ptr handle;
} GCALIGNED_STRUCT;

#ifdef HAVE_NATIVE_COMP

INLINE_HEADER_BEGIN

INLINE bool
NATIVE_COMP_UNITP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_NATIVE_COMP_UNIT);
}

INLINE struct Lisp_Native_Comp_Unit *
XNATIVE_COMP_UNIT (Lisp_Object a)
{
  eassert (NATIVE_COMP_UNITP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_Native_Comp_Unit);
}

/* Defined in comp.c.  */

extern void hash_native_abi (void);

extern Lisp_Object load_comp_unit (struct Lisp_Native_Comp_Unit *comp_u);
extern void unload_comp_unit (struct Lisp_Native_Comp_Unit *);

extern Lisp_Object native_function_doc (Lisp_Object function);

extern void syms_of_comp (void);

#else /* #ifdef HAVE_NATIVE_COMP */

static inline
void unload_comp_unit (struct Lisp_Native_Comp_Unit *cu)
{}

extern void syms_of_comp (void);

INLINE_HEADER_END

#endif /* #ifdef HAVE_NATIVE_COMP */

#endif /* #ifndef COMP_H */
