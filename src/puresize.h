/* How much read-only Lisp storage a dumped Emacs needs.
   Copyright (C) 1993, 2001-2024 Free Software Foundation, Inc.

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

#ifndef EMACS_PURESIZE_H
#define EMACS_PURESIZE_H

#include "lisp.h"

INLINE_HEADER_BEGIN

/* Number bytes of pure Lisp code to leave space for.  */

#ifndef SYSTEM_PURESIZE_EXTRA
#define SYSTEM_PURESIZE_EXTRA 0
#endif

#ifndef BASE_PURESIZE
#define BASE_PURESIZE (3000000 + SYSTEM_PURESIZE_EXTRA)
#endif

/* Increase BASE_PURESIZE by a ratio depending on the machine's word size.  */
#ifndef PURESIZE_RATIO
#if EMACS_INT_MAX >> 31 != 0
#if PTRDIFF_MAX >> 31 != 0
#define PURESIZE_RATIO 10 / 6	/* Don't surround with `()'.  */
#else
#define PURESIZE_RATIO 8 / 6	/* Don't surround with `()'.  */
#endif
#else
#define PURESIZE_RATIO 1
#endif
#endif

#ifndef PURESIZE
#define PURESIZE  (BASE_PURESIZE * PURESIZE_RATIO)
#endif

extern AVOID pure_write_error (Lisp_Object);

extern EMACS_INT pure[];

/* The puresize_h_* macros are private to this include file.  */

#define puresize_h_PURE_P(ptr) \
  ((uintptr_t) (ptr) - (uintptr_t) pure <= PURESIZE)

INLINE bool
PURE_P (void *ptr)
{
  return puresize_h_PURE_P (ptr);
}

/* Signal an error if OBJ is pure.  PTR is OBJ untagged.  */

#define puresize_h_CHECK_IMPURE(obj, ptr) \
  (PURE_P (ptr) ? pure_write_error (obj) : (void) 0)

INLINE void
CHECK_IMPURE (Lisp_Object obj, void *ptr)
{
  puresize_h_CHECK_IMPURE (obj, ptr);
}

INLINE_HEADER_END

#endif /* EMACS_PURESIZE_H */
