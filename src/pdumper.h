/* Header file for the portable dumper.

Copyright (C) 2016, 2018-2024 Free Software Foundation, Inc.

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

#ifndef EMACS_PDUMPER_H
#define EMACS_PDUMPER_H

#include <stdio.h>
#include "fingerprint.h"
#include "lisp.h"
#include "bitset.h"

INLINE_HEADER_BEGIN

enum { PDUMPER_NO_OBJECT = -1 };

/* "Remember" functions preserve auxiliary C variables not pointed to by
   Lisp heap (as one does in staticpro).  */

extern void pdumper_remember_scalar (void *data, ptrdiff_t nbytes);
extern void pdumper_remember (void *ptr, enum Lisp_Type type);

#define PDUMPER_REMEMBER_SCALAR(thing)                  \
  pdumper_remember_scalar (&(thing), sizeof (thing))

/* Programmer remark deliberately not remembering THING.  */
#define PDUMPER_IGNORE(thing) ((void) &(thing))

typedef void (*pdumper_hook)(void);
extern void pdumper_do_now_and_after_load (pdumper_hook hook);

/* Macros useful in pdumper callback functions.  Assign a value if
   we're loading a dump and the value needs to be reset to its
   original value, and if we're initializing for the first time,
   assert that the value has the expected original value.  */

#define PDUMPER_RESET(variable, value)         \
  do {                                         \
    if (was_dumped_p ())		       \
      (variable) = (value);                    \
    else                                       \
      eassert ((variable) == (value));         \
  } while (0)

#define PDUMPER_RESET_LV(variable, value)         \
  do {                                            \
    if (was_dumped_p ())			  \
      (variable) = (value);                       \
    else                                          \
      eassert (EQ (variable, value));		  \
  } while (0)

enum pdumper_load_result
  {
    PDUMPER_LOAD_SUCCESS,
    PDUMPER_NOT_LOADED /* Not returned: useful for callers */,
    PDUMPER_LOAD_FILE_NOT_FOUND,
    PDUMPER_LOAD_BAD_FILE_TYPE,
    PDUMPER_LOAD_FAILED_DUMP,
    PDUMPER_LOAD_OOM,
    PDUMPER_LOAD_VERSION_MISMATCH,
    PDUMPER_LOAD_ERROR /* Must be last, as errno may be added.  */
  };

int pdumper_load (char *dump_filename);

/* Whether OBJ points somewhere into the loaded dump file. */
bool pdumper_address_p (const void *obj);
bool pdumper_cold_p (const void *obj);
int pdumper_precise_type (const void *obj);

INLINE _GL_ATTRIBUTE_CONST bool
pdumper_precise_p (const void *obj)
{
  return pdumper_precise_type (obj) != PDUMPER_NO_OBJECT;
}

bool pdumper_marked_p (const void *obj);
void pdumper_set_marked (const void *obj);
void pdumper_clear_marks (void);

/* Record directory where dump was loaded.  */
void pdumper_record_wd (const char *);
void pdumper_fingerprint (FILE *output, const char *label,
			  unsigned char const fingerp[sizeof fingerprint]);

void init_pdumper_once (void);
void syms_of_pdumper (void);

INLINE_HEADER_END
#endif
