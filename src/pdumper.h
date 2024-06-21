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


#define PDUMPER_REMEMBER_SCALAR(thing)                  \
  pdumper_remember_scalar (&(thing), sizeof (thing))

/* Programmer remark deliberately not remembering THING.  */
#define PDUMPER_IGNORE(thing) ((void) &(thing))

typedef void (*pdumper_hook)(void);

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

#define VM_POSIX 1
#define VM_MS_WINDOWS 2

#if defined (HAVE_MMAP) && defined (MAP_FIXED)
# define VM_SUPPORTED VM_POSIX
# if !defined (MAP_POPULATE) && defined (MAP_PREFAULT_READ)
#  define MAP_POPULATE MAP_PREFAULT_READ
# elif !defined (MAP_POPULATE)
#  define MAP_POPULATE 0
# endif
#elif defined (WINDOWSNT)
  /* Use a float infinity, to avoid compiler warnings in comparing vs
     candidates' score.  */
# undef INFINITY
# define INFINITY __builtin_inff ()
# include <windows.h>
# define VM_SUPPORTED VM_MS_WINDOWS
#else
# define VM_SUPPORTED 0
#endif

typedef int_least32_t dump_off;

/* Some relocations must occur before others. */
enum reloc_phase
  {
    EARLY_RELOCS,
#ifdef HAVE_NATIVE_COMP
    NATIVE_COMP_RELOCS,
#endif
    LATE_RELOCS, /* lisp can be called */
    RELOC_NUM_PHASES
  };

struct dump_locator
{
  dump_off offset;
  dump_off nr_entries;
};

extern const char dump_magic[16];

struct dump_header
{
  /* File type magic.  */
  char magic[sizeof (dump_magic)];

  /* Associated Emacs binary.  */
  unsigned char fingerprint[sizeof fingerprint];

  /* Find dump relocations.  */
  struct dump_locator dump_relocs[RELOC_NUM_PHASES];

  /* Find types for ambiguous Lisp_Object pointers (mark_memory).  */
  struct dump_locator object_starts;

  /* Find executable relocations.  */
  struct dump_locator emacs_relocs;

  /* Marks end of hot region.  Discardable objects are copied into the
     emacs executable when loading dump.  */
  dump_off discardable_start;

  /* Marks end of discardable region.  Cold objects do not require
     relocations and are memory-mapped directly (possibly incurring
     modest copy-on-write faults).

     While cold objects are intended to be immutable, this region
     remains modifiable for string data that might be ill-advisedly
     ASET.  */
  dump_off cold_start;

  /* Offset of a vector of the dumped hash tables.  */
  dump_off hash_list;
};

/* Worst-case allocation granularity.  */
#define MAX_PAGE_SIZE (64 * 1024)

struct pdumper_info
{
  struct dump_header header;
  bitset mark_bits;
  double load_time;
  char *filename;
  uintptr_t addr_beg;
  uintptr_t addr_end;
};

extern struct pdumper_info pdumper_info;

/* Whether OBJ points somewhere into the loaded dump file.
   Gets called a lot so inline.  */
INLINE _GL_ATTRIBUTE_CONST bool
pdumper_address_p (const void *obj)
{
  uintptr_t obj_addr = (uintptr_t) obj;
  return pdumper_info.addr_beg <= obj_addr && obj_addr < pdumper_info.addr_end;
}

typedef int_least32_t dump_off;
#define DUMP_OFF_MIN INT_LEAST32_MIN
#define DUMP_OFF_MAX INT_LEAST32_MAX
#define DUMP_OFF_NBITS INT_LEAST32_WIDTH
#define PRIdDUMP_OFF PRIdLEAST32

enum
  {
    RELOC_LV_NTYPES = 8,
    RELOC_TYPE_NBITS = 5,
    RELOC_OFFS_NBITS = DUMP_OFF_NBITS - RELOC_TYPE_NBITS,
    DUMP_ALIGNMENT = max (GCALIGNMENT, 4),
  };

enum reloc_type
  {
    /* dump_ptr = dump_ptr + emacs_basis() */
    RELOC_EMACS_PTR,
    /* dump_ptr = dump_ptr + dump_basis */
    RELOC_DUMP_PTR,
    /* dump_mpz = [rebuild bignum] */
    RELOC_NATIVE_COMP_UNIT,
    RELOC_NATIVE_SUBR,
    RELOC_BIGNUM,
    /* Copy raw bytes from the dump into executable */
    RELOC_COPY_FROM_DUMP,
    /* Set a memory location to the verbatim value */
    RELOC_IMMEDIATE,
    /* make_lisp_ptr (reloc.offset + dump_basis(), reloc.type - RELOC_DUMP_LV) */
    RELOC_DUMP_LV,
    /* make_lisp_ptr (reloc.offset + emacs_basis(), reloc.type - RELOC_EMACS_LV) */
    RELOC_EMACS_LV = RELOC_DUMP_LV + RELOC_LV_NTYPES,
  };

verify (RELOC_EMACS_LV + RELOC_LV_NTYPES < (1 << RELOC_TYPE_NBITS));
verify (DUMP_ALIGNMENT >= GCALIGNMENT);

struct dump_start
{
  ENUM_BF (Lisp_Type) type : RELOC_TYPE_NBITS;
  dump_off offset : RELOC_OFFS_NBITS;
};

struct dump_reloc
{
  ENUM_BF (reloc_type) type : RELOC_TYPE_NBITS;
  dump_off offset : RELOC_OFFS_NBITS;
};

/* The various metadata need to be packed as dump_off's.  */
verify (sizeof (struct dump_start) == sizeof (dump_off));
verify (sizeof (struct dump_reloc) == sizeof (dump_off));

struct emacs_reloc
{
  ENUM_BF (reloc_type) type;
  dump_off length;
  dump_off offset;
  union
  {
    dump_off offset;    /* RELOC_DUMP_PTR, RELOC_EMACS_PTR */
    intmax_t immediate; /* RELOC_IMMEDIATE */
  } ptr;
};

struct bignum_reload_info
{
  dump_off data_location;
  dump_off nlimbs;
};

extern pdumper_hook dump_hooks[24];
extern int nr_dump_hooks;

extern uintptr_t emacs_basis (void);

/* "Remember" functions preserve auxiliary C variables not pointed to by
   Lisp heap (as one does in staticpro).  */
extern void pdumper_remember_scalar (void *data, ptrdiff_t nbytes);
extern void pdumper_remember (void *ptr, enum Lisp_Type type);

extern const struct dump_start *pdumper_object_start (const void *obj);
extern void pdumper_do_now_and_after_load (pdumper_hook hook);
extern int pdumper_load (char *dump_filename);
extern bool pdumper_cold_p (const void *obj);

extern void pdumper_fingerprint (FILE *output, char const *label,
				 unsigned char const xfingerprint[sizeof fingerprint]);
extern bool pdumper_marked_p (const void *obj);
extern void pdumper_set_marked (const void *obj);
extern void pdumper_clear_marks (void);

/* Record directory where dump was loaded.  */
extern void pdumper_record_wd (const char *);

extern ssize_t read_bytes (int fd, void *buf, size_t bytes_to_read);
extern void init_pdumper_once (void);
extern void syms_of_pdumper (void);

INLINE_HEADER_END
#endif
