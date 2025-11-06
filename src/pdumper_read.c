/* Copyright (C) 2018-2024 Free Software Foundation, Inc.

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

#include <config.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <sys/mman.h>
#include <unistd.h>
#include "timespec.h"
#include "pdumper.h"
#include "bignum.h"

/* Dump runtime */
enum memory_protection
{
  MEMORY_ACCESS_NONE = 1,
  MEMORY_ACCESS_READ = 2,
  MEMORY_ACCESS_READWRITE = 3,
};

struct memory_map_spec
{
  int fd;  /* File to map; anon zero if negative.  */
  size_t size;  /* Number of bytes to map.  */
  off_t offset;  /* Offset within fd.  */
  enum memory_protection protection;
};

struct memory_map
{
  struct memory_map_spec spec;
  void *mapping;  /* Actual mapped memory.  */
  void (*release) (struct memory_map *);
  void *private;
};

enum dump_section
  {
    DS_HOT,
    DS_DISCARDABLE,
    DS_COLD,
    NUMBER_DUMP_SECTIONS,
  };

static inline void *
emacs_ptr_at (const ptrdiff_t offset)
{
  return (void *) (emacs_basis () + offset);
}

/* Read a pointer-sized word at OFFSET within the readback.  */
static uintptr_t
read_word (dump_off offset)
{
  uintptr_t value;
  memcpy (&value, (char *) pdumper_info.addr_beg + offset, sizeof (value));
  return value;
}

/* Write word-sized VALUE to readback at OFFSET.  */
static void
write_word (dump_off offset, uintptr_t value)
{
  memcpy ((char *) pdumper_info.addr_beg + offset, &value, sizeof (value));
}

/* Write a Lisp_Object into readback at OFFSET.  */
static void
write_lv (dump_off offset, Lisp_Object value)
{
  memcpy ((char *) pdumper_info.addr_beg + offset, &value, sizeof (value));
}

static void
reloc_dump (const struct dump_header *const header, const enum reloc_phase phase)
{
  const struct dump_reloc *r
    = (struct dump_reloc *) ((char *) pdumper_info.addr_beg + header->dump_relocs[phase].offset);
  const dump_off nr = header->dump_relocs[phase].nr_entries;
  for (dump_off i = 0; i < nr; ++i)
    {
      struct dump_reloc reloc = r[i];

      /* Never relocate in the cold section.  */
      eassert (reloc.offset < pdumper_info.header.cold_start);

      /* Extract lisp type from reloc type if applicable.  */
      enum Lisp_Type lisp_type;
      if (RELOC_DUMP_LV <= reloc.type
	  && reloc.type < RELOC_DUMP_LV + RELOC_LV_NTYPES)
	{
	  lisp_type = reloc.type - RELOC_DUMP_LV;
	  reloc.type = RELOC_DUMP_LV;
	}
      else if (RELOC_EMACS_LV <= reloc.type
	       && reloc.type < RELOC_EMACS_LV + RELOC_LV_NTYPES)
	{
	  lisp_type = reloc.type - RELOC_EMACS_LV;
	  reloc.type = RELOC_EMACS_LV;
	}

      switch (reloc.type)
	{
	case RELOC_EMACS_PTR:
	  {
	    uintptr_t value = read_word (reloc.offset);
	    eassert (sizeof (void *) == sizeof (value));
	    value += emacs_basis ();
	    write_word (reloc.offset, value);
	  }
	  break;
	case RELOC_DUMP_PTR:
	  {
	    uintptr_t value = read_word (reloc.offset);
	    eassert (sizeof (void *) == sizeof (value));
	    value += pdumper_info.addr_beg;
	    write_word (reloc.offset, value);
	  }
	  break;
#ifdef HAVE_NATIVE_COMP
	case RELOC_NATIVE_COMP_UNIT:
	  {
	    struct Lisp_Native_Comp_Unit *comp_u
	      = (struct Lisp_Native_Comp_Unit *) ((char *) pdumper_info.addr_beg
						  + reloc.offset);
	    comp_u->lambda_gc_guard_h = CALLN (Fmake_hash_table, QCtest, Qeq);
	    if (!STRINGP (comp_u->file))
	      error ("bad compilation unit was dumped");
	    comp_u->handle = dynlib_open_for_eln (SSDATA (comp_u->file));
	    if (!comp_u->handle)
	      error ("%s: %s", SSDATA (comp_u->file), dynlib_error ());
	    eassume (!initialized);
	    load_comp_unit (comp_u);
	  }
	  break;
	case RELOC_NATIVE_SUBR:
	  {
	    /* Revive them one-by-one.  */
	    struct Lisp_Subr *subr =
	      (struct Lisp_Subr *) ((char *) pdumper_info.addr_beg + reloc.offset);
	    struct Lisp_Native_Comp_Unit *comp_u =
	      XNATIVE_COMP_UNIT (subr->native_comp_u);
	    if (!comp_u->handle)
	      error ("NULL handle in compilation unit %s", SSDATA (comp_u->file));
	    const char *c_name = subr->native_c_name;
	    eassert (c_name);
	    void *func = dynlib_sym (comp_u->handle, c_name);
	    if (!func)
	      error ("can't find function \"%s\" in compilation unit %s", c_name,
		     SSDATA (comp_u->file));
	    subr->function.a0 = func;
	    Lisp_Object lambda_data_idx =
	      Fgethash (build_string (c_name), comp_u->lambda_c_name_idx_h, Qnil);
	    if (!NILP (lambda_data_idx))
	      {
		/* This is an anonymous lambda.  We must fixup d_reloc
		   so the lambda can be referenced by code.  */
		Lisp_Object tem;
		XSETSUBR (tem, subr);
		Lisp_Object *fixup =
		  &(comp_u->data_relocs[XFIXNUM (lambda_data_idx)]);
		eassert (EQ (*fixup, Q__lambda_fixup));
		*fixup = tem;
		Fputhash (tem, Qt, comp_u->lambda_gc_guard_h);
	      }
	  }
	  break;
#endif
	case RELOC_BIGNUM:
	  {
	    struct Lisp_Bignum *bignum = (struct Lisp_Bignum *)
	      ((char *) pdumper_info.addr_beg + reloc.offset);
	    struct bignum_reload_info reload_info;
	    static_assert (sizeof (reload_info) <= sizeof (*bignum_val (bignum)));
	    memcpy (&reload_info, bignum_val (bignum), sizeof (reload_info));
	    const mp_limb_t *limbs = (mp_limb_t *)
	      ((char *) pdumper_info.addr_beg + reload_info.data_location);
	    mpz_roinit_n (bignum->value, limbs, reload_info.nlimbs);
	  }
	  break;
	case RELOC_DUMP_LV:
	  {
	    uintptr_t value = pdumper_info.addr_beg + read_word (reloc.offset);
	    write_lv (reloc.offset, make_lisp_ptr ((void *) value, lisp_type));
	  }
	  break;
	case RELOC_EMACS_LV:
	  {
	    uintptr_t value = emacs_basis () + read_word (reloc.offset);
	    write_lv (reloc.offset, make_lisp_ptr ((void *) value, lisp_type));
	  }
	  break;
	default:
	  emacs_abort ();
	  break;
	}
    }
}

static void
reloc_emacs (const struct dump_header *const header)
{
  const dump_off nr = header->emacs_relocs.nr_entries;
  struct emacs_reloc *r
    = (struct emacs_reloc *) ((char *) pdumper_info.addr_beg + header->emacs_relocs.offset);
  for (dump_off i = 0; i < nr; ++i)
    {
      ptrdiff_t pval;
      Lisp_Object lv;
      const struct emacs_reloc reloc = r[i];
      switch (reloc.type)
	{
	case RELOC_COPY_FROM_DUMP:
	  eassume (reloc.length > 0);
	  memcpy (emacs_ptr_at (reloc.offset),
		  (char *) pdumper_info.addr_beg + reloc.ptr.offset,
		  reloc.length);
	  break;
	case RELOC_IMMEDIATE:
	  eassume (0 < reloc.length);
	  eassume (reloc.length <= sizeof (reloc.ptr.immediate));
	  memcpy (emacs_ptr_at (reloc.offset), &reloc.ptr.immediate, reloc.length);
	  break;
	case RELOC_DUMP_PTR:
	  pval = reloc.ptr.offset + pdumper_info.addr_beg;
	  memcpy (emacs_ptr_at (reloc.offset), &pval, sizeof (pval));
	  break;
	case RELOC_EMACS_PTR:
	  pval = reloc.ptr.offset + emacs_basis ();
	  memcpy (emacs_ptr_at (reloc.offset), &pval, sizeof (pval));
	  break;
	case RELOC_DUMP_LV:
	case RELOC_EMACS_LV:
	  {
	    eassume (reloc.length < Lisp_Type_Max);
	    void *obj_ptr = reloc.type == RELOC_DUMP_LV
	      ? (char *) pdumper_info.addr_beg + reloc.ptr.offset
	      : emacs_ptr_at (reloc.ptr.offset);
	    lv = make_lisp_ptr (obj_ptr, reloc.length);
	    memcpy (emacs_ptr_at (reloc.offset), &lv, sizeof (lv));
	  }
	  break;
	default:
	  fatal ("unrecognized relocation type %d", (int) reloc.type);
	  break;
	}
    }
}

/* Mark the pages as unneeded, potentially zeroing them, without
   releasing the address space reservation.  */
static void
discard_mem (void *mem, size_t size)
{
#if VM_SUPPORTED == VM_MS_WINDOWS
      /* Discard COWed pages.  */
      (void) VirtualFree (mem, size, MEM_DECOMMIT);
      /* Release the commit charge for the mapping.  */
      DWORD old_prot;
      (void) VirtualProtect (mem, size, PAGE_NOACCESS, &old_prot);
#elif VM_SUPPORTED == VM_POSIX
# ifdef HAVE_POSIX_MADVISE
      /* Discard COWed pages.  */
      (void) posix_madvise (mem, size, POSIX_MADV_DONTNEED);
# endif
      /* Release the commit charge for the mapping.  */
      (void) mprotect (mem, size, PROT_NONE);
#endif
}

#if VM_SUPPORTED == VM_MS_WINDOWS
static void *
anonymous_allocate_w32 (void *base,
			size_t size,
			enum memory_protection protection)
{
  void *ret;
  DWORD mem_type;
  DWORD mem_prot;

  switch (protection)
    {
    case MEMORY_ACCESS_NONE:
      mem_type = MEM_RESERVE;
      mem_prot = PAGE_NOACCESS;
      break;
    case MEMORY_ACCESS_READ:
      mem_type = MEM_COMMIT;
      mem_prot = PAGE_READONLY;
      break;
    case MEMORY_ACCESS_READWRITE:
      mem_type = MEM_COMMIT;
      mem_prot = PAGE_READWRITE;
      break;
    default:
      emacs_abort ();
    }

  ret = VirtualAlloc (base, size, mem_type, mem_prot);
  if (ret == NULL)
    errno = (base && GetLastError () == ERROR_INVALID_ADDRESS)
      ? EBUSY
      : EPERM;
  return ret;
}
#endif

#if VM_SUPPORTED == VM_POSIX

/* Old versions of macOS only define MAP_ANON, not MAP_ANONYMOUS.
   FIXME: This probably belongs elsewhere (gnulib/autoconf?)  */
# ifndef MAP_ANONYMOUS
#  define MAP_ANONYMOUS MAP_ANON
# endif

static void *
anonymous_allocate_posix (void *base,
			  size_t size,
			  enum memory_protection protection)
{
  void *ret;
  int mem_prot;

  switch (protection)
    {
    case MEMORY_ACCESS_NONE:
      mem_prot = PROT_NONE;
      break;
    case MEMORY_ACCESS_READ:
      mem_prot = PROT_READ;
      break;
    case MEMORY_ACCESS_READWRITE:
      mem_prot = PROT_READ | PROT_WRITE;
      break;
    default:
      emacs_abort ();
    }

  int mem_flags = MAP_PRIVATE | MAP_ANONYMOUS;
  if (mem_prot != PROT_NONE)
    mem_flags |= MAP_POPULATE;
  if (base)
    mem_flags |= MAP_FIXED;

  bool retry;
  do
    {
      retry = false;
      ret = mmap (base, size, mem_prot, mem_flags, -1, 0);
      if (ret == MAP_FAILED
	  && errno == EINVAL
	  && (mem_flags & MAP_POPULATE))
        {
          /* This system didn't understand MAP_POPULATE, so try
             again without it.  */
          mem_flags &= ~MAP_POPULATE;
          retry = true;
        }
    }
  while (retry);

  if (ret == MAP_FAILED)
    ret = NULL;
  return ret;
}
#endif

/* Perform anonymous memory allocation.  */
static void *
anonymous_allocate (void *base,
		    const size_t size,
		    enum memory_protection protection)
{
#if VM_SUPPORTED == VM_POSIX
  return anonymous_allocate_posix (base, size, protection);
#elif VM_SUPPORTED == VM_MS_WINDOWS
  return anonymous_allocate_w32 (base, size, protection);
#else
  errno = ENOSYS;
  return NULL;
#endif
}

/* Undo the effect of dump_reserve_address_space().  */
static void
anonymous_release (void *addr, size_t size)
{
  eassert (size >= 0);
#if VM_SUPPORTED == VM_MS_WINDOWS
  (void) size;
  if (!VirtualFree (addr, 0, MEM_RELEASE))
    emacs_abort ();
#elif VM_SUPPORTED == VM_POSIX
  if (munmap (addr, size) < 0)
    emacs_abort ();
#else
  (void) addr;
  (void) size;
  emacs_abort ();
#endif
}

static void
mmap_discard_contents (struct memory_map *map)
{
  if (map->mapping)
    discard_mem (map->mapping, map->spec.size);
}

static void
mmap_reset (struct memory_map *map)
{
  map->mapping = NULL;
  map->release = NULL;
  map->private = NULL;
}

static void
mmap_release (struct memory_map *map)
{
  if (map->release)
    map->release (map);
  mmap_reset (map);
}

/* Allows heap-allocated mmap to "free" maps individually.  */
struct memory_map_heap_control_block
{
  int refcount;
  void *mem;
};

static void
mmap_heap_cb_release (struct memory_map_heap_control_block *cb)
{
  eassert (cb->refcount > 0);
  if (--cb->refcount == 0)
    {
      free (cb->mem);
      free (cb);
    }
}

static void
mmap_release_heap (struct memory_map *map)
{
  mmap_heap_cb_release (map->private);
}

/* Remove a virtual memory mapping.

   On failure, abort Emacs.  For maximum platform compatibility, ADDR
   and SIZE must match the mapping exactly.  */
static void
unmap_file (void *addr, size_t size)
{
  eassert (size >= 0);
#if !VM_SUPPORTED
  (void) addr;
  (void) size;
  emacs_abort ();
#elif defined (WINDOWSNT)
  (void) size;
  if (!UnmapViewOfFile (addr))
    emacs_abort ();
#else
  if (munmap (addr, size) < 0)
    emacs_abort ();
#endif
}

static void
mmap_release_vm (struct memory_map *map)
{
  if (map->spec.fd < 0)
    anonymous_release (map->mapping, map->spec.size);
  else
    unmap_file (map->mapping, map->spec.size);
}

static bool
needs_mmap_retry_p (void)
{
#if defined CYGWIN || VM_SUPPORTED == VM_MS_WINDOWS || defined _AIX
  return true;
#else
  return false;
#endif
}

#if VM_SUPPORTED == VM_MS_WINDOWS
static void *
map_file_w32 (void *base, int fd, off_t offset, size_t size,
	      enum memory_protection protection)
{
  void *ret = NULL;
  HANDLE section = NULL;
  HANDLE file;

  uint64_t full_offset = offset;
  uint32_t offset_high = (uint32_t) (full_offset >> 32);
  uint32_t offset_low = (uint32_t) (full_offset & 0xffffffff);

  int error;
  DWORD protect;
  DWORD map_access;

  file = (HANDLE) _get_osfhandle (fd);
  if (file == INVALID_HANDLE_VALUE)
    goto out;

  switch (protection)
    {
    case MEMORY_ACCESS_READWRITE:
      protect = PAGE_WRITECOPY;	/* for Windows 9X */
      break;
    default:
    case MEMORY_ACCESS_NONE:
    case MEMORY_ACCESS_READ:
      protect = PAGE_READONLY;
      break;
    }

  section = CreateFileMapping (file,
			       /*lpAttributes=*/NULL,
			       protect,
			       /*dwMaximumSizeHigh=*/0,
			       /*dwMaximumSizeLow=*/0,
			       /*lpName=*/NULL);
  if (!section)
    {
      errno = EINVAL;
      goto out;
    }

  switch (protection)
    {
    case MEMORY_ACCESS_NONE:
    case MEMORY_ACCESS_READ:
      map_access = FILE_MAP_READ;
      break;
    case MEMORY_ACCESS_READWRITE:
      map_access = FILE_MAP_COPY;
      break;
    default:
      emacs_abort ();
    }

  ret = MapViewOfFileEx (section,
                         map_access,
                         offset_high,
                         offset_low,
                         size,
                         base);

  error = GetLastError ();
  if (ret == NULL)
    errno = (error == ERROR_INVALID_ADDRESS ? EBUSY : EPERM);
 out:
  if (section && !CloseHandle (section))
    emacs_abort ();
  return ret;
}
#endif

#if VM_SUPPORTED == VM_POSIX
static void *
map_file_posix (void *base, int fd, off_t offset, size_t size,
		enum memory_protection protection)
{
  void *ret;
  int mem_prot;
  int mem_flags;

  switch (protection)
    {
    case MEMORY_ACCESS_NONE:
      mem_prot = PROT_NONE;
      mem_flags = MAP_SHARED;
      break;
    case MEMORY_ACCESS_READ:
      mem_prot = PROT_READ;
      mem_flags = MAP_SHARED;
      break;
    case MEMORY_ACCESS_READWRITE:
      mem_prot = PROT_READ | PROT_WRITE;
      mem_flags = MAP_PRIVATE;
      break;
    default:
      emacs_abort ();
    }

  if (base)
    mem_flags |= MAP_FIXED;

  ret = mmap (base, size, mem_prot, mem_flags, fd, offset);
  if (ret == MAP_FAILED)
    ret = NULL;
  return ret;
}
#endif

/* Map a file into memory.  */
static void *
map_file (void *base, int fd, off_t offset, size_t size,
	  enum memory_protection protection)
{
#if VM_SUPPORTED == VM_POSIX
  return map_file_posix (base, fd, offset, size, protection);
#elif VM_SUPPORTED == VM_MS_WINDOWS
  return map_file_w32 (base, fd, offset, size, protection);
#else
  errno = ENOSYS;
  return NULL;
#endif
}

static bool
mmap_contiguous_vm (struct memory_map *maps, int nr_maps,
		    size_t total_size)
{
  bool ret = false;
  void *resv = NULL;
  bool retry = false;
  const bool need_retry = needs_mmap_retry_p ();

  do
    {
      if (retry)
        {
          eassert (need_retry);
          retry = false;
          for (int i = 0; i < nr_maps; ++i)
            mmap_release (&maps[i]);
        }

      eassert (resv == NULL);
      resv = anonymous_allocate (NULL, total_size, MEMORY_ACCESS_NONE);
      if (!resv)
        goto out;

      char *mem = resv;

      if (need_retry)
        {
          /* Windows lacks atomic mapping replace; need to release the
             reservation so we can allocate within it.  Will retry the
             loop if someone squats on our address space before we can
             finish allocation.  On POSIX systems, we leave the
             reservation around for atomicity.  */
          anonymous_release (resv, total_size);
          resv = NULL;
        }

      for (int i = 0; i < nr_maps; ++i)
        {
          const struct memory_map_spec spec = maps[i].spec;
          if (!spec.size)
            continue;
          else if (spec.fd < 0)
	    maps[i].mapping = anonymous_allocate (mem, spec.size,
						  spec.protection);
          else
	    maps[i].mapping = map_file (mem, spec.fd, spec.offset,
					spec.size, spec.protection);
          mem += spec.size;
	  if (need_retry && maps[i].mapping == NULL
	      && (errno == EBUSY
#ifdef CYGWIN
		  || errno == EINVAL
#endif
		  ))
            {
              retry = true;
              continue;
            }
          if (maps[i].mapping == NULL)
            goto out;
          maps[i].release = mmap_release_vm;
        }
    }
  while (retry);

  ret = true;
  resv = NULL;
 out:
  if (resv)
    anonymous_release (resv, total_size);
  if (!ret)
    {
      for (int i = 0; i < nr_maps; ++i)
	{
	  if (need_retry)
	    mmap_reset (&maps[i]);
	  else
	    mmap_release (&maps[i]);
	}
    }
  return ret;
}

/* Implement mmap using malloc and read.  */
static bool
mmap_contiguous_heap (struct memory_map *maps, int nr_maps,
		      size_t total_size)
{
  bool ret = false;

  /* Sometimes never freed.  */
  struct memory_map_heap_control_block *cb = calloc (1, sizeof (*cb));
  if (!cb)
    goto out;
  __lsan_ignore_object (cb);

  cb->refcount = 1;
  cb->mem = malloc (total_size);
  if (!cb->mem)
    goto out;
  char *mem = cb->mem;
  for (int i = 0; i < nr_maps; ++i)
    {
      const struct memory_map_spec spec = maps[i].spec;
      if (!spec.size)
        continue;
      maps[i].mapping = mem;
      mem += spec.size;
      maps[i].release = mmap_release_heap;
      maps[i].private = cb;
      ++cb->refcount;
      if (spec.fd < 0)
        memset (maps[i].mapping, 0, spec.size);
      else
        {
          if (lseek (spec.fd, spec.offset, SEEK_SET) < 0)
            goto out;
          ssize_t nb = read_bytes (spec.fd, maps[i].mapping, spec.size);
          if (nb != spec.size)
	    {
	      if (nb >= 0)
		errno = EIO;
	      goto out;
	    }
        }
    }

  ret = true;
 out:
  mmap_heap_cb_release (cb);
  if (!ret)
    for (int i = 0; i < nr_maps; ++i)
      mmap_release (&maps[i]);
  return ret;
}

/* Map a range of addresses into a chunk of contiguous memory.

   Each memory_map structure describes how to fill the
   corresponding range of memory. On input, all members except MAPPING
   are valid. On output, MAPPING contains the location of the given
   chunk of memory. The MAPPING for MAPS[N] is MAPS[N-1].mapping +
   MAPS[N-1].size.

   Return true on success or false on failure with errno set.  */
static bool
mmap_contiguous (struct memory_map *maps, int nr_maps)
{
  if (!nr_maps)
    return true;

  size_t total_size = 0;
  for (int i = 0; i < nr_maps; ++i)
    {
      eassert (maps[i].mapping == NULL);
      eassert (maps[i].release == NULL);
      eassert (maps[i].private == NULL);
      /* Maps except last must be a multiple of pagesize.  */
      eassert (i == nr_maps - 1
	       || 0 == maps[i].spec.size % MAX_PAGE_SIZE);
      total_size += maps[i].spec.size;
    }

  return (VM_SUPPORTED ? mmap_contiguous_vm : mmap_contiguous_heap)
    (maps, nr_maps, total_size);
}

static size_t
divide_round_up (size_t x, size_t y)
{
  return (x + y - 1) / y;
}

/* Pointer to a stack variable to avoid having to staticpro it.  */
static Lisp_Object *pdumper_hashes = &zero_vector;

static void
thaw_hash_tables (void)
{
  Lisp_Object hash_tables = *pdumper_hashes;
  for (ptrdiff_t i = 0; i < ASIZE (hash_tables); ++i)
    hash_table_thaw (AREF (hash_tables, i));
}

void
init_pdumper_once (void)
{
  pdumper_do_now_and_after_load (thaw_hash_tables);
}

int
pdumper_load (char *filename)
{
  intptr_t dump_size;
  struct stat stat;
  dump_off discardable_start;
  enum pdumper_load_result result = PDUMPER_LOAD_SUCCESS;
  struct dump_header header_buf = { 0 };
  struct dump_header *header = &header_buf;
  struct memory_map sections[NUMBER_DUMP_SECTIONS] = { 0 };
  const struct timespec start_time = current_timespec ();
  int dump_fd = emacs_open_noquit (filename, O_RDONLY, 0);

  if (dump_fd < 0)
    {
      result = (errno == ENOENT || errno == ENOTDIR
		? PDUMPER_LOAD_FILE_NOT_FOUND
		: PDUMPER_LOAD_ERROR + errno);
      goto out;
    }

  if (fstat (dump_fd, &stat) < 0)
    {
      result = PDUMPER_LOAD_FILE_NOT_FOUND;
      goto out;
    }

  if (stat.st_size > INTPTR_MAX)
    {
      result = PDUMPER_LOAD_BAD_FILE_TYPE;
      goto out;
    }

  dump_size = (intptr_t) stat.st_size;
  if (dump_size < sizeof (*header))
    {
      result = PDUMPER_LOAD_BAD_FILE_TYPE;
      goto out;
    }

  if (read_bytes (dump_fd, header, sizeof (*header)) < sizeof (*header))
    {
      result = PDUMPER_LOAD_BAD_FILE_TYPE;
      goto out;
    }

  if (memcmp (header->magic, dump_magic, sizeof (dump_magic)) != 0)
    {
      result = PDUMPER_LOAD_BAD_FILE_TYPE;
      if (header->magic[0] == '!'
	  && (header->magic[0] = dump_magic[0],
	      memcmp (header->magic, dump_magic, sizeof (dump_magic)) == 0))
	result = PDUMPER_LOAD_FAILED_DUMP;
      goto out;
    }

  static_assert (sizeof (header->fingerprint) == sizeof (fingerprint));
  unsigned char desired[sizeof fingerprint];
  for (int i = 0; i < sizeof fingerprint; ++i)
    desired[i] = fingerprint[i];
  if (memcmp (header->fingerprint, desired, sizeof desired) != 0)
    {
      pdumper_fingerprint (stderr, "desired fingerprint", desired);
      pdumper_fingerprint (stderr, "found fingerprint", header->fingerprint);
      result = PDUMPER_LOAD_VERSION_MISMATCH;
      goto out;
    }

  discardable_start = ROUNDUP (header->discardable_start, MAX_PAGE_SIZE);
  eassert (discardable_start % MAX_PAGE_SIZE == 0);
  eassert (discardable_start <= header->cold_start);

  sections[DS_HOT].spec = (struct memory_map_spec)
    {
     .fd = dump_fd,
     .size = discardable_start,
     .offset = 0,
     .protection = MEMORY_ACCESS_READWRITE,
    };

  sections[DS_DISCARDABLE].spec = (struct memory_map_spec)
    {
     .fd = dump_fd,
     .size = header->cold_start - discardable_start,
     .offset = discardable_start,
     .protection = MEMORY_ACCESS_READWRITE,
    };

  sections[DS_COLD].spec = (struct memory_map_spec)
    {
     .fd = dump_fd,
     .size = dump_size - header->cold_start,
     .offset = header->cold_start,
     .protection = MEMORY_ACCESS_READWRITE,
    };

  if (!mmap_contiguous (sections, ARRAYELTS (sections)))
    {
      result = PDUMPER_LOAD_OOM;
      goto out;
    }

  /* Point of no return.  */
  gflags.was_dumped_ = true;
  pdumper_info.header = *header;
  pdumper_info.mark_bits = bitset_create
    (divide_round_up (header->discardable_start, DUMP_ALIGNMENT),
     BITSET_FIXED);
  pdumper_info.addr_beg = (uintptr_t) sections[0].mapping;
  pdumper_info.addr_end = (uintptr_t) ((char *) pdumper_info.addr_beg + dump_size);

  reloc_dump (header, EARLY_RELOCS);
  reloc_emacs (header);

  mmap_discard_contents (&sections[DS_DISCARDABLE]);
  for (int i = 0; i < ARRAYELTS (sections); ++i)
    mmap_reset (&sections[i]);

  Lisp_Object hashes = zero_vector;
  if (header->hash_list)
    {
      struct Lisp_Vector *hash_tables
	= (struct Lisp_Vector *) ((char *) pdumper_info.addr_beg + header->hash_list);
      hashes = make_lisp_ptr (hash_tables, Lisp_Vectorlike);
    }

  pdumper_hashes = &hashes;
  for (int i = 0; i < nr_dump_hooks; ++i)
    dump_hooks[i] ();

#ifdef HAVE_NATIVE_COMP
  reloc_dump (header, NATIVE_COMP_RELOCS);
#endif
  reloc_dump (header, LATE_RELOCS);

  eassert (!initialized);
  initialized = true;

  struct timespec load_timespec = timespec_sub (current_timespec (), start_time);
  pdumper_info.load_time = timespectod (load_timespec);
  pdumper_info.filename = xstrdup (filename);

 out:
  for (int i = 0; i < ARRAYELTS (sections); ++i)
    mmap_release (&sections[i]);
  if (dump_fd >= 0)
    emacs_close (dump_fd);

  return result;
}
