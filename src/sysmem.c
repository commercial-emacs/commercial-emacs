#include "sysmem.h"
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#ifdef WINDOWSNT
# include <windows.h>
#endif
#include "bitset.h"

#define VM_POSIX 1
#define VM_MS_WINDOWS 2

#if defined (HAVE_MMAP) && defined (MAP_FIXED)
# define VM_SUPPORTED VM_POSIX
#elif defined (WINDOWSNT)
# define VM_SUPPORTED VM_MS_WINDOWS
#else
# define VM_SUPPORTED 0
#endif

#if VM_SUPPORTED == VM_POSIX
# if !defined (MAP_POPULATE) && defined (MAP_PREFAULT_READ)
#  define MAP_POPULATE MAP_PREFAULT_READ
# elif !defined (MAP_POPULATE)
#  define MAP_POPULATE 0
# endif
#endif

/* Old versions of macOS only define MAP_ANON, not MAP_ANONYMOUS.
   FIXME: This probably belongs elsewhere (gnulib/autoconf?)  */
#if VM_SUPPORTED == VM_POSIX
# ifndef MAP_ANONYMOUS
#  define MAP_ANONYMOUS MAP_ANON
# endif
#endif

/* Find the smallest power of two equal to VALUE or greater than
   VALUE, place the result in *OUT, and return true.  If no such value
   exists (can happen due to range limitations), return false and set
   errno to ERANGE.  */
static bool
try_next_pow2 (const size_t value, size_t *const out)
{
  if (POWER_OF_2 (value))
    {
      *out = value;
      return true;
    }
  const int leading_zeros = emacs_clz_z (value);
  eassume (leading_zeros >= 0);
  if (leading_zeros == 0)
    {
      errno = ERANGE;
      return false;
    }
  const int total_bits = CHAR_BIT * sizeof (value);
  *out = (size_t) 1 << (total_bits - leading_zeros);
  return true;
}

/* Find the smallest multiple of MULTIPLE equal to VALUE or greater
   than VALUE, place the result in *OUT, and return true.  If no such
   value exists (can happen due to range limitations), return false
   and set errno to ERANGE.  */
static bool
try_round_up (const size_t value,
              const size_t multiple,
              size_t *const out)
{
  const size_t rem = value % multiple;
  if (rem == 0)
    {
      *out = value;
      return true;
    }
  const size_t inc = multiple - rem;
  if (INT_ADD_WRAPV (value, inc, out))
    {
      errno = ERANGE;
      return false;
    }
  eassert (*out % multiple == 0);
  return true;
}

#if VM_SUPPORTED == VM_MS_WINDOWS
static void *
anonymous_allocate_w32 (void *base,
                        size_t size,
                        enum emacs_memory_protection protection)
{
  void *ret;
  DWORD mem_type;
  DWORD mem_prot;

  switch (protection)
    {
    case EMACS_MEMORY_ACCESS_NONE:
      mem_type = MEM_RESERVE;
      mem_prot = PAGE_NOACCESS;
      break;
    case EMACS_MEMORY_ACCESS_READ:
      mem_type = MEM_COMMIT;
      mem_prot = PAGE_READONLY;
      break;
    case EMACS_MEMORY_ACCESS_READWRITE:
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
#endif /* VM_MS_WINDOWS */


#if VM_SUPPORTED == VM_POSIX
/* Like mmap(), except automatically retry if MAP_POPULATE isn't
   supported.  On error, always return NULL (with errno set) and not
   MAP_FAILED.  */
static void *
emacs_mmap_posix (void *addr, size_t length, int prot, int flags,
                  int fd, off_t offset)
{
  void *ret;
  bool retry;
  do
    {
      retry = false;
      ret = mmap (addr, length, prot, flags, fd, offset);
      if (ret == MAP_FAILED
	  && errno == EINVAL
	  && (flags & MAP_POPULATE))
        {
          /* This system didn't understand MAP_POPULATE, so try
             again without it.  */
          flags &= ~MAP_POPULATE;
          retry = true;
        }
    }
  while (retry);

  if (ret == MAP_FAILED)
    ret = NULL;
  return ret;
}
#endif /* VM_POSIX */

#if VM_SUPPORTED == VM_POSIX
static void *
emacs_anonymous_allocate_posix (void *base,
                                size_t size,
                                enum emacs_memory_protection protection,
                                bool populate)
{
  int mem_prot;

  switch (protection)
    {
    case EMACS_MEMORY_ACCESS_NONE:
      mem_prot = PROT_NONE;
      break;
    case EMACS_MEMORY_ACCESS_READ:
      mem_prot = PROT_READ;
      break;
    case EMACS_MEMORY_ACCESS_READWRITE:
      mem_prot = PROT_READ | PROT_WRITE;
      break;
    default:
      emacs_abort ();
    }

  int mem_flags = MAP_PRIVATE | MAP_ANONYMOUS;
  if (populate)
    mem_flags |= MAP_POPULATE;
  if (base)
    mem_flags |= MAP_FIXED;

  return emacs_mmap_posix (base, size, mem_prot, mem_flags, -1, 0);
}
#endif /* VM_POSIX */

/* Perform anonymous memory allocation.  */
static void *
emacs_anonymous_allocate (void *base,
                          const size_t size,
                          enum emacs_memory_protection protection,
                          bool populate)
{
#if VM_SUPPORTED == VM_POSIX
  return emacs_anonymous_allocate_posix (base, size, protection, populate);
#elif VM_SUPPORTED == VM_MS_WINDOWS
  return emacs_anonymous_allocate_w32 (base, size, protection);
#else
  errno = ENOSYS;
  return NULL;
#endif
}

/* Release an anonymous memory allocation.  */
static void
emacs_unmap_anonymous (void *addr, size_t size)
{
  eassert (size >= 0);
#if VM_SUPPORTED == VM_MS_WINDOWS
  (void) size;
  if (!VirtualFree (addr, 0, MEM_RELEASE))
    emacs_abort ();
#elif VM_SUPPORTED == VM_POSIX
  if (munmap (addr, size))
    emacs_abort ();
#else
  (void) addr;
  (void) size;
  emacs_abort ();
#endif
}

#if VM_SUPPORTED == VM_MS_WINDOWS
static void *
emacs_map_file_w32 (void *base, int fd, off_t offset, size_t size,
                    enum emacs_memory_protection protection)
{
  void *ret = NULL;
  HANDLE section = NULL;
  HANDLE file;

  uint64_t full_offset = offset;
  uint32_t offset_high = (uint32_t) (full_offset >> 32);
  uint32_t offset_low = (uint32_t) (full_offset & 0xffffffff);

  int error;
  DWORD map_access;

  file = (HANDLE) _get_osfhandle (fd);
  if (file == INVALID_HANDLE_VALUE)
    goto out;

  section = CreateFileMapping (file,
			       /*lpAttributes=*/NULL,
			       PAGE_READONLY,
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
    case EMACS_MEMORY_ACCESS_NONE:
    case EMACS_MEMORY_ACCESS_READ:
      map_access = FILE_MAP_READ;
      break;
    case EMACS_MEMORY_ACCESS_READWRITE:
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
emacs_map_file_posix (void *base, int fd, off_t offset, size_t size,
                      enum emacs_memory_protection protection,
                      bool populate)
{
  int mem_prot;
  int mem_flags;

  switch (protection)
    {
    case EMACS_MEMORY_ACCESS_NONE:
      mem_prot = PROT_NONE;
      mem_flags = MAP_SHARED;
      break;
    case EMACS_MEMORY_ACCESS_READ:
      mem_prot = PROT_READ;
      mem_flags = MAP_SHARED;
      break;
    case EMACS_MEMORY_ACCESS_READWRITE:
      mem_prot = PROT_READ | PROT_WRITE;
      mem_flags = MAP_PRIVATE;
      break;
    default:
      emacs_abort ();
    }

  if (base)
    mem_flags |= MAP_FIXED;

  if (populate)
    mem_flags |= MAP_POPULATE;

  return emacs_mmap_posix (base, size, mem_prot, mem_flags, fd, offset);
}
#endif

/* Map a file into memory.  */
static void *
emacs_map_file (void *base, int fd, off_t offset, size_t size,
                enum emacs_memory_protection protection,
                bool populate)
{
#if VM_SUPPORTED == VM_POSIX
  return emacs_map_file_posix (base, fd, offset, size, protection,
                               populate);
#elif VM_SUPPORTED == VM_MS_WINDOWS
  return emacs_map_file_w32 (base, fd, offset, size, protection);
#else
  errno = ENOSYS;
  return NULL;
#endif
}

/* Remove a virtual memory mapping.

   On failure, abort Emacs.  For maximum platform compatibility, ADDR
   and SIZE must match the mapping exactly.  */
static void
emacs_unmap_file (void *addr, size_t size)
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

#if VM_SUPPORTED == VM_POSIX && defined (GNU_LINUX)
static void
emacs_zero_page_aligned_memory_linux (void *const mem, const size_t size)
{
  if (!madvise (mem, size, MADV_DONTNEED))
    return;
  /* MADV_DONTNEED can fail with EINVAL if the memory
     given is locked, and we have no control over whether
     that memory might be locked.  Use memset as a
     fallback in this case.  */
  if (errno != EINVAL)
    emacs_abort ();
  memset (mem, 0, size);
}

static void
emacs_discard_page_aligned_memory_linux (void *const mem, const size_t size)
{
  if (!madvise (mem, size, MADV_FREE))
    return;
  if (errno != EINVAL)
    emacs_abort ();
  /* MADV_FREE only works on anonymous ranges.  If we're
     clearing a file-backed range (as we are in pdumper)
     try MADV_DONTNEED instead.  See the caveat above about
     MADV_DONTNEED's behavior on locked ranges.  */
  emacs_zero_page_aligned_memory_linux (mem, size);
}
#endif

static void
emacs_zero_or_discard_memory (void *const mem,
                              const size_t size,
                              const bool zero)
{
  if (!size)
    return;
  eassume (mem != NULL);

  void *tail;
  size_t tail_size;

  const uintptr_t pos = (uintptr_t) mem;
  eassume (!INT_ADD_OVERFLOW (pos, size));
  void *head;
  size_t head_size;
  if (pos % EMACS_PAGE_SIZE_MAX != 0)
    {
      const size_t gap = EMACS_PAGE_SIZE_MAX - (pos % EMACS_PAGE_SIZE_MAX);
      head = (void *) pos;
      head_size = min (gap, size);
    }
  else
    {
      head = NULL;
      head_size = 0;
    }
  const size_t nr_middle_pages = (size - head_size) / EMACS_PAGE_SIZE_MAX;
  const size_t middle_size = nr_middle_pages * EMACS_PAGE_SIZE_MAX;
  eassume (middle_size == 0 || (pos + head_size) % EMACS_PAGE_SIZE_MAX == 0);
  void *const middle = (void *) (pos + head_size);
  tail = (void *) (pos + head_size + middle_size);
  tail_size = size - head_size - middle_size;
  eassume (head_size + middle_size + tail_size == size);

#if VM_SUPPORTED == VM_MS_WINDOWS
  if (zero)
    memset (mem, 0, size);
  else if (middle_size)
    DiscardVirtualMemory (middle, middle_size);
#elif VM_SUPPORTED == VM_POSIX && defined (GNU_LINUX)
  if (head_size && zero)
    memset (head, 0, head_size);
  if (middle_size)
    {
      if (zero)
        emacs_zero_page_aligned_memory_linux (middle, middle_size);
      else
        emacs_discard_page_aligned_memory_linux (middle, middle_size);
    }
  if (tail_size && zero)
    memset (tail, 0, tail_size);
#elif VM_SUPPORTED == VM_POSIX && defined (MADV_FREE)  /* FreeBSD, etc. */
  if (zero)
    memset (mem, 0, size);
  else if (middle_size)
    (void )!madvise (middle, middle_size, MADV_FREE);
#else
  if (zero)
    memset (mem, 0, size);
#endif
  /* Suppress unused variable warnings.  */
  (void) head;
  (void) head_size;
  (void) middle;
  (void) middle_size;
  (void) tail;
  (void) tail_size;
}


/* Zero the given memory range.  May use VM primitives to directly
   release pages.  Equivalent to a memset.  */
void
emacs_zero_memory (void *const mem, const size_t size)
{
  emacs_zero_or_discard_memory (mem, size, /*zero=*/true);
}

/* Mark the given memory range as unneeded, potentially zeroing it,
   without releasing the address space reservation and without
   guaranteeing that the memory will be zero when read.  It is legal
   for this function to be a no-op.  */
void
emacs_discard_memory (void *const mem, const size_t size)
{
  emacs_zero_or_discard_memory (mem, size, /*zero=*/false);
}

void
emacs_mmap_discard_contents (struct emacs_memory_map *map)
{
  if (map->mapping)
    emacs_discard_memory (map->mapping, map->spec.size);
}

void
emacs_mmap_release (struct emacs_memory_map *map)
{
  map->mapping = NULL;
  map->unmap = NULL;
  map->private = NULL;
}

void
emacs_mmap_unmap (struct emacs_memory_map *map)
{
  if (map->unmap)
    map->unmap (map);
  emacs_mmap_release (map);
}

/* Allows heap-allocated emacs_mmap to "free" maps individually.  */
struct emacs_memory_map_heap_control_block
{
  int refcount;
  void *mem;
};

static void
emacs_mm_heap_cb_release (struct emacs_memory_map_heap_control_block *cb)
{
  eassert (cb->refcount > 0);
  if (--cb->refcount == 0)
    {
      free (cb->mem);
      free (cb);
    }
}

static void
emacs_mmap_unmap_heap (struct emacs_memory_map *map)
{
  emacs_mm_heap_cb_release (map->private);
}

/* Implement emacs_mmap using malloc and read.  */
static bool
emacs_mmap_contiguous_heap (struct emacs_memory_map *const maps,
                            const int nr_maps,
                            size_t total_size,
                            size_t alignment)
{
  bool ret = false;
  struct emacs_memory_map_heap_control_block *cb = calloc (1, sizeof (*cb));

  /* Find a total_size and alignment figure that works for all aligned
     allocation functions: find an alignment that's both a multiple of
     sizeof (void *) and a power of two, then make the total size a
     multiple of the alignment.  */
  if (!try_round_up (alignment, sizeof (void *), &alignment))
    return false;
  if (!try_next_pow2 (alignment, &alignment))
    return false;
  if (!try_round_up (total_size, alignment, &total_size))
    return false;

  char *mem;
  if (!cb)
    goto out;
  cb->refcount = 1;
  cb->mem = aligned_alloc (alignment, total_size);
  if (!cb->mem)
    goto out;
  mem = cb->mem;
  eassert ((uintptr_t) mem % alignment == 0);

  for (int i = 0; i < nr_maps; ++i)
    {
      struct emacs_memory_map *const map = &maps[i];
      const struct emacs_memory_map_spec spec = map->spec;
      if (!spec.size)
        continue;
      map->mapping = mem;
      mem += spec.size;
      map->unmap = emacs_mmap_unmap_heap;
      map->private = cb;
      cb->refcount += 1;
      if (spec.fd < 0)
        memset (map->mapping, 0, spec.size);
      else
        {
          if (lseek (spec.fd, spec.offset, SEEK_SET) < 0)
            goto out;
          const ssize_t nb = emacs_read_all_nolisp (
            spec.fd, map->mapping, spec.size);
          if (nb >= 0 && nb != spec.size)
            errno = EIO;
          if (nb != spec.size)
            goto out;
        }
    }

  ret = true;
 out:
  emacs_mm_heap_cb_release (cb);
  if (!ret)
    for (int i = 0; i < nr_maps; ++i)
      emacs_mmap_unmap (&maps[i]);
  return ret;
}

static void
emacs_mmap_unmap_vm (struct emacs_memory_map *map)
{
  if (map->spec.fd < 0)
    emacs_unmap_anonymous (map->mapping, map->spec.size);
  else
    emacs_unmap_file (map->mapping, map->spec.size);
}

static bool
mmap_is_atomic (void)
{
#if defined _AIX
  /* TODO: in AIX, detect whether we're in SPEC1170 mode.  If we are,
     mmap is atomic and we should return false here.  If not, we
     behave as POSIX and we shoul return true here.  */
  return false;
#elif defined CYGWIN || VM_SUPPORTED == VM_MS_WINDOWS
  /* TODO: MS-Windows now supports a "placeholder" VMA style
     that  */
#else
  return true;
#endif
}

static bool
is_mmap_conflict_errno (void)
{
  return !mmap_is_atomic () && (errno == EBUSY
#ifdef CYGWIN
                                || errno == EINVAL
#endif
                                );
}

static bool
emacs_mmap_contiguous_vm_1 (struct emacs_memory_map *const maps,
                            const int nr_maps,
                            size_t alloc_size, size_t alignment)
{
  bool success = false;
  if (!try_round_up (alloc_size, EMACS_ALLOCATION_GRANULARITY, &alloc_size))
    return false;
  if (alignment < EMACS_ALLOCATION_GRANULARITY)
    alignment = EMACS_ALLOCATION_GRANULARITY;
  size_t resv_size;
  if (!try_round_up (alloc_size, alignment, &resv_size))
    return false;
  eassert (resv_size % alignment == 0);
  if (alignment > EMACS_ALLOCATION_GRANULARITY)
    {
      if (INT_MULTIPLY_WRAPV (resv_size, 2, &resv_size))
        {
          errno = ERANGE;
          return false;
        }
      resv_size -= EMACS_ALLOCATION_GRANULARITY;
    }
  void *resv = emacs_anonymous_allocate (
    NULL,
    resv_size,
    EMACS_MEMORY_ACCESS_NONE,
    /*populate=*/false);
  if (!resv)
    return false;
  const uintptr_t resv_la = (uintptr_t) resv;
  const uintptr_t resv_end_la = resv_la + resv_size;
  eassume (resv_la % EMACS_ALLOCATION_GRANULARITY == 0);

  if (!mmap_is_atomic ())
    {
      /* MS-Windows lacks atomic mapping replace; need to release the
         reservation so we can allocate within it.  Will retry the
         loop if someone squats on our address space before we can
         finish allocation.  On POSIX systems, we leave the
         reservation around for atomicity.  */
      emacs_unmap_anonymous (resv, resv_size);
      resv = NULL;
    }

  /* If we need larger-than-normal alignment, find a window within the
     reservation region that's aligned the way we need it to be
     aligned.  */
  const uintptr_t alloc_start_la = resv_la % alignment
    ? resv_la + (alignment - (resv_la % alignment)) : resv_la;

  eassume (alloc_start_la >= resv_la);
  eassume ((alloc_start_la - resv_la) % EMACS_ALLOCATION_GRANULARITY == 0);

  uintptr_t alloc_end_la = alloc_start_la;
  for (int i = 0; i < nr_maps; ++i)
    {
      struct emacs_memory_map *map = &maps[i];
      const struct emacs_memory_map_spec spec = map->spec;
      if (!spec.size)
        continue;
      const bool is_last_mapping = (i == nr_maps - 1);
      const size_t spec_size = is_last_mapping
        ? ROUNDUP (spec.size, EMACS_ALLOCATION_GRANULARITY)
        : spec.size;
      eassume (spec_size == spec.size || is_last_mapping);

      if (spec.fd < 0)
        map->mapping = emacs_anonymous_allocate ((void *) alloc_end_la,
                                                 spec_size,
                                                 spec.protection,
                                                 spec.populate);
      else
        map->mapping = emacs_map_file ((void *) alloc_end_la,
                                       spec.fd, spec.offset,
                                       spec_size, spec.protection,
                                       spec.populate);
      if (!map->mapping)
        goto out;
      map->unmap = emacs_mmap_unmap_vm;
      alloc_end_la += spec_size;
    }

  success = true;
 out:
  eassume (alloc_end_la <= resv_end_la);
  eassume ((resv_end_la - alloc_end_la) % EMACS_ALLOCATION_GRANULARITY == 0);

  const int saved_errno = errno;
  if (resv)
    {
      if (alloc_start_la > resv_la)
        emacs_unmap_anonymous (resv, alloc_start_la - resv_la);
      if (alloc_end_la < resv_end_la)
        emacs_unmap_anonymous ((void *) alloc_end_la, resv_end_la - alloc_end_la);
    }
  if (!success)
    for (int i = 0; i < nr_maps; ++i)
      emacs_mmap_unmap (&maps[i]);
  errno = saved_errno;
  return success;
}

static bool
emacs_mmap_contiguous_vm (struct emacs_memory_map *const maps,
                          const int nr_maps,
                          const size_t alloc_size, const size_t alignment)
{
  bool success;
  do
    success = emacs_mmap_contiguous_vm_1 (maps, nr_maps, alloc_size, alignment);
  while (!success && is_mmap_conflict_errno ());
  return success;
}

/* Map a range of addresses into a chunk of contiguous memory.

   Each emacs_memory_map structure describes how to fill the
   corresponding range of memory. On input, all members except MAPPING
   are valid. On output, MAPPING contains the location of the given
   chunk of memory. The MAPPING for MAPS[N] is MAPS[N-1].mapping +
   MAPS[N-1].size.

   Each mapping SIZE (except for the last mapping) must be a multiple
   of the system allocation granularity EMACS_ALLOCATION_GRANULARITY.
   (N.B.  the allocation granularity can be larger than the page size
   on some systems, but is always a multiple of the page size.)
   Return true on success or false on failure with errno set.

   ALIGNMENT is the required alignment of the overall sequence of
   memory regions.  ALIGNMENT must be a power of two and a multiple of
   sizeof (void*).
   */
bool
emacs_mmap_contiguous (struct emacs_memory_map *maps,
                       int nr_maps,
                       size_t alignment)
{
  if (!nr_maps)
    return true;

  size_t total_size = 0;

  for (int i = 0; i < nr_maps; ++i)
    {
      eassert (maps[i].mapping == NULL);
      eassert (maps[i].unmap == NULL);
      eassert (maps[i].private == NULL);
      if (i != nr_maps - 1)
        eassert (maps[i].spec.size % EMACS_ALLOCATION_GRANULARITY == 0);
      if (INT_ADD_OVERFLOW (total_size, maps[i].spec.size))
        {
          errno = ERANGE;
          return false;
        }
      total_size += maps[i].spec.size;
    }

  return (VM_SUPPORTED
          ? emacs_mmap_contiguous_vm
          : emacs_mmap_contiguous_heap)
    (maps, nr_maps, total_size, alignment);
}

void
emacs_set_memory_protection (void *const mem,
                             const size_t nr_bytes,
                             enum emacs_memory_protection protection)
{
#if VM_SUPPORTED == VM_POSIX
  switch (protection)
    {
    case EMACS_MEMORY_ACCESS_NONE:
      if (mprotect (mem, nr_bytes, PROT_NONE))
        emacs_abort ();
      return;
    case EMACS_MEMORY_ACCESS_READ:
      if (mprotect (mem, nr_bytes, PROT_READ))
        emacs_abort ();
      return;
    case EMACS_MEMORY_ACCESS_READWRITE:
      if (mprotect (mem, nr_bytes, PROT_READ | PROT_WRITE))
        emacs_abort ();
      return;
    }
#elif VM_SUPPORTED == VM_MS_WINDOWS
  switch (protection)
    {
    case EMACS_MEMORY_ACCESS_NONE:
      if (!VirtualProtect (mem, nr_bytes, PAGE_NOACCESS, NULL))
        emacs_abort ();
      return;
    case EMACS_MEMORY_ACCESS_READ:
      if (!VirtualProtect (mem, nr_bytes, PAGE_READONLY, NULL))
        emacs_abort ();
      return;
    case EMACS_MEMORY_ACCESS_READWRITE:
      if (!VirtualProtect (mem, nr_bytes, PAGE_READWRITE, NULL))
        emacs_abort ();
      return;
    }
#endif
  emacs_unreachable ();  /* Unsupported */
}
