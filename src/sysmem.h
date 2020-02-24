#ifndef EMACS_SYSMEM_H
#define EMACS_SYSMEM_H 1

#include <config.h>
#include "lisp.h"

/* XXX: work with bigger page sizes */
enum { EMACS_PAGE_SIZE_MIN = 4096 };
enum { EMACS_PAGE_SIZE_MAX = 4096 };

/* Coarseness of mmap.  */
#if defined (WINDOWSNT) || defined (CYGWIN)
enum { EMACS_ALLOCATION_GRANULARITY = 64 * 1024 };
#else
enum { EMACS_ALLOCATION_GRANULARITY = EMACS_PAGE_SIZE_MAX };
#endif

enum emacs_memory_protection
{
  EMACS_MEMORY_ACCESS_NONE = 1,
  EMACS_MEMORY_ACCESS_READ = 2,
  EMACS_MEMORY_ACCESS_READWRITE = 3,
};

struct emacs_memory_map_spec
{
  int fd;  /* File to map; anon if negative.  */
  size_t size;  /* Number of bytes to map.  */
  off_t offset;  /* Offset within fd.  */
  ENUM_BF (emacs_memory_protection) protection;
  bool_bf populate : 1;
};

struct emacs_memory_map
{
  struct emacs_memory_map_spec spec;
  void *mapping;  /* Actual mapped memory.  */
  void (*unmap) (struct emacs_memory_map *);
  void *private; /* Data for the unmap function.  */
};

bool emacs_mmap_contiguous (struct emacs_memory_map *, int, size_t);
void emacs_mmap_discard_contents (struct emacs_memory_map *);
void emacs_mmap_release (struct emacs_memory_map *);
void emacs_mmap_unmap (struct emacs_memory_map *);

void emacs_zero_memory (void *, size_t);
void emacs_discard_memory (void *, size_t);

void emacs_set_memory_protection (void *, size_t,
                                  enum emacs_memory_protection);

#endif /* EMACS_SYSMEM_H */
