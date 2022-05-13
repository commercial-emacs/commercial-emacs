#ifndef EMACS_ALLOC_H
#define EMACS_ALLOC_H

#include <config.h>

#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <limits.h>		/* For CHAR_BIT.  */
#include <signal.h>		/* For SIGABRT, SIGDANGER.  */
#include <sys/mman.h>

#ifdef HAVE_PTHREAD
#include <pthread.h>
#endif

#include "lisp.h"
#include "bignum.h"
#include "dispextern.h"
#include "intervals.h"
#include "puresize.h"
#include "sheap.h"
#include "sysstdio.h"
#include "systime.h"
#include "character.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "frame.h"
#include "blockinput.h"
#include "pdumper.h"
#include "termhooks.h"		/* For struct terminal.  */
#include "bitset.h"
#include "getpagesize.h"
#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

#ifdef HAVE_SQLITE3
#include "sqlite.h"
#endif

#ifdef HAVE_TREE_SITTER
#include "tree-sitter.h"
#endif

#include <flexmember.h>
#include <verify.h>
#include <execinfo.h>           /* For backtrace.  */

#ifdef HAVE_LINUX_SYSINFO
#include <sys/sysinfo.h>
#endif

#ifdef MSDOS
#include "dosfns.h"		/* For dos_memory_info.  */
#endif

#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif

#if (defined ENABLE_CHECKING \
     && defined HAVE_VALGRIND_VALGRIND_H && !defined USE_VALGRIND)
# define USE_VALGRIND 1
#endif

#if USE_VALGRIND
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>
#endif

/* GC_CHECK_MARKED_OBJECTS means do sanity checks on allocated objects.
   We turn that on by default when ENABLE_CHECKING is defined;
   define GC_CHECK_MARKED_OBJECTS to zero to disable.  */

#if defined ENABLE_CHECKING && !defined GC_CHECK_MARKED_OBJECTS
# define GC_CHECK_MARKED_OBJECTS 1
#endif

/* GC_MALLOC_CHECK defined means perform validity checks of malloc'd
   memory.  Can do this only if using gmalloc.c and if not checking
   marked objects.  */

#if (defined SYSTEM_MALLOC || defined HYBRID_MALLOC || GC_CHECK_MARKED_OBJECTS)
#undef GC_MALLOC_CHECK
#endif

#include <unistd.h>
#include <fcntl.h>

#ifdef USE_GTK
# include "gtkutil.h"
#endif
#ifdef WINDOWSNT
#include "w32.h"
#include "w32heap.h"	/* for sbrk */
#endif

/* A type with alignment at least as large as any object that Emacs
   allocates.  This is not max_align_t because some platforms (e.g.,
   mingw) have buggy malloc implementations that do not align for
   max_align_t.  This union contains types of all GCALIGNED_STRUCT
   components visible here.  */
union emacs_align_type
{
  struct frame frame;
  struct Lisp_Bignum Lisp_Bignum;
  struct Lisp_Bool_Vector Lisp_Bool_Vector;
  struct Lisp_Char_Table Lisp_Char_Table;
  struct Lisp_CondVar Lisp_CondVar;
  struct Lisp_Finalizer Lisp_Finalizer;
  struct Lisp_Float Lisp_Float;
  struct Lisp_Hash_Table Lisp_Hash_Table;
  struct Lisp_Marker Lisp_Marker;
  struct Lisp_Misc_Ptr Lisp_Misc_Ptr;
  struct Lisp_Mutex Lisp_Mutex;
  struct Lisp_Overlay Lisp_Overlay;
  struct Lisp_Sub_Char_Table Lisp_Sub_Char_Table;
  struct Lisp_Subr Lisp_Subr;
  struct Lisp_User_Ptr Lisp_User_Ptr;
  struct Lisp_Vector Lisp_Vector;
  struct terminal terminal;
  struct thread_state thread_state;
  struct window window;
#ifdef HAVE_TREE_SITTER
  struct Lisp_Tree_Sitter tree_sitter;
  struct Lisp_Tree_Sitter_Node tree_sitter_node;
#endif

  /* Omit the following since they would require including process.h
     etc.  In practice their alignments never exceed that of the
     structs already listed.  */
#if 0
  struct Lisp_Module_Function Lisp_Module_Function;
  struct Lisp_Process Lisp_Process;
  struct save_window_data save_window_data;
  struct scroll_bar scroll_bar;
  struct xwidget_view xwidget_view;
  struct xwidget xwidget;
#endif
};

/* LISP_ALIGNMENT must be at least GCALIGNMENT to allow USE_LSB_TAG.
   Further, it must also be as wide as alignof (emacs_align_type)
   which encompasses all pseudovector lisp fields.  */

typedef union {
  union emacs_align_type x;
  GCALIGNED_UNION_MEMBER
} Lisp_Aligned;

extern struct Lisp_String *(*static_string_allocator) (void);

enum { LISP_ALIGNMENT = alignof (Lisp_Aligned) };

struct mem_node *mem_find (void *start);

void test_me (void);

#endif  /* EMACS_ALLOC_H */
