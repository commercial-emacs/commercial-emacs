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
  struct Lisp_Tree_Sitter_Cursor tree_sitter_cursor;
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

enum { LISP_ALIGNMENT = alignof (Lisp_Aligned) };

enum _GL_ATTRIBUTE_PACKED mem_type
{
  MEM_TYPE_NON_LISP,
  MEM_TYPE_CONS,
  MEM_TYPE_STRING,
  MEM_TYPE_SYMBOL,
  MEM_TYPE_FLOAT,
  /* Includes vectors but not non-bool vectorlikes. */
  MEM_TYPE_VECTORLIKE,
  /* Non-bool vectorlikes.  */
  MEM_TYPE_VBLOCK,
  MEM_TYPE_INTERVAL,
  MEM_TYPE_NTYPES,
};

enum _GL_ATTRIBUTE_PACKED Space_Type
  {
    Space_Symbol = Lisp_Symbol,
    Space_String = Lisp_String,
    Space_Vectorlike = Lisp_Vectorlike,
    Space_Cons = Lisp_Cons,
    Space_Float = Lisp_Float,
    Space_Interval,
    Space_Type_Max,
  };

#define XMARK_VECTOR(V)		((V)->header.size |= ARRAY_MARK_FLAG)
#define XUNMARK_VECTOR(V)	((V)->header.size &= ~ARRAY_MARK_FLAG)
#define XVECTOR_MARKED_P(V)	(((V)->header.size & ARRAY_MARK_FLAG) != 0)

#define XMARK_STRING(S)		((S)->u.s.size |= ARRAY_MARK_FLAG)
#define XUNMARK_STRING(S)	((S)->u.s.size &= ~ARRAY_MARK_FLAG)
#define XSTRING_MARKED_P(S)	(((S)->u.s.size & ARRAY_MARK_FLAG) != 0)

typedef struct sdata
{
  /* Back-pointer to the Lisp_String whose u.s.data points to DATA.  */
  struct Lisp_String *string;
  ptrdiff_t nbytes;
  unsigned char data[FLEXIBLE_ARRAY_MEMBER];
} sdata;

#define SDATA_OF_LISP_STRING(S) \
  ((sdata *) ((S)->u.s.data - FLEXSIZEOF (struct sdata, data, 0)))

struct mem_node *mem_find (void *start);

enum { BLOCK_NOT_FOUND = EMACS_INT_MAX };

/* Analogous to pdumper_object_p().  Return whether the OBJ points
   into a copy-collector block for getting a reprieve from
   GC_CHECK_MARKED_OBJECTS.  */
_GL_ATTRIBUTE_CONST bool mgc_xpntr_p (const void *obj);
_GL_ATTRIBUTE_CONST bool wrong_xpntr_p (const void *obj);

void mgc_flip_space (void);

void *mgc_flip_xpntr (void *xpntr, enum Space_Type objtype);

void *mgc_fwd_xpntr (const void *addr);

void mgc_initialize_spaces (void);

enum Space_Type mgc_find_xpntr (void *p, void **xpntr);

#endif  /* EMACS_ALLOC_H */
