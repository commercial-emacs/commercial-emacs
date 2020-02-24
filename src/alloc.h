#ifndef EMACS_ALLOC_H
#define EMACS_ALLOC_H
#include "lisp.h"

#ifdef USE_GTK
#include "gtkutil.h"
#endif

typedef enum gc_phase gc_phase;
struct thread_state;
union specbinding;
struct image_cache;
struct frame;

//
enum gc_phase {
  GC_PHASE_NOT_IN_PROGRESS = -2,
  GC_PHASE_PREPARE = -1,
  GC_PHASE_MARK = 0,
  GC_PHASE_PLAN_SWEEP = 2,
  GC_PHASE_SWEEP = 1,
  GC_PHASE_CLEANUP = 3,
};

extern void xscan_reference (Lisp_Object *, gc_phase);
extern void xscan_reference_pinned (Lisp_Object, gc_phase);

extern void xscan_reference_pointer_to_vectorlike_2 (union vectorlike_header **, gc_phase);
INLINE void xscan_reference_pointer_to_vectorlike_1 (
  void *const ptr, union vectorlike_header *const hdr, const gc_phase phase)
{
  (void) hdr;
  xscan_reference_pointer_to_vectorlike_2 (ptr, phase);
}

#define xscan_reference_pointer_to_vectorlike(x, phase)                  \
  xscan_reference_pointer_to_vectorlike_1 ((x), &(*(x))->header, phase)

extern void xscan_vector_lisp_fields (union vectorlike_header *, gc_phase phase);
extern void sweep_pdumper_object (void *, enum Lisp_Type);

extern bool gc_object_limit_try_increase (size_t delta);
extern void xscan_maybe_objects (Lisp_Object const *, ptrdiff_t, gc_phase);
extern void xscan_stack (char const *, char const *, gc_phase);
extern void flush_stack_call_func (void (*func) (void *arg), void *arg);

extern void scan_kboards (gc_phase);
extern void scan_thread (struct thread_state *, gc_phase);
extern void scan_thread_roots (gc_phase);
extern void scan_fringe_data (gc_phase);
extern void scan_modules (gc_phase);
extern void scan_specpdl (union specbinding *, union specbinding *, gc_phase);
extern void scan_image_cache (struct image_cache *, gc_phase);

extern void scan_dispnew_roots (gc_phase);
extern void scan_marker_roots (gc_phase);
extern void scan_xdisp_roots (gc_phase);
extern void scan_syntax_roots (gc_phase);
extern void scan_process_roots (gc_phase);

struct tty_display_info;
struct x_display_info;
struct w32_display_info;
struct ns_display_info;

extern void scan_terminal_display_info_tty (struct tty_display_info *,
                                            gc_phase);
extern void scan_terminal_display_info_x (struct x_display_info *,
                                          gc_phase);
extern void scan_terminal_display_info_w32 (struct w32_display_info *,
                                            gc_phase);
extern void scan_terminal_display_info_ns (struct ns_display_info *,
                                           gc_phase);

#ifdef USE_GTK
extern void xg_scan_data (gc_phase);
extern void xg_scan_frame (struct frame *, gc_phase);
struct _EmacsFixedPrivate;
extern void xg_scan_frame_widget (GtkWidget *, gc_phase);
#endif

#endif  /* EMACS_ALLOC_H */
