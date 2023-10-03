#ifndef MEM_NODE_H
#define MEM_NODE_H

#include <config.h>

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

struct mem_node
{
  /* Children of node, or mem_nil (but not NULL).  */
  struct mem_node *left, *right;

  /* The parent of this node.  In the root node, this is NULL.  */
  struct mem_node *parent;

  /* Start and end of allocated region.  */
  void *start, *end;

  /* Node color.  */
  enum {MEM_BLACK, MEM_RED} color;

  /* Memory type.  */
  enum mem_type type;
};

struct thread_state;
extern struct mem_node *mem_nil;
void mem_merge_into (struct mem_node **into, struct mem_node *from);
struct mem_node *mem_find (struct thread_state *thr, void *start);
void mem_delete_root (struct mem_node **root);
struct mem_node *mem_insert (void *, void *, enum mem_type, struct mem_node **root);
void mem_delete (struct mem_node *node, struct mem_node **root);
#ifdef ENABLE_CHECKING
struct mem_node *mem_find_which_thread (void *start, struct thread_state **which);
#endif

#ifdef HAVE_GCC_TLS
# define THREAD_FIELD(thr, field) (thr->field)
#else
# define THREAD_FIELD(thr, field) (main_thread->field)
#endif

#endif  /* MEM_NODE_H */
