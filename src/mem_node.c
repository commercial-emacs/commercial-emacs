#include "mem_node.h"
#include "lisp.h"

/* Conservative stack scanning (mark_maybe_pointer) needs to
   trace an arbitrary address back to its respective memory block.

   To make these searches efficient, new blocks are stored in a global
   red-black tree which is "fixed" after every insertion or deletion
   such that:

   1. Every node is either red or black.
   2. Every leaf is black.
   3. If a node is red, then both its children are black.
   4. Every simple path from a node to a descendant leaf contains
      the same number of black nodes.
   5. The root is always black.

   These invariants balance the tree so that its height can be no
   greater than 2 log(N+1), where N is the number of internal nodes.
   Searches, insertions and deletions are done in O(log N).
  */

static struct mem_node _mem_nil;
struct mem_node *mem_nil = &_mem_nil;

static void mem_insert_fixup (struct mem_node *, struct mem_node **root);
static void mem_delete_fixup (struct mem_node *, struct mem_node **root);
static void mem_rotate_left (struct mem_node *, struct mem_node **root);
static void mem_rotate_right (struct mem_node *, struct mem_node **root);

struct mem_node *
mem_find (struct thread_state *thr, void *start)
{
  struct mem_node *p = THREAD_FIELD (thr, m_mem_root);
  while (p != mem_nil)
    {
      if (start < p->start)
	p = p->left;
      else if (start >= p->end)
	p = p->right;
      else
	break;
    }
  return p;
}

struct mem_node *
mem_find_which_thread (const void *start, struct thread_state **which)
{
  struct mem_node *p = mem_nil;
  for (struct thread_state *thr = all_threads;
       thr != NULL && p == mem_nil;
       thr = thr->next_thread)
    {
      p = thr->m_mem_root;
      while (p != mem_nil)
	{
	  if (start < p->start)
	    p = p->left;
	  else if (start >= p->end)
	    p = p->right;
	  else
	    {
	      if (which)
		*which = thr;
	      break;
	    }
	}
    }
  return p;
}

/* Destroy tree at ROOT.  */

void
mem_delete_root (struct mem_node **root)
{
  if (*root == mem_nil)
    return;
  mem_delete_root (&((*root)->left));
  mem_delete_root (&((*root)->right));
  mem_delete (*root, root);
}

/* Merge FROM into INTO.  */

void
mem_merge_into (struct mem_node **into, struct mem_node *from)
{
  if (*into == from || from == mem_nil)
    return;
  mem_insert (from->start, from->end, from->type, into);
  mem_merge_into (into, from->left);
  mem_merge_into (into, from->right);
}

/* Insert node representing mem block of TYPE spanning START and END.
   Return the inserted node.  */

struct mem_node *
mem_insert (void *start, void *end, enum mem_type type, struct mem_node **root)
{
  for (struct mem_node *node = *root, *parent = NULL; ; )
    {
      if (node == mem_nil)
	{
	  // insert here
	  struct mem_node *x = xmalloc (sizeof *x);
	  x->parent = parent;
	  x->start = start;
	  x->end = end;
	  x->type = type;
	  x->left = x->right = mem_nil;
	  x->color = MEM_RED;

	  // establish linkages
	  if (node == *root)
	    *root = x;
	  else if (start < parent->start)
	    parent->left = x;
	  else
	    parent->right = x;

	  mem_insert_fixup (x, root);
	  return x;
	}
      else
	{
	  parent = node;
	  node = start < node->start ? node->left : node->right;
	}
    }
  emacs_abort (); // should have returned by now
}

/* Insert node X, then rebalance red-black tree.  X is always red.  */

static void
mem_insert_fixup (struct mem_node *x, struct mem_node **root)
{
  while (x != *root && x->parent->color == MEM_RED)
    {
      /* Ensure property 3, if node is red, children black. */
      if (x->parent == x->parent->parent->left)
	{
	  /* X is a left grand-child, and Y is uncle.  */
	  struct mem_node *y = x->parent->parent->right;

	  if (y->color == MEM_RED)
	    {
	      /* Parent and uncle are red.  Change both to black
		 because X is red.  Then proceed with grandparent.  */
	      x->parent->color = MEM_BLACK;
	      y->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      x = x->parent->parent;
            }
	  else
	    {
	      /* Parent is red but uncle is black.  */
	      if (x == x->parent->right)
		{
		  /* collapsing a degenerate self-sibling? */
		  x = x->parent;
		  mem_rotate_left (x, root);
                }

	      x->parent->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      mem_rotate_right (x->parent->parent, root);
            }
        }
      else
	{
	  /* Symmetrical where X is right grand-child  */
	  struct mem_node *y = x->parent->parent->left;

	  if (y->color == MEM_RED)
	    {
	      x->parent->color = MEM_BLACK;
	      y->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      x = x->parent->parent;
            }
	  else
	    {
	      if (x == x->parent->left)
		{
		  x = x->parent;
		  mem_rotate_right (x, root);
		}

	      x->parent->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      mem_rotate_left (x->parent->parent, root);
            }
        }
    }
  (*root)->color = MEM_BLACK;   /* Invariant: root is black.  */
}

/*   (x)                   (y)
     / \                   / \
    a   (y)      ===>    (x)  c
        / \              / \
       b   c            a   b  */

static void
mem_rotate_left (struct mem_node *x, struct mem_node **root)
{
  struct mem_node *y;

  /* Turn y's left sub-tree into x's right sub-tree.  */
  y = x->right;
  x->right = y->left;
  if (y->left != mem_nil)
    y->left->parent = x;

  /* Y's parent was x's parent.  */
  if (y != mem_nil)
    y->parent = x->parent;

  /* Get the parent to point to y instead of x.  */
  if (x->parent)
    {
      if (x == x->parent->left)
	x->parent->left = y;
      else
	x->parent->right = y;
    }
  else
    *root = y;

  /* Put x on y's left.  */
  y->left = x;
  if (x != mem_nil)
    x->parent = y;
}

/*     (x)                (Y)
       / \                / \
     (y)  c      ===>    a  (x)
     / \                    / \
    a   b                  b   c  */

static void
mem_rotate_right (struct mem_node *x, struct mem_node **root)
{
  struct mem_node *y = x->left;

  x->left = y->right;
  if (y->right != mem_nil)
    y->right->parent = x;

  if (y != mem_nil)
    y->parent = x->parent;
  if (x->parent)
    {
      if (x == x->parent->right)
	x->parent->right = y;
      else
	x->parent->left = y;
    }
  else
    *root = y;

  y->right = x;
  if (x != mem_nil)
    x->parent = y;
}

void
mem_delete (struct mem_node *z, struct mem_node **root)
{
  struct mem_node *x, *y;

  if (!z || z == mem_nil)
    return;

  if (z->left == mem_nil || z->right == mem_nil)
    y = z;
  else
    {
      y = z->right;
      while (y->left != mem_nil)
	y = y->left;
    }

  if (y->left != mem_nil)
    x = y->left;
  else
    x = y->right;

  x->parent = y->parent;
  if (y->parent)
    {
      if (y == y->parent->left)
	y->parent->left = x;
      else
	y->parent->right = x;
    }
  else
    *root = x;

  if (y != z)
    {
      z->start = y->start;
      z->end = y->end;
      z->type = y->type;
    }

  if (y->color == MEM_BLACK)
    mem_delete_fixup (x, root);

  xfree (y);
}

/* Delete X, then rebalance red-black tree.  */

static void
mem_delete_fixup (struct mem_node *x, struct mem_node **root)
{
  while (x != *root && x->color == MEM_BLACK)
    {
      if (x == x->parent->left)
	{
	  struct mem_node *w = x->parent->right;

	  if (w->color == MEM_RED)
	    {
	      w->color = MEM_BLACK;
	      x->parent->color = MEM_RED;
	      mem_rotate_left (x->parent, root);
	      w = x->parent->right;
            }

	  if (w->left->color == MEM_BLACK && w->right->color == MEM_BLACK)
	    {
	      w->color = MEM_RED;
	      x = x->parent;
            }
	  else
	    {
	      if (w->right->color == MEM_BLACK)
		{
		  w->left->color = MEM_BLACK;
		  w->color = MEM_RED;
		  mem_rotate_right (w, root);
		  w = x->parent->right;
                }
	      w->color = x->parent->color;
	      x->parent->color = MEM_BLACK;
	      w->right->color = MEM_BLACK;
	      mem_rotate_left (x->parent, root);
	      x = *root;
            }
        }
      else
	{
	  struct mem_node *w = x->parent->left;

	  if (w->color == MEM_RED)
	    {
	      w->color = MEM_BLACK;
	      x->parent->color = MEM_RED;
	      mem_rotate_right (x->parent, root);
	      w = x->parent->left;
            }

	  if (w->right->color == MEM_BLACK && w->left->color == MEM_BLACK)
	    {
	      w->color = MEM_RED;
	      x = x->parent;
            }
	  else
	    {
	      if (w->left->color == MEM_BLACK)
		{
		  w->right->color = MEM_BLACK;
		  w->color = MEM_RED;
		  mem_rotate_left (w, root);
		  w = x->parent->left;
                }

	      w->color = x->parent->color;
	      x->parent->color = MEM_BLACK;
	      w->left->color = MEM_BLACK;
	      mem_rotate_right (x->parent, root);
	      x = *root;
            }
        }
    }

  x->color = MEM_BLACK;
}
