#ifndef EMACS_ILIST_H
#define EMACS_ILIST_H 1

/* Intrusive linked list */

#include <config.h>
#include <stdint.h>
#include "lisp.h"

typedef struct emacs_list_link emacs_list_link;
typedef struct emacs_list_head emacs_list_head;

struct emacs_list_link {
  emacs_list_link* next;
  emacs_list_link* prev;
};

struct emacs_list_head {
  struct emacs_list_link link;
};

INLINE void
emacs_list_check (const emacs_list_head *const lh)
{
  eassume (lh->link.next);
  eassume (lh->link.prev);
}

INLINE bool
emacs_list_empty_p (const emacs_list_head *const lh)
{
  emacs_list_check (lh);
  return lh->link.next == &lh->link;
}

INLINE bool
emacs_list_first_p (const emacs_list_head *const lh,
                   const emacs_list_link *const ll)
{
  return ll->prev == &lh->link;
}

INLINE bool
emacs_list_last_p (const emacs_list_head *const lh,
                   const emacs_list_link *const ll)
{
  return ll->next == &lh->link;
}

INLINE void
emacs_list_head_init (emacs_list_head *const lh)
{
  lh->link.next = lh->link.prev = &lh->link;
  emacs_list_check (lh);
}

#define EMACS_LIST_HEAD_INITIALIZER(var) { { &var.link, &var.link } }
#define EMACS_LIST_INVALID_LINK ((emacs_list_link *) -1)

INLINE void
emacs_list_link_init (emacs_list_link *const ll)
{
  if (enable_checking)
    ll->next = ll->prev = EMACS_LIST_INVALID_LINK;
}

INLINE void
emacs_list_check_link_uninitialized (const emacs_list_link *const ll)
{
  if (enable_checking)
    {
      eassume (ll->next == EMACS_LIST_INVALID_LINK);
      eassume (ll->prev == EMACS_LIST_INVALID_LINK);
    }
}

INLINE void
emacs_list_check_link_initialized (const emacs_list_link *const ll)
{
  if (enable_checking)
    {
      eassume (ll->next != EMACS_LIST_INVALID_LINK);
      eassume (ll->prev != EMACS_LIST_INVALID_LINK);
    }
}

INLINE emacs_list_link *
emacs_list_first (emacs_list_head *const lh)
{
  emacs_list_check (lh);
  eassume (lh->link.next != &lh->link);
  return lh->link.next;
}

INLINE emacs_list_link *
emacs_list_last (emacs_list_head *const lh)
{
  emacs_list_check (lh);
  eassume (lh->link.prev != &lh->link);
  return lh->link.prev;
}

INLINE void
emacs_list_insert_before (
  emacs_list_link *const place, emacs_list_link *const ll)
{
  emacs_list_check_link_uninitialized (ll);
  ll->prev = place->prev;
  ll->next = place;
  place->prev->next = ll;
  place->prev = ll;
}

INLINE void
emacs_list_insert_after (
  emacs_list_link *const place, emacs_list_link *const ll)
{
  emacs_list_check_link_uninitialized (ll);
  ll->prev = place;
  ll->next = place->next;
  place->next->prev = ll;
  place->next = ll;
}

INLINE void
emacs_list_insert_first (
  emacs_list_head *const lh, emacs_list_link *const ll)
{
  emacs_list_check (lh);
  emacs_list_insert_after (&lh->link, ll);
}

INLINE void
emacs_list_insert_last (
  emacs_list_head *const lh,
  emacs_list_link *const ll)
{
  emacs_list_check (lh);
  emacs_list_insert_before (&lh->link, ll);
}

INLINE void
emacs_list_remove (emacs_list_link *const ll)
{
  emacs_list_check_link_initialized (ll);
  ll->prev->next = ll->next;
  ll->next->prev = ll->prev;
  emacs_list_link_init (ll);
}

INLINE emacs_list_link *
emacs_list_pop_first (emacs_list_head *const lh)
{
  emacs_list_link *const first = emacs_list_first (lh);
  emacs_list_remove (first);
  return first;
}

INLINE emacs_list_link *
emacs_list_pop_last (emacs_list_head *const lh)
{
  emacs_list_link *const last = emacs_list_last (lh);
  emacs_list_remove (last);
  return last;
}

#endif /* EMACS_ILIST_H */
