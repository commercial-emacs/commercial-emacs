/* blockinterrupts.h - 1990s-era interrupt blocking.
   Copyright (C) 1989, 1993, 2001-2023 Free Software Foundation, Inc.

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

#ifndef EMACS_BLOCKINTERRUPTS_H
#define EMACS_BLOCKINTERRUPTS_H

INLINE_HEADER_BEGIN

/* This module was formerly known as block_input(), with "input" being
   short for "X input signal."  But "signal" was the salient word, not
   "input."

   Fence uninterruptible code with block_interrupts() and
   unblock_interrupts().

   Should interrupts_blocked_p() be true, interrupt handling code can set
   `pending_signals' true, which causes unblock_interrupts() to catch up
   processing interrupts and timers.  */

extern volatile int interrupts_blocked;

/* Begin critical section. */

INLINE void
block_interrupts (void)
{
  interrupts_blocked++;
}

extern void unblock_interrupts (void);
extern void totally_unblock_interrupts (void);
extern void unblock_interrupts_to (int);

/* In critical section?  */

INLINE bool
interrupts_blocked_p (void)
{
  return interrupts_blocked > 0;
}

INLINE_HEADER_END

#endif /* EMACS_BLOCKINTERRUPTS_H */
