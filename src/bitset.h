#ifndef BITSET_H
#define BITSET_H 1

#include <config.h>
#include <stdint.h>
#include "lisp.h"

// *At least* 32 bits with "best performance".  It's 64 bits for me.
typedef uint_fast32_t bitset_word;
enum { bitset_word_nbits = sizeof (bitset_word) * CHAR_BIT };

#define BITSET_WORDS(nr_bits)                           \
  (((nr_bits) + (bitset_word_nbits - 1))                       \
   / bitset_word_nbits)

INLINE bitset_word *
bitset_bit_nr (const bitset_word *const words,
                        const size_t nr_words,
                        const ptrdiff_t bitno)
{
  eassume (bitno >= 0);
  const size_t word_nr = bitno / bitset_word_nbits;
  eassume (word_nr < nr_words);
  return (bitset_word *) &words[word_nr];
}

/* Return whether the bit set identified by WORDS and NR_WORDS has
   a bit set at BITNO.  */
INLINE bool
bitset_bit_set_p (const bitset_word *const words,
                        const size_t nr_words,
                        const ptrdiff_t bitno)
{
  const bitset_word bit = ((bitset_word) 1) <<
    (bitno % bitset_word_nbits);
  return *bitset_bit_nr (words, nr_words, bitno) & bit;
}

INLINE void
bitset_set_bit_value (bitset_word *const words,
                             const size_t nr_words,
                             const ptrdiff_t bitno,
                             const bool bit_is_set)
{
  bitset_word *const bit_nr = bitset_bit_nr (
    words, nr_words, bitno);
  const bitset_word bit = ((bitset_word) 1) <<
    (bitno % bitset_word_nbits);
  if (bit_is_set)
    *bit_nr = *bit_nr | bit;
  else
    *bit_nr = *bit_nr & ~bit;
}

/* Set a bit in the bitset identified by WORDS and NR_WORDS.
   BITNO is the number of the bit to set.  */
INLINE void
bitset_set_bit (bitset_word *const words,
                      const size_t nr_words,
                      const ptrdiff_t bitno)
{
  bitset_set_bit_value (words, nr_words, bitno, true);
}

/* Clear a bit in the bitset identified by WORDS and NR_WORDS.
   BITNO is the number of the bit to clear.  */
INLINE void
bitset_clear_bit (bitset_word *const words,
                        const size_t nr_words,
                        const ptrdiff_t bitno)
{
  bitset_set_bit_value (words, nr_words, bitno, false);
}

/* emacs_clz_*: return the number of leading zero bits in VALUE.
   If VALUE is zero, return -1.  */

INLINE int
emacs_clz_ll (const unsigned long long value)
{
  return value ? __builtin_clzll (value) : 0;
}

INLINE int
emacs_clz_l (const unsigned long value)
{
  return value ? __builtin_clzl (value) : 0;
}

INLINE int
emacs_clz (const unsigned int value)
{
  return value ? __builtin_clz (value) : 0;
}

INLINE int
emacs_clz_z (const size_t value)
{
  if (sizeof (value) == sizeof (unsigned long long))
    return emacs_clz_ll ((unsigned long long) value);
  if (sizeof (value) == sizeof (unsigned long))
    return emacs_clz_l ((unsigned long) value);
  if (sizeof (value) == sizeof (unsigned int))
    return emacs_clz (value);
  emacs_unreachable ();
}

INLINE int
emacs_clz_bw (const bitset_word value)
{
  if (sizeof (value) == sizeof (unsigned long long))
    return emacs_clz_ll ((unsigned long long) value);
  if (sizeof (value) == sizeof (unsigned long))
    return emacs_clz_l ((unsigned long) value);
  if (sizeof (value) <= sizeof (unsigned int))
    return emacs_clz (value)
      - ((sizeof (unsigned int) - sizeof (bitset_word))
       * CHAR_BIT);
}

/* emacs_ctz_*: return the number of trailing zero bits in VALUE.  */

INLINE int
emacs_ctz_ll (const unsigned long long value)
{
  return value ? __builtin_ctzll (value) : 0;
}

INLINE int
emacs_ctz_l (const unsigned long value)
{
  return value ? __builtin_ctzl (value) : 0;
}

INLINE int
emacs_ctz (const unsigned int value)
{
  return value ? __builtin_ctz (value) : 0;
}

INLINE int
emacs_ctz_z (const size_t value)
{
  if (sizeof (value) == sizeof (unsigned long long))
    return emacs_ctz_ll ((unsigned long long) value);
  if (sizeof (value) == sizeof (unsigned long))
    return emacs_ctz_l ((unsigned long) value);
  if (sizeof (value) == sizeof (unsigned int))
    return emacs_ctz (value);
  emacs_unreachable ();
}

INLINE int
emacs_ctz_bw (const bitset_word value)
{
  if (sizeof (value) == sizeof (unsigned long long))
    return emacs_ctz_ll ((unsigned long long) value);
  if (sizeof (value) == sizeof (unsigned long))
    return emacs_ctz_l ((unsigned long) value);
  if (sizeof (value) == sizeof (unsigned int))
    return emacs_ctz (value);
  if (sizeof (value) <= sizeof (unsigned int))
    return emacs_ctz ((unsigned int) value);
  emacs_unreachable ();
}

/* emacs_popcount_*: return the number of one bits in VALUE.  */

INLINE int
emacs_popcount_ll (const unsigned long long value)
{
  return __builtin_popcountll (value);
}

INLINE int
emacs_popcount_l (const unsigned long value)
{
  return __builtin_popcountl (value);
}

INLINE int
emacs_popcount (const unsigned int value)
{
  return __builtin_popcount (value);
}

INLINE int
emacs_popcount_z (const size_t value)
{
  if (sizeof (value) == sizeof (unsigned long long))
    return emacs_popcount_ll ((unsigned long long) value);
  if (sizeof (value) == sizeof (unsigned long))
    return emacs_popcount_l ((unsigned long) value);
  if (sizeof (value) == sizeof (unsigned int))
    return emacs_popcount (value);
  emacs_unreachable ();
}

INLINE int
emacs_popcount_bw (const bitset_word value)
{
  if (sizeof (value) == sizeof (unsigned long long))
    return emacs_popcount_ll ((unsigned long long) value);
  if (sizeof (value) == sizeof (unsigned long))
    return emacs_popcount_l ((unsigned long) value);
  if (sizeof (value) == sizeof (unsigned int))
    return emacs_popcount (value);
  if (sizeof (value) <= sizeof (unsigned int))
    return emacs_popcount ((unsigned int) value);
  emacs_unreachable ();
}

/* Bitset scans.  */

INLINE
bitset_word
bitset_mask (int nr_set_low_bits)
{
  eassume (0 <= nr_set_low_bits);
  eassume (nr_set_low_bits < bitset_word_nbits);
  return (((bitset_word) 1) << nr_set_low_bits) - 1;
}

/* Find the first set bit in the range [START_BITNO, LIMIT_BITNO) in
   the bitset identified by WORDS and NR_WORDS.  START_BITNO must
   be less than or equal to LIMIT_BITNO, and LIMIT_BITNO must be less
   than or equal to the number of bits in the bit set.
   Return LIMIT_BITNO if we don't find a set bit.  */
INLINE
ptrdiff_t
bitset_scan_forward (const bitset_word *const words,
                           const size_t nr_words,
                           const ptrdiff_t start_bitno,
                           const ptrdiff_t limit_bitno)
{
  eassume (nr_words < (size_t) PTRDIFF_MAX);
  eassume (!INT_MULTIPLY_OVERFLOW ((ptrdiff_t) bitset_word_nbits,
                                   (ptrdiff_t) nr_words));
  const ptrdiff_t last_bit =
    bitset_word_nbits * (ptrdiff_t) nr_words;
  eassume (0 <= start_bitno
           && start_bitno <= limit_bitno
           && limit_bitno <= last_bit);
  if (start_bitno == limit_bitno)
    return limit_bitno;
  const ptrdiff_t first_wordno = start_bitno / bitset_word_nbits;
  const ptrdiff_t start_bitno_in_first_word =
    start_bitno % bitset_word_nbits;
  const ptrdiff_t last_wordno =
    (limit_bitno - 1) / bitset_word_nbits;
  const ptrdiff_t limit_bitno_in_last_word =
    limit_bitno % bitset_word_nbits;
  for (ptrdiff_t wordno = first_wordno; wordno <= last_wordno; ++wordno)
    {
      bitset_word word = words[wordno];
      /* First word: mask off the bits before the start.  */
      if (wordno == first_wordno)
        word &= ~bitset_mask (start_bitno_in_first_word);
      /* Last word: mask off bits at the limit_bitno and above.  */
      if (wordno == last_wordno && limit_bitno_in_last_word)
        word &= bitset_mask (limit_bitno_in_last_word);
      if (word)
        return (ptrdiff_t) wordno * bitset_word_nbits
          + emacs_ctz_bw (word);
    }
  return limit_bitno;
}

/* Find the last set bit in the range (LIMIT_BITNO, START_BITNO] in
   the bitset identified by WORDS and NR_WORDS.  START_BITNO must
   be less than the number of bits in the set, and LIMIT_BITNO must be
   greater than or equal to -1.  Return LIMIT_BITNO if we don't find a
   set bit.  */
INLINE
ptrdiff_t
bitset_scan_backward (const bitset_word *const words,
                            const size_t nr_words,
                            const ptrdiff_t start_bitno,
                            const ptrdiff_t limit_bitno)
{
  eassume (nr_words < (size_t) PTRDIFF_MAX);
  eassume (!INT_MULTIPLY_OVERFLOW ((ptrdiff_t) bitset_word_nbits,
                                   (ptrdiff_t) nr_words));
  const ptrdiff_t last_bit =
    bitset_word_nbits * (ptrdiff_t) nr_words;
  eassume (-1 <= limit_bitno
           && limit_bitno <= start_bitno
           && start_bitno < last_bit);
  if (start_bitno == limit_bitno)
    return limit_bitno;
  const ptrdiff_t first_wordno = start_bitno / bitset_word_nbits;
  const ptrdiff_t start_bitno_in_first_word =
    start_bitno % bitset_word_nbits;
  const ptrdiff_t last_wordno =
    (limit_bitno + 1) / bitset_word_nbits;
  const ptrdiff_t limit_bitno_in_last_word =
    (limit_bitno + 1) % bitset_word_nbits;
  for (ptrdiff_t wordno = first_wordno; wordno >= last_wordno; --wordno)
    {
      bitset_word word = words[wordno];
      /* First word: mask off the bits after the start.  */
      if (wordno == first_wordno &&
          start_bitno_in_first_word < (bitset_word_nbits - 1))
        word &= bitset_mask (start_bitno_in_first_word + 1);
      /* Last word: mask off bits below the search limit.  */
      if (wordno == last_wordno)
        word &= ~bitset_mask (limit_bitno_in_last_word);
      if (word)
        return (ptrdiff_t) wordno * bitset_word_nbits
          + bitset_word_nbits - emacs_clz_bw (word) - 1;
    }
  return limit_bitno;
}

#endif
