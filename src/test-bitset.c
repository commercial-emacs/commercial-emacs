#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "bitset.h"
#include "lisp.h"

bool suppress_checking;

void
die (const char *msg, const char *file, int line)
{
  fprintf (stderr, "\r\n%s:%d: Emacs fatal error: assertion failed: %s\r\n",
	   file, line, msg);
  abort ();
}

#define SET_BIT(n) emacs_bitset_set_bit (words, nr_words, (n))
#define BIT_SET_P(n) emacs_bitset_bit_set_p (words, nr_words, (n))
#define SCAN_FORWARD(start, limit)                              \
  emacs_bitset_scan_forward (words, nr_words, (start), (limit))
#define SCAN_BACKWARD(start, limit)                             \
  emacs_bitset_scan_backward (words, nr_words, (start), (limit))
#define CLEAR_BITS() memset (&words, 0, sizeof (words))

static void
test_bitset_basic (void)
{
  printf ("Testing bitset basics...");
  enum { nr_words = 2 };
  emacs_bitset_word words[nr_words] = {};
  eassert (!BIT_SET_P (2));
  SET_BIT (2);
  eassert (BIT_SET_P (2));
  eassert (words[0] == 0b100);
  eassert (words[1] == 0);
  SET_BIT (emacs_bitset_bits_per_word);
  eassert (BIT_SET_P (emacs_bitset_bits_per_word));
  eassert (words[1] == 1);
  words[1] = 0;
  SET_BIT (emacs_bitset_bits_per_word * 2 - 1);
  eassert (BIT_SET_P (emacs_bitset_bits_per_word * 2 - 1));
  eassert (words[1] == ((emacs_bitset_word) 1)
           << (emacs_bitset_bits_per_word - 1));
  printf (" pass\n");
}

static void
test_bitset_scan_forward (void)
{
  printf ("Testing bitset scan forward...");
  enum { nr_words = 3 };
  enum { nr_bits = nr_words * emacs_bitset_bits_per_word };
  const ptrdiff_t last_bit = nr_bits - 1;
  emacs_bitset_word words[nr_words];

  CLEAR_BITS ();
  eassert (SCAN_FORWARD (0, nr_bits) == nr_bits);
  eassert (SCAN_FORWARD (0, 0) == 0);
  eassert (SCAN_FORWARD (nr_bits, nr_bits) == nr_bits);
  SET_BIT (0);
  eassert (SCAN_FORWARD (0, nr_bits) == 0);
  eassert (SCAN_FORWARD (1, nr_bits) == nr_bits);

  CLEAR_BITS ();
  SET_BIT (5);
  eassert (SCAN_FORWARD (0, nr_bits) == 5);
  eassert (SCAN_FORWARD (0, 3) == 3);
  eassert (SCAN_FORWARD (0, 4) == 4);
  eassert (SCAN_FORWARD (0, 5) == 5);
  eassert (SCAN_FORWARD (0, 6) == 5);
  eassert (SCAN_FORWARD (4, nr_bits) == 5);
  eassert (SCAN_FORWARD (5, nr_bits) == 5);
  eassert (SCAN_FORWARD (6, nr_bits) == nr_bits);

  CLEAR_BITS ();
  SET_BIT (5);
  SET_BIT (last_bit);
  eassert (SCAN_FORWARD (0, nr_bits) == 5);
  eassert (SCAN_FORWARD (0, 3) == 3);
  eassert (SCAN_FORWARD (5, nr_bits) == 5);
  eassert (SCAN_FORWARD (6, nr_bits) == last_bit);
  eassert (SCAN_FORWARD (6, last_bit) == last_bit);
  eassert (SCAN_FORWARD (6, last_bit - 1) == last_bit - 1);

  CLEAR_BITS ();
  SET_BIT (emacs_bitset_bits_per_word - 1);
  eassert (SCAN_FORWARD (6, nr_bits) == emacs_bitset_bits_per_word - 1);

  CLEAR_BITS ();
  SET_BIT (emacs_bitset_bits_per_word);
  eassert (SCAN_FORWARD (6, nr_bits) == emacs_bitset_bits_per_word);

  CLEAR_BITS ();
  SET_BIT (emacs_bitset_bits_per_word + 1);
  eassert (SCAN_FORWARD (6, nr_bits) == emacs_bitset_bits_per_word + 1);

  printf (" pass\n");
}

static void
test_bitset_scan_backward (void)
{
  printf ("Testing bitset scan backward...");
  enum { nr_words = 3 };
  enum { nr_bits = nr_words * emacs_bitset_bits_per_word };
  const ptrdiff_t last_bit = nr_bits - 1;
  emacs_bitset_word words[nr_words];

  CLEAR_BITS ();
  eassert (SCAN_BACKWARD (last_bit, -1) == -1);
  eassert (SCAN_BACKWARD (0, -1) == -1);
  eassert (SCAN_BACKWARD (-1, -1) == -1);
  SET_BIT (0);
  eassert (SCAN_BACKWARD (last_bit, -1) == 0);
  eassert (SCAN_BACKWARD (last_bit, 0) == 0);
  eassert (SCAN_BACKWARD (last_bit, 1) == 1);
  eassert (SCAN_BACKWARD (last_bit, 2) == 2);

  CLEAR_BITS ();
  SET_BIT (1);
  SET_BIT (4);
  eassert (SCAN_BACKWARD (last_bit, -1) == 4);
  eassert (SCAN_BACKWARD (last_bit, 0) == 4);
  eassert (SCAN_BACKWARD (last_bit, 1) == 4);
  eassert (SCAN_BACKWARD (last_bit, 2) == 4);
  eassert (SCAN_BACKWARD (last_bit, 5) == 5);
  eassert (SCAN_BACKWARD (5, -1) == 4);
  eassert (SCAN_BACKWARD (4, -1) == 4);
  eassert (SCAN_BACKWARD (3, -1) == 1);
  eassert (SCAN_BACKWARD (1, 1) == 1);
  eassert (SCAN_BACKWARD (1, 0) == 1);

  CLEAR_BITS ();
  const ptrdiff_t xbit = emacs_bitset_bits_per_word;
  SET_BIT (xbit);
  eassert (SCAN_BACKWARD (last_bit, -1) == xbit);
  eassert (SCAN_BACKWARD (last_bit, xbit) == xbit);
  eassert (SCAN_BACKWARD (last_bit, xbit - 1) == xbit);
  eassert (SCAN_BACKWARD (xbit, xbit - 1) == xbit);
  SET_BIT (xbit + 1);
  eassert (SCAN_BACKWARD (xbit, xbit - 1) == xbit);

  CLEAR_BITS ();
  SET_BIT (xbit - 1);
  SET_BIT (xbit - 2);
  SET_BIT (xbit - 3);
  eassert (SCAN_BACKWARD (xbit - 2, xbit - 3) == xbit - 2);

  printf (" pass\n");
}

int
main (void)
{
  setvbuf (stdout, NULL, _IONBF, 0);
  setvbuf (stderr, NULL, _IONBF, 0);
  test_bitset_basic ();
  test_bitset_scan_forward ();
  test_bitset_scan_backward ();
  return 0;
}
