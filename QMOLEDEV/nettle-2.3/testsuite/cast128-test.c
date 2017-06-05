#include "testutils.h"
#include "cast128.h"

int
test_main(void)
{
  /* Test vectors from B.1. Single Plaintext-Key-Ciphertext Sets, RFC
   * 2144 */

  /* 128 bit key */
  test_cipher(&nettle_cast128,
	      HL("01 23 45 67 12 34 56 78"
		 "23 45 67 89 34 56 78 9A"),
	      HL("01 23 45 67 89 AB CD EF"),
	      H("23 8B 4F E5 84 7E 44 B2"));
  
  /* 80 bit key */
  test_cipher(&nettle_cast128,
	      HL("01 23 45 67 12 34 56 78 23 45"),
	      HL("01 23 45 67 89 AB CD EF"),
	      H("EB 6A 71 1A 2C 02 27 1B"));

  /* 40 bit key */
  test_cipher(&nettle_cast128,
	      HL("01 23 45 67 12"),
	      HL("01 23 45 67 89 AB CD EF"),
	      H("7A C8 16 D1 6E 9B 30 2E"));

  SUCCESS();
}
