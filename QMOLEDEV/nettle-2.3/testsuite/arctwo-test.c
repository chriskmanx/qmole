/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2004 Simon Josefsson
 * Copyright (C) 2004 Niels Möller
 *
 * The nettle library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * The nettle library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the nettle library; see the file COPYING.LIB.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 */

#include "testutils.h"
#include "arctwo.h"

/* For tests with obscure values of ebk. */
static void
test_arctwo(unsigned ekb,
	    unsigned key_length,
	    const uint8_t *key,
	    unsigned length,
	    const uint8_t *cleartext,
	    const uint8_t *ciphertext)
{
  struct arctwo_ctx ctx;
  uint8_t *data = xalloc(length);

  arctwo_set_key_ekb(&ctx, key_length, key, ekb);
  arctwo_encrypt(&ctx, length, data, cleartext);

  if (!MEMEQ(length, data, ciphertext))
    FAIL();

  arctwo_decrypt(&ctx, length, data, data);

  if (!MEMEQ(length, data, cleartext))
    FAIL();

  free(data);
}

int
test_main(void)
{
  /* Test vectors from Peter Gutmann's paper. */
  test_cipher(&nettle_arctwo_gutmann128,
	      HL("00000000 00000000 00000000 00000000"),
	      HL("00000000 00000000"),
	      H ("1c198a83 8df028b7"));

  test_cipher(&nettle_arctwo_gutmann128,
	      HL("00010203 04050607 08090a0b 0c0d0e0f"),
	      HL("00000000 00000000"),
	      H ("50dc0162 bd757f31"));

  /* This one was checked against libmcrypt's RFC2268. */
  test_cipher(&nettle_arctwo_gutmann128,
	      HL("30000000 00000000 00000000 00000000"),
	      HL("10000000 00000000"),
	      H ("8fd10389 336bf95e"));

  /* Test vectors from RFC 2268. */
  test_cipher(&nettle_arctwo64,
	      HL("ffffffff ffffffff"),
	      HL("ffffffff ffffffff"),
	      H ("278b27e4 2e2f0d49"));

  test_cipher(&nettle_arctwo64,
	      HL("30000000 00000000"),
	      HL("10000000 00000001"),
	      H ("30649edf 9be7d2c2"));

  test_cipher(&nettle_arctwo128,
	      HL("88bca90e 90875a7f 0f79c384 627bafb2"),
	      HL("00000000 00000000"),
	      H ("2269552a b0f85ca6"));

  /* More obscure tests from RFC 2286 */
  test_arctwo(63,
	      HL("00000000 00000000"),
	      HL("00000000 00000000"),
	      H ("ebb773f9 93278eff"));

  test_arctwo(64,
	      HL("88"),
	      HL("00000000 00000000"),
	      H ("61a8a244 adacccf0"));

  test_arctwo(64,
	      HL("88bca90e 90875a"),
	      HL("00000000 00000000"),
	      H ("6ccf4308 974c267f"));

  test_arctwo(64,
	      HL("88bca90e 90875a7f 0f79c384 627bafb2"),
	      HL("00000000 00000000"),
	      H ("1a807d27 2bbe5db1"));

  test_arctwo(129,
	      HL("88bca90e 90875a7f 0f79c384 627bafb2"
		 "16f80a6f 85920584 c42fceb0 be255daf 1e"),
	      HL("00000000 00000000"),
	      H ("5b78d3a4 3dfff1f1"));

  SUCCESS ();
}
