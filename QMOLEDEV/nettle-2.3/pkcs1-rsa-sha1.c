/* pkcs1-rsa-sha1.c
 *
 * PKCS stuff for rsa-sha1.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001, 2003 Niels Möller
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

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "rsa.h"

#include "bignum.h"
#include "pkcs1.h"

#include "nettle-internal.h"

/* From pkcs-1v2
 *
 *   id-sha1 OBJECT IDENTIFIER ::=
 *     {iso(1) identified-organization(3) oiw(14) secsig(3)
 *   	 algorithms(2) 26}
 *   
 *   The default hash function is SHA-1: 
 *   sha1Identifier ::= AlgorithmIdentifier {id-sha1, NULL}
 */

static const uint8_t
sha1_prefix[] =
{
  /* 15 octets prefix, 20 octets hash, total 35 */
  0x30,       33, /* SEQUENCE */
    0x30,      9, /* SEQUENCE */
      0x06,    5, /* OBJECT IDENTIFIER */
  	  0x2b, 0x0e, 0x03, 0x02, 0x1a,
      0x05,    0, /* NULL */
    0x04,     20  /* OCTET STRING */
      /* Here comes the raw hash value */
};

int
pkcs1_rsa_sha1_encode(mpz_t m, unsigned size, struct sha1_ctx *hash)
{
  TMP_DECL(em, uint8_t, NETTLE_MAX_BIGNUM_BITS / 8);
  TMP_ALLOC(em, size);

  if (pkcs1_signature_prefix(size, em,
			     sizeof(sha1_prefix),
			     sha1_prefix,
			     SHA1_DIGEST_SIZE))
    {
      sha1_digest(hash, SHA1_DIGEST_SIZE, em + size - SHA1_DIGEST_SIZE);
      nettle_mpz_set_str_256_u(m, size, em);
      return 1;
    }
  else
    return 0;
}

int
pkcs1_rsa_sha1_encode_digest(mpz_t m, unsigned size, const uint8_t *digest)
{
  TMP_DECL(em, uint8_t, NETTLE_MAX_BIGNUM_BITS / 8);
  TMP_ALLOC(em, size);

  if (pkcs1_signature_prefix(size, em,
			     sizeof(sha1_prefix),
			     sha1_prefix,
			     SHA1_DIGEST_SIZE))
    {
      memcpy(em + size - SHA1_DIGEST_SIZE, digest, SHA1_DIGEST_SIZE);
      nettle_mpz_set_str_256_u(m, size, em);
      return 1;
    }
  else
    return 0;
}
