/* pkcs1-rsa-sha256.c
 *
 * PKCS stuff for rsa-sha256.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001, 2003, 2006 Niels Möller
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

/* From RFC 3447, Public-Key Cryptography Standards (PKCS) #1: RSA
 * Cryptography Specifications Version 2.1.
 *
 *     id-sha256    OBJECT IDENTIFIER ::=
 *       {joint-iso-itu-t(2) country(16) us(840) organization(1)
 *         gov(101) csor(3) nistalgorithm(4) hashalgs(2) 1}
 */

static const uint8_t
sha256_prefix[] =
{
  /* 19 octets prefix, 32 octets hash, total 51 */
  0x30,      49, /* SEQUENCE */
    0x30,    13, /* SEQUENCE */
      0x06,   9, /* OBJECT IDENTIFIER */
        0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x01,
      0x05,   0, /* NULL */
    0x04,    32  /* OCTET STRING */
      /* Here comes the raw hash value */
};

int
pkcs1_rsa_sha256_encode(mpz_t m, unsigned size, struct sha256_ctx *hash)
{
  TMP_DECL(em, uint8_t, NETTLE_MAX_BIGNUM_BITS / 8);
  TMP_ALLOC(em, size);

  if (pkcs1_signature_prefix(size, em,
			     sizeof(sha256_prefix),
			     sha256_prefix,
			     SHA256_DIGEST_SIZE))
    {
      sha256_digest(hash, SHA256_DIGEST_SIZE, em + size - SHA256_DIGEST_SIZE);
      nettle_mpz_set_str_256_u(m, size, em);
      return 1;
    }
  else
    return 0;	
}

int
pkcs1_rsa_sha256_encode_digest(mpz_t m, unsigned size, const uint8_t *digest)
{
  TMP_DECL(em, uint8_t, NETTLE_MAX_BIGNUM_BITS / 8);
  TMP_ALLOC(em, size);

  if (pkcs1_signature_prefix(size, em,
			     sizeof(sha256_prefix),
			     sha256_prefix,
			     SHA256_DIGEST_SIZE))
    {
      memcpy(em + size - SHA256_DIGEST_SIZE, digest, SHA256_DIGEST_SIZE);
      nettle_mpz_set_str_256_u(m, size, em);
      return 1;
    }
  else
    return 0;
}
