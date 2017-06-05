/* seskey.c - Session key routines
 * Copyright (C) 1998, 1999, 2000, 2002, 2003, 2007, 2008, 2010 Free
 * Software Foundation, Inc.
 *
 * Author: Timo Schulz
 *
 * This file is part of OpenCDK.
 *
 * The OpenCDK library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>

#include "opencdk.h"
#include "main.h"
#include "packet.h"


/* We encode the MD in this way:
 *
 * 0  1 PAD(n bytes)   0  ASN(asnlen bytes)  MD(len bytes)
 *
 * PAD consists of FF bytes.
 */
static cdk_error_t
do_encode_md (byte ** r_frame, size_t * r_flen, const byte * md, int algo,
              size_t len, unsigned nbits, const byte * asn, size_t asnlen)
{
  byte *frame = NULL;
  size_t nframe = (nbits + 7) / 8;
  ssize_t i;
  size_t n = 0;

  if (!asn || !md || !r_frame || !r_flen)
    return CDK_Inv_Value;

  if (len + asnlen + 4 > nframe)
    return CDK_General_Error;

  frame = cdk_calloc (1, nframe);
  if (!frame)
    return CDK_Out_Of_Core;
  frame[n++] = 0;
  frame[n++] = 1;
  i = nframe - len - asnlen - 3;
  if (i < 0)
    {
      cdk_free (frame);
      return CDK_Inv_Value;
    }
  memset (frame + n, 0xFF, i);
  n += i;
  frame[n++] = 0;
  memcpy (frame + n, asn, asnlen);
  n += asnlen;
  memcpy (frame + n, md, len);
  n += len;
  if (n != nframe)
    {
      cdk_free (frame);
      return CDK_Inv_Value;
    }
  *r_frame = frame;
  *r_flen = n;
  return 0;
}

static const byte md5_asn[18] = /* Object ID is 1.2.840.113549.2.5 */
{ 0x30, 0x20, 0x30, 0x0c, 0x06, 0x08, 0x2a, 0x86, 0x48,
  0x86, 0xf7, 0x0d, 0x02, 0x05, 0x05, 0x00, 0x04, 0x10
};

static const byte sha1_asn[15] =        /* Object ID is 1.3.14.3.2.26 */
{ 0x30, 0x21, 0x30, 0x09, 0x06, 0x05, 0x2b, 0x0e, 0x03,
  0x02, 0x1a, 0x05, 0x00, 0x04, 0x14
};

static const byte sha224_asn[19] =      /* Object ID is 2.16.840.1.101.3.4.2.4 */
{ 0x30, 0x2D, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48,
  0x01, 0x65, 0x03, 0x04, 0x02, 0x04, 0x05, 0x00, 0x04,
  0x1C
};

static const byte sha256_asn[19] =      /* Object ID is  2.16.840.1.101.3.4.2.1 */
{ 0x30, 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86,
  0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x01, 0x05,
  0x00, 0x04, 0x20
};

static const byte sha512_asn[] =        /* Object ID is 2.16.840.1.101.3.4.2.3 */
{
  0x30, 0x51, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86,
  0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x03, 0x05,
  0x00, 0x04, 0x40
};

static const byte sha384_asn[] =        /* Object ID is 2.16.840.1.101.3.4.2.2 */
{
  0x30, 0x41, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86,
  0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x02, 0x05,
  0x00, 0x04, 0x30
};

static const byte rmd160_asn[15] =      /* Object ID is 1.3.36.3.2.1 */
{ 0x30, 0x21, 0x30, 0x09, 0x06, 0x05, 0x2b, 0x24, 0x03,
  0x02, 0x01, 0x05, 0x00, 0x04, 0x14
};

static int
_gnutls_get_digest_oid (gnutls_digest_algorithm_t algo, const byte ** data)
{
  switch (algo)
    {
    case GNUTLS_DIG_MD5:
      *data = md5_asn;
      return sizeof (md5_asn);
    case GNUTLS_DIG_SHA1:
      *data = sha1_asn;
      return sizeof (sha1_asn);
    case GNUTLS_DIG_RMD160:
      *data = rmd160_asn;
      return sizeof (rmd160_asn);
    case GNUTLS_DIG_SHA256:
      *data = sha256_asn;
      return sizeof (sha256_asn);
    case GNUTLS_DIG_SHA384:
      *data = sha384_asn;
      return sizeof (sha384_asn);
    case GNUTLS_DIG_SHA512:
      *data = sha512_asn;
      return sizeof (sha512_asn);
    case GNUTLS_DIG_SHA224:
      *data = sha224_asn;
      return sizeof (sha224_asn);
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }
}


/* Encode the given digest into a pkcs#1 compatible format. */
cdk_error_t
_cdk_digest_encode_pkcs1 (byte ** r_md, size_t * r_mdlen, int pk_algo,
                          const byte * md, int digest_algo, unsigned nbits)
{
  size_t dlen;

  if (!md || !r_md || !r_mdlen)
    return CDK_Inv_Value;

  dlen = _gnutls_hash_get_algo_len (digest_algo);
  if (dlen <= 0)
    return CDK_Inv_Algo;
  if (is_DSA (pk_algo))
    {                           /* DSS does not use a special encoding. */
      *r_md = cdk_malloc (dlen + 1);
      if (!*r_md)
        return CDK_Out_Of_Core;
      *r_mdlen = dlen;
      memcpy (*r_md, md, dlen);
      return 0;
    }
  else
    {
      const byte *asn;
      int asnlen;
      cdk_error_t rc;

      asnlen = _gnutls_get_digest_oid (digest_algo, &asn);
      if (asnlen < 0)
        return asnlen;

      rc = do_encode_md (r_md, r_mdlen, md, digest_algo, dlen,
                         nbits, asn, asnlen);
      return rc;
    }
  return 0;
}


/**
 * cdk_s2k_new:
 * @ret_s2k: output for the new S2K object
 * @mode: the S2K mode (simple, salted, iter+salted)
 * @digest_algo: the hash algorithm
 * @salt: random salt
 * 
 * Create a new S2K object with the given parameter.
 * The @salt parameter must be always 8 octets.
 **/
cdk_error_t
cdk_s2k_new (cdk_s2k_t * ret_s2k, int mode, int digest_algo,
             const byte * salt)
{
  cdk_s2k_t s2k;

  if (!ret_s2k)
    return CDK_Inv_Value;

  if (mode != 0x00 && mode != 0x01 && mode != 0x03)
    return CDK_Inv_Mode;

  if (_gnutls_hash_get_algo_len (digest_algo) <= 0)
    return CDK_Inv_Algo;

  s2k = cdk_calloc (1, sizeof *s2k);
  if (!s2k)
    return CDK_Out_Of_Core;
  s2k->mode = mode;
  s2k->hash_algo = digest_algo;
  if (salt)
    memcpy (s2k->salt, salt, 8);
  *ret_s2k = s2k;
  return 0;
}


/**
 * cdk_s2k_free:
 * @s2k: the S2K object
 * 
 * Release the given S2K object.
 **/
void
cdk_s2k_free (cdk_s2k_t s2k)
{
  cdk_free (s2k);
}


/* Make a copy of the source s2k into R_DST. */
cdk_error_t
_cdk_s2k_copy (cdk_s2k_t * r_dst, cdk_s2k_t src)
{
  cdk_s2k_t dst;
  cdk_error_t err;

  err = cdk_s2k_new (&dst, src->mode, src->hash_algo, src->salt);
  if (err)
    return err;
  dst->count = src->count;
  *r_dst = dst;

  return 0;
}
