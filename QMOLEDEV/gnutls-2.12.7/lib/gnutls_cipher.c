/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2008, 2009, 2010
 * Free Software Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * The GnuTLS is free software; you can redistribute it and/or
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

/* Some high level functions to be used in the record encryption are
 * included here.
 */

#include "gnutls_int.h"
#include "gnutls_errors.h"
#include "gnutls_compress.h"
#include "gnutls_cipher.h"
#include "gnutls_algorithms.h"
#include "gnutls_hash_int.h"
#include "gnutls_cipher_int.h"
#include "debug.h"
#include "gnutls_num.h"
#include "gnutls_datum.h"
#include "gnutls_kx.h"
#include "gnutls_record.h"
#include "gnutls_constate.h"
#include <random.h>

inline static int
is_write_comp_null (gnutls_session_t session)
{
  record_parameters_st *record_params;

  _gnutls_epoch_get (session, EPOCH_WRITE_CURRENT, &record_params);
  if (record_params->compression_algorithm == GNUTLS_COMP_NULL)
    return 0;

  return 1;
}

inline static int
is_read_comp_null (gnutls_session_t session)
{
  record_parameters_st *record_params;

  _gnutls_epoch_get (session, EPOCH_READ_CURRENT, &record_params);
  if (record_params->compression_algorithm == GNUTLS_COMP_NULL)
    return 0;

  return 1;
}


/* returns ciphertext which contains the headers too. This also
 * calculates the size in the header field.
 * 
 * If random pad != 0 then the random pad data will be appended.
 */
int
_gnutls_encrypt (gnutls_session_t session, const opaque * headers,
                 size_t headers_size, const opaque * data,
                 size_t data_size, opaque * ciphertext,
                 size_t ciphertext_size, content_type_t type, int random_pad,
                 record_parameters_st * params)
{
  gnutls_datum_t plain;
  gnutls_datum_t comp;
  int ret;
  int free_comp = 1;

  plain.data = (opaque *) data;
  plain.size = data_size;

  if (plain.size == 0 || is_write_comp_null (session) == 0)
    {
      comp = plain;
      free_comp = 0;
    }
  else
    {
      /* Here comp is allocated and must be 
       * freed.
       */
      ret = _gnutls_m_plaintext2compressed (session, &comp, &plain, params);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
    }

  ret = _gnutls_compressed2ciphertext (session, &ciphertext[headers_size],
                                       ciphertext_size - headers_size,
                                       comp, type, random_pad, params);

  if (free_comp)
    _gnutls_free_datum (&comp);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }


  /* copy the headers */
  memcpy (ciphertext, headers, headers_size);
  _gnutls_write_uint16 (ret, &ciphertext[3]);

  return ret + headers_size;
}

/* Decrypts the given data.
 * Returns the decrypted data length.
 */
int
_gnutls_decrypt (gnutls_session_t session, opaque * ciphertext,
                 size_t ciphertext_size, uint8_t * data,
                 size_t max_data_size, content_type_t type,
                 record_parameters_st * params)
{
  gnutls_datum_t gtxt;
  gnutls_datum_t gcipher;
  int ret;

  if (ciphertext_size == 0)
    return 0;

  gcipher.size = ciphertext_size;
  gcipher.data = ciphertext;

  ret =
    _gnutls_ciphertext2compressed (session, data, max_data_size,
                                   gcipher, type, params);
  if (ret < 0)
    {
      return ret;
    }

  if (ret == 0 || is_read_comp_null (session) == 0)
    {
      /* ret == ret */

    }
  else
    {
      gnutls_datum_t gcomp;

      /* compression has this malloc overhead.
       */

      gcomp.data = data;
      gcomp.size = ret;
      ret = _gnutls_m_compressed2plaintext (session, &gtxt, &gcomp, params);
      if (ret < 0)
        {
          return ret;
        }

      if (gtxt.size > MAX_RECORD_RECV_SIZE)
        {
          gnutls_assert ();
          _gnutls_free_datum (&gtxt);
          /* This shouldn't have happen and
           * is a TLS fatal error.
           */
          return GNUTLS_E_DECOMPRESSION_FAILED;
        }

      /* This check is not really needed */
      if (max_data_size < MAX_RECORD_RECV_SIZE)
        {
          gnutls_assert ();
          _gnutls_free_datum (&gtxt);
          return GNUTLS_E_INTERNAL_ERROR;
        }

      memcpy (data, gtxt.data, gtxt.size);
      ret = gtxt.size;

      _gnutls_free_datum (&gtxt);
    }

  return ret;
}

static inline int
mac_init (digest_hd_st * td, gnutls_mac_algorithm_t mac, opaque * secret,
          int secret_size, int ver)
{
  int ret = 0;

  if (mac == GNUTLS_MAC_NULL)
    {
      return GNUTLS_E_HASH_FAILED;
    }

  if (ver == GNUTLS_SSL3)
    {                           /* SSL 3.0 */
      ret = _gnutls_mac_init_ssl3 (td, mac, secret, secret_size);
    }
  else
    {                           /* TLS 1.x */
      ret = _gnutls_hmac_init (td, mac, secret, secret_size);
    }

  return ret;
}

static inline void
mac_hash (digest_hd_st * td, void *data, int data_size, int ver)
{
  if (ver == GNUTLS_SSL3)
    {                           /* SSL 3.0 */
      _gnutls_hash (td, data, data_size);
    }
  else
    {
      _gnutls_hmac (td, data, data_size);
    }
}

static inline void
mac_deinit (digest_hd_st * td, opaque * res, int ver)
{
  if (ver == GNUTLS_SSL3)
    {                           /* SSL 3.0 */
      _gnutls_mac_deinit_ssl3 (td, res);
    }
  else
    {
      _gnutls_hmac_deinit (td, res);
    }
}

inline static int
calc_enc_length (gnutls_session_t session, int data_size,
                 int hash_size, uint8_t * pad, int random_pad,
                 cipher_type_t block_algo, uint16_t blocksize)
{
  uint8_t rnd;
  int length, ret;

  *pad = 0;

  switch (block_algo)
    {
    case CIPHER_STREAM:
      length = data_size + hash_size;

      break;
    case CIPHER_BLOCK:
      ret = _gnutls_rnd (GNUTLS_RND_NONCE, &rnd, 1);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      /* make rnd a multiple of blocksize */
      if (session->security_parameters.version == GNUTLS_SSL3 ||
          random_pad == 0)
        {
          rnd = 0;
        }
      else
        {
          rnd = (rnd / blocksize) * blocksize;
          /* added to avoid the case of pad calculated 0
           * seen below for pad calculation.
           */
          if (rnd > blocksize)
            rnd -= blocksize;
        }

      length = data_size + hash_size;

      *pad = (uint8_t) (blocksize - (length % blocksize)) + rnd;

      length += *pad;
      if (_gnutls_version_has_explicit_iv
          (session->security_parameters.version))
        length += blocksize;    /* for the IV */

      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  return length;
}

#define PREAMBLE_SIZE 16
static inline int
make_preamble (opaque * uint64_data, opaque type, uint16_t c_length,
               opaque ver, opaque * preamble)
{
  opaque minor = _gnutls_version_get_minor (ver);
  opaque major = _gnutls_version_get_major (ver);
  opaque *p = preamble;

  memcpy (p, uint64_data, 8);
  p += 8;
  *p = type;
  p++;
  if (_gnutls_version_has_variable_padding (ver))
    {                           /* TLS 1.0 or higher */
      *p = major;
      p++;
      *p = minor;
      p++;
    }
  memcpy (p, &c_length, 2);
  p += 2;
  return p - preamble;
}

/* This is the actual encryption 
 * Encrypts the given compressed datum, and puts the result to cipher_data,
 * which has cipher_size size.
 * return the actual encrypted data length.
 */
int
_gnutls_compressed2ciphertext (gnutls_session_t session,
                               opaque * cipher_data, int cipher_size,
                               gnutls_datum_t compressed,
                               content_type_t _type, int random_pad,
                               record_parameters_st * params)
{
  uint8_t MAC[MAX_HASH_SIZE];
  uint16_t c_length;
  uint8_t pad;
  int length, ret;
  uint8_t type = _type;
  opaque preamble[PREAMBLE_SIZE];
  int preamble_size;
  int hash_size = _gnutls_hash_get_algo_len (params->mac_algorithm);
  int blocksize = gnutls_cipher_get_block_size (params->cipher_algorithm);
  cipher_type_t block_algo =
    _gnutls_cipher_is_block (params->cipher_algorithm);
  opaque *data_ptr;
  int ver = gnutls_protocol_get_version (session);


  /* Initialize MAC */

  c_length = _gnutls_conv_uint16 (compressed.size);

  if (params->mac_algorithm != GNUTLS_MAC_NULL)
    {                           /* actually when the algorithm in not the NULL one */
      digest_hd_st td;

      ret = mac_init (&td, params->mac_algorithm,
                      params->write.mac_secret.data,
                      params->write.mac_secret.size, ver);

      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
      preamble_size =
        make_preamble (UINT64DATA
                       (params->write.sequence_number),
                       type, c_length, ver, preamble);
      mac_hash (&td, preamble, preamble_size, ver);
      mac_hash (&td, compressed.data, compressed.size, ver);
      mac_deinit (&td, MAC, ver);
    }


  /* Calculate the encrypted length (padding etc.)
   */
  length =
    calc_enc_length (session, compressed.size, hash_size, &pad,
                     random_pad, block_algo, blocksize);
  if (length < 0)
    {
      gnutls_assert ();
      return length;
    }

  /* copy the encrypted data to cipher_data.
   */
  if (cipher_size < length)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  data_ptr = cipher_data;
  if (block_algo == CIPHER_BLOCK &&
      _gnutls_version_has_explicit_iv (session->security_parameters.version))
    {
      /* copy the random IV.
       */
      ret = _gnutls_rnd (GNUTLS_RND_NONCE, data_ptr, blocksize);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      data_ptr += blocksize;
    }

  memcpy (data_ptr, compressed.data, compressed.size);
  data_ptr += compressed.size;

  if (hash_size > 0)
    {
      memcpy (data_ptr, MAC, hash_size);
      data_ptr += hash_size;
    }
  if (block_algo == CIPHER_BLOCK && pad > 0)
    {
      memset (data_ptr, pad - 1, pad);
    }


  /* Actual encryption (inplace).
   */
  ret =
    _gnutls_cipher_encrypt (&params->write.cipher_state, cipher_data, length);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return length;
}


/* Deciphers the ciphertext packet, and puts the result to compress_data, of compress_size.
 * Returns the actual compressed packet size.
 */
int
_gnutls_ciphertext2compressed (gnutls_session_t session,
                               opaque * compress_data,
                               int compress_size,
                               gnutls_datum_t ciphertext, uint8_t type,
                               record_parameters_st * params)
{
  uint8_t MAC[MAX_HASH_SIZE];
  uint16_t c_length;
  uint8_t pad;
  int length;
  uint16_t blocksize;
  int ret, i, pad_failed = 0;
  opaque preamble[PREAMBLE_SIZE];
  int preamble_size;
  int ver = gnutls_protocol_get_version (session);
  int hash_size = _gnutls_hash_get_algo_len (params->mac_algorithm);

  blocksize = gnutls_cipher_get_block_size (params->cipher_algorithm);


  /* actual decryption (inplace)
   */
  switch (_gnutls_cipher_is_block (params->cipher_algorithm))
    {
    case CIPHER_STREAM:
      if ((ret =
           _gnutls_cipher_decrypt (&params->read.cipher_state,
                                   ciphertext.data, ciphertext.size)) < 0)
        {
          gnutls_assert ();
          return ret;
        }

      length = ciphertext.size - hash_size;

      break;
    case CIPHER_BLOCK:
      if ((ciphertext.size < blocksize) || (ciphertext.size % blocksize != 0))
        {
          gnutls_assert ();
          return GNUTLS_E_DECRYPTION_FAILED;
        }

      if ((ret =
           _gnutls_cipher_decrypt (&params->read.cipher_state,
                                   ciphertext.data, ciphertext.size)) < 0)
        {
          gnutls_assert ();
          return ret;
        }

      /* ignore the IV in TLS 1.1.
       */
      if (_gnutls_version_has_explicit_iv
          (session->security_parameters.version))
        {
          ciphertext.size -= blocksize;
          ciphertext.data += blocksize;

          if (ciphertext.size == 0)
            {
              gnutls_assert ();
              return GNUTLS_E_DECRYPTION_FAILED;
            }
        }

      pad = ciphertext.data[ciphertext.size - 1] + 1;   /* pad */

      if ((int) pad > (int) ciphertext.size - hash_size)
        {
          gnutls_assert ();
          _gnutls_record_log
            ("REC[%p]: Short record length %d > %d - %d (under attack?)\n",
             session, pad, ciphertext.size, hash_size);
          /* We do not fail here. We check below for the
           * the pad_failed. If zero means success.
           */
          pad_failed = GNUTLS_E_DECRYPTION_FAILED;
        }

      length = ciphertext.size - hash_size - pad;

      /* Check the pading bytes (TLS 1.x)
       */
      if (_gnutls_version_has_variable_padding (ver) && pad_failed == 0)
        for (i = 2; i < pad; i++)
          {
            if (ciphertext.data[ciphertext.size - i] !=
                ciphertext.data[ciphertext.size - 1])
              pad_failed = GNUTLS_E_DECRYPTION_FAILED;
          }
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  if (length < 0)
    length = 0;
  c_length = _gnutls_conv_uint16 ((uint16_t) length);

  /* Pass the type, version, length and compressed through
   * MAC.
   */
  if (params->mac_algorithm != GNUTLS_MAC_NULL)
    {
      digest_hd_st td;

      ret = mac_init (&td, params->mac_algorithm,
                      params->read.mac_secret.data,
                      params->read.mac_secret.size, ver);

      if (ret < 0)
        {
          gnutls_assert ();
          return GNUTLS_E_INTERNAL_ERROR;
        }

      preamble_size =
        make_preamble (UINT64DATA
                       (params->read.sequence_number), type,
                       c_length, ver, preamble);
      mac_hash (&td, preamble, preamble_size, ver);
      if (length > 0)
        mac_hash (&td, ciphertext.data, length, ver);

      mac_deinit (&td, MAC, ver);
    }

  /* This one was introduced to avoid a timing attack against the TLS
   * 1.0 protocol.
   */
  if (pad_failed != 0)
    {
      gnutls_assert ();
      return pad_failed;
    }

  /* HMAC was not the same. 
   */
  if (memcmp (MAC, &ciphertext.data[length], hash_size) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_DECRYPTION_FAILED;
    }

  /* copy the decrypted stuff to compress_data.
   */
  if (compress_size < length)
    {
      gnutls_assert ();
      return GNUTLS_E_DECOMPRESSION_FAILED;
    }
  memcpy (compress_data, ciphertext.data, length);

  return length;
}
