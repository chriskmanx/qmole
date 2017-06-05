/*
 * Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free
 * Software Foundation, Inc.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA
 *
 */

/* Functions on OpenPGP privkey parsing
 */

#include <gnutls_int.h>
#include <gnutls_datum.h>
#include <gnutls_global.h>
#include <gnutls_errors.h>
#include <gnutls_num.h>
#include <openpgp_int.h>
#include <gnutls_openpgp.h>
#include <gnutls_cert.h>
#include <gnutls_sig.h>

/**
 * gnutls_openpgp_privkey_init:
 * @key: The structure to be initialized
 *
 * This function will initialize an OpenPGP key structure.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_openpgp_privkey_init (gnutls_openpgp_privkey_t * key)
{
  *key = gnutls_calloc (1, sizeof (gnutls_openpgp_privkey_int));

  if (*key)
    return 0;                   /* success */
  return GNUTLS_E_MEMORY_ERROR;
}

/**
 * gnutls_openpgp_privkey_deinit:
 * @key: The structure to be initialized
 *
 * This function will deinitialize a key structure.
 **/
void
gnutls_openpgp_privkey_deinit (gnutls_openpgp_privkey_t key)
{
  if (!key)
    return;

  if (key->knode)
    {
      cdk_kbnode_release (key->knode);
      key->knode = NULL;
    }

  gnutls_free (key);
}

/**
 * gnutls_openpgp_privkey_sec_param:
 * @key: a key structure
 *
 * This function will return the security parameter appropriate with
 * this private key.
 *
 * Returns: On success, a valid security parameter is returned otherwise
 * %GNUTLS_SEC_PARAM_UNKNOWN is returned.
 **/
gnutls_sec_param_t
gnutls_openpgp_privkey_sec_param (gnutls_openpgp_privkey_t key)
{
  gnutls_pk_algorithm_t algo;
  unsigned int bits;

  algo = gnutls_openpgp_privkey_get_pk_algorithm (key, &bits);
  if (algo == GNUTLS_PK_UNKNOWN)
    {
      gnutls_assert ();
      return GNUTLS_SEC_PARAM_UNKNOWN;
    }

  return gnutls_pk_bits_to_sec_param (algo, bits);
}

/**
 * gnutls_openpgp_privkey_import:
 * @key: The structure to store the parsed key.
 * @data: The RAW or BASE64 encoded key.
 * @format: One of #gnutls_openpgp_crt_fmt_t elements.
 * @password: not used for now
 * @flags: should be zero
 *
 * This function will convert the given RAW or Base64 encoded key to
 * the native gnutls_openpgp_privkey_t format.  The output will be
 * stored in 'key'.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_openpgp_privkey_import (gnutls_openpgp_privkey_t key,
                               const gnutls_datum_t * data,
                               gnutls_openpgp_crt_fmt_t format,
                               const char *password, unsigned int flags)
{
  cdk_stream_t inp;
  cdk_packet_t pkt;
  int rc;

  if (data->data == NULL || data->size == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_OPENPGP_GETKEY_FAILED;
    }

  if (format == GNUTLS_OPENPGP_FMT_RAW)
    {
      rc = cdk_kbnode_read_from_mem (&key->knode, data->data, data->size);
      if (rc != 0)
        {
          rc = _gnutls_map_cdk_rc (rc);
          gnutls_assert ();
          return rc;
        }
    }
  else
    {
      rc = cdk_stream_tmp_from_mem (data->data, data->size, &inp);
      if (rc != 0)
        {
          rc = _gnutls_map_cdk_rc (rc);
          gnutls_assert ();
          return rc;
        }

      if (cdk_armor_filter_use (inp))
        {
          rc = cdk_stream_set_armor_flag (inp, 0);
          if (rc != 0)
            {
              rc = _gnutls_map_cdk_rc (rc);
              cdk_stream_close (inp);
              gnutls_assert ();
              return rc;
            }
        }

      rc = cdk_keydb_get_keyblock (inp, &key->knode);
      cdk_stream_close (inp);

      if (rc != 0)
        {
          rc = _gnutls_map_cdk_rc (rc);
          gnutls_assert ();
          return rc;
        }
    }

  /* Test if the import was successful. */
  pkt = cdk_kbnode_find_packet (key->knode, CDK_PKT_SECRET_KEY);
  if (pkt == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_OPENPGP_GETKEY_FAILED;
    }

  return 0;
}

/**
 * gnutls_openpgp_privkey_export:
 * @key: Holds the key.
 * @format: One of gnutls_openpgp_crt_fmt_t elements.
 * @password: the password that will be used to encrypt the key. (unused for now)
 * @flags: zero for future compatibility
 * @output_data: will contain the key base64 encoded or raw
 * @output_data_size: holds the size of output_data (and will be
 *   replaced by the actual size of parameters)
 *
 * This function will convert the given key to RAW or Base64 format.
 * If the buffer provided is not long enough to hold the output, then
 * GNUTLS_E_SHORT_MEMORY_BUFFER will be returned.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_export (gnutls_openpgp_privkey_t key,
                               gnutls_openpgp_crt_fmt_t format,
                               const char *password, unsigned int flags,
                               void *output_data, size_t * output_data_size)
{
  /* FIXME for now we do not export encrypted keys */
  return _gnutls_openpgp_export (key->knode, format, output_data,
                                 output_data_size, 1);
}


/**
 * gnutls_openpgp_privkey_get_pk_algorithm:
 * @key: is an OpenPGP key
 * @bits: if bits is non null it will hold the size of the parameters' in bits
 *
 * This function will return the public key algorithm of an OpenPGP
 * certificate.
 *
 * If bits is non null, it should have enough size to hold the parameters
 * size in bits. For RSA the bits returned is the modulus.
 * For DSA the bits returned are of the public exponent.
 *
 * Returns: a member of the #gnutls_pk_algorithm_t enumeration on
 *   success, or a negative value on error.
 *
 * Since: 2.4.0
 **/
gnutls_pk_algorithm_t
gnutls_openpgp_privkey_get_pk_algorithm (gnutls_openpgp_privkey_t key,
                                         unsigned int *bits)
{
  cdk_packet_t pkt;
  int algo;

  if (!key)
    {
      gnutls_assert ();
      return GNUTLS_PK_UNKNOWN;
    }

  algo = 0;
  pkt = cdk_kbnode_find_packet (key->knode, CDK_PKT_SECRET_KEY);
  if (pkt)
    {
      if (bits)
        *bits = cdk_pk_get_nbits (pkt->pkt.secret_key->pk);
      algo = _gnutls_openpgp_get_algo (pkt->pkt.secret_key->pk->pubkey_algo);
    }

  return algo;
}

int
_gnutls_openpgp_get_algo (int cdk_algo)
{
  int algo;

  if (is_RSA (cdk_algo))
    algo = GNUTLS_PK_RSA;
  else if (is_DSA (cdk_algo))
    algo = GNUTLS_PK_DSA;
  else
    {
      _gnutls_x509_log ("Unknown OpenPGP algorithm %d\n", cdk_algo);
      algo = GNUTLS_PK_UNKNOWN;
    }

  return algo;
}

/**
 * gnutls_openpgp_privkey_get_revoked_status:
 * @key: the structure that contains the OpenPGP private key.
 *
 * Get revocation status of key.
 *
 * Returns: true (1) if the key has been revoked, or false (0) if it
 *   has not, or a negative value indicates an error.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_get_revoked_status (gnutls_openpgp_privkey_t key)
{
  cdk_packet_t pkt;

  if (!key)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  pkt = cdk_kbnode_find_packet (key->knode, CDK_PKT_SECRET_KEY);
  if (!pkt)
    return GNUTLS_E_OPENPGP_GETKEY_FAILED;

  if (pkt->pkt.secret_key->is_revoked != 0)
    return 1;
  return 0;
}

/**
 * gnutls_openpgp_privkey_get_fingerprint:
 * @key: the raw data that contains the OpenPGP secret key.
 * @fpr: the buffer to save the fingerprint, must hold at least 20 bytes.
 * @fprlen: the integer to save the length of the fingerprint.
 *
 * Get the fingerprint of the OpenPGP key. Depends on the
 * algorithm, the fingerprint can be 16 or 20 bytes.
 *
 * Returns: On success, 0 is returned, or an error code.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_get_fingerprint (gnutls_openpgp_privkey_t key,
                                        void *fpr, size_t * fprlen)
{
  cdk_packet_t pkt;
  cdk_pkt_pubkey_t pk = NULL;

  if (!fpr || !fprlen)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  *fprlen = 0;

  pkt = cdk_kbnode_find_packet (key->knode, CDK_PKT_SECRET_KEY);
  if (!pkt)
    {
      gnutls_assert ();
      return GNUTLS_E_OPENPGP_GETKEY_FAILED;
    }

  pk = pkt->pkt.secret_key->pk;
  *fprlen = 20;

  if (is_RSA (pk->pubkey_algo) && pk->version < 4)
    *fprlen = 16;

  cdk_pk_get_fingerprint (pk, fpr);

  return 0;
}

/**
 * gnutls_openpgp_privkey_get_key_id:
 * @key: the structure that contains the OpenPGP secret key.
 * @keyid: the buffer to save the keyid.
 *
 * Get key-id.
 *
 * Returns: the 64-bit keyID of the OpenPGP key.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_get_key_id (gnutls_openpgp_privkey_t key,
                                   gnutls_openpgp_keyid_t keyid)
{
  cdk_packet_t pkt;
  uint32_t kid[2];

  if (!key || !keyid)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  pkt = cdk_kbnode_find_packet (key->knode, CDK_PKT_SECRET_KEY);
  if (!pkt)
    return GNUTLS_E_OPENPGP_GETKEY_FAILED;

  cdk_sk_get_keyid (pkt->pkt.secret_key, kid);
  _gnutls_write_uint32 (kid[0], keyid);
  _gnutls_write_uint32 (kid[1], keyid + 4);

  return 0;
}


/**
 * gnutls_openpgp_privkey_get_subkey_count:
 * @key: is an OpenPGP key
 *
 * This function will return the number of subkeys present in the
 * given OpenPGP certificate.
 *
 * Returns: the number of subkeys, or a negative value on error.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_get_subkey_count (gnutls_openpgp_privkey_t key)
{
  cdk_kbnode_t p, ctx;
  cdk_packet_t pkt;
  int subkeys;

  if (key == NULL)
    {
      gnutls_assert ();
      return 0;
    }

  ctx = NULL;
  subkeys = 0;
  while ((p = cdk_kbnode_walk (key->knode, &ctx, 0)))
    {
      pkt = cdk_kbnode_get_packet (p);
      if (pkt->pkttype == CDK_PKT_SECRET_SUBKEY)
        subkeys++;
    }

  return subkeys;
}

/* returns the subkey with the given index */
static cdk_packet_t
_get_secret_subkey (gnutls_openpgp_privkey_t key, unsigned int indx)
{
  cdk_kbnode_t p, ctx;
  cdk_packet_t pkt;
  unsigned int subkeys;

  ctx = NULL;
  subkeys = 0;
  while ((p = cdk_kbnode_walk (key->knode, &ctx, 0)))
    {
      pkt = cdk_kbnode_get_packet (p);
      if (pkt->pkttype == CDK_PKT_SECRET_SUBKEY && indx == subkeys++)
        return pkt;
    }

  return NULL;
}

/**
 * gnutls_openpgp_privkey_get_subkey_revoked_status:
 * @key: the structure that contains the OpenPGP private key.
 * @idx: is the subkey index
 *
 * Get revocation status of key.
 *
 * Returns: true (1) if the key has been revoked, or false (0) if it
 *   has not, or a negative value indicates an error.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_get_subkey_revoked_status (gnutls_openpgp_privkey_t
                                                  key, unsigned int idx)
{
  cdk_packet_t pkt;

  if (!key)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  pkt = _get_secret_subkey (key, idx);
  if (!pkt)
    return GNUTLS_E_OPENPGP_GETKEY_FAILED;

  if (pkt->pkt.secret_key->is_revoked != 0)
    return 1;
  return 0;
}

/**
 * gnutls_openpgp_privkey_get_subkey_pk_algorithm:
 * @key: is an OpenPGP key
 * @idx: is the subkey index
 * @bits: if bits is non null it will hold the size of the parameters' in bits
 *
 * This function will return the public key algorithm of a subkey of an OpenPGP
 * certificate.
 *
 * If bits is non null, it should have enough size to hold the parameters
 * size in bits. For RSA the bits returned is the modulus.
 * For DSA the bits returned are of the public exponent.
 *
 * Returns: a member of the #gnutls_pk_algorithm_t enumeration on
 *   success, or a negative value on error.
 *
 * Since: 2.4.0
 **/
gnutls_pk_algorithm_t
gnutls_openpgp_privkey_get_subkey_pk_algorithm (gnutls_openpgp_privkey_t key,
                                                unsigned int idx,
                                                unsigned int *bits)
{
  cdk_packet_t pkt;
  int algo;

  if (!key)
    {
      gnutls_assert ();
      return GNUTLS_PK_UNKNOWN;
    }

  pkt = _get_secret_subkey (key, idx);

  algo = 0;
  if (pkt)
    {
      if (bits)
        *bits = cdk_pk_get_nbits (pkt->pkt.secret_key->pk);
      algo = pkt->pkt.secret_key->pubkey_algo;
      if (is_RSA (algo))
        algo = GNUTLS_PK_RSA;
      else if (is_DSA (algo))
        algo = GNUTLS_PK_DSA;
      else
        algo = GNUTLS_E_UNKNOWN_PK_ALGORITHM;
    }

  return algo;
}

/**
 * gnutls_openpgp_privkey_get_subkey_idx:
 * @key: the structure that contains the OpenPGP private key.
 * @keyid: the keyid.
 *
 * Get index of subkey.
 *
 * Returns: the index of the subkey or a negative error value.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_get_subkey_idx (gnutls_openpgp_privkey_t key,
                                       const gnutls_openpgp_keyid_t keyid)
{
  int ret;
  uint32_t kid[2];

  if (!key)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  KEYID_IMPORT (kid, keyid);
  ret = _gnutls_openpgp_find_subkey_idx (key->knode, kid, 1);

  if (ret < 0)
    {
      gnutls_assert ();
    }

  return ret;
}

/**
 * gnutls_openpgp_privkey_get_subkey_creation_time:
 * @key: the structure that contains the OpenPGP private key.
 * @idx: the subkey index
 *
 * Get subkey creation time.
 *
 * Returns: the timestamp when the OpenPGP key was created.
 *
 * Since: 2.4.0
 **/
time_t
gnutls_openpgp_privkey_get_subkey_creation_time (gnutls_openpgp_privkey_t key,
                                                 unsigned int idx)
{
  cdk_packet_t pkt;
  time_t timestamp;

  if (!key)
    return (time_t) - 1;

  pkt = _get_secret_subkey (key, idx);
  if (pkt)
    timestamp = pkt->pkt.secret_key->pk->timestamp;
  else
    timestamp = 0;

  return timestamp;
}

/**
 * gnutls_openpgp_privkey_get_subkey_expiration_time:
 * @key: the structure that contains the OpenPGP private key.
 * @idx: the subkey index
 *
 * Get subkey expiration time.  A value of '0' means that the key
 * doesn't expire at all.
 *
 * Returns: the time when the OpenPGP key expires.
 *
 * Since: 2.4.0
 **/
time_t
gnutls_openpgp_privkey_get_subkey_expiration_time (gnutls_openpgp_privkey_t
                                                   key, unsigned int idx)
{
  cdk_packet_t pkt;
  time_t expiredate;

  if (!key)
    return (time_t) - 1;

  pkt = _get_secret_subkey (key, idx);
  if (pkt)
    expiredate = pkt->pkt.secret_key->expiredate;
  else
    expiredate = 0;

  return expiredate;
}

/**
 * gnutls_openpgp_privkey_get_subkey_id:
 * @key: the structure that contains the OpenPGP secret key.
 * @idx: the subkey index
 * @keyid: the buffer to save the keyid.
 *
 * Get the key-id for the subkey.
 *
 * Returns: the 64-bit keyID of the OpenPGP key.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_get_subkey_id (gnutls_openpgp_privkey_t key,
                                      unsigned int idx,
                                      gnutls_openpgp_keyid_t keyid)
{
  cdk_packet_t pkt;
  uint32_t kid[2];

  if (!key || !keyid)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  pkt = _get_secret_subkey (key, idx);
  if (!pkt)
    return GNUTLS_E_OPENPGP_GETKEY_FAILED;

  cdk_sk_get_keyid (pkt->pkt.secret_key, kid);
  _gnutls_write_uint32 (kid[0], keyid);
  _gnutls_write_uint32 (kid[1], keyid + 4);

  return 0;
}

/**
 * gnutls_openpgp_privkey_get_subkey_fingerprint:
 * @key: the raw data that contains the OpenPGP secret key.
 * @idx: the subkey index
 * @fpr: the buffer to save the fingerprint, must hold at least 20 bytes.
 * @fprlen: the integer to save the length of the fingerprint.
 *
 * Get the fingerprint of an OpenPGP subkey.  Depends on the
 * algorithm, the fingerprint can be 16 or 20 bytes.
 *
 * Returns: On success, 0 is returned, or an error code.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_get_subkey_fingerprint (gnutls_openpgp_privkey_t key,
                                               unsigned int idx,
                                               void *fpr, size_t * fprlen)
{
  cdk_packet_t pkt;
  cdk_pkt_pubkey_t pk = NULL;

  if (!fpr || !fprlen)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  *fprlen = 0;

  pkt = _get_secret_subkey (key, idx);
  if (!pkt)
    return GNUTLS_E_OPENPGP_GETKEY_FAILED;


  pk = pkt->pkt.secret_key->pk;
  *fprlen = 20;

  if (is_RSA (pk->pubkey_algo) && pk->version < 4)
    *fprlen = 16;

  cdk_pk_get_fingerprint (pk, fpr);

  return 0;
}

/* Extracts DSA and RSA parameters from a certificate.
 */
int
_gnutls_openpgp_privkey_get_mpis (gnutls_openpgp_privkey_t pkey,
                                  uint32_t * keyid /*[2] */ ,
                                  bigint_t * params, int *params_size)
{
  int result, i;
  int pk_algorithm;
  gnutls_pk_params_st pk_params;
  cdk_packet_t pkt;

  memset (&pk_params, 0, sizeof (pk_params));

  if (keyid == NULL)
    pkt = cdk_kbnode_find_packet (pkey->knode, CDK_PKT_SECRET_KEY);
  else
    pkt = _gnutls_openpgp_find_key (pkey->knode, keyid, 1);

  if (pkt == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_OPENPGP_GETKEY_FAILED;
    }

  pk_algorithm =
    _gnutls_openpgp_get_algo (pkt->pkt.secret_key->pk->pubkey_algo);

  switch (pk_algorithm)
    {
    case GNUTLS_PK_RSA:
      /* openpgp does not hold all parameters as in PKCS #1
       */
      pk_params.params_nr = RSA_PRIVATE_PARAMS - 2;
      break;
    case GNUTLS_PK_DSA:
      pk_params.params_nr = DSA_PRIVATE_PARAMS;
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE;
    }

  for (i = 0; i < pk_params.params_nr; i++)
    {
      result = _gnutls_read_pgp_mpi (pkt, 1, i, &pk_params.params[i]);
      if (result < 0)
        {
          gnutls_assert ();
          goto error;
        }
    }
  
  /* fixup will generate exp1 and exp2 that are not
   * available here.
   */
  result = _gnutls_pk_fixup (pk_algorithm, GNUTLS_IMPORT, &pk_params);
  if (result < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if (*params_size < pk_params.params_nr)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  *params_size = pk_params.params_nr;
  for (i = 0; i < pk_params.params_nr; i++)
    params[i] = pk_params.params[i];

  return 0;

error:
  {
    int j;
    for (j = 0; j < i; j++)
      _gnutls_mpi_release (&pk_params.params[j]);
  }

  return result;
}

/* The internal version of export
 */
static int
_get_sk_rsa_raw (gnutls_openpgp_privkey_t pkey, gnutls_openpgp_keyid_t keyid,
                 gnutls_datum_t * m, gnutls_datum_t * e,
                 gnutls_datum_t * d, gnutls_datum_t * p,
                 gnutls_datum_t * q, gnutls_datum_t * u)
{
  int pk_algorithm, ret, i;
  cdk_packet_t pkt;
  uint32_t kid32[2];
  bigint_t params[MAX_PRIV_PARAMS_SIZE];
  int params_size = MAX_PRIV_PARAMS_SIZE;

  if (pkey == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  KEYID_IMPORT (kid32, keyid);

  pkt = _gnutls_openpgp_find_key (pkey->knode, kid32, 1);
  if (pkt == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_OPENPGP_GETKEY_FAILED;
    }

  pk_algorithm =
    _gnutls_openpgp_get_algo (pkt->pkt.secret_key->pk->pubkey_algo);

  if (pk_algorithm != GNUTLS_PK_RSA)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  ret = _gnutls_openpgp_privkey_get_mpis (pkey, kid32, params, &params_size);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_mpi_dprint (params[0], m);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = _gnutls_mpi_dprint (params[1], e);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (m);
      goto cleanup;
    }

  ret = _gnutls_mpi_dprint (params[2], d);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (m);
      _gnutls_free_datum (e);
      goto cleanup;
    }

  ret = _gnutls_mpi_dprint (params[3], p);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (m);
      _gnutls_free_datum (e);
      _gnutls_free_datum (d);
      goto cleanup;
    }

  ret = _gnutls_mpi_dprint (params[4], q);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (m);
      _gnutls_free_datum (e);
      _gnutls_free_datum (d);
      _gnutls_free_datum (p);
      goto cleanup;
    }

  ret = _gnutls_mpi_dprint (params[5], u);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (q);
      _gnutls_free_datum (m);
      _gnutls_free_datum (e);
      _gnutls_free_datum (d);
      _gnutls_free_datum (p);
      goto cleanup;
    }

  ret = 0;

cleanup:
  for (i = 0; i < params_size; i++)
    {
      _gnutls_mpi_release (&params[i]);
    }
  return ret;
}

static int
_get_sk_dsa_raw (gnutls_openpgp_privkey_t pkey, gnutls_openpgp_keyid_t keyid,
                 gnutls_datum_t * p, gnutls_datum_t * q,
                 gnutls_datum_t * g, gnutls_datum_t * y, gnutls_datum_t * x)
{
  int pk_algorithm, ret, i;
  cdk_packet_t pkt;
  uint32_t kid32[2];
  bigint_t params[MAX_PRIV_PARAMS_SIZE];
  int params_size = MAX_PRIV_PARAMS_SIZE;

  if (pkey == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  KEYID_IMPORT (kid32, keyid);

  pkt = _gnutls_openpgp_find_key (pkey->knode, kid32, 1);
  if (pkt == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_OPENPGP_GETKEY_FAILED;
    }

  pk_algorithm =
    _gnutls_openpgp_get_algo (pkt->pkt.secret_key->pk->pubkey_algo);

  if (pk_algorithm != GNUTLS_PK_DSA)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  ret = _gnutls_openpgp_privkey_get_mpis (pkey, kid32, params, &params_size);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* P */
  ret = _gnutls_mpi_dprint (params[0], p);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  /* Q */
  ret = _gnutls_mpi_dprint (params[1], q);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (p);
      goto cleanup;
    }


  /* G */
  ret = _gnutls_mpi_dprint (params[2], g);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (p);
      _gnutls_free_datum (q);
      goto cleanup;
    }


  /* Y */
  ret = _gnutls_mpi_dprint (params[3], y);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (p);
      _gnutls_free_datum (g);
      _gnutls_free_datum (q);
      goto cleanup;
    }

  ret = _gnutls_mpi_dprint (params[4], x);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (y);
      _gnutls_free_datum (p);
      _gnutls_free_datum (g);
      _gnutls_free_datum (q);
      goto cleanup;
    }

  ret = 0;

cleanup:
  for (i = 0; i < params_size; i++)
    {
      _gnutls_mpi_release (&params[i]);
    }
  return ret;
}


/**
 * gnutls_openpgp_privkey_export_rsa_raw:
 * @pkey: Holds the certificate
 * @m: will hold the modulus
 * @e: will hold the public exponent
 * @d: will hold the private exponent
 * @p: will hold the first prime (p)
 * @q: will hold the second prime (q)
 * @u: will hold the coefficient
 *
 * This function will export the RSA private key's parameters found in
 * the given structure.  The new parameters will be allocated using
 * gnutls_malloc() and will be stored in the appropriate datum.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, otherwise an error.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_export_rsa_raw (gnutls_openpgp_privkey_t pkey,
                                       gnutls_datum_t * m, gnutls_datum_t * e,
                                       gnutls_datum_t * d, gnutls_datum_t * p,
                                       gnutls_datum_t * q, gnutls_datum_t * u)
{
  uint8_t keyid[GNUTLS_OPENPGP_KEYID_SIZE];
  int ret;

  ret = gnutls_openpgp_privkey_get_key_id (pkey, keyid);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return _get_sk_rsa_raw (pkey, keyid, m, e, d, p, q, u);
}

/**
 * gnutls_openpgp_privkey_export_dsa_raw:
 * @pkey: Holds the certificate
 * @p: will hold the p
 * @q: will hold the q
 * @g: will hold the g
 * @y: will hold the y
 * @x: will hold the x
 *
 * This function will export the DSA private key's parameters found in
 * the given certificate.  The new parameters will be allocated using
 * gnutls_malloc() and will be stored in the appropriate datum.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, otherwise an error.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_export_dsa_raw (gnutls_openpgp_privkey_t pkey,
                                       gnutls_datum_t * p, gnutls_datum_t * q,
                                       gnutls_datum_t * g, gnutls_datum_t * y,
                                       gnutls_datum_t * x)
{
  uint8_t keyid[GNUTLS_OPENPGP_KEYID_SIZE];
  int ret;

  ret = gnutls_openpgp_privkey_get_key_id (pkey, keyid);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return _get_sk_dsa_raw (pkey, keyid, p, q, g, y, x);
}

/**
 * gnutls_openpgp_privkey_export_subkey_rsa_raw:
 * @pkey: Holds the certificate
 * @idx: Is the subkey index
 * @m: will hold the modulus
 * @e: will hold the public exponent
 * @d: will hold the private exponent
 * @p: will hold the first prime (p)
 * @q: will hold the second prime (q)
 * @u: will hold the coefficient
 *
 * This function will export the RSA private key's parameters found in
 * the given structure.  The new parameters will be allocated using
 * gnutls_malloc() and will be stored in the appropriate datum.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, otherwise an error.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_export_subkey_rsa_raw (gnutls_openpgp_privkey_t pkey,
                                              unsigned int idx,
                                              gnutls_datum_t * m,
                                              gnutls_datum_t * e,
                                              gnutls_datum_t * d,
                                              gnutls_datum_t * p,
                                              gnutls_datum_t * q,
                                              gnutls_datum_t * u)
{
  uint8_t keyid[GNUTLS_OPENPGP_KEYID_SIZE];
  int ret;

  ret = gnutls_openpgp_privkey_get_subkey_id (pkey, idx, keyid);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return _get_sk_rsa_raw (pkey, keyid, m, e, d, p, q, u);
}

/**
 * gnutls_openpgp_privkey_export_subkey_dsa_raw:
 * @pkey: Holds the certificate
 * @idx: Is the subkey index
 * @p: will hold the p
 * @q: will hold the q
 * @g: will hold the g
 * @y: will hold the y
 * @x: will hold the x
 *
 * This function will export the DSA private key's parameters found
 * in the given certificate.  The new parameters will be allocated
 * using gnutls_malloc() and will be stored in the appropriate datum.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, otherwise an error.
 *
 * Since: 2.4.0
 **/
int
gnutls_openpgp_privkey_export_subkey_dsa_raw (gnutls_openpgp_privkey_t pkey,
                                              unsigned int idx,
                                              gnutls_datum_t * p,
                                              gnutls_datum_t * q,
                                              gnutls_datum_t * g,
                                              gnutls_datum_t * y,
                                              gnutls_datum_t * x)
{
  uint8_t keyid[GNUTLS_OPENPGP_KEYID_SIZE];
  int ret;

  ret = gnutls_openpgp_privkey_get_subkey_id (pkey, idx, keyid);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return _get_sk_dsa_raw (pkey, keyid, p, q, g, y, x);
}

/**
 * gnutls_openpgp_privkey_get_preferred_key_id:
 * @key: the structure that contains the OpenPGP public key.
 * @keyid: the struct to save the keyid.
 *
 * Get the preferred key-id for the key.
 *
 * Returns: the 64-bit preferred keyID of the OpenPGP key, or if it
 *   hasn't been set it returns %GNUTLS_E_INVALID_REQUEST.
 **/
int
gnutls_openpgp_privkey_get_preferred_key_id (gnutls_openpgp_privkey_t key,
                                             gnutls_openpgp_keyid_t keyid)
{
  if (!key->preferred_set)
    return gnutls_assert_val(GNUTLS_E_OPENPGP_PREFERRED_KEY_ERROR);

  if (!key || !keyid)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  memcpy (keyid, key->preferred_keyid, GNUTLS_OPENPGP_KEYID_SIZE);

  return 0;
}

/**
 * gnutls_openpgp_privkey_set_preferred_key_id:
 * @key: the structure that contains the OpenPGP public key.
 * @keyid: the selected keyid
 *
 * This allows setting a preferred key id for the given certificate.
 * This key will be used by functions that involve key handling.
 *
 * Returns: On success, 0 is returned, or an error code.
 **/
int
gnutls_openpgp_privkey_set_preferred_key_id (gnutls_openpgp_privkey_t key,
                                             const gnutls_openpgp_keyid_t
                                             keyid)
{
  int ret;

  if (!key)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  /* check if the id is valid */
  ret = gnutls_openpgp_privkey_get_subkey_idx (key, keyid);
  if (ret < 0)
    {
      _gnutls_x509_log ("the requested subkey does not exist\n");
      gnutls_assert ();
      return ret;
    }

  key->preferred_set = 1;
  memcpy (key->preferred_keyid, keyid, GNUTLS_OPENPGP_KEYID_SIZE);

  return 0;
}

/**
 * gnutls_openpgp_privkey_sign_hash:
 * @key: Holds the key
 * @hash: holds the data to be signed
 * @signature: will contain newly allocated signature
 *
 * This function will sign the given hash using the private key.  You
 * should use gnutls_openpgp_privkey_set_preferred_key_id() before
 * calling this function to set the subkey to use.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 *
 * Deprecated: Use gnutls_privkey_sign_hash() instead.
 */
int
gnutls_openpgp_privkey_sign_hash (gnutls_openpgp_privkey_t key,
                                   const gnutls_datum_t * hash,
                                   gnutls_datum_t * signature)
{
  int result, i;
  bigint_t params[MAX_PRIV_PARAMS_SIZE];
  int params_size = MAX_PRIV_PARAMS_SIZE;
  int pk_algorithm;
  uint8_t keyid[GNUTLS_OPENPGP_KEYID_SIZE];

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  result = gnutls_openpgp_privkey_get_preferred_key_id (key, keyid);
  if (result == 0)
    {
      uint32_t kid[2];
      int idx;

      KEYID_IMPORT (kid, keyid);

      idx = gnutls_openpgp_privkey_get_subkey_idx (key, keyid);
      pk_algorithm =
        gnutls_openpgp_privkey_get_subkey_pk_algorithm (key, idx, NULL);
      result =
        _gnutls_openpgp_privkey_get_mpis (key, kid, params, &params_size);
    }
  else
    {
      pk_algorithm = gnutls_openpgp_privkey_get_pk_algorithm (key, NULL);
      result = _gnutls_openpgp_privkey_get_mpis (key, NULL,
                                                 params, &params_size);
    }

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }


  result =
    _gnutls_soft_sign (pk_algorithm, params, params_size, hash, signature);

  for (i = 0; i < params_size; i++)
    _gnutls_mpi_release (&params[i]);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}

/*-
 * _gnutls_openpgp_privkey_decrypt_data:
 * @key: Holds the key
 * @flags: zero for now
 * @ciphertext: holds the data to be decrypted
 * @plaintext: will contain newly allocated plaintext
 *
 * This function will sign the given hash using the private key.  You
 * should use gnutls_openpgp_privkey_set_preferred_key_id() before
 * calling this function to set the subkey to use.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 -*/
int
_gnutls_openpgp_privkey_decrypt_data (gnutls_openpgp_privkey_t key,
                                     unsigned int flags,
                                     const gnutls_datum_t * ciphertext,
                                     gnutls_datum_t * plaintext)
{
  int result, i;
  bigint_t params[MAX_PRIV_PARAMS_SIZE];
  int params_size = MAX_PRIV_PARAMS_SIZE;
  int pk_algorithm;
  uint8_t keyid[GNUTLS_OPENPGP_KEYID_SIZE];

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  result = gnutls_openpgp_privkey_get_preferred_key_id (key, keyid);
  if (result == 0)
    {
      uint32_t kid[2];

      KEYID_IMPORT (kid, keyid);
      result = _gnutls_openpgp_privkey_get_mpis (key, kid,
                                                 params, &params_size);

      i = gnutls_openpgp_privkey_get_subkey_idx (key, keyid);

      pk_algorithm = gnutls_openpgp_privkey_get_subkey_pk_algorithm (key, i, NULL);
    }
  else
    {
      pk_algorithm = gnutls_openpgp_privkey_get_pk_algorithm (key, NULL);

      result = _gnutls_openpgp_privkey_get_mpis (key, NULL,
                                                 params, &params_size);

    }

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  if (pk_algorithm != GNUTLS_PK_RSA)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  result =
    _gnutls_pkcs1_rsa_decrypt (plaintext, ciphertext, params, params_size, 2);

  for (i = 0; i < params_size; i++)
    _gnutls_mpi_release (&params[i]);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}
