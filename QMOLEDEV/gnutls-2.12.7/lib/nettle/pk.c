/*
 * Copyright (C) 2010
 * Free Software Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GNUTLS.
 *
 * The GNUTLS library is free software; you can redistribute it and/or
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

/* This file contains the functions needed for RSA/DSA public key
 * encryption and signatures. 
 */

#include <gnutls_int.h>
#include <gnutls_mpi.h>
#include <gnutls_pk.h>
#include <gnutls_errors.h>
#include <gnutls_datum.h>
#include <gnutls_global.h>
#include <gnutls_num.h>
#include <x509/x509_int.h>
#include <x509/common.h>
#include <random.h>
#include <gnutls_pk.h>
#include <nettle/dsa.h>
#include <nettle/rsa.h>
#include <random.h>
#include <gnutls/crypto.h>

#define TOMPZ(x) (*((mpz_t*)(x)))

static void
rnd_func (void *_ctx, unsigned length, uint8_t * data)
{
  _gnutls_rnd (GNUTLS_RND_RANDOM, data, length);
}

static void
_dsa_params_to_pubkey (const gnutls_pk_params_st * pk_params,
                       struct dsa_public_key *pub)
{
  memcpy (&pub->p, pk_params->params[0], sizeof (mpz_t));
  memcpy (&pub->q, pk_params->params[1], sizeof (mpz_t));
  memcpy (&pub->g, pk_params->params[2], sizeof (mpz_t));
  memcpy (&pub->y, pk_params->params[3], sizeof (mpz_t));
}

static void
_dsa_params_to_privkey (const gnutls_pk_params_st * pk_params,
                        struct dsa_private_key *pub)
{
  memcpy (&pub->x, pk_params->params[4], sizeof (mpz_t));
}

static void
_rsa_params_to_privkey (const gnutls_pk_params_st * pk_params,
                        struct rsa_private_key *priv)
{
  memcpy (&priv->d, pk_params->params[2], sizeof (mpz_t));
  memcpy (&priv->p, pk_params->params[3], sizeof (mpz_t));
  memcpy (&priv->q, pk_params->params[4], sizeof (mpz_t));
  memcpy (&priv->c, pk_params->params[5], sizeof (mpz_t));
  memcpy (&priv->a, pk_params->params[6], sizeof (mpz_t));
  memcpy (&priv->b, pk_params->params[7], sizeof (mpz_t));

}

static int
_wrap_nettle_pk_encrypt (gnutls_pk_algorithm_t algo,
                         gnutls_datum_t * ciphertext,
                         const gnutls_datum_t * plaintext,
                         const gnutls_pk_params_st * pk_params)
{
  int ret;

  /* make a sexp from pkey */
  switch (algo)
    {
    case GNUTLS_PK_RSA:
      {
        bigint_t p;

        if (_gnutls_mpi_scan_nz (&p, plaintext->data, plaintext->size) != 0)
          {
            gnutls_assert ();
            return GNUTLS_E_MPI_SCAN_FAILED;
          }

        mpz_powm (p, p, TOMPZ (pk_params->params[1]) /*e */ ,
                  TOMPZ (pk_params->params[0] /*m */ ));

        ret = _gnutls_mpi_dprint_size (p, ciphertext, plaintext->size);
        _gnutls_mpi_release (&p);

        if (ret < 0)
          {
            gnutls_assert ();
            goto cleanup;
          }

        break;
      }
    default:
      gnutls_assert ();
      ret = GNUTLS_E_INTERNAL_ERROR;
      goto cleanup;
    }

  ret = 0;

cleanup:

  return ret;
}

/* returns the blinded c and the inverse of a random
 * number r;
 */
static bigint_t
rsa_blind (bigint_t c, bigint_t e, bigint_t n, bigint_t * _ri)
{
  bigint_t nc = NULL, r = NULL, ri = NULL;

  /* nc = c*(r^e)
   * ri = r^(-1)
   */
  nc = _gnutls_mpi_alloc_like (n);
  if (nc == NULL)
    {
      gnutls_assert ();
      return NULL;
    }

  ri = _gnutls_mpi_alloc_like (n);
  if (nc == NULL)
    {
      gnutls_assert ();
      goto fail;
    }

  r = _gnutls_mpi_randomize (NULL, _gnutls_mpi_get_nbits (n),
                             GNUTLS_RND_NONCE);
  if (r == NULL)
    {
      gnutls_assert ();
      goto fail;
    }

  /* invert r */
  if (mpz_invert (ri, r, n) == 0)
    {
      gnutls_assert ();
      goto fail;
    }

  /* r = r^e */

  _gnutls_mpi_powm (r, r, e, n);

  _gnutls_mpi_mulm (nc, c, r, n);

  *_ri = ri;

  _gnutls_mpi_release (&r);

  return nc;
fail:
  _gnutls_mpi_release (&nc);
  _gnutls_mpi_release (&r);
  return NULL;
}

/* c = c*ri mod n
 */
static inline void
rsa_unblind (bigint_t c, bigint_t ri, bigint_t n)
{
  _gnutls_mpi_mulm (c, c, ri, n);
}

static int
_wrap_nettle_pk_decrypt (gnutls_pk_algorithm_t algo,
                         gnutls_datum_t * plaintext,
                         const gnutls_datum_t * ciphertext,
                         const gnutls_pk_params_st * pk_params)
{
  int ret;

  /* make a sexp from pkey */
  switch (algo)
    {
    case GNUTLS_PK_RSA:
      {
        struct rsa_private_key priv;
        bigint_t c, ri, nc;

        if (_gnutls_mpi_scan_nz (&c, ciphertext->data, ciphertext->size) != 0)
          {
            gnutls_assert ();
            return GNUTLS_E_MPI_SCAN_FAILED;
          }

        nc = rsa_blind (c, pk_params->params[1] /*e */ ,
                        pk_params->params[0] /*m */ , &ri);
        _gnutls_mpi_release (&c);
        if (nc == NULL)
          {
            gnutls_assert ();
            return GNUTLS_E_MEMORY_ERROR;
          }

        memset(&priv, 0, sizeof(priv));
        _rsa_params_to_privkey (pk_params, &priv);

        rsa_compute_root (&priv, TOMPZ (nc), TOMPZ (nc));

        rsa_unblind (nc, ri, pk_params->params[0] /*m */ );

        ret = _gnutls_mpi_dprint_size (nc, plaintext, ciphertext->size);

        _gnutls_mpi_release (&nc);
        _gnutls_mpi_release (&ri);

        if (ret < 0)
          {
            gnutls_assert ();
            goto cleanup;
          }

        break;
      }
    default:
      gnutls_assert ();
      ret = GNUTLS_E_INTERNAL_ERROR;
      goto cleanup;
    }

  ret = 0;

cleanup:

  return ret;
}

/* in case of DSA puts into data, r,s
 */
static int
_wrap_nettle_pk_sign (gnutls_pk_algorithm_t algo,
                      gnutls_datum_t * signature,
                      const gnutls_datum_t * vdata,
                      const gnutls_pk_params_st * pk_params)
{
  int ret, hash;

  switch (algo)
    {

    case GNUTLS_PK_DSA:
      {
        struct dsa_public_key pub;
        struct dsa_private_key priv;
        struct dsa_signature sig;
        unsigned int hash_len;

        memset(&priv, 0, sizeof(priv));
        memset(&pub, 0, sizeof(pub));
        _dsa_params_to_pubkey (pk_params, &pub);
        _dsa_params_to_privkey (pk_params, &priv);

        dsa_signature_init (&sig);

        hash = _gnutls_dsa_q_to_hash (pub.q, &hash_len);
        if (hash_len > vdata->size)
          {
            gnutls_assert ();
            _gnutls_debug_log("Security level of algorithm requires hash %s(%d) or better\n", gnutls_mac_get_name(hash), hash_len);
            hash_len = vdata->size;
          }

        ret =
          _dsa_sign (&pub, &priv, NULL, rnd_func,
                     hash_len, vdata->data, &sig);
        if (ret == 0)
          {
            gnutls_assert ();
            ret = GNUTLS_E_PK_SIGN_FAILED;
            goto dsa_fail;
          }

        ret = _gnutls_encode_ber_rs (signature, &sig.r, &sig.s);

      dsa_fail:
        dsa_signature_clear (&sig);

        if (ret < 0)
          {
            gnutls_assert ();
            goto cleanup;
          }
        break;
      }
    case GNUTLS_PK_RSA:
      {
        struct rsa_private_key priv;
        bigint_t hash, nc, ri;

        if (_gnutls_mpi_scan_nz (&hash, vdata->data, vdata->size) != 0)
          {
            gnutls_assert ();
            return GNUTLS_E_MPI_SCAN_FAILED;
          }

        memset(&priv, 0, sizeof(priv));
        _rsa_params_to_privkey (pk_params, &priv);

        nc = rsa_blind (hash, pk_params->params[1] /*e */ ,
                        pk_params->params[0] /*m */ , &ri);

        _gnutls_mpi_release (&hash);

        if (nc == NULL)
          {
            gnutls_assert ();
            ret = GNUTLS_E_MEMORY_ERROR;
            goto rsa_fail;
          }

        rsa_compute_root (&priv, TOMPZ (nc), TOMPZ (nc));

        rsa_unblind (nc, ri, pk_params->params[0] /*m */ );

        ret = _gnutls_mpi_dprint (nc, signature);

rsa_fail:
        _gnutls_mpi_release (&nc);
        _gnutls_mpi_release (&ri);

        if (ret < 0)
          {
            gnutls_assert ();
            goto cleanup;
          }

        break;
      }
    default:
      gnutls_assert ();
      ret = GNUTLS_E_INTERNAL_ERROR;
      goto cleanup;
    }

  ret = 0;

cleanup:

  return ret;
}

static int
_int_rsa_verify (const gnutls_pk_params_st * pk_params,
                 bigint_t m, bigint_t s)
{
  int res;

  mpz_t m1;

  if ((mpz_sgn (TOMPZ (s)) <= 0)
      || (mpz_cmp (TOMPZ (s), TOMPZ (pk_params->params[0])) >= 0))
    return GNUTLS_E_PK_SIG_VERIFY_FAILED;

  mpz_init (m1);

  mpz_powm (m1, TOMPZ (s), TOMPZ (pk_params->params[1]),
            TOMPZ (pk_params->params[0]));

  res = !mpz_cmp (TOMPZ (m), m1);

  mpz_clear (m1);

  if (res == 0)
    res = GNUTLS_E_PK_SIG_VERIFY_FAILED;
  else
    res = 0;

  return res;
}

static int
_wrap_nettle_pk_verify (gnutls_pk_algorithm_t algo,
                        const gnutls_datum_t * vdata,
                        const gnutls_datum_t * signature,
                        const gnutls_pk_params_st * pk_params)
{
  int ret, hash;
  bigint_t tmp[2] = { NULL, NULL };

  switch (algo)
    {
    case GNUTLS_PK_DSA:
      {
        struct dsa_public_key pub;
        struct dsa_signature sig;
        unsigned int hash_len;

        ret = _gnutls_decode_ber_rs (signature, &tmp[0], &tmp[1]);
        if (ret < 0)
          {
            gnutls_assert ();
            goto cleanup;
          }
        memset(&pub, 0, sizeof(pub));
        _dsa_params_to_pubkey (pk_params, &pub);
        memcpy (&sig.r, tmp[0], sizeof (sig.r));
        memcpy (&sig.s, tmp[1], sizeof (sig.s));

        hash = _gnutls_dsa_q_to_hash (pub.q, &hash_len);

        if (hash_len > vdata->size)
          {
            gnutls_assert ();
            _gnutls_debug_log("Security level of algorithm requires hash %s(%d) or better\n", gnutls_mac_get_name(hash), hash_len);
            hash_len = vdata->size;
          }

        ret = _dsa_verify (&pub, hash_len, vdata->data, &sig);
        if (ret == 0)
          {
            gnutls_assert();
            ret = GNUTLS_E_PK_SIG_VERIFY_FAILED;
          }
        else
          ret = 0;

        _gnutls_mpi_release (&tmp[0]);
        _gnutls_mpi_release (&tmp[1]);
        break;
      }
    case GNUTLS_PK_RSA:
      {
        bigint_t hash;

        if (_gnutls_mpi_scan_nz (&hash, vdata->data, vdata->size) != 0)
          {
            gnutls_assert ();
            return GNUTLS_E_MPI_SCAN_FAILED;
          }

        ret = _gnutls_mpi_scan_nz (&tmp[0], signature->data, signature->size);
        if (ret < 0)
          {
            gnutls_assert ();
            goto cleanup;
          }

        ret = _int_rsa_verify (pk_params, hash, tmp[0]);
        _gnutls_mpi_release (&tmp[0]);
        _gnutls_mpi_release (&hash);
        break;
      }
    default:
      gnutls_assert ();
      ret = GNUTLS_E_INTERNAL_ERROR;
      goto cleanup;
    }

cleanup:

  return ret;
}

static int
wrap_nettle_pk_generate_params (gnutls_pk_algorithm_t algo,
                                unsigned int level /*bits */ ,
                                gnutls_pk_params_st * params)
{
  int ret, i;
  int q_bits;

  memset(params, 0, sizeof(*params));

  switch (algo)
    {

    case GNUTLS_PK_DSA:
      {
        struct dsa_public_key pub;
        struct dsa_private_key priv;

        dsa_public_key_init (&pub);
        dsa_private_key_init (&priv);

        /* the best would be to use _gnutls_pk_bits_to_subgroup_bits()
         * but we do NIST DSA here */
        if (level <= 1024)
          q_bits = 160;
        else
          q_bits = 256;

        ret =
          dsa_generate_keypair (&pub, &priv, NULL,
                                rnd_func, NULL, NULL, level, q_bits);
        if (ret != 1)
          {
            gnutls_assert ();
            ret = GNUTLS_E_INTERNAL_ERROR;
            goto dsa_fail;
          }

        params->params_nr = 0;
        for (i = 0; i < DSA_PRIVATE_PARAMS; i++)
          {
            params->params[i] = _gnutls_mpi_alloc_like (&pub.p);
            if (params->params[i] == NULL)
              {
                ret = GNUTLS_E_MEMORY_ERROR;
                goto dsa_fail;
              }
            params->params_nr++;
          }

        ret = 0;
        _gnutls_mpi_set (params->params[0], pub.p);
        _gnutls_mpi_set (params->params[1], pub.q);
        _gnutls_mpi_set (params->params[2], pub.g);
        _gnutls_mpi_set (params->params[3], pub.y);
        _gnutls_mpi_set (params->params[4], priv.x);

dsa_fail:
        dsa_private_key_clear (&priv);
        dsa_public_key_clear (&pub);

        if (ret < 0)
          goto fail;

        break;
      }
    case GNUTLS_PK_RSA:
      {
        struct rsa_public_key pub;
        struct rsa_private_key priv;

        rsa_public_key_init (&pub);
        rsa_private_key_init (&priv);

        _gnutls_mpi_set_ui (&pub.e, 65537);

        ret =
          rsa_generate_keypair (&pub, &priv, NULL,
                                rnd_func, NULL, NULL, level, 0);
        if (ret != 1)
          {
            gnutls_assert ();
            ret = GNUTLS_E_INTERNAL_ERROR;
            goto rsa_fail;
          }

        params->params_nr = 0;
        for (i = 0; i < RSA_PRIVATE_PARAMS; i++)
          {
            params->params[i] = _gnutls_mpi_alloc_like (&pub.n);
            if (params->params[i] == NULL)
              {
                ret = GNUTLS_E_MEMORY_ERROR;
                goto rsa_fail;
              }
            params->params_nr++;

          }
          
        ret = 0;

        _gnutls_mpi_set (params->params[0], pub.n);
        _gnutls_mpi_set (params->params[1], pub.e);
        _gnutls_mpi_set (params->params[2], priv.d);
        _gnutls_mpi_set (params->params[3], priv.p);
        _gnutls_mpi_set (params->params[4], priv.q);
        _gnutls_mpi_set (params->params[5], priv.c);
        _gnutls_mpi_set (params->params[6], priv.a);
        _gnutls_mpi_set (params->params[7], priv.b);

rsa_fail:
        rsa_private_key_clear (&priv);
        rsa_public_key_clear (&pub);

        if (ret < 0)
          goto fail;

        break;
      }
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return 0;

fail:

  for (i = 0; i < params->params_nr; i++)
    {
      _gnutls_mpi_release (&params->params[i]);
    }
  params->params_nr = 0;

  return ret;
}


static int
wrap_nettle_pk_fixup (gnutls_pk_algorithm_t algo,
                      gnutls_direction_t direction,
                      gnutls_pk_params_st * params)
{
  int result;

  if (direction == GNUTLS_IMPORT && algo == GNUTLS_PK_RSA)
    {
      /* do not trust the generated values. Some old private keys
       * generated by us have mess on the values. Those were very
       * old but it seemed some of the shipped example private
       * keys were as old.
       */
      mpz_invert (TOMPZ (params->params[5]),
                  TOMPZ (params->params[4]), TOMPZ (params->params[3]));

      /* calculate exp1 [6] and exp2 [7] */
      _gnutls_mpi_release (&params->params[6]);
      _gnutls_mpi_release (&params->params[7]);

      result = _gnutls_calc_rsa_exp (params->params, RSA_PRIVATE_PARAMS - 2);
      if (result < 0)
        {
          gnutls_assert ();
          return result;
        }
      params->params_nr = RSA_PRIVATE_PARAMS;
    }

  return 0;
}

int crypto_pk_prio = INT_MAX;

gnutls_crypto_pk_st _gnutls_pk_ops = {
  .encrypt = _wrap_nettle_pk_encrypt,
  .decrypt = _wrap_nettle_pk_decrypt,
  .sign = _wrap_nettle_pk_sign,
  .verify = _wrap_nettle_pk_verify,
  .generate = wrap_nettle_pk_generate_params,
  .pk_fixup_private_params = wrap_nettle_pk_fixup,
};
