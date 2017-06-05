/*
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2008, 2009, 2010 Free
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 */

#include <gnutls_int.h>
#include <gnutls_errors.h>
#include <auth_srp.h>
#include <gnutls_state.h>

#ifdef ENABLE_SRP

#include <gnutls_srp.h>
#include <auth_srp_passwd.h>
#include <gnutls_mpi.h>
#include <gnutls_num.h>
#include <gnutls_helper.h>

#include "debug.h"


/* Here functions for SRP (like g^x mod n) are defined 
 */

int
_gnutls_srp_gx (opaque * text, size_t textsize, opaque ** result,
                bigint_t g, bigint_t prime, gnutls_alloc_function galloc_func)
{
  bigint_t x, e;
  size_t result_size;
  int ret;

  if (_gnutls_mpi_scan_nz (&x, text, textsize))
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  e = _gnutls_mpi_alloc_like (prime);
  if (e == NULL)
    {
      gnutls_assert ();
      _gnutls_mpi_release (&x);
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* e = g^x mod prime (n) */
  _gnutls_mpi_powm (e, g, x, prime);
  _gnutls_mpi_release (&x);

  ret = _gnutls_mpi_print (e, NULL, &result_size);
  if (ret == GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      *result = galloc_func (result_size);
      if ((*result) == NULL)
        return GNUTLS_E_MEMORY_ERROR;

      _gnutls_mpi_print (e, *result, &result_size);
      ret = result_size;
    }
  else
    {
      gnutls_assert ();
      ret = GNUTLS_E_MPI_PRINT_FAILED;
    }

  _gnutls_mpi_release (&e);

  return ret;

}


/****************
 * Choose a random value b and calculate B = (k* v + g^b) % N.
 * where k == SHA1(N|g)
 * Return: B and if ret_b is not NULL b.
 */
bigint_t
_gnutls_calc_srp_B (bigint_t * ret_b, bigint_t g, bigint_t n, bigint_t v)
{
  bigint_t tmpB = NULL, tmpV = NULL;
  bigint_t b = NULL, B = NULL, k = NULL;
  int bits;


  /* calculate:  B = (k*v + g^b) % N 
   */
  bits = _gnutls_mpi_get_nbits (n);

  tmpV = _gnutls_mpi_alloc_like (n);

  if (tmpV == NULL)
    {
      gnutls_assert ();
      goto error;
    }

  b = _gnutls_mpi_randomize (NULL, bits, GNUTLS_RND_RANDOM);

  tmpB = _gnutls_mpi_new (bits);
  if (tmpB == NULL)
    {
      gnutls_assert ();
      goto error;
    }

  B = _gnutls_mpi_new (bits);
  if (B == NULL)
    {
      gnutls_assert ();
      goto error;
    }

  k = _gnutls_calc_srp_u (n, g, n);
  if (k == NULL)
    {
      gnutls_assert ();
      goto error;
    }

  _gnutls_mpi_mulm (tmpV, k, v, n);
  _gnutls_mpi_powm (tmpB, g, b, n);

  _gnutls_mpi_addm (B, tmpV, tmpB, n);

  _gnutls_mpi_release (&k);
  _gnutls_mpi_release (&tmpB);
  _gnutls_mpi_release (&tmpV);

  if (ret_b)
    *ret_b = b;
  else
    _gnutls_mpi_release (&b);

  return B;

error:
  _gnutls_mpi_release (&b);
  _gnutls_mpi_release (&B);
  _gnutls_mpi_release (&k);
  _gnutls_mpi_release (&tmpB);
  _gnutls_mpi_release (&tmpV);
  return NULL;

}

/* This calculates the SHA1(A | B)
 * A and B will be left-padded with zeros to fill n_size.
 */
bigint_t
_gnutls_calc_srp_u (bigint_t A, bigint_t B, bigint_t n)
{
  size_t b_size, a_size;
  opaque *holder, hd[MAX_HASH_SIZE];
  size_t holder_size, hash_size, n_size;
  digest_hd_st td;
  int ret;
  bigint_t res;

  /* get the size of n in bytes */
  _gnutls_mpi_print (n, NULL, &n_size);

  _gnutls_mpi_print (A, NULL, &a_size);
  _gnutls_mpi_print (B, NULL, &b_size);

  if (a_size > n_size || b_size > n_size)
    {
      gnutls_assert ();
      return NULL;              /* internal error */
    }

  holder_size = n_size + n_size;

  holder = gnutls_calloc (1, holder_size);
  if (holder == NULL)
    return NULL;

  _gnutls_mpi_print (A, &holder[n_size - a_size], &a_size);
  _gnutls_mpi_print (B, &holder[n_size + n_size - b_size], &b_size);

  ret = _gnutls_hash_init (&td, GNUTLS_MAC_SHA1);
  if (ret < 0)
    {
      gnutls_free (holder);
      gnutls_assert ();
      return NULL;
    }
  _gnutls_hash (&td, holder, holder_size);
  _gnutls_hash_deinit (&td, hd);

  /* convert the bytes of hd to integer
   */
  hash_size = 20;               /* SHA */
  ret = _gnutls_mpi_scan_nz (&res, hd, hash_size);
  gnutls_free (holder);

  if (ret < 0)
    {
      gnutls_assert ();
      return NULL;
    }

  return res;
}

/* S = (A * v^u) ^ b % N 
 * this is our shared key (server premaster secret)
 */
bigint_t
_gnutls_calc_srp_S1 (bigint_t A, bigint_t b, bigint_t u, bigint_t v,
                     bigint_t n)
{
  bigint_t tmp1 = NULL, tmp2 = NULL;
  bigint_t S = NULL;

  S = _gnutls_mpi_alloc_like (n);
  if (S == NULL)
    return NULL;

  tmp1 = _gnutls_mpi_alloc_like (n);
  tmp2 = _gnutls_mpi_alloc_like (n);

  if (tmp1 == NULL || tmp2 == NULL)
    goto freeall;

  _gnutls_mpi_powm (tmp1, v, u, n);
  _gnutls_mpi_mulm (tmp2, A, tmp1, n);
  _gnutls_mpi_powm (S, tmp2, b, n);

  _gnutls_mpi_release (&tmp1);
  _gnutls_mpi_release (&tmp2);

  return S;

freeall:
  _gnutls_mpi_release (&tmp1);
  _gnutls_mpi_release (&tmp2);
  return NULL;
}

/* A = g^a % N 
 * returns A and a (which is random)
 */
bigint_t
_gnutls_calc_srp_A (bigint_t * a, bigint_t g, bigint_t n)
{
  bigint_t tmpa;
  bigint_t A;
  int bits;

  bits = _gnutls_mpi_get_nbits (n);
  tmpa = _gnutls_mpi_randomize (NULL, bits, GNUTLS_RND_RANDOM);

  A = _gnutls_mpi_new (bits);
  if (A == NULL)
    {
      gnutls_assert ();
      _gnutls_mpi_release (&tmpa);
      return NULL;
    }
  _gnutls_mpi_powm (A, g, tmpa, n);

  if (a != NULL)
    *a = tmpa;
  else
    _gnutls_mpi_release (&tmpa);

  return A;
}

/* generate x = SHA(s | SHA(U | ":" | p))
 * The output is exactly 20 bytes
 */
static int
_gnutls_calc_srp_sha (const char *username, const char *password,
                      opaque * salt, int salt_size, size_t * size,
                      void *digest)
{
  digest_hd_st td;
  opaque res[MAX_HASH_SIZE];
  int ret;

  *size = 20;

  ret = _gnutls_hash_init (&td, GNUTLS_MAC_SHA1);
  if (ret < 0)
    {
      return GNUTLS_E_MEMORY_ERROR;
    }
  _gnutls_hash (&td, username, strlen (username));
  _gnutls_hash (&td, ":", 1);
  _gnutls_hash (&td, password, strlen (password));

  _gnutls_hash_deinit (&td, res);

  ret = _gnutls_hash_init (&td, GNUTLS_MAC_SHA1);
  if (ret < 0)
    {
      return GNUTLS_E_MEMORY_ERROR;
    }

  _gnutls_hash (&td, salt, salt_size);
  _gnutls_hash (&td, res, 20);  /* 20 bytes is the output of sha1 */

  _gnutls_hash_deinit (&td, digest);

  return 0;
}

int
_gnutls_calc_srp_x (char *username, char *password, opaque * salt,
                    size_t salt_size, size_t * size, void *digest)
{

  return _gnutls_calc_srp_sha (username, password, salt,
                               salt_size, size, digest);
}


/* S = (B - k*g^x) ^ (a + u * x) % N
 * this is our shared key (client premaster secret)
 */
bigint_t
_gnutls_calc_srp_S2 (bigint_t B, bigint_t g, bigint_t x, bigint_t a,
                     bigint_t u, bigint_t n)
{
  bigint_t S = NULL, tmp1 = NULL, tmp2 = NULL;
  bigint_t tmp4 = NULL, tmp3 = NULL, k = NULL;

  S = _gnutls_mpi_alloc_like (n);
  if (S == NULL)
    return NULL;

  tmp1 = _gnutls_mpi_alloc_like (n);
  tmp2 = _gnutls_mpi_alloc_like (n);
  tmp3 = _gnutls_mpi_alloc_like (n);
  if (tmp1 == NULL || tmp2 == NULL || tmp3 == NULL)
    {
      goto freeall;
    }

  k = _gnutls_calc_srp_u (n, g, n);
  if (k == NULL)
    {
      gnutls_assert ();
      goto freeall;
    }

  _gnutls_mpi_powm (tmp1, g, x, n);     /* g^x */
  _gnutls_mpi_mulm (tmp3, tmp1, k, n);  /* k*g^x mod n */
  _gnutls_mpi_subm (tmp2, B, tmp3, n);

  tmp4 = _gnutls_mpi_alloc_like (n);
  if (tmp4 == NULL)
    goto freeall;

  _gnutls_mpi_mul (tmp1, u, x);
  _gnutls_mpi_add (tmp4, a, tmp1);
  _gnutls_mpi_powm (S, tmp2, tmp4, n);

  _gnutls_mpi_release (&tmp1);
  _gnutls_mpi_release (&tmp2);
  _gnutls_mpi_release (&tmp3);
  _gnutls_mpi_release (&tmp4);
  _gnutls_mpi_release (&k);

  return S;

freeall:
  _gnutls_mpi_release (&k);
  _gnutls_mpi_release (&tmp1);
  _gnutls_mpi_release (&tmp2);
  _gnutls_mpi_release (&tmp3);
  _gnutls_mpi_release (&tmp4);
  _gnutls_mpi_release (&S);
  return NULL;
}

/**
 * gnutls_srp_free_client_credentials:
 * @sc: is a #gnutls_srp_client_credentials_t structure.
 *
 * This structure is complex enough to manipulate directly thus
 * this helper function is provided in order to free (deallocate) it.
 **/
void
gnutls_srp_free_client_credentials (gnutls_srp_client_credentials_t sc)
{
  gnutls_free (sc->username);
  gnutls_free (sc->password);
  gnutls_free (sc);
}

/**
 * gnutls_srp_allocate_client_credentials:
 * @sc: is a pointer to a #gnutls_srp_server_credentials_t structure.
 *
 * This structure is complex enough to manipulate directly thus
 * this helper function is provided in order to allocate it.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, or an
 *   error code.
 **/
int
gnutls_srp_allocate_client_credentials (gnutls_srp_client_credentials_t * sc)
{
  *sc = gnutls_calloc (1, sizeof (srp_client_credentials_st));

  if (*sc == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  return 0;
}

/**
 * gnutls_srp_set_client_credentials:
 * @res: is a #gnutls_srp_client_credentials_t structure.
 * @username: is the user's userid
 * @password: is the user's password
 *
 * This function sets the username and password, in a
 * #gnutls_srp_client_credentials_t structure.  Those will be used in
 * SRP authentication.  @username and @password should be ASCII
 * strings or UTF-8 strings prepared using the "SASLprep" profile of
 * "stringprep".
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, or an
 *   error code.
 **/
int
gnutls_srp_set_client_credentials (gnutls_srp_client_credentials_t res,
                                   const char *username, const char *password)
{

  if (username == NULL || password == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  res->username = gnutls_strdup (username);
  if (res->username == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  res->password = gnutls_strdup (password);
  if (res->password == NULL)
    {
      gnutls_free (res->username);
      return GNUTLS_E_MEMORY_ERROR;
    }

  return 0;
}

/**
 * gnutls_srp_free_server_credentials:
 * @sc: is a #gnutls_srp_server_credentials_t structure.
 *
 * This structure is complex enough to manipulate directly thus
 * this helper function is provided in order to free (deallocate) it.
 **/
void
gnutls_srp_free_server_credentials (gnutls_srp_server_credentials_t sc)
{
  gnutls_free (sc->password_file);
  gnutls_free (sc->password_conf_file);

  gnutls_free (sc);
}

/**
 * gnutls_srp_allocate_server_credentials:
 * @sc: is a pointer to a #gnutls_srp_server_credentials_t structure.
 *
 * This structure is complex enough to manipulate directly thus this
 * helper function is provided in order to allocate it.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, or an
 *   error code.
 **/
int
gnutls_srp_allocate_server_credentials (gnutls_srp_server_credentials_t * sc)
{
  *sc = gnutls_calloc (1, sizeof (srp_server_cred_st));

  if (*sc == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  return 0;
}

/**
 * gnutls_srp_set_server_credentials_file:
 * @res: is a #gnutls_srp_server_credentials_t structure.
 * @password_file: is the SRP password file (tpasswd)
 * @password_conf_file: is the SRP password conf file (tpasswd.conf)
 *
 * This function sets the password files, in a
 * #gnutls_srp_server_credentials_t structure.  Those password files
 * hold usernames and verifiers and will be used for SRP
 * authentication.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, or an
 *   error code.
 **/
int
gnutls_srp_set_server_credentials_file (gnutls_srp_server_credentials_t res,
                                        const char *password_file,
                                        const char *password_conf_file)
{

  if (password_file == NULL || password_conf_file == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  /* Check if the files can be opened */
  if (_gnutls_file_exists (password_file) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_FILE_ERROR;
    }

  if (_gnutls_file_exists (password_conf_file) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_FILE_ERROR;
    }

  res->password_file = gnutls_strdup (password_file);
  if (res->password_file == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  res->password_conf_file = gnutls_strdup (password_conf_file);
  if (res->password_conf_file == NULL)
    {
      gnutls_assert ();
      gnutls_free (res->password_file);
      res->password_file = NULL;
      return GNUTLS_E_MEMORY_ERROR;
    }

  return 0;
}


/**
 * gnutls_srp_set_server_credentials_function:
 * @cred: is a #gnutls_srp_server_credentials_t structure.
 * @func: is the callback function
 *
 * This function can be used to set a callback to retrieve the user's
 * SRP credentials.  The callback's function form is:
 *
 * int (*callback)(gnutls_session_t, const char* username,
 *  gnutls_datum_t* salt, gnutls_datum_t *verifier, gnutls_datum_t* g,
 *  gnutls_datum_t* n);
 *
 * @username contains the actual username.
 * The @salt, @verifier, @generator and @prime must be filled
 * in using the gnutls_malloc(). For convenience @prime and @generator
 * may also be one of the static parameters defined in extra.h.
 *
 * In case the callback returned a negative number then gnutls will
 * assume that the username does not exist.
 *
 * In order to prevent attackers from guessing valid usernames,
 * if a user does not exist, g and n values should be filled in
 * using a random user's parameters. In that case the callback must
 * return the special value (1).
 *
 * The callback function will only be called once per handshake.
 * The callback function should return 0 on success, while
 * -1 indicates an error.
 **/
void
gnutls_srp_set_server_credentials_function (gnutls_srp_server_credentials_t
                                            cred,
                                            gnutls_srp_server_credentials_function
                                            * func)
{
  cred->pwd_callback = func;
}

/**
 * gnutls_srp_set_client_credentials_function:
 * @cred: is a #gnutls_srp_server_credentials_t structure.
 * @func: is the callback function
 *
 * This function can be used to set a callback to retrieve the
 * username and password for client SRP authentication.  The
 * callback's function form is:
 *
 * int (*callback)(gnutls_session_t, char** username, char**password);
 *
 * The @username and @password must be allocated using
 * gnutls_malloc().  @username and @password should be ASCII strings
 * or UTF-8 strings prepared using the "SASLprep" profile of
 * "stringprep".
 *
 * The callback function will be called once per handshake before the
 * initial hello message is sent.
 *
 * The callback should not return a negative error code the second
 * time called, since the handshake procedure will be aborted.
 *
 * The callback function should return 0 on success.
 * -1 indicates an error.
 **/
void
gnutls_srp_set_client_credentials_function (gnutls_srp_client_credentials_t
                                            cred,
                                            gnutls_srp_client_credentials_function
                                            * func)
{
  cred->get_function = func;
}


/**
 * gnutls_srp_server_get_username:
 * @session: is a gnutls session
 *
 * This function will return the username of the peer.  This should
 * only be called in case of SRP authentication and in case of a
 * server.  Returns NULL in case of an error.
 *
 * Returns: SRP username of the peer, or NULL in case of error.
 **/
const char *
gnutls_srp_server_get_username (gnutls_session_t session)
{
  srp_server_auth_info_t info;

  CHECK_AUTH (GNUTLS_CRD_SRP, NULL);

  info = _gnutls_get_auth_info (session);
  if (info == NULL)
    return NULL;
  return info->username;
}

/**
 * gnutls_srp_verifier:
 * @username: is the user's name
 * @password: is the user's password
 * @salt: should be some randomly generated bytes
 * @generator: is the generator of the group
 * @prime: is the group's prime
 * @res: where the verifier will be stored.
 *
 * This function will create an SRP verifier, as specified in
 * RFC2945.  The @prime and @generator should be one of the static
 * parameters defined in gnutls/extra.h or may be generated using the
 * libgcrypt functions gcry_prime_generate() and
 * gcry_prime_group_generator().
 *
 * The verifier will be allocated with @malloc and will be stored in
 * @res using binary format.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, or an
 *   error code.
 **/
int
gnutls_srp_verifier (const char *username, const char *password,
                     const gnutls_datum_t * salt,
                     const gnutls_datum_t * generator,
                     const gnutls_datum_t * prime, gnutls_datum_t * res)
{
  bigint_t _n, _g;
  int ret;
  size_t digest_size = 20, size;
  opaque digest[20];

  ret = _gnutls_calc_srp_sha (username, password, salt->data,
                              salt->size, &digest_size, digest);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  size = prime->size;
  if (_gnutls_mpi_scan_nz (&_n, prime->data, size))
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  size = generator->size;
  if (_gnutls_mpi_scan_nz (&_g, generator->data, size))
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  ret = _gnutls_srp_gx (digest, 20, &res->data, _g, _n, malloc);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }
  res->size = ret;

  return 0;
}

/**
 * gnutls_srp_set_prime_bits:
 * @session: is a #gnutls_session_t structure.
 * @bits: is the number of bits
 *
 * This function sets the minimum accepted number of bits, for use in
 * an SRP key exchange.  If zero, the default 2048 bits will be used.
 *
 * In the client side it sets the minimum accepted number of bits.  If
 * a server sends a prime with less bits than that
 * %GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER will be returned by the
 * handshake.
 *
 * This function has no effect in server side.
 *
 * Since: 2.6.0
 **/
void
gnutls_srp_set_prime_bits (gnutls_session_t session, unsigned int bits)
{
  session->internals.srp_prime_bits = bits;
}

#endif /* ENABLE_SRP */
