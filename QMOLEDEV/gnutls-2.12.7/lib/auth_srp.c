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

#ifdef ENABLE_SRP

#include "gnutls_errors.h"
#include "auth_srp_passwd.h"
#include "gnutls_auth.h"
#include "gnutls_auth.h"
#include "gnutls_srp.h"
#include "gnutls_num.h"
#include "auth_srp.h"
#include <gnutls_str.h>
#include <gnutls_datum.h>
#include <ext_srp.h>

const mod_auth_st srp_auth_struct = {
  "SRP",
  NULL,
  NULL,
  _gnutls_gen_srp_server_kx,
  _gnutls_gen_srp_client_kx,
  NULL,
  NULL,

  NULL,
  NULL,                         /* certificate */
  _gnutls_proc_srp_server_kx,
  _gnutls_proc_srp_client_kx,
  NULL,
  NULL
};


#define _b session->key->b
#define B session->key->B
#define _a session->key->a
#define A session->key->A
#define N session->key->client_p
#define G session->key->client_g
#define V session->key->x
#define S session->key->KEY

/* Checks if b%n==0 which is a fatal srp error.
 * Returns a proper error code in that case, and 0 when
 * all are ok.
 */
inline static int
check_b_mod_n (bigint_t b, bigint_t n)
{
  int ret;
  bigint_t r;

  r = _gnutls_mpi_mod (b, n);

  if (r == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ret = _gnutls_mpi_cmp_ui (r, 0);

  _gnutls_mpi_release (&r);

  if (ret == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER;
    }

  return 0;
}

/* Checks if a%n==0,+1,-1%n which is a fatal srp error.
 * Returns a proper error code in that case, and 0 when
 * all are ok.
 */
inline static int
check_a_mod_n (bigint_t a, bigint_t n)
{
  int ret;
  bigint_t r;

  r = _gnutls_mpi_mod (a, n);
  if (r == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ret = _gnutls_mpi_cmp_ui (r, 0);

  _gnutls_mpi_release (&r);

  if (ret == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER;
    }

  return 0;
}


/* Send the first key exchange message ( g, n, s) and append the verifier algorithm number 
 * Data is allocated by the caller, and should have data_size size.
 */
int
_gnutls_gen_srp_server_kx (gnutls_session_t session, opaque ** data)
{
  int ret;
  uint8_t *data_n, *data_s;
  uint8_t *data_g;
  char *username;
  SRP_PWD_ENTRY *pwd_entry;
  srp_server_auth_info_t info;
  ssize_t data_size;
  size_t n_b, tmp_size;
  char buf[64];
  uint8_t *data_b;
  extension_priv_data_t epriv;
  srp_ext_st *priv;

  ret = _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_SRP, &epriv);
  if (ret < 0)                  /* peer didn't send a username */
    {
      gnutls_assert ();
      return GNUTLS_E_UNKNOWN_SRP_USERNAME;
    }
  priv = epriv.ptr;

  if ((ret =
       _gnutls_auth_info_set (session, GNUTLS_CRD_SRP,
                              sizeof (srp_server_auth_info_st), 1)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  info = _gnutls_get_auth_info (session);
  username = info->username;

  _gnutls_str_cpy (username, MAX_USERNAME_SIZE, priv->username);

  ret = _gnutls_srp_pwd_read_entry (session, username, &pwd_entry);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* copy from pwd_entry to local variables (actually in session) */
  tmp_size = pwd_entry->g.size;
  if (_gnutls_mpi_scan_nz (&G, pwd_entry->g.data, tmp_size) < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  tmp_size = pwd_entry->n.size;
  if (_gnutls_mpi_scan_nz (&N, pwd_entry->n.data, tmp_size) < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  tmp_size = pwd_entry->v.size;
  if (_gnutls_mpi_scan_nz (&V, pwd_entry->v.data, tmp_size) < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  /* Calculate:  B = (k*v + g^b) % N 
   */
  B = _gnutls_calc_srp_B (&_b, G, N, V);
  if (B == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  if (_gnutls_mpi_print (B, NULL, &n_b) != GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_PRINT_FAILED;
    }


  /* Allocate size to hold the N, g, s, B 
   */

  data_size = (pwd_entry->n.size + 2 + pwd_entry->g.size + 2 +
               pwd_entry->salt.size + 1) + (n_b + 2);

  (*data) = gnutls_malloc (data_size);
  if ((*data) == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* copy N (mod n) 
   */
  data_n = *data;
  _gnutls_write_datum16 (data_n, pwd_entry->n);


  /* copy G (generator) to data 
   */
  data_g = &data_n[2 + pwd_entry->n.size];
  _gnutls_write_datum16 (data_g, pwd_entry->g);


  /* copy the salt 
   */
  data_s = &data_g[2 + pwd_entry->g.size];
  _gnutls_write_datum8 (data_s, pwd_entry->salt);


  /* Copy the B value
   */

  data_b = &data_s[1 + pwd_entry->salt.size];
  if (_gnutls_mpi_print (B, &data_b[2], &n_b) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_PRINT_FAILED;
    }

  _gnutls_write_uint16 (n_b, data_b);

  _gnutls_hard_log ("INT: SRP B[%d]: %s\n", (int) n_b,
                    _gnutls_bin2hex (&data_b[2], n_b, buf, sizeof (buf),
                                     NULL));

  _gnutls_srp_entry_free (pwd_entry);

  return data_size;
}

/* return A = g^a % N */
int
_gnutls_gen_srp_client_kx (gnutls_session_t session, opaque ** data)
{
  size_t n_a;
  int ret;
  uint8_t *data_a;
  char *username, *password;
  char buf[64];
  gnutls_srp_client_credentials_t cred;
  extension_priv_data_t epriv;
  srp_ext_st *priv;

  ret = _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_SRP, &epriv);
  if (ret < 0)                  /* peer didn't send a username */
    {
      gnutls_assert ();
      return GNUTLS_E_UNKNOWN_SRP_USERNAME;
    }
  priv = epriv.ptr;

  cred = (gnutls_srp_client_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_SRP, NULL);

  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  if (priv->username == NULL)
    {
      username = cred->username;
      password = cred->password;
    }
  else
    {

      username = priv->username;
      password = priv->password;
    }

  if (username == NULL || password == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  /* calc A = g^a % N 
   */
  if (G == NULL || N == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  A = _gnutls_calc_srp_A (&_a, G, N);
  if (A == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* Rest of SRP calculations 
   */

  /* calculate u */
  session->key->u = _gnutls_calc_srp_u (A, B, N);
  if (session->key->u == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  _gnutls_mpi_log ("SRP U: ", session->key->u);

  /* S = (B - g^x) ^ (a + u * x) % N */
  S = _gnutls_calc_srp_S2 (B, G, session->key->x, _a, session->key->u, N);
  if (S == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  _gnutls_mpi_log ("SRP B: ", B);

  _gnutls_mpi_release (&_b);
  _gnutls_mpi_release (&V);
  _gnutls_mpi_release (&session->key->u);
  _gnutls_mpi_release (&B);

  ret = _gnutls_mpi_dprint (session->key->KEY, &session->key->key);
  _gnutls_mpi_release (&S);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (_gnutls_mpi_print (A, NULL, &n_a) != GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_PRINT_FAILED;
    }

  (*data) = gnutls_malloc (n_a + 2);
  if ((*data) == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* copy A */
  data_a = (*data);
  if (_gnutls_mpi_print (A, &data_a[2], &n_a) != 0)
    {
      gnutls_free (*data);
      return GNUTLS_E_MPI_PRINT_FAILED;
    }

  _gnutls_hard_log ("INT: SRP A[%d]: %s\n", (int) n_a,
                    _gnutls_bin2hex (&data_a[2], n_a, buf, sizeof (buf),
                                     NULL));

  _gnutls_mpi_release (&A);

  _gnutls_write_uint16 (n_a, data_a);

  return n_a + 2;
}


/* just read A and put it to session */
int
_gnutls_proc_srp_client_kx (gnutls_session_t session, opaque * data,
                            size_t _data_size)
{
  size_t _n_A;
  ssize_t data_size = _data_size;
  int ret;

  DECR_LEN (data_size, 2);
  _n_A = _gnutls_read_uint16 (&data[0]);

  DECR_LEN (data_size, _n_A);
  if (_gnutls_mpi_scan_nz (&A, &data[2], _n_A) || A == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  _gnutls_mpi_log ("SRP A: ", A);
  _gnutls_mpi_log ("SRP B: ", B);

  /* Checks if A % n == 0.
   */
  if ((ret = check_a_mod_n (A, N)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* Start the SRP calculations.
   * - Calculate u 
   */
  session->key->u = _gnutls_calc_srp_u (A, B, N);
  if (session->key->u == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  _gnutls_mpi_log ("SRP U: ", session->key->u);

  /* S = (A * v^u) ^ b % N 
   */
  S = _gnutls_calc_srp_S1 (A, _b, session->key->u, V, N);
  if (S == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  _gnutls_mpi_log ("SRP S: ", S);

  _gnutls_mpi_release (&A);
  _gnutls_mpi_release (&_b);
  _gnutls_mpi_release (&V);
  _gnutls_mpi_release (&session->key->u);
  _gnutls_mpi_release (&B);

  ret = _gnutls_mpi_dprint (session->key->KEY, &session->key->key);
  _gnutls_mpi_release (&S);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}



/* Static parameters according to draft-ietf-tls-srp-07
 * Note that if more parameters are added check_g_n()
 * and _gnutls_srp_entry_free() should be changed.
 */
static const unsigned char srp_params_1024[] = {
  0xEE, 0xAF, 0x0A, 0xB9, 0xAD, 0xB3, 0x8D, 0xD6,
  0x9C, 0x33, 0xF8, 0x0A, 0xFA, 0x8F, 0xC5, 0xE8,
  0x60, 0x72, 0x61, 0x87, 0x75, 0xFF, 0x3C, 0x0B,
  0x9E, 0xA2, 0x31, 0x4C, 0x9C, 0x25, 0x65, 0x76,
  0xD6, 0x74, 0xDF, 0x74, 0x96, 0xEA, 0x81, 0xD3,
  0x38, 0x3B, 0x48, 0x13, 0xD6, 0x92, 0xC6, 0xE0,
  0xE0, 0xD5, 0xD8, 0xE2, 0x50, 0xB9, 0x8B, 0xE4,
  0x8E, 0x49, 0x5C, 0x1D, 0x60, 0x89, 0xDA, 0xD1,
  0x5D, 0xC7, 0xD7, 0xB4, 0x61, 0x54, 0xD6, 0xB6,
  0xCE, 0x8E, 0xF4, 0xAD, 0x69, 0xB1, 0x5D, 0x49,
  0x82, 0x55, 0x9B, 0x29, 0x7B, 0xCF, 0x18, 0x85,
  0xC5, 0x29, 0xF5, 0x66, 0x66, 0x0E, 0x57, 0xEC,
  0x68, 0xED, 0xBC, 0x3C, 0x05, 0x72, 0x6C, 0xC0,
  0x2F, 0xD4, 0xCB, 0xF4, 0x97, 0x6E, 0xAA, 0x9A,
  0xFD, 0x51, 0x38, 0xFE, 0x83, 0x76, 0x43, 0x5B,
  0x9F, 0xC6, 0x1D, 0x2F, 0xC0, 0xEB, 0x06, 0xE3
};

static const unsigned char srp_generator = 0x02;

const gnutls_datum_t gnutls_srp_1024_group_prime = {
  (void *) srp_params_1024, sizeof (srp_params_1024)
};

const gnutls_datum_t gnutls_srp_1024_group_generator = {
  (void *) &srp_generator, sizeof (srp_generator)
};

static const unsigned char srp_params_1536[] = {
  0x9D, 0xEF, 0x3C, 0xAF, 0xB9, 0x39, 0x27, 0x7A, 0xB1,
  0xF1, 0x2A, 0x86, 0x17, 0xA4, 0x7B, 0xBB, 0xDB, 0xA5,
  0x1D, 0xF4, 0x99, 0xAC, 0x4C, 0x80, 0xBE, 0xEE, 0xA9,
  0x61, 0x4B, 0x19, 0xCC, 0x4D, 0x5F, 0x4F, 0x5F, 0x55,
  0x6E, 0x27, 0xCB, 0xDE, 0x51, 0xC6, 0xA9, 0x4B, 0xE4,
  0x60, 0x7A, 0x29, 0x15, 0x58, 0x90, 0x3B, 0xA0, 0xD0,
  0xF8, 0x43, 0x80, 0xB6, 0x55, 0xBB, 0x9A, 0x22, 0xE8,
  0xDC, 0xDF, 0x02, 0x8A, 0x7C, 0xEC, 0x67, 0xF0, 0xD0,
  0x81, 0x34, 0xB1, 0xC8, 0xB9, 0x79, 0x89, 0x14, 0x9B,
  0x60, 0x9E, 0x0B, 0xE3, 0xBA, 0xB6, 0x3D, 0x47, 0x54,
  0x83, 0x81, 0xDB, 0xC5, 0xB1, 0xFC, 0x76, 0x4E, 0x3F,
  0x4B, 0x53, 0xDD, 0x9D, 0xA1, 0x15, 0x8B, 0xFD, 0x3E,
  0x2B, 0x9C, 0x8C, 0xF5, 0x6E, 0xDF, 0x01, 0x95, 0x39,
  0x34, 0x96, 0x27, 0xDB, 0x2F, 0xD5, 0x3D, 0x24, 0xB7,
  0xC4, 0x86, 0x65, 0x77, 0x2E, 0x43, 0x7D, 0x6C, 0x7F,
  0x8C, 0xE4, 0x42, 0x73, 0x4A, 0xF7, 0xCC, 0xB7, 0xAE,
  0x83, 0x7C, 0x26, 0x4A, 0xE3, 0xA9, 0xBE, 0xB8, 0x7F,
  0x8A, 0x2F, 0xE9, 0xB8, 0xB5, 0x29, 0x2E, 0x5A, 0x02,
  0x1F, 0xFF, 0x5E, 0x91, 0x47, 0x9E, 0x8C, 0xE7, 0xA2,
  0x8C, 0x24, 0x42, 0xC6, 0xF3, 0x15, 0x18, 0x0F, 0x93,
  0x49, 0x9A, 0x23, 0x4D, 0xCF, 0x76, 0xE3, 0xFE, 0xD1,
  0x35, 0xF9, 0xBB
};

const gnutls_datum_t gnutls_srp_1536_group_prime = {
  (void *) srp_params_1536, sizeof (srp_params_1536)
};

const gnutls_datum_t gnutls_srp_1536_group_generator = {
  (void *) &srp_generator, sizeof (srp_generator)
};

static const unsigned char srp_params_2048[] = {
  0xAC, 0x6B, 0xDB, 0x41, 0x32, 0x4A, 0x9A, 0x9B, 0xF1,
  0x66, 0xDE, 0x5E, 0x13, 0x89, 0x58, 0x2F, 0xAF, 0x72,
  0xB6, 0x65, 0x19, 0x87, 0xEE, 0x07, 0xFC, 0x31, 0x92,
  0x94, 0x3D, 0xB5, 0x60, 0x50, 0xA3, 0x73, 0x29, 0xCB,
  0xB4, 0xA0, 0x99, 0xED, 0x81, 0x93, 0xE0, 0x75, 0x77,
  0x67, 0xA1, 0x3D, 0xD5, 0x23, 0x12, 0xAB, 0x4B, 0x03,
  0x31, 0x0D, 0xCD, 0x7F, 0x48, 0xA9, 0xDA, 0x04, 0xFD,
  0x50, 0xE8, 0x08, 0x39, 0x69, 0xED, 0xB7, 0x67, 0xB0,
  0xCF, 0x60, 0x95, 0x17, 0x9A, 0x16, 0x3A, 0xB3, 0x66,
  0x1A, 0x05, 0xFB, 0xD5, 0xFA, 0xAA, 0xE8, 0x29, 0x18,
  0xA9, 0x96, 0x2F, 0x0B, 0x93, 0xB8, 0x55, 0xF9, 0x79,
  0x93, 0xEC, 0x97, 0x5E, 0xEA, 0xA8, 0x0D, 0x74, 0x0A,
  0xDB, 0xF4, 0xFF, 0x74, 0x73, 0x59, 0xD0, 0x41, 0xD5,
  0xC3, 0x3E, 0xA7, 0x1D, 0x28, 0x1E, 0x44, 0x6B, 0x14,
  0x77, 0x3B, 0xCA, 0x97, 0xB4, 0x3A, 0x23, 0xFB, 0x80,
  0x16, 0x76, 0xBD, 0x20, 0x7A, 0x43, 0x6C, 0x64, 0x81,
  0xF1, 0xD2, 0xB9, 0x07, 0x87, 0x17, 0x46, 0x1A, 0x5B,
  0x9D, 0x32, 0xE6, 0x88, 0xF8, 0x77, 0x48, 0x54, 0x45,
  0x23, 0xB5, 0x24, 0xB0, 0xD5, 0x7D, 0x5E, 0xA7, 0x7A,
  0x27, 0x75, 0xD2, 0xEC, 0xFA, 0x03, 0x2C, 0xFB, 0xDB,
  0xF5, 0x2F, 0xB3, 0x78, 0x61, 0x60, 0x27, 0x90, 0x04,
  0xE5, 0x7A, 0xE6, 0xAF, 0x87, 0x4E, 0x73, 0x03, 0xCE,
  0x53, 0x29, 0x9C, 0xCC, 0x04, 0x1C, 0x7B, 0xC3, 0x08,
  0xD8, 0x2A, 0x56, 0x98, 0xF3, 0xA8, 0xD0, 0xC3, 0x82,
  0x71, 0xAE, 0x35, 0xF8, 0xE9, 0xDB, 0xFB, 0xB6, 0x94,
  0xB5, 0xC8, 0x03, 0xD8, 0x9F, 0x7A, 0xE4, 0x35, 0xDE,
  0x23, 0x6D, 0x52, 0x5F, 0x54, 0x75, 0x9B, 0x65, 0xE3,
  0x72, 0xFC, 0xD6, 0x8E, 0xF2, 0x0F, 0xA7, 0x11, 0x1F,
  0x9E, 0x4A, 0xFF, 0x73
};

const gnutls_datum_t gnutls_srp_2048_group_prime = {
  (void *) srp_params_2048, sizeof (srp_params_2048)
};

const gnutls_datum_t gnutls_srp_2048_group_generator = {
  (void *) &srp_generator, sizeof (srp_generator)
};


/* Check if G and N are parameters from the SRP draft.
 */
static int
check_g_n (const opaque * g, size_t n_g, const opaque * n, size_t n_n)
{

  if (n_g != 1 || g[0] != srp_generator)
    {
      gnutls_assert ();
      return GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER;
    }

  if (n_n == sizeof (srp_params_1024) &&
      memcmp (srp_params_1024, n, n_n) == 0)
    {
      return 0;
    }

  if (n_n == sizeof (srp_params_1536) &&
      memcmp (srp_params_1536, n, n_n) == 0)
    {
      return 0;
    }

  if (n_n == sizeof (srp_params_2048) &&
      memcmp (srp_params_2048, n, n_n) == 0)
    {
      return 0;
    }

  gnutls_assert ();
  return GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER;
}

/* Check if N is a prime and G a generator of the
 * group. This is check only done if N is big enough.
 * Otherwise only the included parameters must be used.
 */
static int
group_check_g_n (gnutls_session_t session, bigint_t g, bigint_t n)
{
  bigint_t q = NULL, two = NULL, w = NULL;
  int ret;

  if (_gnutls_mpi_get_nbits (n) < (session->internals.srp_prime_bits
                                   ? session->internals.srp_prime_bits
                                   : 2048))
    {
      gnutls_assert ();
      return GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER;
    }

  /* N must be of the form N=2q+1
   * where q is also a prime.
   */
  if (_gnutls_prime_check (n) != 0)
    {
      _gnutls_mpi_log ("no prime N: ", n);
      gnutls_assert ();
      return GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER;
    }

  two = _gnutls_mpi_new (4);
  if (two == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  q = _gnutls_mpi_alloc_like (n);
  if (q == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto error;
    }

  /* q = n-1 
   */
  _gnutls_mpi_sub_ui (q, n, 1);

  /* q = q/2, remember that q is divisible by 2 (prime - 1)
   */
  _gnutls_mpi_set_ui (two, 2);
  _gnutls_mpi_div (q, q, two);

  if (_gnutls_prime_check (q) != 0)
    {
      /* N was not on the form N=2q+1, where q = prime
       */
      _gnutls_mpi_log ("no prime Q: ", q);
      gnutls_assert ();
      return GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER;
    }

  /* We also check whether g is a generator,
   */

  /* check if g < q < N
   */
  if (_gnutls_mpi_cmp (g, q) >= 0)
    {
      gnutls_assert ();
      ret = GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER;
      goto error;
    }

  w = _gnutls_mpi_alloc_like (q);
  if (w == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto error;
    }

  /* check if g^q mod N == N-1
   * w = g^q mod N
   */
  _gnutls_mpi_powm (w, g, q, n);

  /* w++
   */
  _gnutls_mpi_add_ui (w, w, 1);

  if (_gnutls_mpi_cmp (w, n) != 0)
    {
      gnutls_assert ();
      ret = GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER;
      goto error;
    }

  ret = 0;

error:
  _gnutls_mpi_release (&q);
  _gnutls_mpi_release (&two);
  _gnutls_mpi_release (&w);

  return ret;

}

/* receive the key exchange message ( n, g, s, B) 
 */
int
_gnutls_proc_srp_server_kx (gnutls_session_t session, opaque * data,
                            size_t _data_size)
{
  uint8_t n_s;
  uint16_t n_g, n_n, n_b;
  size_t _n_s, _n_g, _n_n, _n_b;
  const uint8_t *data_n;
  const uint8_t *data_g;
  const uint8_t *data_s;
  const uint8_t *data_b;
  int i, ret;
  opaque hd[SRP_MAX_HASH_SIZE];
  char *username, *password;
  ssize_t data_size = _data_size;
  gnutls_srp_client_credentials_t cred;
  extension_priv_data_t epriv;
  srp_ext_st *priv;

  ret = _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_SRP, &epriv);
  if (ret < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_UNKNOWN_SRP_USERNAME;
    }
  priv = epriv.ptr;

  cred = (gnutls_srp_client_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_SRP, NULL);

  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  if (priv->username == NULL)
    {
      username = cred->username;
      password = cred->password;
    }
  else
    {
      username = priv->username;
      password = priv->password;
    }

  if (username == NULL || password == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  i = 0;

  /* Read N 
   */
  DECR_LEN (data_size, 2);
  n_n = _gnutls_read_uint16 (&data[i]);
  i += 2;

  DECR_LEN (data_size, n_n);
  data_n = &data[i];
  i += n_n;

  /* Read G 
   */
  DECR_LEN (data_size, 2);
  n_g = _gnutls_read_uint16 (&data[i]);
  i += 2;

  DECR_LEN (data_size, n_g);
  data_g = &data[i];
  i += n_g;

  /* Read salt 
   */
  DECR_LEN (data_size, 1);
  n_s = data[i];
  i += 1;

  DECR_LEN (data_size, n_s);
  data_s = &data[i];
  i += n_s;

  /* Read B 
   */
  DECR_LEN (data_size, 2);
  n_b = _gnutls_read_uint16 (&data[i]);
  i += 2;

  DECR_LEN (data_size, n_b);
  data_b = &data[i];
  i += n_b;

  _n_s = n_s;
  _n_g = n_g;
  _n_n = n_n;
  _n_b = n_b;

  if (_gnutls_mpi_scan_nz (&N, data_n, _n_n) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  if (_gnutls_mpi_scan_nz (&G, data_g, _n_g) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  if (_gnutls_mpi_scan_nz (&B, data_b, _n_b) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }


  /* Check if the g and n are from the SRP
   * draft. Otherwise check if N is a prime and G
   * a generator.
   */
  if ((ret = check_g_n (data_g, _n_g, data_n, _n_n)) < 0)
    {
      _gnutls_x509_log ("Checking the SRP group parameters.\n");
      if ((ret = group_check_g_n (session, G, N)) < 0)
        {
          gnutls_assert ();
          return ret;
        }
    }

  /* Checks if b % n == 0
   */
  if ((ret = check_b_mod_n (B, N)) < 0)
    {
      gnutls_assert ();
      return ret;
    }


  /* generate x = SHA(s | SHA(U | ":" | p))
   * (or the equivalent using bcrypt)
   */
  if ((ret =
       _gnutls_calc_srp_x (username, password, (opaque *) data_s, n_s,
                           &_n_g, hd)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (_gnutls_mpi_scan_nz (&session->key->x, hd, _n_g) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }


  return i;                     /* return the processed data
                                 * needed in auth_srp_rsa.
                                 */
}

#endif /* ENABLE_SRP */
