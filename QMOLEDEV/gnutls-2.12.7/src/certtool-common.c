/*
 * Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
 *   2011 Free Software Foundation, Inc.
 *
 * This file is part of GnuTLS.
 *
 * GnuTLS is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <gnutls/gnutls.h>
#include <gnutls/extra.h>
#include <gnutls/x509.h>
#include <gnutls/openpgp.h>
#include <gnutls/pkcs12.h>
#include <gnutls/pkcs11.h>
#include <gnutls/abstract.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <error.h>
#include "certtool-common.h"
#include "certtool-cfg.h"

/* Gnulib portability files. */
#include <read-file.h>

unsigned char buffer[64 * 1024];
const int buffer_size = sizeof (buffer);


FILE *
safe_open_rw (const char *file, int privkey_op)
{
  mode_t omask = 0;
  FILE *fh;

  if (privkey_op != 0)
    {
      omask = umask (S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
    }

  fh = fopen (file, "wb");

  if (privkey_op != 0)
    {
      umask (omask);
    }

  return fh;
}

gnutls_datum_t *
load_secret_key (int mand, common_info_st * info)
{
  unsigned char raw_key[64];
  size_t raw_key_size = sizeof (raw_key);
  static gnutls_datum_t key;
  gnutls_datum_t hex_key;
  int ret;

  fprintf (stderr, "Loading secret key...\n");

  if (info->secret_key == NULL)
    {
      if (mand)
        error (EXIT_FAILURE, 0, "missing --secret-key");
      else
        return NULL;
    }

  hex_key.data = (char *) info->secret_key;
  hex_key.size = strlen (info->secret_key);

  ret = gnutls_hex_decode (&hex_key, raw_key, &raw_key_size);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "hex_decode: %s", gnutls_strerror (ret));

  key.data = raw_key;
  key.size = raw_key_size;

  return &key;
}

static gnutls_privkey_t _load_privkey(gnutls_datum_t *dat, common_info_st * info)
{
int ret;
gnutls_privkey_t key;
gnutls_x509_privkey_t xkey;

  ret = gnutls_x509_privkey_init (&xkey);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "x509_privkey_init: %s", gnutls_strerror (ret));

  ret = gnutls_privkey_init (&key);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "privkey_init: %s", gnutls_strerror (ret));

  if (info->pkcs8)
    {
      const char *pass = get_pass ();
      ret =
        gnutls_x509_privkey_import_pkcs8 (xkey, dat, info->incert_format,
                                          pass, 0);
    }
  else
    ret = gnutls_x509_privkey_import (xkey, dat, info->incert_format);

  if (ret == GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR)
    {
      error (EXIT_FAILURE, 0,
             "import error: could not find a valid PEM header; "
             "check if your key is PKCS #8 or PKCS #12 encoded");
    }

  if (ret < 0)
    error (EXIT_FAILURE, 0, "importing --load-privkey: %s: %s",
           info->privkey, gnutls_strerror (ret));

  ret = gnutls_privkey_import_x509(key, xkey, GNUTLS_PRIVKEY_IMPORT_AUTO_RELEASE);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "gnutls_privkey_import_x509: %s",
           gnutls_strerror (ret));
  
  return key;
}

static gnutls_privkey_t _load_pkcs11_privkey(const char* url)
{
int ret;
gnutls_pkcs11_privkey_t p11key;
gnutls_privkey_t key;

  ret = gnutls_privkey_init (&key);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "privkey_init: %s", gnutls_strerror (ret));

  ret = gnutls_pkcs11_privkey_init (&p11key);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "pkcs11_privkey_init: %s", gnutls_strerror (ret));

  ret = gnutls_pkcs11_privkey_import_url(p11key, url, 0);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "importing PKCS #11 key: %s: %s",
           url, gnutls_strerror (ret));

  ret = gnutls_privkey_import_pkcs11(key, p11key, GNUTLS_PRIVKEY_IMPORT_AUTO_RELEASE);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "gnutls_privkey_import_pkcs11: %s",
           gnutls_strerror (ret));
  
  return key;
}

static gnutls_pubkey_t _load_pkcs11_pubkey(const char* url)
{
int ret;
gnutls_pkcs11_obj_t obj;
gnutls_x509_crt_t xcrt;
gnutls_pubkey_t pubkey;
unsigned int obj_flags = 0;

  ret = gnutls_pubkey_init (&pubkey);
  if (ret < 0)
    {
      fprintf (stderr, "Error in %s:%d: %s\n", __func__, __LINE__,
               gnutls_strerror (ret));
      exit (1);
    }

  ret = gnutls_pkcs11_obj_init (&obj);
  if (ret < 0)
    {
      fprintf (stderr, "Error in %s:%d: %s\n", __func__, __LINE__,
               gnutls_strerror (ret));
      exit (1);
    }

  ret = gnutls_pkcs11_obj_import_url (obj, url, obj_flags);
  if (ret < 0)
    {
      fprintf (stderr, "Error in %s:%d: %s: %s\n", __func__, __LINE__,
               gnutls_strerror (ret), url);
      exit (1);
    }

  switch (gnutls_pkcs11_obj_get_type (obj))
    {
    case GNUTLS_PKCS11_OBJ_X509_CRT:
      ret = gnutls_x509_crt_init (&xcrt);
      if (ret < 0)
        {
          fprintf (stderr, "Error in %s:%d: %s\n", __func__, __LINE__,
                   gnutls_strerror (ret));
          exit (1);
        }

      ret = gnutls_x509_crt_import_pkcs11 (xcrt, obj);
      if (ret < 0)
        {
          fprintf (stderr, "Error in %s:%d: %s\n", __func__, __LINE__,
                   gnutls_strerror (ret));
          exit (1);
        }

      ret = gnutls_pubkey_import_x509 (pubkey, xcrt, 0);
      if (ret < 0)
        {
          fprintf (stderr, "Error in %s:%d: %s\n", __func__, __LINE__,
                   gnutls_strerror (ret));
          exit (1);
        }

      gnutls_x509_crt_deinit (xcrt);
      break;
    case GNUTLS_PKCS11_OBJ_PUBKEY:

      ret = gnutls_pubkey_import_pkcs11 (pubkey, obj, 0);
      if (ret < 0)
        {
          fprintf (stderr, "Error in %s:%d: %s\n", __func__, __LINE__,
                   gnutls_strerror (ret));
          exit (1);
        }

      break;
    default:
      {
        fprintf(stderr, "Unsupported PKCS #11 object\n");
        exit (1);
        break;
      }
    }
  
  gnutls_pkcs11_obj_deinit (obj);
  return pubkey;
}


/* Load the private key.
 * @mand should be non zero if it is required to read a private key.
 */
gnutls_privkey_t
load_private_key (int mand, common_info_st * info)
{
  gnutls_privkey_t key;
  gnutls_datum_t dat;
  size_t size;

  if (!info->privkey && !mand)
    return NULL;

  if (info->privkey == NULL)
    error (EXIT_FAILURE, 0, "missing --load-privkey");

  if (strncmp(info->privkey, "pkcs11:", 7) == 0)
    return _load_pkcs11_privkey(info->privkey);

  dat.data = read_binary_file (info->privkey, &size);
  dat.size = size;

  if (!dat.data)
    error (EXIT_FAILURE, errno, "reading --load-privkey: %s", info->privkey);

  key = _load_privkey(&dat, info);

  free (dat.data);

  return key;
}

/* Load the private key.
 * @mand should be non zero if it is required to read a private key.
 */
gnutls_x509_privkey_t
load_x509_private_key (int mand, common_info_st * info)
{
  gnutls_x509_privkey_t key;
  int ret;
  gnutls_datum_t dat;
  size_t size;

  if (!info->privkey && !mand)
    return NULL;

  if (info->privkey == NULL)
    error (EXIT_FAILURE, 0, "missing --load-privkey");

  ret = gnutls_x509_privkey_init (&key);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "privkey_init: %s", gnutls_strerror (ret));

  dat.data = read_binary_file (info->privkey, &size);
  dat.size = size;

  if (!dat.data)
    error (EXIT_FAILURE, errno, "reading --load-privkey: %s", info->privkey);

  if (info->pkcs8)
    {
      const char *pass = get_pass ();
      ret =
        gnutls_x509_privkey_import_pkcs8 (key, &dat, info->incert_format,
                                          pass, 0);
    }
  else
    ret = gnutls_x509_privkey_import (key, &dat, info->incert_format);

  free (dat.data);

  if (ret == GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR)
    {
      error (EXIT_FAILURE, 0,
             "import error: could not find a valid PEM header; "
             "check if your key is PKCS #8 or PKCS #12 encoded");
    }

  if (ret < 0)
    error (EXIT_FAILURE, 0, "importing --load-privkey: %s: %s",
           info->privkey, gnutls_strerror (ret));

  return key;
}

/* Loads the certificate
 * If mand is non zero then a certificate is mandatory. Otherwise
 * null will be returned if the certificate loading fails.
 */
gnutls_x509_crt_t
load_cert (int mand, common_info_st * info)
{
  gnutls_x509_crt_t *crt;
  size_t size;

  crt = load_cert_list (mand, &size, info);

  return crt ? crt[0] : NULL;
}

#define MAX_CERTS 256

/* Loads a certificate list
 */
gnutls_x509_crt_t *
load_cert_list (int mand, size_t * crt_size, common_info_st * info)
{
  FILE *fd;
  static gnutls_x509_crt_t crt[MAX_CERTS];
  char *ptr;
  int ret, i;
  gnutls_datum_t dat;
  size_t size;
  int ptr_size;

  *crt_size = 0;
  fprintf (stderr, "Loading certificate list...\n");

  if (info->cert == NULL)
    {
      if (mand)
        error (EXIT_FAILURE, 0, "missing --load-certificate");
      else
        return NULL;
    }

  fd = fopen (info->cert, "r");
  if (fd == NULL)
    error (EXIT_FAILURE, errno, "%s", info->cert);

  size = fread (buffer, 1, sizeof (buffer) - 1, fd);
  buffer[size] = 0;

  fclose (fd);

  ptr = buffer;
  ptr_size = size;

  for (i = 0; i < MAX_CERTS; i++)
    {
      ret = gnutls_x509_crt_init (&crt[i]);
      if (ret < 0)
        error (EXIT_FAILURE, 0, "crt_init: %s", gnutls_strerror (ret));

      dat.data = ptr;
      dat.size = ptr_size;

      ret = gnutls_x509_crt_import (crt[i], &dat, info->incert_format);
      if (ret < 0 && *crt_size > 0)
        break;
      if (ret < 0)
        error (EXIT_FAILURE, 0, "crt_import: %s", gnutls_strerror (ret));

      ptr = strstr (ptr, "---END");
      if (ptr == NULL)
        break;
      ptr++;

      ptr_size = size;
      ptr_size -=
        (unsigned int) ((unsigned char *) ptr - (unsigned char *) buffer);

      if (ptr_size < 0)
        break;

      (*crt_size)++;
    }
  fprintf (stderr, "Loaded %d certificates.\n", (int) *crt_size);

  return crt;
}

/* Load the Certificate Request.
 */
gnutls_x509_crq_t
load_request (common_info_st * info)
{
  gnutls_x509_crq_t crq;
  int ret;
  gnutls_datum_t dat;
  size_t size;

  if (!info->request)
    return NULL;

  ret = gnutls_x509_crq_init (&crq);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "crq_init: %s", gnutls_strerror (ret));

  dat.data = read_binary_file (info->request, &size);
  dat.size = size;

  if (!dat.data)
    error (EXIT_FAILURE, errno, "reading --load-request: %s", info->request);

  ret = gnutls_x509_crq_import (crq, &dat, info->incert_format);
  if (ret == GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR)
    {
      error (EXIT_FAILURE, 0,
             "import error: could not find a valid PEM header");
    }

  free (dat.data);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "importing --load-request: %s: %s",
           info->request, gnutls_strerror (ret));

  return crq;
}

/* Load the CA's private key.
 */
gnutls_privkey_t
load_ca_private_key (common_info_st * info)
{
  gnutls_privkey_t key;
  gnutls_datum_t dat;
  size_t size;

  if (info->ca_privkey == NULL)
    error (EXIT_FAILURE, 0, "missing --load-ca-privkey");

  if (strncmp(info->ca_privkey, "pkcs11:", 7) == 0)
    return _load_pkcs11_privkey(info->ca_privkey);

  dat.data = read_binary_file (info->ca_privkey, &size);
  dat.size = size;

  if (!dat.data)
    error (EXIT_FAILURE, errno, "reading --load-ca-privkey: %s",
           info->ca_privkey);

  key = _load_privkey(&dat, info);

  free (dat.data);

  return key;
}

/* Loads the CA's certificate
 */
gnutls_x509_crt_t
load_ca_cert (common_info_st * info)
{
  gnutls_x509_crt_t crt;
  int ret;
  gnutls_datum_t dat;
  size_t size;

  if (info->ca == NULL)
    error (EXIT_FAILURE, 0, "missing --load-ca-certificate");

  ret = gnutls_x509_crt_init (&crt);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "crt_init: %s", gnutls_strerror (ret));

  dat.data = read_binary_file (info->ca, &size);
  dat.size = size;

  if (!dat.data)
    error (EXIT_FAILURE, errno, "reading --load-ca-certificate: %s",
           info->ca);

  ret = gnutls_x509_crt_import (crt, &dat, info->incert_format);
  free (dat.data);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "importing --load-ca-certificate: %s: %s",
           info->ca, gnutls_strerror (ret));

  return crt;
}

/* Load a public key.
 * @mand should be non zero if it is required to read a public key.
 */
gnutls_pubkey_t
load_pubkey (int mand, common_info_st * info)
{
  gnutls_pubkey_t key;
  int ret;
  gnutls_datum_t dat;
  size_t size;

  if (!info->pubkey && !mand)
    return NULL;

  if (info->pubkey == NULL)
    error (EXIT_FAILURE, 0, "missing --load-pubkey");

  if (strncmp(info->pubkey, "pkcs11:", 7) == 0)
    return _load_pkcs11_pubkey(info->pubkey);

  ret = gnutls_pubkey_init (&key);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "privkey_init: %s", gnutls_strerror (ret));

  dat.data = read_binary_file (info->pubkey, &size);
  dat.size = size;

  if (!dat.data)
    error (EXIT_FAILURE, errno, "reading --load-pubkey: %s", info->pubkey);

  ret = gnutls_pubkey_import (key, &dat, info->incert_format);

  free (dat.data);

  if (ret == GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR)
    {
      error (EXIT_FAILURE, 0,
             "import error: could not find a valid PEM header; "
             "check if your key has the PUBLIC KEY header");
    }

  if (ret < 0)
    error (EXIT_FAILURE, 0, "importing --load-pubkey: %s: %s",
           info->pubkey, gnutls_strerror (ret));

  return key;
}

gnutls_pubkey_t load_public_key_or_import(int mand, gnutls_privkey_t privkey, common_info_st * info)
{
gnutls_pubkey_t pubkey;
int ret;

  ret = gnutls_pubkey_init(&pubkey);
  if (ret < 0)
    error (EXIT_FAILURE, 0, "gnutls_pubkey_init: %s",
           gnutls_strerror (ret));

  ret = gnutls_pubkey_import_privkey(pubkey, privkey, 0, 0);
  if (ret < 0) /* could not get (e.g. on PKCS #11 */
    {
      gnutls_pubkey_deinit(pubkey);
      return load_pubkey(mand, info);
    }

  return pubkey;
}

