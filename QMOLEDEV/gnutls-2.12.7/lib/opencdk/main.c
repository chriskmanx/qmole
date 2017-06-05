/* main.c
 * Copyright (C) 2001, 2002, 2003, 2007, 2008, 2010 Free Software
 * Foundation, Inc.
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
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef _WIN32
#include <windows.h>
#endif

#include "opencdk.h"
#include "main.h"
#include "packet.h"


/* Set a default cipher algorithm and a digest algorithm.
   Even if AES and SHA-256 are not 'MUST' in the latest
   OpenPGP draft, AES seems to be a good choice. */
#define DEFAULT_DIGEST_ALGO GNUTLS_DIG_SHA256

/* Use the passphrase callback in the handle HD or
   return NULL if there is no valid callback. */
char *
_cdk_passphrase_get (cdk_ctx_t hd, const char *prompt)
{
  if (!hd || !hd->passphrase_cb)
    return NULL;
  return hd->passphrase_cb (hd->passphrase_cb_value, prompt);
}


static void
handle_set_digest (cdk_ctx_t hd, int digest)
{
  if (!hd)
    return;
  if (_gnutls_hash_get_algo_len (digest) <= 0)
    digest = DEFAULT_DIGEST_ALGO;
  hd->digest_algo = digest;
}


static void
handle_set_s2k (cdk_ctx_t hd, int mode, int digest)
{
  if (!hd)
    return;
  if (_gnutls_hash_get_algo_len (digest) <= 0)
    digest = DEFAULT_DIGEST_ALGO;
  if (mode != CDK_S2K_SIMPLE &&
      mode != CDK_S2K_SALTED && mode != CDK_S2K_ITERSALTED)
    mode = CDK_S2K_ITERSALTED;
  hd->_s2k.mode = mode;
  hd->_s2k.digest_algo = digest;
}


static void
handle_set_compress (cdk_ctx_t hd, int algo, int level)
{
  if (!hd)
    return;
  if (algo < 0 || algo > 2)
    algo = 0;
  hd->compress.algo = algo;
  if (!algo)
    hd->opt.compress = 0;
  else
    {
      if (level > 0 && level < 10)
        hd->compress.level = level;
      else
        hd->compress.level = 6;
    }
}


/**
 * cdk_handle_control:
 * @hd: session handle
 * @action: flag which indicates whether put or get is requested
 * @cmd: command id
 *
 * Perform various control operations for the current session.
 **/
int
cdk_handle_control (cdk_ctx_t hd, int action, int cmd, ...)
{
  va_list arg_ptr;
  int set = action == CDK_CTLF_SET, val = 0;

  if (!hd)
    return -1;

  if (action != CDK_CTLF_SET && action != CDK_CTLF_GET)
    return -1;
  va_start (arg_ptr, cmd);
  switch (cmd)
    {
    case CDK_CTL_ARMOR:
      if (set)
        hd->opt.armor = va_arg (arg_ptr, int);
      else
        val = hd->opt.armor;
      break;

    case CDK_CTL_DIGEST:
      if (set)
        handle_set_digest (hd, va_arg (arg_ptr, int));
      else
        val = hd->digest_algo;
      break;

    case CDK_CTL_OVERWRITE:
      if (set)
        hd->opt.overwrite = va_arg (arg_ptr, int);
      else
        val = hd->opt.overwrite;
      break;

    case CDK_CTL_COMPRESS:
      if (set)
        {
          int algo = va_arg (arg_ptr, int);
          int level = va_arg (arg_ptr, int);
          handle_set_compress (hd, algo, level);
        }
      else
        val = hd->compress.algo;
      break;

    case CDK_CTL_S2K:
      if (set)
        {
          int mode = va_arg (arg_ptr, int);
          int digest = va_arg (arg_ptr, int);
          handle_set_s2k (hd, mode, digest);
        }
      else
        val = hd->_s2k.mode;
      break;

    case CDK_CTL_FORCE_DIGEST:
      if (set)
        hd->opt.force_digest = va_arg (arg_ptr, int);
      else
        val = hd->opt.force_digest;
      break;

    case CDK_CTL_BLOCKMODE_ON:
      if (set)
        hd->opt.blockmode = va_arg (arg_ptr, int);
      else
        val = hd->opt.blockmode;
      break;

    default:
      val = -1;
      break;
    }
  va_end (arg_ptr);
  return val;
}



/**
 * cdk_handle_new:
 * @r_ctx: context to store the handle
 *
 * create a new session handle.
 **/
cdk_error_t
cdk_handle_new (cdk_ctx_t * r_ctx)
{
  cdk_ctx_t c;

  if (!r_ctx)
    return CDK_Inv_Value;

  c = cdk_calloc (1, sizeof *c);
  if (!c)
    return CDK_Out_Of_Core;

  /* For S2K use the iterated and salted mode and use the
     default digest and cipher algorithms. Because the MDC
     feature will be used, the default cipher should use a 
     blocksize of 128 bits. */
  c->_s2k.mode = CDK_S2K_ITERSALTED;
  c->_s2k.digest_algo = DEFAULT_DIGEST_ALGO;

  c->opt.mdc = 1;
  c->opt.compress = 1;
  c->opt.armor = 0;
  c->opt.textmode = 0;

  c->digest_algo = DEFAULT_DIGEST_ALGO;

  c->compress.algo = CDK_COMPRESS_ZIP;
  c->compress.level = 6;

  *r_ctx = c;
  return 0;
}


/**
 * cdk_handle_set_keyring:
 * @hd: session handle
 * @type: public=0 or secret=1 keyring type
 * @kringname: file name of the keyring which shall be used.
 * 
 * Convenient function to set the keyring for the current session.
 */
cdk_error_t
cdk_handle_set_keyring (cdk_ctx_t hd, int type, const char *kringname)
{
  cdk_keydb_hd_t db;
  cdk_error_t err;

  err = cdk_keydb_new_from_file (&db, type, kringname);
  if (err)
    return err;

  if (!type)
    hd->db.pub = db;
  else
    hd->db.sec = db;
  hd->db.close_db = 1;
  return 0;
}


/**
 * cdk_handle_set_keydb:
 * @hd: session handle
 * @db: the database handle
 *
 * set the key database handle.
 * the function automatically detects whether this is a public or
 * secret keyring and the right handle is set.
 **/
void
cdk_handle_set_keydb (cdk_ctx_t hd, cdk_keydb_hd_t db)
{
  if (!hd)
    return;
  if (_cdk_keydb_is_secret (db))
    hd->db.sec = db;
  else
    hd->db.pub = db;
}


/**
 * cdk_handle_get_keydb:
 * @hd: session handle
 * @type: type of the keyring
 *
 * Return the keydb handle from the session handle.
 * The caller should not free these handles.
 **/
cdk_keydb_hd_t
cdk_handle_get_keydb (cdk_ctx_t hd, int type)
{
  if (!hd)
    return NULL;
  if (type == CDK_DBTYPE_PK_KEYRING)
    return hd->db.pub;
  else if (type == CDK_DBTYPE_SK_KEYRING)
    return hd->db.sec;
  return NULL;
}


/**
 * cdk_handle_set_passphrase_cb:
 * @hd: session handle
 * @cb: callback function
 * @cb_value: the opaque value for the cb function
 *
 * set the passphrase callback.
 **/
void
cdk_handle_set_passphrase_cb (cdk_ctx_t hd,
                              char *(*cb) (void *opa, const char *prompt),
                              void *cb_value)
{
  if (!hd)
    return;
  hd->passphrase_cb = cb;
  hd->passphrase_cb_value = cb_value;
}

/**
 * cdk_handle_free:
 * @hd: the handle
 *
 * Release the main handle.
 **/
void
cdk_handle_free (cdk_ctx_t hd)
{
  if (!hd)
    return;

  /* If cdk_handle_set_keyring() were used, we need to free the key db
     handles here because the handles are not controlled by the user. */
  if (hd->db.close_db)
    {
      if (hd->db.pub)
        cdk_keydb_free (hd->db.pub);
      if (hd->db.sec)
        cdk_keydb_free (hd->db.sec);
      hd->db.pub = hd->db.sec = NULL;
    }
  cdk_free (hd);
}
