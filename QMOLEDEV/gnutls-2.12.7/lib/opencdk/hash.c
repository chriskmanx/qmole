/* hash.c - Hash filters
 * Copyright (C) 2002, 2003, 2007, 2008, 2010 Free Software Foundation,
 * Inc.
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
#include <sys/stat.h>

#include "opencdk.h"
#include "main.h"
#include "filters.h"

static cdk_error_t
hash_encode (void *data, FILE * in, FILE * out)
{
  md_filter_t *mfx = data;
  byte buf[BUFSIZE];
  int err;
  int nread;

  if (!mfx)
    {
      gnutls_assert ();
      return CDK_Inv_Value;
    }

  _cdk_log_debug ("hash filter: encode algo=%d\n", mfx->digest_algo);

  if (!mfx->md_initialized)
    {
      err = _gnutls_hash_init (&mfx->md, mfx->digest_algo);
      if (err < 0)
        {
          gnutls_assert ();
          return map_gnutls_error (err);
        }

      mfx->md_initialized = 1;
    }

  while (!feof (in))
    {
      nread = fread (buf, 1, BUFSIZE, in);
      if (!nread)
        break;
      _gnutls_hash (&mfx->md, buf, nread);
    }

  wipemem (buf, sizeof (buf));
  return 0;
}

cdk_error_t
_cdk_filter_hash (void *data, int ctl, FILE * in, FILE * out)
{
  if (ctl == STREAMCTL_READ)
    return hash_encode (data, in, out);
  else if (ctl == STREAMCTL_FREE)
    {
      md_filter_t *mfx = data;
      if (mfx)
        {
          _cdk_log_debug ("free hash filter\n");
          _gnutls_hash_deinit (&mfx->md, NULL);
          mfx->md_initialized = 0;
          return 0;
        }
    }

  gnutls_assert ();
  return CDK_Inv_Mode;
}
