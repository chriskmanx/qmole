/*
 * Copyright (C) 2005, 2008, 2010 Free Software Foundation, Inc.
 *
 * Author: Simon Josefsson
 *
 * This file is part of GnuTLS-EXTRA.
 *
 * GnuTLS-extra is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * GnuTLS-extra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 */

#ifndef EXT_IA_H
#define EXT_IA_H

#include <gnutls_extensions.h>

#define IA_PEER_ENABLE (1 << 1)
#define IA_PEER_ALLOW_SKIP (1 << 2)
#define IA_ENABLE (1 << 3)
#define IA_ALLOW_SKIP (1 << 4)

extern extension_entry_st ext_mod_ia;

typedef struct
{
  unsigned int flags;
  /* For TLS/IA.  XXX: Move to IA credential? */
  opaque inner_secret[GNUTLS_MASTER_SIZE];
} ia_ext_st;

inline static void
_gnutls_ia_derive_inner_secret (gnutls_session_t session)
{
  extension_priv_data_t epriv;
  ia_ext_st *priv;
  int ret;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_INNER_APPLICATION,
                                  &epriv);
  if (ret < 0)
    {
      return;
    }
  priv = epriv.ptr;

  memcpy (priv->inner_secret,
          session->security_parameters.master_secret, GNUTLS_MASTER_SIZE);

}

#endif
