/*
 * Copyright (C) 2005, 2006, 2008, 2010 Free Software Foundation, Inc.
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

#include "gnutls_int.h"
#include "gnutls_auth.h"
#include "gnutls_errors.h"
#include "gnutls_num.h"
#include "ext_inner_application.h"
#include <gnutls/extra.h>

#define NO 0
#define YES 1

static int _gnutls_inner_application_recv_params (gnutls_session_t session,
                                                  const opaque * data,
                                                  size_t data_size);
static int _gnutls_inner_application_send_params (gnutls_session_t session,
                                                  opaque * data, size_t);
static int ia_unpack (gnutls_buffer_st * ps, extension_priv_data_t * _priv);
static int ia_pack (extension_priv_data_t _priv, gnutls_buffer_st * ps);
static void ia_deinit_data (extension_priv_data_t priv);

extension_entry_st ext_mod_ia = {
  .name = "INNER APPLICATION",
  .type = GNUTLS_EXTENSION_INNER_APPLICATION,
  .parse_type = GNUTLS_EXT_TLS,

  .recv_func = _gnutls_inner_application_recv_params,
  .send_func = _gnutls_inner_application_send_params,
  .pack_func = ia_pack,
  .unpack_func = ia_unpack,
  .deinit_func = ia_deinit_data,
};

static int
_gnutls_inner_application_recv_params (gnutls_session_t session,
                                       const opaque * data, size_t data_size)
{
  extension_priv_data_t epriv;
  ia_ext_st *priv;
  int ret;

  if (data_size != 1)
    {
      gnutls_assert ();
      return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
    }

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_INNER_APPLICATION,
                                  &epriv);
  if (ret < 0)
    {
      priv = gnutls_calloc (1, sizeof (*priv));
      if (priv == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      epriv.ptr = priv;
      _gnutls_ext_set_session_data (session,
                                    GNUTLS_EXTENSION_INNER_APPLICATION,
                                    epriv);
    }
  else
    priv = epriv.ptr;

  priv->flags |= IA_PEER_ENABLE;
  priv->flags &= ~IA_PEER_ALLOW_SKIP;

  switch ((unsigned char) *data)
    {
    case NO:                   /* Peer's ia_on_resume == no */
      priv->flags |= IA_PEER_ALLOW_SKIP;
      break;

    case YES:
      break;

    default:
      gnutls_assert ();
    }


  return 0;
}


/* returns data_size or a negative number on failure
 */
static int
_gnutls_inner_application_send_params (gnutls_session_t session,
                                       opaque * data, size_t data_size)
{
  extension_priv_data_t epriv;
  ia_ext_st *priv = NULL;
  int ret;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_INNER_APPLICATION,
                                  &epriv);
  if (ret < 0)
    {
      priv = gnutls_calloc (1, sizeof (*priv));
      if (priv == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      epriv.ptr = priv;
      _gnutls_ext_set_session_data (session,
                                    GNUTLS_EXTENSION_INNER_APPLICATION,
                                    epriv);
    }
  else
    priv = epriv.ptr;

  /* Set ext->gnutls_ia_enable depending on whether we have a TLS/IA
     credential in the session. */

  if (session->security_parameters.entity == GNUTLS_CLIENT)
    {
      gnutls_ia_client_credentials_t cred = (gnutls_ia_client_credentials_t)
        _gnutls_get_cred (session->key, GNUTLS_CRD_IA, NULL);

      if (cred)
        priv->flags |= IA_ENABLE;
    }
  else                          /* SERVER */
    {
      gnutls_ia_server_credentials_t cred;

      cred = (gnutls_ia_server_credentials_t)
        _gnutls_get_cred (session->key, GNUTLS_CRD_IA, NULL);

      if (cred)
        priv->flags |= IA_PEER_ENABLE;
    }

  /* If we don't want gnutls_ia locally, or we are a server and the
   * client doesn't want it, don't advertise TLS/IA support at all, as
   * required. */

  if (!(priv->flags & IA_ENABLE))
    return 0;

  if (session->security_parameters.entity == GNUTLS_SERVER &&
      !(priv->flags & IA_PEER_ENABLE))
    return 0;

  /* We'll advertise. Check if there's room in the hello buffer. */

  if (data_size < 1)
    {
      gnutls_assert ();
      return GNUTLS_E_SHORT_MEMORY_BUFFER;
    }

  /* default: require new application phase */

  *data = YES;

  if (session->security_parameters.entity == GNUTLS_CLIENT)
    {

      /* Client: value follows local setting */

      if (priv->flags & IA_ALLOW_SKIP)
        *data = NO;
    }
  else
    {

      /* Server: value follows local setting and client's setting, but only
       * if we are resuming.
       *
       * XXX Can server test for resumption at this stage?
       *
       * Ai! It seems that read_client_hello only calls parse_extensions if
       * we're NOT resuming! That would make us automatically violate the IA
       * draft; if we're resuming, we must first learn what the client wants
       * -- IA or no IA -- and then prepare our response. Right now we'll
       * always skip IA on resumption, because recv_ext isn't even called
       * to record the peer's support for IA at all. Simon? */

      if ((priv->flags & IA_ALLOW_SKIP) &&
          (priv->flags & IA_PEER_ALLOW_SKIP) &&
          session->internals.resumed == RESUME_TRUE)
        *data = NO;
    }

  return 1;
}

static void
ia_deinit_data (extension_priv_data_t priv)
{
  gnutls_free (priv.ptr);
}

static int
ia_pack (extension_priv_data_t epriv, gnutls_buffer_st * ps)
{
  ia_ext_st *priv = epriv.ptr;
  int ret;

  BUFFER_APPEND_NUM (ps, priv->flags);
  BUFFER_APPEND_PFX (ps, priv->inner_secret, GNUTLS_MASTER_SIZE);

  return 0;
}

static int
ia_unpack (gnutls_buffer_st * ps, extension_priv_data_t * _priv)
{
  ia_ext_st *priv;
  int size, ret;
  extension_priv_data_t epriv;

  priv = gnutls_calloc (1, sizeof (*priv));
  if (priv == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  BUFFER_POP_NUM (ps, priv->flags);
  BUFFER_POP_NUM (ps, size);
  if (size != GNUTLS_MASTER_SIZE)
    {
      gnutls_assert ();
      return GNUTLS_E_PARSING_ERROR;
    }
  BUFFER_POP (ps, priv->inner_secret, GNUTLS_MASTER_SIZE);

  epriv.ptr = priv;
  *_priv = epriv;

  return 0;

error:
  gnutls_free (priv);
  return ret;
}
