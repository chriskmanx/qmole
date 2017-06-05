/*
 * Copyright (C) 2005, 2006, 2008, 2009, 2010 Free Software Foundation,
 * Inc.
 *
 * Author: Simon Josefsson
 *
 * This file is part of GnuTLS-EXTRA.
 *
 * GnuTLS-extra is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS-extra is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "gnutls_int.h"
#include "gnutls_record.h"
#include "gnutls_errors.h"
#include "gnutls_num.h"
#include "gnutls_state.h"
#include <gnutls/extra.h>
#include <ext_inner_application.h>

#define CHECKSUM_SIZE 12

struct gnutls_ia_client_credentials_st
{
  gnutls_ia_avp_func avp_func;
  void *avp_ptr;
};

struct gnutls_ia_server_credentials_st
{
  gnutls_ia_avp_func avp_func;
  void *avp_ptr;
};

static const char server_finished_label[] = "server phase finished";
static const char client_finished_label[] = "client phase finished";
static const char inner_permutation_label[] = "inner secret permutation";
static const char challenge_label[] = "inner application challenge";

/*
 * The TLS/IA packet is the InnerApplication token, described as
 * follows in draft-funk-tls-inner-application-extension-01.txt:
 *
 * enum {
 *   application_payload(0), intermediate_phase_finished(1),
 *   final_phase_finished(2), (255)
 * } InnerApplicationType;
 *
 * struct {
 *   InnerApplicationType msg_type;
 *   uint24 length;
 *   select (InnerApplicationType) {
 *     case application_payload:           ApplicationPayload;
 *     case intermediate_phase_finished:   IntermediatePhaseFinished;
 *     case final_phase_finished:          FinalPhaseFinished;
 *   } body;
 * } InnerApplication;
 *
 */

/* Send TLS/IA data.  If data==NULL && sizeofdata==NULL, then the last
   send was interrupted for some reason, and then we try to send it
   again.  Returns the number of bytes sent, or an error code.  If
   this return E_AGAIN and E_INTERRUPTED, call this function again
   with data==NULL&&sizeofdata=0NULL until it returns successfully. */
static ssize_t
_gnutls_send_inner_application (gnutls_session_t session,
                                gnutls_ia_apptype_t msg_type,
                                const char *data, size_t sizeofdata)
{
  opaque *p = NULL;
  size_t plen = 0;
  ssize_t len;

  if (data != NULL)
    {
      plen = sizeofdata + 4;
      p = gnutls_malloc (plen);
      if (!p)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      *(unsigned char *) p = (unsigned char) (msg_type & 0xFF);
      _gnutls_write_uint24 (sizeofdata, p + 1);
      memcpy (p + 4, data, sizeofdata);
    }

  len =
    _gnutls_send_int (session, GNUTLS_INNER_APPLICATION, -1,
                      EPOCH_WRITE_CURRENT, p, plen, MBUFFER_FLUSH);

  if (p)
    gnutls_free (p);

  return len;
}

/* Receive TLS/IA data.  Store received TLS/IA message type in
   *MSG_TYPE, and the data in DATA of max SIZEOFDATA size.  Return the
   number of bytes read, or an error code. */
static ssize_t
_gnutls_recv_inner_application (gnutls_session_t session,
                                gnutls_ia_apptype_t * msg_type,
                                opaque * data, size_t sizeofdata)
{
  ssize_t len;
  uint32_t len24;
  opaque pkt[4];

  len = _gnutls_recv_int (session, GNUTLS_INNER_APPLICATION, -1, pkt, 4);
  if (len != 4)
    {
      gnutls_assert ();
      return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
    }

  *msg_type = pkt[0];
  len24 = _gnutls_read_uint24 (&pkt[1]);

  if (*msg_type != GNUTLS_IA_APPLICATION_PAYLOAD && len24 != CHECKSUM_SIZE)
    {
      gnutls_assert ();
      return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
    }

  if (sizeofdata < len24)
    {
      /* XXX push back pkt to IA buffer? */
      gnutls_assert ();
      return GNUTLS_E_SHORT_MEMORY_BUFFER;
    }

  if (len24 > 0)
    {
      uint32_t tmplen = len24;

      len24 = _gnutls_recv_int (session, GNUTLS_INNER_APPLICATION, -1,
                                data, tmplen);
      if (len24 != tmplen)
        {
          gnutls_assert ();
          /* XXX Correct? */
          return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
        }
    }

  return len24;
}

/* Apply the TLS PRF using the TLS/IA inner secret as keying material,
   where the seed is the client random concatenated with the server
   random concatenated EXTRA of EXTRA_SIZE length (which can be NULL/0
   respectively).  LABEL and LABEL_SIZE is used as the label.  The
   result is placed in pre-allocated OUT of OUTSIZE length. */
static int
_gnutls_ia_prf (gnutls_session_t session,
                size_t label_size,
                const char *label,
                size_t extra_size,
                const char *extra, size_t outsize, opaque * out)
{
  int ret;
  opaque *seed;
  size_t seedsize = 2 * GNUTLS_RANDOM_SIZE + extra_size;
  extension_priv_data_t epriv;
  ia_ext_st *priv;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_INNER_APPLICATION,
                                  &epriv);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }
  priv = epriv.ptr;

  seed = gnutls_malloc (seedsize);
  if (!seed)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  memcpy (seed, session->security_parameters.server_random,
          GNUTLS_RANDOM_SIZE);
  memcpy (seed + GNUTLS_RANDOM_SIZE,
          session->security_parameters.client_random, GNUTLS_RANDOM_SIZE);
  memcpy (seed + 2 * GNUTLS_RANDOM_SIZE, extra, extra_size);

  ret = _gnutls_PRF (session, priv->inner_secret,
                     GNUTLS_MASTER_SIZE,
                     label, label_size, seed, seedsize, outsize, out);

  gnutls_free (seed);

  return ret;
}

/**
 * gnutls_ia_permute_inner_secret:
 * @session: is a #gnutls_session_t structure.
 * @session_keys_size: Size of generated session keys (0 if none).
 * @session_keys: Generated session keys, used to permute inner secret
 *                (NULL if none).
 *
 * Permute the inner secret using the generated session keys.
 *
 * This can be called in the TLS/IA AVP callback to mix any generated
 * session keys with the TLS/IA inner secret.
 *
 * Return value: Return zero on success, or a negative error code.
 **/
int
gnutls_ia_permute_inner_secret (gnutls_session_t session,
                                size_t session_keys_size,
                                const char *session_keys)
{
  extension_priv_data_t epriv;
  ia_ext_st *priv;
  int ret;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_INNER_APPLICATION,
                                  &epriv);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }
  priv = epriv.ptr;

  return _gnutls_ia_prf (session,
                         sizeof (inner_permutation_label) - 1,
                         inner_permutation_label,
                         session_keys_size,
                         session_keys,
                         GNUTLS_RANDOM_SIZE, priv->inner_secret);
}

/**
 * gnutls_ia_generate_challenge:
 * @session: is a #gnutls_session_t structure.
 * @buffer_size: size of output buffer.
 * @buffer: pre-allocated buffer to contain @buffer_size bytes of output.
 *
 * Generate an application challenge that the client cannot control or
 * predict, based on the TLS/IA inner secret.
 *
 * Return value: Returns 0 on success, or an negative error code.
 **/
int
gnutls_ia_generate_challenge (gnutls_session_t session,
                              size_t buffer_size, char *buffer)
{
  return _gnutls_ia_prf (session,
                         sizeof (challenge_label) - 1,
                         challenge_label, 0, NULL, buffer_size, buffer);
}

/**
 * gnutls_ia_extract_inner_secret:
 * @session: is a #gnutls_session_t structure.
 * @buffer: pre-allocated buffer to hold 48 bytes of inner secret.
 *
 * Copy the 48 bytes large inner secret into the specified buffer
 *
 * This function is typically used after the TLS/IA handshake has
 * concluded.  The TLS/IA inner secret can be used as input to a PRF
 * to derive session keys.  Do not use the inner secret directly as a
 * session key, because for a resumed session that does not include an
 * application phase, the inner secret will be identical to the inner
 * secret in the original session.  It is important to include, for
 * example, the client and server randomness when deriving a sesssion
 * key from the inner secret.
 **/
void
gnutls_ia_extract_inner_secret (gnutls_session_t session, char *buffer)
{
  extension_priv_data_t epriv;
  ia_ext_st *priv;
  int ret;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_INNER_APPLICATION,
                                  &epriv);
  if (ret < 0)
    {
      gnutls_assert ();
      return;
    }
  priv = epriv.ptr;

  memcpy (buffer, priv->inner_secret, GNUTLS_MASTER_SIZE);
}

/**
 * gnutls_ia_endphase_send:
 * @session: is a #gnutls_session_t structure.
 * @final_p: Set iff this should signal the final phase.
 *
 * Send a TLS/IA end phase message.
 *
 * In the client, this should only be used to acknowledge an end phase
 * message sent by the server.
 *
 * In the server, this can be called instead of gnutls_ia_send() if
 * the server wishes to end an application phase.
 *
 * Return value: Return 0 on success, or an error code.
 **/
int
gnutls_ia_endphase_send (gnutls_session_t session, int final_p)
{
  opaque local_checksum[CHECKSUM_SIZE];
  int client = session->security_parameters.entity == GNUTLS_CLIENT;
  const char *label = client ? client_finished_label : server_finished_label;
  int size_of_label = client ? sizeof (client_finished_label) :
    sizeof (server_finished_label);
  ssize_t len;
  int ret;
  extension_priv_data_t epriv;
  ia_ext_st *priv;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_INNER_APPLICATION,
                                  &epriv);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }
  priv = epriv.ptr;

  ret = _gnutls_PRF (session, priv->inner_secret,
                     GNUTLS_MASTER_SIZE, label, size_of_label - 1,
                     /* XXX specification unclear on seed. */
                     "", 0, CHECKSUM_SIZE, local_checksum);
  if (ret < 0)
    return ret;

  len = _gnutls_send_inner_application
    (session,
     final_p ? GNUTLS_IA_FINAL_PHASE_FINISHED :
     GNUTLS_IA_INTERMEDIATE_PHASE_FINISHED, local_checksum, CHECKSUM_SIZE);

  /* XXX  Instead of calling this function over and over...?
   * while (len == GNUTLS_E_AGAIN || len == GNUTLS_E_INTERRUPTED)
   *  len = _gnutls_io_write_flush(session);
   */

  if (len < 0)
    {
      gnutls_assert ();
      return len;
    }

  return 0;
}

/**
 * gnutls_ia_verify_endphase:
 * @session: is a #gnutls_session_t structure.
 * @checksum: 12-byte checksum data, received from gnutls_ia_recv().
 *
 * Verify TLS/IA end phase checksum data.  If verification fails, the
 * %GNUTLS_A_INNER_APPLICATION_VERIFICATION alert is sent to the other
 * sie.
 *
 * This function is called when gnutls_ia_recv() return
 * %GNUTLS_E_WARNING_IA_IPHF_RECEIVED or
 * %GNUTLS_E_WARNING_IA_FPHF_RECEIVED.
 *
 * Return value: Return 0 on successful verification, or an error
 * code.  If the checksum verification of the end phase message fails,
 * %GNUTLS_E_IA_VERIFY_FAILED is returned.
 **/
int
gnutls_ia_verify_endphase (gnutls_session_t session, const char *checksum)
{
  char local_checksum[CHECKSUM_SIZE];
  int client = session->security_parameters.entity == GNUTLS_CLIENT;
  const char *label = client ? server_finished_label : client_finished_label;
  int size_of_label = client ? sizeof (server_finished_label) :
    sizeof (client_finished_label);
  int ret;
  extension_priv_data_t epriv;
  ia_ext_st *priv;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_INNER_APPLICATION,
                                  &epriv);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }
  priv = epriv.ptr;

  ret = _gnutls_PRF (session, priv->inner_secret,
                     GNUTLS_MASTER_SIZE,
                     label, size_of_label - 1,
                     "", 0, CHECKSUM_SIZE, local_checksum);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (memcmp (local_checksum, checksum, CHECKSUM_SIZE) != 0)
    {
      ret = gnutls_alert_send (session, GNUTLS_AL_FATAL,
                               GNUTLS_A_INNER_APPLICATION_VERIFICATION);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      return GNUTLS_E_IA_VERIFY_FAILED;
    }

  return 0;
}

/**
 * gnutls_ia_send:
 * @session: is a #gnutls_session_t structure.
 * @data: contains the data to send
 * @sizeofdata: is the length of the data
 *
 * Send TLS/IA application payload data.  This function has the
 * similar semantics with send().  The only difference is that it
 * accepts a GnuTLS session, and uses different error codes.
 *
 * The TLS/IA protocol is synchronous, so you cannot send more than
 * one packet at a time.  The client always send the first packet.
 *
 * To finish an application phase in the server, use
 * gnutls_ia_endphase_send().  The client cannot end an application
 * phase unilaterally; rather, a client is required to respond with an
 * endphase of its own if gnutls_ia_recv indicates that the server has
 * sent one.
 *
 * If the EINTR is returned by the internal push function (the default
 * is send()} then %GNUTLS_E_INTERRUPTED will be returned.  If
 * %GNUTLS_E_INTERRUPTED or %GNUTLS_E_AGAIN is returned, you must call
 * this function again, with the same parameters; alternatively you
 * could provide a %NULL pointer for data, and 0 for size.
 *
 * Returns: The number of bytes sent, or a negative error code.
 **/
ssize_t
gnutls_ia_send (gnutls_session_t session, const char *data, size_t sizeofdata)
{
  ssize_t len;

  len = _gnutls_send_inner_application (session,
                                        GNUTLS_IA_APPLICATION_PAYLOAD,
                                        data, sizeofdata);

  return len;
}

/**
 * gnutls_ia_recv:
 * @session: is a #gnutls_session_t structure.
 * @data: the buffer that the data will be read into, must hold >= 12 bytes.
 * @sizeofdata: the number of requested bytes, must be >= 12.
 *
 * Receive TLS/IA data.  This function has the similar semantics with
 * recv().  The only difference is that it accepts a GnuTLS session,
 * and uses different error codes.
 *
 * If the server attempt to finish an application phase, this function
 * will return %GNUTLS_E_WARNING_IA_IPHF_RECEIVED or
 * %GNUTLS_E_WARNING_IA_FPHF_RECEIVED.  The caller should then invoke
 * gnutls_ia_verify_endphase(), and if it runs the client side, also
 * send an endphase message of its own using gnutls_ia_endphase_send.
 *
 * If EINTR is returned by the internal push function (the default is
 * @code{recv()}) then GNUTLS_E_INTERRUPTED will be returned.  If
 * GNUTLS_E_INTERRUPTED or GNUTLS_E_AGAIN is returned, you must call
 * this function again, with the same parameters; alternatively you
 * could provide a NULL pointer for data, and 0 for size.
 *
 * Returns: The number of bytes received.  A negative error code is
 * returned in case of an error.  The
 * %GNUTLS_E_WARNING_IA_IPHF_RECEIVED and
 * %GNUTLS_E_WARNING_IA_FPHF_RECEIVED errors are returned when an
 * application phase finished message has been sent by the server.
 **/
ssize_t
gnutls_ia_recv (gnutls_session_t session, char *data, size_t sizeofdata)
{
  gnutls_ia_apptype_t msg_type = 0;
  ssize_t len;

  len = _gnutls_recv_inner_application (session, &msg_type, data, sizeofdata);

  if (msg_type == GNUTLS_IA_INTERMEDIATE_PHASE_FINISHED)
    return GNUTLS_E_WARNING_IA_IPHF_RECEIVED;
  else if (msg_type == GNUTLS_IA_FINAL_PHASE_FINISHED)
    return GNUTLS_E_WARNING_IA_FPHF_RECEIVED;

  return len;
}

/* XXX rewrite the following two functions as state machines, to
   handle EAGAIN/EINTERRUPTED?  just add more problems to callers,
   though.  */

static int
_gnutls_ia_client_handshake (gnutls_session_t session)
{
  char *buf = NULL;
  size_t buflen = 0;
  char tmp[1024];               /* XXX */
  ssize_t len;
  int ret;
  const struct gnutls_ia_client_credentials_st *cred =
    _gnutls_get_cred (session->key, GNUTLS_CRD_IA, NULL);

  if (cred == NULL)
    return GNUTLS_E_INTERNAL_ERROR;

  while (1)
    {
      char *avp;
      size_t avplen;

      ret = cred->avp_func (session, cred->avp_ptr,
                            buf, buflen, &avp, &avplen);
      if (ret)
        {
          int tmpret;
          tmpret = gnutls_alert_send (session, GNUTLS_AL_FATAL,
                                      GNUTLS_A_INNER_APPLICATION_FAILURE);
          if (tmpret < 0)
            gnutls_assert ();
          return ret;
        }

      len = gnutls_ia_send (session, avp, avplen);
      gnutls_free (avp);
      if (len < 0)
        return len;

      len = gnutls_ia_recv (session, tmp, sizeof (tmp));
      if (len == GNUTLS_E_WARNING_IA_IPHF_RECEIVED ||
          len == GNUTLS_E_WARNING_IA_FPHF_RECEIVED)
        {
          ret = gnutls_ia_verify_endphase (session, tmp);
          if (ret < 0)
            return ret;

          ret = gnutls_ia_endphase_send
            (session, len == GNUTLS_E_WARNING_IA_FPHF_RECEIVED);
          if (ret < 0)
            return ret;
        }

      if (len == GNUTLS_E_WARNING_IA_IPHF_RECEIVED)
        {
          buf = NULL;
          buflen = 0;
          continue;
        }
      else if (len == GNUTLS_E_WARNING_IA_FPHF_RECEIVED)
        break;

      if (len < 0)
        return len;

      buflen = len;
      buf = tmp;
    }

  return 0;
}

static int
_gnutls_ia_server_handshake (gnutls_session_t session)
{
  gnutls_ia_apptype_t msg_type;
  ssize_t len;
  char buf[1024];
  int ret;
  const struct gnutls_ia_server_credentials_st *cred =
    _gnutls_get_cred (session->key, GNUTLS_CRD_IA, NULL);

  if (cred == NULL)
    return GNUTLS_E_INTERNAL_ERROR;

  do
    {
      char *avp;
      size_t avplen;

      len = gnutls_ia_recv (session, buf, sizeof (buf));
      if (len == GNUTLS_E_WARNING_IA_IPHF_RECEIVED ||
          len == GNUTLS_E_WARNING_IA_FPHF_RECEIVED)
        {
          ret = gnutls_ia_verify_endphase (session, buf);
          if (ret < 0)
            return ret;
        }

      if (len == GNUTLS_E_WARNING_IA_IPHF_RECEIVED)
        continue;
      else if (len == GNUTLS_E_WARNING_IA_FPHF_RECEIVED)
        break;

      if (len < 0)
        return len;

      avp = NULL;
      avplen = 0;

      ret = cred->avp_func (session, cred->avp_ptr, buf, len, &avp, &avplen);
      if (ret < 0)
        {
          int tmpret;
          tmpret = gnutls_alert_send (session, GNUTLS_AL_FATAL,
                                      GNUTLS_A_INNER_APPLICATION_FAILURE);
          if (tmpret < 0)
            gnutls_assert ();
          return ret;
        }

      msg_type = ret;

      if (msg_type != GNUTLS_IA_APPLICATION_PAYLOAD)
        {
          ret = gnutls_ia_endphase_send (session, msg_type ==
                                         GNUTLS_IA_FINAL_PHASE_FINISHED);
          if (ret < 0)
            return ret;
        }
      else
        {
          len = gnutls_ia_send (session, avp, avplen);
          gnutls_free (avp);
          if (len < 0)
            return len;
        }
    }
  while (1);

  return 0;
}

/**
 * gnutls_ia_handshake_p:
 * @session: is a #gnutls_session_t structure.
 *
 * Predicate to be used after gnutls_handshake() to decide whether to
 * invoke gnutls_ia_handshake().  Usable by both clients and servers.
 *
 * Return value: non-zero if TLS/IA handshake is expected, zero
 *   otherwise.
 **/
int
gnutls_ia_handshake_p (gnutls_session_t session)
{
  extension_priv_data_t epriv;
  ia_ext_st *priv;
  int ret;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_SERVER_NAME,
                                  &epriv);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }
  priv = epriv.ptr;

  /* Either local side or peer doesn't do TLS/IA: don't do IA */

  if (!(priv->flags & IA_ENABLE) || !(priv->flags & IA_PEER_ENABLE))
    return 0;

  /* Not resuming or we don't allow skipping on resumption locally: do IA */

  if (!(priv->flags & IA_ALLOW_SKIP) || !gnutls_session_is_resumed (session))
    return 1;

  /* If we're resuming and we and the peer both allow skipping on resumption: 
   * don't do IA */

  return !(priv->flags & IA_PEER_ALLOW_SKIP);
}


/**
 * gnutls_ia_handshake:
 * @session: is a #gnutls_session_t structure.
 *
 * Perform a TLS/IA handshake.  This should be called after
 * gnutls_handshake() iff gnutls_ia_handshake_p().
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (zero) is returned,
 *   otherwise an error code is returned.
 **/
int
gnutls_ia_handshake (gnutls_session_t session)
{
  int ret;

  if (session->security_parameters.entity == GNUTLS_CLIENT)
    ret = _gnutls_ia_client_handshake (session);
  else
    ret = _gnutls_ia_server_handshake (session);

  return ret;
}

/**
 * gnutls_ia_allocate_client_credentials:
 * @sc: is a pointer to a #gnutls_ia_server_credentials_t structure.
 *
 * This structure is complex enough to manipulate directly thus this
 * helper function is provided in order to allocate it.
 *
 * Adding this credential to a session will enable TLS/IA, and will
 * require an Application Phase after the TLS handshake (if the server
 * support TLS/IA).  Use gnutls_ia_enable() to toggle the TLS/IA mode.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, otherwise
 *   an error code is returned.
 **/
int
gnutls_ia_allocate_client_credentials (gnutls_ia_client_credentials_t * sc)
{
  *sc = gnutls_calloc (1, sizeof (**sc));

  if (*sc == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  return 0;
}

/**
 * gnutls_ia_free_client_credentials:
 * @sc: is a #gnutls_ia_client_credentials_t structure.
 *
 * This structure is complex enough to manipulate directly thus this
 * helper function is provided in order to free (deallocate) it.
 *
 **/
void
gnutls_ia_free_client_credentials (gnutls_ia_client_credentials_t sc)
{
  gnutls_free (sc);
}

/**
 * gnutls_ia_set_client_avp_function:
 * @cred: is a #gnutls_ia_client_credentials_t structure.
 * @avp_func: is the callback function
 *
 * Set the TLS/IA AVP callback handler used for the session.
 *
 * The AVP callback is called to process AVPs received from the
 * server, and to get a new AVP to send to the server.
 *
 * The callback's function form is:
 * int (*avp_func) (gnutls_session_t session, void *ptr,
 *                  const char *last, size_t lastlen,
 *                  char **next, size_t *nextlen);
 *
 * The @session parameter is the #gnutls_session_t structure
 * corresponding to the current session.  The @ptr parameter is the
 * application hook pointer, set through
 * gnutls_ia_set_client_avp_ptr().  The AVP received from the server
 * is present in @last of @lastlen size, which will be %NULL on the
 * first invocation.  The newly allocated output AVP to send to the
 * server should be placed in *@next of *@nextlen size.
 *
 * The callback may invoke gnutls_ia_permute_inner_secret() to mix any
 * generated session keys with the TLS/IA inner secret.
 *
 * Return 0 (%GNUTLS_IA_APPLICATION_PAYLOAD) on success, or a negative
 * error code to abort the TLS/IA handshake.
 *
 * Note that the callback must use allocate the @next parameter using
 * gnutls_malloc(), because it is released via gnutls_free() by the
 * TLS/IA handshake function.
 *
 **/
void
gnutls_ia_set_client_avp_function (gnutls_ia_client_credentials_t cred,
                                   gnutls_ia_avp_func avp_func)
{
  cred->avp_func = avp_func;
}

/**
 * gnutls_ia_set_client_avp_ptr:
 * @cred: is a #gnutls_ia_client_credentials_t structure.
 * @ptr: is the pointer
 *
 * Sets the pointer that will be provided to the TLS/IA callback
 * function as the first argument.
 **/
void
gnutls_ia_set_client_avp_ptr (gnutls_ia_client_credentials_t cred, void *ptr)
{
  cred->avp_ptr = ptr;
}

/**
 * gnutls_ia_get_client_avp_ptr:
 * @cred: is a #gnutls_ia_client_credentials_t structure.
 *
 * Returns the pointer that will be provided to the TLS/IA callback
 * function as the first argument.
 *
 * Returns: The client callback data pointer.
 **/
void *
gnutls_ia_get_client_avp_ptr (gnutls_ia_client_credentials_t cred)
{
  return cred->avp_ptr;
}

/**
 * gnutls_ia_allocate_server_credentials:
 * @sc: is a pointer to a #gnutls_ia_server_credentials_t structure.
 *
 * This structure is complex enough to manipulate directly thus this
 * helper function is provided in order to allocate it.
 *
 * Adding this credential to a session will enable TLS/IA, and will
 * require an Application Phase after the TLS handshake (if the client
 * support TLS/IA).  Use gnutls_ia_enable() to toggle the TLS/IA mode.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, otherwise
 *   an error code is returned.
 **/
int
gnutls_ia_allocate_server_credentials (gnutls_ia_server_credentials_t * sc)
{
  *sc = gnutls_calloc (1, sizeof (**sc));

  if (*sc == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  return 0;
}

/**
 * gnutls_ia_free_server_credentials:
 * @sc: is a #gnutls_ia_server_credentials_t structure.
 *
 * This structure is complex enough to manipulate directly thus this
 * helper function is provided in order to free (deallocate) it.
 *
 **/
void
gnutls_ia_free_server_credentials (gnutls_ia_server_credentials_t sc)
{
  gnutls_free (sc);
}

/**
 * gnutls_ia_set_server_credentials_function:
 * @cred: is a #gnutls_ia_server_credentials_t structure.
 * @func: is the callback function
 *
 * Set the TLS/IA AVP callback handler used for the session.
 *
 * The callback's function form is:
 * int (*avp_func) (gnutls_session_t session, void *ptr,
 *                  const char *last, size_t lastlen,
 *                  char **next, size_t *nextlen);
 *
 * The @session parameter is the #gnutls_session_t structure
 * corresponding to the current session.  The @ptr parameter is the
 * application hook pointer, set through
 * gnutls_ia_set_server_avp_ptr().  The AVP received from the client
 * is present in @last of @lastlen size.  The newly allocated output
 * AVP to send to the client should be placed in *@next of *@nextlen
 * size.
 *
 * The AVP callback is called to process incoming AVPs from the
 * client, and to get a new AVP to send to the client.  It can also be
 * used to instruct the TLS/IA handshake to do go into the
 * Intermediate or Final phases.  It return a negative error code, or
 * a #gnutls_ia_apptype_t message type.
 *
 * The callback may invoke gnutls_ia_permute_inner_secret() to mix any
 * generated session keys with the TLS/IA inner secret.
 *
 * Specifically, return %GNUTLS_IA_APPLICATION_PAYLOAD (0) to send
 * another AVP to the client, return
 * %GNUTLS_IA_INTERMEDIATE_PHASE_FINISHED (1) to indicate that an
 * IntermediatePhaseFinished message should be sent, and return
 * %GNUTLS_IA_FINAL_PHASE_FINISHED (2) to indicate that an
 * FinalPhaseFinished message should be sent.  In the last two cases,
 * the contents of the @next and @nextlen parameter is not used.
 *
 * Note that the callback must use allocate the @next parameter using
 * gnutls_malloc(), because it is released via gnutls_free() by the
 * TLS/IA handshake function.
 **/
void
gnutls_ia_set_server_avp_function (gnutls_ia_server_credentials_t cred,
                                   gnutls_ia_avp_func avp_func)
{
  cred->avp_func = avp_func;
}

/**
 * gnutls_ia_set_server_avp_ptr:
 * @cred: is a #gnutls_ia_client_credentials_t structure.
 * @ptr: is the pointer
 *
 * Sets the pointer that will be provided to the TLS/IA callback
 * function as the first argument.
 **/
void
gnutls_ia_set_server_avp_ptr (gnutls_ia_server_credentials_t cred, void *ptr)
{
  cred->avp_ptr = ptr;
}

/**
 * gnutls_ia_get_server_avp_ptr:
 * @cred: is a #gnutls_ia_client_credentials_t structure.
 *
 * Returns the pointer that will be provided to the TLS/IA callback
 * function as the first argument.
 *
 * Returns: The server callback data pointer.
 **/
void *
gnutls_ia_get_server_avp_ptr (gnutls_ia_server_credentials_t cred)
{
  return cred->avp_ptr;
}

/**
 * gnutls_ia_enable:
 * @session: is a #gnutls_session_t structure.
 * @allow_skip_on_resume: non-zero if local party allows one to skip the
 *			  TLS/IA application phases for a resumed session.
 *
 * Specify whether we must advertise support for the TLS/IA extension
 * during the handshake.
 *
 * At the client side, we always advertise TLS/IA if gnutls_ia_enable
 * was called before the handshake; at the server side, we also
 * require that the client has advertised that it wants to run TLS/IA
 * before including the advertisement, as required by the protocol.
 *
 * Similarly, at the client side we always advertise that we allow
 * TLS/IA to be skipped for resumed sessions if @allow_skip_on_resume
 * is non-zero; at the server side, we also require that the session
 * is indeed resumable and that the client has also advertised that it
 * allows TLS/IA to be skipped for resumed sessions.
 *
 * After the TLS handshake, call gnutls_ia_handshake_p() to find out
 * whether both parties agreed to do a TLS/IA handshake, before
 * calling gnutls_ia_handshake() or one of the lower level gnutls_ia_*
 * functions.
 **/
void
gnutls_ia_enable (gnutls_session_t session, int allow_skip_on_resume)
{
  extension_priv_data_t epriv;
  ia_ext_st *priv;

  priv = gnutls_calloc (1, sizeof (*priv));
  if (priv == NULL)
    {
      gnutls_assert ();
      return;
    }

  epriv.ptr = priv;

  priv->flags |= IA_ENABLE;
  if (allow_skip_on_resume)
    priv->flags |= IA_ALLOW_SKIP;

  _gnutls_ext_set_session_data (session, GNUTLS_EXTENSION_INNER_APPLICATION,
                                epriv);

}
