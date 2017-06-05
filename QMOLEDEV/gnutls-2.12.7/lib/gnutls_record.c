/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
 * 2009, 2010 Free Software Foundation, Inc.
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

/* Functions that are record layer specific, are included in this file.
 */

#include "gnutls_int.h"
#include "gnutls_errors.h"
#include "debug.h"
#include "gnutls_compress.h"
#include "gnutls_cipher.h"
#include "gnutls_buffers.h"
#include "gnutls_mbuffers.h"
#include "gnutls_handshake.h"
#include "gnutls_hash_int.h"
#include "gnutls_cipher_int.h"
#include "gnutls_algorithms.h"
#include "gnutls_db.h"
#include "gnutls_auth.h"
#include "gnutls_num.h"
#include "gnutls_record.h"
#include "gnutls_datum.h"
#include "gnutls_constate.h"
#include "ext_max_record.h"
#include <gnutls_state.h>
#include <gnutls_dh.h>

void
_gnutls_transport_set_lowat (gnutls_session_t session, int num);

/**
 * gnutls_protocol_get_version:
 * @session: is a #gnutls_session_t structure.
 *
 * Get TLS version, a #gnutls_protocol_t value.
 *
 * Returns: the version of the currently used protocol.
 **/
gnutls_protocol_t
gnutls_protocol_get_version (gnutls_session_t session)
{
  return session->security_parameters.version;
}

void
_gnutls_set_current_version (gnutls_session_t session,
                             gnutls_protocol_t version)
{
  session->security_parameters.version = version;
}

/* Added to avoid issue in C++ interface not being able to
 * call deprecated functions.
 */
void
_gnutls_transport_set_lowat (gnutls_session_t session, int num)
{
  session->internals.lowat = num;
}

/**
 * gnutls_transport_set_lowat:
 * @session: is a #gnutls_session_t structure.
 * @num: is the low water value.
 *
 * Used to set the lowat value in order for select to check if there
 * are pending data to socket buffer. Used only if you have changed
 * the default low water value (default is 1).  Normally you will not
 * need that function.  This function is only useful if using
 * berkeley style sockets.  Otherwise it must be called and set lowat
 * to zero.
 **/
void
gnutls_transport_set_lowat (gnutls_session_t session, int num)
{
  _gnutls_transport_set_lowat(session, num);
}

/**
 * gnutls_record_disable_padding:
 * @session: is a #gnutls_session_t structure.
 *
 * Used to disabled padding in TLS 1.0 and above.  Normally you do not
 * need to use this function, but there are buggy clients that
 * complain if a server pads the encrypted data.  This of course will
 * disable protection against statistical attacks on the data.
 *
 * Normally only servers that require maximum compatibility with everything
 * out there, need to call this function.
 **/
void
gnutls_record_disable_padding (gnutls_session_t session)
{
  session->internals.priorities.no_padding = 1;
}

/**
 * gnutls_transport_set_ptr:
 * @session: is a #gnutls_session_t structure.
 * @ptr: is the value.
 *
 * Used to set the first argument of the transport function (like PUSH
 * and PULL).  In berkeley style sockets this function will set the
 * connection handle.
 **/
void
gnutls_transport_set_ptr (gnutls_session_t session,
                          gnutls_transport_ptr_t ptr)
{
  session->internals.transport_recv_ptr = ptr;
  session->internals.transport_send_ptr = ptr;
}

/**
 * gnutls_transport_set_ptr2:
 * @session: is a #gnutls_session_t structure.
 * @recv_ptr: is the value for the pull function
 * @send_ptr: is the value for the push function
 *
 * Used to set the first argument of the transport function (like PUSH
 * and PULL). In berkeley style sockets this function will set the
 * connection handle.  With this function you can use two different
 * pointers for receiving and sending.
 **/
void
gnutls_transport_set_ptr2 (gnutls_session_t session,
                           gnutls_transport_ptr_t recv_ptr,
                           gnutls_transport_ptr_t send_ptr)
{
  session->internals.transport_send_ptr = send_ptr;
  session->internals.transport_recv_ptr = recv_ptr;
}

/**
 * gnutls_transport_get_ptr:
 * @session: is a #gnutls_session_t structure.
 *
 * Used to get the first argument of the transport function (like
 * PUSH and PULL).  This must have been set using
 * gnutls_transport_set_ptr().
 *
 * Returns: first argument of the transport function.
 **/
gnutls_transport_ptr_t
gnutls_transport_get_ptr (gnutls_session_t session)
{
  return session->internals.transport_recv_ptr;
}

/**
 * gnutls_transport_get_ptr2:
 * @session: is a #gnutls_session_t structure.
 * @recv_ptr: will hold the value for the pull function
 * @send_ptr: will hold the value for the push function
 *
 * Used to get the arguments of the transport functions (like PUSH
 * and PULL).  These should have been set using
 * gnutls_transport_set_ptr2().
 **/
void
gnutls_transport_get_ptr2 (gnutls_session_t session,
                           gnutls_transport_ptr_t * recv_ptr,
                           gnutls_transport_ptr_t * send_ptr)
{

  *recv_ptr = session->internals.transport_recv_ptr;
  *send_ptr = session->internals.transport_send_ptr;
}

/**
 * gnutls_bye:
 * @session: is a #gnutls_session_t structure.
 * @how: is an integer
 *
 * Terminates the current TLS/SSL connection. The connection should
 * have been initiated using gnutls_handshake().  @how should be one
 * of %GNUTLS_SHUT_RDWR, %GNUTLS_SHUT_WR.
 *
 * In case of %GNUTLS_SHUT_RDWR then the TLS connection gets
 * terminated and further receives and sends will be disallowed.  If
 * the return value is zero you may continue using the connection.
 * %GNUTLS_SHUT_RDWR actually sends an alert containing a close
 * request and waits for the peer to reply with the same message.
 *
 * In case of %GNUTLS_SHUT_WR then the TLS connection gets terminated
 * and further sends will be disallowed. In order to reuse the
 * connection you should wait for an EOF from the peer.
 * %GNUTLS_SHUT_WR sends an alert containing a close request.
 *
 * Note that not all implementations will properly terminate a TLS
 * connection.  Some of them, usually for performance reasons, will
 * terminate only the underlying transport layer, thus causing a
 * transmission error to the peer.  This error cannot be
 * distinguished from a malicious party prematurely terminating the
 * session, thus this behavior is not recommended.
 *
 * This function may also return %GNUTLS_E_AGAIN or
 * %GNUTLS_E_INTERRUPTED; cf.  gnutls_record_get_direction().
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code, see
 *   function documentation for entire semantics.
 **/
int
gnutls_bye (gnutls_session_t session, gnutls_close_request_t how)
{
  int ret = 0;

  switch (STATE)
    {
    case STATE0:
    case STATE60:
      ret = _gnutls_io_write_flush (session);
      STATE = STATE60;
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

    case STATE61:
      ret =
        gnutls_alert_send (session, GNUTLS_AL_WARNING, GNUTLS_A_CLOSE_NOTIFY);
      STATE = STATE61;
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

    case STATE62:
      STATE = STATE62;
      if (how == GNUTLS_SHUT_RDWR)
        {
          do
            {
              _gnutls_io_clear_peeked_data (session);
              ret = _gnutls_recv_int (session, GNUTLS_ALERT, -1, NULL, 0);
            }
          while (ret == GNUTLS_E_GOT_APPLICATION_DATA);

          if (ret >= 0)
            session->internals.may_not_read = 1;

          if (ret < 0)
            {
              gnutls_assert ();
              return ret;
            }
        }
      STATE = STATE62;

      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  STATE = STATE0;

  session->internals.may_not_write = 1;
  return 0;
}

inline static void
session_invalidate (gnutls_session_t session)
{
  session->internals.invalid_connection = 1;
}


inline static void
session_unresumable (gnutls_session_t session)
{
  session->internals.resumable = RESUME_FALSE;
}

/* returns 0 if session is valid
 */
inline static int
session_is_valid (gnutls_session_t session)
{
  if (session->internals.invalid_connection != 0)
    return GNUTLS_E_INVALID_SESSION;

  return 0;
}

/* Copies the record version into the headers. The 
 * version must have 2 bytes at least.
 */
inline static void
copy_record_version (gnutls_session_t session,
                     gnutls_handshake_description_t htype, opaque version[2])
{
  gnutls_protocol_t lver;

  if (session->internals.initial_negotiation_completed || htype != GNUTLS_HANDSHAKE_CLIENT_HELLO
      || session->internals.default_record_version[0] == 0)
    {
      lver = gnutls_protocol_get_version (session);

      version[0] = _gnutls_version_get_major (lver);
      version[1] = _gnutls_version_get_minor (lver);
    }
  else
    {
      version[0] = session->internals.default_record_version[0];
      version[1] = session->internals.default_record_version[1];
    }
}

/* This function behaves exactly like write(). The only difference is
 * that it accepts, the gnutls_session_t and the content_type_t of data to
 * send (if called by the user the Content is specific)
 * It is intended to transfer data, under the current session.    
 *
 * Oct 30 2001: Removed capability to send data more than MAX_RECORD_SIZE.
 * This makes the function much easier to read, and more error resistant
 * (there were cases were the old function could mess everything up).
 * --nmav
 *
 * This function may accept a NULL pointer for data, and 0 for size, if
 * and only if the previous send was interrupted for some reason.
 *
 */
ssize_t
_gnutls_send_int (gnutls_session_t session, content_type_t type,
                  gnutls_handshake_description_t htype,
                  unsigned int epoch_rel, const void *_data,
                  size_t sizeofdata, unsigned int mflags)
{
  mbuffer_st *bufel;
  size_t cipher_size;
  int retval, ret;
  int data2send_size;
  uint8_t headers[5];
  const uint8_t *data = _data;
  record_parameters_st *record_params;
  record_state_st *record_state;

  ret = _gnutls_epoch_get (session, epoch_rel, &record_params);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* Safeguard against processing data with an incomplete cipher state. */
  if (!record_params->initialized)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  record_state = &record_params->write;

  /* Do not allow null pointer if the send buffer is empty.
   * If the previous send was interrupted then a null pointer is
   * ok, and means to resume.
   */
  if (session->internals.record_send_buffer.byte_length == 0 &&
      (sizeofdata == 0 && _data == NULL))
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (type != GNUTLS_ALERT)     /* alert messages are sent anyway */
    if (session_is_valid (session) || session->internals.may_not_write != 0)
      {
        gnutls_assert ();
        return GNUTLS_E_INVALID_SESSION;
      }

  headers[0] = type;

  /* Use the default record version, if it is
   * set.
   */
  copy_record_version (session, htype, &headers[1]);


  _gnutls_record_log
    ("REC[%p]: Sending Packet[%d] %s(%d) with length: %d\n", session,
     (int) _gnutls_uint64touint32 (&record_state->sequence_number),
     _gnutls_packet2str (type), type, (int) sizeofdata);

  if (sizeofdata > MAX_RECORD_SEND_SIZE)
    data2send_size = MAX_RECORD_SEND_SIZE;
  else
    data2send_size = sizeofdata;

  /* Only encrypt if we don't have data to send 
   * from the previous run. - probably interrupted.
   */
  if (mflags != 0 && session->internals.record_send_buffer.byte_length > 0)
    {
      ret = _gnutls_io_write_flush (session);
      if (ret > 0)
        cipher_size = ret;
      else
        cipher_size = 0;

      retval = session->internals.record_send_buffer_user_size;
    }
  else
    {

      /* now proceed to packet encryption
       */
      cipher_size = data2send_size + MAX_RECORD_OVERHEAD;
      bufel = _mbuffer_alloc (cipher_size, cipher_size);
      if (bufel == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      cipher_size =
        _gnutls_encrypt (session, headers, RECORD_HEADER_SIZE, data,
                         data2send_size, _mbuffer_get_udata_ptr (bufel),
                         cipher_size, type,
                         (session->internals.priorities.no_padding ==
                          0) ? 1 : 0, record_params);
      if (cipher_size <= 0)
        {
          gnutls_assert ();
          if (cipher_size == 0)
            cipher_size = GNUTLS_E_ENCRYPTION_FAILED;
          gnutls_free (bufel);
          return cipher_size;   /* error */
        }

      retval = data2send_size;
      session->internals.record_send_buffer_user_size = data2send_size;

      /* increase sequence number
       */
      if (_gnutls_uint64pp (&record_state->sequence_number) != 0)
        {
          session_invalidate (session);
          gnutls_assert ();
          gnutls_free (bufel);
          return GNUTLS_E_RECORD_LIMIT_REACHED;
        }

      _mbuffer_set_udata_size (bufel, cipher_size);
      ret = _gnutls_io_write_buffered (session, bufel, mflags);
    }

  if (ret != cipher_size)
    {
      if (ret < 0 && gnutls_error_is_fatal (ret) == 0)
        {
          /* If we have sent any data then just return
           * the error value. Do not invalidate the session.
           */
          gnutls_assert ();
          return ret;
        }

      if (ret > 0)
        {
          gnutls_assert ();
          ret = GNUTLS_E_INTERNAL_ERROR;
        }
      session_unresumable (session);
      session->internals.may_not_write = 1;
      gnutls_assert ();
      return ret;
    }

  session->internals.record_send_buffer_user_size = 0;

  _gnutls_record_log ("REC[%p]: Sent Packet[%d] %s(%d) with length: %d\n",
                      session,
                      (int)
                      _gnutls_uint64touint32
                      (&record_state->sequence_number),
                      _gnutls_packet2str (type), type, (int) cipher_size);

  return retval;
}

/* This function is to be called if the handshake was successfully 
 * completed. This sends a Change Cipher Spec packet to the peer.
 */
ssize_t
_gnutls_send_change_cipher_spec (gnutls_session_t session, int again)
{
  static const opaque data[1] = { GNUTLS_TYPE_CHANGE_CIPHER_SPEC };

  _gnutls_handshake_log ("REC[%p]: Sent ChangeCipherSpec\n", session);

  if (again == 0)
    return _gnutls_send_int (session, GNUTLS_CHANGE_CIPHER_SPEC, -1,
                             EPOCH_WRITE_CURRENT, data, 1, MBUFFER_FLUSH);
  else
    {
      return _gnutls_io_write_flush (session);
    }
}

inline static int
check_recv_type (content_type_t recv_type)
{
  switch (recv_type)
    {
    case GNUTLS_CHANGE_CIPHER_SPEC:
    case GNUTLS_ALERT:
    case GNUTLS_HANDSHAKE:
    case GNUTLS_APPLICATION_DATA:
    case GNUTLS_INNER_APPLICATION:
      return 0;
    default:
      gnutls_assert ();
      return GNUTLS_E_UNEXPECTED_PACKET;
    }

}


/* Checks if there are pending data in the record buffers. If there are
 * then it copies the data.
 */
static int
check_buffers (gnutls_session_t session, content_type_t type,
               opaque * data, int sizeofdata)
{
  if ((type == GNUTLS_APPLICATION_DATA ||
       type == GNUTLS_HANDSHAKE ||
       type == GNUTLS_INNER_APPLICATION)
      && _gnutls_record_buffer_get_size (type, session) > 0)
    {
      int ret, ret2;
      ret = _gnutls_record_buffer_get (type, session, data, sizeofdata);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      /* if the buffer just got empty */
      if (_gnutls_record_buffer_get_size (type, session) == 0)
        {
          if ((ret2 = _gnutls_io_clear_peeked_data (session)) < 0)
            {
              gnutls_assert ();
              return ret2;
            }
        }

      return ret;
    }

  return 0;
}


/* Checks the record headers and returns the length, version and
 * content type.
 */
static int
record_check_headers (gnutls_session_t session,
                      uint8_t headers[RECORD_HEADER_SIZE],
                      content_type_t type,
                      gnutls_handshake_description_t htype,
                      /*output */ content_type_t * recv_type,
                      opaque version[2], uint16_t * length,
                      uint16_t * header_size)
{

  /* Read the first two bytes to determine if this is a 
   * version 2 message 
   */

  if (htype == GNUTLS_HANDSHAKE_CLIENT_HELLO && type == GNUTLS_HANDSHAKE
      && headers[0] > 127)
    {

      /* if msb set and expecting handshake message
       * it should be SSL 2 hello 
       */
      version[0] = 3;           /* assume SSL 3.0 */
      version[1] = 0;

      *length = (((headers[0] & 0x7f) << 8)) | headers[1];

      /* SSL 2.0 headers */
      *header_size = 2;
      *recv_type = GNUTLS_HANDSHAKE;    /* we accept only v2 client hello
                                         */

      /* in order to assist the handshake protocol.
       * V2 compatibility is a mess.
       */
      session->internals.v2_hello = *length;

      _gnutls_record_log ("REC[%p]: V2 packet received. Length: %d\n",
                          session, *length);

    }
  else
    {
      /* version 3.x 
       */
      *recv_type = headers[0];
      version[0] = headers[1];
      version[1] = headers[2];

      /* No DECR_LEN, since headers has enough size. 
       */
      *length = _gnutls_read_uint16 (&headers[3]);
    }

  return 0;
}

/* Here we check if the advertized version is the one we
 * negotiated in the handshake.
 */
inline static int
record_check_version (gnutls_session_t session,
                      gnutls_handshake_description_t htype, opaque version[2])
{
  if (htype == GNUTLS_HANDSHAKE_CLIENT_HELLO)
    {
      /* Reject hello packets with major version higher than 3.
       */
      if (version[0] > 3)
        {
          gnutls_assert ();
          _gnutls_record_log
            ("REC[%p]: INVALID VERSION PACKET: (%d) %d.%d\n", session,
             htype, version[0], version[1]);
          return GNUTLS_E_UNSUPPORTED_VERSION_PACKET;
        }
    }
  else if (htype != GNUTLS_HANDSHAKE_SERVER_HELLO &&
           gnutls_protocol_get_version (session) !=
           _gnutls_version_get (version[0], version[1]))
    {
      /* Reject record packets that have a different version than the
       * one negotiated. Note that this version is not protected by any
       * mac. I don't really think that this check serves any purpose.
       */
      gnutls_assert ();
      _gnutls_record_log ("REC[%p]: INVALID VERSION PACKET: (%d) %d.%d\n",
                          session, htype, version[0], version[1]);

      return GNUTLS_E_UNSUPPORTED_VERSION_PACKET;
    }

  return 0;
}

/* This function will check if the received record type is
 * the one we actually expect.
 */
static int
record_check_type (gnutls_session_t session,
                   content_type_t recv_type, content_type_t type,
                   gnutls_handshake_description_t htype, opaque * data,
                   int data_size)
{

  int ret;

  if ((recv_type == type)
      && (type == GNUTLS_APPLICATION_DATA ||
          type == GNUTLS_HANDSHAKE || type == GNUTLS_INNER_APPLICATION))
    {
      _gnutls_record_buffer_put (type, session, (void *) data, data_size);
    }
  else
    {
      switch (recv_type)
        {
        case GNUTLS_ALERT:

          _gnutls_record_log
            ("REC[%p]: Alert[%d|%d] - %s - was received\n", session,
             data[0], data[1], gnutls_alert_get_name ((int) data[1]));

          session->internals.last_alert = data[1];

          /* if close notify is received and
           * the alert is not fatal
           */
          if (data[1] == GNUTLS_A_CLOSE_NOTIFY && data[0] != GNUTLS_AL_FATAL)
            {
              /* If we have been expecting for an alert do 
               */
              session->internals.read_eof = 1;
              return GNUTLS_E_INT_RET_0;        /* EOF */
            }
          else
            {

              /* if the alert is FATAL or WARNING
               * return the apropriate message
               */

              gnutls_assert ();
              ret = GNUTLS_E_WARNING_ALERT_RECEIVED;
              if (data[0] == GNUTLS_AL_FATAL)
                {
                  session_unresumable (session);
                  session_invalidate (session);
                  ret = GNUTLS_E_FATAL_ALERT_RECEIVED;
                }

              return ret;
            }
          break;

        case GNUTLS_CHANGE_CIPHER_SPEC:
          /* this packet is now handled in the recv_int()
           * function
           */
          gnutls_assert ();

          return GNUTLS_E_UNEXPECTED_PACKET;

        case GNUTLS_APPLICATION_DATA:
          if (session->internals.initial_negotiation_completed == 0)
            {
              return gnutls_assert_val(GNUTLS_E_UNEXPECTED_PACKET);
            }

          /* even if data is unexpected put it into the buffer */
          if ((ret =
               _gnutls_record_buffer_put (recv_type, session,
                                          (void *) data, data_size)) < 0)
            {
              gnutls_assert ();
              return ret;
            }

          /* the got_application data is only returned
           * if expecting client hello (for rehandshake
           * reasons). Otherwise it is an unexpected packet
           */
          if (type == GNUTLS_ALERT || (htype == GNUTLS_HANDSHAKE_CLIENT_HELLO
                                       && type == GNUTLS_HANDSHAKE))
            return GNUTLS_E_GOT_APPLICATION_DATA;
          else
            {
              gnutls_assert ();
              return GNUTLS_E_UNEXPECTED_PACKET;
            }

          break;
        case GNUTLS_HANDSHAKE:
          /* This is legal if HELLO_REQUEST is received - and we are a client.
           * If we are a server, a client may initiate a renegotiation at any time.
           */
          if (session->security_parameters.entity == GNUTLS_SERVER)
            {
              gnutls_assert ();
              ret =
                _gnutls_record_buffer_put (recv_type, session, (void *) data,
                                           data_size);
              if (ret < 0)
                {
                  gnutls_assert ();
                  return ret;
                }
              return GNUTLS_E_REHANDSHAKE;
            }

          /* If we are already in a handshake then a Hello
           * Request is illegal. But here we don't really care
           * since this message will never make it up here.
           */

          /* So we accept it */
          return _gnutls_recv_hello_request (session, data, data_size);

          break;
        case GNUTLS_INNER_APPLICATION:
          /* even if data is unexpected put it into the buffer */
          if ((ret = _gnutls_record_buffer_put (recv_type, session,
                                                (void *) data,
                                                data_size)) < 0)
            {
              gnutls_assert ();
              return ret;
            }
          gnutls_assert ();
          return GNUTLS_E_UNEXPECTED_PACKET;
          break;
        default:

          _gnutls_record_log
            ("REC[%p]: Received Unknown packet %d expecting %d\n",
             session, recv_type, type);

          gnutls_assert ();
          return GNUTLS_E_INTERNAL_ERROR;
        }
    }

  return 0;

}


/* This function will return the internal (per session) temporary
 * recv buffer. If the buffer was not initialized before it will
 * also initialize it.
 */
inline static int
get_temp_recv_buffer (gnutls_session_t session, gnutls_datum_t * tmp)
{
  size_t max_record_size;

  if (gnutls_compression_get (session) != GNUTLS_COMP_NULL ||
      session->internals.priorities.allow_large_records != 0)
    max_record_size = MAX_RECORD_RECV_SIZE + EXTRA_COMP_SIZE;
  else
    max_record_size = MAX_RECORD_RECV_SIZE;

  /* We allocate MAX_RECORD_RECV_SIZE length
   * because we cannot predict the output data by the record
   * packet length (due to compression).
   */

  if (max_record_size > session->internals.recv_buffer.size ||
      session->internals.recv_buffer.data == NULL)
    {

      /* Initialize the internal buffer.
       */
      session->internals.recv_buffer.data =
        gnutls_realloc (session->internals.recv_buffer.data, max_record_size);

      if (session->internals.recv_buffer.data == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      session->internals.recv_buffer.size = max_record_size;
    }

  tmp->data = session->internals.recv_buffer.data;
  tmp->size = session->internals.recv_buffer.size;

  return 0;
}


#define MAX_EMPTY_PACKETS_SEQUENCE 4

/* This function behaves exactly like read(). The only difference is
 * that it accepts the gnutls_session_t and the content_type_t of data to
 * receive (if called by the user the Content is Userdata only)
 * It is intended to receive data, under the current session.
 *
 * The gnutls_handshake_description_t was introduced to support SSL V2.0 client hellos.
 */
ssize_t
_gnutls_recv_int (gnutls_session_t session, content_type_t type,
                  gnutls_handshake_description_t htype,
                  opaque * data, size_t sizeofdata)
{
  int decrypted_length;
  opaque version[2];
  content_type_t recv_type;
  uint16_t length;
  uint8_t *ciphertext;
  int ret, ret2;
  uint16_t header_size;
  int empty_packet = 0;
  gnutls_datum_t data_enc, tmp;
  record_parameters_st *record_params;
  record_state_st *record_state;

  ret = _gnutls_epoch_get (session, EPOCH_READ_CURRENT, &record_params);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* Safeguard against processing data with an incomplete cipher state. */
  if (!record_params->initialized)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  record_state = &record_params->read;

  if (type != GNUTLS_ALERT && (sizeofdata == 0 || data == NULL))
    {
      return GNUTLS_E_INVALID_REQUEST;
    }

begin:

  if (empty_packet > MAX_EMPTY_PACKETS_SEQUENCE)
    {
      gnutls_assert ();
      return GNUTLS_E_TOO_MANY_EMPTY_PACKETS;
    }

  if (session->internals.read_eof != 0)
    {
      /* if we have already read an EOF
       */
      return 0;
    }
  else if (session_is_valid (session) != 0
           || session->internals.may_not_read != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_SESSION;
    }

/* If we have enough data in the cache do not bother receiving
 * a new packet. (in order to flush the cache)
 */
  ret = check_buffers (session, type, data, sizeofdata);
  if (ret != 0)
    return ret;


/* default headers for TLS 1.0
 */
  header_size = RECORD_HEADER_SIZE;

  if ((ret =
       _gnutls_io_read_buffered (session, header_size, -1)) != header_size)
    {
      if (ret < 0 && gnutls_error_is_fatal (ret) == 0)
        return ret;

      session_invalidate (session);
      if (type == GNUTLS_ALERT)
        {
          gnutls_assert ();
          return 0;             /* we were expecting close notify */
        }
      session_unresumable (session);
      gnutls_assert ();
      return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
    }

  ret = _mbuffer_linearize (&session->internals.record_recv_buffer);
  if (ret != 0)
    {
      gnutls_assert ();
      return ret;
    }

  _mbuffer_get_first (&session->internals.record_recv_buffer, &data_enc);

  if ((ret =
       record_check_headers (session, data_enc.data, type, htype, &recv_type,
                             version, &length, &header_size)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

/* Here we check if the Type of the received packet is
 * ok. 
 */
  if ((ret = check_recv_type (recv_type)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

/* Here we check if the advertized version is the one we
 * negotiated in the handshake.
 */
  if ((ret = record_check_version (session, htype, version)) < 0)
    {
      gnutls_assert ();
      session_invalidate (session);
      return ret;
    }

  _gnutls_record_log
    ("REC[%p]: Expected Packet[%d] %s(%d) with length: %d\n", session,
     (int) _gnutls_uint64touint32 (&record_state->sequence_number),
     _gnutls_packet2str (type), type, (int) sizeofdata);
  _gnutls_record_log ("REC[%p]: Received Packet[%d] %s(%d) with length: %d\n",
                      session,
                      (int)
                      _gnutls_uint64touint32 (&record_state->sequence_number),
                      _gnutls_packet2str (recv_type), recv_type, length);

  if (length > MAX_RECV_SIZE)
    {
      _gnutls_record_log
        ("REC[%p]: FATAL ERROR: Received packet with length: %d\n",
         session, length);

      session_unresumable (session);
      session_invalidate (session);
      gnutls_assert ();
      return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
    }

/* check if we have that data into buffer. 
 */
  if ((ret =
       _gnutls_io_read_buffered (session, header_size + length,
                                 recv_type)) != header_size + length)
    {
      if (ret < 0 && gnutls_error_is_fatal (ret) == 0)
        return ret;

      session_unresumable (session);
      session_invalidate (session);
      gnutls_assert ();
      return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
    }

/* ok now we are sure that we can read all the data - so
 * move on !
 */

  ret = _mbuffer_linearize (&session->internals.record_recv_buffer);
  if (ret != 0)
    {
      gnutls_assert ();
      return ret;
    }
  _mbuffer_get_first (&session->internals.record_recv_buffer, &data_enc);
  ciphertext = &data_enc.data[header_size];

  ret = get_temp_recv_buffer (session, &tmp);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

/* decrypt the data we got. 
 */
  ret =
    _gnutls_decrypt (session, ciphertext, length, tmp.data, tmp.size,
                     recv_type, record_params);
  if (ret < 0)
    {
      session_unresumable (session);
      session_invalidate (session);
      gnutls_assert ();
      return ret;
    }
  _mbuffer_remove_bytes (&session->internals.record_recv_buffer,
                         header_size + length);
  decrypted_length = ret;

/* Check if this is a CHANGE_CIPHER_SPEC
 */
  if (type == GNUTLS_CHANGE_CIPHER_SPEC &&
      recv_type == GNUTLS_CHANGE_CIPHER_SPEC)
    {

      _gnutls_record_log
        ("REC[%p]: ChangeCipherSpec Packet was received\n", session);

      if ((size_t) ret != sizeofdata)
        {                       /* sizeofdata should be 1 */
          gnutls_assert ();
          return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
        }
      memcpy (data, tmp.data, sizeofdata);

      return ret;
    }

  _gnutls_record_log
    ("REC[%p]: Decrypted Packet[%d] %s(%d) with length: %d\n", session,
     (int) _gnutls_uint64touint32 (&record_state->sequence_number),
     _gnutls_packet2str (recv_type), recv_type, decrypted_length);

/* increase sequence number 
 */
  if (_gnutls_uint64pp (&record_state->sequence_number) != 0)
    {
      session_invalidate (session);
      gnutls_assert ();
      return GNUTLS_E_RECORD_LIMIT_REACHED;
    }

  ret =
    record_check_type (session, recv_type, type, htype, tmp.data,
                       decrypted_length);
  if (ret < 0)
    {
      if (ret == GNUTLS_E_INT_RET_0)
        return 0;
      gnutls_assert ();
      return ret;
    }

/* Get Application data from buffer 
 */
  if ((recv_type == type) &&
      (type == GNUTLS_APPLICATION_DATA ||
       type == GNUTLS_HANDSHAKE || type == GNUTLS_INNER_APPLICATION))
    {

      ret = _gnutls_record_buffer_get (type, session, data, sizeofdata);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      /* if the buffer just got empty 
       */
      if (_gnutls_record_buffer_get_size (type, session) == 0)
        {
          if ((ret2 = _gnutls_io_clear_peeked_data (session)) < 0)
            {
              gnutls_assert ();
              return ret2;
            }
        }
    }
  else
    {
      gnutls_assert ();
      return GNUTLS_E_UNEXPECTED_PACKET;
      /* we didn't get what we wanted to 
       */
    }

/* (originally for) TLS 1.0 CBC protection. 
 * Actually this code is called if we just received
 * an empty packet. An empty TLS packet is usually
 * sent to protect some vulnerabilities in the CBC mode.
 * In that case we go to the beginning and start reading
 * the next packet.
 */
  if (ret == 0)
    {
      empty_packet++;
      goto begin;
    }

  return ret;
}


/**
 * gnutls_record_send:
 * @session: is a #gnutls_session_t structure.
 * @data: contains the data to send
 * @sizeofdata: is the length of the data
 *
 * This function has the similar semantics with send().  The only
 * difference is that it accepts a GnuTLS session, and uses different
 * error codes.
 *
 * Note that if the send buffer is full, send() will block this
 * function.  See the send() documentation for full information.  You
 * can replace the default push function by using
 * gnutls_transport_set_ptr2() with a call to send() with a
 * MSG_DONTWAIT flag if blocking is a problem.
 *
 * If the EINTR is returned by the internal push function (the
 * default is send()} then %GNUTLS_E_INTERRUPTED will be returned. If
 * %GNUTLS_E_INTERRUPTED or %GNUTLS_E_AGAIN is returned, you must
 * call this function again, with the same parameters; alternatively
 * you could provide a %NULL pointer for data, and 0 for
 * size. cf. gnutls_record_get_direction().
 *
 * Returns: the number of bytes sent, or a negative error code.  The
 *   number of bytes sent might be less than @sizeofdata.  The maximum
 *   number of bytes this function can send in a single call depends
 *   on the negotiated maximum record size.
  **/
ssize_t
gnutls_record_send (gnutls_session_t session, const void *data,
                    size_t sizeofdata)
{
  return _gnutls_send_int (session, GNUTLS_APPLICATION_DATA, -1,
                           EPOCH_WRITE_CURRENT, data, sizeofdata,
                           MBUFFER_FLUSH);
}

/**
 * gnutls_record_recv:
 * @session: is a #gnutls_session_t structure.
 * @data: the buffer that the data will be read into
 * @sizeofdata: the number of requested bytes
 *
 * This function has the similar semantics with recv().  The only
 * difference is that it accepts a GnuTLS session, and uses different
 * error codes.
 *
 * In the special case that a server requests a renegotiation, the
 * client may receive an error code of %GNUTLS_E_REHANDSHAKE.  This
 * message may be simply ignored, replied with an alert
 * %GNUTLS_A_NO_RENEGOTIATION, or replied with a new handshake,
 * depending on the client's will.
 *
 * If %EINTR is returned by the internal push function (the default
 * is recv()) then %GNUTLS_E_INTERRUPTED will be returned.  If
 * %GNUTLS_E_INTERRUPTED or %GNUTLS_E_AGAIN is returned, you must
 * call this function again to get the data.  See also
 * gnutls_record_get_direction().
 *
 * A server may also receive %GNUTLS_E_REHANDSHAKE when a client has
 * initiated a handshake. In that case the server can only initiate a
 * handshake or terminate the connection.
 *
 * Returns: the number of bytes received and zero on EOF.  A negative
 *   error code is returned in case of an error.  The number of bytes
 *   received might be less than @sizeofdata.
 **/
ssize_t
gnutls_record_recv (gnutls_session_t session, void *data, size_t sizeofdata)
{
  return _gnutls_recv_int (session, GNUTLS_APPLICATION_DATA, -1, data,
                           sizeofdata);
}
