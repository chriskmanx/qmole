/* GIO - GLib Input, Output and Streaming Library
 *
 * Copyright © 2010 Red Hat, Inc
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "config.h"
#include "glib.h"

#include <errno.h>
#include <gnutls/gnutls.h>
#include <gnutls/x509.h>

#include "gtlsserverconnection-gnutls.h"
#include "gtlscertificate-gnutls.h"
#include <glib/gi18n-lib.h>

enum
{
  PROP_0,
  PROP_AUTHENTICATION_MODE
};

static void g_tls_server_connection_gnutls_get_property (GObject    *object,
							 guint       prop_id,
							 GValue     *value,
							 GParamSpec *pspec);
static void g_tls_server_connection_gnutls_set_property (GObject      *object,
							 guint         prop_id,
							 const GValue *value,
							 GParamSpec   *pspec);

static void     g_tls_server_connection_gnutls_begin_handshake  (GTlsConnectionGnutls  *conn);
static gboolean g_tls_server_connection_gnutls_verify_peer      (GTlsConnectionGnutls  *gnutls,
								 GTlsCertificate       *peer_certificate,
								 GTlsCertificateFlags  *errors);
static void     g_tls_server_connection_gnutls_finish_handshake (GTlsConnectionGnutls  *conn,
								 GError               **inout_error);

static void g_tls_server_connection_gnutls_server_connection_interface_init (GTlsServerConnectionInterface *iface);

static int g_tls_server_connection_gnutls_retrieve_function (gnutls_session_t  session,
							     gnutls_retr_st   *st);

G_DEFINE_TYPE_WITH_CODE (GTlsServerConnectionGnutls, g_tls_server_connection_gnutls, G_TYPE_TLS_CONNECTION_GNUTLS,
			 G_IMPLEMENT_INTERFACE (G_TYPE_TLS_SERVER_CONNECTION,
						g_tls_server_connection_gnutls_server_connection_interface_init))

struct _GTlsServerConnectionGnutlsPrivate
{
  GTlsAuthenticationMode authentication_mode;
};

static void
g_tls_server_connection_gnutls_class_init (GTlsServerConnectionGnutlsClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GTlsConnectionGnutlsClass *connection_gnutls_class = G_TLS_CONNECTION_GNUTLS_CLASS (klass);

  g_type_class_add_private (klass, sizeof (GTlsServerConnectionGnutlsPrivate));

  gobject_class->get_property = g_tls_server_connection_gnutls_get_property;
  gobject_class->set_property = g_tls_server_connection_gnutls_set_property;

  connection_gnutls_class->begin_handshake  = g_tls_server_connection_gnutls_begin_handshake;
  connection_gnutls_class->verify_peer      = g_tls_server_connection_gnutls_verify_peer;
  connection_gnutls_class->finish_handshake = g_tls_server_connection_gnutls_finish_handshake;

  g_object_class_override_property (gobject_class, PROP_AUTHENTICATION_MODE, "authentication-mode");
}

static void
g_tls_server_connection_gnutls_server_connection_interface_init (GTlsServerConnectionInterface *iface)
{
}

static void
g_tls_server_connection_gnutls_init (GTlsServerConnectionGnutls *gnutls)
{
  gnutls_certificate_credentials_t creds;

  gnutls->priv = G_TYPE_INSTANCE_GET_PRIVATE (gnutls, G_TYPE_TLS_SERVER_CONNECTION_GNUTLS, GTlsServerConnectionGnutlsPrivate);

  creds = g_tls_connection_gnutls_get_credentials (G_TLS_CONNECTION_GNUTLS (gnutls));
  gnutls_certificate_server_set_retrieve_function (creds, g_tls_server_connection_gnutls_retrieve_function);
}

static void
g_tls_server_connection_gnutls_get_property (GObject    *object,
					     guint       prop_id,
					     GValue     *value,
					     GParamSpec *pspec)
{
  GTlsServerConnectionGnutls *gnutls = G_TLS_SERVER_CONNECTION_GNUTLS (object);

  switch (prop_id)
    {
    case PROP_AUTHENTICATION_MODE:
      g_value_set_enum (value, gnutls->priv->authentication_mode);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
g_tls_server_connection_gnutls_set_property (GObject      *object,
					     guint         prop_id,
					     const GValue *value,
					     GParamSpec   *pspec)
{
  GTlsServerConnectionGnutls *gnutls = G_TLS_SERVER_CONNECTION_GNUTLS (object);

  switch (prop_id)
    {
    case PROP_AUTHENTICATION_MODE:
      gnutls->priv->authentication_mode = g_value_get_enum (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static int
g_tls_server_connection_gnutls_retrieve_function (gnutls_session_t  session,
						  gnutls_retr_st   *st)
{
  g_tls_connection_gnutls_get_certificate (gnutls_transport_get_ptr (session), st);
  return 0;
}

static void
g_tls_server_connection_gnutls_begin_handshake (GTlsConnectionGnutls *conn)
{
  GTlsServerConnectionGnutls *gnutls = G_TLS_SERVER_CONNECTION_GNUTLS (conn);
  gnutls_session_t session;
  gnutls_certificate_request_t req_mode;

  switch (gnutls->priv->authentication_mode)
    {
    case G_TLS_AUTHENTICATION_REQUESTED:
      req_mode = GNUTLS_CERT_REQUEST;
      break;
    case G_TLS_AUTHENTICATION_REQUIRED:
      req_mode = GNUTLS_CERT_REQUIRE;
      break;
    default:
      req_mode = GNUTLS_CERT_IGNORE;
      break;
    }

  session = g_tls_connection_gnutls_get_session (conn);
  gnutls_certificate_server_set_request (session, req_mode);
}

static gboolean
g_tls_server_connection_gnutls_verify_peer (GTlsConnectionGnutls  *gnutls,
					    GTlsCertificate       *peer_certificate,
					    GTlsCertificateFlags  *errors)
{
  return g_tls_connection_emit_accept_certificate (G_TLS_CONNECTION (gnutls),
						   peer_certificate, *errors);
}

static void
g_tls_server_connection_gnutls_finish_handshake (GTlsConnectionGnutls  *gnutls,
						 GError               **inout_error)
{
}
