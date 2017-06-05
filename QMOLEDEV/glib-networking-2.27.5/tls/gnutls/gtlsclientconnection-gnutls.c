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
#include <string.h>

#include "gtlsclientconnection-gnutls.h"
#include "gtlsbackend-gnutls.h"
#include "gtlscertificate-gnutls.h"
#include <glib/gi18n-lib.h>

enum
{
  PROP_0,
  PROP_VALIDATION_FLAGS,
  PROP_SERVER_IDENTITY,
  PROP_USE_SSL3,
  PROP_ACCEPTED_CAS
};

static void g_tls_client_connection_gnutls_get_property (GObject    *object,
							 guint       prop_id,
							 GValue     *value,
							 GParamSpec *pspec);
static void g_tls_client_connection_gnutls_set_property (GObject      *object,
							 guint         prop_id,
							 const GValue *value,
							 GParamSpec   *pspec);
static void g_tls_client_connection_gnutls_constructed  (GObject      *object);
static void g_tls_client_connection_gnutls_finalize     (GObject      *object);

static void     g_tls_client_connection_gnutls_begin_handshake  (GTlsConnectionGnutls  *conn);
static gboolean g_tls_client_connection_gnutls_verify_peer      (GTlsConnectionGnutls  *gnutls,
								 GTlsCertificate       *peer_certificate,
								 GTlsCertificateFlags  *errors);
static void     g_tls_client_connection_gnutls_finish_handshake (GTlsConnectionGnutls  *conn,
								 GError               **inout_error);

static void g_tls_client_connection_gnutls_client_connection_interface_init (GTlsClientConnectionInterface *iface);

static int g_tls_client_connection_gnutls_retrieve_function (gnutls_session_t             session,
							     const gnutls_datum_t        *req_ca_rdn,
							     int                          nreqs,
							     const gnutls_pk_algorithm_t *pk_algos,
							     int                          pk_algos_length,
							     gnutls_retr_st              *st);

G_DEFINE_TYPE_WITH_CODE (GTlsClientConnectionGnutls, g_tls_client_connection_gnutls, G_TYPE_TLS_CONNECTION_GNUTLS,
			 G_IMPLEMENT_INTERFACE (G_TYPE_TLS_CLIENT_CONNECTION,
						g_tls_client_connection_gnutls_client_connection_interface_init));

struct _GTlsClientConnectionGnutlsPrivate
{
  GTlsCertificateFlags validation_flags;
  GSocketConnectable *server_identity;
  gboolean use_ssl3;

  char *session_id;

  gboolean cert_requested;
  char **accepted_cas;
};

static void
g_tls_client_connection_gnutls_class_init (GTlsClientConnectionGnutlsClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GTlsConnectionGnutlsClass *connection_gnutls_class = G_TLS_CONNECTION_GNUTLS_CLASS (klass);

  g_type_class_add_private (klass, sizeof (GTlsClientConnectionGnutlsPrivate));

  gobject_class->get_property = g_tls_client_connection_gnutls_get_property;
  gobject_class->set_property = g_tls_client_connection_gnutls_set_property;
  gobject_class->constructed  = g_tls_client_connection_gnutls_constructed;
  gobject_class->finalize     = g_tls_client_connection_gnutls_finalize;

  connection_gnutls_class->begin_handshake  = g_tls_client_connection_gnutls_begin_handshake;
  connection_gnutls_class->verify_peer      = g_tls_client_connection_gnutls_verify_peer;
  connection_gnutls_class->finish_handshake = g_tls_client_connection_gnutls_finish_handshake;

  g_object_class_override_property (gobject_class, PROP_VALIDATION_FLAGS, "validation-flags");
  g_object_class_override_property (gobject_class, PROP_SERVER_IDENTITY, "server-identity");
  g_object_class_override_property (gobject_class, PROP_USE_SSL3, "use-ssl3");
  g_object_class_override_property (gobject_class, PROP_ACCEPTED_CAS, "accepted-cas");
}

static void
g_tls_client_connection_gnutls_client_connection_interface_init (GTlsClientConnectionInterface *iface)
{
}

static void
g_tls_client_connection_gnutls_init (GTlsClientConnectionGnutls *gnutls)
{
  gnutls_certificate_credentials_t creds;

  gnutls->priv = G_TYPE_INSTANCE_GET_PRIVATE (gnutls, G_TYPE_TLS_CLIENT_CONNECTION_GNUTLS, GTlsClientConnectionGnutlsPrivate);

  creds = g_tls_connection_gnutls_get_credentials (G_TLS_CONNECTION_GNUTLS (gnutls));
  gnutls_certificate_client_set_retrieve_function (creds, g_tls_client_connection_gnutls_retrieve_function);
}

static void
g_tls_client_connection_gnutls_constructed (GObject *object)
{
  GTlsClientConnectionGnutls *gnutls = G_TLS_CLIENT_CONNECTION_GNUTLS (object);
  GSocketConnection *base_conn;
  GSocketAddress *remote_addr;
  GInetAddress *iaddr;
  guint port;

  /* We base the session ID on the IP address rather than on
   * server-identity, because it's likely that different virtual
   * servers on the same host will have access to the same session
   * cache, whereas different hosts serving the same hostname/service
   * likely won't. Note that session IDs are opaque, and transmitted
   * in the clear anyway, so there are no security issues if we send
   * one to the "wrong" server; we'll just fail to get a resumed
   * session.
   */
  g_object_get (G_OBJECT (gnutls), "base-io-stream", &base_conn, NULL);
  if (G_IS_SOCKET_CONNECTION (base_conn))
    {
      remote_addr = g_socket_connection_get_remote_address (base_conn, NULL);
      if (G_IS_INET_SOCKET_ADDRESS (remote_addr))
	{
	  GInetSocketAddress *isaddr = G_INET_SOCKET_ADDRESS (remote_addr);
	  gchar *addrstr;

	  iaddr = g_inet_socket_address_get_address (isaddr);
	  port = g_inet_socket_address_get_port (isaddr);

	  addrstr = g_inet_address_to_string (iaddr);
	  gnutls->priv->session_id = g_strdup_printf ("%s/%d", addrstr, port);
	  g_free (addrstr);
	}
    }
  g_object_unref (base_conn);

  if (G_OBJECT_CLASS (g_tls_client_connection_gnutls_parent_class)->constructed)
    G_OBJECT_CLASS (g_tls_client_connection_gnutls_parent_class)->constructed (object);
}

static void
g_tls_client_connection_gnutls_finalize (GObject *object)
{
  GTlsClientConnectionGnutls *gnutls = G_TLS_CLIENT_CONNECTION_GNUTLS (object);

  if (gnutls->priv->server_identity)
    g_object_unref (gnutls->priv->server_identity);
  if (gnutls->priv->accepted_cas)
    g_strfreev (gnutls->priv->accepted_cas);
  if (gnutls->priv->session_id)
    g_free (gnutls->priv->session_id);

  G_OBJECT_CLASS (g_tls_client_connection_gnutls_parent_class)->finalize (object);
}

static void
g_tls_client_connection_gnutls_get_property (GObject    *object,
					     guint       prop_id,
					     GValue     *value,
					     GParamSpec *pspec)
{
  GTlsClientConnectionGnutls *gnutls = G_TLS_CLIENT_CONNECTION_GNUTLS (object);

  switch (prop_id)
    {
    case PROP_VALIDATION_FLAGS:
      g_value_set_flags (value, gnutls->priv->validation_flags);
      break;

    case PROP_SERVER_IDENTITY:
      g_value_set_object (value, gnutls->priv->server_identity);
      break;

    case PROP_USE_SSL3:
      g_value_set_boolean (value, gnutls->priv->use_ssl3);
      break;

    case PROP_ACCEPTED_CAS:
      g_value_set_boxed (value, gnutls->priv->accepted_cas);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
g_tls_client_connection_gnutls_set_property (GObject      *object,
					     guint         prop_id,
					     const GValue *value,
					     GParamSpec   *pspec)
{
  GTlsClientConnectionGnutls *gnutls = G_TLS_CLIENT_CONNECTION_GNUTLS (object);
  const char *hostname;

  switch (prop_id)
    {
    case PROP_VALIDATION_FLAGS:
      gnutls->priv->validation_flags = g_value_get_flags (value);
      break;

    case PROP_SERVER_IDENTITY:
      if (gnutls->priv->server_identity)
	g_object_unref (gnutls->priv->server_identity);
      gnutls->priv->server_identity = g_value_dup_object (value);

      if (G_IS_NETWORK_ADDRESS (gnutls->priv->server_identity))
	hostname = g_network_address_get_hostname (G_NETWORK_ADDRESS (gnutls->priv->server_identity));
      else if (G_IS_NETWORK_SERVICE (gnutls->priv->server_identity))
	hostname = g_network_service_get_domain (G_NETWORK_SERVICE (gnutls->priv->server_identity));
      else
	hostname = NULL;

      if (hostname)
	{
	  gnutls_session_t session = g_tls_connection_gnutls_get_session (G_TLS_CONNECTION_GNUTLS (gnutls));

	  gnutls_server_name_set (session, GNUTLS_NAME_DNS,
				  hostname, strlen (hostname));
	}
      break;

    case PROP_USE_SSL3:
      gnutls->priv->use_ssl3 = g_value_get_boolean (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static int
g_tls_client_connection_gnutls_retrieve_function (gnutls_session_t             session,
						  const gnutls_datum_t        *req_ca_rdn,
						  int                          nreqs,
						  const gnutls_pk_algorithm_t *pk_algos,
						  int                          pk_algos_length,
						  gnutls_retr_st              *st)
{
  GTlsClientConnectionGnutls *gnutls = gnutls_transport_get_ptr (session);
  GPtrArray *accepted_cas;
  int i;
  char *buf, dummy[1];
  size_t size;

  gnutls->priv->cert_requested = TRUE;

  accepted_cas = g_ptr_array_new ();
  for (i = 0; i < nreqs; i++)
    {
      size = sizeof (dummy);
      gnutls_x509_rdn_get (&req_ca_rdn[i], dummy, &size);
      buf = g_malloc (size);
      gnutls_x509_rdn_get (&req_ca_rdn[i], buf, &size);
      g_ptr_array_add (accepted_cas, buf);
    }
  g_ptr_array_add (accepted_cas, NULL);

  gnutls->priv->accepted_cas = (char **)accepted_cas->pdata;
  g_ptr_array_free (accepted_cas, FALSE);

  g_tls_connection_gnutls_get_certificate (G_TLS_CONNECTION_GNUTLS (gnutls), st);
  return 0;
}

static void
g_tls_client_connection_gnutls_begin_handshake (GTlsConnectionGnutls *conn)
{
  GTlsClientConnectionGnutls *gnutls = G_TLS_CLIENT_CONNECTION_GNUTLS (conn);

  /* Try to get a cached session */
  if (gnutls->priv->session_id)
    {
      GByteArray *session_data;

      session_data = g_tls_backend_gnutls_lookup_session_data (gnutls->priv->session_id);
      if (session_data)
	{
	  gnutls_session_set_data (g_tls_connection_gnutls_get_session (conn),
				   session_data->data, session_data->len);
	  g_byte_array_unref (session_data);
	}
    }

  gnutls->priv->cert_requested = FALSE;
}

static gboolean
g_tls_client_connection_gnutls_verify_peer (GTlsConnectionGnutls  *conn_gnutls,
					    GTlsCertificate       *peer_certificate,
					    GTlsCertificateFlags  *errors)
{
  GTlsClientConnectionGnutls *gnutls = G_TLS_CLIENT_CONNECTION_GNUTLS (conn_gnutls);
  gboolean accepted;

  if (gnutls->priv->server_identity)
    {
      *errors |= g_tls_certificate_gnutls_verify_identity (G_TLS_CERTIFICATE_GNUTLS (peer_certificate),
							   gnutls->priv->server_identity);
    }

  if (*errors & gnutls->priv->validation_flags)
    accepted = g_tls_connection_emit_accept_certificate (G_TLS_CONNECTION (gnutls), peer_certificate, *errors);
  else
    accepted = TRUE;

  return accepted;
}

static void
g_tls_client_connection_gnutls_finish_handshake (GTlsConnectionGnutls  *conn,
						 GError               **inout_error)
{
  GTlsClientConnectionGnutls *gnutls = G_TLS_CLIENT_CONNECTION_GNUTLS (conn);

  if (g_error_matches (*inout_error, G_TLS_ERROR, G_TLS_ERROR_NOT_TLS) &&
      gnutls->priv->cert_requested)
    {
      g_clear_error (inout_error);
      g_set_error_literal (inout_error, G_TLS_ERROR, G_TLS_ERROR_CERTIFICATE_REQUIRED,
			   _("Server required TLS certificate"));
    }

  if (gnutls->priv->session_id)
    {
      gnutls_datum session_data;

      if (!*inout_error &&
	  gnutls_session_get_data2 (g_tls_connection_gnutls_get_session (conn),
				    &session_data) == 0)
	{
	  g_tls_backend_gnutls_cache_session_data (gnutls->priv->session_id,
						   session_data.data,
						   session_data.size);
	  gnutls_free (session_data.data);
	}
      else
	g_tls_backend_gnutls_uncache_session_data (gnutls->priv->session_id);
    }
}
