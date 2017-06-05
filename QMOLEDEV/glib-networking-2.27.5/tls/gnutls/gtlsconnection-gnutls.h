/* GIO - GLib Connection, Output and Gnutlsing Library
 *
 * Copyright © 2009 Red Hat, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2 of the licence or (at
 * your option) any later version.
 *
 * See the included COPYING file for more information.
 */

#ifndef __G_TLS_CONNECTION_GNUTLS_H__
#define __G_TLS_CONNECTION_GNUTLS_H__

#include <gio/gio.h>
#include <gnutls/gnutls.h>

G_BEGIN_DECLS

#define G_TYPE_TLS_CONNECTION_GNUTLS            (g_tls_connection_gnutls_get_type ())
#define G_TLS_CONNECTION_GNUTLS(inst)           (G_TYPE_CHECK_INSTANCE_CAST ((inst), G_TYPE_TLS_CONNECTION_GNUTLS, GTlsConnectionGnutls))
#define G_TLS_CONNECTION_GNUTLS_CLASS(class)    (G_TYPE_CHECK_CLASS_CAST ((class), G_TYPE_TLS_CONNECTION_GNUTLS, GTlsConnectionGnutlsClass))
#define G_IS_TLS_CONNECTION_GNUTLS(inst)        (G_TYPE_CHECK_INSTANCE_TYPE ((inst), G_TYPE_TLS_CONNECTION_GNUTLS))
#define G_IS_TLS_CONNECTION_GNUTLS_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE ((class), G_TYPE_TLS_CONNECTION_GNUTLS))
#define G_TLS_CONNECTION_GNUTLS_GET_CLASS(inst) (G_TYPE_INSTANCE_GET_CLASS ((inst), G_TYPE_TLS_CONNECTION_GNUTLS, GTlsConnectionGnutlsClass))

typedef struct _GTlsConnectionGnutlsPrivate                   GTlsConnectionGnutlsPrivate;
typedef struct _GTlsConnectionGnutlsClass                     GTlsConnectionGnutlsClass;
typedef struct _GTlsConnectionGnutls                          GTlsConnectionGnutls;

struct _GTlsConnectionGnutlsClass
{
  GTlsConnectionClass parent_class;

  void     (*begin_handshake)  (GTlsConnectionGnutls  *gnutls);
  gboolean (*verify_peer)      (GTlsConnectionGnutls  *gnutls,
				GTlsCertificate       *peer_certificate,
				GTlsCertificateFlags  *errors);
  void     (*finish_handshake) (GTlsConnectionGnutls  *gnutls,
				GError               **inout_error);
};

struct _GTlsConnectionGnutls
{
  GTlsConnection parent_instance;
  GTlsConnectionGnutlsPrivate *priv;
};

GType g_tls_connection_gnutls_get_type (void) G_GNUC_CONST;

gnutls_certificate_credentials g_tls_connection_gnutls_get_credentials (GTlsConnectionGnutls *connection);
gnutls_session                 g_tls_connection_gnutls_get_session     (GTlsConnectionGnutls *connection);
void                           g_tls_connection_gnutls_get_certificate (GTlsConnectionGnutls *gnutls,
									 gnutls_retr_st       *st);

gssize   g_tls_connection_gnutls_read          (GTlsConnectionGnutls  *gnutls,
						void                  *buffer,
						gsize                  size,
						gboolean               blocking,
						GCancellable          *cancellable,
						GError               **error);
gssize   g_tls_connection_gnutls_write         (GTlsConnectionGnutls  *gnutls,
						const void            *buffer,
						gsize                  size,
						gboolean               blocking,
						GCancellable          *cancellable,
						GError               **error);

gboolean g_tls_connection_gnutls_check         (GTlsConnectionGnutls  *gnutls,
						GIOCondition           condition);
GSource *g_tls_connection_gnutls_create_source (GTlsConnectionGnutls  *gnutls,
						GIOCondition           condition,
						GCancellable          *cancellable);

G_END_DECLS

#endif /* __G_TLS_CONNECTION_GNUTLS_H___ */
