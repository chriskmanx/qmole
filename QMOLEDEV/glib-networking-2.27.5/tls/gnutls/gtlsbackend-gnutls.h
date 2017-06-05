/* GIO - GLib Backend, Output and Gnutlsing Library
 *
 * Copyright © 2010 Red Hat, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2 of the licence or (at
 * your option) any later version.
 *
 * See the included COPYING file for more information.
 */

#ifndef __G_TLS_BACKEND_GNUTLS_H__
#define __G_TLS_BACKEND_GNUTLS_H__

#include <gio/gio.h>
#include <gnutls/gnutls.h>

G_BEGIN_DECLS

#define G_TYPE_TLS_BACKEND_GNUTLS            (g_tls_backend_gnutls_get_type ())
#define G_TLS_BACKEND_GNUTLS(inst)           (G_TYPE_CHECK_INSTANCE_CAST ((inst), G_TYPE_TLS_BACKEND_GNUTLS, GTlsBackendGnutls))
#define G_TLS_BACKEND_GNUTLS_CLASS(class)    (G_TYPE_CHECK_CLASS_CAST ((class), G_TYPE_TLS_BACKEND_GNUTLS, GTlsBackendGnutlsClass))
#define G_IS_TLS_BACKEND_GNUTLS(inst)        (G_TYPE_CHECK_INSTANCE_TYPE ((inst), G_TYPE_TLS_BACKEND_GNUTLS))
#define G_IS_TLS_BACKEND_GNUTLS_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE ((class), G_TYPE_TLS_BACKEND_GNUTLS))
#define G_TLS_BACKEND_GNUTLS_GET_CLASS(inst) (G_TYPE_INSTANCE_GET_CLASS ((inst), G_TYPE_TLS_BACKEND_GNUTLS, GTlsBackendGnutlsClass))

typedef struct _GTlsBackendGnutlsClass GTlsBackendGnutlsClass;
typedef struct _GTlsBackendGnutls      GTlsBackendGnutls;

struct _GTlsBackendGnutlsClass
{
  GObjectClass parent_class;
};

struct _GTlsBackendGnutls
{
  GObject parent_instance;
};

GType g_tls_backend_gnutls_get_type (void) G_GNUC_CONST;
void  g_tls_backend_gnutls_register (GIOModule *module);

const GList *g_tls_backend_gnutls_get_system_ca_list_gtls   (void) G_GNUC_CONST;
void         g_tls_backend_gnutls_get_system_ca_list_gnutls (gnutls_x509_crt_t **cas,
							     int                *num_cas);

void         g_tls_backend_gnutls_cache_session_data        (const gchar *session_id,
							     guchar      *session_data,
							     gsize        session_data_length);
void         g_tls_backend_gnutls_uncache_session_data      (const gchar *session_id);
GByteArray  *g_tls_backend_gnutls_lookup_session_data       (const gchar *session_id);

G_END_DECLS

#endif /* __G_TLS_BACKEND_GNUTLS_H___ */
