/* GIO - GLib Certificate, Output and Gnutlsing Library
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

#ifndef __G_TLS_CERTIFICATE_GNUTLS_H__
#define __G_TLS_CERTIFICATE_GNUTLS_H__

#include <gio/gio.h>

G_BEGIN_DECLS

#define G_TYPE_TLS_CERTIFICATE_GNUTLS            (g_tls_certificate_gnutls_get_type ())
#define G_TLS_CERTIFICATE_GNUTLS(inst)           (G_TYPE_CHECK_INSTANCE_CAST ((inst), G_TYPE_TLS_CERTIFICATE_GNUTLS, GTlsCertificateGnutls))
#define G_TLS_CERTIFICATE_GNUTLS_CLASS(class)    (G_TYPE_CHECK_CLASS_CAST ((class), G_TYPE_TLS_CERTIFICATE_GNUTLS, GTlsCertificateGnutlsClass))
#define G_IS_TLS_CERTIFICATE_GNUTLS(inst)        (G_TYPE_CHECK_INSTANCE_TYPE ((inst), G_TYPE_TLS_CERTIFICATE_GNUTLS))
#define G_IS_TLS_CERTIFICATE_GNUTLS_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE ((class), G_TYPE_TLS_CERTIFICATE_GNUTLS))
#define G_TLS_CERTIFICATE_GNUTLS_GET_CLASS(inst) (G_TYPE_INSTANCE_GET_CLASS ((inst), G_TYPE_TLS_CERTIFICATE_GNUTLS, GTlsCertificateGnutlsClass))

typedef struct _GTlsCertificateGnutlsPrivate                   GTlsCertificateGnutlsPrivate;
typedef struct _GTlsCertificateGnutlsClass                     GTlsCertificateGnutlsClass;
typedef struct _GTlsCertificateGnutls                          GTlsCertificateGnutls;

struct _GTlsCertificateGnutlsClass
{
  GTlsCertificateClass parent_class;
};

struct _GTlsCertificateGnutls
{
  GTlsCertificate parent_instance;
  GTlsCertificateGnutlsPrivate *priv;
};

GType g_tls_certificate_gnutls_get_type (void) G_GNUC_CONST;

GTlsCertificate *            g_tls_certificate_gnutls_new             (const gnutls_datum    *datum,
								       GTlsCertificate       *issuer);

const gnutls_x509_crt_t      g_tls_certificate_gnutls_get_cert        (GTlsCertificateGnutls *gnutls);
const gnutls_x509_privkey_t  g_tls_certificate_gnutls_get_key         (GTlsCertificateGnutls *gnutls);

gnutls_x509_crt_t            g_tls_certificate_gnutls_copy_cert       (GTlsCertificateGnutls *gnutls);
gnutls_x509_privkey_t        g_tls_certificate_gnutls_copy_key        (GTlsCertificateGnutls *gnutls);

GTlsCertificateFlags         g_tls_certificate_gnutls_verify_identity (GTlsCertificateGnutls *gnutls,
								       GSocketConnectable    *identity);

GTlsCertificateFlags         g_tls_certificate_gnutls_convert_flags   (guint                  gnutls_flags);


G_END_DECLS

#endif /* __G_TLS_CERTIFICATE_GNUTLS_H___ */
