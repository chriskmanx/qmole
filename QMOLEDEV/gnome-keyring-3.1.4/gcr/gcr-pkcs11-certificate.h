/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Collabora Ltd
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#if !defined (__GCR_H_INSIDE__) && !defined (GCR_COMPILATION)
#error "Only <gcr/gcr.h> can be included directly."
#endif

#ifndef __GCR_PKCS11_CERTIFICATE_H__
#define __GCR_PKCS11_CERTIFICATE_H__

#include "gcr-types.h"
#include "gcr-certificate.h"

#include <glib-object.h>

G_BEGIN_DECLS

#define GCR_TYPE_PKCS11_CERTIFICATE               (gcr_pkcs11_certificate_get_type ())
#define GCR_PKCS11_CERTIFICATE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_PKCS11_CERTIFICATE, GcrPkcs11Certificate))
#define GCR_PKCS11_CERTIFICATE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_PKCS11_CERTIFICATE, GcrPkcs11CertificateClass))
#define GCR_IS_PKCS11_CERTIFICATE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_PKCS11_CERTIFICATE))
#define GCR_IS_PKCS11_CERTIFICATE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_PKCS11_CERTIFICATE))
#define GCR_PKCS11_CERTIFICATE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_PKCS11_CERTIFICATE, GcrPkcs11CertificateClass))

typedef struct _GcrPkcs11Certificate GcrPkcs11Certificate;
typedef struct _GcrPkcs11CertificateClass GcrPkcs11CertificateClass;
typedef struct _GcrPkcs11CertificatePrivate GcrPkcs11CertificatePrivate;

struct _GcrPkcs11Certificate {
	GckObject parent;

	/*< private >*/
	GcrPkcs11CertificatePrivate *pv;
};

struct _GcrPkcs11CertificateClass {
	/*< private >*/
	GckObjectClass parent_class;
};

GType                   gcr_pkcs11_certificate_get_type               (void);

GckAttributes*          gcr_pkcs11_certificate_get_attributes         (GcrPkcs11Certificate *self);

GcrCertificate*         gcr_pkcs11_certificate_lookup_issuer          (GcrCertificate *certificate,
                                                                       GCancellable *cancellable,
                                                                       GError **error);

void                    gcr_pkcs11_certificate_lookup_issuer_async    (GcrCertificate *certificate,
                                                                       GCancellable *cancellable,
                                                                       GAsyncReadyCallback callback,
                                                                       gpointer user_data);

GcrCertificate*         gcr_pkcs11_certificate_lookup_issuer_finish   (GAsyncResult *result,
                                                                       GError **error);

G_END_DECLS

#endif /* __GCR_PKCS11_CERTIFICATE_H__ */
