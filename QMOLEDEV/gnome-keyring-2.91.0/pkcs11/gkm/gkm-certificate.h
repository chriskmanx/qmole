/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef __GKM_CERTIFICATE_H__
#define __GKM_CERTIFICATE_H__

#include <glib-object.h>

#include "gkm-object.h"
#include "gkm-types.h"

#define GKM_FACTORY_CERTIFICATE            (gkm_certificate_get_factory ())

#define GKM_TYPE_CERTIFICATE               (gkm_certificate_get_type ())
#define GKM_CERTIFICATE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_CERTIFICATE, GkmCertificate))
#define GKM_CERTIFICATE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_CERTIFICATE, GkmCertificateClass))
#define GKM_IS_CERTIFICATE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_CERTIFICATE))
#define GKM_IS_CERTIFICATE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_CERTIFICATE))
#define GKM_CERTIFICATE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_CERTIFICATE, GkmCertificateClass))

typedef struct _GkmCertificateClass GkmCertificateClass;
typedef struct _GkmCertificatePrivate GkmCertificatePrivate;

struct _GkmCertificate {
	GkmObject parent;
	GkmCertificatePrivate *pv;
};

struct _GkmCertificateClass {
	GkmObjectClass parent_class;
};

GType                      gkm_certificate_get_type               (void);

GkmFactory*                gkm_certificate_get_factory            (void);

gboolean                   gkm_certificate_calc_category          (GkmCertificate *self,
                                                                   GkmSession *session,
                                                                   CK_ULONG* category);

GkmCertificateKey*         gkm_certificate_get_public_key         (GkmCertificate *self);

const guchar*              gkm_certificate_get_extension          (GkmCertificate *self,
                                                                   GQuark oid,
                                                                   gsize *n_extension,
                                                                   gboolean *critical);

const gchar*               gkm_certificate_get_label              (GkmCertificate *self);

void                       gkm_certificate_set_label              (GkmCertificate *self,
                                                                   const gchar *label);

guchar*                    gkm_certificate_hash                   (GkmCertificate *self,
                                                                   int hash_algo,
                                                                   gsize *n_hash);


#endif /* __GKM_CERTIFICATE_H__ */
