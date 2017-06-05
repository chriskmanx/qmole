/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
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
 */

#ifndef __GKM_CERTIFICATE_KEY_H__
#define __GKM_CERTIFICATE_KEY_H__

#include <glib-object.h>

#include "gkm-public-xsa-key.h"
#include "gkm-types.h"

#define GKM_TYPE_CERTIFICATE_KEY               (gkm_certificate_key_get_type ())
#define GKM_CERTIFICATE_KEY(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_CERTIFICATE_KEY, GkmCertificateKey))
#define GKM_CERTIFICATE_KEY_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_CERTIFICATE_KEY, GkmCertificateKeyClass))
#define GKM_IS_CERTIFICATE_KEY(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_CERTIFICATE_KEY))
#define GKM_IS_CERTIFICATE_KEY_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_CERTIFICATE_KEY))
#define GKM_CERTIFICATE_KEY_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_CERTIFICATE_KEY, GkmCertificateKeyClass))

typedef struct _GkmCertificateKeyClass GkmCertificateKeyClass;
typedef struct _GkmCertificateKeyPrivate GkmCertificateKeyPrivate;

struct _GkmCertificateKey {
	GkmPublicXsaKey parent;
	GkmCertificateKeyPrivate *pv;
};

struct _GkmCertificateKeyClass {
	GkmPublicXsaKeyClass parent_class;
};

GType               gkm_certificate_key_get_type               (void);

GkmCertificateKey*  gkm_certificate_key_new                    (GkmModule *module,
                                                                GkmManager *manager,
                                                                GkmCertificate *cert);

GkmCertificate*     gkm_certificate_key_get_certificate        (GkmCertificateKey *self);

#endif /* __GKM_CERTIFICATE_KEY_H__ */
