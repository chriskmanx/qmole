/*
 * gnome-trustring
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

#ifndef __GKM_CERTIFICATE_TRUST_H__
#define __GKM_CERTIFICATE_TRUST_H__

#include <glib-object.h>

#include "gkm-object.h"
#include "gkm-types.h"

#define GKM_TYPE_CERTIFICATE_TRUST               (gkm_certificate_trust_get_type ())
#define GKM_CERTIFICATE_TRUST(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_CERTIFICATE_TRUST, GkmCertificateTrust))
#define GKM_CERTIFICATE_TRUST_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_CERTIFICATE_TRUST, GkmCertificateTrustClass))
#define GKM_IS_CERTIFICATE_TRUST(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_CERTIFICATE_TRUST))
#define GKM_IS_CERTIFICATE_TRUST_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_CERTIFICATE_TRUST))
#define GKM_CERTIFICATE_TRUST_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_CERTIFICATE_TRUST, GkmCertificateTrustClass))

typedef struct _GkmCertificateTrustClass GkmCertificateTrustClass;
typedef struct _GkmCertificateTrustPrivate GkmCertificateTrustPrivate;

struct _GkmCertificateTrust {
	GkmObject parent;
	GkmCertificateTrustPrivate *pv;
};

struct _GkmCertificateTrustClass {
	GkmObjectClass parent_class;
};

GType                 gkm_certificate_trust_get_type               (void);

GkmCertificateTrust*  gkm_certificate_trust_new                    (GkmModule *module,
                                                                    GkmManager *manager,
                                                                    GkmCertificate *cert);

GkmCertificate*       gkm_certificate_trust_get_certificate        (GkmCertificateTrust *self);

#endif /* __GKM_CERTIFICATE_TRUST_H__ */
