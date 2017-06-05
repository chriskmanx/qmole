/*
 * gnome-trustring
 *
 * Copyright (C) 2008 Stefan Walter
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
 */

#ifndef __GKM_ROOTS_TRUST_H__
#define __GKM_ROOTS_TRUST_H__

#include <glib-object.h>

#include "gkm/gkm-trust.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-types.h"

#define GKM_ROOTS_TYPE_TRUST               (gkm_roots_trust_get_type ())
#define GKM_ROOTS_TRUST(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_ROOTS_TYPE_TRUST, GkmRootsTrust))
#define GKM_ROOTS_TRUST_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_ROOTS_TYPE_TRUST, GkmRootsTrustClass))
#define GKM_ROOTS_IS_TRUST(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_ROOTS_TYPE_TRUST))
#define GKM_ROOTS_IS_TRUST_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_ROOTS_TYPE_TRUST))
#define GKM_ROOTS_TRUST_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_ROOTS_TYPE_TRUST, GkmRootsTrustClass))

typedef struct _GkmRootsTrust GkmRootsTrust;
typedef struct _GkmRootsTrustClass GkmRootsTrustClass;
typedef struct _GkmRootsTrustPrivate GkmRootsTrustPrivate;

struct _GkmRootsTrust {
	GkmTrust parent;
	GkmRootsTrustPrivate *pv;
};

struct _GkmRootsTrustClass {
	GkmTrustClass parent_class;
};

GType                 gkm_roots_trust_get_type               (void);

GkmRootsTrust*        gkm_roots_trust_new                    (GkmModule *module,
                                                              GkmManager *manager,
                                                              GkmCertificate *cert);

GkmCertificate*       gkm_roots_trust_get_certificate        (GkmRootsTrust *self);

#endif /* __GKM_ROOTS_TRUST_H__ */
