/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
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

#ifndef __GKM_TRUST_H__
#define __GKM_TRUST_H__

#include <glib-object.h>

#include "gkm-object.h"

typedef enum _GkmTrustLevel {
	GKM_TRUST_UNKNOWN = 0,
	GKM_TRUST_DISTRUSTED = 1,
	GKM_TRUST_TRUSTED = 2,
	GKM_TRUST_ANCHOR = 3,
} GkmTrustLevel;

#define GKM_FACTORY_TRUST            (gkm_trust_get_factory ())
#define GKM_TYPE_TRUST               (gkm_trust_get_type ())
#define GKM_TRUST(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_TRUST, GkmTrust))
#define GKM_TRUST_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_TRUST, GkmTrustClass))
#define GKM_IS_TRUST(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_TRUST))
#define GKM_IS_TRUST_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_TRUST))
#define GKM_TRUST_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_TRUST, GkmTrustClass))

typedef struct _GkmTrustClass GkmTrustClass;
typedef struct _GkmTrustPrivate GkmTrustPrivate;

struct _GkmTrust {
	GkmObject parent;
	GkmTrustPrivate *pv;
};

struct _GkmTrustClass {
	GkmObjectClass parent_class;

	GkmTrustLevel (*get_trust_level) (GkmTrust *self, const gchar *purpose);
};

GType                 gkm_trust_get_type               (void);

GkmTrustLevel         gkm_trust_get_level_for_purpose  (GkmTrust *self, const gchar *purpose);

#endif /* __GKM_TRUST_H__ */
