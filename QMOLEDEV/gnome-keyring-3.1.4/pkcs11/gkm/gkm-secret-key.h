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

#ifndef __GKM_SECRET_KEY_H__
#define __GKM_SECRET_KEY_H__

#include <glib-object.h>

#include "gkm-object.h"
#include "gkm-types.h"

#define GKM_TYPE_SECRET_KEY               (gkm_secret_key_get_type ())
#define GKM_SECRET_KEY(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_SECRET_KEY, GkmSecretKey))
#define GKM_SECRET_KEY_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_SECRET_KEY, GkmSecretKeyClass))
#define GKM_IS_SECRET_KEY(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_SECRET_KEY))
#define GKM_IS_SECRET_KEY_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_SECRET_KEY))
#define GKM_SECRET_KEY_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_SECRET_KEY, GkmSecretKeyClass))

typedef struct _GkmSecretKeyClass GkmSecretKeyClass;
typedef struct _GkmSecretKeyPrivate GkmSecretKeyPrivate;

struct _GkmSecretKey {
	GkmObject parent;
	GkmSecretKeyPrivate *pv;
};

struct _GkmSecretKeyClass {
	GkmObjectClass parent_class;

	/* virtual methods  --------------------------------------------------------- */

	gconstpointer (*get_key_value) (GkmSecretKey *key, gsize *n_value);
};

GType                     gkm_secret_key_get_type           (void);

gconstpointer             gkm_secret_key_get_key_value      (GkmSecretKey *self,
                                                             gsize *n_value);

#endif /* __GKM_SECRET_KEY_H__ */
