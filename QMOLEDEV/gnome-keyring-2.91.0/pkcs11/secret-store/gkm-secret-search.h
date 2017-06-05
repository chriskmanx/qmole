/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#ifndef __GKM_SECRET_SEARCH_H__
#define __GKM_SECRET_SEARCH_H__

#include "gkm-secret-types.h"

#include "gkm/gkm-object.h"

#include <glib-object.h>

#define GKM_FACTORY_SECRET_SEARCH            (gkm_secret_search_get_factory ())

#define GKM_TYPE_SECRET_SEARCH               (gkm_secret_search_get_type ())
#define GKM_SECRET_SEARCH(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_SECRET_SEARCH, GkmSecretSearch))
#define GKM_SECRET_SEARCH_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_SECRET_SEARCH, GkmSecretSearchClass))
#define GKM_IS_SECRET_SEARCH(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_SECRET_SEARCH))
#define GKM_IS_SECRET_SEARCH_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_SECRET_SEARCH))
#define GKM_SECRET_SEARCH_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_SECRET_SEARCH, GkmSecretSearchClass))

typedef struct _GkmSecretSearchClass GkmSecretSearchClass;

struct _GkmSecretSearchClass {
	GkmObjectClass parent_class;
};

GType                gkm_secret_search_get_type        (void) G_GNUC_CONST;

GkmFactory*          gkm_secret_search_get_factory     (void) G_GNUC_CONST;

GHashTable*          gkm_secret_search_get_fields      (GkmSecretSearch *self);

const gchar*      gkm_secret_search_get_collection_id  (GkmSecretSearch *self);

#endif /* __GKM_SECRET_SEARCH_H__ */
