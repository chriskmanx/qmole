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

#ifndef __GKM_SECRET_ITEM_H__
#define __GKM_SECRET_ITEM_H__

#include <glib-object.h>

#include "gkm-secret-object.h"
#include "gkm-secret-collection.h"

#define GKM_FACTORY_SECRET_ITEM             (gkm_secret_item_get_factory ())

#define GKM_TYPE_SECRET_ITEM               (gkm_secret_item_get_type ())
#define GKM_SECRET_ITEM(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_SECRET_ITEM, GkmSecretItem))
#define GKM_SECRET_ITEM_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_SECRET_ITEM, GkmSecretItemClass))
#define GKM_IS_SECRET_ITEM(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_SECRET_ITEM))
#define GKM_IS_SECRET_ITEM_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_SECRET_ITEM))
#define GKM_SECRET_ITEM_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_SECRET_ITEM, GkmSecretItemClass))

typedef struct _GkmSecretItemClass GkmSecretItemClass;

struct _GkmSecretItemClass {
	GkmSecretObjectClass parent_class;
};

GType                  gkm_secret_item_get_type               (void);

GkmFactory*            gkm_secret_item_get_factory            (void) G_GNUC_CONST;

GkmSecretCollection*   gkm_secret_item_get_collection         (GkmSecretItem *self);

GHashTable*            gkm_secret_item_get_fields             (GkmSecretItem *self);

void                   gkm_secret_item_set_fields             (GkmSecretItem *self,
                                                               GHashTable *fields);

const gchar*           gkm_secret_item_get_schema             (GkmSecretItem *self);

void                   gkm_secret_item_set_schema             (GkmSecretItem *self,
                                                               const gchar *type);

#endif /* __GKM_SECRET_ITEM_H__ */
