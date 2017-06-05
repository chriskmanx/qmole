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

#ifndef __GKM_SECRET_COLLECTION_H__
#define __GKM_SECRET_COLLECTION_H__

#include <glib-object.h>

#include "gkm-secret-object.h"

#define GKM_FACTORY_SECRET_COLLECTION            (gkm_secret_collection_get_factory ())

#define GKM_TYPE_SECRET_COLLECTION               (gkm_secret_collection_get_type ())
#define GKM_SECRET_COLLECTION(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_SECRET_COLLECTION, GkmSecretCollection))
#define GKM_SECRET_COLLECTION_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_SECRET_COLLECTION, GkmSecretCollectionClass))
#define GKM_IS_SECRET_COLLECTION(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_SECRET_COLLECTION))
#define GKM_IS_SECRET_COLLECTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_SECRET_COLLECTION))
#define GKM_SECRET_COLLECTION_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_SECRET_COLLECTION, GkmSecretCollectionClass))

typedef struct _GkmSecretCollectionClass GkmSecretCollectionClass;

struct _GkmSecretCollectionClass {
	GkmSecretObjectClass parent_class;
	GHashTable *identifiers;
};

GType                gkm_secret_collection_get_type        (void);

GkmFactory*          gkm_secret_collection_get_factory     (void) G_GNUC_CONST;

GkmSecretCollection* gkm_secret_collection_find            (GkmSession *session,
                                                            CK_ATTRIBUTE_PTR attr,
                                                            ...) G_GNUC_NULL_TERMINATED;

GkmDataResult        gkm_secret_collection_load            (GkmSecretCollection *self);

void                 gkm_secret_collection_save            (GkmSecretCollection *self,
                                                            GkmTransaction *transaction);

void                 gkm_secret_collection_destroy         (GkmSecretCollection *self,
                                                            GkmTransaction *transaction);

const gchar*         gkm_secret_collection_get_filename    (GkmSecretCollection *self);

void                 gkm_secret_collection_set_filename    (GkmSecretCollection *self,
                                                            const gchar *filename);

GList*               gkm_secret_collection_get_items       (GkmSecretCollection *self);

GkmSecretItem*       gkm_secret_collection_get_item        (GkmSecretCollection *self,
                                                            const gchar *identifier);

gboolean             gkm_secret_collection_has_item        (GkmSecretCollection *self,
                                                            GkmSecretItem *item);

GkmSecretItem*       gkm_secret_collection_new_item        (GkmSecretCollection *self,
                                                            const gchar *identifier);

void                 gkm_secret_collection_remove_item     (GkmSecretCollection *self,
                                                            GkmSecretItem *item);

GkmSecretItem*       gkm_secret_collection_create_item     (GkmSecretCollection *self,
                                                            GkmTransaction *transaction);

void                 gkm_secret_collection_destroy_item    (GkmSecretCollection *self,
                                                            GkmTransaction *transaction,
                                                            GkmSecretItem *item);

void                 gkm_secret_collection_unlocked_clear  (GkmSecretCollection *self);

GkmSecretData*       gkm_secret_collection_unlocked_use    (GkmSecretCollection *self,
                                                            GkmSession *session);

gboolean             gkm_secret_collection_unlocked_have   (GkmSecretCollection *self,
                                                            GkmSession *session);

gint                 gkm_secret_collection_get_lock_idle   (GkmSecretCollection *self);

void                 gkm_secret_collection_set_lock_idle   (GkmSecretCollection *self,
                                                            gint lock_timeout);

gint                 gkm_secret_collection_get_lock_after  (GkmSecretCollection *self);

void                 gkm_secret_collection_set_lock_after  (GkmSecretCollection *self,
                                                            gint lock_timeout);

#endif /* __GKM_SECRET_COLLECTION_H__ */
