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

#ifndef __GKM_USER_STORAGE_H__
#define __GKM_USER_STORAGE_H__

#include <glib-object.h>

#include "gkm/gkm-manager.h"
#include "gkm/gkm-secret.h"
#include "gkm/gkm-store.h"
#include "gkm/gkm-transaction.h"

#define GKM_TYPE_USER_STORAGE               (gkm_user_storage_get_type ())
#define GKM_USER_STORAGE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_USER_STORAGE, GkmUserStorage))
#define GKM_USER_STORAGE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_USER_STORAGE, GkmUserStorageClass))
#define GKM_IS_USER_STORAGE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_USER_STORAGE))
#define GKM_IS_USER_STORAGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_USER_STORAGE))
#define GKM_USER_STORAGE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_USER_STORAGE, GkmUserStorageClass))

typedef struct _GkmUserStorage GkmUserStorage;
typedef struct _GkmUserStorageClass GkmUserStorageClass;

struct _GkmUserStorageClass {
	GkmStoreClass parent_class;
};

GType                       gkm_user_storage_get_type               (void);

GkmUserStorage*             gkm_user_storage_new                    (GkmModule *module,
                                                                     const gchar *directory);

GkmManager*                 gkm_user_storage_get_manager            (GkmUserStorage *self);

const gchar*                gkm_user_storage_get_directory          (GkmUserStorage *self);

GkmSecret*                  gkm_user_storage_get_login              (GkmUserStorage *self);

gulong                      gkm_user_storage_token_flags            (GkmUserStorage *self);

CK_RV                       gkm_user_storage_refresh                (GkmUserStorage *self);

void                        gkm_user_storage_create                 (GkmUserStorage *self,
                                                                     GkmTransaction *transaction,
                                                                     GkmObject *object);

void                        gkm_user_storage_destroy                (GkmUserStorage *self,
                                                                     GkmTransaction *transaction,
                                                                     GkmObject *object);

void                        gkm_user_storage_relock                 (GkmUserStorage *self,
                                                                     GkmTransaction *transaction,
                                                                     GkmSecret *old_login,
                                                                     GkmSecret *new_login);

CK_RV                       gkm_user_storage_unlock                 (GkmUserStorage *self,
                                                                     GkmSecret *login);

CK_RV                       gkm_user_storage_lock                   (GkmUserStorage *self);

#endif /* __GKM_USER_STORAGE_H__ */
