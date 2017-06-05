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

#ifndef __GKM_GNOME2_STORAGE_H__
#define __GKM_GNOME2_STORAGE_H__

#include <glib-object.h>

#include "gkm/gkm-manager.h"
#include "gkm/gkm-secret.h"
#include "gkm/gkm-store.h"
#include "gkm/gkm-transaction.h"

#define GKM_TYPE_GNOME2_STORAGE               (gkm_gnome2_storage_get_type ())
#define GKM_GNOME2_STORAGE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_GNOME2_STORAGE, GkmGnome2Storage))
#define GKM_GNOME2_STORAGE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_GNOME2_STORAGE, GkmGnome2StorageClass))
#define GKM_IS_GNOME2_STORAGE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_GNOME2_STORAGE))
#define GKM_IS_GNOME2_STORAGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_GNOME2_STORAGE))
#define GKM_GNOME2_STORAGE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_GNOME2_STORAGE, GkmGnome2StorageClass))

typedef struct _GkmGnome2Storage GkmGnome2Storage;
typedef struct _GkmGnome2StorageClass GkmGnome2StorageClass;

struct _GkmGnome2StorageClass {
	GkmStoreClass parent_class;
};

GType                       gkm_gnome2_storage_get_type               (void);

GkmGnome2Storage*             gkm_gnome2_storage_new                    (GkmModule *module,
                                                                     const gchar *directory);

GkmManager*                 gkm_gnome2_storage_get_manager            (GkmGnome2Storage *self);

const gchar*                gkm_gnome2_storage_get_directory          (GkmGnome2Storage *self);

GkmSecret*                  gkm_gnome2_storage_get_login              (GkmGnome2Storage *self);

gulong                      gkm_gnome2_storage_token_flags            (GkmGnome2Storage *self);

CK_RV                       gkm_gnome2_storage_refresh                (GkmGnome2Storage *self);

void                        gkm_gnome2_storage_create                 (GkmGnome2Storage *self,
                                                                     GkmTransaction *transaction,
                                                                     GkmObject *object);

void                        gkm_gnome2_storage_destroy                (GkmGnome2Storage *self,
                                                                     GkmTransaction *transaction,
                                                                     GkmObject *object);

void                        gkm_gnome2_storage_relock                 (GkmGnome2Storage *self,
                                                                     GkmTransaction *transaction,
                                                                     GkmSecret *old_login,
                                                                     GkmSecret *new_login);

CK_RV                       gkm_gnome2_storage_unlock                 (GkmGnome2Storage *self,
                                                                     GkmSecret *login);

CK_RV                       gkm_gnome2_storage_lock                   (GkmGnome2Storage *self);

#endif /* __GKM_GNOME2_STORAGE_H__ */
