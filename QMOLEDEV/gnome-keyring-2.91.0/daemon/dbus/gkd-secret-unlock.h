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

#ifndef __GKD_SECRET_UNLOCK_H__
#define __GKD_SECRET_UNLOCK_H__

#include <glib-object.h>

#include "gkd-secret-types.h"

#define GKD_SECRET_TYPE_UNLOCK               (gkd_secret_unlock_get_type ())
#define GKD_SECRET_UNLOCK(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKD_SECRET_TYPE_UNLOCK, GkdSecretUnlock))
#define GKD_SECRET_UNLOCK_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKD_SECRET_TYPE_UNLOCK, GkdSecretUnlockClass))
#define GKD_SECRET_IS_UNLOCK(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKD_SECRET_TYPE_UNLOCK))
#define GKD_SECRET_IS_UNLOCK_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKD_SECRET_TYPE_UNLOCK))
#define GKD_SECRET_UNLOCK_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKD_SECRET_TYPE_UNLOCK, GkdSecretUnlockClass))

typedef struct _GkdSecretUnlockClass GkdSecretUnlockClass;

struct _GkdSecretUnlockClass {
	GObjectClass parent_class;
};

GType               gkd_secret_unlock_get_type                (void);

GkdSecretUnlock*    gkd_secret_unlock_new                     (GkdSecretService *service,
                                                               const gchar *caller);

void                gkd_secret_unlock_queue                   (GkdSecretUnlock *self,
                                                               const gchar *objpath);

gboolean            gkd_secret_unlock_have_queued             (GkdSecretUnlock *self);

gchar**             gkd_secret_unlock_get_results             (GkdSecretUnlock *self,
                                                               gint *n_results);

void                gkd_secret_unlock_reset_results           (GkdSecretUnlock *self);

gboolean            gkd_secret_unlock_with_secret             (GckObject *collection,
                                                               GkdSecretSecret *master,
                                                               DBusError *derr);

gboolean            gkd_secret_unlock_with_password           (GckObject *collection,
                                                               const guchar *password,
                                                               gsize n_password,
                                                               DBusError *derr);

#endif /* __GKD_SECRET_UNLOCK_H__ */
