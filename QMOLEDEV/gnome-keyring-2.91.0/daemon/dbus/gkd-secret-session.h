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

#ifndef __GKD_SECRET_SESSION_H__
#define __GKD_SECRET_SESSION_H__

#include <glib-object.h>

#include "gkd-secret-types.h"

#define GKD_SECRET_TYPE_SESSION               (gkd_secret_session_get_type ())
#define GKD_SECRET_SESSION(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKD_SECRET_TYPE_SESSION, GkdSecretSession))
#define GKD_SECRET_SESSION_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKD_SECRET_TYPE_SESSION, GkdSecretSessionClass))
#define GKD_SECRET_IS_SESSION(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKD_SECRET_TYPE_SESSION))
#define GKD_SECRET_IS_SESSION_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKD_SECRET_TYPE_SESSION))
#define GKD_SECRET_SESSION_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKD_SECRET_TYPE_SESSION, GkdSecretSessionClass))

typedef struct _GkdSecretSessionClass GkdSecretSessionClass;

struct _GkdSecretSessionClass {
	GObjectClass parent_class;
};

GType               gkd_secret_session_get_type                (void);

GkdSecretSession*   gkd_secret_session_new                     (GkdSecretService *service,
                                                                const gchar *caller);

gpointer            gkd_secret_session_begin                   (GkdSecretSession *self,
                                                                const gchar *group,
                                                                gsize *n_public);

gboolean            gkd_secret_session_complete                (GkdSecretSession *self,
                                                                gconstpointer peer,
                                                                gsize n_peer);

const gchar*        gkd_secret_session_get_caller              (GkdSecretSession *self);

const gchar*        gkd_secret_session_get_caller_executable   (GkdSecretSession *self);

GckSession*         gkd_secret_session_get_pkcs11_session      (GkdSecretSession *self);

GkdSecretSecret*    gkd_secret_session_get_item_secret         (GkdSecretSession *self,
                                                                GckObject *item,
                                                                DBusError *derr);

gboolean            gkd_secret_session_set_item_secret         (GkdSecretSession *self,
                                                                GckObject *item,
                                                                GkdSecretSecret *secret,
                                                                DBusError *derr);

GckObject*          gkd_secret_session_create_credential       (GkdSecretSession *self,
                                                                GckSession *session,
                                                                GckAttributes *attrs,
                                                                GkdSecretSecret *secret,
                                                                DBusError *derr);

DBusMessage*        gkd_secret_session_handle_open             (GkdSecretSession *self,
                                                                DBusMessage *message);

#endif /* __GKD_SECRET_SESSION_H__ */
