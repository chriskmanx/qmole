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

#ifndef __GKD_SECRET_SERVICE_H__
#define __GKD_SECRET_SERVICE_H__

#include "gkd-secret-types.h"

#include "gck/gck.h"

#include <dbus/dbus.h>

#include <glib-object.h>

#define GKD_SECRET_TYPE_SERVICE               (gkd_secret_service_get_type ())
#define GKD_SECRET_SERVICE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKD_SECRET_TYPE_SERVICE, GkdSecretService))
#define GKD_SECRET_SERVICE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKD_SECRET_TYPE_SERVICE, GkdSecretServiceClass))
#define GKD_SECRET_IS_SERVICE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKD_SECRET_TYPE_SERVICE))
#define GKD_SECRET_IS_SERVICE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKD_SECRET_TYPE_SERVICE))
#define GKD_SECRET_SERVICE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKD_SECRET_TYPE_SERVICE, GkdSecretServiceClass))

typedef struct _GkdSecretServiceClass GkdSecretServiceClass;

struct _GkdSecretServiceClass {
	GObjectClass parent_class;
};

GType                   gkd_secret_service_get_type                (void);

DBusConnection*         gkd_secret_service_get_connection          (GkdSecretService *self);

GckSlot*                gkd_secret_service_get_pkcs11_slot         (GkdSecretService *self);

GckSession*             gkd_secret_service_get_pkcs11_session      (GkdSecretService *self,
                                                                    const gchar *caller);

GkdSecretObjects*       gkd_secret_service_get_objects             (GkdSecretService *self);

GkdSecretIndex*         gkd_secret_service_get_index               (GkdSecretService *self);

GkdSecretSession*       gkd_secret_service_lookup_session          (GkdSecretService *self,
                                                                    const gchar *path,
                                                                    const gchar *caller);

void                    gkd_secret_service_close_session           (GkdSecretService *self,
                                                                    GkdSecretSession *sess);

void                    gkd_secret_service_send                    (GkdSecretService *self,
                                                                    DBusMessage *message);

#endif /* ___SECRET_SERVICE_H__ */
