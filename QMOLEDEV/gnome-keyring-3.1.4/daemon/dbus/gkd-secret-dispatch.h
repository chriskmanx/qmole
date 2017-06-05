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

#ifndef __GKD_SECRET_DISPATCH_H__
#define __GKD_SECRET_DISPATCH_H__

#include "gkd-secret-types.h"

#include <dbus/dbus.h>

#include <glib-object.h>

G_BEGIN_DECLS

#define GKD_SECRET_TYPE_DISPATCH                 (gkd_secret_dispatch_get_type())
#define GKD_SECRET_DISPATCH(obj)                 (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKD_SECRET_TYPE_DISPATCH, GkdSecretDispatch))
#define GKD_SECRET_IS_DISPATCH(obj)              (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKD_SECRET_TYPE_DISPATCH))
#define GKD_SECRET_DISPATCH_GET_INTERFACE(inst)  (G_TYPE_INSTANCE_GET_INTERFACE ((inst), GKD_SECRET_TYPE_DISPATCH, GkdSecretDispatchIface))

typedef struct _GkdSecretDispatchIface GkdSecretDispatchIface;

struct _GkdSecretDispatchIface {
	GTypeInterface parent;

	const gchar* (*get_path) (GkdSecretDispatch *self);
	DBusMessage* (*dispatch_message) (GkdSecretDispatch *self, DBusMessage *message);
};

GType                  gkd_secret_dispatch_get_type                          (void) G_GNUC_CONST;

const gchar*           gkd_secret_dispatch_get_object_path                   (GkdSecretDispatch *self);

DBusMessage*           gkd_secret_dispatch_message                           (GkdSecretDispatch *self,
                                                                              DBusMessage *message);

G_END_DECLS

#endif /* __GKD_SECRET_DISPATCH_H__ */
