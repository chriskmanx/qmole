/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkd-dbus.h - header for dbus component

   Copyright (C) 2009, Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#ifndef GKD_DBUS_H
#define GKD_DBUS_H

#include <glib.h>
#include <glib-object.h>
#include <dbus/dbus.h>

#define       GKD_DBUS_TYPE_CONNECTION                    (gkd_dbus_connection_get_boxed_type ())

GType         gkd_dbus_connection_get_boxed_type          (void) G_GNUC_CONST;

gboolean      gkd_dbus_interface_match                    (const gchar *interface, const gchar *match);

DBusMessage*  gkd_dbus_introspect_handle                  (DBusMessage *message, const gchar *type);

#endif /* GKD_DBUS_H */
