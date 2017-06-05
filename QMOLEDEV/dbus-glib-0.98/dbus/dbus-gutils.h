/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gutils.h Utils shared between convenience lib and installed lib
 *
 * Copyright (C) 2003  Red Hat, Inc.
 *
 * Licensed under the Academic Free License version 2.1
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#ifndef DBUS_GLIB_UTILS_H
#define DBUS_GLIB_UTILS_H

#include <dbus/dbus.h>
#include <glib-object.h>

G_BEGIN_DECLS

char      **_dbus_gutils_split_path     (const char *path);

char       *_dbus_gutils_wincaps_to_uscore (const char *uscore);

/* These munge the pointer to enforce that a plain cast won't work,
 * accessor functions must be used; i.e. to ensure the ABI
 * reflects our encapsulation.
 */
#define _DBUS_POINTER_SHIFT(p)   ((void*) (((char*)p) + sizeof (void*)))
#define _DBUS_POINTER_UNSHIFT(p) ((void*) (((char*)p) - sizeof (void*)))

#define DBUS_CONNECTION_FROM_G_CONNECTION(x)     ((DBusConnection*) _DBUS_POINTER_UNSHIFT(x))
#define DBUS_MESSAGE_FROM_G_MESSAGE(x)           ((DBusMessage*) _DBUS_POINTER_UNSHIFT(x))
#define DBUS_PENDING_CALL_FROM_G_PENDING_CALL(x) ((DBusPendingCall*) _DBUS_POINTER_UNSHIFT(x))

#define DBUS_G_CONNECTION_FROM_CONNECTION(x)     ((DBusGConnection*) _DBUS_POINTER_SHIFT(x))
#define DBUS_G_MESSAGE_FROM_MESSAGE(x)           ((DBusGMessage*) _DBUS_POINTER_SHIFT(x))
#define DBUS_G_PENDING_CALL_FROM_PENDING_CALL(x) ((DBusGPendingCall*) _DBUS_POINTER_SHIFT(x))

G_END_DECLS

#endif /* DBUS_GLIB_UTILS_H */
