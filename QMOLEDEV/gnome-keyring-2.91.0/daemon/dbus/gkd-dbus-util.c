/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkd-dbus.c - hook into dbus, call other bits

   Copyright (C) 2007, 2009, Stefan Walter

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

#include "config.h"

#include "gkd-dbus-util.h"
#include "gkd-secret-types.h"

#include "egg/egg-error.h"

#include <string.h>

GType
gkd_dbus_connection_get_boxed_type (void)
{
	static GType type = 0;
	if (!type)
		type = g_boxed_type_register_static ("GkdDBusConnection",
		                                     (GBoxedCopyFunc)dbus_connection_ref,
		                                     (GBoxedFreeFunc)dbus_connection_unref);
	return type;
}

gboolean
gkd_dbus_interface_match (const gchar *interface, const gchar *match)
{
	g_return_val_if_fail (interface, FALSE);

	/* Null or zero length matches anything */
	if (!match || !match[0])
		return TRUE;

	return strcmp (interface, match) == 0;
}

DBusMessage*
gkd_dbus_introspect_handle (DBusMessage *message, const gchar *type)
{
	GError *error = NULL;
	DBusMessage *reply;
	gchar *filename;
	gchar *data;

	g_return_val_if_fail (message, NULL);
	g_return_val_if_fail (type, NULL);

	if (dbus_message_is_method_call (message, DBUS_INTERFACE_INTROSPECTABLE, "Introspect") &&
	    dbus_message_get_args (message, NULL, DBUS_TYPE_INVALID)) {

		filename = g_strconcat (INTROSPECTDIR, G_DIR_SEPARATOR_S, "introspect-", type, ".xml", NULL);
		g_file_get_contents (filename, &data, NULL, &error);
		g_free (filename);

		if (error != NULL) {
			g_warning ("couldn't load introspect data file: %s: %s",
			           filename, egg_error_message (error));
			g_clear_error (&error);
			return NULL;
		}

		reply = dbus_message_new_method_return (message);
		if (!dbus_message_append_args (reply, DBUS_TYPE_STRING, &data, DBUS_TYPE_INVALID))
			g_return_val_if_reached (NULL);
		g_free (data);
		return reply;
	}

	return NULL;
}
