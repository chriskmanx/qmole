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

#include "config.h"

#include <glib.h>

#include "gkd-secret-error.h"

DBusMessage*
gkd_secret_error_no_such_object (DBusMessage *message)
{
	g_return_val_if_fail (message, NULL);
	return dbus_message_new_error_printf (message, SECRET_ERROR_NO_SUCH_OBJECT,
	                                      "The '%s' object does not exist", dbus_message_get_path (message));
}

DBusMessage*
gkd_secret_error_to_reply (DBusMessage *message, DBusError *derr)
{
	DBusMessage *reply;

	g_return_val_if_fail (message, NULL);
	g_return_val_if_fail (derr, NULL);
	g_return_val_if_fail (dbus_error_is_set (derr), NULL);

	reply = dbus_message_new_error (message, derr->name, derr->message);
	dbus_error_free (derr);
	return reply;
}
