/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkd-dbus-session.c - daemon registering environment variables with session

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

#include "gkd-dbus-private.h"
#include "gkd-util.h"

#include <dbus/dbus.h>

#include <string.h>

#define SERVICE_SESSION_MANAGER	"org.gnome.SessionManager"
#define PATH_SESSION_MANAGER	"/org/gnome/SessionManager"
#define IFACE_SESSION_MANAGER   "org.gnome.SessionManager"

void
gkd_dbus_environment_cleanup (DBusConnection *conn)
{
	/* Nothing to do here */
}

static void
on_setenv_reply (DBusPendingCall *pending, void *user_data)
{
	DBusMessage *reply;
	DBusError derr = DBUS_ERROR_INIT;

	reply = dbus_pending_call_steal_reply (pending);
	g_return_if_fail (reply);

	if (dbus_set_error_from_message (&derr, reply)) {
		if (!dbus_error_has_name (&derr, "org.gnome.SessionManager.NotInInitialization"))
			g_message ("couldn't set environment variable in session: %s", derr.message);
		dbus_error_free (&derr);
	}

	dbus_message_unref (reply);
}

static void
setenv_request (DBusConnection *conn, const gchar *env)
{
	DBusPendingCall *pending = NULL;
	DBusError derr = DBUS_ERROR_INIT;
	DBusMessage *msg;
	const gchar *value;
	gchar *name;

	/* Find the value part of the environment variable */
	value = strchr (env, '=');
	if (!value)
		return;

	name = g_strndup (env, value - env);
	++value;

	msg = dbus_message_new_method_call (SERVICE_SESSION_MANAGER,
	                                    PATH_SESSION_MANAGER,
	                                    IFACE_SESSION_MANAGER,
	                                    "Setenv");
	g_return_if_fail (msg);

	if (!dbus_message_append_args (msg, DBUS_TYPE_STRING, &name,
	                               DBUS_TYPE_STRING, &value,
	                               DBUS_TYPE_INVALID))
		g_return_if_reached ();

	g_free (name);
	value = name = NULL;

	/* Send message and get a handle for a reply */
	dbus_connection_send_with_reply (conn, msg, &pending, -1);
	dbus_message_unref (msg);
	if (pending) {
		dbus_pending_call_set_notify (pending, on_setenv_reply, NULL, NULL);
		dbus_pending_call_unref (pending);
	} else {
		g_warning ("couldn't send dbus message: %s",
		           derr.message ? derr.message : "");
		dbus_error_free (&derr);
	}
}

static void
on_watch_environment (gpointer data, gpointer user_data)
{
	DBusConnection *conn = user_data;
	const gchar *env = data;
	setenv_request (conn, env);
}

void
gkd_dbus_environment_init (DBusConnection *conn)
{
	const gchar **envp;

	/*
	 * The list of all environment variables registered by
	 * various components in the daemon.
	 */
	envp = gkd_util_get_environment ();

	for (; *envp; ++envp)
		setenv_request (conn, *envp);

	gkd_util_watch_environment (on_watch_environment, dbus_connection_ref (conn),
	                            (GDestroyNotify)dbus_connection_unref);
}
