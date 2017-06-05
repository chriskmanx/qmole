/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkd-dbus-session.c - daemon registering with the session

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
#include "gkd-main.h"

#include <dbus/dbus.h>

#include <string.h>

#define SERVICE_SESSION_MANAGER	"org.gnome.SessionManager"
#define PATH_SESSION_MANAGER	"/org/gnome/SessionManager"
#define IFACE_SESSION_MANAGER   "org.gnome.SessionManager"
#define IFACE_SESSION_CLIENT    "org.gnome.SessionManager.Client"
#define IFACE_SESSION_PRIVATE   "org.gnome.SessionManager.ClientPrivate"

static gchar *client_session_path = NULL;
static gchar *client_session_rule = NULL;

static void
send_end_session_response (DBusConnection *conn)
{
	DBusMessageIter args;
	DBusMessage *msg;
	DBusMessage *reply;
	DBusError derr = { 0 };
	const gchar *reason = "";
	dbus_bool_t is_ok = TRUE;

	g_return_if_fail (client_session_path);

	msg = dbus_message_new_method_call (SERVICE_SESSION_MANAGER,
	                                    client_session_path,
	                                    IFACE_SESSION_PRIVATE,
	                                    "EndSessionResponse");
	g_return_if_fail (msg);

	dbus_message_iter_init_append (msg, &args);
	if (!dbus_message_iter_append_basic (&args, DBUS_TYPE_BOOLEAN, &is_ok) ||
	    !dbus_message_iter_append_basic (&args, DBUS_TYPE_STRING, &reason))
		g_return_if_reached ();

	reply = dbus_connection_send_with_reply_and_block (conn, msg, 1000, &derr);
	dbus_message_unref (msg);

	if (!reply) {
		g_message ("dbus failure responding to ending session: %s", derr.message);
		return;
	}

	dbus_message_unref (reply);
}

static void
unregister_daemon_in_session (DBusConnection *conn)
{
	DBusMessageIter args;
	DBusMessage *msg;

	if (client_session_rule) {
		dbus_bus_remove_match (conn, client_session_rule, NULL);
		g_free (client_session_rule);
		client_session_rule = NULL;
	}

	if (!client_session_path)
		return;

	msg = dbus_message_new_method_call (SERVICE_SESSION_MANAGER,
	                                    PATH_SESSION_MANAGER,
	                                    IFACE_SESSION_MANAGER,
	                                    "UnregisterClient");
	g_return_if_fail (msg);

	dbus_message_iter_init_append (msg, &args);
	if (!dbus_message_iter_append_basic (&args, DBUS_TYPE_OBJECT_PATH, &client_session_path))
		g_return_if_reached ();

	dbus_message_set_no_reply (msg, TRUE);
	dbus_connection_send (conn, msg, NULL);
	dbus_connection_flush (conn);
	dbus_message_unref (msg);

	g_free (client_session_path);
	client_session_path = NULL;
}

static DBusHandlerResult
signal_filter (DBusConnection *conn, DBusMessage *msg, void *user_data)
{
	/* Quit the daemon when the session is over */
	if (dbus_message_is_signal (msg, IFACE_SESSION_PRIVATE, "Stop")) {
		unregister_daemon_in_session (conn);
		gkd_main_quit ();
		return DBUS_HANDLER_RESULT_HANDLED;
	} else if (dbus_message_is_signal (msg, IFACE_SESSION_PRIVATE, "QueryEndSession")) {
		send_end_session_response (conn);
		return DBUS_HANDLER_RESULT_HANDLED;
	} else if (dbus_message_is_signal (msg, IFACE_SESSION_PRIVATE, "EndSession")) {
		send_end_session_response (conn);
		unregister_daemon_in_session (conn);
		gkd_main_quit ();
		return DBUS_HANDLER_RESULT_HANDLED;
	} else if (dbus_message_is_signal (msg, DBUS_INTERFACE_LOCAL, "Disconnected")) {
		gkd_main_quit ();
		return DBUS_HANDLER_RESULT_HANDLED;
	}

	return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

void
gkd_dbus_session_cleanup (DBusConnection *conn)
{
	g_free (client_session_path);
	client_session_path = NULL;

	g_free (client_session_rule);
	client_session_rule = NULL;
}

/*
 * Here we register our desktop autostart id gnome-session style
 * session manager via DBus.
 */
void
gkd_dbus_session_init (DBusConnection *conn)
{
	DBusMessageIter args;
	DBusMessage *msg;
	DBusMessage *reply;
	DBusError derr = { 0 };
	const gchar *app_id = "gnome-keyring-daemon";
	const gchar *client_id;

	client_id = g_getenv ("DESKTOP_AUTOSTART_ID");
	if (!client_id)
		return;

	msg = dbus_message_new_method_call (SERVICE_SESSION_MANAGER,
	                                    PATH_SESSION_MANAGER,
	                                    IFACE_SESSION_MANAGER,
	                                    "RegisterClient");
	g_return_if_fail (msg);

	dbus_message_iter_init_append (msg, &args);
	if (!dbus_message_iter_append_basic (&args, DBUS_TYPE_STRING, &app_id) ||
	    !dbus_message_iter_append_basic (&args, DBUS_TYPE_STRING, &client_id))
		g_return_if_reached ();

	/* Send message and get a handle for a reply */
	reply = dbus_connection_send_with_reply_and_block (conn, msg, 1000, &derr);
	dbus_message_unref (msg);

	if (!reply) {
		g_message ("couldn't register in session: %s", derr.message);
		dbus_error_free (&derr);
		return;
	}

	/* Get out our client path */
	if (!dbus_message_iter_init (reply, &args) ||
	    dbus_message_iter_get_arg_type (&args) != DBUS_TYPE_OBJECT_PATH) {
		g_message ("invalid register response from session");
	} else {
		dbus_message_iter_get_basic (&args, &client_session_path);
		client_session_path = g_strdup (client_session_path);
	}

	dbus_message_unref (reply);

	/*
	 * Unset DESKTOP_AUTOSTART_ID in order to avoid child processes to
	 * use the same client id.
	 */
	g_unsetenv ("DESKTOP_AUTOSTART_ID");

	/*
	 * Now we register for DBus signals on that client session path
	 * These are fired specifically for us.
	 */
	client_session_rule = g_strdup_printf("type='signal',"
	                                      "interface='org.gnome.SessionManager.ClientPrivate',"
	                                      "path='%s'",
	                                      client_session_path);
	dbus_bus_add_match (conn, client_session_rule, &derr);

	if(dbus_error_is_set(&derr)) {
		g_message ("couldn't listen for signals in session: %s", derr.message);
		dbus_error_free (&derr);
		g_free (client_session_rule);
		client_session_rule = NULL;
		return;
	}

	dbus_connection_add_filter (conn, signal_filter, NULL, NULL);
}
