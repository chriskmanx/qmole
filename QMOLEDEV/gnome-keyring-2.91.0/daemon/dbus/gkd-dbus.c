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

#include "gkd-dbus.h"
#include "gkd-dbus-private.h"

#include "gkd-util.h"

#include "egg/egg-cleanup.h"
#include "egg/egg-dbus.h"

#include <glib.h>

#include <dbus/dbus.h>

static DBusConnection *dbus_conn = NULL;
static gboolean object_registered = FALSE;
static gboolean acquired_asked = FALSE;
static gboolean acquired_service = FALSE;

#define GNOME_KEYRING_DAEMON_SERVICE    "org.gnome.keyring"
#define GNOME_KEYRING_DAEMON_PATH       "/org/gnome/keyring/daemon"
#define GNOME_KEYRING_DAEMON_INTERFACE  "org.gnome.keyring.Daemon"

static void
cleanup_session_bus (gpointer unused)
{
	if (!dbus_conn)
		return;

	egg_dbus_disconnect_from_mainloop (dbus_conn, NULL);
	dbus_connection_unref (dbus_conn);
	dbus_conn = NULL;
}

static gboolean
connect_to_session_bus (void)
{
	DBusError derr = { 0 };

	if (dbus_conn)
		return TRUE;

	dbus_error_init (&derr);

	/* Get the dbus bus and hook up */
	dbus_conn = dbus_bus_get (DBUS_BUS_SESSION, &derr);
	if (!dbus_conn) {
		g_message ("couldn't connect to dbus session bus: %s", derr.message);
		dbus_error_free (&derr);
		return FALSE;
	}

	egg_dbus_connect_with_mainloop (dbus_conn, NULL);
	dbus_connection_set_exit_on_disconnect (dbus_conn, FALSE);
	egg_cleanup_register (cleanup_session_bus, NULL);
	return TRUE;
}

static DBusHandlerResult
message_handler_cb (DBusConnection *conn, DBusMessage *message, void *user_data)
{
	/*
	 * Here we handle the requests to our own gnome-keyring DBus interfaces
	 */

	DBusMessageIter args;
	DBusMessage *reply = NULL;

	/* GetEnvironment */
	if (dbus_message_get_type (message) == DBUS_MESSAGE_TYPE_METHOD_CALL &&
	    dbus_message_is_method_call (message, GNOME_KEYRING_DAEMON_INTERFACE, "GetEnvironment") &&
	    g_str_equal (dbus_message_get_signature (message), "")) {

		const gchar **env;
		DBusMessageIter items, entry;
		gchar **parts;

		env = gkd_util_get_environment ();
		g_return_val_if_fail (env, DBUS_HANDLER_RESULT_NOT_YET_HANDLED);

		/* Setup the result */
		reply = dbus_message_new_method_return (message);
		dbus_message_iter_init_append (reply, &args);
		if (!dbus_message_iter_open_container (&args, DBUS_TYPE_ARRAY, "{ss}", &items))
			g_return_val_if_reached (DBUS_HANDLER_RESULT_NEED_MEMORY);
		while (*env) {
			parts = g_strsplit (*env, "=", 2);
			g_return_val_if_fail (parts && parts[0] && parts[1], DBUS_HANDLER_RESULT_NOT_YET_HANDLED);
			if (!dbus_message_iter_open_container (&items, DBUS_TYPE_DICT_ENTRY, NULL, &entry) ||
			    !dbus_message_iter_append_basic (&entry, DBUS_TYPE_STRING, &parts[0]) ||
			    !dbus_message_iter_append_basic (&entry, DBUS_TYPE_STRING, &parts[1]) ||
			    !dbus_message_iter_close_container (&items, &entry))
				g_return_val_if_reached (DBUS_HANDLER_RESULT_NEED_MEMORY);
			++env;
		}
		if (!dbus_message_iter_close_container (&args, &items))
			g_return_val_if_reached (DBUS_HANDLER_RESULT_NEED_MEMORY);

	/* GetControlDirectory */
	} else if (dbus_message_get_type (message) == DBUS_MESSAGE_TYPE_METHOD_CALL &&
	           dbus_message_is_method_call (message, GNOME_KEYRING_DAEMON_INTERFACE, "GetControlDirectory") &&
	           g_str_equal (dbus_message_get_signature (message), "")) {

		/* Setup the result */
		const gchar *directory = gkd_util_get_master_directory ();
		reply = dbus_message_new_method_return (message);
		dbus_message_append_args (reply, DBUS_TYPE_STRING, &directory, DBUS_TYPE_INVALID);

	/* Unknown call */
	} else {
		return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
	}

	/* Send the reply */
	if (!dbus_connection_send (conn, reply, NULL))
		g_return_val_if_reached (DBUS_HANDLER_RESULT_NEED_MEMORY);
	dbus_connection_flush (conn);

	return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusObjectPathVTable object_vtable  = {
	NULL,
	message_handler_cb,
	NULL,
};

static void
cleanup_singleton (gpointer unused)
{
	g_return_if_fail (dbus_conn);
	if (object_registered)
		dbus_connection_unregister_object_path (dbus_conn, GNOME_KEYRING_DAEMON_PATH);
	object_registered = FALSE;
}

gboolean
gkd_dbus_singleton_acquire (gboolean *acquired)
{
	DBusError derr = DBUS_ERROR_INIT;
	dbus_uint32_t res = 0;
	const gchar *service = NULL;
	unsigned int flags;

	g_assert (acquired);

	if (!connect_to_session_bus ())
		return FALSE;

	/* First register the object */
	if (!object_registered) {
		if (dbus_connection_register_object_path (dbus_conn, GNOME_KEYRING_DAEMON_PATH,
		                                          &object_vtable, NULL)) {
			object_registered = TRUE;
			egg_cleanup_register (cleanup_singleton, NULL);
		} else {
			g_message ("couldn't register dbus object path");
		}
	}

	/* Try and grab our name */
	if (!acquired_asked) {

#ifdef WITH_TESTS
		service = g_getenv ("GNOME_KEYRING_TEST_SERVICE");
		if (service && service[0])
			flags = DBUS_NAME_FLAG_ALLOW_REPLACEMENT | DBUS_NAME_FLAG_REPLACE_EXISTING;
		else
#endif
			service = GNOME_KEYRING_DAEMON_SERVICE;

		res = dbus_bus_request_name (dbus_conn, service, 0, &derr);
		if (dbus_error_is_set (&derr)) {
			g_message ("couldn't request name '%s' on session bus: %s", service, derr.message);
			dbus_error_free (&derr);
			return FALSE;
		}

		acquired_asked = TRUE;
		switch (res) {
		/* We acquired the service name */
		case DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER:
		case DBUS_REQUEST_NAME_REPLY_ALREADY_OWNER:
			acquired_service = TRUE;
			break;
			/* Another daemon is running */
		case DBUS_REQUEST_NAME_REPLY_IN_QUEUE:
		case DBUS_REQUEST_NAME_REPLY_EXISTS:
			acquired_service = FALSE;
			break;
		default:
			acquired_service = FALSE;
			g_return_val_if_reached (FALSE);
			break;
		};
	}

	*acquired = acquired_service;
	return TRUE;
}

gchar*
gkd_dbus_singleton_control (void)
{
	DBusError derr = DBUS_ERROR_INIT;
	DBusMessage *msg, *reply;
	gchar *control = NULL;
	const char *path;

	/* If tried to aquire the service must have failed */
	g_return_val_if_fail (!acquired_service, NULL);

	if (!connect_to_session_bus ())
		return NULL;

	msg = dbus_message_new_method_call (GNOME_KEYRING_DAEMON_SERVICE,
	                                    GNOME_KEYRING_DAEMON_PATH,
	                                    GNOME_KEYRING_DAEMON_INTERFACE,
	                                    "GetControlDirectory");
	g_return_val_if_fail (msg, NULL);
	dbus_message_set_auto_start (msg, FALSE);

	/* Send message and get a handle for a reply */
	reply = dbus_connection_send_with_reply_and_block (dbus_conn, msg, 1000, &derr);
	dbus_message_unref (msg);

	if (!reply) {
		if (!dbus_error_has_name (&derr, "org.freedesktop.DBus.Error.NameHasNoOwner"))
			g_message ("couldn't communicate with already running daemon: %s", derr.message);
		dbus_error_free (&derr);
		return NULL;
	}

	/* Get out our client path */
	if (!dbus_message_get_args (reply, &derr, DBUS_TYPE_STRING, &path, DBUS_TYPE_INVALID)) {
		g_message ("couldn't parse response from already running daemon: %s", derr.message);
		dbus_error_free (&derr);
		control = NULL;
	} else {
		control = g_strdup (path);
	}

	dbus_message_unref (reply);
	return control;
}

static void
dbus_cleanup (gpointer unused)
{
	g_return_if_fail (dbus_conn);
	gkd_dbus_secrets_cleanup (dbus_conn);
	gkd_dbus_session_cleanup (dbus_conn);
	gkd_dbus_environment_cleanup (dbus_conn);
}

void
gkd_dbus_setup (void)
{
	gboolean unused;

	if (!connect_to_session_bus ())
		return;

	/* Our singleton, and internal service API */
	gkd_dbus_singleton_acquire (&unused);

	/* Session stuff */
	gkd_dbus_environment_init (dbus_conn);
	gkd_dbus_session_init (dbus_conn);

	/* Secrets API */
	gkd_dbus_secrets_init (dbus_conn);

	egg_cleanup_register (dbus_cleanup, NULL);
}
