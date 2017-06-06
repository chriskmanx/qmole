/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2008 Matthias Clasen <mclasen@redhat.com>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <glib.h>
#include <glib-object.h>

#include <dbus/dbus-glib.h>
#include <dbus/dbus-glib-lowlevel.h>


#include "gconf-defaults.h"

static DBusGProxy *
get_bus_proxy (DBusGConnection *connection)
{
        DBusGProxy *bus_proxy;

	bus_proxy = dbus_g_proxy_new_for_name (connection,
                                               DBUS_SERVICE_DBUS,
                                               DBUS_PATH_DBUS,
                                               DBUS_INTERFACE_DBUS);
        return bus_proxy;
}

#define BUS_NAME "org.gnome.GConf.Defaults"

static gboolean
acquire_name_on_proxy (DBusGProxy *bus_proxy)
{
        GError     *error;
        guint       result;
        gboolean    res;
        gboolean    ret;

        ret = FALSE;

        if (bus_proxy == NULL) {
                goto out;
        }

        error = NULL;
	res = dbus_g_proxy_call (bus_proxy,
                                 "RequestName",
                                 &error,
                                 G_TYPE_STRING, BUS_NAME,
                                 G_TYPE_UINT, 0,
                                 G_TYPE_INVALID,
                                 G_TYPE_UINT, &result,
                                 G_TYPE_INVALID);
        if (! res) {
                if (error != NULL) {
                        g_warning ("Failed to acquire %s: %s", BUS_NAME, error->message);
                        g_error_free (error);
                } else {
                        g_warning ("Failed to acquire %s", BUS_NAME);
                }
                goto out;
	}

 	if (result != DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER) {
                if (error != NULL) {
                        g_warning ("Failed to acquire %s: %s", BUS_NAME, error->message);
                        g_error_free (error);
                } else {
                        g_warning ("Failed to acquire %s", BUS_NAME);
                }
                goto out;
        }

        ret = TRUE;

 out:
        return ret;
}

static DBusGConnection *
get_system_bus (void)
{
        GError          *error;
        DBusGConnection *bus;
        DBusConnection  *connection;

        error = NULL;
        bus = dbus_g_bus_get (DBUS_BUS_SYSTEM, &error);
        if (bus == NULL) {
                g_warning ("Couldn't connect to system bus: %s", error->message);
                g_error_free (error);
                goto out;
        }

        connection = dbus_g_connection_get_connection (bus);
 out:
        return bus;
}

extern gboolean disable_killtimer;
gboolean debug = FALSE;

GOptionEntry entries [] = {
	{ "debug", 0, 0, G_OPTION_ARG_NONE, &debug, "Emit debug output", NULL },
	{ "no-kill", 0, 0, G_OPTION_ARG_NONE, &disable_killtimer, "Don't exit when idle", NULL },
	{ NULL, }
};

static gint log_levels = (G_LOG_LEVEL_ERROR | G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING);

void
log_default_handler (const gchar   *log_domain,
                     GLogLevelFlags log_level,
                     const gchar   *message,
                     gpointer       unused_data)
{
	if ((log_level & log_levels) != 0) {
		g_log_default_handler (log_domain, log_level, message, unused_data);
	}
}


int
main (int argc, char **argv)
{
        GMainLoop           *loop;
        GConfDefaults       *mechanism;
        DBusGProxy          *bus_proxy;
        DBusGConnection     *connection;
        int                  ret;
	GOptionContext      *options;
	GError              *error = NULL;

        ret = 1;

        if (! g_thread_supported ()) {
                g_thread_init (NULL);
        }
        dbus_g_thread_init ();
        g_type_init ();

	options = g_option_context_new (NULL);
	g_option_context_add_main_entries (options, entries, NULL);
	if (!g_option_context_parse (options, &argc, &argv, &error)) {
		g_warning ("Failed to parse options: %s\n", error->message);
		g_error_free (error);
	}
	g_option_context_free (options);

	g_log_set_default_handler (log_default_handler, NULL);
	if (debug) {
		log_levels = log_levels | G_LOG_LEVEL_DEBUG;
	}

        connection = get_system_bus ();
        if (connection == NULL) {
                g_warning ("Could not get system bus connection; bailing out");
                goto out;
        }

        bus_proxy = get_bus_proxy (connection);
        if (bus_proxy == NULL) {
                g_warning ("Could not construct bus_proxy object; bailing out");
                goto out;
        }

        mechanism = gconf_defaults_new ();

        if (mechanism == NULL) {
                goto out;
        }

        if (!acquire_name_on_proxy (bus_proxy)) {
                g_warning ("Could not acquire name; bailing out");
                goto out;
        }

        loop = g_main_loop_new (NULL, FALSE);

        g_main_loop_run (loop);

        g_object_unref (mechanism);
        g_main_loop_unref (loop);
        ret = 0;

out:
        return ret;
}
