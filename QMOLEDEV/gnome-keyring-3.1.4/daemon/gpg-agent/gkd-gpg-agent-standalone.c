/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
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

#include "gkd-gpg-agent.h"
#include "gkd-gpg-agent-private.h"

#include "egg/egg-error.h"
#include "egg/egg-secure-memory.h"

#include "gck/gck.h"

#include <glib.h>
#include <glib-object.h>

#include <pwd.h>
#include <string.h>
#include <unistd.h>

EGG_SECURE_GLIB_DEFINITIONS();

static gboolean
accept_client (GIOChannel *channel, GIOCondition cond, gpointer unused)
{
	gkd_gpg_agent_accept ();
	return TRUE;
}

static gboolean
authenticate_slot (GckModule *module, GckSlot *slot, gchar *label, gchar **password, gpointer unused)
{
	gchar *prompt = g_strdup_printf ("Enter token password (%s): ", label);
	char *result = getpass (prompt);
	g_free (prompt);
	*password = g_strdup (result);
	memset (result, 0, strlen (result));
	return TRUE;
}

static gboolean
authenticate_object (GckModule *module, GckObject *object, gchar *label, gchar **password)
{
	gchar *prompt = g_strdup_printf ("Enter object password (%s): ", label);
	char *result = getpass (prompt);
	g_free (prompt);
	*password = g_strdup (result);
	memset (result, 0, strlen (result));
	return TRUE;
}

int
main(int argc, char *argv[])
{
	GckModule *module;
	GError *error = NULL;
	GIOChannel *channel;
	GMainLoop *loop;
	gboolean ret;
	int sock;

	g_type_init ();

	if (!g_thread_supported ())
		g_thread_init (NULL);

	if (argc <= 1) {
		g_message ("specify pkcs11 module on the command line");
		return 1;
	}

	module = gck_module_initialize (argv[1], &error);
	if (!module) {
		g_message ("couldn't load pkcs11 module: %s", egg_error_message (error));
		g_clear_error (&error);
		return 1;
	}

	g_signal_connect (module, "authenticate-slot", G_CALLBACK (authenticate_slot), NULL);
	g_signal_connect (module, "authenticate-object", G_CALLBACK (authenticate_object), NULL);

	ret = gkd_gpg_agent_initialize_with_module (module);
	g_object_unref (module);

	if (ret == FALSE)
		return 1;

	sock = gkd_gpg_agent_startup ("/tmp");
	if (sock == -1)
		return 1;

	channel = g_io_channel_unix_new (sock);
	g_io_add_watch (channel, G_IO_IN | G_IO_HUP, accept_client, NULL);
	g_io_channel_unref (channel);

	g_print ("GPG_AGENT_INFO=%s\n", g_getenv ("GPG_AGENT_INFO"));

	/* Run a main loop */
	loop = g_main_loop_new (NULL, FALSE);
	g_main_loop_run (loop);
	g_main_loop_unref (loop);

	gkd_gpg_agent_shutdown ();
	gkd_gpg_agent_uninitialize ();

	return 0;
}
