/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkr-daemon-util.c - Helper utilities for the daemon

   Copyright (C) 2007, Stefan Walter

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

#include "gkd-util.h"

#include "egg/egg-cleanup.h"
#include "egg/egg-mkdtemp.h"
#include "egg/egg-unix-credentials.h"

#include <glib.h>

#include <sys/stat.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * A list of all the environment variables the daemon can
 * possibly send out when it starts.
 */
const gchar *GKD_UTIL_OUT_ENVIRONMENT[] = {
	"SSH_AUTH_SOCK",
	"GNOME_KEYRING_CONTROL",
	"GNOME_KEYRING_PID",
	"SSH_AGENT_PID",
	NULL
};

/*
 * A list of all the environment variables the daemon
 * is interested in from clients if it was started
 * early before these environment variables were set.
 */
const gchar *GKD_UTIL_IN_ENVIRONMENT[] = {
	"DISPLAY",
	"DBUS_SESSION_BUS_ADDRESS",
	"DESKTOP_AUTOSTART_ID",
	"ICEAUTHORITY",
	"LANG",
	"XAUTHORITY",
	"XAUTHLOCALHOSTNAME",
	"XDG_SESSION_COOKIE",
	"LOGNAME",
	"USERNAME",
	NULL
};

static gchar* master_directory = NULL;
static GArray* published_environ = NULL;

static GFunc watch_environ = NULL;
static gpointer watch_user_data = NULL;
static GDestroyNotify watch_destroy_notify = NULL;

static void
uninit_master_directory (gpointer data)
{
	g_assert (master_directory);
	rmdir (master_directory);
	g_free (master_directory);
	master_directory = NULL;
}

void
gkd_util_init_master_directory (const gchar *replace)
{
	gboolean exists = FALSE;
	gboolean valid = FALSE;
	struct stat st;

	if (replace) {
		exists = TRUE;
		if (lstat (replace, &st) < 0) {
			if (errno == ENOTDIR || errno == ENOENT) {
				exists = FALSE;
				valid = TRUE;
			}
		} else if (st.st_uid != geteuid ()) {
			g_message ("The gnome-keyring control directory is not owned with the same "
			           "credentials as the user login: %s", replace);
		} else if ((st.st_mode & 0777) != 0700) {
			g_message ("The gnome-keyring control directory has invalid permissions. It "
			           "must be only be accessible by its owner (ie: 0700): %s", replace);
		} else {
			valid = TRUE;
		}
	}

	/* Generate a new directory */
	if (!valid) {
		master_directory = g_build_filename (g_get_tmp_dir (), "keyring-XXXXXX", NULL);
		if (egg_mkdtemp (master_directory) == NULL)
			g_warning ("couldn't create socket directory: %s", g_strerror (errno));

	/* A directory was supplied, but doesn't exist yet */
	} else if (!exists) {
		g_assert (replace);
		master_directory = g_strdup (replace);
		if (g_mkdir_with_parents (master_directory, 0700) < 0)
			g_warning ("couldn't create socket directory: %s", g_strerror (errno));

	/* A valid existing directory was supplied */
	} else {
		g_assert (replace);
		master_directory = g_strdup (replace);
	}

	gkd_util_push_environment (GKD_UTIL_ENV_CONTROL, master_directory);
	egg_cleanup_register (uninit_master_directory, NULL);
}

const gchar*
gkd_util_get_master_directory (void)
{
	g_return_val_if_fail (master_directory, NULL);
	return master_directory;
}

static void
uninit_environment (gpointer data)
{
	guint i;

	if (published_environ) {
		for (i = 0; i < published_environ->len; ++i)
			g_free (g_array_index (published_environ, gchar*, i));
		g_array_free (published_environ, TRUE);
	}

	published_environ = NULL;

	if (watch_destroy_notify && watch_user_data)
		(watch_destroy_notify) (watch_user_data);
	watch_user_data = NULL;
	watch_destroy_notify = NULL;
	watch_environ = NULL;
}

static void
init_environment ()
{
	if (published_environ)
		return;
	published_environ = g_array_new (TRUE, TRUE, sizeof (gchar*));
	egg_cleanup_register (uninit_environment, NULL);
}

void
gkd_util_push_environment (const gchar *name, const gchar *value)
{
	gchar *env;

	init_environment ();

	env = g_strdup_printf ("%s=%s", name, value);
	g_array_append_val (published_environ, env);

	if (watch_environ)
		(watch_environ) (env, watch_user_data);
}

void
gkd_util_push_environment_full (const gchar *var)
{
	gchar *env;

	g_return_if_fail (strchr (var, '=') != NULL);
	init_environment ();

	env = g_strdup (var);
	g_array_append_val (published_environ, env);

	if (watch_environ)
		(watch_environ) (env, watch_user_data);
}

const gchar**
gkd_util_get_environment (void)
{
	init_environment ();
	return (const gchar**)published_environ->data;
}

void
gkd_util_watch_environment (GFunc func, gpointer user_data,
                            GDestroyNotify destroy_notify)
{
	g_return_if_fail (func);
	g_return_if_fail (!watch_environ);

	watch_environ = func;
	watch_user_data = user_data;
	watch_destroy_notify = destroy_notify;
}

gchar**
gkd_util_build_environment (const gchar **names)
{
	GArray *array = g_array_sized_new (TRUE, TRUE, sizeof (gchar*), 8);
	const gchar *value;
	const gchar **name;
	gchar *env;

	/* Transform them into NAME=VALUE pairs */
	for (name = names; *name; ++name) {
		value = g_getenv (*name);
		if (value) {
			env = g_strdup_printf ("%s=%s", *name, value);
			g_array_append_val (array, env);
		}
	}

	return (gchar**)g_array_free (array, FALSE);
}
