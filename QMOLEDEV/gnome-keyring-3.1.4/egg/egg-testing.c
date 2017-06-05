/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *
 * Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "egg-testing.h"

#include <errno.h>
#include <unistd.h>

static GCond *wait_condition = NULL;
static GCond *wait_start = NULL;
static GMutex *wait_mutex = NULL;
static gboolean wait_waiting = FALSE;

static const char HEXC[] = "0123456789ABCDEF";

static gchar*
hex_dump (const guchar *data, gsize n_data)
{
	GString *result;
	gsize i;
	guchar j;

	g_assert (data);

	result = g_string_sized_new (n_data * 2 + 1);
	for (i = 0; i < n_data; ++i) {
		g_string_append (result, "\\x");

		j = data[i] >> 4 & 0xf;
		g_string_append_c (result, HEXC[j]);
		j = data[i] & 0xf;
		g_string_append_c (result, HEXC[j]);
	}

	return g_string_free (result, FALSE);
}

void
egg_assertion_message_cmpmem (const char     *domain,
                              const char     *file,
                              int             line,
                              const char     *func,
                              const char     *expr,
                              gconstpointer   arg1,
                              gsize           n_arg1,
                              const char     *cmp,
                              gconstpointer   arg2,
                              gsize           n_arg2)
{
  char *a1, *a2, *s;
  a1 = arg1 ? hex_dump (arg1, n_arg1) : g_strdup ("NULL");
  a2 = arg2 ? hex_dump (arg2, n_arg2) : g_strdup ("NULL");
  s = g_strdup_printf ("assertion failed (%s): (%s %s %s)", expr, a1, cmp, a2);
  g_free (a1);
  g_free (a2);
  g_assertion_message (domain, file, line, func, s);
  g_free (s);
}

void
egg_tests_chdir_base (gchar* argv0)
{
	gchar *dir, *base;

	dir = g_path_get_dirname (argv0);
	if (chdir (dir) < 0)
		g_warning ("couldn't change directory to: %s: %s",
		           dir, g_strerror (errno));

	base = g_path_get_basename (dir);
	if (strcmp (base, ".libs") == 0) {
		if (chdir ("..") < 0)
			g_warning ("couldn't change directory to ..: %s",
			           g_strerror (errno));
	}

	g_free (base);
	g_free (dir);
}

void
egg_test_wait_stop (void)
{
	GTimeVal tv;

	g_get_current_time (&tv);
	g_time_val_add (&tv, 1000);

	g_assert (wait_mutex);
	g_assert (wait_condition);
	g_mutex_lock (wait_mutex);
		if (!wait_waiting)
			g_cond_timed_wait (wait_start, wait_mutex, &tv);
		g_assert (wait_waiting);
		g_cond_broadcast (wait_condition);
	g_mutex_unlock (wait_mutex);
}

gboolean
egg_test_wait_until (int timeout)
{
	GTimeVal tv;
	gboolean ret;

	g_get_current_time (&tv);
	g_time_val_add (&tv, timeout * 1000);

	g_assert (wait_mutex);
	g_assert (wait_condition);
	g_mutex_lock (wait_mutex);
		g_assert (!wait_waiting);
		wait_waiting = TRUE;
		g_cond_broadcast (wait_start);
		ret = g_cond_timed_wait (wait_condition, wait_mutex, &tv);
		g_assert (wait_waiting);
		wait_waiting = FALSE;
	g_mutex_unlock (wait_mutex);

	return ret;
}

static gpointer
testing_thread (gpointer loop)
{
	/* Must have been defined by the test including this file */
	gint ret = g_test_run ();
	g_main_loop_quit (loop);
	return GINT_TO_POINTER (ret);
}

gint
egg_tests_run_in_thread_with_loop (void)
{
	GThread *thread;
	GMainLoop *loop;
	gpointer ret;

	g_thread_init (NULL);

	loop = g_main_loop_new (NULL, FALSE);
	wait_condition = g_cond_new ();
	wait_start = g_cond_new ();
	wait_mutex = g_mutex_new ();

	thread = g_thread_create (testing_thread, loop, TRUE, NULL);
	g_assert (thread);

	g_main_loop_run (loop);
	ret = g_thread_join (thread);
	g_main_loop_unref (loop);

	g_cond_free (wait_condition);
	g_mutex_free (wait_mutex);

	return GPOINTER_TO_INT (ret);
}
