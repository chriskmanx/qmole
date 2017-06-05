/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-helpers.c: Common functions called from gtest unit tests

   Copyright (C) 2008 Stefan Walter

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

/* This file is included into the main .c file for each gtest unit-test program */

#include "config.h"

#include <glib.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "testing.h"

#include "egg/egg-error.h"
#include "egg/egg-secure-memory.h"

#include "pkcs11/pkcs11.h"

#ifdef WITH_P11_TESTS
#include <p11-tests.h>
#endif

/* Forward declaration */
void testing_test_p11_module (CK_FUNCTION_LIST_PTR module, const gchar *config);

static const gchar *test_path = NULL;

EGG_SECURE_GLIB_DEFINITIONS ();

static GCond *wait_condition = NULL;
static GCond *wait_start = NULL;
static GMutex *wait_mutex = NULL;
static gboolean wait_waiting = FALSE;

void
testing_wait_stop (void)
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
testing_wait_until (int timeout)
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

const gchar*
testing_scratch_directory (void)
{
	return test_path;
}

gchar*
testing_scratch_filename (const gchar *basename)
{
	return g_build_filename (test_path, basename, NULL);
}

gchar*
testing_data_filename (const gchar *basename)
{
	return g_build_filename (testing_data_directory (), basename, NULL);
}

const gchar*
testing_data_directory (void)
{
	const gchar *dir;
	gchar *cur, *env;

	dir = g_getenv ("TEST_DATA");
	if (dir == NULL)
		dir = "./test-data";
	if (!g_path_is_absolute (dir)) {
		cur = g_get_current_dir ();
		if (strncmp (dir, "./", 2) == 0)
			dir += 2;
		env = g_build_filename (cur, dir, NULL);
		g_free (cur);
		g_setenv ("TEST_DATA", env, TRUE);
		g_free (env);
		dir = g_getenv ("TEST_DATA");
	}

	return dir;
}

guchar*
testing_data_read (const gchar *basename, gsize *n_result)
{
	GError *error = NULL;
	gchar *result;
	gchar *file;

	file = testing_data_filename (basename);
	if (!g_file_get_contents (file, &result, n_result, &error)) {
		g_warning ("could not read test data file: %s: %s", file,
		           egg_error_message (error));
		g_assert_not_reached ();
	}

	g_free (file);
	return (guchar*)result;
}

#if WITH_P11_TESTS

static void
on_p11_tests_log (int level, const char *section, const char *message)
{
	if (level == P11_TESTS_NONE) {
		g_message ("%s", message);
	} else if (level != P11_TESTS_FAIL) {
		g_message ("%s: %s", section, message);
	} else {
		g_print ("/%s/%s: FAIL: %s\n", testing_external_name (), section, message);
		testing_external_fail ();
	}
}

void
testing_test_p11_module (CK_FUNCTION_LIST_PTR module, const gchar *config)
{
	p11_tests_set_log_func (on_p11_tests_log);
	p11_tests_set_unexpected (1);
	p11_tests_set_verbose (0);
	p11_tests_set_write_session (1);
	if (config)
		p11_tests_load_config (config);
	p11_tests_perform (module);
}

#else /* !WITH_P11_TESTS */

void
testing_test_p11_module (CK_FUNCTION_LIST_PTR module, const gchar *config)
{
	g_message ("p11-tests support not built in");
}

#endif /* !WITH_P11_TESTS */

static const gchar *external_name = NULL;
static gint external_fails = 0;

void
testing_external_run (const gchar *name, TestingExternalFunc func, int *result)
{
	if (result != 0)
		return;

	external_fails = 0;
	external_name = name;
	func ();
	if (external_fails) {
		g_printerr ("/%s: FAIL: %d failures", name, external_fails);
		*result = external_fails;
	}
}

const gchar*
testing_external_name (void)
{
	return external_name;
}

void
testing_external_fail (void)
{
	++external_fails;
}

static void
chdir_base_dir (char* argv0)
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

	g_free (dir);
}

static gpointer
testing_thread (gpointer loop)
{
	/* Must have been defined by the test including this file */
	gint ret = run();
	g_main_loop_quit (loop);
	return GINT_TO_POINTER (ret);
}

int
main (int argc, char* argv[])
{
	GLogLevelFlags fatal_mask;
	GThread *thread;
	GMainLoop *loop;
	gpointer ret;

	g_type_init ();
	g_thread_init (NULL);

	test_path = getenv ("GNOME_KEYRING_TEST_PATH");
	if (!test_path) {
		test_path = "/tmp/test-gnome-keyring";
		setenv ("GNOME_KEYRING_TEST_PATH", test_path, 1);
		g_mkdir_with_parents (test_path, 0777);
	}

	chdir_base_dir (argv[0]);
	g_test_init (&argc, &argv, NULL);
	gtk_init (&argc, &argv);

	loop = g_main_loop_new (NULL, FALSE);
	wait_condition = g_cond_new ();
	wait_start = g_cond_new ();
	wait_mutex = g_mutex_new ();

	fatal_mask = g_log_set_always_fatal (G_LOG_FATAL_MASK);
	fatal_mask |= G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL;
	g_log_set_always_fatal (fatal_mask);

	thread = g_thread_create (testing_thread, loop, TRUE, NULL);
	g_assert (thread);

	g_main_loop_run (loop);
	ret = g_thread_join (thread);
	g_main_loop_unref (loop);

	g_cond_free (wait_condition);
	g_mutex_free (wait_mutex);

	return GPOINTER_TO_INT (ret);
}
