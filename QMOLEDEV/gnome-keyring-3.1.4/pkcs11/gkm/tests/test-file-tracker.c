/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-file-tracker.c: File tracker tests

   Copyright (C) 2007 Stefan Walter

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "gkm/gkm-file-tracker.h"

#include <glib/gstdio.h>

#define DATA "test-data"
#define SUBDIR "test-subdir"
#define WILDCARD "*.woo?"

typedef struct {
	GkmFileTracker *the_tracker;
	gchar *test_dir;
	gchar *test_file;
	guint n_files_added;
	gchar* last_file_added;
	guint n_files_changed;
	gchar* last_file_changed;
	guint n_files_removed;
	gchar* last_file_removed;
} Test;

static void
file_added (GkmFileTracker *tracker, const gchar *path, gpointer user_data)
{
	Test *test = user_data;

	g_assert ("should be a non-null path" && path != NULL);

	++test->n_files_added;
	g_free (test->last_file_added);
	test->last_file_added = g_strdup (path);
}

static void
file_changed (GkmFileTracker *tracker, const gchar *path, gpointer user_data)
{
	Test *test = user_data;

	g_assert ("should be a non-null path" && path != NULL);

	++test->n_files_changed;
	g_free (test->last_file_changed);
	test->last_file_changed = g_strdup (path);
}

static void
file_removed (GkmFileTracker *tracker, const gchar *path, gpointer user_data)
{
	Test *test = user_data;

	g_assert ("should be a non-null path" && path != NULL);

	++test->n_files_removed;
	g_free (test->last_file_removed);
	test->last_file_removed = g_strdup (path);
}

static void
file_reset_stats (Test *test)
{
	g_free (test->last_file_removed);
	g_free (test->last_file_added);
	g_free (test->last_file_changed);
	test->last_file_removed = test->last_file_added = test->last_file_changed = NULL;
	test->n_files_added = test->n_files_changed = test->n_files_removed = 0;
}

static void
setup (Test *test, gconstpointer unused)
{
	/* Make a test directory */
	test->test_dir = g_build_filename ("/tmp", SUBDIR, NULL);

	test->the_tracker = gkm_file_tracker_new (test->test_dir, WILDCARD, NULL);
	g_signal_connect (test->the_tracker, "file-added", G_CALLBACK (file_added), test);
	g_signal_connect (test->the_tracker, "file-removed", G_CALLBACK (file_removed), test);
	g_signal_connect (test->the_tracker, "file-changed", G_CALLBACK (file_changed), test);

	/* Mtime must change so wait between tests */
	sleep (1);

	test->test_file = g_build_filename (test->test_dir, "my-file.woof", NULL);
	g_unlink (test->test_file);
}

static void
teardown (Test *test, gconstpointer unused)
{
	file_reset_stats (test);
	g_object_unref (test->the_tracker);
	g_free (test->test_dir);
	g_free (test->test_file);
}

static void
test_file_watch (Test *test, gconstpointer unused)
{
	/* A watch for an non-existant directory, should have no responses */
	gkm_file_tracker_refresh (test->the_tracker, FALSE);

	g_assert_cmpint (0, ==, test->n_files_added);
	g_assert_cmpint (0, ==, test->n_files_changed);
	g_assert_cmpint (0, ==, test->n_files_removed);

	g_mkdir_with_parents (test->test_dir, 0700);

	/* Should still have no responses even though it exists */
	gkm_file_tracker_refresh (test->the_tracker, FALSE);

	g_assert_cmpint (0, ==, test->n_files_added);
	g_assert_cmpint (0, ==, test->n_files_changed);
	g_assert_cmpint (0, ==, test->n_files_removed);
}

static void
test_watch_file (Test *test, gconstpointer unused)
{
	gboolean ret;

	/* Make sure things are clean */
	gkm_file_tracker_refresh (test->the_tracker, FALSE);

	test->n_files_added = test->n_files_changed = test->n_files_removed = 0;
	test->last_file_added = test->last_file_changed = test->last_file_removed = 0;

	ret = g_file_set_contents (test->test_file, DATA, strlen (DATA), NULL);
	g_assert (ret);

	/* Now make sure that file is located */
	gkm_file_tracker_refresh (test->the_tracker, FALSE);

	g_assert_cmpint (1, ==, test->n_files_added);
	g_assert_cmpint (0, ==, test->n_files_changed);
	g_assert_cmpint (0, ==, test->n_files_removed);

	/* The added one should match our file */
	g_assert_cmpstr (test->last_file_added, ==, test->test_file);

	file_reset_stats (test);
	sleep (1);

	/* Shouldn't find the file again */
	gkm_file_tracker_refresh (test->the_tracker, FALSE);
	g_assert_cmpint (0, ==, test->n_files_added);
	g_assert_cmpint (0, ==, test->n_files_changed);
	g_assert_cmpint (0, ==, test->n_files_removed);

	/* But we should find the file if forced to */
	gkm_file_tracker_refresh (test->the_tracker, TRUE);
	g_assert_cmpint (0, ==, test->n_files_added);
	g_assert_cmpint (1, ==, test->n_files_changed);
	g_assert_cmpint (0, ==, test->n_files_removed);
	g_assert_cmpstr (test->last_file_changed, ==, test->test_file);

	file_reset_stats (test);

	ret = g_file_set_contents (test->test_file, DATA, strlen (DATA), NULL);
	g_assert (ret);

	/* File was updated */
	gkm_file_tracker_refresh (test->the_tracker, FALSE);
	g_assert_cmpint (0, ==, test->n_files_added);
	g_assert_cmpint (1, ==, test->n_files_changed);
	g_assert_cmpint (0, ==, test->n_files_removed);
	g_assert_cmpstr (test->last_file_changed, ==, test->test_file);

	file_reset_stats (test);
	g_unlink (test->test_file);

	/* Now file should be removed */
	gkm_file_tracker_refresh (test->the_tracker, FALSE);

	g_assert_cmpint (0, ==, test->n_files_added);
	g_assert_cmpint (0, ==, test->n_files_changed);
	g_assert_cmpint (1, ==, test->n_files_removed);
	g_assert_cmpstr (test->last_file_removed, ==, test->test_file);
}

static void
test_nomatch (Test *test, gconstpointer unused)
{
	gchar *file = g_build_filename (test->test_dir, "my-file.toot", NULL);
	gboolean ret;

	/* Mtime must change so wait between tests */
	sleep (1);

	ret = g_file_set_contents (file, DATA, strlen (DATA), NULL);
	g_assert (ret);

	file_reset_stats (test);

	/* Now make sure that file is not located */
	gkm_file_tracker_refresh (test->the_tracker, FALSE);

	g_assert_cmpint (0, ==, test->n_files_added);
	g_assert_cmpint (0, ==, test->n_files_changed);
	g_assert_cmpint (0, ==, test->n_files_removed);

	g_unlink (file);
	g_free (file);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gkm/file-tracker/file_watch", Test, NULL, setup, test_file_watch, teardown);
	g_test_add ("/gkm/file-tracker/watch_file", Test, NULL, setup, test_watch_file, teardown);
	g_test_add ("/gkm/file-tracker/nomatch", Test, NULL, setup, test_nomatch, teardown);

	return g_test_run ();
}
