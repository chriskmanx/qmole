/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-location.c: Test location functionality

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "test-suite.h"

#include "gkm/gkm-file-tracker.h"

#include <glib/gstdio.h>

#define DATA "test-data"
#define SUBDIR "test-subdir"
#define WILDCARD "*.woo?"

static GkmFileTracker *the_tracker = NULL;
static gchar *test_dir = NULL;
static gchar *test_file = NULL;

static guint n_files_added = 0;
static gchar* last_file_added = 0;

static guint n_files_changed = 0;
static gchar* last_file_changed = 0;

static guint n_files_removed = 0;
static gchar* last_file_removed = 0;

static void
file_added (GkmFileTracker *tracker, const gchar *path, gpointer unused)
{
	g_assert ("should be a non-null path" && path != NULL);

	++n_files_added;
	g_free (last_file_added);
	last_file_added = g_strdup (path);
}

static void
file_changed (GkmFileTracker *tracker, const gchar *path, gpointer unused)
{
	g_assert ("should be a non-null path" && path != NULL);

	++n_files_changed;
	g_free (last_file_changed);
	last_file_changed = g_strdup (path);
}

static void
file_removed (GkmFileTracker *tracker, const gchar *path, gpointer unused)
{
	g_assert ("should be a non-null path" && path != NULL);

	++n_files_removed;
	g_free (last_file_removed);
	last_file_removed = g_strdup (path);
}

static void
file_reset_stats (void)
{
	g_free (last_file_removed);
	g_free (last_file_added);
	g_free (last_file_changed);
	last_file_removed = last_file_added = last_file_changed = NULL;
	n_files_added = n_files_changed = n_files_removed = 0;
}

DEFINE_SETUP(tracker)
{
	/* Make a test directory */
	test_dir = g_build_filename ("/tmp", SUBDIR, NULL);

	the_tracker = gkm_file_tracker_new (test_dir, WILDCARD, NULL);
	g_signal_connect (the_tracker, "file-added", G_CALLBACK (file_added), NULL);
	g_signal_connect (the_tracker, "file-removed", G_CALLBACK (file_removed), NULL);
	g_signal_connect (the_tracker, "file-changed", G_CALLBACK (file_changed), NULL);

	/* Mtime must change so wait between tests */
	sleep (1);

	test_file = g_build_filename (test_dir, "my-file.woof", NULL);
	g_unlink (test_file);
}

DEFINE_TEARDOWN(tracker)
{
	file_reset_stats ();
	g_object_unref (the_tracker);
	g_free (test_dir);
	g_free (test_file);
}

DEFINE_TEST(file_watch)
{
	/* A watch for an non-existant directory, should have no responses */
	gkm_file_tracker_refresh (the_tracker, FALSE);

	g_assert_cmpint (0, ==, n_files_added);
	g_assert_cmpint (0, ==, n_files_changed);
	g_assert_cmpint (0, ==, n_files_removed);

	g_mkdir_with_parents (test_dir, 0700);

	/* Should still have no responses even though it exists */
	gkm_file_tracker_refresh (the_tracker, FALSE);

	g_assert_cmpint (0, ==, n_files_added);
	g_assert_cmpint (0, ==, n_files_changed);
	g_assert_cmpint (0, ==, n_files_removed);
}

DEFINE_TEST(watch_file)
{
	gboolean ret;

	/* Make sure things are clean */
	gkm_file_tracker_refresh (the_tracker, FALSE);

	n_files_added = n_files_changed = n_files_removed = 0;
	last_file_added = last_file_changed = last_file_removed = 0;

	ret = g_file_set_contents (test_file, DATA, strlen (DATA), NULL);
	g_assert (ret);

	/* Now make sure that file is located */
	gkm_file_tracker_refresh (the_tracker, FALSE);

	g_assert_cmpint (1, ==, n_files_added);
	g_assert_cmpint (0, ==, n_files_changed);
	g_assert_cmpint (0, ==, n_files_removed);

	/* The added one should match our file */
	g_assert_cmpstr (last_file_added, ==, test_file);

	file_reset_stats ();
	sleep (1);

	/* Shouldn't find the file again */
	gkm_file_tracker_refresh (the_tracker, FALSE);
	g_assert_cmpint (0, ==, n_files_added);
	g_assert_cmpint (0, ==, n_files_changed);
	g_assert_cmpint (0, ==, n_files_removed);

	/* But we should find the file if forced to */
	gkm_file_tracker_refresh (the_tracker, TRUE);
	g_assert_cmpint (0, ==, n_files_added);
	g_assert_cmpint (1, ==, n_files_changed);
	g_assert_cmpint (0, ==, n_files_removed);
	g_assert_cmpstr (last_file_changed, ==, test_file);

	file_reset_stats ();

	ret = g_file_set_contents (test_file, DATA, strlen (DATA), NULL);
	g_assert (ret);

	/* File was updated */
	gkm_file_tracker_refresh (the_tracker, FALSE);
	g_assert_cmpint (0, ==, n_files_added);
	g_assert_cmpint (1, ==, n_files_changed);
	g_assert_cmpint (0, ==, n_files_removed);
	g_assert_cmpstr (last_file_changed, ==, test_file);

	file_reset_stats ();
	g_unlink (test_file);

	/* Now file should be removed */
	gkm_file_tracker_refresh (the_tracker, FALSE);

	g_assert_cmpint (0, ==, n_files_added);
	g_assert_cmpint (0, ==, n_files_changed);
	g_assert_cmpint (1, ==, n_files_removed);
	g_assert_cmpstr (last_file_removed, ==, test_file);
}

DEFINE_TEST(nomatch)
{
	gchar *file = g_build_filename (test_dir, "my-file.toot", NULL);
	gboolean ret;

	/* Mtime must change so wait between tests */
	sleep (1);

	ret = g_file_set_contents (file, DATA, strlen (DATA), NULL);
	g_assert (ret);

	file_reset_stats ();

	/* Now make sure that file is not located */
	gkm_file_tracker_refresh (the_tracker, FALSE);

	g_assert_cmpint (0, ==, n_files_added);
	g_assert_cmpint (0, ==, n_files_changed);
	g_assert_cmpint (0, ==, n_files_removed);

	g_unlink (file);
	g_free (file);
}
