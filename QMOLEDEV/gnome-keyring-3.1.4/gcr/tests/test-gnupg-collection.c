/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/*
   Copyright (C) 2010 Collabora Ltd

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

   Author: Stef Walter <stefw@collabora.co.uk>
*/

#include "config.h"

#include "gcr/gcr.h"
#include "gcr/gcr-gnupg-collection.h"
#include "gcr/gcr-gnupg-key.h"
#include "gcr/gcr-record.h"

#include "egg/egg-testing.h"

#include <glib.h>

#include <errno.h>
#include <string.h>

typedef struct {
	GcrGnupgCollection *collection;
	gchar *directory;
	GHashTable *keys;
	GAsyncResult *result;
} Test;

static void
on_collection_added (GcrCollection *collection, GObject *object, gpointer user_data)
{
	Test *test = user_data;
	GcrGnupgKey *key;
	const gchar *keyid;

	g_assert (GCR_COLLECTION (test->collection) == collection);

	g_assert (GCR_IS_GNUPG_KEY (object));
	key = GCR_GNUPG_KEY (object);

	keyid = _gcr_gnupg_key_get_keyid (key);
	g_assert (keyid);
	g_assert (!g_hash_table_lookup (test->keys, keyid));

	g_hash_table_insert (test->keys, g_strdup (keyid), key);
}

static void
on_collection_removed (GcrCollection *collection, GObject *object, gpointer user_data)
{
	Test *test = user_data;
	GcrGnupgKey *key;
	const gchar *keyid;

	g_assert (GCR_COLLECTION (test->collection) == collection);
	g_assert (GCR_IS_GNUPG_KEY (object));

	keyid = _gcr_gnupg_key_get_keyid (GCR_GNUPG_KEY (object));
	key = g_hash_table_lookup (test->keys, keyid);
	g_assert (key == GCR_GNUPG_KEY (object));

	if (!g_hash_table_remove (test->keys, keyid))
		g_assert_not_reached ();
}

static void
setup (Test *test, gconstpointer unused)
{
	GcrCollection *collection;
	gchar *directory;

	directory = g_get_current_dir ();
	test->directory = g_build_filename (directory, "files", "gnupg-homedir", NULL);

	collection = _gcr_gnupg_collection_new (test->directory);
	test->collection = GCR_GNUPG_COLLECTION (collection);

	test->keys = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
	g_signal_connect (collection, "added", G_CALLBACK (on_collection_added), test);
	g_signal_connect (collection, "removed", G_CALLBACK (on_collection_removed), test);

	g_free (directory);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_hash_table_destroy (test->keys);

	if (test->result)
		g_object_unref (test->result);

	g_object_unref (test->collection);
	g_free (test->directory);
}

static void
on_async_ready (GObject *source, GAsyncResult *res, gpointer user_data)
{
	Test *test = user_data;
	g_assert (G_OBJECT (test->collection) == source);
	g_assert (test->result == NULL);
	test->result = g_object_ref (res);
	egg_test_wait_stop ();
}

static void
test_properties (Test *test, gconstpointer unused)
{
	gchar *directory;
	g_object_get (test->collection, "directory", &directory, NULL);
	g_assert_cmpstr (directory, ==, test->directory);
	g_free (directory);
}

static void
test_load (Test *test, gconstpointer unused)
{
	GError *error = NULL;
	GcrGnupgKey *key;
	GList *l, *objects;
	GcrRecord *record;
	GHashTable *check;

	_gcr_gnupg_collection_load_async (test->collection, NULL, on_async_ready, test);
	egg_test_wait_until (500000);

	g_assert (test->result);
	_gcr_gnupg_collection_load_finish (test->collection, test->result, &error);
	g_assert_no_error (error);

	/* Werner Koch (a public key) */
	key = g_hash_table_lookup (test->keys, "5DE249965B0358A2");
	g_assert (GCR_IS_GNUPG_KEY (key));
	g_assert (_gcr_gnupg_key_get_secret_records (key) == NULL);

	/* Test Number 2 (a secret key)*/
	key = g_hash_table_lookup (test->keys, "268FEE686262C395");
	g_assert (GCR_IS_GNUPG_KEY (key));
	g_assert (_gcr_gnupg_key_get_secret_records (key));

	/* The length of collection should be correct */
	g_assert_cmpuint (g_hash_table_size (test->keys), ==,
	                  gcr_collection_get_length (GCR_COLLECTION (test->collection)));

	/* The list of objects should be correct */
	objects = gcr_collection_get_objects (GCR_COLLECTION (test->collection));
	g_assert_cmpuint (g_hash_table_size (test->keys), ==, g_list_length (objects));
	check = g_hash_table_new (g_str_hash, g_str_equal);
	for (l = objects; l != NULL; l = g_list_next (l)) {
		g_assert (GCR_IS_GNUPG_KEY (l->data));
		key = g_hash_table_lookup (test->keys, _gcr_gnupg_key_get_keyid (l->data));
		g_assert (key == l->data);
		g_hash_table_replace (check, (gchar*)_gcr_gnupg_key_get_keyid (l->data), "");
	}
	g_assert_cmpuint (g_hash_table_size (check), ==, g_hash_table_size (test->keys));
	g_hash_table_destroy (check);
	g_list_free (objects);

	/* Phillip R. Zimmerman's key should have a photo */
	key = g_hash_table_lookup (test->keys, "C7463639B2D7795E");
	g_assert (GCR_IS_GNUPG_KEY (key));
	record = _gcr_record_find (_gcr_gnupg_key_get_public_records (key), GCR_RECORD_SCHEMA_XA1);
	g_assert (record);
}

static void
test_reload (Test *test, gconstpointer unused)
{
	GError *error = NULL;
	GcrGnupgKey *key;

	_gcr_gnupg_collection_load_async (test->collection, NULL, on_async_ready, test);
	egg_test_wait_until (500);
	g_assert (test->result);
	_gcr_gnupg_collection_load_finish (test->collection, test->result, &error);
	g_assert_no_error (error);

	g_object_unref (test->result);
	test->result = NULL;

	_gcr_gnupg_collection_load_async (test->collection, NULL, on_async_ready, test);
	egg_test_wait_until (500000);
	g_assert (test->result);
	_gcr_gnupg_collection_load_finish (test->collection, test->result, &error);
	g_assert_no_error (error);

	/* Werner Koch (a public key) */
	key = g_hash_table_lookup (test->keys, "5DE249965B0358A2");
	g_assert (GCR_IS_GNUPG_KEY (key));
	g_assert (_gcr_gnupg_key_get_secret_records (key) == NULL);

	/* Test Number 2 (a secret key)*/
	key = g_hash_table_lookup (test->keys, "268FEE686262C395");
	g_assert (GCR_IS_GNUPG_KEY (key));
	g_assert (_gcr_gnupg_key_get_secret_records (key));
}

int
main (int argc, char **argv)
{
	const gchar *srcdir;

	g_type_init ();
	g_test_init (&argc, &argv, NULL);
	g_set_prgname ("test-gnupg-collection");

	srcdir = g_getenv ("SRCDIR");
	if (srcdir && chdir (srcdir) < 0)
		g_error ("couldn't change directory to: %s: %s", srcdir, g_strerror (errno));

	g_test_add ("/gcr/gnupg-collection/properties", Test, NULL, setup, test_properties, teardown);
	g_test_add ("/gcr/gnupg-collection/load", Test, NULL, setup, test_load, teardown);
	g_test_add ("/gcr/gnupg-collection/reload", Test, NULL, setup, test_reload, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
