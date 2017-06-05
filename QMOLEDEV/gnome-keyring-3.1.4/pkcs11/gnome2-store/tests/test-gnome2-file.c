/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-file-store.c: Test file store functionality

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

#include "config.h"

#include "gnome2-store/gkm-gnome2-file.h"

#include "gkm/gkm-object.h"

#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"

#include <glib/gstdio.h>

#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct {
	GkmGnome2File *data_file;
	gchar *public_filename;
	gchar *private_filename;
	gchar *write_filename;
	int write_fd;
	int public_fd;
	int private_fd;
	GkmSecret *login;
} Test;

EGG_SECURE_GLIB_DEFINITIONS ();

static void
setup (Test *test, gconstpointer unused)
{
	test->public_filename = g_build_filename (SRCDIR "/files", "data-file-public.store", NULL);
	test->private_filename = g_build_filename (SRCDIR "/files", "data-file-private.store", NULL);
	test->write_filename = g_build_filename ("/tmp", "unit-test-file.store", NULL);

	test->data_file = gkm_gnome2_file_new ();

	test->public_fd = g_open (test->public_filename, O_RDONLY, 0);
	test->private_fd = g_open (test->private_filename, O_RDONLY, 0);
	test->write_fd = g_open (test->write_filename, O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
	g_assert (test->write_fd != -1);

	test->login = gkm_secret_new ((guchar*)"booo", 4);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_free (test->public_filename);
	g_free (test->private_filename);
	g_free (test->write_filename);

	g_object_unref (test->data_file);

	if (test->public_fd != -1)
		close (test->public_fd);
	if (test->private_fd != -1)
		close (test->private_fd);
	if (test->write_fd != -1)
		close (test->write_fd);
	test->public_fd = test->private_fd = test->write_fd = -1;

	g_object_unref (test->login);
}

static void
test_file_create (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	res = gkm_gnome2_file_create_entry (test->data_file, "identifier-public", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Should be able to create private in a new file */
	res = gkm_gnome2_file_create_entry (test->data_file, "identifier-public", GKM_GNOME2_FILE_SECTION_PRIVATE);
	g_assert (res == GKM_DATA_SUCCESS);
}

static void
test_file_write_value (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	/* Can't write when no identifier present */
	res = gkm_gnome2_file_write_value (test->data_file, "identifier-public", CKA_LABEL, "public-label", 12);
	g_assert (res == GKM_DATA_UNRECOGNIZED);

	res = gkm_gnome2_file_create_entry (test->data_file, "identifier-public", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Should be able to write now */
	res = gkm_gnome2_file_write_value (test->data_file, "identifier-public", CKA_LABEL, "public-label", 12);
	g_assert (res == GKM_DATA_SUCCESS);
}

static void
test_file_read_value (Test *test, gconstpointer unused)
{
	gconstpointer value = NULL;
	GkmDataResult res;
	gsize n_value;
	guint number = 7778;

	/* Write some stuff in */
	res = gkm_gnome2_file_create_entry (test->data_file, "ident", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);
	res = gkm_gnome2_file_write_value (test->data_file, "ident", CKA_LABEL, "TWO-label", 10);
	g_assert (res == GKM_DATA_SUCCESS);
	res = gkm_gnome2_file_write_value (test->data_file, "ident", CKA_VALUE, &number, sizeof (number));
	g_assert (res == GKM_DATA_SUCCESS);

	/* Read for an invalid item */
	res = gkm_gnome2_file_read_value (test->data_file, "non-existant", CKA_LABEL, &value, &n_value);
	g_assert (res == GKM_DATA_UNRECOGNIZED);

	/* Read for an invalid attribute */
	res = gkm_gnome2_file_read_value (test->data_file, "ident", CKA_ID, &value, &n_value);
	g_assert (res == GKM_DATA_UNRECOGNIZED);

	/* Read out a valid number */
	res = gkm_gnome2_file_read_value (test->data_file, "ident", CKA_VALUE, &value, &n_value);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (value);
	g_assert (n_value == sizeof (number));
	g_assert_cmpuint (*((guint*)value), ==, number);

	/* Read out the valid string */
	res = gkm_gnome2_file_read_value (test->data_file, "ident", CKA_LABEL, &value, &n_value);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (value);
	g_assert (n_value == 10);
	g_assert_cmpstr ((const gchar*)value, ==, "TWO-label");
}

static void
test_file_read (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);
}

static void
test_file_lookup (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	guint section;
	gboolean ret;

	/* Invalid shouldn't succeed */
	ret = gkm_gnome2_file_lookup_entry (test->data_file, "non-existant", &section);
	g_assert (ret == FALSE);

	/* Create a test item */
	res = gkm_gnome2_file_create_entry (test->data_file, "test-ident", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);

	ret = gkm_gnome2_file_lookup_entry (test->data_file, "test-ident", &section);
	g_assert (ret == TRUE);
	g_assert (section == GKM_GNOME2_FILE_SECTION_PUBLIC);

	/* Should be able to call without asking for section */
	ret = gkm_gnome2_file_lookup_entry (test->data_file, "test-ident", NULL);
	g_assert (ret == TRUE);
}

static void
file_read_private_without_login (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	guint section;
	gconstpointer value;
	gsize n_value;
	gboolean ret;

	res = gkm_gnome2_file_read_fd (test->data_file, test->private_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Items from the private section should exist */
	ret = gkm_gnome2_file_lookup_entry (test->data_file, "identifier-private", &section);
	g_assert (ret);
	g_assert (section == GKM_GNOME2_FILE_SECTION_PRIVATE);

	/* But we shouldn't be able to read values from those private items */
	ret = gkm_gnome2_file_read_value (test->data_file, "identifier-private", CKA_LABEL, &value, &n_value);
	g_assert (ret == GKM_DATA_LOCKED);

	/* Shouldn't be able to create private items */
	res = gkm_gnome2_file_create_entry (test->data_file, "dummy-private", GKM_GNOME2_FILE_SECTION_PRIVATE);
	g_assert (res == GKM_DATA_LOCKED);

	/* Shouldn't be able to write with another test->login */
	res = gkm_gnome2_file_write_fd (test->data_file, test->write_fd, test->login);
	g_assert (res == GKM_DATA_LOCKED);

	/* Now load a public file without private bits*/
	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Now we should be able to load private stuff */
	res = gkm_gnome2_file_create_entry (test->data_file, "dummy-private", GKM_GNOME2_FILE_SECTION_PRIVATE);
	g_assert (res == GKM_DATA_SUCCESS);
}

static void
test_file_write (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	res = gkm_gnome2_file_create_entry (test->data_file, "identifier-public", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);

	res = gkm_gnome2_file_write_value (test->data_file, "identifier-public", CKA_LABEL, "public-label", 12);
	g_assert (res == GKM_DATA_SUCCESS);

	res = gkm_gnome2_file_create_entry (test->data_file, "identifier-two", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);

	res = gkm_gnome2_file_write_fd (test->data_file, test->write_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);
}

static void
test_cant_write_private_without_login (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	res = gkm_gnome2_file_create_entry (test->data_file, "identifier_private", GKM_GNOME2_FILE_SECTION_PRIVATE);
	g_assert (res == GKM_DATA_SUCCESS);

	res = gkm_gnome2_file_write_fd (test->data_file, test->write_fd, NULL);
	g_assert (res == GKM_DATA_LOCKED);
}

static void
test_write_private_with_login (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	gulong value;

	res = gkm_gnome2_file_create_entry (test->data_file, "identifier-public", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);
	res = gkm_gnome2_file_write_value (test->data_file, "identifier-public", CKA_LABEL, "public-label", 12);
	g_assert (res == GKM_DATA_SUCCESS);

	res = gkm_gnome2_file_create_entry (test->data_file, "identifier-two", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);
	res = gkm_gnome2_file_write_value (test->data_file, "identifier-two", CKA_LABEL, "TWO-label", 9);
	g_assert (res == GKM_DATA_SUCCESS);
	value = 555;
	res = gkm_gnome2_file_write_value (test->data_file, "identifier-two", CKA_VALUE, &value, sizeof (value));
	g_assert (res == GKM_DATA_SUCCESS);

	res = gkm_gnome2_file_create_entry (test->data_file, "identifier-private", GKM_GNOME2_FILE_SECTION_PRIVATE);
	g_assert (res == GKM_DATA_SUCCESS);
	res = gkm_gnome2_file_write_value (test->data_file, "identifier-private", CKA_LABEL, "private-label", 13);
	g_assert (res == GKM_DATA_SUCCESS);

	res = gkm_gnome2_file_write_fd (test->data_file, test->write_fd, test->login);
	g_assert (res == GKM_DATA_SUCCESS);
}

static void
test_read_private_with_login (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	gconstpointer value;
	gsize n_value;

	res = gkm_gnome2_file_read_fd (test->data_file, test->private_fd, test->login);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Should be able to read private items */
	res = gkm_gnome2_file_read_value (test->data_file, "identifier-private", CKA_LABEL, &value, &n_value);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert_cmpuint (n_value, ==, 13);
	g_assert (memcmp (value, "private-label", 13) == 0);
}

static void
test_destroy_entry (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	res = gkm_gnome2_file_destroy_entry (test->data_file, "non-existant");
	g_assert (res == GKM_DATA_UNRECOGNIZED);

	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Make sure it's here */
	g_assert (gkm_gnome2_file_lookup_entry (test->data_file, "identifier-public", NULL));

	res = gkm_gnome2_file_destroy_entry (test->data_file, "identifier-public");
	g_assert (res == GKM_DATA_SUCCESS);

	/* Make sure it's gone */
	g_assert (!gkm_gnome2_file_lookup_entry (test->data_file, "identifier-public", NULL));
}

static void
test_destroy_entry_by_loading (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	/* Create some extra idenifiers */
	res = gkm_gnome2_file_create_entry (test->data_file, "my-public", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);
	res = gkm_gnome2_file_create_entry (test->data_file, "my-private", GKM_GNOME2_FILE_SECTION_PRIVATE);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Now read from the file */
	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Both should be gone */
	g_assert (!gkm_gnome2_file_lookup_entry (test->data_file, "my-public", NULL));
	g_assert (!gkm_gnome2_file_lookup_entry (test->data_file, "my-private", NULL));
}


static void
test_destroy_private_without_login (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	res = gkm_gnome2_file_read_fd (test->data_file, test->private_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Make sure it's here */
	g_assert (gkm_gnome2_file_lookup_entry (test->data_file, "identifier-private", NULL));

	/* Shouldn't be able to destroy */
	res = gkm_gnome2_file_destroy_entry (test->data_file, "identifier-private");
	g_assert (res == GKM_DATA_LOCKED);

	/* Make sure it's still here */
	g_assert (gkm_gnome2_file_lookup_entry (test->data_file, "identifier-private", NULL));
}

static void
entry_added_one (GkmGnome2File *df, const gchar *identifier, gboolean *added)
{
	g_assert (GKM_IS_GNOME2_FILE (df));
	g_assert (identifier);
	g_assert (added);

	/* Should only be called once */
	g_assert (!*added);
	*added = TRUE;
}

static void
test_entry_added_signal (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	gboolean added;

	g_signal_connect (test->data_file, "entry-added", G_CALLBACK (entry_added_one), &added);

	/* Should fire the signal */
	added = FALSE;
	res = gkm_gnome2_file_create_entry (test->data_file, "identifier-public", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (added == TRUE);

	/* Another one should be added when we load */
	added = FALSE;
	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (added == TRUE);
}

static void
entry_changed_one (GkmGnome2File *df, const gchar *identifier, gulong type, gboolean *changed)
{
	g_assert (GKM_IS_GNOME2_FILE (df));
	g_assert (identifier);
	g_assert (changed);
	g_assert (type == CKA_LABEL);

	/* Should only be called once */
	g_assert (!*changed);
	*changed = TRUE;
}

static void
test_entry_changed_signal (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	gboolean changed;

	g_signal_connect (test->data_file, "entry-changed", G_CALLBACK (entry_changed_one), &changed);

	/* Loading shouldn't fire the signal */
	changed = FALSE;
	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (changed == FALSE);

	/* Shouldn't fire the signal on nonexistant */
	changed = FALSE;
	res = gkm_gnome2_file_write_value (test->data_file, "non-existant", CKA_LABEL, "new-value", 10);
	g_assert (res == GKM_DATA_UNRECOGNIZED);
	g_assert (changed == FALSE);

	/* Should fire the signal */
	changed = FALSE;
	res = gkm_gnome2_file_write_value (test->data_file, "identifier-public", CKA_LABEL, "new-value", 10);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (changed == TRUE);

	/* Shouldn't fire the signal, same value again */
	changed = FALSE;
	res = gkm_gnome2_file_write_value (test->data_file, "identifier-public", CKA_LABEL, "new-value", 10);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (changed == FALSE);

	/* Reload file, should revert, fire signal */
	changed = FALSE;
	g_assert (lseek (test->public_fd, 0, SEEK_SET) != -1);
	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (changed == TRUE);
}

static void
entry_removed_one (GkmGnome2File *df, const gchar *identifier, gboolean *removed)
{
	g_assert (GKM_IS_GNOME2_FILE (df));
	g_assert (identifier);
	g_assert (removed);

	/* Should only be called once */
	g_assert (!*removed);
	*removed = TRUE;
}

static void
test_entry_removed_signal (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	gboolean removed;

	g_signal_connect (test->data_file, "entry-removed", G_CALLBACK (entry_removed_one), &removed);

	/* Loading shouldn't fire the signal */
	removed = FALSE;
	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (removed == FALSE);

	/* Shouldn't fire the signal on removing nonexistant */
	removed = FALSE;
	res = gkm_gnome2_file_destroy_entry (test->data_file, "non-existant");
	g_assert (res == GKM_DATA_UNRECOGNIZED);
	g_assert (removed == FALSE);

	/* Remove a real entry */
	removed = FALSE;
	res = gkm_gnome2_file_destroy_entry (test->data_file, "identifier-public");
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (removed == TRUE);

	/* Add a dummy entry */
	res = gkm_gnome2_file_create_entry (test->data_file, "extra-dummy", GKM_GNOME2_FILE_SECTION_PUBLIC);
	g_assert (res == GKM_DATA_SUCCESS);

	/* That one should go away when we reload, fire signal */
	removed = FALSE;
	g_assert (lseek (test->public_fd, 0, SEEK_SET) != -1);
	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (removed == TRUE);
}

static void
foreach_entry (GkmGnome2File *df, const gchar *identifier, gpointer data)
{
	GPtrArray *array = data;
	const gchar *ident;
	int i;

	g_assert (data);
	g_assert (identifier);
	g_assert (GKM_IS_GNOME2_FILE (df));

	/* Check that this is unique */
	for (i = 0; i < array->len; ++i) {
		ident = g_ptr_array_index (array, i);
		g_assert (ident);
		g_assert_cmpstr (ident, !=, identifier);
	}

	/* Add it */
	g_ptr_array_add (array, g_strdup (identifier));
}

static void
test_data_file_foreach (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	GPtrArray *array;

	res = gkm_gnome2_file_read_fd (test->data_file, test->private_fd, test->login);
	g_assert (res == GKM_DATA_SUCCESS);

	array = g_ptr_array_new ();
	gkm_gnome2_file_foreach_entry (test->data_file, foreach_entry, array);
	g_assert (array->len == 4);

	g_ptr_array_add (array, NULL);
	g_strfreev ((gchar**)g_ptr_array_free (array, FALSE));
}

static void
test_unique_entry (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	gchar *identifier;

	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Should change an identifier that conflicts */
	identifier = g_strdup ("identifier-public");
	res = gkm_gnome2_file_unique_entry (test->data_file, &identifier);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert_cmpstr (identifier, !=, "identifier-public");
	g_free (identifier);

	/* Shouldn't change a unique identifier */
	identifier = g_strdup ("identifier-unique");
	res = gkm_gnome2_file_unique_entry (test->data_file, &identifier);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert_cmpstr (identifier, ==, "identifier-unique");
	g_free (identifier);

	/* Should be able to get from NULL */
	identifier = NULL;
	res = gkm_gnome2_file_unique_entry (test->data_file, &identifier);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (identifier != NULL);
	g_assert (identifier[0] != 0);
	g_free (identifier);
}

static void
test_have_sections (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);

	/* No private section */
	g_assert (gkm_gnome2_file_have_section (test->data_file, GKM_GNOME2_FILE_SECTION_PUBLIC));
	g_assert (!gkm_gnome2_file_have_section (test->data_file, GKM_GNOME2_FILE_SECTION_PRIVATE));

	/* Read private stuff into file, without test->login */
	res = gkm_gnome2_file_read_fd (test->data_file, test->private_fd, NULL);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Should have a private section even without test->login */
	g_assert (gkm_gnome2_file_have_section (test->data_file, GKM_GNOME2_FILE_SECTION_PUBLIC));
	g_assert (gkm_gnome2_file_have_section (test->data_file, GKM_GNOME2_FILE_SECTION_PRIVATE));

	/* Read private stuff into file, with test->login */
	g_assert (lseek (test->private_fd, 0, SEEK_SET) == 0);
	res = gkm_gnome2_file_read_fd (test->data_file, test->private_fd, test->login);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Should have a private section now with test->login */
	g_assert (gkm_gnome2_file_have_section (test->data_file, GKM_GNOME2_FILE_SECTION_PUBLIC));
	g_assert (gkm_gnome2_file_have_section (test->data_file, GKM_GNOME2_FILE_SECTION_PRIVATE));

	/* Read public stuff back into file*/
	g_assert (lseek (test->public_fd, 0, SEEK_SET) == 0);
	res = gkm_gnome2_file_read_fd (test->data_file, test->public_fd, test->login);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Shouldn't have a private section now  */
	g_assert (gkm_gnome2_file_have_section (test->data_file, GKM_GNOME2_FILE_SECTION_PUBLIC));
	g_assert (!gkm_gnome2_file_have_section (test->data_file, GKM_GNOME2_FILE_SECTION_PRIVATE));
}


int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	egg_libgcrypt_initialize ();

	g_test_add ("/gnome2-store/gnome2-file/file_create", Test, NULL, setup, test_file_create, teardown);
	g_test_add ("/gnome2-store/gnome2-file/file_write_value", Test, NULL, setup, test_file_write_value, teardown);
	g_test_add ("/gnome2-store/gnome2-file/file_read_value", Test, NULL, setup, test_file_read_value, teardown);
	g_test_add ("/gnome2-store/gnome2-file/file_read", Test, NULL, setup, test_file_read, teardown);
	g_test_add ("/gnome2-store/gnome2-file/file_lookup", Test, NULL, setup, test_file_lookup, teardown);
	g_test_add ("/gnome2-store/gnome2-file/file_read_private_without_login", Test, NULL, setup, file_read_private_without_login, teardown);
	g_test_add ("/gnome2-store/gnome2-file/file_write", Test, NULL, setup, test_file_write, teardown);
	g_test_add ("/gnome2-store/gnome2-file/cant_write_private_without_login", Test, NULL, setup, test_cant_write_private_without_login, teardown);
	g_test_add ("/gnome2-store/gnome2-file/write_private_with_login", Test, NULL, setup, test_write_private_with_login, teardown);
	g_test_add ("/gnome2-store/gnome2-file/read_private_with_login", Test, NULL, setup, test_read_private_with_login, teardown);
	g_test_add ("/gnome2-store/gnome2-file/destroy_entry", Test, NULL, setup, test_destroy_entry, teardown);
	g_test_add ("/gnome2-store/gnome2-file/destroy_entry_by_loading", Test, NULL, setup, test_destroy_entry_by_loading, teardown);
	g_test_add ("/gnome2-store/gnome2-file/destroy_private_without_login", Test, NULL, setup, test_destroy_private_without_login, teardown);
	g_test_add ("/gnome2-store/gnome2-file/entry_added_signal", Test, NULL, setup, test_entry_added_signal, teardown);
	g_test_add ("/gnome2-store/gnome2-file/entry_changed_signal", Test, NULL, setup, test_entry_changed_signal, teardown);
	g_test_add ("/gnome2-store/gnome2-file/entry_removed_signal", Test, NULL, setup, test_entry_removed_signal, teardown);
	g_test_add ("/gnome2-store/gnome2-file/data_file_foreach", Test, NULL, setup, test_data_file_foreach, teardown);
	g_test_add ("/gnome2-store/gnome2-file/unique_entry", Test, NULL, setup, test_unique_entry, teardown);
	g_test_add ("/gnome2-store/gnome2-file/have_sections", Test, NULL, setup, test_have_sections, teardown);

	return g_test_run ();
}
