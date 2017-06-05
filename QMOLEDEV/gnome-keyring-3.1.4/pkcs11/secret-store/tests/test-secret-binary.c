/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-secret-binary.c: Test binary keyring read and write

   Copyright (C) 2009 Stefan Walter

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

#include "mock-secret-module.h"

#include "secret-store/gkm-secret-binary.h"
#include "secret-store/gkm-secret-collection.h"
#include "secret-store/gkm-secret-data.h"
#include "secret-store/gkm-secret-fields.h"
#include "secret-store/gkm-secret-item.h"

#include "gkm/gkm-secret.h"

#include "pkcs11/pkcs11i.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	GkmModule *module;
	GkmSession *session;
	GkmSecretCollection *collection;
	GkmSecretData *sdata;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	GkmSecret *master;

	test->module = test_secret_module_initialize_and_enter ();
	test->session = test_secret_module_open_session (TRUE);

	test->collection = g_object_new (GKM_TYPE_SECRET_COLLECTION,
	                           "module", test->module,
	                           "identifier", "test",
	                           "label", "brigadooooooooooooon",
	                           NULL);

	test->sdata = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	master = gkm_secret_new_from_password ("my-keyring-password");
	gkm_secret_data_set_master (test->sdata, master);
	g_object_unref (master);

	g_assert (GKM_IS_SECRET_COLLECTION (test->collection));

}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->collection);
	g_object_unref (test->sdata);
	test_secret_module_leave_and_finalize ();
}

static GkmDataResult
check_read_keyring_file (Test *test, const gchar *path)
{
	GkmDataResult res;
	gchar *data;
	gsize n_data;

	if (!g_file_get_contents (path, &data, &n_data, NULL))
		g_assert_not_reached ();
	res = gkm_secret_binary_read (test->collection, test->sdata, data, n_data);
	g_free (data);

	return res;
}


static void
test_read_encrypted (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	res = check_read_keyring_file (test, SRCDIR "/files/encrypted.keyring");
	g_assert (res == GKM_DATA_SUCCESS);

	test_secret_collection_validate (test->collection, test->sdata);
}

static void
test_read_wrong_format (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	res = check_read_keyring_file (test, SRCDIR "/files/plain.keyring");
	g_assert (res == GKM_DATA_UNRECOGNIZED);
}

static void
test_read_wrong_master (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	GkmSecret *master;

	master = gkm_secret_new_from_password ("wrong");
	gkm_secret_data_set_master (test->sdata, master);
	g_object_unref (master);

	res = check_read_keyring_file (test, SRCDIR "/files/encrypted.keyring");
	g_assert (res == GKM_DATA_LOCKED);
}

static void
test_read_sdata_but_no_master (Test *test, gconstpointer unused)
{
	GkmDataResult res;

	gkm_secret_data_set_master (test->sdata, NULL);

	res = check_read_keyring_file (test, SRCDIR "/files/encrypted.keyring");
	g_assert (res == GKM_DATA_LOCKED);
}

static void
test_write (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	gpointer data;
	gsize n_data;

	test_secret_collection_populate (test->collection, test->sdata);

	res = gkm_secret_binary_write (test->collection, test->sdata, &data, &n_data);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (data);
	g_assert (n_data);

	/* Try parsing it again */
	res = gkm_secret_binary_read (test->collection, test->sdata, data, n_data);
	g_assert (res == GKM_DATA_SUCCESS);
}

static void
test_remove_unavailable (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	GList *items;
	gchar *data;
	gsize n_data;

	if (!g_file_get_contents (SRCDIR "/files/encrypted.keyring", &data, &n_data, NULL))
		g_assert_not_reached ();
	res = gkm_secret_binary_read (test->collection, test->sdata, data, n_data);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Two items from the file */
	items = gkm_secret_collection_get_items (test->collection);
	g_assert_cmpint (g_list_length (items), ==, 2);
	g_list_free (items);

	/* Fill in some more data */
	test_secret_collection_populate (test->collection, test->sdata);

	/* Should have added three more */
	items = gkm_secret_collection_get_items (test->collection);
	g_assert_cmpint (g_list_length (items), ==, 5);
	g_list_free (items);

	/* Re-read the keyring */
	res = gkm_secret_binary_read (test->collection, test->sdata, data, n_data);
	g_assert (res == GKM_DATA_SUCCESS);

	/* And we're back to two */
	items = gkm_secret_collection_get_items (test->collection);
	g_assert_cmpint (g_list_length (items), ==, 2);
	g_list_free (items);

	g_free (data);
}

static void
test_read_created_on_solaris_opencsw (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	GkmSecret *master;

	master = gkm_secret_new_from_password ("test");
	gkm_secret_data_set_master (test->sdata, master);
	g_object_unref (master);

	res = check_read_keyring_file (test, SRCDIR "/files/created-on-solaris-opencsw.keyring");
	g_assert_cmpint (res, ==, GKM_DATA_SUCCESS);
}

static void
test_read_created_on_rhel (Test *test, gconstpointer unused)
{
	GkmDataResult res;
	GkmSecret *master;

	master = gkm_secret_new_from_password ("test");
	gkm_secret_data_set_master (test->sdata, master);
	g_object_unref (master);

	res = check_read_keyring_file (test, SRCDIR "/files/created-on-rhel.keyring");
	g_assert_cmpint (res, ==, GKM_DATA_SUCCESS);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/secret-store/binary/read_encrypted", Test, NULL, setup, test_read_encrypted, teardown);
	g_test_add ("/secret-store/binary/read_wrong_format", Test, NULL, setup, test_read_wrong_format, teardown);
	g_test_add ("/secret-store/binary/read_wrong_master", Test, NULL, setup, test_read_wrong_master, teardown);
	g_test_add ("/secret-store/binary/read_sdata_but_no_master", Test, NULL, setup, test_read_sdata_but_no_master, teardown);
	g_test_add ("/secret-store/binary/write", Test, NULL, setup, test_write, teardown);
	g_test_add ("/secret-store/binary/remove_unavailable", Test, NULL, setup, test_remove_unavailable, teardown);
	g_test_add ("/secret-store/binary/created_on_rhel", Test, NULL, setup, test_read_created_on_rhel, teardown);
	g_test_add ("/secret-store/binary/created_on_solaris_opencsw", Test, NULL, setup, test_read_created_on_solaris_opencsw, teardown);

	return g_test_run ();
}
