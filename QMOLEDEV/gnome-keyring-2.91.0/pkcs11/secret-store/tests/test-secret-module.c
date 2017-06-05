/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-secret-module.c: A test PKCS#11 module implementation

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
#include "test-secret-module.h"
#include "test-suite.h"

#include "gkm/gkm-secret.h"
#include "gkm/gkm-module.h"

#include "gkm-secret-collection.h"
#include "gkm-secret-data.h"
#include "gkm-secret-fields.h"
#include "gkm-secret-item.h"
#include "gkm-secret-object.h"
#include "gkm-secret-store.h"

#include <string.h>

static GMutex *mutex = NULL;

GkmModule*  _gkm_secret_store_get_module_for_testing (void);
GMutex* _gkm_module_get_scary_mutex_that_you_should_not_touch (GkmModule *module);

static void
copy_scratch_file (const gchar *basename)
{
	gchar *filename;
	gchar *data;
	gsize n_data;

	filename = testing_data_filename (basename);
	if (!g_file_get_contents (filename, &data, &n_data, NULL)) {
		g_warning ("couldn't read: %s", filename);
		g_return_if_reached ();
	}
	g_free (filename);

	filename = testing_scratch_filename (basename);
	if (!g_file_set_contents (filename, data, n_data, NULL))
		g_return_if_reached ();
	g_free (filename);
	g_free (data);
}

GkmModule*
test_secret_module_initialize_and_enter (void)
{
	CK_FUNCTION_LIST_PTR funcs;
	CK_C_INITIALIZE_ARGS args;
	GkmModule *module;
	gchar *string;
	CK_RV rv;

	/* Setup test directory to work in */
	memset (&args, 0, sizeof (args));
	string = g_strdup_printf ("directory='%s'", testing_scratch_directory ());
	args.pReserved = string;
	args.flags = CKF_OS_LOCKING_OK;

	/* Copy files from test-data to scratch */
	copy_scratch_file ("encrypted.keyring");
	copy_scratch_file ("plain.keyring");

	funcs = gkm_secret_store_get_functions ();
	rv = (funcs->C_Initialize) (&args);
	g_return_val_if_fail (rv == CKR_OK, NULL);

	module = _gkm_secret_store_get_module_for_testing ();
	g_return_val_if_fail (module, NULL);

	mutex = _gkm_module_get_scary_mutex_that_you_should_not_touch (module);
	test_secret_module_enter ();

	g_free (string);

	return module;
}

void
test_secret_module_leave_and_finalize (void)
{
	CK_FUNCTION_LIST_PTR funcs;
	CK_RV rv;

	test_secret_module_leave ();

	funcs = gkm_secret_store_get_functions ();
	rv = (funcs->C_Finalize) (NULL);
	g_return_if_fail (rv == CKR_OK);
}

void
test_secret_module_leave (void)
{
	g_assert (mutex);
	g_mutex_unlock (mutex);
}

void
test_secret_module_enter (void)
{
	g_assert (mutex);
	g_mutex_lock (mutex);
}

GkmSession*
test_secret_module_open_session (gboolean writable)
{
	CK_ULONG flags = CKF_SERIAL_SESSION;
	CK_SESSION_HANDLE handle;
	GkmModule *module;
	GkmSession *session;
	CK_RV rv;

	module = _gkm_secret_store_get_module_for_testing ();
	g_return_val_if_fail (module, NULL);

	if (writable)
		flags |= CKF_RW_SESSION;

	rv = gkm_module_C_OpenSession (module, 1, flags, NULL, NULL, &handle);
	g_assert (rv == CKR_OK);

	rv = gkm_module_C_Login (module, handle, CKU_USER, NULL, 0);
	g_assert (rv == CKR_OK);

	session = gkm_module_lookup_session (module, handle);
	g_assert (session);

	return session;
}

/* Validates plain.keyring and encrypted.keyring in test-data */
void
test_secret_collection_validate (GkmSecretCollection *collection, GkmSecretData *sdata)
{
	GkmSecretItem* item;
	GkmSecretObject *obj;
	GHashTable *fields;
	const gchar *value;
	GkmSecret *secret;
	GList *items;
	glong when;
	guint32 num;

	obj = GKM_SECRET_OBJECT (collection);

	/* The keyring itself */
	/* "Missing keyring name" */
	value = gkm_secret_object_get_label (obj);
	g_assert (value != NULL);
	/* "Invalid keyring name" */
	g_assert_cmpstr (value, ==, "unit-test-keyring");
#if 0
	/* "Bad lock settings" */
	g_assert (!keyring->lock_on_idle && keyring->lock_timeout == 0);
#endif
	/* "Bad Creation Time" */
	when = gkm_secret_object_get_created (obj);
	g_assert (when == 1198027852);
	/* "Bad Modification Time" */
	when = gkm_secret_object_get_modified (obj);
	g_assert (when == 1198027852);
	/* "Wrong number of items" */
	items = gkm_secret_collection_get_items (collection);
	g_assert_cmpint (g_list_length (items), ==, 2);
	g_list_free (items);

	/* Item #2 */
	item = gkm_secret_collection_get_item (collection, "2");
	obj = GKM_SECRET_OBJECT (item);
	/* "Couldn't find item" */
	g_assert (item != NULL);
#if 0
	/* "Invalid item type" */
	g_assert_cmpint (item->type, ==, GNOME_KEYRING_ITEM_GENERIC_SECRET);
#endif
	/* "Missing secret" */
	secret = gkm_secret_data_get_secret (sdata, "2");
	g_assert (secret != NULL);
	/* "Wrong secret" */
	g_assert (gkm_secret_equals (secret, (guchar*)"item-secret", -1));
	/* "Bad Creation Time" */
	when = gkm_secret_object_get_created (obj);
	g_assert_cmpint (when, ==, 1198027852);

#if 0
	/* Item #2 ACL */
	/* "Bad ACLs" */
	g_assert_cmpint (g_list_length (item->acl), ==, 1);
	ac = (GnomeKeyringAccessControl*)item->acl->data;
	/* "Invalid ACL" */
	g_assert (ac && ac->application);
	/* "Invalid ACL Path" */
	g_assert (ac->application->pathname && strstr (ac->application->pathname, "test-suite"));
	/* "Invalid ACL Display Name" */
	g_assert (ac->application->display_name);
	g_assert_cmpstr (ac->application->display_name, ==, "test-suite");
	/* "Invalid ACL Access Type" */
	g_assert_cmpint (ac->types_allowed, ==, (GNOME_KEYRING_ACCESS_READ | GNOME_KEYRING_ACCESS_WRITE | GNOME_KEYRING_ACCESS_REMOVE));
#endif

	/* Item #3 */
	item = gkm_secret_collection_get_item (collection, "3");
	obj = GKM_SECRET_OBJECT (item);
	/* "Couldn't find item #3" */
	g_assert (item != NULL);
	fields = gkm_secret_item_get_fields (item);
	g_assert (fields != NULL);
	/* Make fields are the same */
	value = gkm_secret_fields_get (fields, "dog");
	g_assert_cmpstr (value, ==, "woof");
	value = gkm_secret_fields_get (fields, "bird");
	g_assert_cmpstr (value, ==, "cheep");
	value = gkm_secret_fields_get (fields, "iguana");
	g_assert_cmpstr (value, ==, "");
	g_assert (gkm_secret_fields_get_compat_uint32 (fields, "num", &num));
	g_assert_cmpuint (num, ==, 3);
#if 0
	/* "Invalid item type" */
	g_assert_cmpint (item->type, ==, GNOME_KEYRING_ITEM_GENERIC_SECRET);
#endif
	/* "Missing secret" */
	secret = gkm_secret_data_get_secret (sdata, "3");
	g_assert (secret != NULL);
	/* "Wrong secret" */
	g_assert (gkm_secret_equals (secret, (guchar*)"item-secret", -1));
}

/* Fills a collection with some junk data */
void
test_secret_collection_populate (GkmSecretCollection *collection, GkmSecretData *sdata)
{
	GkmSecretItem *item;
	GHashTable *fields;
	GkmSecret *secret;

	item = gkm_secret_collection_new_item (collection, "4");
	gkm_secret_object_set_label (GKM_SECRET_OBJECT (item), "Noises");
	secret = gkm_secret_new_from_password ("4's secret");
	gkm_secret_data_set_secret (sdata, "4", secret);
	g_object_unref (secret);
	fields = gkm_secret_item_get_fields (item);
	gkm_secret_fields_add (fields, "doggy", "fart");
	gkm_secret_fields_add (fields, "pig", "grunt");
	gkm_secret_fields_add_compat_uint32 (fields, "how-many", 292929);

	item = gkm_secret_collection_new_item (collection, "5");
	gkm_secret_object_set_label (GKM_SECRET_OBJECT (item), "Colors");
	secret = gkm_secret_new_from_password ("5's secret");
	gkm_secret_data_set_secret (sdata, "5", secret);
	g_object_unref (secret);
	fields = gkm_secret_item_get_fields (item);
	gkm_secret_fields_add (fields, "barney", "purple");
	gkm_secret_fields_add (fields, "piglet", "pink");
	gkm_secret_fields_add_compat_uint32 (fields, "number", 8);

	item = gkm_secret_collection_new_item (collection, "6");
	gkm_secret_object_set_label (GKM_SECRET_OBJECT (item), "Binary Secret");
	secret = gkm_secret_new ((guchar*)"binary\0secret", 13);
	gkm_secret_data_set_secret (sdata, "6", secret);
	g_object_unref (secret);
	fields = gkm_secret_item_get_fields (item);
	gkm_secret_fields_add (fields, "train", "zoom");
	gkm_secret_fields_add (fields, "hummer", NULL);
	gkm_secret_fields_add_compat_uint32 (fields, "number", 2);
}
