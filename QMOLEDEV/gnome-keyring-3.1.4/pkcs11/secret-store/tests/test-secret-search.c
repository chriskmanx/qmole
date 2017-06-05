/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/*
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

#include "secret-store/gkm-secret-collection.h"
#include "secret-store/gkm-secret-fields.h"
#include "secret-store/gkm-secret-item.h"
#include "secret-store/gkm-secret-search.h"

#include "gkm/gkm-session.h"
#include "gkm/gkm-transaction.h"

#include "pkcs11/pkcs11i.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	GkmModule *module;
	GkmSession *session;
	GkmFactory *factory;
	GkmSecretCollection *collection;
	GkmSecretItem *item;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	GHashTable *fields;

	test->module = test_secret_module_initialize_and_enter ();
	test->session = test_secret_module_open_session (TRUE);
	test->factory = GKM_FACTORY_SECRET_SEARCH;
	g_assert (test->factory);

	test->collection = g_object_new (GKM_TYPE_SECRET_COLLECTION,
	                           "module", test->module,
	                           "manager", gkm_session_get_manager (test->session),
	                           "identifier", "test-collection",
	                           NULL);

	/* Create an test->item */
	test->item = gkm_secret_collection_new_item (test->collection, "test-item");
	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add (fields, "name1", "value1");
	gkm_secret_fields_add (fields, "name2", "value2");
	gkm_secret_item_set_fields (test->item, fields);
	g_hash_table_unref (fields);

	gkm_object_expose (GKM_OBJECT (test->collection), TRUE);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->collection);
	test_secret_module_leave_and_finalize ();
}

static void
test_incomplete (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attrs[1];
	GkmObject *object = NULL;
	GkmTransaction *transaction = gkm_transaction_new ();

	object = gkm_session_create_object_for_factory (test->session, test->factory, transaction, attrs, 0);
	g_assert (gkm_transaction_complete_and_unref (transaction) == CKR_TEMPLATE_INCOMPLETE);
	g_assert (object == NULL);
}

static void
test_bad_fields (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attrs[] = {
	        { CKA_G_FIELDS, "bad-value", 9 },
	};

	GkmObject *object = NULL;
	GkmTransaction *transaction = gkm_transaction_new ();

	object = gkm_session_create_object_for_factory (test->session, test->factory, transaction, attrs, 1);
	g_assert (gkm_transaction_complete_and_unref (transaction) == CKR_ATTRIBUTE_VALUE_INVALID);
	g_assert (object == NULL);
}

static void
test_new (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attrs[] = {
	        { CKA_G_FIELDS, "test\0value\0two\0value2", 22 },
	};

	const gchar *identifier;
	GkmObject *object = NULL;
	GHashTable *fields;
	gpointer vdata;
	gulong vulong;
	gboolean vbool;
	gsize vsize;

	object = gkm_session_create_object_for_factory (test->session, test->factory, NULL, attrs, 1);
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_SEARCH (object));

	if (!gkm_object_get_attribute_ulong (object, test->session, CKA_CLASS, &vulong))
		g_assert_not_reached ();
	g_assert (vulong == CKO_G_SEARCH);

	if (!gkm_object_get_attribute_boolean (object, test->session, CKA_MODIFIABLE, &vbool))
		g_assert_not_reached ();
	g_assert (vbool == CK_TRUE);

	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_FIELDS, &vsize);
	g_assert (vdata);
	g_assert (vsize == attrs[0].ulValueLen);
	g_free (vdata);

	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_COLLECTION, &vsize);
	g_assert (vdata);
	g_assert (vsize == 0);
	g_free (vdata);

	/* No objects matched */
	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_MATCHED, &vsize);
	g_assert (vdata);
	g_assert (vsize == 0);
	g_free (vdata);

	/* Get the fields object and check */
	fields = gkm_secret_search_get_fields (GKM_SECRET_SEARCH (object));
	g_assert (fields);
	g_assert_cmpstr (gkm_secret_fields_get (fields, "test"), ==, "value");

	/* No test->collection */
	identifier = gkm_secret_search_get_collection_id (GKM_SECRET_SEARCH (object));
	g_assert (identifier == NULL);

	g_object_unref (object);
}

static void
test_and_match (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attrs[] = {
	        { CKA_G_FIELDS, "name1\0value1\0name2\0value2", 26 },
	};

	GkmObject *object = NULL;
	gpointer vdata;
	gsize vsize;

	object = gkm_session_create_object_for_factory (test->session, test->factory, NULL, attrs, 1);
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_SEARCH (object));

	/* One object matched */
	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_MATCHED, &vsize);
	g_assert (vdata);
	g_assert (vsize == sizeof (CK_OBJECT_HANDLE));
	g_assert (*((CK_OBJECT_HANDLE_PTR)vdata) == gkm_object_get_handle (GKM_OBJECT (test->item)));
	g_free (vdata);

	g_object_unref (object);
}

static void
test_and_change_to_match (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attrs[] = {
	        { CKA_G_FIELDS, "name1\0value1", 13 },
	};

	GkmObject *object = NULL;
	GHashTable *fields;
	gpointer vdata;
	gsize vsize;

	/* Make it not match */
	fields = gkm_secret_fields_new ();
	gkm_secret_item_set_fields (test->item, fields);
	g_hash_table_unref (fields);

	object = gkm_session_create_object_for_factory (test->session, test->factory, NULL, attrs, 1);
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_SEARCH (object));

	/* Nothing matched */
	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_MATCHED, &vsize);
	g_assert (vsize == 0);
	g_free (vdata);

	/* Make it match */
	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add (fields, "name1", "value1");
	gkm_secret_fields_add (fields, "name2", "value2");
	gkm_secret_item_set_fields (test->item, fields);
	g_hash_table_unref (fields);

	/* One object matched */
	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_MATCHED, &vsize);
	g_assert (vdata);
	g_assert (vsize == sizeof (CK_OBJECT_HANDLE));
	g_assert (*((CK_OBJECT_HANDLE_PTR)vdata) == gkm_object_get_handle (GKM_OBJECT (test->item)));
	g_free (vdata);

	g_object_unref (object);
}

static void
test_and_change_to_not_match (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attrs[] = {
	        { CKA_G_FIELDS, "name1\0value1", 13 },
	};

	GkmObject *object = NULL;
	GHashTable *fields;
	gpointer vdata;
	gsize vsize;

	object = gkm_session_create_object_for_factory (test->session, test->factory, NULL, attrs, 1);
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_SEARCH (object));

	/* One object matched */
	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_MATCHED, &vsize);
	g_assert (vdata);
	g_assert (vsize == sizeof (CK_OBJECT_HANDLE));
	g_assert (*((CK_OBJECT_HANDLE_PTR)vdata) == gkm_object_get_handle (GKM_OBJECT (test->item)));
	g_free (vdata);

	/* Make it not match */
	fields = gkm_secret_fields_new ();
	gkm_secret_item_set_fields (test->item, fields);
	g_hash_table_unref (fields);

	/* Nothing matched */
	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_MATCHED, &vsize);
	g_assert (vsize == 0);
	g_free (vdata);

	g_object_unref (object);
}

static void
test_for_bad_collection (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attrs[] = {
	        { CKA_G_FIELDS, "name1\0value1", 13 },
	        { CKA_G_COLLECTION, "bad-test->collection", 14 },
	};

	GkmObject *object = NULL;
	GkmTransaction *transaction = gkm_transaction_new ();

	object = gkm_session_create_object_for_factory (test->session, test->factory, transaction, attrs, 2);
	g_assert (gkm_transaction_complete_and_unref (transaction) == CKR_OK);

	g_object_unref (object);
}

static void
test_for_collection (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attrs[] = {
	        { CKA_G_FIELDS, "name1\0value1", 13 },
	        { CKA_G_COLLECTION, "test-collection", 15 },
	};

	GkmObject *object = NULL;
	gpointer vdata;
	gsize vsize;

	object = gkm_session_create_object_for_factory (test->session, test->factory, NULL, attrs, 2);
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_SEARCH (object));

	/* Should have the test->collection set properly */
	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_COLLECTION , &vsize);
	g_assert (vdata);
	g_assert (vsize == 15);
	g_assert (memcmp (vdata, "test-collection", 15) == 0);
	g_free (vdata);

	/* One object matched */
	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_MATCHED, &vsize);
	g_assert (vdata);
	g_assert (vsize == sizeof (CK_OBJECT_HANDLE));
	g_assert (*((CK_OBJECT_HANDLE_PTR)vdata) == gkm_object_get_handle (GKM_OBJECT (test->item)));
	g_free (vdata);

	g_object_unref (object);
}

static void
test_for_collection_no_match (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attrs[] = {
	        { CKA_G_FIELDS, "test\0value", 11 },
	        { CKA_G_COLLECTION, "test-collection", 15 },
	};

	GkmObject *object = NULL;
	GkmSecretCollection *ocoll;
	GkmSecretItem *oitem;
	GHashTable *fields;
	gpointer vdata;
	gsize vsize;

	ocoll = g_object_new (GKM_TYPE_SECRET_COLLECTION,
	                      "module", test->module,
	                      "manager", gkm_session_get_manager (test->session),
	                      "identifier", "other-collection",
	                      NULL);
	oitem = gkm_secret_collection_new_item (ocoll, "other-item");
	gkm_object_expose (GKM_OBJECT (ocoll), TRUE);

	/* Make it match, but remember, wrong collection*/
	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add (fields, "test", "value");
	gkm_secret_item_set_fields (oitem, fields);
	g_hash_table_unref (fields);

	object = gkm_session_create_object_for_factory (test->session, test->factory, NULL, attrs, 2);
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_SEARCH (object));

	/* No objects matched */
	vdata = gkm_object_get_attribute_data (object, test->session, CKA_G_MATCHED, &vsize);
	g_assert (vsize == 0);
	g_free (vdata);

	g_object_unref (object);
	g_object_unref (ocoll);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/secret-store/search/new", Test, NULL, setup, test_new, teardown);
	g_test_add ("/secret-store/search/incomplete", Test, NULL, setup, test_incomplete, teardown);
	g_test_add ("/secret-store/search/bad_fields", Test, NULL, setup, test_bad_fields, teardown);
	g_test_add ("/secret-store/search/and_match", Test, NULL, setup, test_and_match, teardown);
	g_test_add ("/secret-store/search/and_change_to_match", Test, NULL, setup, test_and_change_to_match, teardown);
	g_test_add ("/secret-store/search/and_change_to_not_match", Test, NULL, setup, test_and_change_to_not_match, teardown);
	g_test_add ("/secret-store/search/for_bad_collection", Test, NULL, setup, test_for_bad_collection, teardown);
	g_test_add ("/secret-store/search/for_collection", Test, NULL, setup, test_for_collection, teardown);
	g_test_add ("/secret-store/search/for_collection_no_match", Test, NULL, setup, test_for_collection_no_match, teardown);

	return g_test_run ();
}
