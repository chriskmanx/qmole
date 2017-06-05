/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-secret-item.c: Test secret item

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

#include "test-suite.h"
#include "test-secret-module.h"

#include "gkm-secret-collection.h"
#include "gkm-secret-fields.h"
#include "gkm-secret-item.h"

#include "gkm/gkm-credential.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-transaction.h"

#include "pkcs11/pkcs11i.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static GkmModule *module = NULL;
static GkmSession *session = NULL;
static GkmSecretCollection *collection = NULL;

DEFINE_SETUP(secret_item)
{
	module = test_secret_module_initialize_and_enter ();
	session = test_secret_module_open_session (TRUE);

	collection = g_object_new (GKM_TYPE_SECRET_COLLECTION,
	                           "module", module,
	                           "identifier", "test",
	                           NULL);
}

DEFINE_TEARDOWN(secret_item)
{
	if (collection)
		g_object_unref (collection);
	collection = NULL;

	test_secret_module_leave_and_finalize ();
	module = NULL;
	session = NULL;

}

static void
unlock_collection(void)
{
	GkmCredential *cred;
	GkmObject *object;
	CK_RV rv;

	/* Create credential, which unlocks collection */
	object = GKM_OBJECT (collection);
	rv = gkm_credential_create (gkm_object_get_module (object),
	                            gkm_session_get_manager (session),
	                            object, NULL, 0, &cred);
	g_assert (rv == CKR_OK);

	gkm_session_add_session_object (session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);
}

DEFINE_TEST(secret_item_new)
{
	GkmSecretItem *item;

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	g_assert (GKM_IS_SECRET_ITEM (item));
	g_assert_cmpstr (gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (item)), ==, "the-identifier");
}

DEFINE_TEST(secret_item_create)
{
	GkmTransaction *transaction;
	GkmSecretItem *item;

	transaction = gkm_transaction_new ();
	item = gkm_secret_collection_create_item (collection, transaction);
	g_assert (GKM_IS_SECRET_ITEM (item));
	g_object_ref (item);
	g_assert (gkm_secret_collection_has_item (collection, item));

	gkm_transaction_complete (transaction);
	g_object_unref (transaction);

	/* Should still be there */
	g_assert (gkm_secret_collection_has_item (collection, item));
	g_object_unref (item);
}

DEFINE_TEST(secret_item_create_failed)
{
	GkmTransaction *transaction;
	GkmSecretItem *item;

	transaction = gkm_transaction_new ();
	item = gkm_secret_collection_create_item (collection, transaction);
	g_assert (GKM_IS_SECRET_ITEM (item));
	g_object_ref (item);
	g_assert (gkm_secret_collection_has_item (collection, item));

	gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);
	gkm_transaction_complete (transaction);
	g_object_unref (transaction);

	/* Should no longer be there */
	g_assert (!gkm_secret_collection_has_item (collection, item));
	g_object_unref (item);
}

DEFINE_TEST(secret_item_destroy)
{
	GkmTransaction *transaction;
	GkmSecretItem *item;

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	g_assert (gkm_secret_collection_has_item (collection, item));
	g_object_ref (item);

	transaction = gkm_transaction_new ();
	gkm_secret_collection_destroy_item (collection, transaction, item);
	g_assert (!gkm_secret_collection_has_item (collection, item));

	gkm_transaction_complete (transaction);
	g_object_unref (transaction);

	/* Should not be there */
	g_assert (!gkm_secret_collection_has_item (collection, item));
	g_object_unref (item);
}

DEFINE_TEST(secret_item_destroy_failed)
{
	GkmTransaction *transaction;
	GkmSecretItem *item;

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	g_assert (gkm_secret_collection_has_item (collection, item));
	g_object_ref (item);

	transaction = gkm_transaction_new ();
	gkm_secret_collection_destroy_item (collection, transaction, item);
	g_assert (!gkm_secret_collection_has_item (collection, item));

	gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);
	gkm_transaction_complete (transaction);
	g_object_unref (transaction);

	/* Should be there */
	g_assert (gkm_secret_collection_has_item (collection, item));
	g_object_unref (item);
}

DEFINE_TEST(secret_item_collection_get)
{
	GkmSecretItem *item, *check;

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	g_assert (GKM_IS_SECRET_ITEM (item));

	check = gkm_secret_collection_get_item (collection, "the-identifier");
	g_assert (item == check);
}

DEFINE_TEST(secret_item_collection_items)
{
	GList *l, *items;
	const gchar *identifier;

	gkm_secret_collection_new_item (collection, "one-identifier");
	gkm_secret_collection_new_item (collection, "two-identifier");
	gkm_secret_collection_new_item (collection, "three-identifier");

	items = gkm_secret_collection_get_items (collection);
	g_assert_cmpuint (g_list_length (items), ==, 3);
	for (l = items; l; l = g_list_next (l)) {
		identifier = gkm_secret_object_get_identifier (l->data);
		if (!g_str_equal (identifier, "one-identifier") &&
		    !g_str_equal (identifier, "two-identifier") &&
		    !g_str_equal (identifier, "three-identifier"))
			g_assert_not_reached ();
	}

	g_list_free (items);
}

DEFINE_TEST(secret_item_collection_remove)
{
	GkmSecretItem *item;

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	g_assert (gkm_secret_collection_get_item (collection, "the-identifier") == item);

	gkm_secret_collection_remove_item (collection, item);
	g_assert (gkm_secret_collection_get_item (collection, "the-identifier") == NULL);
}

DEFINE_TEST(secret_item_is_locked)
{
	GkmSecretItem *item;

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	g_assert (gkm_secret_object_is_locked (GKM_SECRET_OBJECT (item), session) ==
	          gkm_secret_object_is_locked (GKM_SECRET_OBJECT (collection), session));

	unlock_collection();

	g_assert (gkm_secret_object_is_locked (GKM_SECRET_OBJECT (item), session) == FALSE);
}

DEFINE_TEST(secret_item_get_collection)
{
	GkmSecretItem *item;
	item = gkm_secret_collection_new_item (collection, "the-identifier");
	g_assert (gkm_secret_item_get_collection (item) == collection);
}

DEFINE_TEST(secret_item_tracks_collection)
{
	GkmSecretItem *item;
	item = gkm_secret_collection_new_item (collection, "the-identifier");
	g_object_ref (item);

	unlock_collection();

	/* At this point the item should be 'unlocked' */
	g_assert (gkm_secret_object_is_locked (GKM_SECRET_OBJECT (item), session) == FALSE);

	g_object_unref (collection);
	collection = NULL;

	/* Collection went away */
	g_assert (gkm_secret_item_get_collection (item) == NULL);
	g_assert (gkm_secret_object_is_locked (GKM_SECRET_OBJECT (item), session) == TRUE);

	g_object_unref (item);
}

DEFINE_TEST(secret_item_get_set_fields)
{
	GHashTable *fields = gkm_secret_fields_new ();
	GHashTable *check;
	GkmSecretItem *item;

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	gkm_secret_item_set_fields (item, fields);
	gkm_secret_item_set_fields (item, fields);

	check = gkm_secret_item_get_fields (item);
	g_assert (check == fields);

	g_hash_table_unref (fields);
}

DEFINE_TEST(secret_item_collection_attr)
{
	gchar buffer[32];
	CK_ATTRIBUTE check = { CKA_G_COLLECTION, buffer, 32 };
	GkmSecretItem *item;
	CK_RV rv;

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	rv = gkm_object_get_attribute (GKM_OBJECT (item), session, &check);
	g_assert (rv == CKR_OK);
	g_assert (check.ulValueLen == 4);
	g_assert (memcmp (buffer, "test", 4) == 0);
}

DEFINE_TEST(secret_item_secret_attr)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	CK_ATTRIBUTE attr = { CKA_VALUE, "hello", 5 };
	gchar buffer[32];
	CK_ATTRIBUTE check = { CKA_VALUE, buffer, 32 };
	GkmSecretItem *item;
	CK_RV rv;

	unlock_collection ();

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	gkm_object_set_attribute (GKM_OBJECT (item), session, transaction, &attr);
	g_assert (gkm_transaction_get_failed (transaction) == FALSE);
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_result (transaction) == CKR_OK);

	g_object_unref (transaction);

	rv = gkm_object_get_attribute (GKM_OBJECT (item), session, &check);
	g_assert (rv == CKR_OK);
	g_assert (check.ulValueLen == 5);
	g_assert (memcmp (buffer, "hello", 5) == 0);
}

DEFINE_TEST(secret_item_secret_attr_locked)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	CK_ATTRIBUTE attr = { CKA_VALUE, "hello", 5 };
	gchar buffer[32];
	CK_ATTRIBUTE check = { CKA_VALUE, buffer, 32 };
	GkmSecretItem *item;
	CK_RV rv;

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	gkm_object_set_attribute (GKM_OBJECT (item), session, transaction, &attr);
	g_assert (gkm_transaction_get_failed (transaction) == TRUE);
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_result (transaction) == CKR_USER_NOT_LOGGED_IN);

	g_object_unref (transaction);

	rv = gkm_object_get_attribute (GKM_OBJECT (item), session, &check);
	g_assert (rv == CKR_USER_NOT_LOGGED_IN);
}

DEFINE_TEST(secret_item_fields_attr)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, "name1\0value1\0name2\0value2", 26 };
	gchar buffer[32];
	CK_ATTRIBUTE check = { CKA_G_FIELDS, buffer, 32 };
	GkmSecretItem *item;
	GHashTable *fields;
	const gchar *value;
	CK_RV rv;

	unlock_collection ();

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	gkm_object_set_attribute (GKM_OBJECT (item), session, transaction, &attr);
	g_assert (gkm_transaction_get_failed (transaction) == FALSE);
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_result (transaction) == CKR_OK);

	g_object_unref (transaction);

	rv = gkm_object_get_attribute (GKM_OBJECT (item), session, &check);
	g_assert (rv == CKR_OK);
	g_assert (check.ulValueLen == 26);
	g_assert (memcmp (buffer, "name1\0value1\0name2\0value2", 26) == 0);

	fields = gkm_secret_item_get_fields (item);
	g_assert (fields);
	value = gkm_secret_fields_get (fields, "name1");
	g_assert_cmpstr (value, ==, "value1");
	value = gkm_secret_fields_get (fields, "name2");
	g_assert_cmpstr (value, ==, "value2");
}

DEFINE_TEST(secret_item_fields_attr_locked)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, "name1\0value1\0name2\0value2", 26 };
	GkmSecretItem *item;

	item = gkm_secret_collection_new_item (collection, "the-identifier");
	gkm_object_set_attribute (GKM_OBJECT (item), session, transaction, &attr);
	g_assert (gkm_transaction_get_failed (transaction) == TRUE);
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_result (transaction) == CKR_USER_NOT_LOGGED_IN);

	g_object_unref (transaction);
}

DEFINE_TEST(secret_item_fields_attr_reverts)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, "new\0value\0", 10 };
	gchar buffer[32];
	CK_ATTRIBUTE check = { CKA_G_FIELDS, buffer, 32 };
	GkmSecretItem *item;
	GHashTable *fields;
	CK_RV rv;

	unlock_collection ();

	item = gkm_secret_collection_new_item (collection, "the-identifier");

	/* Set the old value like so */
	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add (fields, "old", "value");
	gkm_secret_item_set_fields (item, fields);
	g_hash_table_unref (fields);

	/* Should show old value */
	rv = gkm_object_get_attribute (GKM_OBJECT (item), session, &check);
	g_assert (rv == CKR_OK);
	g_assert (check.ulValueLen == 10);
	g_assert (memcmp (buffer, "old\0value\0", 10) == 0);

	/* Set the new values */
	gkm_object_set_attribute (GKM_OBJECT (item), session, transaction, &attr);
	g_assert (gkm_transaction_get_failed (transaction) == FALSE);

	/* Should have the new value */
	rv = gkm_object_get_attribute (GKM_OBJECT (item), session, &check);
	g_assert (rv == CKR_OK);
	g_assert (check.ulValueLen == 10);
	g_assert (memcmp (buffer, "new\0value\0", 10) == 0);

	/* Fail the transaction */
	gkm_transaction_fail (transaction, CKR_CANCEL);
	gkm_transaction_complete (transaction);

	/* Should show the old value */
	rv = gkm_object_get_attribute (GKM_OBJECT (item), session, &check);
	g_assert (rv == CKR_OK);
	g_assert (check.ulValueLen == 10);
	g_assert (memcmp (buffer, "old\0value\0", 10) == 0);

	g_object_unref (transaction);
}
