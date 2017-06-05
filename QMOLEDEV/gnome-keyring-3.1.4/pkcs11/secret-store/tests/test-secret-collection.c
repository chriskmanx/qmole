/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-secret-test->collection.c: Test the test->collection keyring

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

#include "secret-store/gkm-secret-data.h"
#include "secret-store/gkm-secret-collection.h"
#include "secret-store/gkm-secret-item.h"

#include "gkm/gkm-credential.h"
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
	CK_OBJECT_HANDLE credential;
	CK_OBJECT_HANDLE credential2;
	GkmSecretCollection *collection;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_CREDENTIAL;
	GkmObject *cred;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_VALUE, NULL, 0 }
	};

	test->module = test_secret_module_initialize_and_enter ();
	test->session = test_secret_module_open_session (TRUE);

	test->collection = g_object_new (GKM_TYPE_SECRET_COLLECTION,
	                           "module", test->module,
	                           "identifier", "test",
	                           NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (test->collection));

	/* Make two credentials */
	cred = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_CREDENTIAL, NULL,
	                                            attrs, G_N_ELEMENTS (attrs));
	g_assert (cred != NULL);
	test->credential = gkm_object_get_handle (GKM_OBJECT (cred));
	g_object_unref (cred);

	cred = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_CREDENTIAL, NULL,
	                                            attrs, G_N_ELEMENTS (attrs));
	g_assert (cred != NULL);
	test->credential2 = gkm_object_get_handle (GKM_OBJECT (cred));
	g_object_unref (cred);
}

static void
teardown (Test *test, gconstpointer unused)
{
	if (test->collection)
		g_object_unref (test->collection);
	test->collection = NULL;

	test_secret_module_leave_and_finalize ();
	test->module = NULL;
	test->session = NULL;
	test->credential = 0;
}

static void
test_is_locked (Test *test, gconstpointer unused)
{
	gboolean locked;

	/* By default is locked */
	locked = gkm_secret_object_is_locked (GKM_SECRET_OBJECT (test->collection), test->session);
	g_assert (locked == TRUE);
}

static void
test_unlocked_data (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	CK_RV rv;

	/* Create test->credential, which unlocks test->collection */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection), NULL, 0, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (test->session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	/* Collection should now be unlocked */
	sdata = gkm_secret_collection_unlocked_use (test->collection, test->session);
	g_assert (GKM_IS_SECRET_DATA (sdata));
	g_assert (!gkm_secret_object_is_locked (GKM_SECRET_OBJECT (test->collection), test->session));
	g_object_unref (sdata);
}

static void
test_get_filename (Test *test, gconstpointer unused)
{
	GkmSecretCollection *other;
	const gchar *filename;

	other = g_object_new (GKM_TYPE_SECRET_COLLECTION,
	                      "module", test->module,
	                      "identifier", "test",
	                      "filename", "/tmp/filename.keyring",
	                      NULL);

	filename = gkm_secret_collection_get_filename (other);
	g_assert_cmpstr (filename, ==, "/tmp/filename.keyring");

	g_object_unref (other);
}

static void
test_set_filename (Test *test, gconstpointer unused)
{
	const gchar *filename;

	gkm_secret_collection_set_filename (test->collection, "/tmp/filename.keyring");

	filename = gkm_secret_collection_get_filename (test->collection);
	g_assert_cmpstr (filename, ==, "/tmp/filename.keyring");
}

static void
test_has_item (Test *test, gconstpointer unused)
{
	GkmSecretItem *item;

	item = gkm_secret_collection_new_item (test->collection, "testo");
	g_assert (gkm_secret_collection_has_item (test->collection, item));
}

static void
test_load_unlock_plain (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	GkmDataResult res;
	CK_RV rv;

	gkm_secret_collection_set_filename (test->collection, SRCDIR "/files/plain.keyring");

	/* Load the data in the file */
	res = gkm_secret_collection_load (test->collection);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Unlock the keyring, which should load again */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection), NULL, 0, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (test->session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	sdata = gkm_secret_collection_unlocked_use (test->collection, test->session);
	g_assert (sdata != NULL && GKM_IS_SECRET_DATA (sdata));
	test_secret_collection_validate (test->collection, sdata);
	g_object_unref (sdata);
}

static void
test_load_unlock_encrypted (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	GkmDataResult res;
	CK_RV rv;

	gkm_secret_collection_set_filename (test->collection, SRCDIR "/files/encrypted.keyring");

	/* Load the data in the file */
	res = gkm_secret_collection_load (test->collection);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Unlock the keyring, which should load again */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection),
	                            (guchar*)"my-keyring-password", 19, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (test->session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	sdata = gkm_secret_collection_unlocked_use (test->collection, test->session);
	g_assert (sdata != NULL && GKM_IS_SECRET_DATA (sdata));
	test_secret_collection_validate (test->collection, sdata);
	g_object_unref (sdata);
}

static void
test_load_unlock_bad_password (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GkmDataResult res;
	CK_RV rv;

	gkm_secret_collection_set_filename (test->collection, SRCDIR "/files/encrypted.keyring");

	/* Load the data in the file */
	res = gkm_secret_collection_load (test->collection);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Unlock the keyring, which should load again */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection),
	                            (guchar*)"wrong", 5, &cred);
	g_assert (rv == CKR_PIN_INCORRECT);
}

static void
test_unlock_without_load (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	CK_RV rv;

	gkm_secret_collection_set_filename (test->collection, SRCDIR "/files/encrypted.keyring");

	/* Unlock the keyring, which should load it */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection),
	                            (guchar*)"my-keyring-password", 19, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (test->session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	sdata = gkm_secret_collection_unlocked_use (test->collection, test->session);
	g_assert (sdata != NULL && GKM_IS_SECRET_DATA (sdata));
	test_secret_collection_validate (test->collection, sdata);
	g_object_unref (sdata);
}

static void
test_twice_unlock (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	CK_RV rv;

	gkm_secret_collection_set_filename (test->collection, SRCDIR "/files/encrypted.keyring");

	/* Unlock the keyring, which should load */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection),
	                            (guchar*)"my-keyring-password", 19, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (test->session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	/* Unlock the keyring again, which should not reload */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection),
	                            (guchar*)"my-keyring-password", 19, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (test->session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	sdata = gkm_secret_collection_unlocked_use (test->collection, test->session);
	g_assert (sdata != NULL && GKM_IS_SECRET_DATA (sdata));
	test_secret_collection_validate (test->collection, sdata);
	g_object_unref (sdata);
}

static void
test_twice_unlock_bad_password (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	CK_RV rv;

	gkm_secret_collection_set_filename (test->collection, SRCDIR "/files/encrypted.keyring");

	/* Unlock the keyring, which should load */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection),
	                               (guchar*)"my-keyring-password", 19, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (test->session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	/* Unlock the keyring again, wrong password */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection),
	                            (guchar*)"wrong", 5, &cred);
	g_assert (rv == CKR_PIN_INCORRECT);

	sdata = gkm_secret_collection_unlocked_use (test->collection, test->session);
	g_assert (sdata != NULL && GKM_IS_SECRET_DATA (sdata));
	test_secret_collection_validate (test->collection, sdata);
	g_object_unref (sdata);
}

static void
test_memory_unlock (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GkmDataResult res;
	CK_RV rv;

	/* Load the data in the file */
	res = gkm_secret_collection_load (test->collection);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Unlock the keyring, which should load again */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection),
	                            NULL, 0, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (test->session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);
}

static void
test_memory_unlock_bad_password (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GkmDataResult res;
	CK_RV rv;

	/* Load the data in the file */
	res = gkm_secret_collection_load (test->collection);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Unlock the keyring, which should load again */
	rv = gkm_credential_create (test->module, gkm_session_get_manager (test->session), GKM_OBJECT (test->collection),
	                            (guchar*)"wrong", 5, &cred);
	g_assert (rv == CKR_PIN_INCORRECT);
}

static void
test_factory (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	GkmObject *object;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_LABEL, "blah", 4 },
		{ CKA_G_CREDENTIAL, &test->credential, sizeof (test->credential) },
	};

	object = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	g_assert_cmpstr (gkm_secret_object_get_label (GKM_SECRET_OBJECT (object)), ==, "blah");
	g_object_unref (object);
}

static void
test_factory_unnamed (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	const gchar *identifier;
	GkmObject *object;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_G_CREDENTIAL, &test->credential, sizeof (test->credential) },
	};

	object = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_assert_cmpstr (identifier, !=, "");
	g_object_unref (object);
}

static void
test_factory_token (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	const gchar *identifier;
	GkmObject *object;
	CK_BBOOL token = CK_TRUE;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_LABEL, "blah", 4 },
		{ CKA_G_CREDENTIAL, &test->credential, sizeof (test->credential) },
	};

	object = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_assert (strstr (identifier, "blah"));
	g_object_unref (object);
}

static void
test_factory_duplicate (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	const gchar *identifier1, *identifier2;
	GkmObject *object;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_G_CREDENTIAL, &test->credential, sizeof (test->credential) },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_LABEL, "blah", 4 },
	};

	object = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	identifier1 = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_assert (strstr (identifier1, "blah"));
	g_object_unref (object);

	/* Use second test->credential for second object */
	attrs[0].pValue = &test->credential2;
	object = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	identifier2 = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_assert (strstr (identifier2, "blah"));
	g_object_unref (object);

	g_assert_cmpstr (identifier1, !=, identifier2);
}

static void
test_factory_item (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS c_klass = CKO_G_COLLECTION;
	CK_OBJECT_CLASS i_klass = CKO_SECRET_KEY;
	const gchar *identifier;
	GkmObject *object;
	CK_BBOOL token = CK_TRUE;

	CK_ATTRIBUTE c_attrs[] = {
		{ CKA_CLASS, &c_klass, sizeof (c_klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_LABEL, "three", 5 },
		{ CKA_G_CREDENTIAL, &test->credential, sizeof (test->credential) },
	};

	CK_ATTRIBUTE i_attrs[] = {
		{ CKA_G_COLLECTION, NULL, 0 }, /* Filled below */
		{ CKA_CLASS, &i_klass, sizeof (i_klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_LABEL, "Item", 4 },
	};

	object = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                c_attrs, G_N_ELEMENTS (c_attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));
	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_object_unref (object);

	i_attrs[0].pValue = (gpointer)identifier;
	i_attrs[0].ulValueLen = strlen (identifier);
	object = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_SECRET_ITEM, NULL,
	                                                i_attrs, G_N_ELEMENTS (i_attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_ITEM (object));
	g_object_unref (object);
}

static void
test_token_remove (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	GkmTransaction *transaction;
	GkmObject *object;
	CK_BBOOL token = CK_TRUE;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_LABEL, "blah", 4 },
		{ CKA_G_CREDENTIAL, &test->credential, sizeof (test->credential) },
	};

	object = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	transaction = gkm_transaction_new ();
	gkm_module_remove_token_object (test->module, transaction, object);
	g_assert (!gkm_transaction_get_failed (transaction));
	gkm_transaction_complete (transaction);
	g_object_unref (transaction);
	g_object_unref (object);
}

static void
test_token_item_remove (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS c_klass = CKO_G_COLLECTION;
	CK_OBJECT_CLASS i_klass = CKO_SECRET_KEY;
	GkmTransaction *transaction;
	const gchar *identifier;
	GkmObject *object;
	CK_BBOOL token = CK_TRUE;

	CK_ATTRIBUTE c_attrs[] = {
		{ CKA_CLASS, &c_klass, sizeof (c_klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_LABEL, "three", 5 },
		{ CKA_G_CREDENTIAL, &test->credential, sizeof (test->credential) },
	};

	CK_ATTRIBUTE i_attrs[] = {
		{ CKA_G_COLLECTION, NULL, 0 }, /* Filled below */
		{ CKA_CLASS, &i_klass, sizeof (i_klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_LABEL, "Item", 4 },
	};

	object = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                c_attrs, G_N_ELEMENTS (c_attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));
	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_object_unref (object);

	i_attrs[0].pValue = (gpointer)identifier;
	i_attrs[0].ulValueLen = strlen (identifier);
	object = gkm_session_create_object_for_factory (test->session, GKM_FACTORY_SECRET_ITEM, NULL,
	                                                i_attrs, G_N_ELEMENTS (i_attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_ITEM (object));

	transaction = gkm_transaction_new ();
	gkm_module_remove_token_object (test->module, transaction, object);
	g_assert (!gkm_transaction_get_failed (transaction));
	gkm_transaction_complete (transaction);
	g_object_unref (transaction);
	g_object_unref (object);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/secret-store/collection/is_locked", Test, NULL, setup, test_is_locked, teardown);
	g_test_add ("/secret-store/collection/unlocked_data", Test, NULL, setup, test_unlocked_data, teardown);
	g_test_add ("/secret-store/collection/get_filename", Test, NULL, setup, test_get_filename, teardown);
	g_test_add ("/secret-store/collection/set_filename", Test, NULL, setup, test_set_filename, teardown);
	g_test_add ("/secret-store/collection/has_item", Test, NULL, setup, test_has_item, teardown);
	g_test_add ("/secret-store/collection/load_unlock_plain", Test, NULL, setup, test_load_unlock_plain, teardown);
	g_test_add ("/secret-store/collection/load_unlock_encrypted", Test, NULL, setup, test_load_unlock_encrypted, teardown);
	g_test_add ("/secret-store/collection/load_unlock_bad_password", Test, NULL, setup, test_load_unlock_bad_password, teardown);
	g_test_add ("/secret-store/collection/unlock_without_load", Test, NULL, setup, test_unlock_without_load, teardown);
	g_test_add ("/secret-store/collection/twice_unlock", Test, NULL, setup, test_twice_unlock, teardown);
	g_test_add ("/secret-store/collection/twice_unlock_bad_password", Test, NULL, setup, test_twice_unlock_bad_password, teardown);
	g_test_add ("/secret-store/collection/memory_unlock", Test, NULL, setup, test_memory_unlock, teardown);
	g_test_add ("/secret-store/collection/memory_unlock_bad_password", Test, NULL, setup, test_memory_unlock_bad_password, teardown);
	g_test_add ("/secret-store/collection/factory", Test, NULL, setup, test_factory, teardown);
	g_test_add ("/secret-store/collection/factory_unnamed", Test, NULL, setup, test_factory_unnamed, teardown);
	g_test_add ("/secret-store/collection/factory_token", Test, NULL, setup, test_factory_token, teardown);
	g_test_add ("/secret-store/collection/factory_duplicate", Test, NULL, setup, test_factory_duplicate, teardown);
	g_test_add ("/secret-store/collection/factory_item", Test, NULL, setup, test_factory_item, teardown);
	g_test_add ("/secret-store/collection/token_remove", Test, NULL, setup, test_token_remove, teardown);
	g_test_add ("/secret-store/collection/token_item_remove", Test, NULL, setup, test_token_item_remove, teardown);

	return g_test_run ();
}
