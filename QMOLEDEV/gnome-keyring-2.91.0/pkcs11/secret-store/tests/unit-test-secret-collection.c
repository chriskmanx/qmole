/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-secret-collection.c: Test the collection keyring

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

#include "gkm-secret-data.h"
#include "gkm-secret-collection.h"
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
static CK_OBJECT_HANDLE credential = 0;
static CK_OBJECT_HANDLE credential2 = 0;
static GkmSecretCollection *collection = NULL;

DEFINE_SETUP(secret_collection)
{
	CK_OBJECT_CLASS klass = CKO_G_CREDENTIAL;
	GkmObject *cred;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_VALUE, NULL, 0 }
	};

	module = test_secret_module_initialize_and_enter ();
	session = test_secret_module_open_session (TRUE);

	collection = g_object_new (GKM_TYPE_SECRET_COLLECTION,
	                           "module", module,
	                           "identifier", "test",
	                           NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (collection));

	/* Make two credentials */
	cred = gkm_session_create_object_for_factory (session, GKM_FACTORY_CREDENTIAL, NULL,
	                                            attrs, G_N_ELEMENTS (attrs));
	g_assert (cred != NULL);
	credential = gkm_object_get_handle (GKM_OBJECT (cred));
	g_object_unref (cred);

	cred = gkm_session_create_object_for_factory (session, GKM_FACTORY_CREDENTIAL, NULL,
	                                            attrs, G_N_ELEMENTS (attrs));
	g_assert (cred != NULL);
	credential2 = gkm_object_get_handle (GKM_OBJECT (cred));
	g_object_unref (cred);
}

DEFINE_TEARDOWN(secret_collection)
{
	if (collection)
		g_object_unref (collection);
	collection = NULL;

	test_secret_module_leave_and_finalize ();
	module = NULL;
	session = NULL;
	credential = 0;
}

DEFINE_TEST(secret_collection_is_locked)
{
	gboolean locked;

	/* By default is locked */
	locked = gkm_secret_object_is_locked (GKM_SECRET_OBJECT (collection), session);
	g_assert (locked == TRUE);
}

DEFINE_TEST(secret_collection_unlocked_data)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	CK_RV rv;

	/* Create credential, which unlocks collection */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection), NULL, 0, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	/* Collection should now be unlocked */
	sdata = gkm_secret_collection_unlocked_use (collection, session);
	g_assert (GKM_IS_SECRET_DATA (sdata));
	g_assert (!gkm_secret_object_is_locked (GKM_SECRET_OBJECT (collection), session));
	g_object_unref (sdata);
}

DEFINE_TEST(secret_collection_get_filename)
{
	GkmSecretCollection *other;
	const gchar *filename;

	other = g_object_new (GKM_TYPE_SECRET_COLLECTION,
	                      "module", module,
	                      "identifier", "test",
	                      "filename", "/tmp/filename.keyring",
	                      NULL);

	filename = gkm_secret_collection_get_filename (other);
	g_assert_cmpstr (filename, ==, "/tmp/filename.keyring");

	g_object_unref (other);
}

DEFINE_TEST(secret_collection_set_filename)
{
	const gchar *filename;

	gkm_secret_collection_set_filename (collection, "/tmp/filename.keyring");

	filename = gkm_secret_collection_get_filename (collection);
	g_assert_cmpstr (filename, ==, "/tmp/filename.keyring");
}

DEFINE_TEST(secret_collection_has_item)
{
	GkmSecretItem *item;

	item = gkm_secret_collection_new_item (collection, "testo");
	g_assert (gkm_secret_collection_has_item (collection, item));
}

DEFINE_TEST(secret_collection_load_unlock_plain)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	GkmDataResult res;
	gchar *filename;
	CK_RV rv;

	filename = testing_data_filename ("plain.keyring");
	gkm_secret_collection_set_filename (collection, filename);
	g_free (filename);

	/* Load the data in the file */
	res = gkm_secret_collection_load (collection);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Unlock the keyring, which should load again */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection), NULL, 0, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	sdata = gkm_secret_collection_unlocked_use (collection, session);
	g_assert (sdata != NULL && GKM_IS_SECRET_DATA (sdata));
	test_secret_collection_validate (collection, sdata);
	g_object_unref (sdata);
}

DEFINE_TEST(secret_collection_load_unlock_encrypted)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	GkmDataResult res;
	gchar *filename;
	CK_RV rv;

	filename = testing_data_filename ("encrypted.keyring");
	gkm_secret_collection_set_filename (collection, filename);
	g_free (filename);

	/* Load the data in the file */
	res = gkm_secret_collection_load (collection);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Unlock the keyring, which should load again */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection),
	                            (guchar*)"my-keyring-password", 19, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	sdata = gkm_secret_collection_unlocked_use (collection, session);
	g_assert (sdata != NULL && GKM_IS_SECRET_DATA (sdata));
	test_secret_collection_validate (collection, sdata);
	g_object_unref (sdata);
}

DEFINE_TEST(secret_collection_load_unlock_bad_password)
{
	GkmCredential *cred;
	GkmDataResult res;
	gchar *filename;
	CK_RV rv;

	filename = testing_data_filename ("encrypted.keyring");
	gkm_secret_collection_set_filename (collection, filename);
	g_free (filename);

	/* Load the data in the file */
	res = gkm_secret_collection_load (collection);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Unlock the keyring, which should load again */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection),
	                            (guchar*)"wrong", 5, &cred);
	g_assert (rv == CKR_PIN_INCORRECT);
}

DEFINE_TEST(secret_collection_unlock_without_load)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	gchar *filename;
	CK_RV rv;

	filename = testing_data_filename ("encrypted.keyring");
	gkm_secret_collection_set_filename (collection, filename);
	g_free (filename);

	/* Unlock the keyring, which should load it */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection),
	                            (guchar*)"my-keyring-password", 19, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	sdata = gkm_secret_collection_unlocked_use (collection, session);
	g_assert (sdata != NULL && GKM_IS_SECRET_DATA (sdata));
	test_secret_collection_validate (collection, sdata);
	g_object_unref (sdata);
}

DEFINE_TEST(secret_collection_twice_unlock)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	gchar *filename;
	CK_RV rv;

	filename = testing_data_filename ("encrypted.keyring");
	gkm_secret_collection_set_filename (collection, filename);
	g_free (filename);

	/* Unlock the keyring, which should load */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection),
	                            (guchar*)"my-keyring-password", 19, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	/* Unlock the keyring again, which should not reload */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection),
	                            (guchar*)"my-keyring-password", 19, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	sdata = gkm_secret_collection_unlocked_use (collection, session);
	g_assert (sdata != NULL && GKM_IS_SECRET_DATA (sdata));
	test_secret_collection_validate (collection, sdata);
	g_object_unref (sdata);
}

DEFINE_TEST(secret_collection_twice_unlock_bad_password)
{
	GkmCredential *cred;
	GkmSecretData *sdata;
	gchar *filename;
	CK_RV rv;

	filename = testing_data_filename ("encrypted.keyring");
	gkm_secret_collection_set_filename (collection, filename);
	g_free (filename);

	/* Unlock the keyring, which should load */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection),
	                               (guchar*)"my-keyring-password", 19, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);

	/* Unlock the keyring again, wrong password */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection),
	                            (guchar*)"wrong", 5, &cred);
	g_assert (rv == CKR_PIN_INCORRECT);

	sdata = gkm_secret_collection_unlocked_use (collection, session);
	g_assert (sdata != NULL && GKM_IS_SECRET_DATA (sdata));
	test_secret_collection_validate (collection, sdata);
	g_object_unref (sdata);
}

DEFINE_TEST(secret_collection_memory_unlock)
{
	GkmCredential *cred;
	GkmDataResult res;
	CK_RV rv;

	/* Load the data in the file */
	res = gkm_secret_collection_load (collection);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Unlock the keyring, which should load again */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection),
	                            NULL, 0, &cred);
	g_assert (rv == CKR_OK);
	gkm_session_add_session_object (session, NULL, GKM_OBJECT (cred));
	g_object_unref (cred);
}

DEFINE_TEST(secret_collection_memory_unlock_bad_password)
{
	GkmCredential *cred;
	GkmDataResult res;
	CK_RV rv;

	/* Load the data in the file */
	res = gkm_secret_collection_load (collection);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Unlock the keyring, which should load again */
	rv = gkm_credential_create (module, gkm_session_get_manager (session), GKM_OBJECT (collection),
	                            (guchar*)"wrong", 5, &cred);
	g_assert (rv == CKR_PIN_INCORRECT);
}

DEFINE_TEST(secret_collection_factory)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	GkmObject *object;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_LABEL, "blah", 4 },
		{ CKA_G_CREDENTIAL, &credential, sizeof (credential) },
	};

	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	g_assert_cmpstr (gkm_secret_object_get_label (GKM_SECRET_OBJECT (object)), ==, "blah");
	g_object_unref (object);
}

DEFINE_TEST(secret_collection_factory_unnamed)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	const gchar *identifier;
	GkmObject *object;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_G_CREDENTIAL, &credential, sizeof (credential) },
	};

	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_assert_cmpstr (identifier, !=, "");
	g_object_unref (object);
}

DEFINE_TEST(secret_collection_factory_token)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	const gchar *identifier;
	GkmObject *object;
	CK_BBOOL token = CK_TRUE;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_LABEL, "blah", 4 },
		{ CKA_G_CREDENTIAL, &credential, sizeof (credential) },
	};

	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_assert (strstr (identifier, "blah"));
	g_object_unref (object);
}

DEFINE_TEST(secret_collection_factory_duplicate)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	const gchar *identifier1, *identifier2;
	GkmObject *object;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_G_CREDENTIAL, &credential, sizeof (credential) },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_LABEL, "blah", 4 },
	};

	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	identifier1 = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_assert (strstr (identifier1, "blah"));
	g_object_unref (object);

	/* Use second credential for second object */
	attrs[0].pValue = &credential2;
	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	identifier2 = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_assert (strstr (identifier2, "blah"));
	g_object_unref (object);

	g_assert_cmpstr (identifier1, !=, identifier2);
}

DEFINE_TEST(secret_collection_factory_item)
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
		{ CKA_G_CREDENTIAL, &credential, sizeof (credential) },
	};

	CK_ATTRIBUTE i_attrs[] = {
		{ CKA_G_COLLECTION, NULL, 0 }, /* Filled below */
		{ CKA_CLASS, &i_klass, sizeof (i_klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_LABEL, "Item", 4 },
	};

	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                c_attrs, G_N_ELEMENTS (c_attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));
	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_object_unref (object);

	i_attrs[0].pValue = (gpointer)identifier;
	i_attrs[0].ulValueLen = strlen (identifier);
	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_SECRET_ITEM, NULL,
	                                                i_attrs, G_N_ELEMENTS (i_attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_ITEM (object));
	g_object_unref (object);
}

DEFINE_TEST(secret_collection_token_remove)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	GkmTransaction *transaction;
	GkmObject *object;
	CK_BBOOL token = CK_TRUE;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_LABEL, "blah", 4 },
		{ CKA_G_CREDENTIAL, &credential, sizeof (credential) },
	};

	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                attrs, G_N_ELEMENTS (attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));

	transaction = gkm_transaction_new ();
	gkm_module_remove_token_object (module, transaction, object);
	g_assert (!gkm_transaction_get_failed (transaction));
	gkm_transaction_complete (transaction);
	g_object_unref (transaction);
	g_object_unref (object);
}

DEFINE_TEST(secret_collection_token_item_remove)
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
		{ CKA_G_CREDENTIAL, &credential, sizeof (credential) },
	};

	CK_ATTRIBUTE i_attrs[] = {
		{ CKA_G_COLLECTION, NULL, 0 }, /* Filled below */
		{ CKA_CLASS, &i_klass, sizeof (i_klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_LABEL, "Item", 4 },
	};

	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_SECRET_COLLECTION, NULL,
	                                                c_attrs, G_N_ELEMENTS (c_attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_COLLECTION (object));
	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (object));
	g_object_unref (object);

	i_attrs[0].pValue = (gpointer)identifier;
	i_attrs[0].ulValueLen = strlen (identifier);
	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_SECRET_ITEM, NULL,
	                                                i_attrs, G_N_ELEMENTS (i_attrs));
	g_assert (object != NULL);
	g_assert (GKM_IS_SECRET_ITEM (object));

	transaction = gkm_transaction_new ();
	gkm_module_remove_token_object (module, transaction, object);
	g_assert (!gkm_transaction_get_failed (transaction));
	gkm_transaction_complete (transaction);
	g_object_unref (transaction);
	g_object_unref (object);
}
