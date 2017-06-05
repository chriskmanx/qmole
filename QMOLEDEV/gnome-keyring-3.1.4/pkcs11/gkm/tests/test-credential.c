/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-credential.c: Test credentials

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

#include "mock-module.h"
#include "mock-locked-object.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-credential.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-secret.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-module.h"

#include "pkcs11i.h"

typedef struct {
	GkmModule *module;
	GkmSession *session;
	GkmObject *object;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	CK_RV rv;
	test->module = mock_module_initialize_and_enter ();
	test->session = mock_module_open_session (TRUE);

	rv = gkm_module_C_Login (test->module, gkm_session_get_handle (test->session), CKU_USER, NULL, 0);
	g_assert (rv == CKR_OK);

	test->object = mock_locked_object_new (test->module, gkm_module_get_manager (test->module));
	gkm_object_expose (test->object, TRUE);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->object);
	mock_module_leave_and_finalize ();
}

static void
test_create (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_CREDENTIAL;
	CK_OBJECT_HANDLE locked = gkm_object_get_handle (test->object);

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_G_OBJECT, &locked, sizeof (locked) },
		{ CKA_VALUE, "mock", 4 },
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_OK);
	g_assert (handle != 0);

	rv = gkm_session_C_DestroyObject (test->session, handle);
	g_assert (rv == CKR_OK);
}

static void
test_create_missing_pin (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_CREDENTIAL;
	CK_OBJECT_HANDLE locked = gkm_object_get_handle (test->object);

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_G_OBJECT, &locked, sizeof (locked) },
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_USER_NOT_LOGGED_IN);
}

static void
test_create_no_object (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_CREDENTIAL;
	CK_BBOOL token = CK_FALSE;
	CK_OBJECT_HANDLE objhand = (CK_ULONG)-1;
	CK_ATTRIBUTE attr;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_CLASS, &klass, sizeof (klass) },
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_OK);
	g_assert (handle != 0);

	attr.type = CKA_G_OBJECT;
	attr.pValue = &objhand;
	attr.ulValueLen = sizeof (objhand);
	rv = gkm_session_C_GetAttributeValue (test->session, handle, &attr, 1);
	g_assert (rv == CKR_OK);
	g_assert (objhand == 0);
}

static void
test_create_invalid_object (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_CREDENTIAL;
	CK_OBJECT_HANDLE locked = 0;
	CK_BBOOL token = CK_FALSE;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_G_OBJECT, &locked, sizeof (locked) },
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_OBJECT_HANDLE_INVALID);
}

static void
test_get_attributes (Test *test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_G_CREDENTIAL;
	CK_OBJECT_HANDLE locked = gkm_object_get_handle (test->object);

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_G_OBJECT, &locked, sizeof (locked) },
		{ CKA_VALUE, "mock", 4 },
	};

	CK_OBJECT_HANDLE handle;
	CK_ATTRIBUTE check;
	CK_ULONG value;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_OK);
	g_assert (handle != 0);

	check.type = CKA_G_OBJECT;
	check.pValue = &value;
	check.ulValueLen = sizeof (value);

	rv = gkm_session_C_GetAttributeValue (test->session, handle, &check, 1);
	g_assert (rv == CKR_OK);
	g_assert (check.ulValueLen == sizeof (value));
	g_assert (value == locked);
}

static void
test_object_property (Test *test, gconstpointer unused)
{
	GkmCredential *auth;
	GkmObject *check;
	CK_RV rv;

	rv = gkm_credential_create (test->module, NULL, test->object, (guchar*)"mock", 4, &auth);
	g_assert (rv == CKR_OK);
	g_assert (auth);

	g_object_get (auth, "object", &check, NULL);
	g_assert (check == test->object);
	g_object_unref (check);

	check = gkm_credential_get_object (auth);
	g_assert (check == test->object);

	g_object_unref (auth);
}

static void
test_login_property (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GkmSecret *check, *secret;
	const gchar *password;
	gsize n_password;
	CK_RV rv;

	rv = gkm_credential_create (test->module, NULL, test->object, (guchar*)"mock", 4, &cred);
	g_assert (rv == CKR_OK);
	g_assert (cred);

	g_object_get (cred, "secret", &check, NULL);
	g_assert (check);
	password = gkm_secret_get_password (check, &n_password);
	g_assert (n_password == 4);
	g_assert (memcmp (password, "mock", 4) == 0);
	g_object_unref (check);

	check = gkm_credential_get_secret (cred);
	g_assert (n_password == 4);
	g_assert (memcmp (password, "mock", 4) == 0);

	secret = gkm_secret_new ((guchar*)"xxx", -1);
	gkm_credential_set_secret (cred, secret);
	check = gkm_credential_get_secret (cred);
	g_assert (check == secret);
	g_object_unref (secret);

	g_object_unref (cred);
}

static GType
boxed_string (void)
{
	static GType type = 0;
	if (!type)
		type = g_boxed_type_register_static ("TestBoxedString",
		                                     (GBoxedCopyFunc)g_strdup,
		                                     (GBoxedFreeFunc)g_free);
	return type;
}

static void
test_data (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	GType type = boxed_string ();
	gchar *check;
	CK_RV rv;

	rv = gkm_credential_create (test->module, NULL, test->object, (guchar*)"mock", 4, &cred);
	g_assert (rv == CKR_OK);
	g_assert (cred);

	g_assert (gkm_credential_peek_data (cred, type) == NULL);

	gkm_credential_set_data (cred, type, "one");

	check = gkm_credential_pop_data (cred, type);
	g_assert_cmpstr ("one", ==, check);
	g_free (check);

	g_assert_cmpstr ("one", ==, gkm_credential_peek_data (cred, type));

	gkm_credential_set_data (cred, type, "ONE");
	g_assert_cmpstr ("ONE", ==, gkm_credential_peek_data (cred, type));

	gkm_credential_set_data (cred, 0, NULL);
	g_assert (gkm_credential_peek_data (cred, 0) == NULL);

	g_object_unref (cred);
}

static void
test_connect_object (Test *test, gconstpointer unused)
{
	GkmCredential *cred;
	CK_RV rv;

	rv = gkm_credential_create (test->module, NULL, NULL, (guchar*)"mock", 4, &cred);
	g_assert (rv == CKR_OK);
	g_assert (cred);

	gkm_credential_connect (cred, test->object);
	g_assert (gkm_credential_get_object (cred) == test->object);

	g_object_unref (cred);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gkm/credential/create", Test, NULL, setup, test_create, teardown);
	g_test_add ("/gkm/credential/create_missing_pin", Test, NULL, setup, test_create_missing_pin, teardown);
	g_test_add ("/gkm/credential/create_no_object", Test, NULL, setup, test_create_no_object, teardown);
	g_test_add ("/gkm/credential/create_invalid_object", Test, NULL, setup, test_create_invalid_object, teardown);
	g_test_add ("/gkm/credential/get_attributes", Test, NULL, setup, test_get_attributes, teardown);
	g_test_add ("/gkm/credential/object_property", Test, NULL, setup, test_object_property, teardown);
	g_test_add ("/gkm/credential/login_property", Test, NULL, setup, test_login_property, teardown);
	g_test_add ("/gkm/credential/data", Test, NULL, setup, test_data, teardown);
	g_test_add ("/gkm/credential/connect_object", Test, NULL, setup, test_connect_object, teardown);

	return g_test_run ();
}
