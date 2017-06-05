/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "test-suite.h"

#include "egg/egg-secure-memory.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-mock.h"
#include "gkm/gkm-test.h"

#include "wrap-layer/gkm-wrap-layer.h"
#include "wrap-layer/gkm-wrap-login.h"

extern CK_FUNCTION_LIST mock_secret_store;
static CK_FUNCTION_LIST functions;
static CK_FUNCTION_LIST_PTR module = NULL;

DEFINE_SETUP (login_keyring)
{
	CK_RV rv;

	/* Always start off with test functions */
	memcpy (&functions, &mock_secret_store, sizeof (functions));
	gkm_wrap_layer_reset_modules ();
	gkm_wrap_layer_add_module (&functions);
	module = gkm_wrap_layer_get_functions ();

	/* Initialize */
	rv = (module->C_Initialize) (NULL);
	gkm_assert_cmprv (rv, ==, CKR_OK);
}

DEFINE_TEARDOWN (login_keyring)
{
	CK_RV rv;

	rv = (module->C_Finalize) (NULL);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	module = NULL;
}

DEFINE_TEST (login_is_usable)
{
	gboolean ret;

	ret = gkm_wrap_login_is_usable ();
	g_assert (ret == TRUE);
}

DEFINE_TEST (login_usable_fail_open_session)
{
	gboolean ret;

	functions.C_OpenSession = gkm_mock_fail_C_OpenSession;
	ret = gkm_wrap_login_is_usable ();
	g_assert (ret == FALSE);
}

DEFINE_TEST (login_usable_fail_not_trusted)
{
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE attr;
	CK_BBOOL bval;
	gboolean ret;

	ret = gkm_wrap_login_is_usable ();
	g_assert (ret == TRUE);

	bval = CK_TRUE;
	attr.type = CKA_G_LOGIN_COLLECTION;
	attr.pValue = &bval;
	attr.ulValueLen = sizeof (bval);

	object = gkm_mock_module_find_object (0, &attr, 1);
	gkm_assert_cmpulong (object, !=, 0);

	bval = CK_FALSE;
	attr.type = CKA_TRUSTED;
	attr.pValue = &bval;
	attr.ulValueLen = sizeof (bval);

	gkm_mock_module_set_object (object, &attr, 1);

	/* Not trusted, so no longer usable */
	ret = gkm_wrap_login_is_usable ();
	g_assert (ret == FALSE);
}

DEFINE_TEST (login_usable_fail_locked)
{
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE attr;
	CK_BBOOL bval;
	gboolean ret;

	ret = gkm_wrap_login_is_usable ();
	g_assert (ret == TRUE);

	bval = CK_TRUE;
	attr.type = CKA_G_LOGIN_COLLECTION;
	attr.pValue = &bval;
	attr.ulValueLen = sizeof (bval);

	object = gkm_mock_module_find_object (0, &attr, 1);
	gkm_assert_cmpulong (object, !=, 0);

	bval = CK_TRUE;
	attr.type = CKA_G_LOCKED;
	attr.pValue = &bval;
	attr.ulValueLen = sizeof (bval);

	gkm_mock_module_set_object (object, &attr, 1);

	/* Not unlocked, so no longer usable */
	ret = gkm_wrap_login_is_usable ();
	g_assert (ret == FALSE);
}

DEFINE_TEST (login_lookup_secret_no_match)
{
	gchar *password;

	password = gkm_wrap_login_lookup_secret ("invalid", "attribute",
	                                         "second", "attribute", NULL);
	g_assert_cmpstr (password, ==, NULL);
}

DEFINE_TEST (login_lookup_secret_and_match)
{
	gchar *password;

	/* Secret stored in mock-secret-store.c */
	password = gkm_wrap_login_lookup_secret ("one", "1",
	                                         "two", "2", NULL);
	g_assert_cmpstr (password, ==, "mock");

	egg_secure_free (password);
}

DEFINE_TEST (login_lookup_store_secret)
{
	CK_OBJECT_CLASS klass = CKO_SECRET_KEY;
	CK_BBOOL tval = CK_TRUE;
	CK_ATTRIBUTE attrs[] = {
		{ CKA_LABEL, "Unlock password for: The label", 30 },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_VALUE, "the password", 12 },
		{ CKA_TOKEN, &tval, sizeof (tval) },
		{ CKA_G_COLLECTION, "login", 5 },
		{ CKA_G_FIELDS, "one\0" "1\0" "three\0" "3\0", 14 },
	};

	CK_OBJECT_HANDLE object;

	/* Secret stored in mock-secret-store.c */
	gkm_wrap_login_attach_secret ("The label", "the password",
	                              "one", "1",
	                              "three", "3", NULL);

	object = gkm_mock_module_find_object (0, attrs, G_N_ELEMENTS (attrs));
	gkm_assert_cmpulong (object, !=, 0);
}

DEFINE_TEST (login_lookup_store_secret_overwrite)
{
	CK_OBJECT_CLASS klass = CKO_SECRET_KEY;
	CK_BBOOL tval = CK_TRUE;
	CK_ATTRIBUTE attrs[] = {
		{ CKA_VALUE, "the password", 12 },
		{ CKA_LABEL, "Unlock password for: The label", 30 },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_TOKEN, &tval, sizeof (tval) },
		{ CKA_G_COLLECTION, "login", 5 },
		{ CKA_G_FIELDS, "one\0" "1\0" "three\0" "3\0", 14 },
	};

	CK_OBJECT_HANDLE object1, object2;

	/* Secret stored in mock-secret-store.c */
	gkm_wrap_login_attach_secret ("The label", "the password",
	                              "one", "1",
	                              "three", "3", NULL);

	object1 = gkm_mock_module_find_object (0, attrs, G_N_ELEMENTS (attrs));
	gkm_assert_cmpulong (object1, !=, 0);

	/* Secret stored in mock-secret-store.c */
	gkm_wrap_login_attach_secret ("The label", "other",
	                              "one", "1",
	                              "three", "3", NULL);

	attrs[0].pValue = "other";
	attrs[0].ulValueLen = 5;

	object2 = gkm_mock_module_find_object (0, attrs, G_N_ELEMENTS (attrs));
	gkm_assert_cmpulong (object2, !=, 0);

	/* Should have been stored on same object */
	gkm_assert_cmpulong (object1, ==, object2);
}

DEFINE_TEST (login_lookup_store_null_secret)
{
	CK_OBJECT_CLASS klass = CKO_SECRET_KEY;
	CK_BBOOL tval = CK_TRUE;
	CK_ATTRIBUTE attrs[] = {
		{ CKA_LABEL, "Unlock password for: The label", 30 },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_VALUE, "", 0 },
		{ CKA_TOKEN, &tval, sizeof (tval) },
		{ CKA_G_COLLECTION, "login", 5 },
		{ CKA_G_FIELDS, "one\0" "1\0" "three\0" "3\0", 14 },
	};

	CK_OBJECT_HANDLE object;

	gkm_wrap_login_attach_secret ("The label", NULL,
	                              "one", "1",
	                              "three", "3", NULL);

	object = gkm_mock_module_find_object (0, attrs, G_N_ELEMENTS (attrs));
	gkm_assert_cmpulong (object, !=, 0);
}

DEFINE_TEST (login_lookup_store_no_attributes_not_stored)
{
	CK_OBJECT_CLASS klass = CKO_SECRET_KEY;
	CK_BBOOL tval = CK_TRUE;
	CK_ATTRIBUTE attrs[] = {
		{ CKA_LABEL, "Unlock password for: The label", 30 },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_VALUE, "the password", 0 },
		{ CKA_TOKEN, &tval, sizeof (tval) },
		{ CKA_G_COLLECTION, "login", 5 },
		{ CKA_G_FIELDS, "", 0 },
	};

	CK_OBJECT_HANDLE object;

	gkm_wrap_login_attach_secret ("The label", "the password", NULL);

	object = gkm_mock_module_find_object (0, attrs, G_N_ELEMENTS (attrs));
	gkm_assert_cmpulong (object, ==, 0);
}


DEFINE_TEST (login_lookup_remove_present)
{
	CK_OBJECT_CLASS klass = CKO_SECRET_KEY;
	CK_BBOOL tval = CK_TRUE;
	CK_ATTRIBUTE attrs[] = {
		{ CKA_LABEL, "Unlock password for: Mock", 25 },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_VALUE, "mock", 4 },
		{ CKA_TOKEN, &tval, sizeof (tval) },
		{ CKA_G_COLLECTION, "login", 5 },
		{ CKA_G_FIELDS, "one\0" "1\0" "two\0" "2\0", 12 },
	};

	CK_OBJECT_HANDLE object;

	/* This object is created in mock-secret-store.c */
	object = gkm_mock_module_find_object (0, attrs, G_N_ELEMENTS (attrs));
	gkm_assert_cmpulong (object, !=, 0);

	gkm_wrap_login_remove_secret ("one", "1", "two", "2", NULL);

	/* No longer there */
	object = gkm_mock_module_find_object (0, attrs, G_N_ELEMENTS (attrs));
	gkm_assert_cmpulong (object, ==, 0);
}

DEFINE_TEST (login_lookup_remove_no_attributes)
{
	guint n_objects, check;

	n_objects = gkm_mock_module_count_objects (0);
	g_assert_cmpuint (n_objects, >, 0);

	/* Shouldn't remove anything if no attributes */
	gkm_wrap_login_remove_secret (NULL);

	check = gkm_mock_module_count_objects (0);
	g_assert_cmpuint (check, ==, n_objects);
}
