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

#include "egg/egg-secure-memory.h"
#include "egg/egg-testing.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-mock.h"
#include "gkm/gkm-test.h"

#include "wrap-layer/gkm-wrap-layer.h"
#include "wrap-layer/gkm-wrap-login.h"

#include <glib-object.h>

extern CK_FUNCTION_LIST mock_secret_store;

typedef struct {
	CK_FUNCTION_LIST functions;
	CK_FUNCTION_LIST_PTR module;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	CK_RV rv;

	/* Always start off with test test->functions */
	memcpy (&test->functions, &mock_secret_store, sizeof (test->functions));
	gkm_wrap_layer_reset_modules ();
	gkm_wrap_layer_add_module (&test->functions);
	test->module = gkm_wrap_layer_get_functions ();

	/* Initialize */
	rv = (test->module->C_Initialize) (NULL);
	gkm_assert_cmprv (rv, ==, CKR_OK);
}

static void
teardown (Test *test, gconstpointer unused)
{
	CK_RV rv;

	rv = (test->module->C_Finalize) (NULL);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	test->module = NULL;
}

static void
test_is_usable (Test *test, gconstpointer unused)
{
	gboolean ret;

	ret = gkm_wrap_login_is_usable ();
	g_assert (ret == TRUE);
}

static void
test_usable_fail_open_session (Test *test, gconstpointer unused)
{
	gboolean ret;

	test->functions.C_OpenSession = gkm_mock_fail_C_OpenSession;
	ret = gkm_wrap_login_is_usable ();
	g_assert (ret == FALSE);
}

static void
test_usable_fail_not_trusted (Test *test, gconstpointer unused)
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

static void
test_usable_fail_locked (Test *test, gconstpointer unused)
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

static void
test_lookup_secret_no_match (Test *test, gconstpointer unused)
{
	gchar *password;

	password = gkm_wrap_login_lookup_secret ("invalid", "attribute",
	                                         "second", "attribute", NULL);
	g_assert_cmpstr (password, ==, NULL);
}

static void
test_lookup_secret_and_match (Test *test, gconstpointer unused)
{
	gchar *password;

	/* Secret stored in mock-secret-store.c */
	password = gkm_wrap_login_lookup_secret ("one", "1",
	                                         "two", "2", NULL);
	g_assert_cmpstr (password, ==, "mock");

	egg_secure_free (password);
}

static void
test_lookup_store_secret (Test *test, gconstpointer unused)
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

static void
test_lookup_store_secret_overwrite (Test *test, gconstpointer unused)
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

static void
test_lookup_store_null_secret (Test *test, gconstpointer unused)
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

static void
test_lookup_store_no_attributes_not_stored (Test *test, gconstpointer unused)
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


static void
test_lookup_remove_present (Test *test, gconstpointer unused)
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

static void
test_lookup_remove_no_attributes (Test *test, gconstpointer unused)
{
	guint n_objects, check;

	n_objects = gkm_mock_module_count_objects (0);
	g_assert_cmpuint (n_objects, >, 0);

	/* Shouldn't remove anything if no attributes */
	gkm_wrap_login_remove_secret (NULL);

	check = gkm_mock_module_count_objects (0);
	g_assert_cmpuint (check, ==, n_objects);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/wrap-layer/login-keyring/is_usable", Test, NULL, setup, test_is_usable, teardown);
	g_test_add ("/wrap-layer/login-keyring/usable_fail_open_session", Test, NULL, setup, test_usable_fail_open_session, teardown);
	g_test_add ("/wrap-layer/login-keyring/usable_fail_not_trusted", Test, NULL, setup, test_usable_fail_not_trusted, teardown);
	g_test_add ("/wrap-layer/login-keyring/usable_fail_locked", Test, NULL, setup, test_usable_fail_locked, teardown);
	g_test_add ("/wrap-layer/login-keyring/lookup_secret_no_match", Test, NULL, setup, test_lookup_secret_no_match, teardown);
	g_test_add ("/wrap-layer/login-keyring/lookup_secret_and_match", Test, NULL, setup, test_lookup_secret_and_match, teardown);
	g_test_add ("/wrap-layer/login-keyring/lookup_store_secret", Test, NULL, setup, test_lookup_store_secret, teardown);
	g_test_add ("/wrap-layer/login-keyring/lookup_store_secret_overwrite", Test, NULL, setup, test_lookup_store_secret_overwrite, teardown);
	g_test_add ("/wrap-layer/login-keyring/lookup_store_null_secret", Test, NULL, setup, test_lookup_store_null_secret, teardown);
	g_test_add ("/wrap-layer/login-keyring/lookup_store_no_attributes_not_stored", Test, NULL, setup, test_lookup_store_no_attributes_not_stored, teardown);
	g_test_add ("/wrap-layer/login-keyring/lookup_remove_present", Test, NULL, setup, test_lookup_remove_present, teardown);
	g_test_add ("/wrap-layer/login-keyring/lookup_remove_no_attributes", Test, NULL, setup, test_lookup_remove_no_attributes, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
