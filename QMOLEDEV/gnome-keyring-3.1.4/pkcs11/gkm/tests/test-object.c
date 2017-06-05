/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-object.c: Test GkmObject

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

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-module.h"
#include "gkm/gkm-transaction.h"

#include "egg/egg-testing.h"

#include "pkcs11i.h"

typedef struct {
	GkmModule *module;
	GkmSession *session;
	gchar *certificate_data;
	gsize n_certificate_data;
} Test;

static void
setup (Test* test, gconstpointer unused)
{
	test->module = mock_module_initialize_and_enter ();
	test->session = mock_module_open_session (TRUE);

	if (!g_file_get_contents (SRCDIR "/files/test-certificate-1.der", &test->certificate_data, &test->n_certificate_data, NULL))
		g_assert_not_reached ();
}

static void
teardown (Test* test, gconstpointer unused)
{
	g_free (test->certificate_data);
	mock_module_leave_and_finalize ();
}

static gboolean
check_object_exists (CK_OBJECT_HANDLE handle, Test *test)
{
	CK_BBOOL token;
	CK_ATTRIBUTE attr = { CKA_TOKEN, &token, sizeof (token) };
	CK_RV rv;

	rv = gkm_session_C_GetAttributeValue (test->session, handle, &attr, 1);
	if (rv == CKR_OBJECT_HANDLE_INVALID)
		return FALSE;

	g_assert (rv == CKR_OK);
	return TRUE;
}

static void
test_create_destroy_transient (Test* test, gconstpointer unused)
{
	CK_BBOOL transient = CK_TRUE;
	CK_BBOOL token = CK_TRUE;
	CK_OBJECT_CLASS klass = CKO_CERTIFICATE;
	CK_CERTIFICATE_TYPE type = CKC_X_509;

	CK_ATTRIBUTE attrs[] = {
	        { CKA_TOKEN, &token, sizeof (token) },
		{ CKA_GNOME_TRANSIENT, &transient, sizeof (transient) },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_CERTIFICATE_TYPE, &type, sizeof (type) },
		{ CKA_VALUE, test->certificate_data, test->n_certificate_data },
	};

	CK_ATTRIBUTE lookup = { CKA_GNOME_TRANSIENT, &transient, sizeof (transient) };
	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_OK);
	g_assert (handle != 0);

	g_assert (check_object_exists (handle, test));

	transient = CK_FALSE;
	rv = gkm_session_C_GetAttributeValue (test->session, handle, &lookup, 1);
	g_assert (rv == CKR_OK);
	g_assert (transient == CK_TRUE);

	rv = gkm_session_C_DestroyObject (test->session, handle);
	g_assert (rv == CKR_OK);

	g_assert (!check_object_exists (handle, test));
}

static void
test_transient_transacted_fail (Test* test, gconstpointer unused)
{
	CK_BBOOL transient = CK_TRUE;
	CK_BBOOL token = CK_TRUE;
	CK_OBJECT_CLASS klass = CKO_CERTIFICATE;
	CK_CERTIFICATE_TYPE type = CKC_X_509;
	CK_ULONG invalid = 4;

	CK_ATTRIBUTE attrs[] = {
	        { CKA_TOKEN, &token, sizeof (token) },
		{ CKA_GNOME_TRANSIENT, &transient, sizeof (transient) },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_CERTIFICATE_TYPE, &type, sizeof (type) },
		{ CKA_VALUE, test->certificate_data, test->n_certificate_data },

		/* An invalid attribute, should cause transaction to fail */
		{ CKA_BITS_PER_PIXEL, &invalid, sizeof (invalid) }
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_ATTRIBUTE_TYPE_INVALID);
}

static void
test_create_transient_bad_value (Test* test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_CERTIFICATE;
	CK_CERTIFICATE_TYPE type = CKC_X_509;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_GNOME_TRANSIENT, NULL, 0 },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_CERTIFICATE_TYPE, &type, sizeof (type) },
		{ CKA_VALUE, test->certificate_data, test->n_certificate_data },
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	/* Can't have a non-transient object that auto-destructs */
	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_create_auto_destruct (Test* test, gconstpointer unused)
{
	CK_BBOOL token = CK_FALSE;
	CK_OBJECT_CLASS klass = CKO_CERTIFICATE;
	CK_CERTIFICATE_TYPE type = CKC_X_509;
	CK_ULONG lifetime = 2;
	CK_ULONG check;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_G_DESTRUCT_AFTER, &lifetime, sizeof (lifetime) },
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_CERTIFICATE_TYPE, &type, sizeof (type) },
		{ CKA_VALUE, test->certificate_data, test->n_certificate_data },
	};

	CK_BBOOL transient;

	CK_ATTRIBUTE lookups[] = {
		{ CKA_G_DESTRUCT_AFTER, &check, sizeof (check) },
		{ CKA_GNOME_TRANSIENT, &transient, sizeof (transient) }
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_OK);
	g_assert (handle != 0);

	g_assert (check_object_exists (handle, test));

	transient = CK_FALSE;
	rv = gkm_session_C_GetAttributeValue (test->session, handle, lookups, G_N_ELEMENTS (lookups));
	g_assert (rv == CKR_OK);
	g_assert (transient == TRUE);
	g_assert (memcmp (&lifetime, &check, sizeof (lifetime)) == 0);

	mock_module_leave ();
	egg_test_wait_until (2200);
	mock_module_enter ();

	g_assert (!check_object_exists (handle, test));
}

static void
test_create_auto_destruct_not_transient (Test* test, gconstpointer unused)
{
	CK_OBJECT_CLASS klass = CKO_CERTIFICATE;
	CK_CERTIFICATE_TYPE type = CKC_X_509;
	CK_BBOOL transient = CK_FALSE;
	CK_ULONG after = 1;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_G_DESTRUCT_AFTER, &after, sizeof (after) },
		{ CKA_GNOME_TRANSIENT, &transient, sizeof (transient) },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_CERTIFICATE_TYPE, &type, sizeof (type) },
		{ CKA_VALUE, test->certificate_data, test->n_certificate_data },
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	/* Can't have a non-transient object that auto-destructs */
	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_TEMPLATE_INCONSISTENT);
}

static void
test_expose (Test* test, gconstpointer unused)
{
	CK_OBJECT_HANDLE handle;
	GkmManager *manager;
	GkmObject *check, *object;

	manager = gkm_session_get_manager (test->session);
	object = mock_module_object_new (test->session);

	handle = gkm_object_get_handle (object);
	gkm_object_expose (object, TRUE);

	/* Now it should have a handle, and be visible */
	check = gkm_manager_find_by_handle (manager, handle);
	g_assert (check == object);

	gkm_object_expose (object, FALSE);

	/* Now should be invisible */
	check = gkm_manager_find_by_handle (manager, handle);
	g_assert (check == NULL);
}

static void
test_expose_transaction (Test* test, gconstpointer unused)
{
	CK_OBJECT_HANDLE handle;
	GkmManager *manager;
	GkmObject *check, *object;
	GkmTransaction *transaction;

	manager = gkm_session_get_manager (test->session);
	object = mock_module_object_new (test->session);

	handle = gkm_object_get_handle (object);
	transaction = gkm_transaction_new ();

	/* Should be hidden */
	gkm_object_expose (object, FALSE);
	check = gkm_manager_find_by_handle (manager, handle);
	g_assert (check == NULL);

	/* Now it should have a handle, and be visible */
	gkm_object_expose_full (object, transaction, TRUE);
	check = gkm_manager_find_by_handle (manager, handle);
	g_assert (check == object);

	gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);
	gkm_transaction_complete (transaction);

	/* Now should be invisible */
	check = gkm_manager_find_by_handle (manager, handle);
	g_assert (check == NULL);

	g_object_unref (transaction);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gkm/object/create_destroy_transient", Test, NULL, setup, test_create_destroy_transient, teardown);
	g_test_add ("/gkm/object/transient_transacted_fail", Test, NULL, setup, test_transient_transacted_fail, teardown);
	g_test_add ("/gkm/object/create_transient_bad_value", Test, NULL, setup, test_create_transient_bad_value, teardown);
	g_test_add ("/gkm/object/create_auto_destruct", Test, NULL, setup, test_create_auto_destruct, teardown);
	g_test_add ("/gkm/object/create_auto_destruct_not_transient", Test, NULL, setup, test_create_auto_destruct_not_transient, teardown);
	g_test_add ("/gkm/object/expose", Test, NULL, setup, test_expose, teardown);
	g_test_add ("/gkm/object/expose_transaction", Test, NULL, setup, test_expose_transaction, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
