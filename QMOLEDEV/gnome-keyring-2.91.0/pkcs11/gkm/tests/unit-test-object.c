/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-timer.c: Test thread timer functionality

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

#include "test-suite.h"
#include "test-module.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-module.h"
#include "gkm/gkm-transaction.h"

#include "pkcs11i.h"

static GkmModule *module = NULL;
static GkmSession *session = NULL;
static guchar *certificate_data = NULL;
static gsize certificate_n_data = 0;

DEFINE_SETUP(object_setup)
{
	module = test_module_initialize_and_enter ();
	session = test_module_open_session (TRUE);
	certificate_data = testing_data_read ("test-certificate-1.der", &certificate_n_data);
}

DEFINE_TEARDOWN(object_teardown)
{
	g_free (certificate_data);
	certificate_data = NULL;
	certificate_n_data = 0;

	test_module_leave_and_finalize ();
	module = NULL;
	session = NULL;
}

static gboolean
check_object_exists (CK_OBJECT_HANDLE handle)
{
	CK_BBOOL token;
	CK_ATTRIBUTE attr = { CKA_TOKEN, &token, sizeof (token) };
	CK_RV rv;

	rv = gkm_session_C_GetAttributeValue (session, handle, &attr, 1);
	if (rv == CKR_OBJECT_HANDLE_INVALID)
		return FALSE;

	g_assert (rv == CKR_OK);
	return TRUE;
}

DEFINE_TEST(object_create_destroy_transient)
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
		{ CKA_VALUE, certificate_data, certificate_n_data },
	};

	CK_ATTRIBUTE lookup = { CKA_GNOME_TRANSIENT, &transient, sizeof (transient) };
	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_OK);
	g_assert (handle != 0);

	g_assert (check_object_exists (handle));

	transient = CK_FALSE;
	rv = gkm_session_C_GetAttributeValue (session, handle, &lookup, 1);
	g_assert (rv == CKR_OK);
	g_assert (transient == CK_TRUE);

	rv = gkm_session_C_DestroyObject (session, handle);
	g_assert (rv == CKR_OK);

	g_assert (!check_object_exists (handle));
}

DEFINE_TEST(object_transient_transacted_fail)
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
		{ CKA_VALUE, certificate_data, certificate_n_data },

		/* An invalid attribute, should cause transaction to fail */
		{ CKA_BITS_PER_PIXEL, &invalid, sizeof (invalid) }
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_ATTRIBUTE_TYPE_INVALID);
}

DEFINE_TEST(object_create_transient_bad_value)
{
	CK_OBJECT_CLASS klass = CKO_CERTIFICATE;
	CK_CERTIFICATE_TYPE type = CKC_X_509;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_GNOME_TRANSIENT, NULL, 0 },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_CERTIFICATE_TYPE, &type, sizeof (type) },
		{ CKA_VALUE, certificate_data, certificate_n_data },
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	/* Can't have a non-transient object that auto-destructs */
	rv = gkm_session_C_CreateObject (session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

DEFINE_TEST(object_create_auto_destruct)
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
		{ CKA_VALUE, certificate_data, certificate_n_data },
	};

	CK_BBOOL transient;

	CK_ATTRIBUTE lookups[] = {
		{ CKA_G_DESTRUCT_AFTER, &check, sizeof (check) },
		{ CKA_GNOME_TRANSIENT, &transient, sizeof (transient) }
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	rv = gkm_session_C_CreateObject (session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_OK);
	g_assert (handle != 0);

	g_assert (check_object_exists (handle));

	transient = CK_FALSE;
	rv = gkm_session_C_GetAttributeValue (session, handle, lookups, G_N_ELEMENTS (lookups));
	g_assert (rv == CKR_OK);
	g_assert (transient == TRUE);
	g_assert (memcmp (&lifetime, &check, sizeof (lifetime)) == 0);

	test_module_leave ();
	testing_wait_until (2200);
	test_module_enter ();

	g_assert (!check_object_exists (handle));
}

DEFINE_TEST(object_create_auto_destruct_not_transient)
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
		{ CKA_VALUE, certificate_data, certificate_n_data },
	};

	CK_OBJECT_HANDLE handle;
	CK_RV rv;

	/* Can't have a non-transient object that auto-destructs */
	rv = gkm_session_C_CreateObject (session, attrs, G_N_ELEMENTS (attrs), &handle);
	g_assert (rv == CKR_TEMPLATE_INCONSISTENT);
}

DEFINE_TEST(object_expose)
{
	CK_OBJECT_HANDLE handle;
	GkmManager *manager;
	GkmObject *check, *object;

	manager = gkm_session_get_manager (session);
	object = test_module_object_new (session);

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

DEFINE_TEST(object_expose_transaction)
{
	CK_OBJECT_HANDLE handle;
	GkmManager *manager;
	GkmObject *check, *object;
	GkmTransaction *transaction;

	manager = gkm_session_get_manager (session);
	object = test_module_object_new (session);

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
