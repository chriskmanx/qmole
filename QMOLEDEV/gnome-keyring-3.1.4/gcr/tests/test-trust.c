/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/*
   Copyright (C) 2010 Stefan Walter

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

#include "gcr/gcr.h"
#include "gcr/gcr-internal.h"

#include "gck/gck-mock.h"
#include "gck/gck-test.h"

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11n.h"
#include "pkcs11/pkcs11x.h"

#include "egg/egg-testing.h"

#include <glib.h>

#include <errno.h>

typedef struct {
	CK_FUNCTION_LIST funcs;
	GcrCertificate *certificate;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	GList *modules = NULL;
	CK_FUNCTION_LIST_PTR f;
	GckModule *module;
	gchar *contents;
	const gchar *uris[2];
	gsize len;
	CK_RV rv;

	if (!g_file_get_contents (SRCDIR "/files/der-certificate.crt", &contents, &len, NULL))
		g_assert_not_reached ();
	g_assert (contents);

	test->certificate = gcr_simple_certificate_new (contents, len);
	g_free (contents);

	rv = gck_mock_C_GetFunctionList (&f);
	gck_assert_cmprv (rv, ==, CKR_OK);
	memcpy (&test->funcs, f, sizeof (test->funcs));

	/* Open a session */
	rv = (test->funcs.C_Initialize) (NULL);
	gck_assert_cmprv (rv, ==, CKR_OK);

	g_assert (!modules);
	module = gck_module_new (&test->funcs);
	modules = g_list_prepend (modules, module);
	gcr_pkcs11_set_modules (modules);
	gck_list_unref_free (modules);

	uris[0] = GCK_MOCK_SLOT_ONE_URI;
	uris[1] = NULL;

	gcr_pkcs11_set_trust_store_uri (GCK_MOCK_SLOT_ONE_URI);
	gcr_pkcs11_set_trust_lookup_uris (uris);
}

static void
teardown (Test *test, gconstpointer unused)
{
	CK_RV rv;

	g_object_unref (test->certificate);

	rv = (test->funcs.C_Finalize) (NULL);
	gck_assert_cmprv (rv, ==, CKR_OK);
}

static void
test_is_pinned_none (Test *test, gconstpointer unused)
{
	GError *error = NULL;
	gboolean trust;

	trust = gcr_trust_is_certificate_pinned (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, &error);
	g_assert_cmpint (trust, ==, FALSE);
	g_assert (error == NULL);
}

static void
test_add_and_is_pinned (Test *test, gconstpointer unused)
{
	GError *error = NULL;
	gboolean trust;
	gboolean ret;

	trust = gcr_trust_is_certificate_pinned (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, &error);
	g_assert_cmpint (trust, ==, FALSE);
	g_assert (error == NULL);

	ret = gcr_trust_add_pinned_certificate (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, &error);
	g_assert (ret == TRUE);
	g_assert (error == NULL);

	trust = gcr_trust_is_certificate_pinned (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, &error);
	g_assert_cmpint (trust, ==, TRUE);
	g_assert (error == NULL);
}

static void
test_add_certificate_pinned_fail (Test *test, gconstpointer unused)
{
	GError *error = NULL;
	gboolean ret;

	/* Make this function fail */
	test->funcs.C_CreateObject = gck_mock_fail_C_CreateObject;

	ret = gcr_trust_add_pinned_certificate (test->certificate, GCR_PURPOSE_CLIENT_AUTH, "peer", NULL, &error);
	g_assert (ret == FALSE);
	g_assert_error (error, GCK_ERROR, CKR_FUNCTION_FAILED);
	g_clear_error (&error);
}

static void
test_add_and_remov_pinned (Test *test, gconstpointer unused)
{
	GError *error = NULL;
	gboolean trust;
	gboolean ret;

	ret = gcr_trust_add_pinned_certificate (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, &error);
	g_assert (ret == TRUE);
	g_assert (error == NULL);

	trust = gcr_trust_is_certificate_pinned (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, &error);
	g_assert_cmpint (trust, ==, TRUE);
	g_assert (error == NULL);

	ret = gcr_trust_remove_pinned_certificate (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, &error);
	g_assert (ret == TRUE);
	g_assert (error == NULL);

	trust = gcr_trust_is_certificate_pinned (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, &error);
	g_assert_cmpint (trust, ==, FALSE);
	g_assert (error == NULL);
}

static void
fetch_async_result (GObject *source, GAsyncResult *result, gpointer user_data)
{
	*((GAsyncResult**)user_data) = result;
	g_object_ref (result);
	egg_test_wait_stop ();
}

static void
test_add_and_is_pinned_async (Test *test, gconstpointer unused)
{
	GAsyncResult *result = NULL;
	GError *error = NULL;
	gboolean trust;
	gboolean ret;

	gcr_trust_is_certificate_pinned_async (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result);
	trust = gcr_trust_is_certificate_pinned_finish (result, &error);
	g_assert (trust == FALSE);
	g_assert (error == NULL);
	g_object_unref (result);
	result = NULL;

	gcr_trust_add_pinned_certificate_async (test->certificate, GCR_PURPOSE_EMAIL, "host",
	                                        NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result);
	ret = gcr_trust_add_pinned_certificate_finish (result, &error);
	g_assert (ret == TRUE);
	g_assert (error == NULL);
	g_object_unref (result);
	result = NULL;

	gcr_trust_is_certificate_pinned_async (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result);
	trust = gcr_trust_is_certificate_pinned_finish (result, &error);
	g_assert (trust == TRUE);
	g_assert (error == NULL);
	g_object_unref (result);
	result = NULL;
}

static void
test_add_and_remov_pinned_async (Test *test, gconstpointer unused)
{
	GAsyncResult *result = NULL;
	GError *error = NULL;
	gboolean trust;
	gboolean ret;

	gcr_trust_add_pinned_certificate_async (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result);
	ret = gcr_trust_add_pinned_certificate_finish (result, &error);
	g_assert (ret == TRUE);
	g_assert (error == NULL);
	g_object_unref (result);
	result = NULL;

	gcr_trust_is_certificate_pinned_async (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result);
	trust = gcr_trust_is_certificate_pinned_finish (result, &error);
	g_assert (trust == TRUE);
	g_assert (error == NULL);
	g_object_unref (result);
	result = NULL;

	gcr_trust_remove_pinned_certificate_async (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result);
	ret = gcr_trust_remove_pinned_certificate_finish (result, &error);
	g_assert (ret == TRUE);
	g_assert (error == NULL);
	g_object_unref (result);
	result = NULL;

	gcr_trust_is_certificate_pinned_async (test->certificate, GCR_PURPOSE_EMAIL, "host", NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result);
	trust = gcr_trust_is_certificate_pinned_finish (result, &error);
	g_assert (trust == FALSE);
	g_assert (error == NULL);
	g_object_unref (result);
	result = NULL;
}

static void
test_is_certificate_anchored_not (Test *test, gconstpointer unused)
{
	GError *error = NULL;
	gboolean ret;

	ret = gcr_trust_is_certificate_anchored (test->certificate, GCR_PURPOSE_CLIENT_AUTH, NULL, &error);
	g_assert (ret == FALSE);
	g_assert (error == NULL);
}

static void
test_is_certificate_anchored_yes (Test *test, gconstpointer unused)
{
	GError *error = NULL;
	GckAttributes *attrs;
	gconstpointer der;
	gsize n_der;
	gboolean ret;

	/* Create a certificate root trust */
	attrs = gck_attributes_new ();
	der = gcr_certificate_get_der_data (test->certificate, &n_der);
	gck_attributes_add_data (attrs, CKA_X_CERTIFICATE_VALUE, der, n_der);
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_X_TRUST_ASSERTION);
	gck_attributes_add_boolean (attrs, CKA_TOKEN, TRUE);
	gck_attributes_add_string (attrs, CKA_X_PURPOSE, GCR_PURPOSE_CLIENT_AUTH);
	gck_attributes_add_ulong (attrs, CKA_X_ASSERTION_TYPE, CKT_X_ANCHORED_CERTIFICATE);
	gck_mock_module_take_object (attrs);

	ret = gcr_trust_is_certificate_anchored (test->certificate, GCR_PURPOSE_CLIENT_AUTH, NULL, &error);
	g_assert (ret == TRUE);
	g_assert (error == NULL);
}

static void
test_is_certificate_anchored_async (Test *test, gconstpointer unused)
{
	GAsyncResult *result = NULL;
	GError *error = NULL;
	gboolean ret;

	gcr_trust_is_certificate_anchored_async (test->certificate, GCR_PURPOSE_CLIENT_AUTH, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result);

	ret = gcr_trust_is_certificate_anchored_finish (result, &error);
	g_assert (ret == FALSE);
	g_assert (error == NULL);

	g_object_unref (result);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);
	g_set_prgname ("test-trust");

	g_test_add ("/gcr/trust/is_pinned_none", Test, NULL, setup, test_is_pinned_none, teardown);
	g_test_add ("/gcr/trust/add_and_is_pinned", Test, NULL, setup, test_add_and_is_pinned, teardown);
	g_test_add ("/gcr/trust/add_certificate_pinned_fail", Test, NULL, setup, test_add_certificate_pinned_fail, teardown);
	g_test_add ("/gcr/trust/add_and_remov_pinned", Test, NULL, setup, test_add_and_remov_pinned, teardown);
	g_test_add ("/gcr/trust/add_and_is_pinned_async", Test, NULL, setup, test_add_and_is_pinned_async, teardown);
	g_test_add ("/gcr/trust/add_and_remov_pinned_async", Test, NULL, setup, test_add_and_remov_pinned_async, teardown);
	g_test_add ("/gcr/trust/is_certificate_anchored_not", Test, NULL, setup, test_is_certificate_anchored_not, teardown);
	g_test_add ("/gcr/trust/is_certificate_anchored_yes", Test, NULL, setup, test_is_certificate_anchored_yes, teardown);
	g_test_add ("/gcr/trust/is_certificate_anchored_async", Test, NULL, setup, test_is_certificate_anchored_async, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
