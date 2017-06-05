/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/*
   Copyright (C) 2010 Collabora Ltd

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

   Author: Stef Walter <stefw@collabora.co.uk>
*/

#include "config.h"

#include "gcr/gcr.h"
#include "gcr/gcr-internal.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"
#include "egg/egg-testing.h"

#include "gck/gck-mock.h"
#include "gck/gck-test.h"

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11x.h"

#include <glib.h>

#include <errno.h>
#include <string.h>

/* ---------------------------------------------------------------------------
 * A Mock certificate that checks that it's always called on the
 * same thread. A GcrCertificate implemented on top of a non-thread-safe
 * crypto library would require this behavior.
 */

GType               mock_certificate_get_type               (void);

#define MOCK_CERTIFICATE(obj) \
	(G_TYPE_CHECK_INSTANCE_CAST ((obj), mock_certificate_get_type (), MockCertificate))

typedef struct _MockCertificate {
	GObject parent;
	GThread *created_on;
	gpointer data;
	gsize n_data;
} MockCertificate;

typedef struct _MockCertificateClass {
	GObjectClass parent_class;
} MockCertificateClass;

static void mock_certificate_iface (GcrCertificateIface *iface);
G_DEFINE_TYPE_WITH_CODE (MockCertificate, mock_certificate, G_TYPE_OBJECT,
	GCR_CERTIFICATE_MIXIN_IMPLEMENT_COMPARABLE ();
	G_IMPLEMENT_INTERFACE (GCR_TYPE_CERTIFICATE, mock_certificate_iface);
);

static void
mock_certificate_init (MockCertificate *self)
{
	self->created_on = g_thread_self ();
}

static void
mock_certificate_finalize (GObject *obj)
{
	MockCertificate *self = MOCK_CERTIFICATE (obj);
	g_assert (self->created_on == g_thread_self ());
	g_free (self->data);
	G_OBJECT_CLASS (mock_certificate_parent_class)->finalize (obj);
}

static void
mock_certificate_class_init (MockCertificateClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	gobject_class->finalize = mock_certificate_finalize;
	gobject_class->get_property = gcr_certificate_mixin_get_property;
	gcr_certificate_mixin_class_init (gobject_class);
}

static gconstpointer
mock_certificate_real_get_der_data (GcrCertificate *base, gsize *n_data)
{
	MockCertificate *self = MOCK_CERTIFICATE (base);
	g_assert (self->created_on == g_thread_self ());
	*n_data = self->n_data;
	return self->data;
}

static void
mock_certificate_iface (GcrCertificateIface *iface)
{
	iface->get_der_data = (gpointer)mock_certificate_real_get_der_data;
}

static GcrCertificate*
mock_certificate_new (gconstpointer data, gsize n_data)
{
	MockCertificate *self = g_object_new (mock_certificate_get_type (), NULL);
	self->data = g_memdup (data, n_data);
	self->n_data = n_data;
	g_assert (self->created_on == g_thread_self ());
	return GCR_CERTIFICATE (self);
}

/* ----------------------------------------------------------------------------
 * TESTS
 */

typedef struct {
	GcrCertificate *cert_self;
	GcrCertificate *cert_ca;
	GcrCertificate *cert_signed;
	CK_FUNCTION_LIST funcs;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	GList *modules = NULL;
	CK_FUNCTION_LIST_PTR f;
	gchar *contents;
	gsize n_contents;
	const gchar *uris[2];
	CK_RV rv;
	GckModule *module;

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
	uris[0] = GCK_MOCK_SLOT_ONE_URI;
	uris[1] = NULL;
	gcr_pkcs11_set_trust_lookup_uris (uris);
	gcr_pkcs11_set_trust_store_uri (GCK_MOCK_SLOT_ONE_URI);
	gck_list_unref_free (modules);

	/* A self-signed certificate */
	if (!g_file_get_contents (SRCDIR "/files/der-certificate.crt", &contents, &n_contents, NULL))
		g_assert_not_reached ();
	test->cert_self = gcr_simple_certificate_new (contents, n_contents);
	g_free (contents);

	/* A signed certificate */
	if (!g_file_get_contents (SRCDIR "/files/dhansak-collabora.cer", &contents, &n_contents, NULL))
		g_assert_not_reached ();
	test->cert_signed = mock_certificate_new (contents, n_contents);
	g_free (contents);

	/* The signer for the above certificate */
	if (!g_file_get_contents (SRCDIR "/files/collabora-ca.cer", &contents, &n_contents, NULL))
		g_assert_not_reached ();
	test->cert_ca = mock_certificate_new (contents, n_contents);
	g_free (contents);
}

static void
add_certificate_to_module (GcrCertificate *certificate)
{
	GckAttributes *attrs;
	gconstpointer data;
	gsize n_data, n_subject;
	gpointer subject;

	data = gcr_certificate_get_der_data (certificate, &n_data);
	g_assert (data);

	subject = gcr_certificate_get_subject_raw (certificate, &n_subject);
	g_assert (subject);

	/* Add a certificate to the module */
	attrs = gck_attributes_new ();
	gck_attributes_add_data (attrs, CKA_VALUE, data, n_data);
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_CERTIFICATE);
	gck_attributes_add_ulong (attrs, CKA_CERTIFICATE_TYPE, CKC_X_509);
	gck_attributes_add_data (attrs, CKA_SUBJECT, subject, n_subject);
	gck_mock_module_take_object (attrs);

	g_free (subject);
}

static void
add_anchor_to_module (GcrCertificate *certificate, const gchar *purpose)
{
	GckAttributes *attrs;
	gconstpointer data;
	gsize n_data;

	data = gcr_certificate_get_der_data (certificate, &n_data);
	g_assert (data);

	/* And add a pinned certificate for the signed certificate */
	attrs = gck_attributes_new ();
	gck_attributes_add_data (attrs, CKA_X_CERTIFICATE_VALUE, data, n_data);
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_X_TRUST_ASSERTION);
	gck_attributes_add_ulong (attrs, CKA_X_ASSERTION_TYPE, CKT_X_ANCHORED_CERTIFICATE);
	gck_attributes_add_string (attrs, CKA_X_PURPOSE, purpose);
	gck_mock_module_take_object (attrs);
}

static void
add_pinned_to_module (GcrCertificate *certificate, const gchar *purpose, const gchar *host)
{
	GckAttributes *attrs;
	gconstpointer data;
	gsize n_data;

	data = gcr_certificate_get_der_data (certificate, &n_data);
	g_assert (data);

	/* And add a pinned certificate for the signed certificate */
	attrs = gck_attributes_new ();
	gck_attributes_add_data (attrs, CKA_X_CERTIFICATE_VALUE, data, n_data);
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_X_TRUST_ASSERTION);
	gck_attributes_add_ulong (attrs, CKA_X_ASSERTION_TYPE, CKT_X_PINNED_CERTIFICATE);
	gck_attributes_add_string (attrs, CKA_X_PURPOSE, purpose);
	gck_attributes_add_string (attrs, CKA_X_PEER, host);
	gck_mock_module_take_object (attrs);
}

static void
teardown (Test *test, gconstpointer unused)
{
	CK_RV rv;

	g_object_unref (test->cert_self);
	g_object_unref (test->cert_signed);
	g_object_unref (test->cert_ca);

	rv = (test->funcs.C_Finalize) (NULL);
	gck_assert_cmprv (rv, ==, CKR_OK);
}

static void
test_new (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;

	chain = gcr_certificate_chain_new ();

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_UNKNOWN);
	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 0);

	g_assert (gcr_certificate_chain_get_endpoint (chain) == NULL);

	g_object_unref (chain);
}

static void
test_new_with_cert (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GcrCertificate *check;
	guint status, length;

	chain = gcr_certificate_chain_new ();
	gcr_certificate_chain_add (chain, test->cert_signed);
	gcr_certificate_chain_add (chain, test->cert_ca);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_UNKNOWN);
	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 2);

	status = G_MAXUINT;
	length = 0;
	g_object_get (chain, "status", &status, "length", &length, NULL);
	g_assert_cmpuint (status, ==, GCR_CERTIFICATE_CHAIN_UNKNOWN);
	g_assert_cmpuint (length, ==, 2);

	check = gcr_certificate_chain_get_certificate (chain, 1);
	g_assert (check == test->cert_ca);

	/* Not yet completed */
	check = gcr_certificate_chain_get_anchor (chain);
	g_assert (check == NULL);

	check = gcr_certificate_chain_get_endpoint (chain);
	g_assert (check == test->cert_signed);

	g_object_unref (chain);
}

static void
test_selfsigned (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;

	chain = gcr_certificate_chain_new ();

	/* Add a self-signed certificate */
	gcr_certificate_chain_add (chain, test->cert_self);

	if (!gcr_certificate_chain_build (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                  NULL, 0, NULL, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_SELFSIGNED);

	g_object_unref (chain);
}

static void
test_incomplete (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;

	chain = gcr_certificate_chain_new ();

	/* Add a signed certificate */
	gcr_certificate_chain_add (chain, test->cert_signed);

	if (!gcr_certificate_chain_build (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                  NULL, 0, NULL, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_INCOMPLETE);

	g_object_unref (chain);
}

static void
test_empty (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;

	chain = gcr_certificate_chain_new ();

	/* Add no certificate */

	if (!gcr_certificate_chain_build (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                  NULL, 0, NULL, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_UNKNOWN);

	g_object_unref (chain);
}

static void
test_trim_extras (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;

	chain = gcr_certificate_chain_new ();

	/* Add two unrelated certificates */
	gcr_certificate_chain_add (chain, test->cert_self);
	gcr_certificate_chain_add (chain, test->cert_signed);

	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 2);

	if (!gcr_certificate_chain_build (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                  NULL, 0, NULL, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_SELFSIGNED);
	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 1);

	g_object_unref (chain);
}

static void
fetch_async_result (GObject *source, GAsyncResult *result, gpointer user_data)
{
	*((GAsyncResult**)user_data) = result;
	g_object_ref (result);
	egg_test_wait_stop ();
}

static void
test_complete_async (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;
	GAsyncResult *result = NULL;

	chain = gcr_certificate_chain_new ();

	/* Add a whole bunch of certificates */
	gcr_certificate_chain_add (chain, test->cert_signed);
	gcr_certificate_chain_add (chain, test->cert_ca);
	gcr_certificate_chain_add (chain, test->cert_self);

	gcr_certificate_chain_build_async (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                   NULL, 0, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	if (!gcr_certificate_chain_build_finish (chain, result, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);
	g_object_unref (result);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_SELFSIGNED);
	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 2);

	g_object_unref (chain);
}

static void
test_with_anchor (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;

	chain = gcr_certificate_chain_new ();

	/* Two certificates in chain with ca trust anchor */
	gcr_certificate_chain_add (chain, test->cert_signed);
	gcr_certificate_chain_add (chain, test->cert_ca);
	add_anchor_to_module (test->cert_ca, GCR_PURPOSE_CLIENT_AUTH);

	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 2);

	if (!gcr_certificate_chain_build (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                  NULL, 0, NULL, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_ANCHORED);
	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 2);
	g_assert (gcr_certificate_chain_get_anchor (chain) == test->cert_ca);

	g_object_unref (chain);
}

static void
test_with_anchor_and_lookup_ca (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;

	chain = gcr_certificate_chain_new ();

	/* One signed certificate, with CA in pkcs11, and trust anchor */
	gcr_certificate_chain_add (chain, test->cert_signed);
	add_certificate_to_module (test->cert_ca);
	add_anchor_to_module (test->cert_ca, GCR_PURPOSE_CLIENT_AUTH);

	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 1);

	if (!gcr_certificate_chain_build (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                  NULL, 0, NULL, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_ANCHORED);
	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 2);
	g_assert (gcr_certificate_chain_get_anchor (chain) != NULL);

	g_object_unref (chain);
}

static void
test_with_pinned (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;

	chain = gcr_certificate_chain_new ();

	/* One certificate, and add CA to pkcs11 */
	gcr_certificate_chain_add (chain, test->cert_signed);
	gcr_certificate_chain_add (chain, test->cert_ca);
	add_pinned_to_module (test->cert_signed, GCR_PURPOSE_CLIENT_AUTH, "pinned.example.com");

	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 2);

	/* But we don't allow the lookup to happen */
	if (!gcr_certificate_chain_build (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                  "pinned.example.com", 0, NULL, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_PINNED);
	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 1);
	g_assert (gcr_certificate_chain_get_anchor (chain) == NULL);

	g_object_unref (chain);
}

static void
test_without_lookups (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;

	chain = gcr_certificate_chain_new ();

	/* One certificate, and add CA to pkcs11 */
	gcr_certificate_chain_add (chain, test->cert_signed);
	add_certificate_to_module (test->cert_ca);

	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 1);

	/* But we don't allow the lookup to happen */
	if (!gcr_certificate_chain_build (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                  NULL, GCR_CERTIFICATE_CHAIN_FLAG_NO_LOOKUPS,
	                                  NULL, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_INCOMPLETE);
	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 1);
	g_assert (gcr_certificate_chain_get_anchor (chain) == NULL);

	g_object_unref (chain);
}

static void
test_with_lookup_error (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;

	/* Make the lookup fail */
	test->funcs.C_GetAttributeValue = gck_mock_fail_C_GetAttributeValue;

	chain = gcr_certificate_chain_new ();

	/* Two certificates in chain with ca trust anchor */
	gcr_certificate_chain_add (chain, test->cert_signed);
	add_certificate_to_module (test->cert_ca);

	g_assert_cmpuint (gcr_certificate_chain_get_length (chain), ==, 1);

	if (gcr_certificate_chain_build (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                 NULL, 0, NULL, &error))
		g_assert_not_reached ();
	g_assert_error (error, GCK_ERROR, CKR_FUNCTION_FAILED);
	g_clear_error (&error);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_UNKNOWN);

	g_object_unref (chain);
}

static void
test_with_anchor_error (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;

	/* Make the lookup fail */
	test->funcs.C_GetAttributeValue = gck_mock_fail_C_GetAttributeValue;

	chain = gcr_certificate_chain_new ();

	/* Two certificates in chain with ca trust anchor */
	gcr_certificate_chain_add (chain, test->cert_signed);
	add_certificate_to_module (test->cert_ca);

	if (gcr_certificate_chain_build (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                 NULL, 0, NULL, &error))
		g_assert_not_reached ();
	g_assert_error (error, GCK_ERROR, CKR_FUNCTION_FAILED);
	g_clear_error (&error);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_UNKNOWN);

	g_object_unref (chain);
}

static void
test_with_anchor_error_async (Test *test, gconstpointer unused)
{
	GcrCertificateChain *chain;
	GError *error = NULL;
	GAsyncResult *result;

	/* Make the lookup fail */
	test->funcs.C_GetAttributeValue = gck_mock_fail_C_GetAttributeValue;

	chain = gcr_certificate_chain_new ();

	/* Two certificates in chain with ca trust anchor */
	gcr_certificate_chain_add (chain, test->cert_signed);
	add_certificate_to_module (test->cert_ca);

	gcr_certificate_chain_build_async (chain, GCR_PURPOSE_CLIENT_AUTH,
	                                   NULL, 0, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	if (gcr_certificate_chain_build_finish (chain, result, &error))
		g_assert_not_reached ();
	g_assert_error (error, GCK_ERROR, CKR_FUNCTION_FAILED);
	g_clear_error (&error);
	g_object_unref (result);

	g_assert_cmpuint (gcr_certificate_chain_get_status (chain), ==,
	                  GCR_CERTIFICATE_CHAIN_UNKNOWN);

	g_object_unref (chain);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);
	g_set_prgname ("test-certificate-chain");

	g_test_add ("/gcr/certificate-chain/new", Test, NULL, setup, test_new, teardown);
	g_test_add ("/gcr/certificate-chain/new_with_cert", Test, NULL, setup, test_new_with_cert, teardown);
	g_test_add ("/gcr/certificate-chain/selfsigned", Test, NULL, setup, test_selfsigned, teardown);
	g_test_add ("/gcr/certificate-chain/incomplete", Test, NULL, setup, test_incomplete, teardown);
	g_test_add ("/gcr/certificate-chain/empty", Test, NULL, setup, test_empty, teardown);
	g_test_add ("/gcr/certificate-chain/trim_extras", Test, NULL, setup, test_trim_extras, teardown);
	g_test_add ("/gcr/certificate-chain/complete_async", Test, NULL, setup, test_complete_async, teardown);
	g_test_add ("/gcr/certificate-chain/with_anchor", Test, NULL, setup, test_with_anchor, teardown);
	g_test_add ("/gcr/certificate-chain/with_anchor_and_lookup_ca", Test, NULL, setup, test_with_anchor_and_lookup_ca, teardown);
	g_test_add ("/gcr/certificate-chain/with_pinned", Test, NULL, setup, test_with_pinned, teardown);
	g_test_add ("/gcr/certificate-chain/without_lookups", Test, NULL, setup, test_without_lookups, teardown);
	g_test_add ("/gcr/certificate-chain/with_lookup_error", Test, NULL, setup, test_with_lookup_error, teardown);
	g_test_add ("/gcr/certificate-chain/with_anchor_error", Test, NULL, setup, test_with_anchor_error, teardown);
	g_test_add ("/gcr/certificate-chain/with_anchor_error_async", Test, NULL, setup, test_with_anchor_error_async, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
