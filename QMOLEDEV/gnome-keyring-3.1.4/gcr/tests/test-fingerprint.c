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
#define GCR_COMPILATION 1

#include "gcr/gcr.h"
#include "gcr/gcr-internal.h"
#include "gcr/gcr-fingerprint.h"

#include "gck/gck-test.h"

#include "pkcs11/pkcs11n.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"
#include "egg/egg-testing.h"

#include <glib.h>

#include <errno.h>

typedef struct {
	gpointer cert_rsa;
	gsize n_cert_rsa;
	gpointer key_rsa;
	gsize n_key_rsa;
	gpointer cert_dsa;
	gsize n_cert_dsa;
	gpointer key_dsa;
	gsize n_key_dsa;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	if (!g_file_get_contents ("files/client.crt", (gchar**)&test->cert_rsa,
	                          &test->n_cert_rsa, NULL))
		g_assert_not_reached ();
	g_assert (test->cert_rsa);

	if (!g_file_get_contents ("files/client.key", (gchar**)&test->key_rsa,
	                          &test->n_key_rsa, NULL))
		g_assert_not_reached ();
	g_assert (test->key_rsa);

	if (!g_file_get_contents ("files/generic-dsa.crt", (gchar**)&test->cert_dsa,
	                          &test->n_cert_dsa, NULL))
		g_assert_not_reached ();
	g_assert (test->cert_dsa);

	if (!g_file_get_contents ("files/generic-dsa.key", (gchar**)&test->key_dsa,
	                          &test->n_key_dsa, NULL))
		g_assert_not_reached ();
	g_assert (test->key_dsa);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_free (test->cert_rsa);
	g_free (test->key_rsa);
	g_free (test->cert_dsa);
	g_free (test->key_dsa);
}

static GckAttributes*
parse_attributes_for_key (gpointer data, gsize n_data)
{
	GcrParser *parser;
	GckAttributes *attrs;
	GError *error = NULL;

	parser = gcr_parser_new ();
	gcr_parser_parse_data (parser, data, n_data, &error);
	g_assert_no_error (error);

	attrs = gcr_parser_get_parsed_attributes (parser);
	g_assert (attrs);
	gck_attributes_ref (attrs);

	g_object_unref (parser);
	return attrs;
}

static gconstpointer
parse_subject_public_key_info_for_cert (gpointer data, gsize n_data, gsize *n_info)
{
	gconstpointer info;
	GNode *asn;

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "Certificate", data, n_data);
	g_assert (asn);

	info = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "tbsCertificate", "subjectPublicKeyInfo", NULL), n_info);
	g_assert (info);

	egg_asn1x_destroy (asn);
	return info;
}

static void
test_rsa (Test *test, gconstpointer unused)
{
	GckAttributes *key;
	gconstpointer info;
	gsize n_info;
	gpointer fingerprint1, fingerprint2;
	gsize n_fingerprint1, n_fingerprint2;

	key = parse_attributes_for_key (test->key_rsa, test->n_key_rsa);
	info = parse_subject_public_key_info_for_cert (test->cert_rsa, test->n_cert_rsa, &n_info);

	fingerprint1 = _gcr_fingerprint_from_subject_public_key_info (info, n_info, G_CHECKSUM_SHA1, &n_fingerprint1);
	fingerprint2 = _gcr_fingerprint_from_attributes (key, G_CHECKSUM_SHA1, &n_fingerprint2);

	egg_assert_cmpmem (fingerprint1, n_fingerprint1, ==, fingerprint2, n_fingerprint2);

	g_free (fingerprint1);
	g_free (fingerprint2);
	gck_attributes_unref (key);
}

static void
test_dsa (Test *test, gconstpointer unused)
{
	GckAttributes *key;
	gconstpointer info;
	gsize n_info;
	gpointer fingerprint1, fingerprint2;
	gsize n_fingerprint1, n_fingerprint2;

	key = parse_attributes_for_key (test->key_dsa, test->n_key_dsa);
	info = parse_subject_public_key_info_for_cert (test->cert_dsa, test->n_cert_dsa, &n_info);

	fingerprint1 = _gcr_fingerprint_from_subject_public_key_info (info, n_info, G_CHECKSUM_SHA1, &n_fingerprint1);
	fingerprint2 = _gcr_fingerprint_from_attributes (key, G_CHECKSUM_SHA1, &n_fingerprint2);

	egg_assert_cmpmem (fingerprint1, n_fingerprint1, ==, fingerprint2, n_fingerprint2);

	g_free (fingerprint1);
	g_free (fingerprint2);
	gck_attributes_unref (key);
}

int
main (int argc, char **argv)
{
	const gchar *srcdir;

	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	srcdir = g_getenv ("SRCDIR");
	if (srcdir && chdir (srcdir) < 0)
		g_error ("couldn't change directory to: %s: %s", srcdir, g_strerror (errno));

	g_test_add ("/gcr/fingerprint/rsa", Test, NULL, setup, test_rsa, teardown);
	g_test_add ("/gcr/fingerprint/dsa", Test, NULL, setup, test_dsa, teardown);

	return g_test_run ();
}
