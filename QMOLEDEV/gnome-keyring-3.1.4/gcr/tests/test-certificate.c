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

#include "egg/egg-testing.h"

#include <glib.h>

#include <errno.h>
#include <string.h>

typedef struct {
	GcrCertificate *certificate;
	GcrCertificate *dsa_cert;
	GcrCertificate *dhansak_cert;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	gchar *contents;
	gsize n_contents;

	if (!g_file_get_contents (SRCDIR "/files/der-certificate.crt", &contents, &n_contents, NULL))
		g_assert_not_reached ();
	test->certificate = gcr_simple_certificate_new (contents, n_contents);
	g_assert (test->certificate);
	g_free (contents);

	if (!g_file_get_contents (SRCDIR "/files/der-certificate-dsa.cer", &contents, &n_contents, NULL))
		g_assert_not_reached ();
	test->dsa_cert = gcr_simple_certificate_new (contents, n_contents);
	g_assert (test->dsa_cert);
	g_free (contents);

	if (!g_file_get_contents (SRCDIR "/files/dhansak-collabora.cer", &contents, &n_contents, NULL))
		g_assert_not_reached ();
	test->dhansak_cert = gcr_simple_certificate_new (contents, n_contents);
	g_assert (test->certificate);
	g_free (contents);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->certificate);
	g_object_unref (test->dsa_cert);
	g_object_unref (test->dhansak_cert);
}

static void
test_issuer_cn (Test *test, gconstpointer unused)
{
	gchar *cn = gcr_certificate_get_issuer_cn (test->certificate);
	g_assert (cn);
	g_assert_cmpstr (cn, ==, "http://www.valicert.com/");
	g_free (cn);
}

static void
test_issuer_dn (Test *test, gconstpointer unused)
{
	gchar *dn = gcr_certificate_get_issuer_dn (test->certificate);
	g_assert (dn);
	g_assert_cmpstr (dn, ==, "L=ValiCert Validation Network, O=ValiCert, Inc., OU=ValiCert Class 3 Policy Validation Authority, CN=http://www.valicert.com/, EMAIL=info@valicert.com");
	g_free (dn);
}

static void
test_issuer_part (Test *test, gconstpointer unused)
{
	gchar *part = gcr_certificate_get_issuer_part (test->certificate, "l");
	g_assert (part);
	g_assert_cmpstr (part, ==, "ValiCert Validation Network");
	g_free (part);
}

static void
test_issuer_raw (Test *test, gconstpointer unused)
{
	gpointer der;
	gsize n_der;

	der = gcr_certificate_get_issuer_raw (test->certificate, &n_der);
	g_assert (der);
	egg_assert_cmpsize (n_der, ==, 190);
	g_free (der);
}

static void
test_subject_cn (Test *test, gconstpointer unused)
{
	gchar *cn = gcr_certificate_get_subject_cn (test->certificate);
	g_assert (cn);
	g_assert_cmpstr (cn, ==, "http://www.valicert.com/");
	g_free (cn);

	cn = gcr_certificate_get_subject_cn (test->dhansak_cert);
	g_assert (cn);
	g_assert_cmpstr (cn, ==, "dhansak.collabora.co.uk");
	g_free (cn);
}

static void
test_subject_dn (Test *test, gconstpointer unused)
{
	gchar *dn = gcr_certificate_get_subject_dn (test->certificate);
	g_assert (dn);
	g_assert_cmpstr (dn, ==, "L=ValiCert Validation Network, O=ValiCert, Inc., OU=ValiCert Class 3 Policy Validation Authority, CN=http://www.valicert.com/, EMAIL=info@valicert.com");
	g_free (dn);

	dn = gcr_certificate_get_subject_dn (test->dhansak_cert);
	g_assert (dn);
	g_assert_cmpstr (dn, ==, "CN=dhansak.collabora.co.uk, EMAIL=sysadmin@collabora.co.uk");
	g_free (dn);

}

static void
test_subject_part (Test *test, gconstpointer unused)
{
	gchar *part = gcr_certificate_get_subject_part (test->certificate, "OU");
	g_assert (part);
	g_assert_cmpstr (part, ==, "ValiCert Class 3 Policy Validation Authority");
	g_free (part);

	part = gcr_certificate_get_subject_part (test->dhansak_cert, "EMAIL");
	g_assert (part);
	g_assert_cmpstr (part, ==, "sysadmin@collabora.co.uk");
	g_free (part);

}

static void
test_subject_raw (Test *test, gconstpointer unused)
{
	gpointer der;
	gsize n_der;

	der = gcr_certificate_get_subject_raw (test->certificate, &n_der);
	g_assert (der);
	egg_assert_cmpsize (n_der, ==, 190);
	g_free (der);

	der = gcr_certificate_get_subject_raw (test->dhansak_cert, &n_der);
	g_assert (der);
	egg_assert_cmpsize (n_der, ==, 77);
	g_free (der);
}

static void
test_issued_date (Test *test, gconstpointer unused)
{
	GDate *date = gcr_certificate_get_issued_date (test->certificate);
	g_assert (date);
	g_assert_cmpuint (g_date_get_year (date), ==, 1999);
	g_assert_cmpuint (g_date_get_month (date), ==, 6);
	g_assert_cmpuint (g_date_get_day (date), ==, 26);
	g_date_free (date);
}

static void
test_expiry_date (Test *test, gconstpointer unused)
{
	GDate *date = gcr_certificate_get_expiry_date (test->certificate);
	g_assert (date);
	g_assert_cmpuint (g_date_get_year (date), ==, 2019);
	g_assert_cmpuint (g_date_get_month (date), ==, 6);
	g_assert_cmpuint (g_date_get_day (date), ==, 26);
	g_date_free (date);
}

static void
test_serial_number (Test *test, gconstpointer unused)
{
	gsize n_serial;
	guchar *serial;
	gchar *hex;

	serial = gcr_certificate_get_serial_number (test->certificate, &n_serial);
	g_assert (serial);
	g_assert_cmpuint (n_serial, ==, 1);
	g_assert (memcmp (serial, "\1", n_serial) == 0);
	g_free (serial);

	hex = gcr_certificate_get_serial_number_hex (test->certificate);
	g_assert (hex);
	g_assert_cmpstr (hex, ==, "01");
	g_free (hex);
}

static void
test_fingerprint (Test *test, gconstpointer unused)
{
	gsize n_print;
	guchar *print = gcr_certificate_get_fingerprint (test->certificate, G_CHECKSUM_MD5, &n_print);
	g_assert (print);
	g_assert_cmpuint (n_print, ==, g_checksum_type_get_length (G_CHECKSUM_MD5));
	g_assert (memcmp (print, "\xa2\x6f\x53\xb7\xee\x40\xdb\x4a\x68\xe7\xfa\x18\xd9\x10\x4b\x72", n_print) == 0);
	g_free (print);
}

static void
test_fingerprint_hex (Test *test, gconstpointer unused)
{
	gchar *print = gcr_certificate_get_fingerprint_hex (test->certificate, G_CHECKSUM_MD5);
	g_assert (print);
	g_assert_cmpstr (print, ==, "A2 6F 53 B7 EE 40 DB 4A 68 E7 FA 18 D9 10 4B 72");
	g_free (print);
}

static void
test_certificate_key_size (Test *test, gconstpointer unused)
{
	guint key_size = gcr_certificate_get_key_size (test->certificate);
	g_assert_cmpuint (key_size, ==, 1024);

	key_size = gcr_certificate_get_key_size (test->dsa_cert);
	g_assert_cmpuint (key_size, ==, 1024);
}

static void
test_certificate_is_issuer (Test *test, gconstpointer unused)
{
	gboolean ret = gcr_certificate_is_issuer (test->certificate, test->certificate);
	g_assert (ret == TRUE);

	ret = gcr_certificate_is_issuer (test->certificate, test->dsa_cert);
	g_assert (ret == FALSE);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);
	g_set_prgname ("test-certificate");

	g_test_add ("/gcr/certificate/issuer_cn", Test, NULL, setup, test_issuer_cn, teardown);
	g_test_add ("/gcr/certificate/issuer_dn", Test, NULL, setup, test_issuer_dn, teardown);
	g_test_add ("/gcr/certificate/issuer_part", Test, NULL, setup, test_issuer_part, teardown);
	g_test_add ("/gcr/certificate/issuer_raw", Test, NULL, setup, test_issuer_raw, teardown);
	g_test_add ("/gcr/certificate/subject_cn", Test, NULL, setup, test_subject_cn, teardown);
	g_test_add ("/gcr/certificate/subject_dn", Test, NULL, setup, test_subject_dn, teardown);
	g_test_add ("/gcr/certificate/subject_part", Test, NULL, setup, test_subject_part, teardown);
	g_test_add ("/gcr/certificate/subject_raw", Test, NULL, setup, test_subject_raw, teardown);
	g_test_add ("/gcr/certificate/issued_date", Test, NULL, setup, test_issued_date, teardown);
	g_test_add ("/gcr/certificate/expiry_date", Test, NULL, setup, test_expiry_date, teardown);
	g_test_add ("/gcr/certificate/serial_number", Test, NULL, setup, test_serial_number, teardown);
	g_test_add ("/gcr/certificate/fingerprint", Test, NULL, setup, test_fingerprint, teardown);
	g_test_add ("/gcr/certificate/fingerprint_hex", Test, NULL, setup, test_fingerprint_hex, teardown);
	g_test_add ("/gcr/certificate/certificate_key_size", Test, NULL, setup, test_certificate_key_size, teardown);
	g_test_add ("/gcr/certificate/certificate_is_issuer", Test, NULL, setup, test_certificate_is_issuer, teardown);

	return g_test_run ();
}
