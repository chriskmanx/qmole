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

#include "gck/gck-test.h"

#include "pkcs11/pkcs11n.h"

#include "egg/egg-testing.h"

#include <glib.h>

#include <errno.h>

typedef struct {
	gpointer cert_data;
	gsize n_cert_data;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	if (!g_file_get_contents (SRCDIR "/files/der-certificate.crt", (gchar**)&test->cert_data,
	                          &test->n_cert_data, NULL))
		g_assert_not_reached ();
	g_assert (test->cert_data);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_free (test->cert_data);
}

static void
test_new (Test *test, gconstpointer unused)
{
	GcrCertificate *cert;
	gconstpointer der;
	gsize n_der;

	cert = gcr_simple_certificate_new (test->cert_data, test->n_cert_data);
	g_assert (GCR_IS_SIMPLE_CERTIFICATE (cert));

	der = gcr_certificate_get_der_data (cert, &n_der);
	g_assert (der);
	egg_assert_cmpmem (der, n_der, ==, test->cert_data, test->n_cert_data);

	g_object_unref (cert);
}

static void
test_new_static (Test *test, gconstpointer unused)
{
	GcrCertificate *cert;
	gconstpointer der;
	gsize n_der;

	cert = gcr_simple_certificate_new_static (test->cert_data, test->n_cert_data);
	g_assert (GCR_IS_SIMPLE_CERTIFICATE (cert));

	der = gcr_certificate_get_der_data (cert, &n_der);
	g_assert (der);
	egg_assert_cmpsize (n_der, ==, test->n_cert_data);
	g_assert (der == test->cert_data); /* Must be same pointer */

	g_object_unref (cert);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);
	g_set_prgname ("test-simple-certificate");

	g_test_add ("/gcr/simple-certificate/new", Test, NULL, setup, test_new, teardown);
	g_test_add ("/gcr/simple-certificate/new_static", Test, NULL, setup, test_new_static, teardown);

	return g_test_run ();
}
