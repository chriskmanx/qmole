/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-pkix-parser.c: Test PKIX parser

   Copyright (C) 2007 Stefan Walter

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

#include "test-suite.h"

#include "egg/egg-asn1-defs.h"
#include "egg/egg-asn1x.h"
#include "egg/egg-dn.h"
#include "egg/egg-oid.h"

#include <glib.h>
#include <gcrypt.h>
#include <libtasn1.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static GNode* asn1_cert = NULL;
static guchar *data_cert = NULL;
static gsize n_data_cert = 0;

DEFINE_SETUP(dn_cert)
{
	data_cert = testing_data_read ("test-certificate-1.der", &n_data_cert);

	asn1_cert = egg_asn1x_create (pkix_asn1_tab, "Certificate");
	g_assert (asn1_cert != NULL);

	if (!egg_asn1x_decode (asn1_cert, data_cert, n_data_cert))
		g_assert_not_reached ();
}

DEFINE_TEARDOWN(dn_cert)
{
	egg_asn1x_destroy (asn1_cert);
	g_free (data_cert);
	data_cert = NULL;
}

DEFINE_TEST(read_dn)
{
	gchar *dn;

	dn = egg_dn_read (egg_asn1x_node (asn1_cert, "tbsCertificate", "issuer", "rdnSequence", NULL));
	g_assert (dn != NULL);
	g_assert_cmpstr (dn, ==, "C=ZA, ST=Western Cape, L=Cape Town, O=Thawte Consulting, OU=Certification Services Division, CN=Thawte Personal Premium CA, EMAIL=personal-premium@thawte.com");

	g_free (dn);
}

DEFINE_TEST(dn_value)
{
	const guchar value[] = { 0x13, 0x1a, 0x54, 0x68, 0x61, 0x77, 0x74, 0x65, 0x20, 0x50, 0x65, 0x72, 0x73, 0x6f, 0x6e, 0x61, 0x6c, 0x20, 0x50, 0x72, 0x65, 0x6d, 0x69, 0x75, 0x6d, 0x20, 0x43, 0x41 };
	gsize n_value = 28;
	GQuark oid;
	gchar *text;

	/* Some printable strings */
	oid = g_quark_from_static_string ("2.5.4.3");
	text = egg_dn_print_value (oid, value, n_value);
	g_assert_cmpstr (text, ==, "Thawte Personal Premium CA");
	g_free (text);

	/* Unknown oid */
	oid = g_quark_from_static_string ("1.1.1.1.1.1");
	text = egg_dn_print_value (oid, value, n_value);
	g_assert_cmpstr (text, ==, "#131A54686177746520506572736F6E616C205072656D69756D204341");
	g_free (text);
}

static int last_index = 0;

static void
concatenate_dn (guint index, GQuark oid, const guchar *value, gsize n_value, gpointer user_data)
{
	GString *dn = user_data;
	gchar *text;

	g_assert (oid);
	g_assert (value);
	g_assert (n_value);

	g_assert (index == last_index);
	++last_index;

	if (index != 1) {
		g_string_append (dn, ", ");
	}

	g_string_append (dn, egg_oid_get_name (oid));
	g_string_append_c (dn, '=');

	text = egg_dn_print_value (oid, value, n_value);
	g_string_append (dn, text);
	g_free (text);
}

DEFINE_TEST(parse_dn)
{
	GString *dn = g_string_new ("");
	last_index = 1;

	if (!egg_dn_parse (egg_asn1x_node (asn1_cert, "tbsCertificate", "issuer", "rdnSequence", NULL), concatenate_dn, dn))
		g_assert_not_reached ();

	g_assert_cmpstr (dn->str, ==, "C=ZA, ST=Western Cape, L=Cape Town, O=Thawte Consulting, OU=Certification Services Division, CN=Thawte Personal Premium CA, EMAIL=personal-premium@thawte.com");
	g_string_free (dn, TRUE);
}

DEFINE_TEST(read_dn_part)
{
	GNode *node;
	gchar *value;

	node = egg_asn1x_node (asn1_cert, "tbsCertificate", "issuer", "rdnSequence", NULL);

	value = egg_dn_read_part (node, "CN");
	g_assert (value != NULL);
	g_assert_cmpstr (value, ==, "Thawte Personal Premium CA");
	g_free (value);

	value = egg_dn_read_part (node, "2.5.4.8");
	g_assert (value != NULL);
	g_assert_cmpstr (value, ==, "Western Cape");
	g_free (value);

	value = egg_dn_read_part (node, "DC");
	g_assert (value == NULL);

	value = egg_dn_read_part (node, "0.0.0.0");
	g_assert (value == NULL);

	value = egg_dn_read_part (node, "2.5.4.9");
	g_assert (value == NULL);
}
