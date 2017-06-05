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
#include "egg/egg-oid.h"

#include <glib.h>
#include <gcrypt.h>
#include <libtasn1.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

extern const ASN1_ARRAY_TYPE test_asn1_tab[];

static GNode *asn1_cert = NULL;
static guchar *data_cert = NULL;
static gsize n_data_cert = 0;

DEFINE_SETUP(asn1_tree)
{
	data_cert = testing_data_read ("test-certificate-1.der", &n_data_cert);

	asn1_cert = egg_asn1x_create_and_decode (pkix_asn1_tab, "Certificate", data_cert, n_data_cert);
	g_assert (asn1_cert != NULL);
}

DEFINE_TEARDOWN(asn1_tree)
{
	egg_asn1x_destroy (asn1_cert);
	g_free (data_cert);
	data_cert = NULL;
}

DEFINE_TEST(node_name)
{
	g_assert_cmpstr (egg_asn1x_name (asn1_cert), ==, "Certificate");
}

DEFINE_TEST(asn1_integers)
{
	GNode *asn;
	guchar *data;
	gsize n_data;
	gboolean ret;
	gulong val;

	asn = egg_asn1x_create (test_asn1_tab, "TestIntegers");
	g_assert ("asn test structure is null" && asn != NULL);

	ret = egg_asn1x_set_integer_as_ulong (egg_asn1x_node (asn, "uint1", NULL), 35);
	g_assert ("couldn't write integer" && ret);

	ret = egg_asn1x_set_integer_as_ulong (egg_asn1x_node (asn, "uint2", NULL), 23456);
	g_assert ("couldn't write integer" && ret);

	ret = egg_asn1x_set_integer_as_ulong (egg_asn1x_node (asn, "uint3", NULL), 209384022);
	g_assert ("couldn't write integer" && ret);
	
	/* Now encode the whole caboodle */
	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert ("encoding asn1 didn't work" && data != NULL);

	egg_asn1x_destroy (asn);

	/* Now decode it all nicely */
	asn = egg_asn1x_create_and_decode (test_asn1_tab, "TestIntegers", data, n_data);
	g_return_if_fail (asn != NULL);

	/* And get out the values */
	ret = egg_asn1x_get_integer_as_ulong (egg_asn1x_node (asn, "uint1", NULL), &val);
	g_assert ("couldn't read integer from asn1" && ret);
	g_assert_cmpuint (val, ==, 35);

	ret = egg_asn1x_get_integer_as_ulong (egg_asn1x_node (asn, "uint2", NULL), &val);
	g_assert ("couldn't read integer from asn1" && ret);
	g_assert_cmpuint (val, ==, 23456);

	ret = egg_asn1x_get_integer_as_ulong (egg_asn1x_node (asn, "uint3", NULL), &val);
	g_assert ("couldn't read integer from asn1" && ret);
	g_assert_cmpuint (val, ==, 209384022);

	g_free (data);
}

DEFINE_TEST(boolean)
{
	GNode *asn = NULL;
	gboolean value, ret;
	gpointer data;
	gsize n_data;

	asn = egg_asn1x_create (test_asn1_tab, "TestBooleanSeq");
	g_assert ("asn test structure is null" && asn != NULL);

	/* Get the default value */
	value = TRUE;
	ret = egg_asn1x_get_boolean (egg_asn1x_node (asn, "boolean", NULL), &value);
	g_assert (ret == TRUE);
	g_assert (value == FALSE);

	ret = egg_asn1x_set_boolean (egg_asn1x_node (asn, "boolean", NULL), TRUE);
	g_assert (ret == TRUE);

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	ret = egg_asn1x_get_boolean (egg_asn1x_node (asn, "boolean", NULL), &value);
	g_assert (ret);
	g_assert (value == TRUE);

	ret = egg_asn1x_set_boolean (egg_asn1x_node (asn, "boolean", NULL), FALSE);
	g_assert (ret == TRUE);

	g_free (data);
	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	ret = egg_asn1x_get_boolean (egg_asn1x_node (asn, "boolean", NULL), &value);
	g_assert (ret);
	g_assert (value == FALSE);

	g_free (data);
	egg_asn1x_destroy (asn);
}

DEFINE_TEST(write_value)
{
	GNode *asn = NULL;
	guchar *data;
	gsize n_data;
	guchar *encoded;
	gsize n_encoded;

	asn = egg_asn1x_create (test_asn1_tab, "TestData");
	g_assert ("asn test structure is null" && asn != NULL);

	if (!egg_asn1x_set_string_as_raw (egg_asn1x_node (asn, "data", NULL), (guchar*)"SOME DATA", 9, NULL))
		g_assert_not_reached ();

	encoded = egg_asn1x_encode (asn, NULL, &n_encoded);
	g_assert (encoded);

	data = egg_asn1x_get_string_as_raw (egg_asn1x_node (asn, "data", NULL), NULL, &n_data);
	g_assert (data != NULL);
	g_assert_cmpuint (n_data, ==, 9);
	g_assert (memcmp (data, "SOME DATA", 9) == 0);
	g_free (data);

	g_free (encoded);
	egg_asn1x_destroy (asn);
}

DEFINE_TEST(element_length_content)
{
	GNode *asn = NULL;
	gchar *buffer;
	const guchar *content;
	gsize n_content;
	gsize n_buffer;
	gssize length;

	asn = egg_asn1x_create (test_asn1_tab, "TestData");
	g_assert ("asn test structure is null" && asn != NULL);

	if (!egg_asn1x_set_string_as_raw (egg_asn1x_node (asn, "data", NULL), (guchar*)"SOME DATA", 9, NULL))
		g_assert_not_reached ();

	buffer = egg_asn1x_encode (asn, NULL, &n_buffer);
	g_assert (buffer != NULL);

	/* Now the real test */
	length = egg_asn1x_element_length (buffer, n_buffer + 1024);
	g_assert_cmpint (length, ==, 13);

	content = egg_asn1x_element_content (buffer, length, &n_content);
	g_assert (content);
	g_assert_cmpuint (n_content, ==, 11);
	
	content = egg_asn1x_element_content (content, n_content, &n_content);
	g_assert (content);
	g_assert_cmpuint (n_content, ==, 9);	
	g_assert (memcmp (content, "SOME DATA", 9) == 0);

	egg_asn1x_destroy (asn);
	g_free (buffer);
}

DEFINE_TEST(read_element)
{
	GNode *asn = NULL;
	guchar *buffer;
	gconstpointer data;
	gsize n_data;
	gsize n_buffer;

	asn = egg_asn1x_create (test_asn1_tab, "TestData");
	g_assert ("asn test structure is null" && asn != NULL);

	if (!egg_asn1x_set_string_as_raw (egg_asn1x_node (asn, "data", NULL), (guchar*)"SOME DATA", 9, NULL))
		g_assert_not_reached ();

	buffer = egg_asn1x_encode (asn, NULL, &n_buffer);
	g_assert (buffer != NULL);

	/* Now the real test */
	data = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "data", NULL), &n_data);
	g_assert (data != NULL);
	g_assert_cmpint (n_data, ==, 11);

	data = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "data", NULL), &n_data);
	g_assert (data);
	g_assert_cmpuint (n_data, ==, 9);
	g_assert (memcmp (data, "SOME DATA", 9) == 0);

	egg_asn1x_destroy (asn);
	g_free (buffer);
}

DEFINE_TEST(oid)
{
	GNode *asn = NULL;
	GQuark oid, check;
	guchar *buffer;
	gsize n_buffer;

	asn = egg_asn1x_create (test_asn1_tab, "TestOid");
	g_assert ("asn test structure is null" && asn != NULL);

	if (!egg_asn1x_set_oid_as_string (egg_asn1x_node (asn, "oid", NULL), "1.2.34567.89"))
		g_assert_not_reached ();

	buffer = egg_asn1x_encode (asn, NULL, &n_buffer);
	g_assert (buffer != NULL);

	/* Now a quark has been defined */
	check = g_quark_from_static_string ("1.2.34567.89");
	oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "oid", NULL));
	g_assert (oid);
	g_assert (check == oid);
	g_assert_cmpstr (g_quark_to_string (oid), ==, "1.2.34567.89");

	/* Write a different OID */ 
	if (!egg_asn1x_set_oid_as_quark (egg_asn1x_node (asn, "oid", NULL), g_quark_from_static_string ("5.4.3.2.1678")))
		g_assert_not_reached ();

	g_free (buffer);
	buffer = egg_asn1x_encode (asn, NULL, &n_buffer);
	g_assert (buffer != NULL);

	oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "oid", NULL));
	g_assert (oid);
	g_assert_cmpstr (g_quark_to_string (oid), ==, "5.4.3.2.1678");

	g_free (buffer);
	egg_asn1x_destroy (asn);
}

typedef struct _TimeTestData {
	gchar *value;
	time_t ref;
} TimeTestData;

static const TimeTestData generalized_time_test_data[] = {
	{ "20070725130528Z", 1185368728 },
	{ "20070725130528.2134Z", 1185368728 },
	{ "20070725140528-0100", 1185368728 },
	{ "20070725040528+0900", 1185368728 },
	{ "20070725013528+1130", 1185368728 },
	{ "20070725Z", 1185321600 },
	{ "20070725+0000", 1185321600 },
	{ NULL, 0 }
};

static const TimeTestData utc_time_test_data[] = {
	/* Test the Y2K style wrap arounds */
	{ "070725130528Z", 1185368728 },  /* The year 2007 */
	{ "020725130528Z", 1027602328 },  /* The year 2002 */
	{ "970725130528Z", 869835928 },	  /* The year 1997 */
	{ "370725130528Z", 2132139928 },  /* The year 2037 */
	
	/* Test the time zones and other formats */
	{ "070725130528.2134Z", 1185368728 },
	{ "070725140528-0100", 1185368728 },
	{ "070725040528+0900", 1185368728 },
	{ "070725013528+1130", 1185368728 },
	{ "070725Z", 1185321600 },
	{ "070725+0000", 1185321600 },
	
	{ NULL, 0 }
};

DEFINE_TEST(general_time)
{
	time_t when;
	const TimeTestData *data;
	
	for (data = generalized_time_test_data; data->value; ++data) {
		when = egg_asn1x_parse_time_general (data->value, -1);
		if (data->ref != when) {
			printf ("%s", data->value);
			printf ("%s != ", ctime (&when));
			printf ("%s\n", ctime (&data->ref));
			fflush (stdout);
		}
			
		g_assert ("decoded time doesn't match reference" && data->ref == when);
	}
}

DEFINE_TEST(utc_time)
{
	time_t when;
	const TimeTestData *data;
	
	for (data = utc_time_test_data; data->value; ++data) {
		when = egg_asn1x_parse_time_utc (data->value, -1);
		if (data->ref != when) {
			printf ("%s", data->value);
			printf ("%s != ", ctime (&when));
			printf ("%s\n", ctime (&data->ref));
			fflush (stdout);
		}
			
		g_assert ("decoded time doesn't match reference" && data->ref == when);
	}
}

DEFINE_TEST(read_time)
{
	glong time;

	time = egg_asn1x_get_time_as_long (egg_asn1x_node (asn1_cert, "tbsCertificate", "validity", "notBefore", NULL));
	g_assert_cmpint (time, ==, 820454400);
}

DEFINE_TEST(read_date)
{
	GDate date;
	if (!egg_asn1x_get_time_as_date (egg_asn1x_node (asn1_cert, "tbsCertificate", "validity", "notAfter", NULL), &date))
		g_assert_not_reached ();
	g_assert_cmpint (date.day, ==, 31);
	g_assert_cmpint (date.month, ==, 12);
	g_assert_cmpint (date.year, ==, 2020);
}

DEFINE_TEST(create_by_oid)
{
	/* id-at-initials = X520initials */
	GNode *node = egg_asn1x_create (pkix_asn1_tab, "2.5.4.43");
	g_assert (node != NULL);
	g_assert_cmpstr (egg_asn1x_name (node), ==, "X520initials");
	egg_asn1x_destroy (node);
}

DEFINE_TEST(create_by_oid_invalid)
{
	GNode *node = egg_asn1x_create (pkix_asn1_tab, "23.23.23.23");
	g_assert (node == NULL);
}

DEFINE_TEST(create_by_bad_order)
{
	/*
	 * In pkix.asn the definition for parts of this oid
	 * come in the wrong order. However this should still work.
	 */

	/* id-pe-authorityInfoAccess = AuthorityInfoAccessSyntax */
	GNode *node = egg_asn1x_create (pkix_asn1_tab, "1.3.6.1.5.5.7.1.1");
	g_assert (node != NULL);
	g_assert_cmpstr (egg_asn1x_name (node), ==, "AuthorityInfoAccessSyntax");
	egg_asn1x_destroy (node);
}

DEFINE_TEST(count)
{
	GNode *node;

	node = egg_asn1x_node (asn1_cert, "tbsCertificate", "issuer", "rdnSequence", NULL);
	g_assert (node);
	g_assert_cmpuint (egg_asn1x_count (node), ==, 7);
}
