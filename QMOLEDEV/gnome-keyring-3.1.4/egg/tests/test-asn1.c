/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-asn1.c: Test ASN1 stuf

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

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"
#include "egg/egg-testing.h"

#include <glib.h>
#include <libtasn1.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

extern const ASN1_ARRAY_TYPE test_asn1_tab[];

const gchar I33[] =           "\x02\x01\x2A";
const gchar BFALSE[] =        "\x01\x01\x00";
const gchar BTRUE[] =         "\x01\x01\xFF";
const gchar SFARNSWORTH[] =   "\x04\x0A""farnsworth";
const gchar SIMPLICIT[] =     "\x85\x08""implicit";
const gchar SEXPLICIT[] =     "\xE5\x0A\x04\x08""explicit";
const gchar TGENERALIZED[] =  "\x18\x0F""20070725130528Z";
const gchar BITS_TEST[] =  "\x03\x04\x06\x6e\x5d\xc0";
const gchar BITS_BAD[] =  "\x03\x04\x06\x6e\x5d\xc1";
const gchar BITS_ZERO[] =  "\x03\x01\x00";
const gchar NULL_TEST[] =  "\x05\x00";

/* ENUM with value = 2 */
const gchar ENUM_TWO[] =           "\x0A\x01\x02";

/* ENUM with value = 3 */
const gchar ENUM_THREE[] =           "\x0A\x01\x03";

#define XL(x) G_N_ELEMENTS (x) - 1

static void
test_boolean (void)
{
	GNode *asn;
	gboolean value;

	asn = egg_asn1x_create (test_asn1_tab, "TestBoolean");
	g_assert (asn);

	/* Shouldn't succeed */
	if (egg_asn1x_get_boolean (asn, &value))
		g_assert_not_reached ();

	/* Decode a false */
	if (!egg_asn1x_decode (asn, BFALSE, XL (BFALSE)))
		g_assert_not_reached ();
	value = TRUE;
	if (!egg_asn1x_get_boolean (asn, &value))
		g_assert_not_reached ();
	g_assert (value == FALSE);

	/* Decode a true */
	if (!egg_asn1x_decode (asn, BTRUE, XL (BTRUE)))
		g_assert_not_reached ();
	value = FALSE;
	if (!egg_asn1x_get_boolean (asn, &value))
		g_assert_not_reached ();
	g_assert (value == TRUE);

	egg_asn1x_clear (asn);

	/* Shouldn't suceed after clear */
	if (egg_asn1x_get_boolean (asn, &value))
		g_assert_not_reached ();

	egg_asn1x_destroy (asn);
}

static void
test_null (void)
{
	GNode *asn;
	gpointer data;
	gsize n_data;

	asn = egg_asn1x_create (test_asn1_tab, "TestNull");
	g_assert (asn);

	if (!egg_asn1x_set_null (asn))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, g_realloc, &n_data);
	egg_assert_cmpmem (NULL_TEST, XL (NULL_TEST), ==, data, n_data);

	if (!egg_asn1x_decode (asn, data, n_data))
		g_assert_not_reached ();

	egg_asn1x_destroy (asn);
	g_free (data);
}

static void
test_integer (void)
{
	GNode *asn;
	gulong value;

	asn = egg_asn1x_create (test_asn1_tab, "TestInteger");
	g_assert (asn);

	/* Shouldn't succeed */
	if (egg_asn1x_get_integer_as_ulong (asn, &value))
		g_assert_not_reached ();

	/* Should suceed now */
	if (!egg_asn1x_decode (asn, I33, XL (I33)))
		g_assert_not_reached ();
	if (!egg_asn1x_get_integer_as_ulong (asn, &value))
		g_assert_not_reached ();
	g_assert (value == 42);

	egg_asn1x_clear (asn);

	/* Shouldn't suceed after clear */
	if (egg_asn1x_get_integer_as_ulong (asn, &value))
		g_assert_not_reached ();

	egg_asn1x_destroy (asn);
}

static void
test_octet_string (void)
{
	GNode *asn;
	gchar *value;

	asn = egg_asn1x_create (test_asn1_tab, "TestOctetString");
	g_assert (asn);

	/* Shouldn't succeed */
	if (egg_asn1x_get_string_as_utf8 (asn, NULL))
		g_assert_not_reached ();

	/* Should work */
	if (!egg_asn1x_decode (asn, SFARNSWORTH, XL (SFARNSWORTH)))
		g_assert_not_reached ();
	value = egg_asn1x_get_string_as_utf8 (asn, NULL);
	g_assert_cmpstr (value, ==, "farnsworth");
	g_free (value);

	egg_asn1x_clear (asn);

	/* Shouldn't succeed */
	if (egg_asn1x_get_string_as_utf8 (asn, NULL))
		g_assert_not_reached ();

	egg_asn1x_destroy (asn);
}

static void
test_generalized_time (void)
{
	GNode *asn;
	glong value;

	asn = egg_asn1x_create (test_asn1_tab, "TestGeneralized");
	g_assert (asn);

	/* Shouldn't succeed */
	value = egg_asn1x_get_time_as_long (asn);
	g_assert (value == -1);

	/* Should work */
	if (!egg_asn1x_decode (asn, TGENERALIZED, XL (TGENERALIZED)))
		g_assert_not_reached ();
	value = egg_asn1x_get_time_as_long (asn);
	g_assert (value == 1185368728);

	egg_asn1x_clear (asn);

	/* Shouldn't succeed */
	value = egg_asn1x_get_time_as_long (asn);
	g_assert (value == -1);

	egg_asn1x_destroy (asn);
}

static void
test_implicit (void)
{
	GNode *asn;
	gchar *value;

	asn = egg_asn1x_create (test_asn1_tab, "TestImplicit");
	g_assert (asn);

	/* Should work */
	if (!egg_asn1x_decode (asn, SIMPLICIT, XL (SIMPLICIT)))
		g_assert_not_reached ();
	value = egg_asn1x_get_string_as_utf8 (asn, NULL);
	g_assert_cmpstr (value, ==, "implicit");
	g_free (value);

	egg_asn1x_destroy (asn);
}

static void
test_explicit (void)
{
	GNode *asn;
	gchar *value;

	asn = egg_asn1x_create (test_asn1_tab, "TestExplicit");
	g_assert (asn);

	/* Should work */
	if (!egg_asn1x_decode (asn, SEXPLICIT, XL (SEXPLICIT)))
		g_assert_not_reached ();
	value = egg_asn1x_get_string_as_utf8 (asn, NULL);
	g_assert_cmpstr (value, ==, "explicit");
	g_free (value);

	egg_asn1x_destroy (asn);
}

static void
test_bit_string_decode (void)
{
	GNode *asn;
	guchar *bits;
	guint n_bits;

	asn = egg_asn1x_create (test_asn1_tab, "TestBitString");
	g_assert (asn);

	/* Should work */
	if (!egg_asn1x_decode (asn, BITS_TEST, XL (BITS_TEST)))
		g_assert_not_reached ();

	bits = egg_asn1x_get_bits_as_raw (asn, NULL, &n_bits);
	g_assert (bits);
	g_assert_cmpuint (n_bits, ==, 18);
	g_assert_cmpint (bits[0], ==, 0x6e);
	g_assert_cmpint (bits[1], ==, 0x5d);
	g_assert_cmpint (bits[2], ==, 0xc0);

	g_free (bits);
	egg_asn1x_destroy (asn);
}

static void
test_bit_string_decode_bad (void)
{
	GNode *asn;

	asn = egg_asn1x_create (test_asn1_tab, "TestBitString");
	g_assert (asn);

	/* Should not work */
	if (egg_asn1x_decode (asn, BITS_BAD, XL (BITS_BAD)))
		g_assert_not_reached ();

	egg_asn1x_destroy (asn);
}

static void
test_bit_string_decode_ulong (void)
{
	GNode *asn;
	gulong bits;
	guint n_bits;

	asn = egg_asn1x_create (test_asn1_tab, "TestBitString");
	g_assert (asn);

	/* Should work */
	if (!egg_asn1x_decode (asn, BITS_TEST, XL (BITS_TEST)))
		g_assert_not_reached ();

	if (!egg_asn1x_get_bits_as_ulong (asn, &bits, &n_bits))
		g_assert_not_reached ();

	g_assert_cmpuint (n_bits, ==, 18);
	g_assert_cmphex (bits, ==, 0x1b977);

	egg_asn1x_destroy (asn);
}

static void
test_bit_string_encode_decode (void)
{
	GNode *asn;
	guchar bits[] = { 0x5d, 0x6e, 0x83 };
	guchar *check;
	guint n_check, n_bits = 17;
	gpointer data;
	gsize n_data;

	asn = egg_asn1x_create (test_asn1_tab, "TestBitString");
	g_assert (asn);

	if (!egg_asn1x_set_bits_as_raw (asn, bits, n_bits, NULL))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	if (!egg_asn1x_decode (asn, data, n_data))
		g_assert_not_reached ();

	check = egg_asn1x_get_bits_as_raw (asn, NULL, &n_check);
	g_assert (check);
	g_assert_cmpuint (n_check, ==, 17);
	g_assert_cmpint (check[0], ==, 0x5d);
	g_assert_cmpint (check[1], ==, 0x6e);
	g_assert_cmpint (check[2], ==, 0x80);

	g_free (check);

	g_free (data);
	egg_asn1x_destroy (asn);
}

static void
test_bit_string_encode_decode_ulong (void)
{
	GNode *asn;
	gulong check, bits = 0x0101b977;
	guint n_check, n_bits = 18;
	gpointer data;
	gsize n_data;

	asn = egg_asn1x_create (test_asn1_tab, "TestBitString");
	g_assert (asn);

	if (!egg_asn1x_set_bits_as_ulong (asn, bits, n_bits))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	if (!egg_asn1x_decode (asn, data, n_data))
		g_assert_not_reached ();

	if (!egg_asn1x_get_bits_as_ulong (asn, &check, &n_check))
		g_assert_not_reached ();

	g_assert_cmpuint (n_check, ==, 18);
	g_assert_cmphex (check, ==, 0x1b977);

	g_free (data);
	egg_asn1x_destroy (asn);
}

static void
test_bit_string_encode_decode_zero (void)
{
	GNode *asn;
	gpointer data;
	gsize n_data;

	asn = egg_asn1x_create (test_asn1_tab, "TestBitString");
	g_assert (asn);

	if (!egg_asn1x_set_bits_as_raw (asn, (guchar*)"", 0, NULL))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	egg_assert_cmpsize (n_data, ==, XL (BITS_ZERO));
	g_assert (memcmp (data, BITS_ZERO, n_data) == 0);

	g_free (data);
	egg_asn1x_destroy (asn);
}

static void
test_have (void)
{
	GNode *asn;
	guchar *data;
	gsize n_data;

	asn = egg_asn1x_create (test_asn1_tab, "TestBoolean");
	g_assert (asn);

	g_assert (!egg_asn1x_have (asn));

	if (!egg_asn1x_set_boolean (asn, TRUE))
		g_assert_not_reached ();

	g_assert (!egg_asn1x_have (asn));

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	g_assert (egg_asn1x_have (asn));

	g_free (data);
	egg_asn1x_destroy (asn);
}

static gboolean is_freed = FALSE;

static void
test_is_freed (gpointer unused)
{
	g_assert (!is_freed);
	is_freed = TRUE;
}

static void
test_any_set_raw (void)
{
	GNode *asn, *node;
	guchar *data;
	const guchar *check;
	gsize n_data, n_check;

	/* ENCODED SEQUENCE ANY with OCTET STRING */
	const gchar SEQ_ENCODING[] =  "\x30\x0C\x04\x0A""farnsworth";

	asn = egg_asn1x_create (test_asn1_tab, "TestAnySeq");
	g_assert (asn);

	is_freed = FALSE;
	node = egg_asn1x_node (asn, "contents", NULL);
	g_assert (node);

	if (!egg_asn1x_set_raw_element (node, (guchar*)SFARNSWORTH, XL (SFARNSWORTH), test_is_freed))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	egg_assert_cmpsize (n_data, ==, XL (SEQ_ENCODING));
	g_assert (memcmp (data, SEQ_ENCODING, n_data) == 0);

	check = egg_asn1x_get_raw_element (node, &n_check);
	g_assert (check);

	egg_assert_cmpsize (n_check, ==, XL (SFARNSWORTH));
	g_assert (memcmp (check, SFARNSWORTH, n_check) == 0);

	g_free (data);
	egg_asn1x_destroy (asn);
	g_assert (is_freed);
}

static void
test_any_set_raw_explicit (void)
{
	GNode *asn, *node;
	guchar *data;
	const guchar *check;
	gsize n_data, n_check;

	/* ENCODED SEQUENCE [89] ANY with OCTET STRING */
	const gchar SEQ_ENCODING[] =  "\x30\x0F\xBF\x59\x0C\x04\x0A""farnsworth";

	asn = egg_asn1x_create (test_asn1_tab, "TestAnyExp");
	g_assert (asn);

	is_freed = FALSE;
	node = egg_asn1x_node (asn, "contents", NULL);
	g_assert (node);

	if (!egg_asn1x_set_raw_element (node, (guchar*)SFARNSWORTH, XL (SFARNSWORTH), test_is_freed))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	egg_assert_cmpsize (n_data, ==, XL (SEQ_ENCODING));
	g_assert (memcmp (data, SEQ_ENCODING, n_data) == 0);

	check = egg_asn1x_get_raw_element (node, &n_check);
	g_assert (check);

	g_assert (n_check == XL (SFARNSWORTH));
	g_assert (memcmp (check, SFARNSWORTH, n_check) == 0);

	g_free (data);
	egg_asn1x_destroy (asn);
	g_assert (is_freed);
}

static void
test_choice_not_chosen (void)
{
	GNode *asn, *node;
	guchar *data;
	gsize n_data;

	asn = egg_asn1x_create (test_asn1_tab, "TestAnyChoice");
	g_assert (asn);

	node = egg_asn1x_node (asn, "choiceShortTag", NULL);
	g_assert (node);

	if (!egg_asn1x_set_raw_element (node, (guchar*)SFARNSWORTH, XL (SFARNSWORTH), NULL))
		g_assert_not_reached ();

	/* egg_asn1x_set_choice() was not called */
	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (!data);
	g_assert (egg_asn1x_message (asn));
	g_assert (strstr (egg_asn1x_message (asn), "TestAnyChoice") != NULL);

	egg_asn1x_destroy (asn);
}

static void
perform_asn1_any_choice_set_raw (const gchar *choice, const gchar *encoding, gsize n_encoding)
{
	GNode *asn, *node;
	guchar *data;
	const guchar *check;
	gsize n_data, n_check;

	asn = egg_asn1x_create (test_asn1_tab, "TestAnyChoice");
	g_assert (asn);

	is_freed = FALSE;
	node = egg_asn1x_node (asn, choice, NULL);
	g_assert (node);

	if (!egg_asn1x_set_choice (asn, node))
		g_assert_not_reached ();

	if (!egg_asn1x_set_raw_element (node, (guchar*)SFARNSWORTH, XL (SFARNSWORTH), test_is_freed))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, NULL, &n_data);
	if (!data) {
		g_printerr ("%s\n", egg_asn1x_message (asn));
		g_assert_not_reached ();
	}
	g_assert (data);

	egg_assert_cmpsize (n_data, ==, n_encoding);
	g_assert (memcmp (data, encoding, n_data) == 0);

	check = egg_asn1x_get_raw_element (node, &n_check);
	g_assert (check);

	g_assert (n_check == XL (SFARNSWORTH));
	g_assert (memcmp (check, SFARNSWORTH, n_check) == 0);

	g_free (data);
	egg_asn1x_destroy (asn);
	g_assert (is_freed);
}

static void
test_any_choice_set_raw_short_tag (void)
{
	const gchar ENCODING[] = "\xBE\x0C\x04\x0A""farnsworth";
	perform_asn1_any_choice_set_raw ("choiceShortTag", ENCODING, XL (ENCODING));
}

static void
test_any_choice_set_raw_long_tag (void)
{
	const gchar ENCODING[] = "\xBF\x1F\x0C\x04\x0A""farnsworth";
	perform_asn1_any_choice_set_raw ("choiceLongTag", ENCODING, XL (ENCODING));
}

static void
test_append (void)
{
	GNode *asn;
	GNode *child;
	gpointer data;
	gsize n_data;

	/* SEQUENCE OF with one INTEGER = 1 */
	const gchar SEQOF_ONE[] =  "\x30\x03\x02\x01\x01";

	/* SEQUENCE OF with two INTEGER = 1, 2 */
	const gchar SEQOF_TWO[] =  "\x30\x06\x02\x01\x01\x02\x01\x02";

	asn = egg_asn1x_create_and_decode (test_asn1_tab, "TestSeqOf", SEQOF_ONE, XL (SEQOF_ONE));
	g_assert (asn);

	child = egg_asn1x_append (asn);
	g_assert (child);

	/* Second integer is 2 */
	if (!egg_asn1x_set_integer_as_ulong (child, 2))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	g_assert (n_data == XL (SEQOF_TWO));
	g_assert (memcmp (data, SEQOF_TWO, n_data) == 0);

	g_free (data);
	egg_asn1x_destroy (asn);
}

static void
test_append_and_clear (void)
{
	GNode *asn;
	gpointer data;
	gsize n_data;

	asn = egg_asn1x_create (test_asn1_tab, "TestSeqOf");
	g_assert (asn);

	g_assert_cmpuint (egg_asn1x_count (asn), ==, 0);

	if (!egg_asn1x_set_integer_as_ulong (egg_asn1x_append (asn), 2))
		g_assert_not_reached ();
	if (!egg_asn1x_set_integer_as_ulong (egg_asn1x_append (asn), 3))
		g_assert_not_reached ();

	g_assert_cmpuint (egg_asn1x_count (asn), ==, 0);

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	g_assert_cmpuint (egg_asn1x_count (asn), ==, 2);

	egg_asn1x_clear (asn);
	g_assert_cmpuint (egg_asn1x_count (asn), ==, 0);

	egg_asn1x_destroy (asn);
	g_free (data);
}

static void
test_setof (void)
{
	GNode *asn;
	gpointer data;
	gsize n_data;

	/* SEQUENCE OF with one INTEGER = 3 */
	const gchar SETOF_ONE[] =  "\x31\x03\x02\x01\x03";

	/* SET OF with two INTEGER = 1, 3, 8 */
	const gchar SETOF_THREE[] =  "\x31\x09\x02\x01\x01\x02\x01\x03\x02\x01\x08";

	asn = egg_asn1x_create_and_decode (test_asn1_tab, "TestSetOf", SETOF_ONE, XL (SETOF_ONE));
	g_assert (asn);

	/* Add integer 1, in SET OF DER should sort to front */
	if (!egg_asn1x_set_integer_as_ulong (egg_asn1x_append (asn), 1))
		g_assert_not_reached ();

	/* Add integer 8, in SET OF DER should sort to back */
	if (!egg_asn1x_set_integer_as_ulong (egg_asn1x_append (asn), 8))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, NULL, &n_data);
	if (!data) {
		g_printerr ("%s\n", egg_asn1x_message (asn));
		g_assert_not_reached ();
	}

	g_assert (n_data == XL (SETOF_THREE));
	g_assert (memcmp (data, SETOF_THREE, n_data) == 0);

	g_free (data);
	egg_asn1x_destroy (asn);
}

static void
test_setof_empty (void)
{
	GNode *asn;
	gpointer data;
	gsize n_data;

	/* SEQUENCE OF with nothing */
	const gchar SETOF_NONE[] =  "\x31\x00";

	asn = egg_asn1x_create (test_asn1_tab, "TestSetOf");
	g_assert (asn);

	data = egg_asn1x_encode (asn, NULL, &n_data);
	if (!data) {
		g_printerr ("%s\n", egg_asn1x_message (asn));
		g_assert_not_reached ();
	}

	g_assert (n_data == XL (SETOF_NONE));
	g_assert (memcmp (data, SETOF_NONE, n_data) == 0);

	g_free (data);
	egg_asn1x_destroy (asn);
}

static void
test_enumerated (void)
{
	GNode *asn;
	gpointer data;
	gsize n_data;
	GQuark value;

	asn = egg_asn1x_create_and_decode (test_asn1_tab, "TestEnumerated", ENUM_TWO, XL (ENUM_TWO));
	g_assert (asn);

	value = egg_asn1x_get_enumerated (asn);
	g_assert (value);
	g_assert_cmpstr (g_quark_to_string (value), ==, "valueTwo");

	if (!egg_asn1x_set_enumerated (asn, g_quark_from_static_string ("valueThree")))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	g_assert (n_data == XL (ENUM_THREE));
	g_assert (memcmp (data, ENUM_THREE, n_data) == 0);

	g_free (data);
	egg_asn1x_destroy (asn);
}

typedef struct {
	GNode *asn1;
	guchar *data;
	gsize n_data;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	if (!g_file_get_contents (SRCDIR "/files/test-certificate-1.der",
	                          (gchar**)&test->data, &test->n_data, NULL))
		g_assert_not_reached ();

	test->asn1 = egg_asn1x_create (pkix_asn1_tab, "Certificate");
	g_assert (test->asn1 != NULL);

	if (!egg_asn1x_decode (test->asn1, test->data, test->n_data))
		g_assert_not_reached ();
}

static void
teardown (Test *test, gconstpointer unused)
{
	egg_asn1x_destroy (test->asn1);
	g_free (test->data);
}

static void
test_node_name (Test* test, gconstpointer unused)
{
	g_assert_cmpstr (egg_asn1x_name (test->asn1), ==, "Certificate");
}

static void
test_asn1_integers (Test* test, gconstpointer unused)
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

static void
test_boolean_seq (Test* test, gconstpointer unused)
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

static void
test_write_value (Test* test, gconstpointer unused)
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

static void
test_element_length_content (Test* test, gconstpointer unused)
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

	const char *BAD_ASN_TAG = "\x00";
	content = egg_asn1x_element_content (BAD_ASN_TAG, 1, &n_content);
	g_assert (content == NULL);

	const char *BAD_ASN_LENGTH = "\x30\x80";
	content = egg_asn1x_element_content (BAD_ASN_LENGTH, 2, &n_content);
	g_assert (content == NULL);

	egg_asn1x_destroy (asn);
	g_free (buffer);
}

static void
test_read_element (Test* test, gconstpointer unused)
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

static void
test_oid (Test* test, gconstpointer unused)
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

	/* Bad ones */
	{ "200707", -1 },

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

	/* Bad ones */
	{ "0707", -1 },

	{ NULL, 0 }
};

static void
test_general_time (Test* test, gconstpointer unused)
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

static void
test_utc_time (Test* test, gconstpointer unused)
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

static void
test_read_time (Test* test, gconstpointer unused)
{
	glong time;

	time = egg_asn1x_get_time_as_long (egg_asn1x_node (test->asn1, "tbsCertificate", "validity", "notBefore", NULL));
	g_assert_cmpint (time, ==, 820454400);
}

static void
test_read_date (Test* test, gconstpointer unused)
{
	GDate date;
	if (!egg_asn1x_get_time_as_date (egg_asn1x_node (test->asn1, "tbsCertificate", "validity", "notAfter", NULL), &date))
		g_assert_not_reached ();
	g_assert_cmpint (date.day, ==, 31);
	g_assert_cmpint (date.month, ==, 12);
	g_assert_cmpint (date.year, ==, 2020);
}

static void
test_create_by_oid (Test* test, gconstpointer unused)
{
	/* id-at-initials = X520initials */
	GNode *node = egg_asn1x_create (pkix_asn1_tab, "2.5.4.43");
	g_assert (node != NULL);
	g_assert_cmpstr (egg_asn1x_name (node), ==, "X520initials");
	egg_asn1x_destroy (node);
}

static void
test_create_by_oid_invalid (Test* test, gconstpointer unused)
{
	GNode *node = egg_asn1x_create (pkix_asn1_tab, "23.23.23.23");
	g_assert (node == NULL);
}

static void
test_create_by_bad_order (Test* test, gconstpointer unused)
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

static void
test_count (Test* test, gconstpointer unused)
{
	GNode *node;

	node = egg_asn1x_node (test->asn1, "tbsCertificate", "issuer", "rdnSequence", NULL);
	g_assert (node);
	g_assert_cmpuint (egg_asn1x_count (node), ==, 7);
}

int
main (int argc, char **argv)
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/asn1/boolean", test_boolean);
	g_test_add_func ("/asn1/null", test_null);
	g_test_add_func ("/asn1/integer", test_integer);
	g_test_add_func ("/asn1/octet_string", test_octet_string);
	g_test_add_func ("/asn1/generalized_time", test_generalized_time);
	g_test_add_func ("/asn1/implicit", test_implicit);
	g_test_add_func ("/asn1/explicit", test_explicit);
	g_test_add_func ("/asn1/bit_string_decode", test_bit_string_decode);
	g_test_add_func ("/asn1/bit_string_decode_bad", test_bit_string_decode_bad);
	g_test_add_func ("/asn1/bit_string_decode_ulong", test_bit_string_decode_ulong);
	g_test_add_func ("/asn1/bit_string_encode_decode", test_bit_string_encode_decode);
	g_test_add_func ("/asn1/bit_string_encode_decode_ulong", test_bit_string_encode_decode_ulong);
	g_test_add_func ("/asn1/bit_string_encode_decode_zero", test_bit_string_encode_decode_zero);
	g_test_add_func ("/asn1/have", test_have);
	g_test_add_func ("/asn1/any_set_raw", test_any_set_raw);
	g_test_add_func ("/asn1/any_set_raw_explicit", test_any_set_raw_explicit);
	g_test_add_func ("/asn1/choice_not_chosen", test_choice_not_chosen);
	g_test_add_func ("/asn1/any_choice_set_raw_short_tag", test_any_choice_set_raw_short_tag);
	g_test_add_func ("/asn1/any_choice_set_raw_long_tag", test_any_choice_set_raw_long_tag);
	g_test_add_func ("/asn1/append", test_append);
	g_test_add_func ("/asn1/append_and_clear", test_append_and_clear);
	g_test_add_func ("/asn1/setof", test_setof);
	g_test_add_func ("/asn1/setof_empty", test_setof_empty);
	g_test_add_func ("/asn1/enumerated", test_enumerated);
	g_test_add ("/asn1/node_name", Test, NULL, setup, test_node_name, teardown);
	g_test_add ("/asn1/asn1_integers", Test, NULL, setup, test_asn1_integers, teardown);
	g_test_add ("/asn1/boolean_seq", Test, NULL, setup, test_boolean_seq, teardown);
	g_test_add ("/asn1/write_value", Test, NULL, setup, test_write_value, teardown);
	g_test_add ("/asn1/element_length_content", Test, NULL, setup, test_element_length_content, teardown);
	g_test_add ("/asn1/read_element", Test, NULL, setup, test_read_element, teardown);
	g_test_add ("/asn1/oid", Test, NULL, setup, test_oid, teardown);
	g_test_add ("/asn1/general_time", Test, NULL, setup, test_general_time, teardown);
	g_test_add ("/asn1/utc_time", Test, NULL, setup, test_utc_time, teardown);
	g_test_add ("/asn1/read_time", Test, NULL, setup, test_read_time, teardown);
	g_test_add ("/asn1/read_date", Test, NULL, setup, test_read_date, teardown);
	g_test_add ("/asn1/create_by_oid", Test, NULL, setup, test_create_by_oid, teardown);
	g_test_add ("/asn1/create_by_oid_invalid", Test, NULL, setup, test_create_by_oid_invalid, teardown);
	g_test_add ("/asn1/create_by_bad_order", Test, NULL, setup, test_create_by_bad_order, teardown);
	g_test_add ("/asn1/count", Test, NULL, setup, test_count, teardown);

	return g_test_run ();
}
