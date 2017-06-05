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

#include "test-suite.h"

#include "egg/egg-asn1x.h"

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

#define XL(x) G_N_ELEMENTS (x) - 1

DEFINE_TEST(asn1_boolean)
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

DEFINE_TEST(asn1_integer)
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

DEFINE_TEST(asn1_octet_string)
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

DEFINE_TEST(asn1_generalized_time)
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

DEFINE_TEST(asn1_implicit)
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

DEFINE_TEST(asn1_explicit)
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

DEFINE_TEST(asn1_bit_string_decode)
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

DEFINE_TEST(asn1_bit_string_decode_bad)
{
	GNode *asn;

	asn = egg_asn1x_create (test_asn1_tab, "TestBitString");
	g_assert (asn);

	/* Should not work */
	if (egg_asn1x_decode (asn, BITS_BAD, XL (BITS_BAD)))
		g_assert_not_reached ();

	egg_asn1x_destroy (asn);
}

DEFINE_TEST(asn1_bit_string_decode_ulong)
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

DEFINE_TEST(asn1_bit_string_encode_decode)
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

DEFINE_TEST(asn1_bit_string_encode_decode_ulong)
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

DEFINE_TEST(asn1_bit_string_encode_decode_zero)
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

	g_assert_cmpsize (n_data, ==, XL (BITS_ZERO));
	g_assert (memcmp (data, BITS_ZERO, n_data) == 0);

	g_free (data);
	egg_asn1x_destroy (asn);
}

DEFINE_TEST(asn1_have)
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

DEFINE_TEST(asn1_any_set_raw)
{
	GNode *asn, *node;
	guchar *data;
	const guchar *check;
	gsize n_data, n_check;

	asn = egg_asn1x_create (test_asn1_tab, "TestAnySeq");
	g_assert (asn);

	is_freed = FALSE;
	node = egg_asn1x_node (asn, "contents", NULL);
	g_assert (node);

	if (!egg_asn1x_set_raw_element (node, (guchar*)SFARNSWORTH, XL (SFARNSWORTH), test_is_freed))
		g_assert_not_reached ();

	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert (data);

	check = egg_asn1x_get_raw_element (node, &n_check);
	g_assert (check);

	g_assert (n_check == XL (SFARNSWORTH));
	g_assert (memcmp (check, SFARNSWORTH, n_check) == 0);

	g_free (data);
	egg_asn1x_destroy (asn);
	g_assert (is_freed);
}
