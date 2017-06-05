/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-util.c: Test gck-util.c

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

#include "egg/egg-hex.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static const guchar TEST_DATA[] = { 0x05, 0xD6, 0x95, 0x96, 0x10, 0x12, 0xAE, 0x35 };
static const gchar *TEST_HEX = "05D695961012AE35";
static const gchar *TEST_HEX_DELIM = "05 D6 95 96 10 12 AE 35";

static void
test_encode (void)
{
	gchar *hex;

	hex = egg_hex_encode (TEST_DATA, sizeof (TEST_DATA));
	g_assert (hex);
	g_assert_cmpstr (hex, ==, TEST_HEX);
}

static void
test_encode_spaces (void)
{
	gchar *hex;

	/* Encode without spaces */
	hex = egg_hex_encode_full (TEST_DATA, sizeof (TEST_DATA), TRUE, 0, 0);
	g_assert (hex);
	g_assert_cmpstr (hex, ==, TEST_HEX);

	/* Encode with spaces */
	hex = egg_hex_encode_full (TEST_DATA, sizeof (TEST_DATA), TRUE, ' ', 1);
	g_assert (hex);
	g_assert_cmpstr (hex, ==, TEST_HEX_DELIM);
}

static void
test_decode (void)
{
	guchar *data;
	gsize n_data;

	data = egg_hex_decode (TEST_HEX, -1, &n_data);
	g_assert (data);
	g_assert (n_data == sizeof (TEST_DATA));
	g_assert (memcmp (data, TEST_DATA, n_data) == 0);
	g_free (data);

	/* Nothing in, empty out */
	data = egg_hex_decode ("AB", 0, &n_data);
	g_assert (data);
	g_assert (n_data == 0);
	g_free (data);

	/* Delimited*/
	data = egg_hex_decode_full (TEST_HEX_DELIM, -1, ' ', 1, &n_data);
	g_assert (data);
	g_assert (n_data == sizeof (TEST_DATA));
	g_assert (memcmp (data, TEST_DATA, n_data) == 0);
	g_free (data);
}

static void
test_decode_fail (void)
{
	guchar *data;
	gsize n_data;

	/* Invalid input, null out */
	data = egg_hex_decode ("AB", 1, &n_data);
	g_assert (!data);

	/* Bad characters, null out */
	data = egg_hex_decode ("ABXX", -1, &n_data);
	g_assert (!data);

	/* Not Delimited, null out*/
	data = egg_hex_decode_full ("ABABAB", -1, ':', 1, &n_data);
	g_assert (!data);
}

int
main (int argc, char **argv)
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/hex/encode", test_encode);
	g_test_add_func ("/hex/encode_spaces", test_encode_spaces);
	g_test_add_func ("/hex/decode", test_decode);
	g_test_add_func ("/hex/decode_fail", test_decode_fail);

	return g_test_run ();
}
