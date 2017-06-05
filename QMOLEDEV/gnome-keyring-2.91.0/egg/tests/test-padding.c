/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-padding.c: Test padding functionality

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "test-suite.h"

#include "egg/egg-padding.h"

#include <gcrypt.h>

static void
test_padding (EggPadding padding, gsize block, gconstpointer input,
              gsize n_input, gconstpointer output, gsize n_output)
{
	gpointer result;
	gsize n_result;

	if (!(padding) (NULL, block, input, n_input, &result, &n_result)) {
		g_assert (output == NULL);
		return;
	}

	g_assert (result != NULL);
	g_assert_cmpsize (n_output, ==, n_result);
	g_assert (memcmp (output, result, n_output) == 0);
	g_free (result);

	/* Now make sure it can tell us the right length */
	if (!(padding) (NULL, block, input, n_input, NULL, &n_result))
		g_assert_not_reached ();

	g_assert_cmpsize (n_output, ==, n_result);
}

DEFINE_TEST(zero_padding)
{
	guchar padded[] = { 0x00, 0x00, 0x00, 0x00, 'T', 'E', 'S', 'T' };
	gchar raw[] = "TEST";
	test_padding (egg_padding_zero_pad, 8, raw, 4, padded, 8);
}

DEFINE_TEST(zero_padding_no_data)
{
	guchar padded[] = { };
	gchar raw[] = "";
	test_padding (egg_padding_zero_pad, 8, raw, 0, padded, 0);
}

DEFINE_TEST(pkcs1_one_padding)
{
	guchar padded[] = { 0x00, 0x01, 0xFF, 0x00, 'T', 'E', 'S', 'T' };
	gchar raw[] = "TEST";
	test_padding (egg_padding_pkcs1_pad_01, 8, raw, 4, padded, 8);
	test_padding (egg_padding_pkcs1_unpad_01, 8, padded, 8, raw, 4);
}

DEFINE_TEST(pkcs1_one_padding_no_data)
{
	guchar padded[] = { 0x00, 0x01, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00 };
	gchar raw[] = "";
	test_padding (egg_padding_pkcs1_pad_01, 8, raw, 0, padded, 8);
	test_padding (egg_padding_pkcs1_unpad_01, 8, padded, 8, raw, 0);
}

DEFINE_TEST(pkcs1_two_padding)
{
	guchar padded[] = { 0x00, 0x02, 0x77, 0x66, 0x55, 0x00, 'T', 'E', };
	gchar raw[] = "TE";
	guchar *result;
	gpointer vesult;
	gsize n_result;

	test_padding (egg_padding_pkcs1_unpad_02, 8, padded, 8, raw, 2);

	/* PKCS#1 02 padding is unpredictable */
	if (!egg_padding_pkcs1_pad_02 (NULL, 8, raw, 2, &vesult, &n_result))
		g_assert_not_reached ();
	result = vesult;
	g_assert (result != NULL);
	g_assert_cmpsize (n_result, ==, 8);
	g_assert (result[0] == 0x00);
	g_assert (result[1] == 0x02);
	g_assert (result[2] != 0x00);
	g_assert (result[3] != 0x00);
	g_assert (result[4] != 0x00);
	g_assert (result[5] == 0x00);
	g_assert (result[6] == 'T');
	g_assert (result[7] == 'E');
}

DEFINE_TEST(pkcs1_padding_invalid_prefix)
{
	guchar padded[] = { 0x01, 0x04, 0x04, 0x04 };
	test_padding (egg_padding_pkcs1_unpad_01, 4, padded, 4, NULL, 0);
}

DEFINE_TEST(pkcs1_padding_invalid_type)
{
	guchar padded[] = { 0x00, 0x03, 0xFF, 0x00, 'T', 'E', 'S', 'T' };
	test_padding (egg_padding_pkcs1_unpad_01, 8, padded, 8, NULL, 0);
}

DEFINE_TEST(pkcs1_padding_invalid_no_zero)
{
	guchar padded[] = { 0x00, 0x01, 0xFF, 0xFF, 'T', 'E', 'S', 'T' };
	test_padding (egg_padding_pkcs1_unpad_01, 8, padded, 8, NULL, 0);
}

DEFINE_TEST(pkcs1_padding_invalid_length)
{
	guchar padded[] = { 0x00, 0x01, 0xFF, 0xFF, 'T', 'E', 'S' };
	test_padding (egg_padding_pkcs1_unpad_01, 8, padded, 7, NULL, 0);
}

DEFINE_TEST(pkcs7_padding)
{
	guchar padded[] = { 'T', 'E', 'S', 'T', 0x04, 0x04, 0x04, 0x04 };
	gchar raw[] = "TEST";

	test_padding (egg_padding_pkcs7_pad, 8, raw, 4, padded, 8);
	test_padding (egg_padding_pkcs7_unpad, 8, padded, 8, raw, 4);
}

DEFINE_TEST(pkcs7_padding_equal_block)
{
	guchar padded[] = { 'T', 'E', 'S', 'T', 'T', 'E', 'S', 'T', 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08 };
	gchar raw[] = "TESTTEST";

	test_padding (egg_padding_pkcs7_pad, 8, raw, 8, padded, 16);
	test_padding (egg_padding_pkcs7_unpad, 8, padded, 16, raw, 8);
}

DEFINE_TEST(pkcs7_padding_zero)
{
	guchar padded[] = { 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08 };
	gchar raw[] = "";

	test_padding (egg_padding_pkcs7_pad, 8, raw, 0, padded, 8);
	test_padding (egg_padding_pkcs7_unpad, 8, padded, 8, raw, 0);
}

DEFINE_TEST(pkcs7_padding_invalid_zero)
{
	guchar padded[] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
	test_padding (egg_padding_pkcs7_unpad, 8, padded, 8, NULL, 0);
}

DEFINE_TEST(pkcs7_padding_invalid_too_long)
{
	guchar padded[] = { 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08 };
	test_padding (egg_padding_pkcs7_unpad, 4, padded, 8, NULL, 0);
	test_padding (egg_padding_pkcs7_unpad, 4, padded, 4, NULL, 0);
}

DEFINE_TEST(pkcs7_padding_invalid_different)
{
	guchar padded[] = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08 };
	test_padding (egg_padding_pkcs7_unpad, 8, padded, 8, NULL, 0);
}
