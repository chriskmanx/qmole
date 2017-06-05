/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-crypto.c: Test crypto stuff

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

#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"
#include "egg/egg-symkey.h"

#include <gcrypt.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

EGG_SECURE_GLIB_DEFINITIONS ();

static const struct {
	const gchar *password;
	int cipher_algo;
	int hash_algo;
	int iterations;
	const gchar *salt;

	const gchar *result_simple;
	const gchar *result_pkcs12;
	const gchar *result_pbkdf2;
	const gchar *result_pbe;
} all_generation_tests[] = {

	{ /* 24 byte output */
		"booo", GCRY_CIPHER_3DES, GCRY_MD_MD5, 1,
		"\x70\x4C\xFF\xD6\x2F\xBA\x03\xE9",
		"\x84\x12\xBB\x34\x94\x8C\x40\xAD\x97\x57\x96\x74\x5B\x6A\xFB\xF8\xD6\x61\x33\x51\xEA\x8C\xCF\xD8",
		NULL,
		NULL,
		NULL
	},

	{ /* 5 byte output */
		"booo", GCRY_CIPHER_RFC2268_40, GCRY_MD_SHA1, 2048,
		"\x8A\x58\xC2\xE8\x7C\x1D\x80\x11",
		NULL,
		"\xD6\xA6\xF0\x76\x66",
		NULL,
		NULL
	},

	{ /* Null Password, 5 byte output */
		NULL, GCRY_CIPHER_RFC2268_40, GCRY_MD_SHA1, 2000,
		"\x04\xE0\x1C\x3E\xF8\xF2\xE9\xFD",
		NULL,
		"\x98\x7F\x20\x97\x1E",
		NULL,
		NULL
	},

	{ /* 24 byte output */
		"booo", GCRY_CIPHER_3DES, GCRY_MD_SHA1, 2048,
		"\xBD\xEE\x0B\xC6\xCF\x43\xAC\x25",
		NULL,
		"\x3F\x38\x1B\x0E\x87\xEB\x19\xBE\xD1\x39\xDC\x5B\xC2\xD2\xB3\x3C\x35\xA8\xB8\xF9\xEE\x66\x48\x94",
		"\x20\x25\x90\xD8\xD6\x98\x3E\x71\x10\x17\x1F\x51\x49\x87\x27\xCA\x97\x27\xD1\xC9\x72\xF8\x11\xBB",
		NULL
	},

	{ /* Empty password, 24 byte output */
		"", GCRY_CIPHER_3DES, GCRY_MD_SHA1, 2048,
		"\xF7\xCF\xD9\xCF\x1F\xF3\xAD\xF6",
		NULL,
		NULL,
		"\x53\xE3\x35\x9E\x5D\xC1\x85\x1A\x71\x3A\x67\x4E\x80\x56\x13\xD6\x4E\x3E\x89\x43\xB7\x1D\x5F\x7F",
		NULL
	},

	{ /* Empty password, 24 byte output */
		"", GCRY_CIPHER_3DES, GCRY_MD_SHA1, 2048,
		"\xD9\xB3\x2E\xC7\xBA\x1A\x8E\x15",
		NULL,
		"\x39\x70\x75\x7C\xF5\xE2\x13\x0B\x5D\xC2\x9D\x96\x8B\x71\xC7\xFC\x5B\x97\x1F\x79\x9F\x06\xFC\xA2",
		NULL,
		NULL
	},

	{ /* 8 byte output */
		"booo", GCRY_CIPHER_DES, GCRY_MD_MD5, 2048,
		"\x93\x4C\x3D\x29\xA2\x42\xB0\xF5",
		NULL,
		NULL,
		NULL,
		"\x8C\x67\x19\x7F\xB9\x23\xE2\x8D"
	}
};

#define N_GENERATION_TESTS (sizeof (all_generation_tests) / sizeof (all_generation_tests[0]))

static void
test_generate_key_simple (void)
{
	int i;
	gboolean ret;
	guchar *key;

	for (i = 0; i < N_GENERATION_TESTS; ++i) {

		if (!all_generation_tests[i].result_simple)
			continue;

		ret = egg_symkey_generate_simple (all_generation_tests[i].cipher_algo,
		                                  all_generation_tests[i].hash_algo,
		                                  all_generation_tests[i].password, -1,
		                                  (guchar*)all_generation_tests[i].salt, 8,
		                                  all_generation_tests[i].iterations,
		                                  &key, NULL);
		g_assert (ret && "key generation failed");

		ret = (memcmp (key, all_generation_tests[i].result_simple,
		               gcry_cipher_get_algo_keylen (all_generation_tests[i].cipher_algo)) == 0);

		g_assert (ret && "invalid simple key generated");
	}
}

static void
test_generate_key_pkcs12 (void)
{
	int i;
	gboolean ret;
	guchar *key;

	for (i = 0; i < N_GENERATION_TESTS; ++i) {

		if (!all_generation_tests[i].result_pkcs12)
			continue;

		ret = egg_symkey_generate_pkcs12 (all_generation_tests[i].cipher_algo,
		                                  all_generation_tests[i].hash_algo,
		                                  all_generation_tests[i].password, -1,
		                                  (guchar*)all_generation_tests[i].salt, 8,
		                                  all_generation_tests[i].iterations,
		                                  &key, NULL);
		g_assert ("failed to generate pkcs12 key" && ret);

		ret = (memcmp (key, all_generation_tests[i].result_pkcs12,
			        gcry_cipher_get_algo_keylen (all_generation_tests[i].cipher_algo)) == 0);

		g_assert ("invalid pkcs12 key generated" && ret);
	}
}

static void
test_generate_key_pbkdf2 (void)
{
	int i;
	gboolean ret;
	guchar *key;

	for (i = 0; i < N_GENERATION_TESTS; ++i) {

		if (!all_generation_tests[i].result_pbkdf2)
			continue;

		ret = egg_symkey_generate_pbkdf2 (all_generation_tests[i].cipher_algo,
		                                  all_generation_tests[i].hash_algo,
		                                  all_generation_tests[i].password, -1,
		                                  (guchar*)all_generation_tests[i].salt, 8,
		                                  all_generation_tests[i].iterations,
		                                  &key, NULL);
		g_assert ("failed to generate pbkdf2 key" && ret);

		ret = (memcmp (key, all_generation_tests[i].result_pbkdf2,
			        gcry_cipher_get_algo_keylen (all_generation_tests[i].cipher_algo)) == 0);

		g_assert ("invalid pbkdf2 key generated" && ret);
	}
}

static void
test_generate_key_pbe (void)
{
	int i;
	gboolean ret;
	guchar *key;

	for (i = 0; i < N_GENERATION_TESTS; ++i) {

		if (!all_generation_tests[i].result_pbe)
			continue;

		ret = egg_symkey_generate_pbe (all_generation_tests[i].cipher_algo,
		                               all_generation_tests[i].hash_algo,
		                               all_generation_tests[i].password, -1,
		                               (guchar*)all_generation_tests[i].salt, 8,
		                               all_generation_tests[i].iterations,
		                               &key, NULL);
		g_assert ("failed to generate pbe key" && ret);

		ret = (memcmp (key, all_generation_tests[i].result_pbe,
			        gcry_cipher_get_algo_keylen (all_generation_tests[i].cipher_algo)) == 0);

		g_assert ("invalid pbe key generated" && ret);

	}
}

int
main (int argc, char **argv)
{
	g_test_init (&argc, &argv, NULL);
	egg_libgcrypt_initialize ();

	g_test_add_func ("/symkey/generate_key_simple", test_generate_key_simple);
	g_test_add_func ("/symkey/generate_key_pkcs12", test_generate_key_pkcs12);
	g_test_add_func ("/symkey/generate_key_pbkdf2", test_generate_key_pbkdf2);
	g_test_add_func ("/symkey/generate_key_pbe", test_generate_key_pbe);

	return g_test_run ();
}
