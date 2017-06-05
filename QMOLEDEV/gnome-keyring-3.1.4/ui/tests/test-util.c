/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-util.c: Test gku-prompt-util.c

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

#include "ui/gku-prompt-util.h"

#include "egg/egg-dh.h"
#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"

#include <gcrypt.h>

#include <glib-object.h>

EGG_SECURE_GLIB_DEFINITIONS ();

typedef struct {
	GKeyFile *key_file;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	egg_libgcrypt_initialize ();
	test->key_file = g_key_file_new ();
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_key_file_free (test->key_file);
	test->key_file = NULL;
}

static void
test_encode_decode_mpi (Test *test, gconstpointer unused)
{
	gcry_mpi_t mpi, check;

	mpi = gcry_mpi_new (512);
	gcry_mpi_randomize (mpi, 512, GCRY_WEAK_RANDOM);

	gku_prompt_util_encode_mpi (test->key_file, "section", "field", mpi);
	if (!gku_prompt_util_decode_mpi (test->key_file, "section", "field", &check))
		g_assert_not_reached ();

	g_assert (gcry_mpi_cmp (mpi, check) == 0);
	gcry_mpi_release (mpi);
	gcry_mpi_release (check);
}

static void
test_decode_nonexistant_mpi (Test *test, gconstpointer unused)
{
	gcry_mpi_t mpi;

	if (gku_prompt_util_decode_mpi (test->key_file, "nonexist", "nope", &mpi))
		g_assert_not_reached ();
}

static void
test_encode_decode_hex (Test *test, gconstpointer unused)
{
	gchar buffer[32];
	gpointer check;
	gsize n_check;

	gcry_create_nonce (buffer, 32);
	gku_prompt_util_encode_hex (test->key_file, "section", "field", buffer, 32);
	check = gku_prompt_util_decode_hex (test->key_file, "section", "field", &n_check);
	g_assert (check);
	g_assert (n_check == 32);
	g_assert (memcmp (buffer, check, 32) == 0);

	g_free (check);
}

static void
test_decode_nonexistant_hex (Test *test, gconstpointer unused)
{
	gsize n_data;

	if (gku_prompt_util_decode_hex (test->key_file, "nonexist", "nope", &n_data))
		g_assert_not_reached ();
}

static void
do_encrypt_decrypt_text (const gchar *text)
{
	gpointer key, enc;
	gsize n_key, n_enc;
	guchar iv[16];
	gchar *check;

	g_test_message ("prompt encrypt/decrypt text: %s", text);

	/* Test making a key */
	n_key = 16;
	key = egg_secure_alloc (n_key);
	gcry_randomize (key, n_key, GCRY_WEAK_RANDOM);

	gcry_create_nonce (iv, 16);
	enc = gku_prompt_util_encrypt_text (key, n_key, iv, 16, text, &n_enc);

	g_assert (enc);
	/* Always greater due to null term */
	g_assert (n_enc > strlen (text));
	g_assert (n_enc % 16 == 0);

	check = gku_prompt_util_decrypt_text (key, n_key, iv, 16, enc, n_enc);
	egg_secure_clear (key, n_key);
	egg_secure_free (key);
	g_free (enc);

	g_assert (check);
	g_assert (strlen (check) < n_enc);
	g_assert_cmpstr (check, ==, text);
}

static void
test_encrypt_decrypt_text (Test *test, gconstpointer unused)
{
	do_encrypt_decrypt_text ("");
	do_encrypt_decrypt_text ("blah");
	do_encrypt_decrypt_text ("0123456789ABCDEF");
	do_encrypt_decrypt_text ("0123456789ABCDE");
	do_encrypt_decrypt_text ("0123456789ABCDEF 12345");
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/ui/util/encode_decode_mpi", Test, NULL, setup, test_encode_decode_mpi, teardown);
	g_test_add ("/ui/util/decode_nonexistant_mpi", Test, NULL, setup, test_decode_nonexistant_mpi, teardown);
	g_test_add ("/ui/util/encode_decode_hex", Test, NULL, setup, test_encode_decode_hex, teardown);
	g_test_add ("/ui/util/decode_nonexistant_hex", Test, NULL, setup, test_decode_nonexistant_hex, teardown);
	g_test_add ("/ui/util/encrypt_decrypt_text", Test, NULL, setup, test_encrypt_decrypt_text, teardown);

	return g_test_run ();
}
