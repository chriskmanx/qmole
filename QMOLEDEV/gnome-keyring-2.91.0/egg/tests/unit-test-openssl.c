/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-pkix-openssl.c: Test PKIX openssl

   Copyright (C) 2008 Stefan Walter

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

#include "egg-symkey.h"
#include "egg-openssl.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

guchar *refenc = NULL;
guchar *refdata = NULL;
gsize n_refenc = 0;
gsize n_refdata = 0;
GHashTable *refheaders = NULL;

static void
copy_each_key_value (gpointer key, gpointer value, gpointer user_data)
{
	g_hash_table_insert ((GHashTable*)user_data, g_strdup ((gchar*)key), g_strdup ((gchar*)value));
}

static void
parse_reference (GQuark type, const guchar *data, gsize n_data,
                 GHashTable *headers, gpointer user_data)
{
	gboolean res;
	const gchar *dekinfo;
	
	g_assert ("no data in PEM callback" && data != NULL);
	g_assert ("no data in PEM callback" && n_data > 0);
	refenc = g_memdup (data, n_data);
	n_refenc = n_data;
	
	g_assert ("no headers present in file" && headers != NULL);
	refheaders = egg_openssl_headers_new ();
	g_hash_table_foreach (headers, copy_each_key_value, refheaders);
	dekinfo = egg_openssl_get_dekinfo (headers);
	g_assert ("no dekinfo in headers" && dekinfo != NULL);
	
	res = egg_openssl_decrypt_block (dekinfo, "booo", 4, data, n_data, &refdata, &n_refdata);
	g_assert ("couldn't openssl decrypt block" && res == TRUE);
	g_assert ("no data returned from openssl decrypt" && refdata != NULL);
	g_assert ("invalid amount of data returned from openssl decrypt" && n_refdata == n_data);
}

DEFINE_TEST(parse_reference)
{
	guchar *input;
	gsize n_input;
	guint num;
	
	input = testing_data_read ("pem-rsa-enc.key", &n_input);

	num = egg_openssl_pem_parse (input, n_input, parse_reference, NULL);
	g_assert ("couldn't PEM block in reference data" && num == 1);
	
	g_assert ("parse_reference() wasn't called" && refdata != NULL);
}

DEFINE_TEST(write_reference)
{
	const gchar *dekinfo;
	guchar *encrypted;
	gsize n_encrypted;
	gboolean ret;
	
	dekinfo = egg_openssl_get_dekinfo (refheaders); 
	g_assert ("no dekinfo in headers" && dekinfo != NULL);

	ret = egg_openssl_encrypt_block (dekinfo, "booo", 4, refdata, n_refdata, &encrypted, &n_encrypted);
	g_assert ("couldn't openssl encrypt block" && ret == TRUE);
	g_assert ("no data returned from openssl encrypt" && encrypted != NULL);
	g_assert ("invalid amount of data returned from openssl encrypt" && n_refdata <= n_encrypted);
	
	g_assert ("data length doesn't match input length" && n_encrypted == n_refenc);
	g_assert ("data doesn't match input" && memcmp (encrypted, refenc, n_encrypted) == 0);
}

/* 29 bytes (prime number, so block length has bad chance of matching */
static const guchar *TEST_DATA = (guchar*)"ABCDEFGHIJKLMNOPQRSTUVWXYZ123";
const gsize TEST_DATA_L = 29;
	
DEFINE_TEST(openssl_roundtrip)
{
	const gchar *dekinfo;
	gboolean res;
	gboolean ret;
	guchar *encrypted, *decrypted;
	gsize n_encrypted, n_decrypted;
	int i;
	
	dekinfo = egg_openssl_prep_dekinfo (refheaders);
	
	ret = egg_openssl_encrypt_block (dekinfo, "password", -1, TEST_DATA, TEST_DATA_L, &encrypted, &n_encrypted);
	g_assert ("couldn't openssl encrypt block" && ret == TRUE);
	g_assert ("no data returned from openssl encrypt" && encrypted != NULL);
	g_assert ("invalid amount of data returned from openssl encrypt" && TEST_DATA_L <= n_encrypted);

	res = egg_openssl_decrypt_block (dekinfo, "password", 8, encrypted, n_encrypted, &decrypted, &n_decrypted);
	g_assert ("couldn't openssl decrypt block" && res == TRUE);
	g_assert ("no data returned from openssl decrypt" && decrypted != NULL);

	/* Check that the data was decrypted properly */
	g_assert ("decrypted data doesn't match length" && n_decrypted >= TEST_DATA_L);
	g_assert ("decrypted data doesn't match" && memcmp (TEST_DATA, decrypted, TEST_DATA_L) == 0);
	
	/* Check that the remainder is all zeros */
	for (i = TEST_DATA_L; i < n_decrypted; ++i)
		g_assert ("non null byte in padding" && decrypted[i] == 0);
}
