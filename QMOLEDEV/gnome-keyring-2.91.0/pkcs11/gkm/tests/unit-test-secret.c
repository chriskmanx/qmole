/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-secret.c: Test gkm-secret.c

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

#include "gkm/gkm-secret.h"

DEFINE_TEST(test_secret)
{
	GkmSecret *secret;
	const gchar *password;
	gsize n_password;

	secret = gkm_secret_new ((guchar*)"test-pin", 8);
	g_assert (GKM_IS_SECRET (secret));

	password = gkm_secret_get_password (secret, &n_password);
	g_assert (password);
	g_assert_cmpuint (n_password, ==, 8);
	g_assert (memcmp (password, "test-pin", 8) == 0);

	g_assert (gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"test-pin", 8));
	g_assert (!gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"test-pino", 9));
	g_assert (!gkm_secret_equals (secret, NULL, 0));
	g_assert (gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"test-pin", -1));
	g_assert (!gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"", 0));

	g_object_unref (secret);
}

DEFINE_TEST(test_secret_from_login)
{
	GkmSecret *secret;
	const gchar *password;
	gsize n_password;

	secret = gkm_secret_new_from_login ((guchar*)"test-pin", 8);
	g_assert (GKM_IS_SECRET (secret));
	password = gkm_secret_get_password (secret, &n_password);
	g_assert (password);
	g_assert_cmpuint (n_password, ==, 8);
	g_assert (memcmp (password, "test-pin", 8) == 0);
	g_object_unref (secret);

	secret = gkm_secret_new_from_login ((guchar*)"test-pin", (CK_ULONG)-1);
	g_assert (GKM_IS_SECRET (secret));
	password = gkm_secret_get_password (secret, &n_password);
	g_assert (password);
	g_assert_cmpuint (n_password, ==, 8);
	g_assert (memcmp (password, "test-pin", 8) == 0);
	g_object_unref (secret);
}

DEFINE_TEST(test_null_terminated)
{
	GkmSecret *secret;
	const gchar *password;
	gsize n_password;

	secret = gkm_secret_new ((CK_UTF8CHAR_PTR)"null-terminated", -1);
	g_assert (GKM_IS_SECRET (secret));

	password = gkm_secret_get_password (secret, &n_password);
	g_assert (password);
	g_assert_cmpstr (password, ==, "null-terminated");
	g_assert_cmpuint (n_password, ==, strlen ("null-terminated"));

	g_assert (gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"null-terminated", strlen ("null-terminated")));
	g_assert (!gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"test-pino", 9));
	g_assert (!gkm_secret_equals (secret, NULL, 0));
	g_assert (gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"null-terminated", -1));
	g_assert (!gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"", 0));

	g_object_unref (secret);
}

DEFINE_TEST(test_always_has_null)
{
	GkmSecret *secret;
	const guchar *memory;
	gsize n_memory;

	/* A 4 byte 'binary' secret */
	secret = gkm_secret_new ((guchar*)"barn", 4);
	g_assert (GKM_IS_SECRET (secret));

	memory = gkm_secret_get (secret, &n_memory);
	g_assert_cmpuint (n_memory, ==, 4);

	/* But it should be null-terminated anyway */
	g_assert (memory[4] == 0);

	g_object_unref (secret);
}

DEFINE_TEST(test_null)
{
	GkmSecret *secret;
	const gchar *password;
	gsize n_password;

	secret = gkm_secret_new (NULL, 0);
	g_assert (GKM_IS_SECRET (secret));

	password = gkm_secret_get_password (secret, &n_password);
	g_assert (password == NULL);
	g_assert_cmpuint (n_password, ==, 0);

	g_assert (!gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"null-terminated", strlen ("null-terminated")));
	g_assert (!gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"test-pino", 9));
	g_assert (gkm_secret_equals (secret, NULL, 0));
	g_assert (!gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"null-terminated", -1));
	g_assert (gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"", 0));

	g_object_unref (secret);
}

DEFINE_TEST(test_empty)
{
	GkmSecret *secret;
	const gchar *password;
	gsize n_password;

	secret = gkm_secret_new ((CK_UTF8CHAR_PTR)"", 0);
	g_assert (GKM_IS_SECRET (secret));

	password = gkm_secret_get_password (secret, &n_password);
	g_assert_cmpstr (password, ==, "");
	g_assert_cmpuint (n_password, ==, 0);

	g_assert (!gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"null-terminated", strlen ("null-terminated")));
	g_assert (!gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"test-pino", 9));
	g_assert (gkm_secret_equals (secret, NULL, 0));
	g_assert (gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"", -1));
	g_assert (gkm_secret_equals (secret, (CK_UTF8CHAR_PTR)"", 0));

	g_object_unref (secret);
}

DEFINE_TEST(test_equal)
{
	GkmSecret *one;
	GkmSecret *two;

	one = two = gkm_secret_new ((CK_UTF8CHAR_PTR)"funny", 5);
	g_assert (gkm_secret_equal (one, two));

	two = gkm_secret_new_from_password ("funny");
	g_assert (gkm_secret_equal (one, two));

	g_object_unref (one);
	one = gkm_secret_new_from_password ("other");
	g_assert (!gkm_secret_equal (one, two));
}
