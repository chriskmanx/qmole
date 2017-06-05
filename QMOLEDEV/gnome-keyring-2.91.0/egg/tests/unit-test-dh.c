/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-dh.c: Test egg-dh.c

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "test-suite.h"

#include "egg-dh.h"
#include "egg-secure-memory.h"

#include <gcrypt.h>

DEFINE_TEST(dh_perform)
{
	gcry_mpi_t p, g;
	gcry_mpi_t x1, X1;
	gcry_mpi_t x2, X2;
	gpointer k1, k2;
	gboolean ret;

	/* Load up the parameters */
	if (!egg_dh_default_params ("ietf-ike-grp-modp-768", &p, &g))
		g_assert_not_reached ();

	/* Generate secrets */
	ret = egg_dh_gen_pair (p, g, 0, &X1, &x1);
	g_assert (ret);
	ret = egg_dh_gen_pair (p, g, 0, &X2, &x2);
	g_assert (ret);

	/* Calculate keys */
	k1 = egg_dh_gen_secret (X2, x1, p, 96);
	g_assert (k1);
	k2 = egg_dh_gen_secret (X1, x2, p, 96);
	g_assert (k2);

	/* Keys must be the same */
	g_assert (memcmp (k1, k2, 96) == 0);

	gcry_mpi_release (p);
	gcry_mpi_release (g);
	gcry_mpi_release (x1);
	gcry_mpi_release (X1);
	egg_secure_free (k1);
	gcry_mpi_release (x2);
	gcry_mpi_release (X2);
	egg_secure_free (k2);
}

DEFINE_TEST(dh_short_pair)
{
	gcry_mpi_t p, g;
	gcry_mpi_t x1, X1;
	gboolean ret;

	/* Load up the parameters */
	ret = egg_dh_default_params ("ietf-ike-grp-modp-1024", &p, &g);
	g_assert (ret);
	g_assert_cmpuint (gcry_mpi_get_nbits (p), ==, 1024);

	/* Generate secrets */
	ret = egg_dh_gen_pair (p, g, 512, &X1, &x1);
	g_assert (ret);
	g_assert_cmpuint (gcry_mpi_get_nbits (x1), <=, 512);

	gcry_mpi_release (p);
	gcry_mpi_release (g);
	gcry_mpi_release (x1);
	gcry_mpi_release (X1);
}

static void
test_dh_default (const gchar *name, guint bits)
{
	gboolean ret;
	gcry_mpi_t p, g, check;
	gconstpointer prime, base;
	gsize n_prime, n_base;
	gcry_error_t gcry;

	ret = egg_dh_default_params (name, &p, &g);
	g_assert (ret);
	g_assert_cmpint (gcry_mpi_get_nbits (p), ==, bits);
	g_assert_cmpint (gcry_mpi_get_nbits (g), <, gcry_mpi_get_nbits (p));

	ret = egg_dh_default_params_raw (name, &prime, &n_prime, &base, &n_base);
	g_assert (ret);
	g_assert (prime != NULL);
	g_assert_cmpsize (n_prime, >, 0);
	g_assert (base != NULL);
	g_assert_cmpsize (n_base, >, 0);

	gcry = gcry_mpi_scan (&check, GCRYMPI_FMT_USG, prime, n_prime, NULL);
	g_assert (gcry == 0);
	g_assert (gcry_mpi_cmp (check, p) == 0);
	gcry_mpi_release (check);

	gcry = gcry_mpi_scan (&check, GCRYMPI_FMT_USG, base, n_base, NULL);
	g_assert (gcry == 0);
	g_assert (gcry_mpi_cmp (check, g) == 0);
	gcry_mpi_release (check);

	gcry_mpi_release (p);
	gcry_mpi_release (g);
}

DEFINE_TEST(dh_default_768)
{
	test_dh_default ("ietf-ike-grp-modp-768", 768);
}

DEFINE_TEST(dh_default_1024)
{
	test_dh_default ("ietf-ike-grp-modp-1024", 1024);
}

DEFINE_TEST(dh_default_1536)
{
	test_dh_default ("ietf-ike-grp-modp-1536", 1536);
}


DEFINE_TEST(dh_default_2048)
{
	test_dh_default ("ietf-ike-grp-modp-2048", 2048);
}

DEFINE_TEST(dh_default_3072)
{
	test_dh_default ("ietf-ike-grp-modp-3072", 3072);
}

DEFINE_TEST(dh_default_4096)
{
	test_dh_default ("ietf-ike-grp-modp-4096", 4096);
}

DEFINE_TEST(dh_default_8192)
{
	test_dh_default ("ietf-ike-grp-modp-8192", 8192);
}

DEFINE_TEST(dh_default_bad)
{
	gboolean ret;
	gcry_mpi_t p, g;

	ret = egg_dh_default_params ("bad-name", &p, &g);
	g_assert (!ret);
}
