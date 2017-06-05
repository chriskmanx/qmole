/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-data-asn1.c: Test ASN.1 routines

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

#include "gkm/gkm-data-asn1.h"

#include <glib.h>
#include <gcrypt.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"

#include "asn1-def-test.h"

static GNode *asn1_cert = NULL;
static guchar *data_cert = NULL;
static gsize n_data_cert = 0;

DEFINE_SETUP(asn1_tree)
{
	data_cert = testing_data_read ("test-certificate-1.der", &n_data_cert);
	asn1_cert = egg_asn1x_create_and_decode (pkix_asn1_tab, "Certificate", data_cert, n_data_cert);
	g_assert (asn1_cert);
}

DEFINE_TEARDOWN(asn1_tree)
{
	egg_asn1x_destroy (asn1_cert);
	g_free (data_cert);
	data_cert = NULL;
}

DEFINE_TEST(asn1_integers)
{
	GNode *asn;
	gcry_mpi_t mpi, mpt;
	guchar *data;
	gsize n_data;
	gboolean ret;

	asn = egg_asn1x_create (test_asn1_tab, "TestIntegers");
	g_assert ("asn test structure is null" && asn != NULL);

	/* Make a random number */
	mpi = gcry_mpi_new (512);
	g_return_if_fail (mpi);
	gcry_mpi_randomize (mpi, 512, GCRY_WEAK_RANDOM);

	/* Write the mpi out */
	ret = gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "mpi", NULL), mpi);

	/* Now encode the whole caboodle */
	data = egg_asn1x_encode (asn, NULL, &n_data);
	g_assert ("encoding asn1 didn't work" && data != NULL);

	egg_asn1x_destroy (asn);

	/* Now decode it all nicely */
	asn = egg_asn1x_create_and_decode (test_asn1_tab, "TestIntegers", data, n_data);
	g_assert (asn);

	ret = gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "mpi", NULL), &mpt);
	g_assert ("couldn't read mpi from asn1" && ret);
	g_assert ("mpi returned is null" && mpt != NULL);
	g_assert ("mpi is wrong number" && gcry_mpi_cmp (mpi, mpt) == 0);

	g_free (data);
}
