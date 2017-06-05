/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-secret-compat.c: Test secret compat files

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

#include "secret-store/gkm-secret-data.h"

#include "gkm/gkm-secret.h"
#include "gkm/gkm-transaction.h"

#include "egg/egg-secure-memory.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

EGG_SECURE_GLIB_DEFINITIONS ();

static void
test_new (void)
{
	GkmSecretData *data = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	g_assert (GKM_IS_SECRET_DATA (data));
	g_object_unref (data);
}

static void
test_get_set (void)
{
	GkmSecretData *data = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	GkmSecret *secret = gkm_secret_new_from_password ("barn");
	GkmSecret *check;

	gkm_secret_data_set_secret (data, "my-identifier", secret);
	g_object_unref (secret);

	check = gkm_secret_data_get_secret (data, "my-identifier");
	g_assert (GKM_IS_SECRET (check));
	g_assert (secret == check);
	g_assert (gkm_secret_equals (check, (guchar*)"barn", -1));

	g_object_unref (data);
}

static void
test_get_raw (void)
{
	GkmSecretData *data = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	GkmSecret *secret = gkm_secret_new_from_password ("barn");
	const guchar *raw;
	gsize n_raw;

	gkm_secret_data_set_secret (data, "my-identifier", secret);
	g_object_unref (secret);

	raw = gkm_secret_data_get_raw (data, "my-identifier", &n_raw);
	g_assert (raw);
	g_assert_cmpuint (n_raw, ==, 4);
	g_assert (memcmp (raw, "barn", 4) == 0);

	raw = gkm_secret_data_get_raw (data, "not-identifier", &n_raw);
	g_assert (raw == NULL);

	g_object_unref (data);
}

static void
test_remove (void)
{
	GkmSecretData *data = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	GkmSecret *secret = gkm_secret_new_from_password ("barn");

	gkm_secret_data_set_secret (data, "my-identifier", secret);
	g_object_unref (secret);

	secret = gkm_secret_data_get_secret (data, "my-identifier");
	g_assert (GKM_IS_SECRET (secret));

	gkm_secret_data_remove_secret (data, "my-identifier");
	secret = gkm_secret_data_get_secret (data, "my-identifier");
	g_assert (!secret);

	g_object_unref (data);
}

static void
test_set_transacted (void)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	GkmSecretData *data = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	GkmSecret *secret = gkm_secret_new_from_password ("barn");

	/* Transaction, but not complete */
	gkm_secret_data_set_transacted (data, transaction, "my-identifier", secret);
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (gkm_secret_data_get_secret (data, "my-identifier") == secret);

	/* Transaction complete */
	gkm_transaction_complete (transaction);
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (gkm_secret_data_get_secret (data, "my-identifier") == secret);

	g_object_unref (data);
	g_object_unref (secret);
	g_object_unref (transaction);
}

static void
test_set_transacted_replace (void)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	GkmSecretData *data = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	GkmSecret *old = gkm_secret_new_from_password ("old");
	GkmSecret *secret = gkm_secret_new_from_password ("secret");

	/* The old secret */
	gkm_secret_data_set_secret (data, "my-identifier", old);
	g_assert (gkm_secret_data_get_secret (data, "my-identifier") == old);

	/* Transaction, but not complete */
	gkm_secret_data_set_transacted (data, transaction, "my-identifier", secret);
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (gkm_secret_data_get_secret (data, "my-identifier") == secret);

	/* Transaction complete */
	gkm_transaction_complete (transaction);
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (gkm_secret_data_get_secret (data, "my-identifier") == secret);

	g_object_unref (old);
	g_object_unref (data);
	g_object_unref (secret);
	g_object_unref (transaction);
}

static void
test_set_transacted_fail (void)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	GkmSecretData *data = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	GkmSecret *secret = gkm_secret_new_from_password ("barn");

	/* Transaction, but not complete */
	gkm_secret_data_set_transacted (data, transaction, "my-identifier", secret);
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (gkm_secret_data_get_secret (data, "my-identifier") == secret);

	/* Transaction fails here */
	gkm_transaction_fail (transaction, CKR_CANCEL);
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_failed (transaction));
	g_assert (gkm_secret_data_get_secret (data, "my-identifier") == NULL);

	g_object_unref (data);
	g_object_unref (secret);
	g_object_unref (transaction);
}

static void
test_set_transacted_fail_revert (void)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	GkmSecretData *data = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	GkmSecret *old = gkm_secret_new_from_password ("old");
	GkmSecret *secret = gkm_secret_new_from_password ("secret");

	/* The old secret */
	gkm_secret_data_set_secret (data, "my-identifier", old);
	g_assert (gkm_secret_data_get_secret (data, "my-identifier") == old);

	/* Transaction, but not complete */
	gkm_secret_data_set_transacted (data, transaction, "my-identifier", secret);
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (gkm_secret_data_get_secret (data, "my-identifier") == secret);

	/* Transaction fails here */
	gkm_transaction_fail (transaction, CKR_CANCEL);
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_failed (transaction));
	g_assert (gkm_secret_data_get_secret (data, "my-identifier") == old);

	g_object_unref (old);
	g_object_unref (data);
	g_object_unref (secret);
	g_object_unref (transaction);
}

static void
test_get_set_master (void)
{
	GkmSecretData *data = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	GkmSecret *master = gkm_secret_new_from_password ("master");
	GkmSecret *check;

	gkm_secret_data_set_master (data, master);
	g_object_unref (master);

	check = gkm_secret_data_get_master (data);
	g_assert (GKM_IS_SECRET (check));
	g_assert (master == check);
	g_assert (gkm_secret_equals (check, (guchar*)"master", -1));

	g_object_unref (data);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/secret-store/data/new", test_new);
	g_test_add_func ("/secret-store/data/get_set", test_get_set);
	g_test_add_func ("/secret-store/data/get_raw", test_get_raw);
	g_test_add_func ("/secret-store/data/remove", test_remove);
	g_test_add_func ("/secret-store/data/set_transacted", test_set_transacted);
	g_test_add_func ("/secret-store/data/set_transacted_replace", test_set_transacted_replace);
	g_test_add_func ("/secret-store/data/set_transacted_fail", test_set_transacted_fail);
	g_test_add_func ("/secret-store/data/set_transacted_fail_revert", test_set_transacted_fail_revert);
	g_test_add_func ("/secret-store/data/get_set_master", test_get_set_master);

	return g_test_run ();
}
