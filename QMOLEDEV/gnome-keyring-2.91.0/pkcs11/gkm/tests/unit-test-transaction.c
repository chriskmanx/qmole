/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-store.c: Test general store functionality

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

#include "test-suite.h"

#include "gkm/gkm-transaction.h"

DEFINE_TEST(transaction_empty)
{
	GkmTransaction *transaction;
	gboolean completed, failed;
	CK_RV result;

	transaction = gkm_transaction_new ();
	g_assert (GKM_IS_TRANSACTION (transaction));

	g_assert (gkm_transaction_get_failed (transaction) == FALSE);
	g_assert (gkm_transaction_get_completed (transaction) == FALSE);
	g_assert (gkm_transaction_get_result (transaction) == CKR_OK);

	gkm_transaction_complete (transaction);

	/* Make sure values are actually set */
	result = (CK_RV)-1;
	completed = failed = FALSE;

	g_object_get (transaction, "completed", &completed, "failed", &failed, "result", &result, NULL);
	g_assert (result == CKR_OK);
	g_assert (completed == TRUE);
	g_assert (failed == FALSE);

	g_object_unref (transaction);
}

DEFINE_TEST(transaction_fail)
{
	GkmTransaction *transaction;

	transaction = gkm_transaction_new ();

	gkm_transaction_fail (transaction, CKR_ARGUMENTS_BAD);

	g_assert (gkm_transaction_get_failed (transaction) == TRUE);
	g_assert (gkm_transaction_get_completed (transaction) == FALSE);
	g_assert (gkm_transaction_get_result (transaction) == CKR_ARGUMENTS_BAD);

	gkm_transaction_complete (transaction);

	g_assert (gkm_transaction_get_failed (transaction) == TRUE);
	g_assert (gkm_transaction_get_completed (transaction) == TRUE);
	g_assert (gkm_transaction_get_result (transaction) == CKR_ARGUMENTS_BAD);

	g_object_unref (transaction);
}


static gboolean
completed_signal (GkmTransaction *transaction, gpointer data)
{
	g_assert (GKM_IS_TRANSACTION (transaction));
	g_assert (data);

	*((guint*)data) = TRUE;
	return TRUE;
}

static gboolean
completed_callback (GkmTransaction *transaction, GObject *object, gpointer data)
{
	g_assert (GKM_IS_TRANSACTION (transaction));
	g_assert (data);

	/* In this case we set the object to the transaction for fun */
	g_assert (GKM_IS_TRANSACTION (transaction));
	g_assert (transaction == GKM_TRANSACTION (object));

	*((guint*)data) = gkm_transaction_get_failed (transaction);
	return TRUE;
}

DEFINE_TEST(transaction_signals_success)
{
	GkmTransaction *transaction = gkm_transaction_new ();

	/* Initialize with some invalid values */
	guint completed = 3;
	guint failed = 3;

	g_signal_connect (transaction, "complete", G_CALLBACK (completed_signal), &completed);
	gkm_transaction_add (transaction, transaction, completed_callback, &failed);

	/* No callbacks called yet */
	g_assert (completed == 3);
	g_assert (failed == 3);

	gkm_transaction_complete (transaction);

	g_assert (completed == TRUE);
	g_assert (failed == FALSE);

	g_object_unref (transaction);
}

DEFINE_TEST(transaction_signals_failure)
{
	GkmTransaction *transaction = gkm_transaction_new ();

	/* Initialize with some invalid values */
	guint completed = 3;
	guint failed = 3;

	g_signal_connect (transaction, "complete", G_CALLBACK (completed_signal), &completed);
	gkm_transaction_add (transaction, transaction, completed_callback, &failed);

	gkm_transaction_fail (transaction, CKR_ARGUMENTS_BAD);

	/* No callbacks called yet */
	g_assert (completed == 3);
	g_assert (failed == 3);

	gkm_transaction_complete (transaction);

	g_assert (completed == TRUE);
	g_assert (failed == TRUE);

	g_object_unref (transaction);
}

static guint order_value = 3;

static gboolean
order_callback (GkmTransaction *transaction, GObject *object, gpointer data)
{
	g_assert (GKM_IS_TRANSACTION (transaction));
	g_assert (data);
	g_assert (GPOINTER_TO_UINT (data) == order_value);
	--order_value;
	return TRUE;
}

DEFINE_TEST(transaction_order_is_reverse)
{
	GkmTransaction *transaction = gkm_transaction_new ();

	order_value = 3;
	gkm_transaction_add (transaction, transaction, order_callback, GUINT_TO_POINTER (1));
	gkm_transaction_add (transaction, transaction, order_callback, GUINT_TO_POINTER (2));
	gkm_transaction_add (transaction, transaction, order_callback, GUINT_TO_POINTER (3));

	gkm_transaction_complete (transaction);
	g_object_unref (transaction);
}

DEFINE_TEST(transaction_dispose_completes)
{
	GkmTransaction *transaction = gkm_transaction_new ();

	/* Initialize with some invalid values */
	guint completed = 3;

	g_signal_connect (transaction, "complete", G_CALLBACK (completed_signal), &completed);

	g_object_run_dispose (G_OBJECT (transaction));

	g_assert (completed == TRUE);

	g_object_unref (transaction);
}

DEFINE_TEST(remove_file_success)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	gchar *filename = testing_scratch_filename ("remove-file");

	g_assert (g_file_set_contents (filename, "xxx", 3, NULL));
	g_assert (g_file_test (filename, G_FILE_TEST_IS_REGULAR));

	gkm_transaction_remove_file (transaction, filename);
	g_assert (!gkm_transaction_get_failed (transaction));

	g_assert (!g_file_test (filename, G_FILE_TEST_IS_REGULAR));

	gkm_transaction_complete (transaction);
	g_assert (!g_file_test (filename, G_FILE_TEST_IS_REGULAR));

	g_object_unref (transaction);
	g_free (filename);
}

DEFINE_TEST(remove_file_abort)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	gchar *filename = testing_scratch_filename ("remove-file");
	gchar *data;
	gsize n_data;

	g_assert (g_file_set_contents (filename, "xxx", 3, NULL));
	g_assert (g_file_test (filename, G_FILE_TEST_IS_REGULAR));

	gkm_transaction_remove_file (transaction, filename);
	g_assert (!gkm_transaction_get_failed (transaction));

	g_assert (!g_file_test (filename, G_FILE_TEST_IS_REGULAR));

	/* Fail the transaction */
	gkm_transaction_fail (transaction, CKR_FUNCTION_FAILED);

	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_failed (transaction));
	g_assert (g_file_test (filename, G_FILE_TEST_IS_REGULAR));

	g_assert (g_file_get_contents (filename, &data, &n_data, NULL));
	g_assert_cmpuint (n_data, ==, 3);
	g_assert_cmpstr (data, ==, "xxx");
	g_free (data);

	g_unlink (filename);
	g_object_unref (transaction);
	g_free (filename);
}

DEFINE_TEST(remove_file_non_exist)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	gchar *filename = testing_scratch_filename ("remove-non-existant");

	g_unlink (filename);

	/* Should succeed even though not exist */
	gkm_transaction_remove_file (transaction, filename);
	g_assert (!gkm_transaction_get_failed (transaction));

	gkm_transaction_complete (transaction);
	g_object_unref (transaction);
	g_free (filename);
}

DEFINE_TEST(write_file)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	gchar *filename = testing_scratch_filename ("write-test");
	gchar *data;
	gsize n_data;

	gkm_transaction_write_file (transaction, filename, (const guchar*)"value", 5);
	g_assert (!gkm_transaction_get_failed (transaction));

	g_assert (g_file_get_contents (filename, &data, &n_data, NULL));
	g_assert_cmpuint (n_data, ==, 5);
	g_assert_cmpstr (data, ==, "value");
	g_free (data);

	gkm_transaction_complete (transaction);

	g_assert (g_file_get_contents (filename, &data, &n_data, NULL));
	g_assert_cmpuint (n_data, ==, 5);
	g_assert_cmpstr (data, ==, "value");
	g_free (data);

	g_object_unref (transaction);
	g_free (filename);
}

DEFINE_TEST(write_file_abort_gone)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	gchar *filename = testing_scratch_filename ("write-test");
	gchar *data;
	gsize n_data;

	g_unlink (filename);

	gkm_transaction_write_file (transaction, filename, (const guchar*)"value", 5);
	g_assert (!gkm_transaction_get_failed (transaction));

	g_assert (g_file_get_contents (filename, &data, &n_data, NULL));
	g_assert_cmpuint (n_data, ==, 5);
	g_assert_cmpstr (data, ==, "value");
	g_free (data);

	gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);
	gkm_transaction_complete (transaction);

	g_assert (!g_file_test (filename, G_FILE_TEST_IS_REGULAR));

	g_object_unref (transaction);
	g_free (filename);
}

DEFINE_TEST(write_file_abort_revert)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	gchar *filename = testing_scratch_filename ("write-test");
	gchar *data;

	g_assert (g_file_set_contents (filename, "my original", -1, NULL));

	gkm_transaction_write_file (transaction, filename, (const guchar*)"new value", 9);
	g_assert (!gkm_transaction_get_failed (transaction));

	g_assert (g_file_get_contents (filename, &data, NULL, NULL));
	g_assert_cmpstr (data, ==, "new value");
	g_free (data);

	gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);
	gkm_transaction_complete (transaction);

	g_assert (g_file_get_contents (filename, &data, NULL, NULL));
	g_assert_cmpstr (data, ==, "my original");
	g_free (data);

	g_object_unref (transaction);
	g_free (filename);
}
