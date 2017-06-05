/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-transaction.c: Test transaction functionality

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

#include "gkm/gkm-transaction.h"

#include <glib/gstdio.h>

typedef struct {
	int unused;
} Test;

static void
setup (Test* test, gconstpointer unused)
{
	GDir *dir;
	const gchar *directory;
	const gchar *basename;
	gchar *filename;

	directory = "/tmp";
	dir = g_dir_open (directory, 0, NULL);
	g_assert (dir);

	for (;;) {
		basename = g_dir_read_name (dir);
		if (basename == NULL)
			break;
		if (g_str_has_prefix (basename, "transaction-")) {
			filename = g_build_filename (directory, basename, NULL);
			g_unlink (filename);
			g_free (filename);
		}
	}

	g_dir_close (dir);
}

static void
teardown (Test *test, gconstpointer unused)
{

}

static void
test_transaction_empty (Test* test, gconstpointer unused)
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

static void
test_transaction_fail (Test* test, gconstpointer unused)
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

static void
test_transaction_signals_success (Test* test, gconstpointer unused)
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

static void
test_transaction_signals_failure (Test* test, gconstpointer unused)
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

static void
test_transaction_order_is_reverse (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();

	order_value = 3;
	gkm_transaction_add (transaction, transaction, order_callback, GUINT_TO_POINTER (1));
	gkm_transaction_add (transaction, transaction, order_callback, GUINT_TO_POINTER (2));
	gkm_transaction_add (transaction, transaction, order_callback, GUINT_TO_POINTER (3));

	gkm_transaction_complete (transaction);
	g_object_unref (transaction);
}

static void
test_transaction_dispose_completes (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();

	/* Initialize with some invalid values */
	guint completed = 3;

	g_signal_connect (transaction, "complete", G_CALLBACK (completed_signal), &completed);

	g_object_run_dispose (G_OBJECT (transaction));

	g_assert (completed == TRUE);

	g_object_unref (transaction);
}

static void
test_remove_file_success (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	const gchar *filename = "/tmp/transaction-remove";

	g_assert (g_file_set_contents (filename, "xxx", 3, NULL));
	g_assert (g_file_test (filename, G_FILE_TEST_IS_REGULAR));

	gkm_transaction_remove_file (transaction, filename);
	g_assert (!gkm_transaction_get_failed (transaction));

	g_assert (!g_file_test (filename, G_FILE_TEST_IS_REGULAR));

	gkm_transaction_complete (transaction);
	g_assert (!g_file_test (filename, G_FILE_TEST_IS_REGULAR));

	g_object_unref (transaction);
}

static void
test_remove_file_abort (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	const gchar *filename = "/tmp/transaction-remove";
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
}

static void
test_remove_file_non_exist (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	const gchar *filename = "/tmp/transaction-non-existant";

	g_unlink (filename);

	/* Should succeed even though not exist */
	gkm_transaction_remove_file (transaction, filename);
	g_assert (!gkm_transaction_get_failed (transaction));

	gkm_transaction_complete (transaction);
	g_object_unref (transaction);
}

static void
test_write_file (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	const gchar *filename = "/tmp/transaction-test";
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
}

static void
test_write_file_abort_gone (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	const gchar *filename = "/tmp/transaction-test";
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
}

static void
test_write_file_abort_revert (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	const gchar *filename = "/tmp/transaction-test";
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
}

static void
test_unique_file_conflict (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	const gchar *filename = "/tmp/transaction-test";
	gchar *dirname;
	gchar *basename;
	gchar *result;

	dirname = g_path_get_dirname (filename);
	basename = g_path_get_basename (filename);

	g_assert (g_file_set_contents (filename, "data", -1, NULL));

	result = gkm_transaction_unique_file (transaction, dirname, basename);
	g_assert (!gkm_transaction_get_failed (transaction));

	g_assert (result);
	g_assert_cmpstr (result, !=, basename);
	g_assert_cmpstr (result, ==, "transaction-test_1");

	g_free (dirname);
	g_free (basename);
	g_free (result);

	g_object_unref (transaction);
}

static void
test_unique_file_conflict_with_ext (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	const gchar *filename = "/tmp/transaction-test.ext";
	gchar *dirname;
	gchar *basename;
	gchar *result;

	dirname = g_path_get_dirname (filename);
	basename = g_path_get_basename (filename);

	g_assert (g_file_set_contents (filename, "data", -1, NULL));

	result = gkm_transaction_unique_file (transaction, dirname, basename);
	g_assert (!gkm_transaction_get_failed (transaction));

	g_assert (result);
	g_assert_cmpstr (result, !=, basename);
	g_assert_cmpstr (result, ==, "transaction-test_1.ext");

	g_free (dirname);
	g_free (basename);
	g_free (result);

	g_object_unref (transaction);
}

static void
test_unique_file_no_conflict (Test* test, gconstpointer unused)
{
	GkmTransaction *transaction = gkm_transaction_new ();
	const gchar *dirname = "/tmp";
	gchar *result;

	result = gkm_transaction_unique_file (transaction, dirname, "transaction-another");
	g_assert (!gkm_transaction_get_failed (transaction));

	g_assert (result);
	g_assert_cmpstr (result, ==, "transaction-another");

	g_free (result);

	g_object_unref (transaction);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gkm/transaction/transaction_empty", Test, NULL, setup, test_transaction_empty, teardown);
	g_test_add ("/gkm/transaction/transaction_fail", Test, NULL, setup, test_transaction_fail, teardown);
	g_test_add ("/gkm/transaction/transaction_signals_success", Test, NULL, setup, test_transaction_signals_success, teardown);
	g_test_add ("/gkm/transaction/transaction_signals_failure", Test, NULL, setup, test_transaction_signals_failure, teardown);
	g_test_add ("/gkm/transaction/transaction_order_is_reverse", Test, NULL, setup, test_transaction_order_is_reverse, teardown);
	g_test_add ("/gkm/transaction/transaction_dispose_completes", Test, NULL, setup, test_transaction_dispose_completes, teardown);
	g_test_add ("/gkm/transaction/remove_file_success", Test, NULL, setup, test_remove_file_success, teardown);
	g_test_add ("/gkm/transaction/remove_file_abort", Test, NULL, setup, test_remove_file_abort, teardown);
	g_test_add ("/gkm/transaction/remove_file_non_exist", Test, NULL, setup, test_remove_file_non_exist, teardown);
	g_test_add ("/gkm/transaction/write_file", Test, NULL, setup, test_write_file, teardown);
	g_test_add ("/gkm/transaction/write_file_abort_gone", Test, NULL, setup, test_write_file_abort_gone, teardown);
	g_test_add ("/gkm/transaction/write_file_abort_revert", Test, NULL, setup, test_write_file_abort_revert, teardown);
	g_test_add ("/gkm/transaction/unique_file_conflict", Test, NULL, setup, test_unique_file_conflict, teardown);
	g_test_add ("/gkm/transaction/unique_file_conflict_with_ext", Test, NULL, setup, test_unique_file_conflict_with_ext, teardown);
	g_test_add ("/gkm/transaction/unique_file_no_conflict", Test, NULL, setup, test_unique_file_no_conflict, teardown);

	return g_test_run ();
}
