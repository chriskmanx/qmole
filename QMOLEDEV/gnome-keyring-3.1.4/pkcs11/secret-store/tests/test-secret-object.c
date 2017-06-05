/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/*
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

#include "mock-secret-module.h"

#include "secret-store/gkm-secret-object.h"

#include "gkm/gkm-transaction.h"

#include "pkcs11/pkcs11i.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	GkmModule *module;
	GkmSession *session;
	GkmSecretObject *object;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	test->module = test_secret_module_initialize_and_enter ();
	test->session = test_secret_module_open_session (TRUE);

	test->object = g_object_new (GKM_TYPE_SECRET_OBJECT,
	                       "module", test->module,
	                       "identifier", "my-identifier",
	                       NULL);

	g_assert (GKM_IS_SECRET_OBJECT (test->object));
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->object);
	test_secret_module_leave_and_finalize ();
}

static void
test_is_locked (Test *test, gconstpointer unused)
{
	/* Plain GkmSecretObject is never locked */
	g_assert (!gkm_secret_object_is_locked (test->object, test->session));
}

static void
test_identifier_prop (Test *test, gconstpointer unused)
{
	const gchar *identifier;
	identifier = gkm_secret_object_get_identifier (test->object);
	g_assert_cmpstr (identifier, ==, "my-identifier");
}

static void
was_notified (GObject *obj, GParamSpec *pspec, gpointer user_data)
{
	gboolean* notified = user_data;
	g_assert (user_data);
	*notified = TRUE;
}

static void
test_created_prop (Test *test, gconstpointer unused)
{
	glong created;

	/* Monitor for changes */
	gboolean notified = FALSE;
	g_signal_connect (test->object, "notify::created", G_CALLBACK (was_notified), &notified);

	/* Default value */
	created = gkm_secret_object_get_created (test->object);
	g_assert (created == 0);

	/* Set a new value */
	gkm_secret_object_set_created (test->object, 1247930171);
	g_assert (notified);
	created = gkm_secret_object_get_created (test->object);
	g_assert (created == 1247930171);
}

static void
test_modified_prop (Test *test, gconstpointer unused)
{
	glong modified;

	/* Monitor for changes */
	gboolean notified = FALSE;
	g_signal_connect (test->object, "notify::modified", G_CALLBACK (was_notified), &notified);

	/* Default value */
	modified = gkm_secret_object_get_modified (test->object);
	g_assert (modified == 0);

	/* Set a new value */
	gkm_secret_object_set_modified (test->object, 1247930171);
	g_assert (notified);
	modified = gkm_secret_object_get_modified (test->object);
	g_assert (modified == 1247930171);
}

static void
test_was_modified (Test *test, gconstpointer unused)
{
	GTimeVal tv;
	g_get_current_time (&tv);
	gkm_secret_object_was_modified (test->object);
	g_assert (tv.tv_sec == gkm_secret_object_get_modified (test->object));
}

static void
test_label_prop (Test *test, gconstpointer unused)
{
	const gchar *label;

	/* Monitor for changes */
	gboolean notified = FALSE;
	g_signal_connect (test->object, "notify::label", G_CALLBACK (was_notified), &notified);

	/* Default value */
	label = gkm_secret_object_get_label (test->object);
	g_assert_cmpstr (label, ==, "");

	/* Set a new value */
	gkm_secret_object_set_label (test->object, "hello");
	g_assert (notified);
	label = gkm_secret_object_get_label (test->object);
	g_assert_cmpstr (label, ==, "hello");
}

static void
test_identifier_get_attr (Test *test, gconstpointer unused)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_ID, buffer, 32 };
	CK_RV rv;

	rv = gkm_object_get_attribute (GKM_OBJECT (test->object), test->session, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 13);
	g_assert (memcmp (buffer, "my-identifier", 13) == 0);
}

static void
test_label_get_attr (Test *test, gconstpointer unused)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_LABEL, buffer, 32 };
	CK_RV rv;

	gkm_secret_object_set_label (test->object, "hello");
	rv = gkm_object_get_attribute (GKM_OBJECT (test->object), test->session, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 5);
	g_assert (memcmp (buffer, "hello", 5) == 0);
}

static void
test_label_set_attr (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr = { CKA_LABEL, "hello", 5 };
	GkmTransaction *transaction = gkm_transaction_new ();

	/* Monitor for changes */
	gboolean notified = FALSE;
	g_signal_connect (test->object, "notify::label", G_CALLBACK (was_notified), &notified);

	gkm_object_set_attribute (GKM_OBJECT (test->object), test->session, transaction, &attr);
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (!notified); /* Not notified yet */

	g_assert_cmpstr (gkm_secret_object_get_label (test->object), ==, "hello");

	gkm_transaction_complete (transaction);
	g_assert_cmpstr (gkm_secret_object_get_label (test->object), ==, "hello");
	g_assert (notified); /* Notified after transaction complete */

	g_object_unref (transaction);
}

static void
test_label_set_attr_fail (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr = { CKA_LABEL, "hello", 5 };
	GkmTransaction *transaction = gkm_transaction_new ();
	gboolean notified = FALSE;

	/* Set an old value */
	gkm_secret_object_set_label (test->object, "old");

	/* Monitor for changes */
	g_signal_connect (test->object, "notify::label", G_CALLBACK (was_notified), &notified);

	/* Set a new value */
	gkm_object_set_attribute (GKM_OBJECT (test->object), test->session, transaction, &attr);
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (!notified); /* Not notified yet */

	/* Temporarily has new value */
	g_assert_cmpstr (gkm_secret_object_get_label (test->object), ==, "hello");

	/* Fail and complete transaction */
	gkm_transaction_fail (transaction, CKR_CANCEL);
	gkm_transaction_complete (transaction);

	/* Back to old value */
	g_assert_cmpstr (gkm_secret_object_get_label (test->object), ==, "old");
	g_assert (!notified); /* Should never have notified */

	g_object_unref (transaction);
}

static void
test_modified_get_attr (Test *test, gconstpointer unused)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_G_MODIFIED, buffer, 32 };
	CK_RV rv;

	gkm_secret_object_set_modified (test->object, 1247930171);
	rv = gkm_object_get_attribute (GKM_OBJECT (test->object), test->session, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 16);
	g_assert (memcmp (buffer, "2009071815161100", 16) == 0);
}

static void
test_created_get_attr (Test *test, gconstpointer unused)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_G_CREATED, buffer, 32 };
	CK_RV rv;

	gkm_secret_object_set_created (test->object, 1247930171);
	rv = gkm_object_get_attribute (GKM_OBJECT (test->object), test->session, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 16);
	g_assert (memcmp (buffer, "2009071815161100", 16) == 0);
}

static void
test_locked_get_attr (Test *test, gconstpointer unused)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_G_LOCKED, buffer, 32 };
	CK_RV rv;

	rv = gkm_object_get_attribute (GKM_OBJECT (test->object), test->session, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 1);
	g_assert (memcmp (buffer, "\0", 1) == 0);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/secret-store/object/is_locked", Test, NULL, setup, test_is_locked, teardown);
	g_test_add ("/secret-store/object/identifier_prop", Test, NULL, setup, test_identifier_prop, teardown);
	g_test_add ("/secret-store/object/created_prop", Test, NULL, setup, test_created_prop, teardown);
	g_test_add ("/secret-store/object/modified_prop", Test, NULL, setup, test_modified_prop, teardown);
	g_test_add ("/secret-store/object/was_modified", Test, NULL, setup, test_was_modified, teardown);
	g_test_add ("/secret-store/object/label_prop", Test, NULL, setup, test_label_prop, teardown);
	g_test_add ("/secret-store/object/identifier_get_attr", Test, NULL, setup, test_identifier_get_attr, teardown);
	g_test_add ("/secret-store/object/label_get_attr", Test, NULL, setup, test_label_get_attr, teardown);
	g_test_add ("/secret-store/object/label_set_attr", Test, NULL, setup, test_label_set_attr, teardown);
	g_test_add ("/secret-store/object/label_set_attr_fail", Test, NULL, setup, test_label_set_attr_fail, teardown);
	g_test_add ("/secret-store/object/modified_get_attr", Test, NULL, setup, test_modified_get_attr, teardown);
	g_test_add ("/secret-store/object/created_get_attr", Test, NULL, setup, test_created_get_attr, teardown);
	g_test_add ("/secret-store/object/locked_get_attr", Test, NULL, setup, test_locked_get_attr, teardown);

	return g_test_run ();
}
