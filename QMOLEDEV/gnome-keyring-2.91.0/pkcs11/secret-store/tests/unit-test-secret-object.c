/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-secret-collection.c: Test the collection keyring

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

#include "test-suite.h"
#include "test-secret-module.h"

#include "gkm-secret-object.h"

#include "gkm/gkm-transaction.h"

#include "pkcs11/pkcs11i.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static GkmModule *module = NULL;
static GkmSession *session = NULL;
static GkmSecretObject *object = NULL;

DEFINE_SETUP(secret_object)
{
	module = test_secret_module_initialize_and_enter ();
	session = test_secret_module_open_session (TRUE);

	object = g_object_new (GKM_TYPE_SECRET_OBJECT,
	                       "module", module,
	                       "identifier", "my-identifier",
	                       NULL);

	g_assert (GKM_IS_SECRET_OBJECT (object));
}

DEFINE_TEARDOWN(secret_object)
{
	g_object_unref (object);
	object = NULL;

	test_secret_module_leave_and_finalize ();
	module = NULL;
	session = NULL;
}

DEFINE_TEST(secret_object_is_locked)
{
	/* Plain GkmSecretObject is never locked */
	g_assert (!gkm_secret_object_is_locked (object, session));
}

DEFINE_TEST(secret_object_identifier_prop)
{
	const gchar *identifier;
	identifier = gkm_secret_object_get_identifier (object);
	g_assert_cmpstr (identifier, ==, "my-identifier");
}

static void
was_notified (GObject *obj, GParamSpec *pspec, gpointer user_data)
{
	gboolean* notified = user_data;
	g_assert (GKM_SECRET_OBJECT (obj) == object);
	g_assert (user_data);
	*notified = TRUE;
}

DEFINE_TEST(secret_object_created_prop)
{
	glong created;

	/* Monitor for changes */
	gboolean notified = FALSE;
	g_signal_connect (object, "notify::created", G_CALLBACK (was_notified), &notified);

	/* Default value */
	created = gkm_secret_object_get_created (object);
	g_assert (created == 0);

	/* Set a new value */
	gkm_secret_object_set_created (object, 1247930171);
	g_assert (notified);
	created = gkm_secret_object_get_created (object);
	g_assert (created == 1247930171);
}

DEFINE_TEST(secret_object_modified_prop)
{
	glong modified;

	/* Monitor for changes */
	gboolean notified = FALSE;
	g_signal_connect (object, "notify::modified", G_CALLBACK (was_notified), &notified);

	/* Default value */
	modified = gkm_secret_object_get_modified (object);
	g_assert (modified == 0);

	/* Set a new value */
	gkm_secret_object_set_modified (object, 1247930171);
	g_assert (notified);
	modified = gkm_secret_object_get_modified (object);
	g_assert (modified == 1247930171);
}

DEFINE_TEST(secret_object_was_modified)
{
	GTimeVal tv;
	g_get_current_time (&tv);
	gkm_secret_object_was_modified (object);
	g_assert (tv.tv_sec == gkm_secret_object_get_modified (object));
}

DEFINE_TEST(secret_object_label_prop)
{
	const gchar *label;

	/* Monitor for changes */
	gboolean notified = FALSE;
	g_signal_connect (object, "notify::label", G_CALLBACK (was_notified), &notified);

	/* Default value */
	label = gkm_secret_object_get_label (object);
	g_assert_cmpstr (label, ==, "");

	/* Set a new value */
	gkm_secret_object_set_label (object, "hello");
	g_assert (notified);
	label = gkm_secret_object_get_label (object);
	g_assert_cmpstr (label, ==, "hello");
}

DEFINE_TEST(secret_object_identifier_get_attr)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_ID, buffer, 32 };
	CK_RV rv;

	rv = gkm_object_get_attribute (GKM_OBJECT (object), session, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 13);
	g_assert (memcmp (buffer, "my-identifier", 13) == 0);
}

DEFINE_TEST(secret_object_label_get_attr)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_LABEL, buffer, 32 };
	CK_RV rv;

	gkm_secret_object_set_label (object, "hello");
	rv = gkm_object_get_attribute (GKM_OBJECT (object), session, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 5);
	g_assert (memcmp (buffer, "hello", 5) == 0);
}

DEFINE_TEST(secret_object_label_set_attr)
{
	CK_ATTRIBUTE attr = { CKA_LABEL, "hello", 5 };
	GkmTransaction *transaction = gkm_transaction_new ();

	/* Monitor for changes */
	gboolean notified = FALSE;
	g_signal_connect (object, "notify::label", G_CALLBACK (was_notified), &notified);

	gkm_object_set_attribute (GKM_OBJECT (object), session, transaction, &attr);
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (!notified); /* Not notified yet */

	g_assert_cmpstr (gkm_secret_object_get_label (object), ==, "hello");

	gkm_transaction_complete (transaction);
	g_assert_cmpstr (gkm_secret_object_get_label (object), ==, "hello");
	g_assert (notified); /* Notified after transaction complete */

	g_object_unref (transaction);
}

DEFINE_TEST(secret_object_label_set_attr_fail)
{
	CK_ATTRIBUTE attr = { CKA_LABEL, "hello", 5 };
	GkmTransaction *transaction = gkm_transaction_new ();
	gboolean notified = FALSE;

	/* Set an old value */
	gkm_secret_object_set_label (object, "old");

	/* Monitor for changes */
	g_signal_connect (object, "notify::label", G_CALLBACK (was_notified), &notified);

	/* Set a new value */
	gkm_object_set_attribute (GKM_OBJECT (object), session, transaction, &attr);
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (!notified); /* Not notified yet */

	/* Temporarily has new value */
	g_assert_cmpstr (gkm_secret_object_get_label (object), ==, "hello");

	/* Fail and complete transaction */
	gkm_transaction_fail (transaction, CKR_CANCEL);
	gkm_transaction_complete (transaction);

	/* Back to old value */
	g_assert_cmpstr (gkm_secret_object_get_label (object), ==, "old");
	g_assert (!notified); /* Should never have notified */

	g_object_unref (transaction);
}

DEFINE_TEST(secret_object_modified_get_attr)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_G_MODIFIED, buffer, 32 };
	CK_RV rv;

	gkm_secret_object_set_modified (object, 1247930171);
	rv = gkm_object_get_attribute (GKM_OBJECT (object), session, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 16);
	g_assert (memcmp (buffer, "2009071815161100", 16) == 0);
}

DEFINE_TEST(secret_object_created_get_attr)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_G_CREATED, buffer, 32 };
	CK_RV rv;

	gkm_secret_object_set_created (object, 1247930171);
	rv = gkm_object_get_attribute (GKM_OBJECT (object), session, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 16);
	g_assert (memcmp (buffer, "2009071815161100", 16) == 0);
}

DEFINE_TEST(secret_object_locked_get_attr)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_G_LOCKED, buffer, 32 };
	CK_RV rv;

	rv = gkm_object_get_attribute (GKM_OBJECT (object), session, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 1);
	g_assert (memcmp (buffer, "\0", 1) == 0);
}
