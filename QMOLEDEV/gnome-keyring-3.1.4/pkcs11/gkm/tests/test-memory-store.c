/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-memory-test->store.c: Test memory test->store functionality

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "mock-module.h"

#include "egg/egg-secure-memory.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-memory-store.h"
#include "gkm/gkm-transaction.h"

typedef struct {
	GkmModule *module;
	GkmStore *store;
	GkmObject *object;
	GkmTransaction *transaction;
	guchar buffer[1024];
} Test;

static CK_RV
check_validator (GkmObject *obj, CK_ATTRIBUTE_PTR attr)
{
	const gchar *data;
	guint i;

	g_assert (attr);
	g_assert (attr->type == CKA_LABEL);

	/* Test that the whole string is ascii and lower case */
	data = attr->pValue;
	for (i = 0; i < attr->ulValueLen; ++i) {
		if (!g_ascii_isprint(data[i]) || !g_ascii_islower (data[i]))
			return CKR_ATTRIBUTE_VALUE_INVALID;
	}

	return CKR_OK;
}

static void
setup (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	CK_ULONG twentyfour = 24;

	test->module = mock_module_initialize_and_enter ();

	attr.type = CKA_LABEL;
	attr.pValue = "label";
	attr.ulValueLen = 5;

	test->store = GKM_STORE (gkm_memory_store_new ());

	gkm_store_register_schema (test->store, &attr, check_validator, 0);
	g_assert (gkm_store_lookup_schema (test->store, CKA_LABEL, NULL));

	attr.type = CKA_VALUE;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	gkm_store_register_schema (test->store, &attr, NULL, GKM_STORE_IS_SENSITIVE);

	attr.type = CKA_BITS_PER_PIXEL;
	attr.pValue = &twentyfour;
	attr.ulValueLen = sizeof (twentyfour);

	gkm_store_register_schema (test->store, &attr, NULL, GKM_STORE_IS_INTERNAL);

	test->object = g_object_new (GKM_TYPE_OBJECT, "module", test->module, NULL);

	test->transaction = gkm_transaction_new ();
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->store);
	test->store = NULL;

	g_object_unref (test->transaction);
	test->transaction = NULL;

	if (test->object != NULL)
		g_object_unref (test->object);
	test->object = NULL;

	mock_module_leave_and_finalize ();
	test->module = NULL;
}

static void
test_get_attribute_default (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_LABEL;
	attr.pValue = NULL;
	rv = gkm_store_get_attribute (test->store, test->object, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 5);
	attr.pValue = test->buffer;
	rv = gkm_store_get_attribute (test->store, test->object, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 5);
	g_assert (memcmp (attr.pValue, "label", 5) == 0);
}

static void
test_read_value_default (Test *test, gconstpointer unused)
{
	gconstpointer value;
	gsize n_value;

	value = gkm_store_read_value (test->store, test->object, CKA_LABEL, &n_value);
	g_assert (value);
	g_assert (n_value == 5);
	g_assert (memcmp (value, "label", 5) == 0);

	value = gkm_store_read_value (test->store, test->object, CKA_BITS_PER_PIXEL, &n_value);
	g_assert (value);
	g_assert (n_value == sizeof (CK_ULONG));
	g_assert (*((CK_ULONG_PTR)value) == 24);
}

static void
test_read_string (Test *test, gconstpointer unused)
{
	gchar *str;

	str = gkm_store_read_string (test->store, test->object, CKA_LABEL);
	g_assert_cmpstr (str, ==, "label");
	g_free (str);
}

static void
test_get_invalid (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_APPLICATION;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	rv = gkm_store_get_attribute (test->store, test->object, &attr);
	g_assert (rv == CKR_ATTRIBUTE_TYPE_INVALID);
}

static void
test_get_sensitive (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_VALUE;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	rv = gkm_store_get_attribute (test->store, test->object, &attr);
	g_assert (rv == CKR_ATTRIBUTE_SENSITIVE);
}

static void
test_get_internal (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_BITS_PER_PIXEL;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	rv = gkm_store_get_attribute (test->store, test->object, &attr);
	g_assert (rv == CKR_ATTRIBUTE_TYPE_INVALID);
}

static void
test_set_invalid (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;

	attr.type = CKA_APPLICATION;
	attr.pValue = "me";
	attr.ulValueLen = 2;

	gkm_store_set_attribute (test->store, test->transaction, test->object, &attr);
	g_assert (gkm_transaction_get_result (test->transaction) == CKR_ATTRIBUTE_TYPE_INVALID);
}

static void
test_set_internal (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	CK_ULONG five = 5;

	attr.type = CKA_BITS_PER_PIXEL;
	attr.pValue = &five;
	attr.ulValueLen = sizeof (five);

	gkm_store_set_attribute (test->store, test->transaction, test->object, &attr);
	g_assert (gkm_transaction_get_result (test->transaction) == CKR_ATTRIBUTE_TYPE_INVALID);
}

static void
test_set_get_attribute (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_LABEL;
	attr.pValue = "booyah";
	attr.ulValueLen = 6;

	gkm_store_set_attribute (test->store, test->transaction, test->object, &attr);

	gkm_transaction_complete (test->transaction);
	g_assert (gkm_transaction_get_result (test->transaction) == CKR_OK);

	attr.pValue = test->buffer;
	attr.ulValueLen = 1024;
	rv = gkm_store_get_attribute (test->store, test->object, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 6);
	g_assert (memcmp (attr.pValue, "booyah", 6) == 0);
}

static void
test_write_read_value (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	CK_ULONG five = 5;
	gconstpointer value;
	gsize n_value;

	attr.type = CKA_BITS_PER_PIXEL;
	attr.pValue = &five;
	attr.ulValueLen = sizeof (five);

	gkm_store_write_value (test->store, test->transaction, test->object, &attr);

	gkm_transaction_complete (test->transaction);
	g_assert (gkm_transaction_get_result (test->transaction) == CKR_OK);

	value = gkm_store_read_value (test->store, test->object, CKA_BITS_PER_PIXEL, &n_value);
	g_assert (value);
	g_assert (n_value == sizeof (five));
	g_assert (memcmp (value, &five, sizeof (five)) == 0);
}

static void
test_set_no_validate (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;

	attr.type = CKA_LABEL;
	attr.pValue = "CAPITALS";
	attr.ulValueLen = 8;

	gkm_store_set_attribute (test->store, test->transaction, test->object, &attr);
	g_assert (gkm_transaction_get_failed (test->transaction));

	gkm_transaction_complete (test->transaction);
	g_assert (gkm_transaction_get_result (test->transaction) == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_set_transaction_default (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	gconstpointer value;
	gsize n_value;


	attr.type = CKA_LABEL;
	attr.pValue = "another";
	attr.ulValueLen = 7;

	/* Change the attribute */
	gkm_store_set_attribute (test->store, test->transaction, test->object, &attr);
	g_assert (gkm_transaction_get_failed (test->transaction) == FALSE);

	/* Should get new value */
	value = gkm_store_read_value (test->store, test->object, CKA_LABEL, &n_value);
	g_assert (value && n_value == attr.ulValueLen);
	g_assert (memcmp (attr.pValue, value, n_value) == 0);

	/* Fail for some arbitrary reason */
	gkm_transaction_fail (test->transaction, CKR_ATTRIBUTE_VALUE_INVALID);

	/* Value should not have changed yet */
	value = gkm_store_read_value (test->store, test->object, CKA_LABEL, &n_value);
	g_assert (value && n_value == attr.ulValueLen);
	g_assert (memcmp (attr.pValue, value, n_value) == 0);

	/* Now complete the test->transaction */
	gkm_transaction_complete (test->transaction);
	g_assert (gkm_transaction_get_failed (test->transaction) == TRUE);

	/* Value should now have changed, back to default */
	value = gkm_store_read_value (test->store, test->object, CKA_LABEL, &n_value);
	g_assert (value && n_value == 5);
	g_assert (memcmp (value, "label", 5) == 0);
}

static void
test_set_transaction_revert_first (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr, prev;
	gconstpointer value;
	gsize n_value;

	prev.type = CKA_LABEL;
	prev.pValue = "numberone";
	prev.ulValueLen = 9;

	/* Change the attribute */
	gkm_store_set_attribute (test->store, test->transaction, test->object, &prev);
	gkm_transaction_complete (test->transaction);
	g_assert (gkm_transaction_get_failed (test->transaction) == FALSE);

	/* Value should be new value */
	value = gkm_store_read_value (test->store, test->object, CKA_LABEL, &n_value);
	g_assert (value && n_value == prev.ulValueLen);
	g_assert (memcmp (prev.pValue, value, n_value) == 0);

	/* A new test->transaction */
	g_object_unref (test->transaction);
	test->transaction = gkm_transaction_new ();

	attr.type = CKA_LABEL;
	attr.pValue = "second";
	attr.ulValueLen = 6;

	gkm_store_set_attribute (test->store, test->transaction, test->object, &attr);
	g_assert (gkm_transaction_get_failed (test->transaction) == FALSE);

	/* Should get new value */
	value = gkm_store_read_value (test->store, test->object, CKA_LABEL, &n_value);
	g_assert (value && n_value == attr.ulValueLen);
	g_assert (memcmp (attr.pValue, value, n_value) == 0);

	attr.type = CKA_LABEL;
	attr.pValue = "third";
	attr.ulValueLen = 5;

	gkm_store_set_attribute (test->store, test->transaction, test->object, &attr);
	g_assert (gkm_transaction_get_failed (test->transaction) == FALSE);

	/* Should get new value */
	value = gkm_store_read_value (test->store, test->object, CKA_LABEL, &n_value);
	g_assert (value && n_value == attr.ulValueLen);
	g_assert (memcmp (attr.pValue, value, n_value) == 0);

	/* Fail for some arbitrary reason */
	gkm_transaction_fail (test->transaction, CKR_ATTRIBUTE_VALUE_INVALID);

	/* Value should not have changed yet */
	value = gkm_store_read_value (test->store, test->object, CKA_LABEL, &n_value);
	g_assert (value && n_value == attr.ulValueLen);
	g_assert (memcmp (attr.pValue, value, n_value) == 0);

	/* Now complete the test->transaction */
	gkm_transaction_complete (test->transaction);
	g_assert (gkm_transaction_get_failed (test->transaction) == TRUE);

	/* Value should now have changed, back to default */
	value = gkm_store_read_value (test->store, test->object, CKA_LABEL, &n_value);
	g_assert (value && n_value == prev.ulValueLen);
	g_assert (memcmp (prev.pValue, value, n_value) == 0);
}

static void
notify_attribute (GkmObject *obj, CK_ATTRIBUTE_TYPE type, gpointer data)
{
	g_assert (type == CKA_LABEL);
	g_assert (data);

	*((CK_ATTRIBUTE_TYPE*)data) = type;
}

static void
test_set_notifies (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	CK_ATTRIBUTE_TYPE type = 0;

	attr.type = CKA_LABEL;
	attr.pValue = "valid";
	attr.ulValueLen = 5;

	g_signal_connect (test->object, "notify-attribute", G_CALLBACK (notify_attribute), &type);

	gkm_store_set_attribute (test->store, test->transaction, test->object, &attr);

	/* We should have been notified that the attribute changed at this point */
	g_assert (type == CKA_LABEL);

	/* Reset for next notify */
	type = 0;

	/* Fail for some arbitrary reason */
	gkm_transaction_fail (test->transaction, CKR_ATTRIBUTE_VALUE_INVALID);

	/* We should not have been notified yet */
	g_assert (type == 0);

	/* Now complete the test->transaction */
	gkm_transaction_complete (test->transaction);
	g_assert (gkm_transaction_get_failed (test->transaction) == TRUE);

	/* Now we should have been notified that this changed back */
	g_assert (type == CKA_LABEL);
}

static void
test_set_object_gone_first (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;

	attr.type = CKA_LABEL;
	attr.pValue = "valid";
	attr.ulValueLen = 5;

	gkm_store_set_attribute (test->store, test->transaction, test->object, &attr);
	gkm_transaction_complete (test->transaction);
	g_assert (gkm_transaction_get_result (test->transaction) == CKR_OK);

	/* This tests memory test->store internal tracking */
	g_object_unref (test->object);
	test->object = NULL;
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gkm/memory-test->store/get_attribute_default", Test, NULL, setup, test_get_attribute_default, teardown);
	g_test_add ("/gkm/memory-test->store/read_value_default", Test, NULL, setup, test_read_value_default, teardown);
	g_test_add ("/gkm/memory-test->store/read_string", Test, NULL, setup, test_read_string, teardown);
	g_test_add ("/gkm/memory-test->store/get_invalid", Test, NULL, setup, test_get_invalid, teardown);
	g_test_add ("/gkm/memory-test->store/get_sensitive", Test, NULL, setup, test_get_sensitive, teardown);
	g_test_add ("/gkm/memory-test->store/get_internal", Test, NULL, setup, test_get_internal, teardown);
	g_test_add ("/gkm/memory-test->store/set_invalid", Test, NULL, setup, test_set_invalid, teardown);
	g_test_add ("/gkm/memory-test->store/set_internal", Test, NULL, setup, test_set_internal, teardown);
	g_test_add ("/gkm/memory-test->store/set_get_attribute", Test, NULL, setup, test_set_get_attribute, teardown);
	g_test_add ("/gkm/memory-test->store/write_read_value", Test, NULL, setup, test_write_read_value, teardown);
	g_test_add ("/gkm/memory-test->store/set_no_validate", Test, NULL, setup, test_set_no_validate, teardown);
	g_test_add ("/gkm/memory-test->store/set_transaction_default", Test, NULL, setup, test_set_transaction_default, teardown);
	g_test_add ("/gkm/memory-test->store/set_transaction_revert_first", Test, NULL, setup, test_set_transaction_revert_first, teardown);
	g_test_add ("/gkm/memory-test->store/set_notifies", Test, NULL, setup, test_set_notifies, teardown);
	g_test_add ("/gkm/memory-test->store/set_object_gone_first", Test, NULL, setup, test_set_object_gone_first, teardown);

	return g_test_run ();
}
