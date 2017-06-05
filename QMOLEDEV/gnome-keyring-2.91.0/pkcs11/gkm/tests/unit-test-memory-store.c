/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-memory-store.c: Test memory store functionality

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "test-suite.h"
#include "test-module.h"

#include "gkm/gkm-object.h"
#include "gkm/gkm-memory-store.h"
#include "gkm/gkm-transaction.h"

static GkmModule *module = NULL;
static GkmStore *store = NULL;
static GkmObject *object = NULL;
static GkmTransaction *transaction = NULL;
static guchar buffer[1024];

static CK_RV
test_validator (GkmObject *obj, CK_ATTRIBUTE_PTR attr)
{
	const gchar *data;
	guint i;

	g_assert (obj == object);
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

DEFINE_SETUP(memory_store)
{
	CK_ATTRIBUTE attr;
	CK_ULONG twentyfour = 24;

	module = test_module_initialize_and_enter ();

	attr.type = CKA_LABEL;
	attr.pValue = "label";
	attr.ulValueLen = 5;

	store = GKM_STORE (gkm_memory_store_new ());

	gkm_store_register_schema (store, &attr, test_validator, 0);
	g_assert (gkm_store_lookup_schema (store, CKA_LABEL, NULL));

	attr.type = CKA_VALUE;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	gkm_store_register_schema (store, &attr, NULL, GKM_STORE_IS_SENSITIVE);

	attr.type = CKA_BITS_PER_PIXEL;
	attr.pValue = &twentyfour;
	attr.ulValueLen = sizeof (twentyfour);

	gkm_store_register_schema (store, &attr, NULL, GKM_STORE_IS_INTERNAL);

	object = g_object_new (GKM_TYPE_OBJECT, "module", module, NULL);

	transaction = gkm_transaction_new ();
}

DEFINE_TEARDOWN(memory_store)
{
	g_object_unref (store);
	store = NULL;

	g_object_unref (transaction);
	transaction = NULL;

	if (object != NULL)
		g_object_unref (object);
	object = NULL;

	test_module_leave_and_finalize ();
	module = NULL;
}

DEFINE_TEST(get_attribute_default)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_LABEL;
	attr.pValue = NULL;
	rv = gkm_store_get_attribute (store, object, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 5);
	attr.pValue = buffer;
	rv = gkm_store_get_attribute (store, object, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 5);
	g_assert (memcmp (attr.pValue, "label", 5) == 0);
}

DEFINE_TEST(read_value_default)
{
	gconstpointer value;
	gsize n_value;

	value = gkm_store_read_value (store, object, CKA_LABEL, &n_value);
	g_assert (value);
	g_assert (n_value == 5);
	g_assert (memcmp (value, "label", 5) == 0);

	value = gkm_store_read_value (store, object, CKA_BITS_PER_PIXEL, &n_value);
	g_assert (value);
	g_assert (n_value == sizeof (CK_ULONG));
	g_assert (*((CK_ULONG_PTR)value) == 24);
}

DEFINE_TEST(read_string)
{
	gchar *str;

	str = gkm_store_read_string (store, object, CKA_LABEL);
	g_assert_cmpstr (str, ==, "label");
	g_free (str);
}

DEFINE_TEST(get_invalid)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_APPLICATION;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	rv = gkm_store_get_attribute (store, object, &attr);
	g_assert (rv == CKR_ATTRIBUTE_TYPE_INVALID);
}

DEFINE_TEST(get_sensitive)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_VALUE;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	rv = gkm_store_get_attribute (store, object, &attr);
	g_assert (rv == CKR_ATTRIBUTE_SENSITIVE);
}

DEFINE_TEST(get_internal)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_BITS_PER_PIXEL;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	rv = gkm_store_get_attribute (store, object, &attr);
	g_assert (rv == CKR_ATTRIBUTE_TYPE_INVALID);
}

DEFINE_TEST(set_invalid)
{
	CK_ATTRIBUTE attr;

	attr.type = CKA_APPLICATION;
	attr.pValue = "me";
	attr.ulValueLen = 2;

	gkm_store_set_attribute (store, transaction, object, &attr);
	g_assert (gkm_transaction_get_result (transaction) == CKR_ATTRIBUTE_TYPE_INVALID);
}

DEFINE_TEST(set_internal)
{
	CK_ATTRIBUTE attr;
	CK_ULONG five = 5;

	attr.type = CKA_BITS_PER_PIXEL;
	attr.pValue = &five;
	attr.ulValueLen = sizeof (five);

	gkm_store_set_attribute (store, transaction, object, &attr);
	g_assert (gkm_transaction_get_result (transaction) == CKR_ATTRIBUTE_TYPE_INVALID);
}

DEFINE_TEST(set_get_attribute)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_LABEL;
	attr.pValue = "booyah";
	attr.ulValueLen = 6;

	gkm_store_set_attribute (store, transaction, object, &attr);

	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_result (transaction) == CKR_OK);

	attr.pValue = buffer;
	attr.ulValueLen = 1024;
	rv = gkm_store_get_attribute (store, object, &attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 6);
	g_assert (memcmp (attr.pValue, "booyah", 6) == 0);
}

DEFINE_TEST(write_read_value)
{
	CK_ATTRIBUTE attr;
	CK_ULONG five = 5;
	gconstpointer value;
	gsize n_value;

	attr.type = CKA_BITS_PER_PIXEL;
	attr.pValue = &five;
	attr.ulValueLen = sizeof (five);

	gkm_store_write_value (store, transaction, object, &attr);

	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_result (transaction) == CKR_OK);

	value = gkm_store_read_value (store, object, CKA_BITS_PER_PIXEL, &n_value);
	g_assert (value);
	g_assert (n_value == sizeof (five));
	g_assert (memcmp (value, &five, sizeof (five)) == 0);
}

DEFINE_TEST(set_no_validate)
{
	CK_ATTRIBUTE attr;

	attr.type = CKA_LABEL;
	attr.pValue = "CAPITALS";
	attr.ulValueLen = 8;

	gkm_store_set_attribute (store, transaction, object, &attr);
	g_assert (gkm_transaction_get_failed (transaction));

	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_result (transaction) == CKR_ATTRIBUTE_VALUE_INVALID);
}

DEFINE_TEST(set_transaction_default)
{
	CK_ATTRIBUTE attr;
	gconstpointer value;
	gsize n_value;


	attr.type = CKA_LABEL;
	attr.pValue = "another";
	attr.ulValueLen = 7;

	/* Change the attribute */
	gkm_store_set_attribute (store, transaction, object, &attr);
	g_assert (gkm_transaction_get_failed (transaction) == FALSE);

	/* Should get new value */
	value = gkm_store_read_value (store, object, CKA_LABEL, &n_value);
	g_assert (value && n_value == attr.ulValueLen);
	g_assert (memcmp (attr.pValue, value, n_value) == 0);

	/* Fail for some arbitrary reason */
	gkm_transaction_fail (transaction, CKR_ATTRIBUTE_VALUE_INVALID);

	/* Value should not have changed yet */
	value = gkm_store_read_value (store, object, CKA_LABEL, &n_value);
	g_assert (value && n_value == attr.ulValueLen);
	g_assert (memcmp (attr.pValue, value, n_value) == 0);

	/* Now complete the transaction */
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_failed (transaction) == TRUE);

	/* Value should now have changed, back to default */
	value = gkm_store_read_value (store, object, CKA_LABEL, &n_value);
	g_assert (value && n_value == 5);
	g_assert (memcmp (value, "label", 5) == 0);
}

DEFINE_TEST(set_transaction_revert_first)
{
	CK_ATTRIBUTE attr, prev;
	gconstpointer value;
	gsize n_value;

	prev.type = CKA_LABEL;
	prev.pValue = "numberone";
	prev.ulValueLen = 9;

	/* Change the attribute */
	gkm_store_set_attribute (store, transaction, object, &prev);
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_failed (transaction) == FALSE);

	/* Value should be new value */
	value = gkm_store_read_value (store, object, CKA_LABEL, &n_value);
	g_assert (value && n_value == prev.ulValueLen);
	g_assert (memcmp (prev.pValue, value, n_value) == 0);

	/* A new transaction */
	g_object_unref (transaction);
	transaction = gkm_transaction_new ();

	attr.type = CKA_LABEL;
	attr.pValue = "second";
	attr.ulValueLen = 6;

	gkm_store_set_attribute (store, transaction, object, &attr);
	g_assert (gkm_transaction_get_failed (transaction) == FALSE);

	/* Should get new value */
	value = gkm_store_read_value (store, object, CKA_LABEL, &n_value);
	g_assert (value && n_value == attr.ulValueLen);
	g_assert (memcmp (attr.pValue, value, n_value) == 0);

	attr.type = CKA_LABEL;
	attr.pValue = "third";
	attr.ulValueLen = 5;

	gkm_store_set_attribute (store, transaction, object, &attr);
	g_assert (gkm_transaction_get_failed (transaction) == FALSE);

	/* Should get new value */
	value = gkm_store_read_value (store, object, CKA_LABEL, &n_value);
	g_assert (value && n_value == attr.ulValueLen);
	g_assert (memcmp (attr.pValue, value, n_value) == 0);

	/* Fail for some arbitrary reason */
	gkm_transaction_fail (transaction, CKR_ATTRIBUTE_VALUE_INVALID);

	/* Value should not have changed yet */
	value = gkm_store_read_value (store, object, CKA_LABEL, &n_value);
	g_assert (value && n_value == attr.ulValueLen);
	g_assert (memcmp (attr.pValue, value, n_value) == 0);

	/* Now complete the transaction */
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_failed (transaction) == TRUE);

	/* Value should now have changed, back to default */
	value = gkm_store_read_value (store, object, CKA_LABEL, &n_value);
	g_assert (value && n_value == prev.ulValueLen);
	g_assert (memcmp (prev.pValue, value, n_value) == 0);
}

static void
notify_attribute (GkmObject *obj, CK_ATTRIBUTE_TYPE type, gpointer data)
{
	g_assert (obj == object);
	g_assert (type == CKA_LABEL);
	g_assert (data);

	*((CK_ATTRIBUTE_TYPE*)data) = type;
}

DEFINE_TEST(set_notifies)
{
	CK_ATTRIBUTE attr;
	CK_ATTRIBUTE_TYPE type = 0;

	attr.type = CKA_LABEL;
	attr.pValue = "valid";
	attr.ulValueLen = 5;

	g_signal_connect (object, "notify-attribute", G_CALLBACK (notify_attribute), &type);

	gkm_store_set_attribute (store, transaction, object, &attr);

	/* We should have been notified that the attribute changed at this point */
	g_assert (type == CKA_LABEL);

	/* Reset for next notify */
	type = 0;

	/* Fail for some arbitrary reason */
	gkm_transaction_fail (transaction, CKR_ATTRIBUTE_VALUE_INVALID);

	/* We should not have been notified yet */
	g_assert (type == 0);

	/* Now complete the transaction */
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_failed (transaction) == TRUE);

	/* Now we should have been notified that this changed back */
	g_assert (type == CKA_LABEL);
}

DEFINE_TEST(set_object_gone_first)
{
	CK_ATTRIBUTE attr;

	attr.type = CKA_LABEL;
	attr.pValue = "valid";
	attr.ulValueLen = 5;

	gkm_store_set_attribute (store, transaction, object, &attr);
	gkm_transaction_complete (transaction);
	g_assert (gkm_transaction_get_result (transaction) == CKR_OK);

	/* This tests memory store internal tracking */
	g_object_unref (object);
	object = NULL;
}
