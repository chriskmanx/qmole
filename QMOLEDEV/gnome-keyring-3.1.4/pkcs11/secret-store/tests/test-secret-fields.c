/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-secret-fields.c: Test secret fields

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

#include "secret-store/gkm-secret-fields.h"

#include "pkcs11/pkcs11i.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void
test_new (void)
{
	GHashTable *fields = gkm_secret_fields_new ();
	g_hash_table_unref (fields);
}

static void
test_boxed (void)
{
	GType boxed = gkm_secret_fields_boxed_type ();
	GType check = gkm_secret_fields_boxed_type ();
	g_assert (boxed == check);
}

static void
test_add_get_values (void)
{
	GHashTable *fields = gkm_secret_fields_new ();
	const gchar *value;

	gkm_secret_fields_add (fields, "one", "value1");
	gkm_secret_fields_take (fields, g_strdup ("two"), g_strdup ("value2"));
	gkm_secret_fields_add (fields, "three", NULL);

	value = gkm_secret_fields_get (fields, "one");
	g_assert_cmpstr (value, ==, "value1");

	value = gkm_secret_fields_get (fields, "two");
	g_assert_cmpstr (value, ==, "value2");

	value = gkm_secret_fields_get (fields, "three");
	g_assert_cmpstr (value, ==, "");

	g_hash_table_unref (fields);
}

static void
test_parse (void)
{
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, "one\0value1\0two\0value2\0three\0value3\0", 35 };
	GHashTable *fields;
	const gchar *value;
	CK_RV rv;

	rv = gkm_secret_fields_parse (&attr, &fields);
	g_assert (rv == CKR_OK);

	g_assert_cmpuint (g_hash_table_size (fields), ==, 3);
	value = g_hash_table_lookup (fields, "one");
	g_assert_cmpstr (value, ==, "value1");
	value = g_hash_table_lookup (fields, "two");
	g_assert_cmpstr (value, ==, "value2");
	value = g_hash_table_lookup (fields, "three");
	g_assert_cmpstr (value, ==, "value3");

	g_hash_table_unref (fields);
}

static void
test_parse_empty (void)
{
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, "", 0 };
	GHashTable *fields;
	CK_RV rv;

	rv = gkm_secret_fields_parse (&attr, &fields);
	g_assert (rv == CKR_OK);

	g_assert_cmpuint (g_hash_table_size (fields), == , 0);

	g_hash_table_unref (fields);
}

static void
test_parse_null_invalid (void)
{
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, NULL, 5 };
	GHashTable *fields;
	CK_RV rv;

	rv = gkm_secret_fields_parse (&attr, &fields);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_parse_missing_value (void)
{
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, "one", 3 };
	GHashTable *fields;
	CK_RV rv;

	rv = gkm_secret_fields_parse (&attr, &fields);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_parse_missing_terminator (void)
{
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, "one\0value", 9 };
	GHashTable *fields;
	CK_RV rv;

	rv = gkm_secret_fields_parse (&attr, &fields);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_parse_not_utf8 (void)
{
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, "one\0not\234utf8\0", 13 };
	GHashTable *fields;
	CK_RV rv;

	rv = gkm_secret_fields_parse (&attr, &fields);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_serialize (void)
{
	gchar buffer[32];
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, buffer, 32 };
	GHashTable *fields;
	CK_RV rv;

	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add (fields, "one", "value1");

	rv = gkm_secret_fields_serialize (&attr, fields);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 11);
	g_assert (memcmp (buffer, "one\0value1\0", 11) == 0);

	g_hash_table_unref (fields);
}

static void
test_serialize_length (void)
{
	CK_ATTRIBUTE attr = { CKA_G_FIELDS, NULL, 0 };
	GHashTable *fields;
	CK_RV rv;

	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add (fields, "one", "value1");

	rv = gkm_secret_fields_serialize (&attr, fields);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 11);

	g_hash_table_unref (fields);
}

static void
test_add_get_compat_uint32 (void)
{
	GHashTable *fields;
	gboolean ret;
	guint32 value;

	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add_compat_uint32 (fields, "field", 992);

	ret = gkm_secret_fields_get_compat_uint32 (fields, "field", &value);
	g_assert (ret == TRUE);
	g_assert_cmpuint (value, ==, 992);

	g_hash_table_unref (fields);
}

static void
test_get_compat_uint32_fail (void)
{
	GHashTable *fields;
	gboolean ret;
	guint32 value;

	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add (fields, "test", "value");

	ret = gkm_secret_fields_get_compat_uint32 (fields, "test", &value);
	g_assert (ret == FALSE);

	g_hash_table_unref (fields);
}

static void
test_get_compat_hashed_string (void)
{
	GHashTable *fields;
	gboolean ret;
	gchar *value;

	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add (fields, "test", "value");

	ret = gkm_secret_fields_get_compat_hashed_string (fields, "test", &value);
	g_assert (ret == TRUE); /* Must be the same as the old gnome-keyring code */
	g_assert_cmpstr (value, ==, "2063c1608d6e0baf80249c42e2be5804");
	g_free (value);

	g_hash_table_unref (fields);
}

static void
test_get_compat_hashed_already (void)
{
	GHashTable *fields;
	gboolean ret;
	gchar *value;

	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add_compat_hashed_string (fields, "test", "e991664529cd0caeb6e9fce8fac3d611");

	ret = gkm_secret_fields_get_compat_hashed_string (fields, "test", &value);
	g_assert (ret == TRUE); /* What went in comes out */
	g_assert_cmpstr (value, ==, "e991664529cd0caeb6e9fce8fac3d611");
	g_free (value);

	g_hash_table_unref (fields);
}

static void
test_get_compat_hashed_uint32 (void)
{
	GHashTable *fields;
	gboolean ret;
	guint32 value;
	guint32 val = 992;

	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add_compat_uint32 (fields, "test", val);

	ret = gkm_secret_fields_get_compat_hashed_uint32 (fields, "test", &value);
	g_assert (ret == TRUE); /* Must be the same as the old gnome-keyring code */
	g_assert_cmpuint (value, ==, 0x18273645 ^ val ^ (val << 16 | val >> 16));

	g_hash_table_unref (fields);
}

static void
test_get_compat_hashed_uint32_already (void)
{
	GHashTable *fields;
	gboolean ret;
	guint32 value;
	guint32 val = 0x1bc735a5;

	fields = gkm_secret_fields_new ();
	gkm_secret_fields_add_compat_hashed_uint32 (fields, "test", val);

	ret = gkm_secret_fields_get_compat_hashed_uint32 (fields, "test", &value);
	g_assert (ret == TRUE); /* What went in comes out */
	g_assert_cmpuint (value, ==, val);

	g_hash_table_unref (fields);
}

static void
test_get_names (void)
{
	GHashTable *fields;
	GList *names, *l;

	fields = gkm_secret_fields_new ();

	gkm_secret_fields_add (fields, "one", "11111");
	gkm_secret_fields_add_compat_uint32 (fields, "two", 2);
	gkm_secret_fields_add_compat_hashed_string (fields, "test", "2063c1608d6e0baf80249c42e2be5804");

	names = gkm_secret_fields_get_names (fields);
	g_assert_cmpuint (g_list_length (names), ==, 3);

	for (l = names; l; l = g_list_next (l)) {
		g_assert (l->data);
		if (!g_str_equal (l->data, "one") &&
		    !g_str_equal (l->data, "two") &&
		    !g_str_equal (l->data, "test"))
			g_assert_not_reached ();
	}

	g_list_free (names);
	g_hash_table_unref (fields);
}

static void
test_match (void)
{
	GHashTable *haystack;
	GHashTable *needle;
	gboolean ret;

	haystack = gkm_secret_fields_new ();
	gkm_secret_fields_add (haystack, "one", "11111");
	gkm_secret_fields_add_compat_uint32 (haystack, "two", 2);
	gkm_secret_fields_add_compat_hashed_string (haystack, "test", "2063c1608d6e0baf80249c42e2be5804"); /* Hashed 'value' */
	gkm_secret_fields_add_compat_hashed_uint32 (haystack, "number", 0x1bc735a5); /* Hashed 992 */
	gkm_secret_fields_add (haystack, "extra", "an extra value");

	needle = gkm_secret_fields_new ();
	gkm_secret_fields_add (needle, "one", "11111");
	gkm_secret_fields_add (needle, "two", "2");
	gkm_secret_fields_add (needle, "test", "value");
	gkm_secret_fields_add (needle, "number", "992");

	ret = gkm_secret_fields_match (haystack, needle);
	g_assert (ret == TRUE);

	g_hash_table_unref (haystack);
	g_hash_table_unref (needle);
}

static void
test_match_mismatch_value (void)
{
	GHashTable *haystack;
	GHashTable *needle;
	gboolean ret;

	haystack = gkm_secret_fields_new ();
	gkm_secret_fields_add (haystack, "field", "value");

	needle = gkm_secret_fields_new ();
	gkm_secret_fields_add (needle, "field", "another");

	ret = gkm_secret_fields_match (haystack, needle);
	g_assert (ret == FALSE);

	g_hash_table_unref (haystack);
	g_hash_table_unref (needle);
}

static void
test_match_mismatch_field (void)
{
	GHashTable *haystack;
	GHashTable *needle;
	gboolean ret;

	haystack = gkm_secret_fields_new ();
	gkm_secret_fields_add (haystack, "test", "value");

	needle = gkm_secret_fields_new ();
	gkm_secret_fields_add (needle, "field", "value");

	ret = gkm_secret_fields_match (haystack, needle);
	g_assert (ret == FALSE);

	g_hash_table_unref (haystack);
	g_hash_table_unref (needle);
}

static void
test_match_wrong_hashed (void)
{
	GHashTable *haystack;
	GHashTable *needle;
	gboolean ret;

	haystack = gkm_secret_fields_new ();
	gkm_secret_fields_add_compat_hashed_uint32 (haystack, "number", 0x1bc735a5); /* Hashed 992 */

	needle = gkm_secret_fields_new ();
	gkm_secret_fields_add (needle, "number", "1000");

	ret = gkm_secret_fields_match (haystack, needle);
	g_assert (ret == FALSE);

	g_hash_table_unref (haystack);
	g_hash_table_unref (needle);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/secret-store/fields/new", test_new);
	g_test_add_func ("/secret-store/fields/boxed", test_boxed);
	g_test_add_func ("/secret-store/fields/add_get_values", test_add_get_values);
	g_test_add_func ("/secret-store/fields/parse", test_parse);
	g_test_add_func ("/secret-store/fields/parse_empty", test_parse_empty);
	g_test_add_func ("/secret-store/fields/parse_null_invalid", test_parse_null_invalid);
	g_test_add_func ("/secret-store/fields/parse_missing_value", test_parse_missing_value);
	g_test_add_func ("/secret-store/fields/parse_missing_terminator", test_parse_missing_terminator);
	g_test_add_func ("/secret-store/fields/parse_not_utf8", test_parse_not_utf8);
	g_test_add_func ("/secret-store/fields/serialize", test_serialize);
	g_test_add_func ("/secret-store/fields/serialize_length", test_serialize_length);
	g_test_add_func ("/secret-store/fields/add_get_compat_uint32", test_add_get_compat_uint32);
	g_test_add_func ("/secret-store/fields/get_compat_uint32_fail", test_get_compat_uint32_fail);
	g_test_add_func ("/secret-store/fields/get_compat_hashed_string", test_get_compat_hashed_string);
	g_test_add_func ("/secret-store/fields/get_compat_hashed_already", test_get_compat_hashed_already);
	g_test_add_func ("/secret-store/fields/get_compat_hashed_uint32", test_get_compat_hashed_uint32);
	g_test_add_func ("/secret-store/fields/get_compat_hashed_uint32_already", test_get_compat_hashed_uint32_already);
	g_test_add_func ("/secret-store/fields/get_names", test_get_names);
	g_test_add_func ("/secret-store/fields/match", test_match);
	g_test_add_func ("/secret-store/fields/match_mismatch_value", test_match_mismatch_value);
	g_test_add_func ("/secret-store/fields/match_mismatch_field", test_match_mismatch_field);
	g_test_add_func ("/secret-store/fields/match_wrong_hashed", test_match_wrong_hashed);

	return g_test_run ();
}
