/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-attributes.c: Test attributes functionality

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

#include "gkm/gkm-attributes.h"

#include <glib-object.h>

/* Some test data */
static CK_OBJECT_CLASS attr_template_klass = CKO_DATA;
static CK_BBOOL attr_template_token = CK_TRUE;
static CK_ATTRIBUTE attr_template[] = {
	{ CKA_LABEL, "funny", 5 },
	{ CKA_CLASS, &attr_template_klass, sizeof (CK_OBJECT_CLASS) },
	{ CKA_ID, "my-identifier", 13 },
	{ CKA_TOKEN, &attr_template_token, sizeof (CK_BBOOL) },
	{ CKA_VALUE, "\a[\315\025", 4 }
};

static void
test_attribute_equal_zero_len_null_ptr (void)
{
	CK_ATTRIBUTE attr1 = { CKA_LABEL, "", 0 };
	CK_ATTRIBUTE attr2 = { CKA_LABEL, NULL, 0 };
	g_assert (gkm_attribute_equal (&attr1, &attr2));
}

static void
test_attribute_consume (void)
{
	CK_ATTRIBUTE attr;
	attr.type = CKA_LABEL;

	gkm_attribute_consume (&attr);
	g_assert (attr.type == (gulong)-1);
}

static void
test_attribute_consumed (void)
{
	CK_ATTRIBUTE attr;
	gboolean ret;

	attr.type = CKA_LABEL;

	ret = gkm_attribute_consumed (&attr);
	g_assert (ret == FALSE);

	gkm_attribute_consume (&attr);

	ret = gkm_attribute_consumed (&attr);
	g_assert (ret == TRUE);
}

static void
test_attribute_set_data (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, sizeof (buffer) };
	CK_RV rv;

	rv = gkm_attribute_set_data (&attr, "mytest", 6);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 6);
	g_assert (memcmp (buffer, "mytest", 6) == 0);
}

static void
test_attribute_set_data_short (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, 4 };
	CK_RV rv;

	rv = gkm_attribute_set_data (&attr, "mytest", 6);
	g_assert (rv == CKR_BUFFER_TOO_SMALL);
	g_assert (attr.ulValueLen == (CK_ULONG)-1);
}

static void
test_attribute_set_data_length (void)
{
	CK_ATTRIBUTE attr = { 0, NULL, 0 };
	CK_RV rv;

	rv = gkm_attribute_set_data (&attr, "mytest", 6);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 6);
}

static void
test_attribute_set_empty (void)
{
	CK_ATTRIBUTE attr;
	gchar buf[30];
	CK_RV rv;

	attr.ulValueLen = 30;
	attr.pValue = buf;
	rv = gkm_attribute_set_empty (&attr);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 0);
}

static void
test_attribute_get_bool (void)
{
	CK_ATTRIBUTE attr;
	CK_BBOOL val = CK_TRUE;
	gboolean value;
	CK_RV rv;

	attr.ulValueLen = sizeof (CK_BBOOL);
	attr.pValue = &val;
	rv = gkm_attribute_get_bool (&attr, &value);
	g_assert (rv == CKR_OK);
	g_assert (value == TRUE);
}

static void
test_attribute_get_bool_invalid (void)
{
	CK_ATTRIBUTE attr;
	CK_ULONG val = 4;
	gboolean value;
	CK_RV rv;

	attr.ulValueLen = sizeof (CK_ULONG);
	attr.pValue = &val;
	rv = gkm_attribute_get_bool (&attr, &value);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_attribute_set_time (void)
{
	CK_ATTRIBUTE attr;
	gchar buf[30];
	CK_RV rv;

	attr.ulValueLen = 30;
	attr.pValue = buf;
	rv = gkm_attribute_set_time (&attr, 1247930171);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 16);
	g_assert (memcmp (attr.pValue, "2009071815161100", 16) == 0);
}

static void
test_attribute_set_time_empty (void)
{
	CK_ATTRIBUTE attr;
	gchar buf[30];
	CK_RV rv;

	attr.ulValueLen = 30;
	attr.pValue = buf;
	rv = gkm_attribute_set_time (&attr, -1);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 0);
}

static void
test_attribute_set_time_length (void)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.pValue = NULL;
	attr.ulValueLen = 0;
	rv = gkm_attribute_set_time (&attr, 1247930171);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 16);
	g_assert (attr.pValue == NULL);
}

static void
test_attribute_get_time (void)
{
	CK_ATTRIBUTE attr;
	glong when;
	CK_RV rv;

	attr.ulValueLen = 16;
	attr.pValue = "2009071815161100";
	rv = gkm_attribute_get_time (&attr, &when);
	g_assert (rv == CKR_OK);
	g_assert (when == 1247930171);
}

static void
test_attribute_get_time_empty (void)
{
	CK_ATTRIBUTE attr;
	glong when;
	CK_RV rv;

	attr.ulValueLen = 0;
	attr.pValue = "";
	rv = gkm_attribute_get_time (&attr, &when);
	g_assert (rv == CKR_OK);
	g_assert (when == -1);
}

static void
test_attribute_get_time_invalid (void)
{
	CK_ATTRIBUTE attr;
	glong when;
	CK_RV rv;

	attr.ulValueLen = 16;
	attr.pValue = "aaaaaaaaaaaaaaaa";
	rv = gkm_attribute_get_time (&attr, &when);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_attribute_get_time_invalid_length (void)
{
	CK_ATTRIBUTE attr;
	glong when;
	CK_RV rv;

	attr.ulValueLen = 8;
	attr.pValue = "2009071815161100";
	rv = gkm_attribute_get_time (&attr, &when);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_attribute_get_string (void)
{
	CK_ATTRIBUTE attr;
	gchar *value;
	CK_RV rv;

	attr.ulValueLen = 4;
	attr.pValue = "blah";

	rv = gkm_attribute_get_string (&attr, &value);
	g_assert (rv == CKR_OK);
	g_assert_cmpstr (value, ==, "blah");

	g_free (value);
}

static void
test_attribute_get_string_null (void)
{
	CK_ATTRIBUTE attr;
	gchar *value;
	CK_RV rv;

	attr.ulValueLen = 0;
	attr.pValue = NULL;

	rv = gkm_attribute_get_string (&attr, &value);
	g_assert (rv == CKR_OK);
	g_assert (value == NULL);
}

static void
test_attribute_get_string_not_utf8 (void)
{
	CK_ATTRIBUTE attr;
	gchar *value;
	CK_RV rv;

	/* No embedded nulls, or non-UTF8 */
	attr.ulValueLen = 5;
	attr.pValue = "\0test";

	rv = gkm_attribute_get_string (&attr, &value);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_attribute_get_string_bad_pointer (void)
{
	CK_ATTRIBUTE attr;
	gchar *value;
	CK_RV rv;

	/* No embedded nulls, or non-UTF8 */
	attr.ulValueLen = 5;
	attr.pValue = NULL;

	rv = gkm_attribute_get_string (&attr, &value);
	g_assert (rv == CKR_ATTRIBUTE_VALUE_INVALID);
}

static void
test_attribute_set_bool (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, sizeof (buffer) };
	CK_RV rv;

	rv = gkm_attribute_set_bool (&attr, CK_TRUE);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 1);
	g_assert (memcmp (buffer, "\1", 1) == 0);

	rv = gkm_attribute_set_bool (&attr, CK_FALSE);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 1);
	g_assert (memcmp (buffer, "\0", 1) == 0);
}

static void
test_attribute_set_bool_short (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, 0 };
	CK_RV rv;

	rv = gkm_attribute_set_bool (&attr, CK_TRUE);
	g_assert (rv == CKR_BUFFER_TOO_SMALL);
	g_assert (attr.ulValueLen == (CK_ULONG)-1);
}

static void
test_attribute_set_bool_length (void)
{
	CK_ATTRIBUTE attr = { 0, NULL, 0 };
	CK_RV rv;

	rv = gkm_attribute_set_bool (&attr, CK_TRUE);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 1);
}

static void
test_attribute_set_ulong (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, sizeof (buffer) };
	CK_ULONG value = 55;
	CK_RV rv;

	rv = gkm_attribute_set_ulong (&attr, value);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == sizeof (CK_ULONG));
	g_assert (memcmp (buffer, &value, sizeof (CK_ULONG)) == 0);
}

static void
test_attribute_set_ulong_short (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, 0 };
	CK_RV rv;

	rv = gkm_attribute_set_ulong (&attr, 1);
	g_assert (rv == CKR_BUFFER_TOO_SMALL);
	g_assert (attr.ulValueLen == (CK_ULONG)-1);
}

static void
test_attribute_set_ulong_length (void)
{
	CK_ATTRIBUTE attr = { 0, NULL, 0 };
	CK_RV rv;

	rv = gkm_attribute_set_ulong (&attr, 98889);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == sizeof (CK_ULONG));
}

static void
test_attribute_set_string (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, sizeof (buffer) };
	CK_RV rv;

	rv = gkm_attribute_set_string (&attr, "hello");
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 5);
	g_assert (memcmp (buffer, "hello", 5) == 0);
}

static void
test_attribute_set_string_null (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, sizeof (buffer) };
	CK_RV rv;

	rv = gkm_attribute_set_string (&attr, NULL);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 0);
}

static void
test_attribute_set_string_short (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, 3 };
	CK_RV rv;

	rv = gkm_attribute_set_string (&attr, "hello");
	g_assert (rv == CKR_BUFFER_TOO_SMALL);
	g_assert (attr.ulValueLen == (CK_ULONG)-1);
}

static void
test_attribute_set_string_length (void)
{
	CK_ATTRIBUTE attr = { 0, NULL, 0 };
	CK_RV rv;

	rv = gkm_attribute_set_string (&attr, "hello");
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 5);
}

static void
test_attribute_set_date (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, sizeof (buffer) };
	CK_DATE *date;
	CK_RV rv;

	rv = gkm_attribute_set_date (&attr, 1249845741);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == sizeof (CK_DATE));
	date = (CK_DATE*)buffer;
	g_assert (memcmp (date->day, "09", 2) == 0);
	g_assert (memcmp (date->month, "08", 2) == 0);
	g_assert (memcmp (date->year, "2009", 4) == 0);
}

static void
test_attribute_set_date_none (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, sizeof (buffer) };
	CK_RV rv;

	rv = gkm_attribute_set_date (&attr, (time_t)-1);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 0);
}

static void
test_attribute_set_date_short (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, 5 };
	CK_RV rv;

	rv = gkm_attribute_set_date (&attr, 1249845741);
	g_assert (rv == CKR_BUFFER_TOO_SMALL);
	g_assert (attr.ulValueLen == (CK_ULONG)-1);
}

static void
test_attribute_set_date_length (void)
{
	CK_ATTRIBUTE attr = { 0, NULL, 0 };
	CK_RV rv;

	rv = gkm_attribute_set_date (&attr, 1249845741);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == sizeof (CK_DATE));
}

static void
test_attribute_set_mpi (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, sizeof (buffer) };
	gcry_mpi_t mpi;
	CK_RV rv;

	mpi = gcry_mpi_new (32);
	gcry_mpi_set_ui (mpi, 123456789);

	rv = gkm_attribute_set_mpi (&attr, mpi);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 4);
	g_assert (memcmp (buffer, "\a[\315\025", 4) == 0);

	gcry_mpi_release (mpi);
}

static void
test_attribute_set_mpi_short (void)
{
	guchar buffer[32];
	CK_ATTRIBUTE attr = { 0, buffer, 2 };
	gcry_mpi_t mpi;
	CK_RV rv;

	mpi = gcry_mpi_new (32);
	gcry_mpi_set_ui (mpi, 123456789);

	rv = gkm_attribute_set_mpi (&attr, mpi);
	g_assert (rv == CKR_BUFFER_TOO_SMALL);
	g_assert (attr.ulValueLen == (CK_ULONG)-1);

	gcry_mpi_release (mpi);
}

static void
test_attribute_set_mpi_length (void)
{
	CK_ATTRIBUTE attr = { 0, NULL, 0 };
	gcry_mpi_t mpi;
	CK_RV rv;

	mpi = gcry_mpi_new (32);
	gcry_mpi_set_ui (mpi, 123456789);

	rv = gkm_attribute_set_mpi (&attr, mpi);
	g_assert (rv == CKR_OK);
	g_assert (attr.ulValueLen == 4);

	gcry_mpi_release (mpi);
}

static void
test_attribute_equal (void)
{
	/* Make sure we actually have two different strings */
	gchar *val1 = g_strdup ("my-identifier");
	gchar *val2 = g_strdup ("my-identifier");
	CK_ATTRIBUTE attr1 = { CKA_ID, val1, 13 };
	CK_ATTRIBUTE attr2 = { CKA_ID, val2, 13 };
	gboolean ret;

	ret = gkm_attribute_equal (&attr1, &attr2);
	g_assert (ret == TRUE);

	g_free (val1);
	g_free (val2);
}

static void
test_attribute_equal_same (void)
{
	CK_ATTRIBUTE attr = { CKA_ID, "my-identifier", 13 };
	gboolean ret;

	ret = gkm_attribute_equal (&attr, &attr);
	g_assert (ret == TRUE);
}

static void
test_attribute_equal_same_pointer (void)
{
	gchar *val = "my-identifier";
	CK_ATTRIBUTE attr1 = { CKA_ID, val, 13 };
	CK_ATTRIBUTE attr2 = { CKA_ID, val, 13 };
	gboolean ret;

	ret = gkm_attribute_equal (&attr1, &attr2);
	g_assert (ret == TRUE);
}

static void
test_attribute_equal_diff_types (void)
{
	gchar *val = "my-identifier";
	CK_ATTRIBUTE attr1 = { CKA_ID, val, 13 };
	CK_ATTRIBUTE attr2 = { CKA_VALUE, val, 13 };
	gboolean ret;

	ret = gkm_attribute_equal (&attr1, &attr2);
	g_assert (ret == FALSE);
}

static void
test_attribute_equal_diff_length (void)
{
	CK_ATTRIBUTE attr1 = { CKA_ID, "my-identifier", 13 };
	CK_ATTRIBUTE attr2 = { CKA_ID, "my-identifier", 2 };
	gboolean ret;

	ret = gkm_attribute_equal (&attr1, &attr2);
	g_assert (ret == FALSE);
}

static void
test_attribute_equal_diff_value (void)
{
	CK_ATTRIBUTE attr1 = { CKA_ID, "my-identifier", 13 };
	CK_ATTRIBUTE attr2 = { CKA_ID, "xy-identifier", 13 };
	gboolean ret;

	ret = gkm_attribute_equal (&attr1, &attr2);
	g_assert (ret == FALSE);
}

static void
test_attribute_hash (void)
{
	CK_ATTRIBUTE attr = { CKA_VALUE, "value", 5 };
	guint hash;

	/* The hash value below could change as code changes */
	hash = gkm_attribute_hash (&attr);
	g_assert_cmpuint (hash, !=, 0U);
}

static void
test_attribute_contains (void)
{
	CK_ATTRIBUTE attr = { CKA_ID, "my-identifier", 13 };
	gboolean ret;

	ret = gkm_attributes_contains (attr_template, G_N_ELEMENTS (attr_template), &attr);
	g_assert (ret == TRUE);
}

static void
test_attribute_contains_no_value (void)
{
	CK_ATTRIBUTE attr = { CKA_ID, "other-identifier", 16 };
	gboolean ret;

	ret = gkm_attributes_contains (attr_template, G_N_ELEMENTS (attr_template), &attr);
	g_assert (ret == FALSE);
}

static void
test_attribute_contains_no_type (void)
{
	CK_ATTRIBUTE attr = { CKA_VALUE, "value", 5 };
	gboolean ret;

	ret = gkm_attributes_contains (attr_template, G_N_ELEMENTS (attr_template), &attr);
	g_assert (ret == FALSE);
}

static void
test_attributes_find (void)
{
	CK_ATTRIBUTE_PTR attr;

	attr = gkm_attributes_find (attr_template, G_N_ELEMENTS (attr_template), CKA_LABEL);
	g_assert (attr != NULL);
	g_assert (attr->type == CKA_LABEL);
}

static void
test_attributes_find_not_found (void)
{
	CK_ATTRIBUTE_PTR attr;

	attr = gkm_attributes_find (attr_template, G_N_ELEMENTS (attr_template), CKA_SENSITIVE);
	g_assert (attr == NULL);
}

static void
test_attribute_find_boolean (void)
{
	gboolean value;
	gboolean ret;

	ret = gkm_attributes_find_boolean (attr_template, G_N_ELEMENTS (attr_template), CKA_TOKEN, &value);
	g_assert (ret == TRUE);
	g_assert (value == TRUE);
}

static void
test_attribute_find_boolean_no_type (void)
{
	gboolean value;
	gboolean ret;

	ret = gkm_attributes_find_boolean (attr_template, G_N_ELEMENTS (attr_template), CKA_SENSITIVE, &value);
	g_assert (ret == FALSE);
}

static void
test_attribute_find_boolean_not_bbool (void)
{
	gboolean value;
	gboolean ret;

	ret = gkm_attributes_find_boolean (attr_template, G_N_ELEMENTS (attr_template), CKA_CLASS, &value);
	g_assert (ret == FALSE);
}

static void
test_attribute_find_ulong (void)
{
	gulong value;
	gboolean ret;

	ret = gkm_attributes_find_ulong (attr_template, G_N_ELEMENTS (attr_template), CKA_CLASS, &value);
	g_assert (ret == TRUE);
	g_assert (value == CKO_DATA);
}

static void
test_attribute_find_ulong_no_type (void)
{
	gulong value;
	gboolean ret;

	ret = gkm_attributes_find_ulong (attr_template, G_N_ELEMENTS (attr_template), CKA_KEY_TYPE, &value);
	g_assert (ret == FALSE);
}

static void
test_attribute_find_ulong_not_ulong (void)
{
	gulong value;
	gboolean ret;

	ret = gkm_attributes_find_ulong (attr_template, G_N_ELEMENTS (attr_template), CKA_ID, &value);
	g_assert (ret == FALSE);
}

static void
test_attribute_find_mpi (void)
{
	gcry_mpi_t mpi = NULL;
	gboolean ret;

	ret = gkm_attributes_find_mpi (attr_template, G_N_ELEMENTS (attr_template), CKA_VALUE, &mpi);
	g_assert (ret == TRUE);
	g_assert (mpi != NULL);

	g_assert (gcry_mpi_cmp_ui (mpi, 123456789) == 0);
	gcry_mpi_release (mpi);
}

static void
test_attribute_find_mpi_no_type (void)
{
	gcry_mpi_t mpi = NULL;
	gboolean ret;

	ret = gkm_attributes_find_mpi (attr_template, G_N_ELEMENTS (attr_template), CKA_MODULUS, &mpi);
	g_assert (ret == FALSE);
	g_assert (mpi == NULL);
}

static void
test_attributes_consume (void)
{
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG n_attrs;

	/* Dup because we're writing to this */
	attrs = g_memdup (attr_template, sizeof (attr_template));
	n_attrs = G_N_ELEMENTS (attr_template);

	/* All these attributes are there */
	g_assert (gkm_attributes_find (attrs, n_attrs, CKA_LABEL) != NULL);
	g_assert (gkm_attributes_find (attrs, n_attrs, CKA_ID) != NULL);
	g_assert (gkm_attributes_find (attrs, n_attrs, CKA_CLASS) != NULL);

	/* Consume some of them */
	gkm_attributes_consume (attrs, n_attrs, CKA_LABEL, CKA_ID, G_MAXULONG);

	/* Two should be gone */
	g_assert (gkm_attributes_find (attrs, n_attrs, CKA_LABEL) == NULL);
	g_assert (gkm_attributes_find (attrs, n_attrs, CKA_ID) == NULL);
	g_assert (gkm_attributes_find (attrs, n_attrs, CKA_CLASS) != NULL);

	g_free (attrs);
}

static void
test_template_new_free (void)
{
	GArray *template = gkm_template_new (attr_template, G_N_ELEMENTS (attr_template));
	g_assert (template);
	gkm_template_free (template);
}

static void
test_template_find (void)
{
	GArray *template = gkm_template_new (attr_template, G_N_ELEMENTS (attr_template));
	gulong uvalue;
	gboolean ret, bvalue;

	ret = gkm_template_find_ulong (template, CKA_CLASS, &uvalue);
	g_assert (ret);
	g_assert (uvalue == attr_template_klass);

	ret = gkm_template_find_boolean (template, CKA_TOKEN, &bvalue);
	g_assert (ret);
	g_assert (bvalue == attr_template_token);

	/* An invalid attribute */
	ret = gkm_template_find_boolean (template, CKA_AC_ISSUER, &bvalue);
	g_assert (!ret);

	gkm_template_free (template);
}

static void
test_template_set_replace (void)
{
	GArray *template = gkm_template_new (attr_template, G_N_ELEMENTS (attr_template));
	CK_OBJECT_CLASS klass = CKO_HW_FEATURE;
	CK_ATTRIBUTE attr = { CKA_CLASS, &klass, sizeof (klass) };
	gulong uvalue;

	if (!gkm_template_find_ulong (template, CKA_CLASS, &uvalue))
		g_assert_not_reached ();
	g_assert (uvalue == attr_template_klass);

	/* Replace a previous attribute */
	gkm_template_set (template, &attr);

	if (!gkm_template_find_ulong (template, CKA_CLASS, &uvalue))
		g_assert_not_reached ();
	g_assert (uvalue == CKO_HW_FEATURE);

	gkm_template_free (template);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/gkm/attributes/attribute_equal_zero_len_null_ptr", test_attribute_equal_zero_len_null_ptr);
	g_test_add_func ("/gkm/attributes/attribute_consume", test_attribute_consume);
	g_test_add_func ("/gkm/attributes/attribute_consumed", test_attribute_consumed);
	g_test_add_func ("/gkm/attributes/attribute_set_data", test_attribute_set_data);
	g_test_add_func ("/gkm/attributes/attribute_set_data_short", test_attribute_set_data_short);
	g_test_add_func ("/gkm/attributes/attribute_set_data_length", test_attribute_set_data_length);
	g_test_add_func ("/gkm/attributes/attribute_set_empty", test_attribute_set_empty);
	g_test_add_func ("/gkm/attributes/attribute_get_bool", test_attribute_get_bool);
	g_test_add_func ("/gkm/attributes/attribute_get_bool_invalid", test_attribute_get_bool_invalid);
	g_test_add_func ("/gkm/attributes/attribute_set_time", test_attribute_set_time);
	g_test_add_func ("/gkm/attributes/attribute_set_time_empty", test_attribute_set_time_empty);
	g_test_add_func ("/gkm/attributes/attribute_set_time_length", test_attribute_set_time_length);
	g_test_add_func ("/gkm/attributes/attribute_get_time", test_attribute_get_time);
	g_test_add_func ("/gkm/attributes/attribute_get_time_empty", test_attribute_get_time_empty);
	g_test_add_func ("/gkm/attributes/attribute_get_time_invalid", test_attribute_get_time_invalid);
	g_test_add_func ("/gkm/attributes/attribute_get_time_invalid_length", test_attribute_get_time_invalid_length);
	g_test_add_func ("/gkm/attributes/attribute_get_string", test_attribute_get_string);
	g_test_add_func ("/gkm/attributes/attribute_get_string_null", test_attribute_get_string_null);
	g_test_add_func ("/gkm/attributes/attribute_get_string_not_utf8", test_attribute_get_string_not_utf8);
	g_test_add_func ("/gkm/attributes/attribute_get_string_bad_pointer", test_attribute_get_string_bad_pointer);
	g_test_add_func ("/gkm/attributes/attribute_set_bool", test_attribute_set_bool);
	g_test_add_func ("/gkm/attributes/attribute_set_bool_short", test_attribute_set_bool_short);
	g_test_add_func ("/gkm/attributes/attribute_set_bool_length", test_attribute_set_bool_length);
	g_test_add_func ("/gkm/attributes/attribute_set_ulong", test_attribute_set_ulong);
	g_test_add_func ("/gkm/attributes/attribute_set_ulong_short", test_attribute_set_ulong_short);
	g_test_add_func ("/gkm/attributes/attribute_set_ulong_length", test_attribute_set_ulong_length);
	g_test_add_func ("/gkm/attributes/attribute_set_string", test_attribute_set_string);
	g_test_add_func ("/gkm/attributes/attribute_set_string_null", test_attribute_set_string_null);
	g_test_add_func ("/gkm/attributes/attribute_set_string_short", test_attribute_set_string_short);
	g_test_add_func ("/gkm/attributes/attribute_set_string_length", test_attribute_set_string_length);
	g_test_add_func ("/gkm/attributes/attribute_set_date", test_attribute_set_date);
	g_test_add_func ("/gkm/attributes/attribute_set_date_none", test_attribute_set_date_none);
	g_test_add_func ("/gkm/attributes/attribute_set_date_short", test_attribute_set_date_short);
	g_test_add_func ("/gkm/attributes/attribute_set_date_length", test_attribute_set_date_length);
	g_test_add_func ("/gkm/attributes/attribute_set_mpi", test_attribute_set_mpi);
	g_test_add_func ("/gkm/attributes/attribute_set_mpi_short", test_attribute_set_mpi_short);
	g_test_add_func ("/gkm/attributes/attribute_set_mpi_length", test_attribute_set_mpi_length);
	g_test_add_func ("/gkm/attributes/attribute_equal", test_attribute_equal);
	g_test_add_func ("/gkm/attributes/attribute_equal_same", test_attribute_equal_same);
	g_test_add_func ("/gkm/attributes/attribute_equal_same_pointer", test_attribute_equal_same_pointer);
	g_test_add_func ("/gkm/attributes/attribute_equal_diff_types", test_attribute_equal_diff_types);
	g_test_add_func ("/gkm/attributes/attribute_equal_diff_length", test_attribute_equal_diff_length);
	g_test_add_func ("/gkm/attributes/attribute_equal_diff_value", test_attribute_equal_diff_value);
	g_test_add_func ("/gkm/attributes/attribute_hash", test_attribute_hash);
	g_test_add_func ("/gkm/attributes/attribute_contains", test_attribute_contains);
	g_test_add_func ("/gkm/attributes/attribute_contains_no_value", test_attribute_contains_no_value);
	g_test_add_func ("/gkm/attributes/attribute_contains_no_type", test_attribute_contains_no_type);
	g_test_add_func ("/gkm/attributes/attributes_find", test_attributes_find);
	g_test_add_func ("/gkm/attributes/attributes_find_not_found", test_attributes_find_not_found);
	g_test_add_func ("/gkm/attributes/attribute_find_boolean", test_attribute_find_boolean);
	g_test_add_func ("/gkm/attributes/attribute_find_boolean_no_type", test_attribute_find_boolean_no_type);
	g_test_add_func ("/gkm/attributes/attribute_find_boolean_not_bbool", test_attribute_find_boolean_not_bbool);
	g_test_add_func ("/gkm/attributes/attribute_find_ulong", test_attribute_find_ulong);
	g_test_add_func ("/gkm/attributes/attribute_find_ulong_no_type", test_attribute_find_ulong_no_type);
	g_test_add_func ("/gkm/attributes/attribute_find_ulong_not_ulong", test_attribute_find_ulong_not_ulong);
	g_test_add_func ("/gkm/attributes/attribute_find_mpi", test_attribute_find_mpi);
	g_test_add_func ("/gkm/attributes/attribute_find_mpi_no_type", test_attribute_find_mpi_no_type);
	g_test_add_func ("/gkm/attributes/attributes_consume", test_attributes_consume);
	g_test_add_func ("/gkm/attributes/template_new_free", test_template_new_free);
	g_test_add_func ("/gkm/attributes/template_find", test_template_find);
	g_test_add_func ("/gkm/attributes/template_set_replace", test_template_set_replace);

	return g_test_run ();
}
