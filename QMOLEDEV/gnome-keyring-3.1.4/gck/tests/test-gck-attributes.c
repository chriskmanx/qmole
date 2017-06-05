/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-gck-attributes.c - the GObject PKCS#11 wrapper library

   Copyright (C) 2011 Collabora Ltd.

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

   Author: Stef Walter <stefw@collabora.co.uk>
*/

#include "config.h"

#include <glib.h>
#include <string.h>

#include "gck/gck.h"

#define ATTR_TYPE 55
#define ATTR_DATA "TEST DATA"
#define N_ATTR_DATA ((gsize)9)

static void
test_init_memory (void)
{
	GckAttribute attr;

	g_assert (sizeof (attr) == sizeof (CK_ATTRIBUTE));

	gck_attribute_init (&attr, ATTR_TYPE, ATTR_DATA, N_ATTR_DATA);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == N_ATTR_DATA);
	g_assert (memcmp (attr.value, ATTR_DATA, attr.length) == 0);

	gck_attribute_clear (&attr);
}

static void
test_value_to_boolean (void)
{
	CK_BBOOL data = CK_TRUE;
	gboolean result = FALSE;

	if (!gck_value_to_boolean (&data, sizeof (data), &result))
		g_assert_not_reached ();

	g_assert (result == TRUE);

	if (!gck_value_to_boolean (&data, sizeof (data), NULL))
		g_assert_not_reached ();

	/* Should fail */
	if (gck_value_to_boolean (&data, 0, NULL))
		g_assert_not_reached ();
	if (gck_value_to_boolean (&data, 2, NULL))
		g_assert_not_reached ();
	if (gck_value_to_boolean (&data, (CK_ULONG)-1, NULL))
		g_assert_not_reached ();
}

static void
test_value_to_ulong (void)
{
	CK_ULONG data = 34343;
	gulong result = 0;

	if (!gck_value_to_ulong (&data, sizeof (data), &result))
		g_assert_not_reached ();

	g_assert (result == 34343);

	if (!gck_value_to_ulong (&data, sizeof (data), NULL))
		g_assert_not_reached ();

	/* Should fail */
	if (gck_value_to_ulong (&data, 0, NULL))
		g_assert_not_reached ();
	if (gck_value_to_ulong (&data, 2, NULL))
		g_assert_not_reached ();
	if (gck_value_to_ulong (&data, (CK_ULONG)-1, NULL))
		g_assert_not_reached ();
}

static void
test_init_boolean (void)
{
	GckAttribute attr;

	gck_attribute_init_boolean (&attr, ATTR_TYPE, TRUE);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == sizeof (CK_BBOOL));
	g_assert (*((CK_BBOOL*)attr.value) == CK_TRUE);

	gck_attribute_clear (&attr);
}

static void
test_init_date (void)
{
	GckAttribute attr;
	CK_DATE ck_date;
	GDate *date;

	date = g_date_new_dmy(05, 06, 1960);
	memcpy (ck_date.year, "1960", 4);
	memcpy (ck_date.month, "06", 2);
	memcpy (ck_date.day, "05", 2);
	gck_attribute_init_date (&attr, ATTR_TYPE, date);
	g_date_free (date);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == sizeof (CK_DATE));
	g_assert (memcmp (attr.value, &ck_date, attr.length) == 0);

	gck_attribute_clear (&attr);
}

static void
test_init_ulong (void)
{
	GckAttribute attr;

	gck_attribute_init_ulong (&attr, ATTR_TYPE, 88);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == sizeof (CK_ULONG));
	g_assert (*((CK_ULONG*)attr.value) == 88);

	gck_attribute_clear (&attr);
}

static void
test_init_string (void)
{
	GckAttribute attr;

	gck_attribute_init_string (&attr, ATTR_TYPE, "a test string");
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == strlen ("a test string"));
	g_assert (memcmp (attr.value, "a test string", attr.length) == 0);

	gck_attribute_clear (&attr);
}

static void
test_init_invalid (void)
{
	GckAttribute attr;

	gck_attribute_init_invalid (&attr, ATTR_TYPE);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == (gulong)-1);
	g_assert (attr.value == NULL);

	g_assert (gck_attribute_is_invalid (&attr));
	gck_attribute_clear (&attr);
}

static void
test_init_empty (void)
{
	GckAttribute attr;

	gck_attribute_init_empty (&attr, ATTR_TYPE);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == 0);
	g_assert (attr.value == NULL);

	gck_attribute_clear (&attr);
}

static void
test_new_memory (void)
{
	GckAttribute *attr;

	attr = gck_attribute_new (ATTR_TYPE, ATTR_DATA, N_ATTR_DATA);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == N_ATTR_DATA);
	g_assert (memcmp (attr->value, ATTR_DATA, attr->length) == 0);

	gck_attribute_free (attr);
}

static void
test_new_boolean (void)
{
	GckAttribute *attr;

	attr = gck_attribute_new_boolean (ATTR_TYPE, TRUE);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == sizeof (CK_BBOOL));
	g_assert (*((CK_BBOOL*)attr->value) == CK_TRUE);

	gck_attribute_free (attr);
}

static void
test_new_date (void)
{
	GckAttribute *attr;
	CK_DATE ck_date;
	GDate *date;

	date = g_date_new_dmy(05, 06, 1800);
	memcpy (ck_date.year, "1800", 4);
	memcpy (ck_date.month, "06", 2);
	memcpy (ck_date.day, "05", 2);
	attr = gck_attribute_new_date (ATTR_TYPE, date);
	g_date_free (date);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == sizeof (CK_DATE));
	g_assert (memcmp (attr->value, &ck_date, attr->length) == 0);

	gck_attribute_free (attr);
}

static void
test_new_ulong (void)
{
	GckAttribute *attr;

	attr = gck_attribute_new_ulong (ATTR_TYPE, 88);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == sizeof (CK_ULONG));
	g_assert (*((CK_ULONG*)attr->value) == 88);

	gck_attribute_free (attr);
}

static void
test_new_string (void)
{
	GckAttribute *attr;

	attr = gck_attribute_new_string (ATTR_TYPE, "a test string");
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == strlen ("a test string"));
	g_assert (memcmp (attr->value, "a test string", attr->length) == 0);

	gck_attribute_free (attr);
}

static void
test_new_invalid (void)
{
	GckAttribute *attr;

	attr = gck_attribute_new_invalid (ATTR_TYPE);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == (gulong)-1);
	g_assert (attr->value == NULL);

	g_assert (gck_attribute_is_invalid (attr));

	gck_attribute_free (attr);
}

static void
test_new_empty (void)
{
	GckAttribute *attr;

	attr = gck_attribute_new_empty (ATTR_TYPE);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == 0);
	g_assert (attr->value == NULL);

	gck_attribute_free (attr);
}

static void
test_get_boolean (void)
{
	GckAttribute *attr;

	attr = gck_attribute_new_boolean (ATTR_TYPE, TRUE);
	g_assert (gck_attribute_get_boolean (attr) == TRUE);
	gck_attribute_free (attr);
}

static void
test_get_date (void)
{
	GckAttribute *attr;
	CK_DATE ck_date;
	GDate date, date2;

	g_date_set_dmy(&date, 05, 06, 1800);
	memcpy (ck_date.year, "1800", 4);
	memcpy (ck_date.month, "06", 2);
	memcpy (ck_date.day, "05", 2);
	attr = gck_attribute_new_date (ATTR_TYPE, &date);
	gck_attribute_get_date (attr, &date2);
	g_assert (g_date_compare (&date, &date2) == 0);
	gck_attribute_free (attr);
}

static void
test_get_ulong (void)
{
	GckAttribute *attr;

	attr = gck_attribute_new_ulong (ATTR_TYPE, 88);
	g_assert (gck_attribute_get_ulong (attr) == 88);
	gck_attribute_free (attr);
}

static void
test_get_string (void)
{
	GckAttribute *attr;
	gchar *value;

	attr = gck_attribute_new_string (ATTR_TYPE, "a test string");
	value = gck_attribute_get_string (attr);
	g_assert (strcmp ("a test string", value) == 0);
	g_free (value);
	gck_attribute_free (attr);

	/* Should be able to store null strings */
	attr = gck_attribute_new_string (ATTR_TYPE, NULL);
	value = gck_attribute_get_string (attr);
	g_assert (value == NULL);
	gck_attribute_free (attr);
}

static void
test_dup_attribute (void)
{
	GckAttribute attr, *dup;

	gck_attribute_init_ulong (&attr, ATTR_TYPE, 88);
	dup = gck_attribute_dup (&attr);
	gck_attribute_clear (&attr);
	g_assert (gck_attribute_get_ulong (dup) == 88);
	g_assert (dup->type == ATTR_TYPE);
	gck_attribute_free (dup);

	/* Should be able to dup null */
	dup = gck_attribute_dup (NULL);
	g_assert (dup == NULL);
}

static void
test_copy_attribute (void)
{
	GckAttribute attr, copy;

	gck_attribute_init_ulong (&attr, ATTR_TYPE, 88);
	gck_attribute_init_copy (&copy, &attr);
	gck_attribute_clear (&attr);
	g_assert (gck_attribute_get_ulong (&copy) == 88);
	g_assert (copy.type == ATTR_TYPE);
	gck_attribute_clear (&copy);
}

static void
test_new_attributes (void)
{
	GckAttributes *attrs;

	attrs = gck_attributes_new ();
	g_assert (attrs != NULL);
	g_assert (gck_attributes_count (attrs) == 0);

	gck_attributes_ref (attrs);
	gck_attributes_unref (attrs);

	gck_attributes_unref (attrs);

	/* Can unref NULL */
	gck_attributes_unref (NULL);
}

static void
test_attributes_contents (GckAttributes *attrs, gboolean extras)
{
	GckAttribute *attr;
	gchar *value;
	GDate date, *check;
	guint count;

	g_assert (attrs != NULL);
	count = extras ? 7 : 5;
	g_assert_cmpuint (gck_attributes_count (attrs), ==, count);

	attr = gck_attributes_at (attrs, 0);
	g_assert (attr->type == 0);
	g_assert (gck_attribute_get_boolean (attr) == TRUE);

	attr = gck_attributes_at (attrs, 1);
	g_assert (attr->type == 101);
	g_assert (gck_attribute_get_ulong (attr) == 888);

	attr = gck_attributes_at (attrs, 2);
	g_assert (attr->type == 202);
	value = gck_attribute_get_string (attr);
	g_assert (strcmp (value, "string") == 0);
	g_free (value);

	attr = gck_attributes_at (attrs, 3);
	g_assert (attr->type == 303);
	check = g_date_new_dmy (11, 12, 2008);
	gck_attribute_get_date (attr, &date);
	g_assert (g_date_compare (&date, check) == 0);
	g_date_free (check);

	attr = gck_attributes_at (attrs, 4);
	g_assert (attr->type == 404);
	g_assert (attr->length == N_ATTR_DATA);
	g_assert (memcmp (attr->value, ATTR_DATA, N_ATTR_DATA) == 0);

	if (!extras)
		return;

	attr = gck_attributes_at (attrs, 5);
	g_assert (attr->type == 505);
	g_assert (attr->length == (gulong)-1);
	g_assert (attr->value == NULL);
	g_assert (gck_attribute_is_invalid (attr));

	attr = gck_attributes_at (attrs, 6);
	g_assert (attr->type == 606);
	g_assert (attr->length == 0);
	g_assert (attr->value == NULL);
}

static void
test_new_empty_attributes (void)
{
	GckAttributes *attrs = gck_attributes_new_empty (101UL, 202UL, 303UL, 404UL, GCK_INVALID);
	GckAttribute *attr;
	guint i;

	g_assert_cmpuint (gck_attributes_count (attrs), ==, 4);
	for (i = 0; i < gck_attributes_count (attrs); ++i) {
		attr = gck_attributes_at (attrs, i);
		g_assert (attr->type == ((i + 1) * 100) + i + 1);
		g_assert (attr->value == NULL);
		g_assert (attr->length == 0);
	}
}

static void
test_add_data_attributes (void)
{
	GckAttributes *attrs;
	GDate *date = g_date_new_dmy (11, 12, 2008);
	attrs = gck_attributes_new ();
	gck_attributes_add_boolean (attrs, 0UL, TRUE);
	gck_attributes_add_ulong (attrs, 101UL, 888);
	gck_attributes_add_string (attrs, 202UL, "string");
	gck_attributes_add_date (attrs, 303UL, date);
	g_date_free (date);
	gck_attributes_add_data (attrs, 404UL, ATTR_DATA, N_ATTR_DATA);
	gck_attributes_add_invalid (attrs, 505UL);
	gck_attributes_add_empty (attrs, 606UL);
	test_attributes_contents (attrs, TRUE);
	gck_attributes_unref (attrs);
}

static void
test_add_attributes (void)
{
	GckAttributes *attrs;
	GckAttribute attr;

	GDate *date = g_date_new_dmy (11, 12, 2008);
	attrs = gck_attributes_new ();

	gck_attribute_init_boolean (&attr, 0UL, TRUE);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);

	gck_attribute_init_ulong (&attr, 101UL, 888);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);
	gck_attribute_init_string (&attr, 202UL, "string");
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);

	gck_attribute_init_date (&attr, 303UL, date);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);
	g_date_free (date);

	gck_attribute_init (&attr, 404UL, ATTR_DATA, N_ATTR_DATA);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);

	gck_attribute_init_invalid (&attr, 505UL);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);

	gck_attribute_init_empty (&attr, 606UL);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);

	test_attributes_contents (attrs, TRUE);
	gck_attributes_unref (attrs);
}

static void
test_add_all_attributes (void)
{
	GckAttributes *attrs;
	GckAttributes *copy;
	GDate *date = g_date_new_dmy (11, 12, 2008);
	attrs = gck_attributes_new ();
	gck_attributes_add_boolean (attrs, 0UL, TRUE);
	gck_attributes_add_ulong (attrs, 101UL, 888);
	gck_attributes_add_string (attrs, 202UL, "string");
	gck_attributes_add_date (attrs, 303UL, date);
	g_date_free (date);
	gck_attributes_add_data (attrs, 404UL, ATTR_DATA, N_ATTR_DATA);
	gck_attributes_add_invalid (attrs, 505UL);
	gck_attributes_add_empty (attrs, 606UL);

	copy = gck_attributes_new ();
	gck_attributes_add_all (copy, attrs);
	test_attributes_contents (copy, TRUE);

	gck_attributes_unref (attrs);
	gck_attributes_unref (copy);
}


static void
test_find_attributes (void)
{
	GckAttribute *attr;
	GDate check, *date = g_date_new_dmy (13, 12, 2008);
	gboolean bvalue, ret;
	gulong uvalue;
	gchar *svalue;

	GckAttributes *attrs = gck_attributes_new ();
	gck_attributes_add_boolean (attrs, 0UL, TRUE);
	gck_attributes_add_ulong (attrs, 101UL, 888UL);
	gck_attributes_add_string (attrs, 202UL, "string");
	gck_attributes_add_date (attrs, 303UL, date);
	gck_attributes_add_data (attrs, 404UL, ATTR_DATA, N_ATTR_DATA);

	attr = gck_attributes_find (attrs, 404);
	g_assert (attr != NULL);
	g_assert (attr->length == N_ATTR_DATA);
	g_assert (memcmp (attr->value, ATTR_DATA, N_ATTR_DATA) == 0);

	ret = gck_attributes_find_boolean (attrs, 0UL, &bvalue);
	g_assert (ret == TRUE);
	g_assert (bvalue == TRUE);

	ret = gck_attributes_find_ulong (attrs, 101UL, &uvalue);
	g_assert (ret == TRUE);
	g_assert (uvalue == 888);

	ret = gck_attributes_find_string (attrs, 202UL, &svalue);
	g_assert (ret == TRUE);
	g_assert (svalue != NULL);
	g_assert (strcmp (svalue, "string") == 0);
	g_free (svalue);

	ret = gck_attributes_find_date (attrs, 303UL, &check);
	g_assert (ret == TRUE);
	g_assert (g_date_compare (date, &check) == 0);

	gck_attributes_unref (attrs);
	g_date_free (date);
}

int
main (int argc, char **argv)
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/gck/attributes/init_memory", test_init_memory);
	g_test_add_func ("/gck/attributes/value_to_boolean", test_value_to_boolean);
	g_test_add_func ("/gck/attributes/value_to_ulong", test_value_to_ulong);
	g_test_add_func ("/gck/attributes/init_boolean", test_init_boolean);
	g_test_add_func ("/gck/attributes/init_date", test_init_date);
	g_test_add_func ("/gck/attributes/init_ulong", test_init_ulong);
	g_test_add_func ("/gck/attributes/init_string", test_init_string);
	g_test_add_func ("/gck/attributes/init_invalid", test_init_invalid);
	g_test_add_func ("/gck/attributes/init_empty", test_init_empty);
	g_test_add_func ("/gck/attributes/new_memory", test_new_memory);
	g_test_add_func ("/gck/attributes/new_boolean", test_new_boolean);
	g_test_add_func ("/gck/attributes/new_date", test_new_date);
	g_test_add_func ("/gck/attributes/new_ulong", test_new_ulong);
	g_test_add_func ("/gck/attributes/new_string", test_new_string);
	g_test_add_func ("/gck/attributes/new_invalid", test_new_invalid);
	g_test_add_func ("/gck/attributes/new_empty", test_new_empty);
	g_test_add_func ("/gck/attributes/get_boolean", test_get_boolean);
	g_test_add_func ("/gck/attributes/get_date", test_get_date);
	g_test_add_func ("/gck/attributes/get_ulong", test_get_ulong);
	g_test_add_func ("/gck/attributes/get_string", test_get_string);
	g_test_add_func ("/gck/attributes/dup_attribute", test_dup_attribute);
	g_test_add_func ("/gck/attributes/copy_attribute", test_copy_attribute);
	g_test_add_func ("/gck/attributes/new_attributes", test_new_attributes);
	g_test_add_func ("/gck/attributes/new_empty_attributes", test_new_empty_attributes);
	g_test_add_func ("/gck/attributes/add_data_attributes", test_add_data_attributes);
	g_test_add_func ("/gck/attributes/add_attributes", test_add_attributes);
	g_test_add_func ("/gck/attributes/add_all_attributes", test_add_all_attributes);
	g_test_add_func ("/gck/attributes/find_attributes", test_find_attributes);

	return g_test_run ();
}
