/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/*
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

#include "gcr/gcr-record.h"

#include "egg/egg-testing.h"

#include <glib.h>

typedef struct {
	GcrRecord *record;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	test->record = _gcr_record_parse_colons ("one:tab\\there::YW9ldTM4Mzg=:f\xfc""nf:3533333:-88", -1);
}

static void
teardown (Test *test, gconstpointer unused)
{
	_gcr_record_free (test->record);
}

static void
test_count (Test *test, gconstpointer unused)
{
	g_assert_cmpuint (_gcr_record_get_count (test->record), ==, 7);
}

static void
test_parse_colons (void)
{
	GcrRecord *record;

	record = _gcr_record_parse_colons ("one:two::four::six", -1);
	g_assert (record);

	g_assert_cmpstr (_gcr_record_get_raw (record, 0), ==, "one");
	g_assert_cmpstr (_gcr_record_get_raw (record, 1), ==, "two");
	g_assert_cmpstr (_gcr_record_get_raw (record, 2), ==, "");
	g_assert_cmpstr (_gcr_record_get_raw (record, 3), ==, "four");
	g_assert_cmpstr (_gcr_record_get_raw (record, 4), ==, "");
	g_assert_cmpstr (_gcr_record_get_raw (record, 5), ==, "six");
	g_assert (_gcr_record_get_raw (record, 6) == NULL);

	_gcr_record_free (record);
}

static void
test_take_colons (void)
{
	GcrRecord *record;

	record = _gcr_record_take_colons (g_strdup ("one:two::four::six"));
	g_assert (record);

	g_assert_cmpstr (_gcr_record_get_raw (record, 0), ==, "one");
	g_assert_cmpstr (_gcr_record_get_raw (record, 1), ==, "two");
	g_assert_cmpstr (_gcr_record_get_raw (record, 2), ==, "");
	g_assert_cmpstr (_gcr_record_get_raw (record, 3), ==, "four");
	g_assert_cmpstr (_gcr_record_get_raw (record, 4), ==, "");
	g_assert_cmpstr (_gcr_record_get_raw (record, 5), ==, "six");
	g_assert (_gcr_record_get_raw (record, 6) == NULL);
	g_assert_cmpuint (_gcr_record_get_count (record), ==, 6);

	_gcr_record_free (record);
}


static void
test_parse_spaces (void)
{
	GcrRecord *record;

	record = _gcr_record_parse_spaces (" one two  four six   ", -1);
	g_assert (record);

	g_assert_cmpstr (_gcr_record_get_raw (record, 0), ==, "one");
	g_assert_cmpstr (_gcr_record_get_raw (record, 1), ==, "two");
	g_assert_cmpstr (_gcr_record_get_raw (record, 2), ==, "four");
	g_assert_cmpstr (_gcr_record_get_raw (record, 3), ==, "six");
	g_assert (_gcr_record_get_raw (record, 4) == NULL);
	g_assert_cmpuint (_gcr_record_get_count (record), ==, 4);

	_gcr_record_free (record);
}

static void
test_parse_part (void)
{
	GcrRecord *record;

	record = _gcr_record_parse_colons ("one:two::four::six", 8);
	g_assert (record);

	g_assert_cmpstr (_gcr_record_get_raw (record, 0), ==, "one");
	g_assert_cmpstr (_gcr_record_get_raw (record, 1), ==, "two");
	g_assert_cmpstr (_gcr_record_get_raw (record, 2), ==, "");
	g_assert (_gcr_record_get_raw (record, 3) == NULL);

	_gcr_record_free (record);
}

static void
test_parse_too_long (void)
{
	GcrRecord *record;

	/* Too many columns */
	record = _gcr_record_parse_colons (":::::::::::::::::::::::::::::::::::::::::::::::::::::", -1);
	g_assert (record == NULL);
}

static void
test_find (void)
{
	GcrRecord *uid, *pub, *one, *check;
	GPtrArray *records;

	records = g_ptr_array_new_with_free_func (_gcr_record_free);

	one = _gcr_record_parse_colons ("one:two::four::six", -1);
	g_ptr_array_add (records, one);
	pub = _gcr_record_parse_colons ("pub:two", -1);
	g_ptr_array_add (records, pub);
	uid = _gcr_record_parse_colons ("uid:two", -1);
	g_ptr_array_add (records, uid);

	check = _gcr_record_find (records, GCR_RECORD_SCHEMA_PUB);
	g_assert (check == pub);

	check = _gcr_record_find (records, GCR_RECORD_SCHEMA_UID);
	g_assert (check == uid);

	g_ptr_array_unref (records);
}

static void
test_get_string (Test *test, gconstpointer unused)
{
	gchar *value = _gcr_record_get_string (test->record, 1);
	g_assert (value);

	g_assert_cmpstr (value, ==, "tab\there");
	g_free (value);
}

static void
test_get_string_null (Test *test, gconstpointer unused)
{
	gchar *value = _gcr_record_get_string (test->record, 35);
	g_assert (value == NULL);
}

static void
test_get_string_latin1 (Test *test, gconstpointer unused)
{
	gchar *value = _gcr_record_get_string (test->record, 4);
	g_assert (value);

	g_assert_cmpstr (value, ==, "f\xc3\xbc""nf");
	g_assert (g_utf8_validate (value, -1, NULL));
	g_free (value);
}

static void
test_get_uint (Test *test, gconstpointer unused)
{
	guint value = 0;

	if (!_gcr_record_get_uint (test->record, 5, &value))
		g_assert_not_reached ();
	g_assert_cmpuint (value, ==, 3533333);
}

static void
test_get_uint_range (Test *test, gconstpointer unused)
{
	guint value = 0;

	if (_gcr_record_get_uint (test->record, 6, &value))
		g_assert_not_reached ();
}

static void
test_get_uint_invalid (Test *test, gconstpointer unused)
{
	guint value = 0;

	if (_gcr_record_get_uint (test->record, 0, &value))
		g_assert_not_reached ();
}

static void
test_get_base64 (Test *test, gconstpointer unused)
{
	gchar *value;
	gsize n_value;

	value = _gcr_record_get_base64 (test->record, 3, &n_value);
	g_assert (value);
	egg_assert_cmpsize (n_value, ==, 8);
	g_assert (memcmp (value, "aoeu3838", n_value) == 0);

	g_free (value);
}


static void
test_free_null (void)
{
	_gcr_record_free (NULL);
}

static void
test_get_schema (Test *test, gconstpointer unused)
{
	GQuark schema;
	GQuark check;

	/* Initialize this quark */
	check = g_quark_from_static_string ("one");

	schema = _gcr_record_get_schema (test->record);
	g_assert (check == schema);
	g_assert_cmpstr (g_quark_to_string (schema), ==, "one");
}

static void
test_copy (Test *test, gconstpointer unused)
{
	GcrRecord *copy;
	guint count, i;

	copy = _gcr_record_copy (test->record);

	count = _gcr_record_get_count (test->record);
	g_assert_cmpuint (_gcr_record_get_count (copy), ==, count);
	for (i = 0; i < count; i++) {
		g_assert_cmpstr (_gcr_record_get_raw (copy, i), ==,
		                 _gcr_record_get_raw (test->record, i));
	}

	g_assert (_gcr_record_get_raw (copy, count) == NULL);
	_gcr_record_free (copy);
}

static void
test_boxed (Test *test, gconstpointer unused)
{
	GcrRecord *copy;
	guint count, i;

	copy = g_boxed_copy (GCR_TYPE_RECORD, test->record);

	count = _gcr_record_get_count (test->record);
	g_assert_cmpuint (_gcr_record_get_count (copy), ==, count);
	for (i = 0; i < count; i++) {
		g_assert_cmpstr (_gcr_record_get_raw (copy, i), ==,
		                 _gcr_record_get_raw (test->record, i));
	}

	g_assert (_gcr_record_get_raw (copy, count) == NULL);
	g_boxed_free (GCR_TYPE_RECORD, copy);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/gcr/record/parse_colons", test_parse_colons);
	g_test_add_func ("/gcr/record/take_colons", test_take_colons);
	g_test_add_func ("/gcr/record/parse_colons", test_parse_spaces);
	g_test_add_func ("/gcr/record/parse_part", test_parse_part);
	g_test_add_func ("/gcr/record/parse_too_long", test_parse_too_long);
	g_test_add_func ("/gcr/record/free_null", test_free_null);
	g_test_add_func ("/gcr/record/find", test_find);
	g_test_add ("/gcr/record/count", Test, NULL, setup, test_count, teardown);
	g_test_add ("/gcr/record/copy", Test, NULL, setup, test_copy, teardown);
	g_test_add ("/gcr/record/boxed", Test, NULL, setup, test_boxed, teardown);
	g_test_add ("/gcr/record/get_string", Test, NULL, setup, test_get_string, teardown);
	g_test_add ("/gcr/record/get_string_null", Test, NULL, setup, test_get_string_null, teardown);
	g_test_add ("/gcr/record/get_string_latin1", Test, NULL, setup, test_get_string_latin1, teardown);
	g_test_add ("/gcr/record/get_uint", Test, NULL, setup, test_get_uint, teardown);
	g_test_add ("/gcr/record/get_uint_invalid", Test, NULL, setup, test_get_uint_invalid, teardown);
	g_test_add ("/gcr/record/get_uint_range", Test, NULL, setup, test_get_uint_range, teardown);
	g_test_add ("/gcr/record/get_base64", Test, NULL, setup, test_get_base64, teardown);
	g_test_add ("/gcr/record/get_schema", Test, NULL, setup, test_get_schema, teardown);

	return g_test_run ();
}
