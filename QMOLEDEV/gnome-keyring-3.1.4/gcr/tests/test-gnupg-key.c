/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/*
   Copyright (C) 2011 Collabora Ltd

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
#include "gcr/gcr-gnupg-key.h"

#include "egg/egg-testing.h"

#include <glib.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
	GPtrArray *records;
	GPtrArray *pubset;
	GPtrArray *secset;
	GcrGnupgKey *key;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	GPtrArray *records;

	records = g_ptr_array_new_with_free_func (_gcr_record_free);
	g_ptr_array_add (records, _gcr_record_parse_colons ("pub:f:1024:17:6C7EE1B8621CC013:899817715:1055898235::m:::scESC:", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("fpr:::::::::ECAF7590EB3443B5C7CF3ACB6C7EE1B8621CC013:", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("uid:f::::::::Werner Koch <wk@g10code.com>:\n", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("uid:f::::::::Werner Koch <wk@gnupg.org>:\n", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("sub:f:1536:16:06AD222CADF6A6E1:919537416:1036177416:::::e:\n", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("fpr:::::::::CF8BCC4B18DE08FCD8A1615906AD222CADF6A6E1:\n", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("sub:r:1536:20:5CE086B5B5A18FF4:899817788:1025961788:::::esc:\n", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("fpr:::::::::AB059359A3B81F410FCFF97F5CE086B5B5A18FF4:", -1));
	test->records = records;

	test->key = _gcr_gnupg_key_new (records, NULL);

	records = g_ptr_array_new_with_free_func (_gcr_record_free);
	g_ptr_array_add (records, _gcr_record_parse_colons ("pub:u:2048:1:4842D952AFC000FD:1305189489:::u:::scESC:", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("uid:u::::1305189849::D0A8FA7B15DC4BE3F8F03A49C372F2718C78AFC0::Dr. Strangelove <lovingbomb@example.com>:", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("uid:u::::1305189489::D449F1605254754B0BBFA424FC34E50609103BBB::Test Number 1 (unlimited) <test-number-1@example.com>:", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("sub:u:2048:1:4852132BBED15014:1305189489::::::e:", -1));
	test->pubset = records;

	records = g_ptr_array_new_with_free_func (_gcr_record_free);
	g_ptr_array_add (records, _gcr_record_parse_colons ("sec::2048:1:4842D952AFC000FD:1305189489::::::::::", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("uid:::::::D449F1605254754B0BBFA424FC34E50609103BBB::Test Number 1 (unlimited) <test-number-1@example.com>:", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("uid:::::::D0A8FA7B15DC4BE3F8F03A49C372F2718C78AFC0::Dr. Strangelove <lovingbomb@example.com>:", -1));
	g_ptr_array_add (records, _gcr_record_parse_colons ("ssb::2048:1:4852132BBED15014:1305189489::::::::::", -1));
	test->secset = records;
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->key);
	g_assert (!GCR_IS_GNUPG_KEY (test->key));

	g_ptr_array_unref (test->records);
	g_ptr_array_unref (test->pubset);
	g_ptr_array_unref (test->secset);
}

static void
test_label (Test *test, gconstpointer unused)
{
	gchar *label;

	g_object_get (test->key, "label", &label, NULL);
	g_assert_cmpstr (label, ==, "Werner Koch <wk@g10code.com>");

	g_free (label);
}

static void
test_markup (Test *test, gconstpointer unused)
{
	gchar *markup;

	g_object_get (test->key, "markup", &markup, NULL);
	g_assert_cmpstr (markup, ==, "Werner Koch\n<small>wk@g10code.com</small>");

	g_free (markup);
}

static void
test_description (Test *test, gconstpointer unused)
{
	gchar *description;

	g_object_get (test->key, "description", &description, NULL);
	g_assert_cmpstr (description, ==, "PGP Key");

	g_free (description);
}

static void
test_records (Test *test, gconstpointer unused)
{
	GPtrArray *records;

	g_object_get (test->key, "public-records", &records, NULL);
	g_assert (records == test->records);

	_gcr_gnupg_key_set_public_records (test->key, records);
	g_assert (records == _gcr_gnupg_key_get_public_records (test->key));

	g_ptr_array_unref (records);
}

static void
test_keyid (Test *test, gconstpointer unused)
{
	gchar *keyid;

	g_object_get (test->key, "keyid", &keyid, NULL);
	g_assert_cmpstr (keyid, ==, "6C7EE1B8621CC013");

	g_free (keyid);

	g_assert_cmpstr (_gcr_gnupg_key_get_keyid (test->key), ==, "6C7EE1B8621CC013");
}

static void
test_short_keyid (Test *test, gconstpointer unused)
{
	gchar *keyid;

	g_object_get (test->key, "short-keyid", &keyid, NULL);
	g_assert_cmpstr (keyid, ==, "621CC013");

	g_free (keyid);
}

static void
test_keyid_for_records (Test *test, gconstpointer unused)
{
	const gchar *keyid;

	keyid = _gcr_gnupg_key_get_keyid_for_records (test->records);
	g_assert_cmpstr (keyid, ==, "6C7EE1B8621CC013");
}

static void
test_with_secret (Test *test, gconstpointer unused)
{
	GcrGnupgKey *key;
	GPtrArray *secset;

	key = _gcr_gnupg_key_new (test->pubset, test->secset);
	g_assert (GCR_IS_GNUPG_KEY (key));

	g_object_get (key, "secret-records", &secset, NULL);
	g_assert (secset == _gcr_gnupg_key_get_secret_records (key));
	g_object_set (key, "secret-records", secset, NULL);

	g_object_unref (key);
}

static void
test_no_change_keyid (Test *test, gconstpointer unused)
{
	if (g_test_trap_fork (50000, G_TEST_TRAP_SILENCE_STDERR)) {
		/* Changing the keyid. This should fail with a warning */
		_gcr_gnupg_key_set_public_records (test->key, test->pubset);
		exit (0);
	}

	g_test_trap_assert_failed ();
	g_test_trap_assert_stderr ("*fingerprint is no longer the same:*");
}

static void
test_secret_mismatched_keyid (Test *test, gconstpointer unused)
{
	if (g_test_trap_fork (50000, G_TEST_TRAP_SILENCE_STDERR)) {
		/* Different keyid for secret part. This should fail with a warning */
		_gcr_gnupg_key_set_secret_records (test->key, test->secset);
		exit (0);
	}

	g_test_trap_assert_failed ();
	g_test_trap_assert_stderr ("*pub and sec parts are not the same:*");
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gcr/gnupg-key/label", Test, NULL, setup, test_label, teardown);
	g_test_add ("/gcr/gnupg-key/description", Test, NULL, setup, test_description, teardown);
	g_test_add ("/gcr/gnupg-key/markup", Test, NULL, setup, test_markup, teardown);
	g_test_add ("/gcr/gnupg-key/records", Test, NULL, setup, test_records, teardown);
	g_test_add ("/gcr/gnupg-key/keyid", Test, NULL, setup, test_keyid, teardown);
	g_test_add ("/gcr/gnupg-key/short_keyid", Test, NULL, setup, test_short_keyid, teardown);
	g_test_add ("/gcr/gnupg-key/keyid_for_records", Test, NULL, setup, test_keyid_for_records, teardown);
	g_test_add ("/gcr/gnupg-key/with_secret", Test, NULL, setup, test_with_secret, teardown);
	g_test_add ("/gcr/gnupg-key/no_change_keyid", Test, NULL, setup, test_no_change_keyid, teardown);
	g_test_add ("/gcr/gnupg-key/secret_mismatched_keyid", Test, NULL, setup, test_secret_mismatched_keyid, teardown);

	return g_test_run ();
}
