/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-gck-modules.c - the GObject PKCS#11 wrapper library

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

#include "gck/gck.h"
#include "gck/gck-mock.h"
#include "gck/gck-private.h"
#include "gck/gck-test.h"

#include "egg/egg-testing.h"

#include <glib.h>

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	GList *modules;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	GckModule *module;
	GError *err = NULL;

	/* Successful load */
	module = gck_module_initialize (BUILDDIR "/.libs/libmock-test-module.so", &err);
	g_assert_no_error (err);
	g_assert (GCK_IS_MODULE (module));

	test->modules = g_list_append (NULL, module);
}

static void
teardown (Test *test, gconstpointer unused)
{
	gck_list_unref_free (test->modules);
	test->modules = NULL;
}

static void
test_enumerate_objects (Test *test, gconstpointer unused)
{
	GckAttributes *attrs;
	GError *error = NULL;
	GckEnumerator *en;
	GList *objects;

	attrs = gck_attributes_new ();
	gck_attributes_add_string (attrs, CKA_LABEL, "Private Capitalize Key");
	en = gck_modules_enumerate_objects (test->modules, attrs, 0);
	g_assert (GCK_IS_ENUMERATOR (en));
	gck_attributes_unref (attrs);

	objects = gck_enumerator_next_n (en, -1, NULL, &error);
	g_assert_no_error (error);
	g_assert_cmpint (g_list_length (objects), ==, 1);
	g_assert (GCK_IS_OBJECT (objects->data));

	gck_list_unref_free (objects);
	g_object_unref (en);
}


static void
test_token_for_uri (Test *test, gconstpointer unused)
{
	GckSlot *slot;
	GError *error = NULL;

	slot = gck_modules_token_for_uri (test->modules, "pkcs11:token=TEST%20LABEL", &error);
	g_assert (GCK_IS_SLOT (slot));

	g_object_unref (slot);
}

static void
test_token_for_uri_not_found (Test *test, gconstpointer unused)
{
	GckSlot *slot;
	GError *error = NULL;

	slot = gck_modules_token_for_uri (test->modules, "pkcs11:token=UNKNOWN", &error);
	g_assert (slot == NULL);
	g_assert (error == NULL);
}

static void
test_token_for_uri_error (Test *test, gconstpointer unused)
{
	GckSlot *slot;
	GError *error = NULL;

	slot = gck_modules_token_for_uri (test->modules, "http://invalid.uri", &error);
	g_assert (slot == NULL);
	g_assert (error != NULL);
	g_assert (g_error_matches (error, GCK_URI_ERROR, GCK_URI_BAD_PREFIX));
	g_error_free (error);
}

static void
test_object_for_uri (Test *test, gconstpointer unused)
{
	GckObject *object;
	GError *error = NULL;

	object = gck_modules_object_for_uri (test->modules, "pkcs11:object=Public%20Capitalize%20Key;objecttype=public", 0, &error);
	g_assert (GCK_IS_OBJECT (object));
	g_object_unref (object);
}

static void
test_object_for_uri_not_found (Test *test, gconstpointer unused)
{
	GckObject *object;
	GError *error = NULL;

	object = gck_modules_object_for_uri (test->modules, "pkcs11:object=Unknown%20Label", 0, &error);
	g_assert (object == NULL);
	g_assert (error == NULL);
}

static void
test_object_for_uri_error (Test *test, gconstpointer unused)
{
	GckObject *object;
	GError *error = NULL;

	object = gck_modules_object_for_uri (test->modules, "http://invalid.uri", 0, &error);
	g_assert (object == NULL);
	g_assert (error != NULL);
	g_assert (g_error_matches (error, GCK_URI_ERROR, GCK_URI_BAD_PREFIX));
	g_error_free (error);
}

static void
test_objects_for_uri (Test *test, gconstpointer unused)
{
	GList *objects;
	GError *error = NULL;

	objects = gck_modules_objects_for_uri (test->modules, "pkcs11:token=TEST%20LABEL", 0, &error);
	g_assert (objects);
	g_assert (!error);
	g_assert_cmpint (g_list_length (objects), ==, 5);

	gck_list_unref_free (objects);
}

static void
test_enumerate_uri (Test *test, gconstpointer unused)
{
	GckEnumerator *en;
	GList *objects;
	GError *error = NULL;

	en = gck_modules_enumerate_uri (test->modules, "pkcs11:token=TEST%20LABEL", 0, &error);
	g_assert (GCK_IS_ENUMERATOR (en));
	g_assert (!error);

	objects = gck_enumerator_next_n (en, -1, NULL, &error);
	g_assert_cmpint (g_list_length (objects), ==, 5);
	g_assert (!error);

	g_object_unref (en);
	gck_list_unref_free (objects);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gck/modules/enumerate_objects", Test, NULL, setup, test_enumerate_objects, teardown);
	g_test_add ("/gck/modules/token_for_uri", Test, NULL, setup, test_token_for_uri, teardown);
	g_test_add ("/gck/modules/token_for_uri_not_found", Test, NULL, setup, test_token_for_uri_not_found, teardown);
	g_test_add ("/gck/modules/token_for_uri_error", Test, NULL, setup, test_token_for_uri_error, teardown);
	g_test_add ("/gck/modules/object_for_uri", Test, NULL, setup, test_object_for_uri, teardown);
	g_test_add ("/gck/modules/object_for_uri_not_found", Test, NULL, setup, test_object_for_uri_not_found, teardown);
	g_test_add ("/gck/modules/object_for_uri_error", Test, NULL, setup, test_object_for_uri_error, teardown);
	g_test_add ("/gck/modules/objects_for_uri", Test, NULL, setup, test_objects_for_uri, teardown);
	g_test_add ("/gck/modules/enumerate_uri", Test, NULL, setup, test_enumerate_uri, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
