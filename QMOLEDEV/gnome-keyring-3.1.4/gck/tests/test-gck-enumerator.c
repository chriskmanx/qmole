/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-gck-enumerator.c - the GObject PKCS#11 wrapper library

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
	GckModule *module;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	GError *err = NULL;

	/* Successful load */
	test->module = gck_module_initialize (BUILDDIR "/.libs/libmock-test-module.so", &err);
	g_assert_no_error (err);
	g_assert (GCK_IS_MODULE (test->module));

	test->modules = g_list_append (NULL, g_object_ref (test->module));
}

static void
teardown (Test *test, gconstpointer unused)
{
	gck_list_unref_free (test->modules);
	test->modules = NULL;

	g_object_unref (test->module);
	test->module = NULL;
}

static void
test_create (Test *test, gconstpointer unused)
{
	GckUriData *uri_data;
	GckEnumerator *en;

	uri_data = gck_uri_data_new ();
	en = _gck_enumerator_new (test->modules, 0, uri_data);
	g_assert (GCK_IS_ENUMERATOR (en));
	g_object_unref (en);
}

static void
test_create_slots (Test *test, gconstpointer unused)
{
	GckUriData *uri_data;
	GckEnumerator *en;
	GList *slots;

	uri_data = gck_uri_data_new ();
	slots = gck_module_get_slots (test->module, FALSE);
	en = _gck_enumerator_new (slots, 0, uri_data);
	g_assert (GCK_IS_ENUMERATOR (en));
	g_object_unref (en);
	gck_list_unref_free (slots);
}

static void
test_next (Test *test, gconstpointer unused)
{
	GckUriData *uri_data;
	GError *error = NULL;
	GckEnumerator *en;
	GckObject *obj;

	uri_data = gck_uri_data_new ();
	en = _gck_enumerator_new (test->modules, 0, uri_data);
	g_assert (GCK_IS_ENUMERATOR (en));

	obj = gck_enumerator_next (en, NULL, &error);
	g_assert (GCK_IS_OBJECT (obj));

	g_object_unref (obj);
	g_object_unref (en);
}

static void
test_next_slots (Test *test, gconstpointer unused)
{
	GckUriData *uri_data;
	GError *error = NULL;
	GList *slots = NULL;
	GckEnumerator *en;
	GckObject *obj;

	uri_data = gck_uri_data_new ();
	slots = gck_module_get_slots (test->module, FALSE);
	en = _gck_enumerator_new (slots, 0, uri_data);
	g_assert (GCK_IS_ENUMERATOR (en));

	obj = gck_enumerator_next (en, NULL, &error);
	g_assert (GCK_IS_OBJECT (obj));

	g_object_unref (obj);
	g_object_unref (en);
	gck_list_unref_free (slots);
}

static void
test_next_and_resume (Test *test, gconstpointer unused)
{
	GckUriData *uri_data;
	GError *error = NULL;
	GckEnumerator *en;
	GckObject *obj, *obj2;

	uri_data = gck_uri_data_new ();
	en = _gck_enumerator_new (test->modules, 0, uri_data);
	g_assert (GCK_IS_ENUMERATOR (en));

	obj = gck_enumerator_next (en, NULL, &error);
	g_assert_no_error (error);
	g_assert (GCK_IS_OBJECT (obj));

	obj2 = gck_enumerator_next (en, NULL, &error);
	g_assert_no_error (error);
	g_assert (GCK_IS_OBJECT (obj2));

	g_assert (!gck_object_equal (obj, obj2));

	g_object_unref (obj);
	g_object_unref (obj2);
	g_object_unref (en);
}

static void
test_next_n (Test *test, gconstpointer unused)
{
	GckUriData *uri_data;
	GError *error = NULL;
	GckEnumerator *en;
	GList *objects, *l;

	uri_data = gck_uri_data_new ();
	en = _gck_enumerator_new (test->modules, 0, uri_data);
	g_assert (GCK_IS_ENUMERATOR (en));

	objects = gck_enumerator_next_n (en, -1, NULL, &error);
	g_assert_no_error (error);
	g_assert_cmpint (g_list_length (objects), ==, 5);
	for (l = objects; l; l = g_list_next (l))
		g_assert (GCK_IS_OBJECT (l->data));

	gck_list_unref_free (objects);
	g_object_unref (en);
}

static void
fetch_async_result (GObject *source, GAsyncResult *result, gpointer user_data)
{
	*((GAsyncResult**)user_data) = result;
	g_object_ref (result);
	egg_test_wait_stop ();
}

static void
test_next_async (Test *test, gconstpointer unused)
{
	GckUriData *uri_data;
	GAsyncResult *result = NULL;
	GError *error = NULL;
	GckEnumerator *en;
	GList *objects, *l;

	uri_data = gck_uri_data_new ();
	en = _gck_enumerator_new (test->modules, 0, uri_data);
	g_assert (GCK_IS_ENUMERATOR (en));

	gck_enumerator_next_async (en, -1, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result);

	objects = gck_enumerator_next_finish (en, result, &error);
	g_assert_no_error (error);
	g_assert_cmpint (g_list_length (objects), ==, 5);
	for (l = objects; l; l = g_list_next (l))
		g_assert (GCK_IS_OBJECT (l->data));

	g_object_unref (result);
	gck_list_unref_free (objects);
	g_object_unref (en);
}

static void
test_attributes (Test *test, gconstpointer unused)
{
	GckUriData *uri_data;
	GError *error = NULL;
	GckEnumerator *en;
	GList *objects;

	uri_data = gck_uri_data_new ();
	uri_data->attributes = gck_attributes_new ();
	gck_attributes_add_string (uri_data->attributes, CKA_LABEL, "Private Capitalize Key");
	en = _gck_enumerator_new (test->modules, 0, uri_data);
	g_assert (GCK_IS_ENUMERATOR (en));

	objects = gck_enumerator_next_n (en, -1, NULL, &error);
	g_assert_no_error (error);
	g_assert_cmpint (g_list_length (objects), ==, 1);
	g_assert (GCK_IS_OBJECT (objects->data));

	gck_list_unref_free (objects);
	g_object_unref (en);
}

static void
test_token_match (Test *test, gconstpointer unused)
{
	GckUriData *uri_data;
	GError *error = NULL;
	GckEnumerator *en;
	GList *objects;

	uri_data = gck_uri_data_new ();
	uri_data->token_info = g_new0 (GckTokenInfo, 1);
	uri_data->token_info->label = g_strdup ("Invalid token name");
	en = _gck_enumerator_new (test->modules, 0, uri_data);
	g_assert (GCK_IS_ENUMERATOR (en));

	objects = gck_enumerator_next_n (en, -1, NULL, &error);
	g_assert_cmpint (g_list_length (objects), ==, 0);
	g_assert (error == NULL);

	gck_list_unref_free (objects);
	g_object_unref (en);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gck/enumerator/create", Test, NULL, setup, test_create, teardown);
	g_test_add ("/gck/enumerator/create_slots", Test, NULL, setup, test_create_slots, teardown);
	g_test_add ("/gck/enumerator/next", Test, NULL, setup, test_next, teardown);
	g_test_add ("/gck/enumerator/next_slots", Test, NULL, setup, test_next_slots, teardown);
	g_test_add ("/gck/enumerator/next_and_resume", Test, NULL, setup, test_next_and_resume, teardown);
	g_test_add ("/gck/enumerator/next_n", Test, NULL, setup, test_next_n, teardown);
	g_test_add ("/gck/enumerator/next_async", Test, NULL, setup, test_next_async, teardown);
	g_test_add ("/gck/enumerator/attributes", Test, NULL, setup, test_attributes, teardown);
	g_test_add ("/gck/enumerator/token_match", Test, NULL, setup, test_token_match, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
