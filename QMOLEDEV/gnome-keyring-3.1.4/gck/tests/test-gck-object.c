/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-gck-object.c - the GObject PKCS#11 wrapper library

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
#include "gck/gck-test.h"

#include "egg/egg-testing.h"

#include <glib.h>

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	GckModule *module;
	GckSlot *slot;
	GckSession *session;
	GckObject *object;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	GError *err = NULL;
	GList *slots;

	/* Successful load */
	test->module = gck_module_initialize (BUILDDIR "/.libs/libmock-test-module.so", &err);
	g_assert_no_error (err);
	g_assert (GCK_IS_MODULE (test->module));

	slots = gck_module_get_slots (test->module, TRUE);
	g_assert (slots != NULL);

	test->slot = GCK_SLOT (slots->data);
	g_object_ref (test->slot);
	gck_list_unref_free (slots);

	test->session = gck_slot_open_session (test->slot, 0, NULL, &err);
	g_assert_no_error (err);
	g_assert (GCK_IS_SESSION (test->session));

	/* Our module always exports a token object with this */
	test->object = gck_object_from_handle (test->session, 2);
	g_assert (test->object != NULL);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->object);
	g_object_unref (test->session);
	g_object_unref (test->slot);
	g_object_unref (test->module);
}

static void
test_object_props (Test *test, gconstpointer unused)
{
	GckSession *sess;
	GckModule *mod;
	CK_OBJECT_HANDLE handle;
	g_object_get (test->object, "session", &sess, "module", &mod, "handle", &handle, NULL);
	g_assert (test->session == sess);
	g_object_unref (sess);
	g_assert (test->module == mod);
	g_object_unref (mod);
	g_assert (handle == 2);
}

static void
test_object_equals_hash (Test *test, gconstpointer unused)
{
	GckSlot *other_slot;
	GckSession *other_session;
	GckObject *other_object;
	GObject *obj;
	GError *err = NULL;
	guint hash;

	hash = gck_object_hash (test->object);
	g_assert (hash != 0);

	g_assert (gck_object_equal (test->object, test->object));

	other_slot = g_object_new (GCK_TYPE_SLOT, "module", test->module, "handle", GCK_MOCK_SLOT_TWO_ID, NULL);
	other_session = gck_slot_open_session (other_slot, 0, NULL, &err);
	g_assert_no_error (err);
	g_assert (GCK_IS_SESSION (other_session));
	other_object = gck_object_from_handle (other_session, gck_object_get_handle (test->object));
	g_assert (!gck_object_equal (test->object, other_object));
	g_object_unref (other_slot);
	g_object_unref (other_session);
	g_object_unref (other_object);

	obj = g_object_new (G_TYPE_OBJECT, NULL);
	g_assert (!gck_object_equal (test->object, obj));
	g_object_unref (obj);

	other_object = gck_object_from_handle (test->session, 383838);
	g_assert (!gck_object_equal (test->object, other_object));
	g_object_unref (other_object);

	other_object = gck_object_from_handle (test->session, gck_object_get_handle (test->object));
	g_assert (gck_object_equal (test->object, other_object));
	g_object_unref (other_object);
}

static void
fetch_async_result (GObject *source, GAsyncResult *result, gpointer user_data)
{
	*((GAsyncResult**)user_data) = result;
	g_object_ref (result);
	egg_test_wait_stop ();
}

static void
test_create_object (Test *test, gconstpointer unused)
{
	GAsyncResult *result = NULL;
	GckAttributes *attrs;
	GckObject *object;
	CK_OBJECT_HANDLE last_handle;
	GError *err = NULL;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_DATA);
	gck_attributes_add_string (attrs, CKA_LABEL, "TEST LABEL");
	gck_attributes_add_boolean (attrs, CKA_TOKEN, CK_FALSE);
	gck_attributes_add_data (attrs, CKA_VALUE, "BLAH", 4);

	object = gck_session_create_object (test->session, attrs, NULL, &err);
	g_assert (GCK_IS_OBJECT (object));
	g_assert_no_error (err);

	last_handle = gck_object_get_handle (object);
	g_object_unref (object);

	/* Using async */
	gck_session_create_object_async (test->session, attrs, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);

	object = gck_session_create_object_finish (test->session, result, &err);
	g_object_unref (result);
	g_assert_no_error (err);
	g_assert (GCK_IS_OBJECT (object));

	g_assert (last_handle != gck_object_get_handle (object));
	g_object_unref (object);

	gck_attributes_unref (attrs);
}

static void
test_destroy_object (Test *test, gconstpointer unused)
{
	GAsyncResult *result = NULL;
	GckAttributes *attrs;
	GckObject *object;
	GError *err = NULL;
	gboolean ret;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_DATA);
	gck_attributes_add_string (attrs, CKA_LABEL, "TEST OBJECT");
	gck_attributes_add_boolean (attrs, CKA_TOKEN, CK_TRUE);

	/* Using simple */
	object = gck_session_create_object (test->session, attrs, NULL, &err);
	g_assert_no_error (err);
	g_assert (GCK_IS_OBJECT (object));

	ret = gck_object_destroy (object, NULL, &err);
	g_assert_no_error (err);
	g_assert (ret);
	g_object_unref (object);

	/* Using async */
	object = gck_session_create_object (test->session, attrs, NULL, &err);
	g_assert_no_error (err);
	g_assert (GCK_IS_OBJECT (object));

	/* Using async */
	gck_object_destroy_async (object, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);

	ret = gck_object_destroy_finish (object, result, &err);
	g_object_unref (result);
	g_assert_no_error (err);
	g_assert (ret);
	g_object_unref (object);
}

static void
test_get_attributes (Test *test, gconstpointer unused)
{
	GAsyncResult *result = NULL;
	GckAttributes *attrs;
	gulong attr_types[2];
	GError *err = NULL;
	gulong klass;
	gchar *value = NULL;

	attr_types[0] = CKA_CLASS;
	attr_types[1] = CKA_LABEL;

	/* Simple */
	attrs = gck_object_get (test->object, NULL, &err, CKA_CLASS, CKA_LABEL, GCK_INVALID);
	g_assert_no_error (err);
	if (attrs != NULL) {
		g_assert (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == CKO_DATA);
		g_assert (gck_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "TEST LABEL") == 0);
		g_free (value); value = NULL;
	}
	gck_attributes_unref (attrs);

	/* Full */
	attrs = gck_object_get_full (test->object, attr_types, G_N_ELEMENTS (attr_types), NULL, &err);
	g_assert_no_error (err);
	g_assert (attrs);
	g_assert (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == CKO_DATA);
	g_assert (gck_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "TEST LABEL") == 0);
	g_free (value); value = NULL;
	gck_attributes_unref (attrs);

	/* Async */
	gck_object_get_async (test->object, attr_types, G_N_ELEMENTS (attr_types), NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);

	attrs = gck_object_get_finish (test->object, result, &err);
	g_object_unref (result);
	g_assert_no_error (err);
	g_assert (attrs);
	g_assert (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == CKO_DATA);
	g_assert (gck_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "TEST LABEL") == 0);
	g_free (value); value = NULL;
	gck_attributes_unref (attrs);
}

static void
test_get_data_attribute (Test *test, gconstpointer unused)
{
	GAsyncResult *result = NULL;
	CK_OBJECT_CLASS_PTR klass;
	gsize n_data;
	GError *err = NULL;

	/* Simple */
	klass = gck_object_get_data (test->object, CKA_CLASS, NULL, &n_data, &err);
	g_assert_no_error (err);
	g_assert (klass);
	g_assert (n_data == sizeof (CK_OBJECT_CLASS));
	g_assert (*klass == CKO_DATA);
	g_free (klass);

	/* Full */
	klass = gck_object_get_data_full (test->object, CKA_CLASS, NULL, NULL, &n_data, &err);
	g_assert_no_error (err);
	g_assert (klass);
	g_assert (n_data == sizeof (CK_OBJECT_CLASS));
	g_assert (*klass == CKO_DATA);
	g_free (klass);

	/* Async */
	gck_object_get_data_async (test->object, CKA_CLASS, NULL, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);

	klass = gck_object_get_data_finish (test->object, result, &n_data, &err);
	g_object_unref (result);
	g_assert_no_error (err);
	g_assert (klass);
	g_assert (n_data == sizeof (CK_OBJECT_CLASS));
	g_assert (*klass == CKO_DATA);
	g_free (klass);

}

static void
test_set_attributes (Test *test, gconstpointer unused)
{
	GAsyncResult *result = NULL;
	GckAttributes *attrs, *templ;
	GError *err = NULL;
	gulong klass;
	gchar *value = NULL;
	gboolean ret;

	templ = gck_attributes_new ();
	gck_attributes_add_ulong (templ, CKA_CLASS, 6);
	gck_attributes_add_string (templ, CKA_LABEL, "CHANGE TWO");

	/* Full */
	ret = gck_object_set (test->object, templ, NULL, &err);
	gck_attributes_unref (templ);
	g_assert_no_error (err);
	g_assert (ret);
	attrs = gck_object_get (test->object, NULL, &err, CKA_CLASS, CKA_LABEL, GCK_INVALID);
	g_assert (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == 6);
	g_assert (gck_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "CHANGE TWO") == 0);
	g_free (value); value = NULL;
	gck_attributes_unref (attrs);

	templ = gck_attributes_new ();
	gck_attributes_add_ulong (templ, CKA_CLASS, 7);
	gck_attributes_add_string (templ, CKA_LABEL, "CHANGE THREE");

	/* Async */
	gck_object_set_async (test->object, templ, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);

	ret = gck_object_set_finish (test->object, result, &err);
	g_object_unref (result);
	g_assert_no_error (err);
	g_assert (ret);
	attrs = gck_object_get (test->object, NULL, &err, CKA_CLASS, CKA_LABEL, GCK_INVALID);
	g_assert (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == 7);
	g_assert (gck_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "CHANGE THREE") == 0);
	g_free (value); value = NULL;
	gck_attributes_unref (attrs);
}

static void
test_find_objects (Test *test, gconstpointer unused)
{
	GAsyncResult *result = NULL;
	GckAttributes *templ, *attrs;
	GList *objects;
	GckObject *testobj;
	GError *err = NULL;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_DATA);
	gck_attributes_add_string (attrs, CKA_LABEL, "UNIQUE LABEL");
	testobj = gck_session_create_object (test->session, attrs, NULL, &err);
	gck_attributes_unref (attrs);
	g_object_unref (testobj);

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_DATA);
	gck_attributes_add_string (attrs, CKA_LABEL, "OTHER LABEL");
	testobj = gck_session_create_object (test->session, attrs, NULL, &err);
	gck_attributes_unref (attrs);
	g_object_unref (testobj);

	/* Simple, "TEST LABEL" */
	attrs = gck_attributes_new ();
	gck_attributes_add_string (attrs, CKA_LABEL, "UNIQUE LABEL");
	objects = gck_session_find_objects (test->session, attrs, NULL, &err);
	g_assert_no_error (err);
	g_assert (g_list_length (objects) == 1);
	gck_list_unref_free (objects);
	gck_attributes_unref (attrs);

	/* Full, All */
	templ = gck_attributes_new ();
	objects = gck_session_find_objects (test->session, templ, NULL, &err);
	g_assert_no_error (err);
	g_assert (g_list_length (objects) > 1);
	gck_list_unref_free (objects);

	/* Async, None */
	gck_attributes_add_string (templ, CKA_LABEL, "blah blah");
	gck_session_find_objects_async (test->session, templ, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);

	objects = gck_session_find_objects_finish (test->session, result, &err);
	g_object_unref (result);
	g_assert (objects == NULL);
	gck_list_unref_free (objects);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gck/object/object_props", Test, NULL, setup, test_object_props, teardown);
	g_test_add ("/gck/object/object_equals_hash", Test, NULL, setup, test_object_equals_hash, teardown);
	g_test_add ("/gck/object/create_object", Test, NULL, setup, test_create_object, teardown);
	g_test_add ("/gck/object/destroy_object", Test, NULL, setup, test_destroy_object, teardown);
	g_test_add ("/gck/object/get_attributes", Test, NULL, setup, test_get_attributes, teardown);
	g_test_add ("/gck/object/get_data_attribute", Test, NULL, setup, test_get_data_attribute, teardown);
	g_test_add ("/gck/object/set_attributes", Test, NULL, setup, test_set_attributes, teardown);
	g_test_add ("/gck/object/find_objects", Test, NULL, setup, test_find_objects, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
