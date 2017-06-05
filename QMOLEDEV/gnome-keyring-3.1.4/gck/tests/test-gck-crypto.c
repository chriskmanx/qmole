/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-gck-crypto.c - the GObject PKCS#11 wrapper library

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
	GckSession *session;
	GckSession *session_with_auth;
} Test;

static gboolean
on_discard_handle_ignore (GckSession *self, CK_OBJECT_HANDLE handle, gpointer unused)
{
	/* Don't close the handle for this session, since it's a duplicate */
	return TRUE;
}

static void
setup (Test *test, gconstpointer unused)
{
	GError *err = NULL;
	GList *slots;
	GckSlot *slot;

	/* Successful load */
	test->module = gck_module_initialize (BUILDDIR "/.libs/libmock-test-module.so", &err);
	g_assert_no_error (err);
	g_assert (GCK_IS_MODULE (test->module));

	slots = gck_module_get_slots (test->module, TRUE);
	g_assert (slots != NULL);

	test->session = gck_slot_open_session (slots->data, 0, NULL, &err);
	g_assert_no_error (err);
	g_assert (GCK_IS_SESSION (test->session));

	slot = gck_session_get_slot (test->session);
	g_assert (slot);

	test->session_with_auth = gck_session_from_handle (slot, gck_session_get_handle (test->session), GCK_SESSION_AUTHENTICATE);
	g_signal_connect (test->session_with_auth, "discard-handle", G_CALLBACK (on_discard_handle_ignore), NULL);
	g_assert (test->session_with_auth);

	g_object_unref (slot);
	gck_list_unref_free (slots);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->session);
	g_object_unref (test->module);
	g_object_unref (test->session_with_auth);
}

static void
fetch_async_result (GObject *source, GAsyncResult *result, gpointer user_data)
{
	*((GAsyncResult**)user_data) = result;
	g_object_ref (result);
	egg_test_wait_stop ();
}

static GckObject*
find_key (GckSession *session, CK_ATTRIBUTE_TYPE method, CK_MECHANISM_TYPE mech)
{
	GList *objects, *l;
	GckAttributes *attrs;
	GckObject *object = NULL;
	CK_MECHANISM_TYPE_PTR mechs;
	gsize n_mechs;

	attrs = gck_attributes_new ();
	gck_attributes_add_boolean (attrs, method, TRUE);
	objects = gck_session_find_objects (session, attrs, NULL, NULL);
	gck_attributes_unref (attrs);
	g_assert (objects);

	for (l = objects; l; l = g_list_next (l)) {
		if (mech) {
			mechs = gck_object_get_data (l->data, CKA_ALLOWED_MECHANISMS, NULL, &n_mechs, NULL);
			g_assert (mechs);
			g_assert (n_mechs == sizeof (CK_MECHANISM_TYPE));
			/* We know all of them only have one allowed mech */
			if (*mechs != mech)
				continue;
		}
		object = l->data;
		g_object_ref (object);
		break;
	}

	gck_list_unref_free (objects);
	return object;
}

static GckObject*
find_key_with_value (GckSession *session, const gchar *value)
{
	GList *objects;
	GckAttributes *attrs;
	GckObject *object;

	attrs = gck_attributes_new ();
	gck_attributes_add_string (attrs, CKA_VALUE, value);
	objects = gck_session_find_objects (session, attrs, NULL, NULL);
	gck_attributes_unref (attrs);
	g_assert (objects);

	object = g_object_ref (objects->data);
	gck_list_unref_free (objects);
	return object;
}

static void
check_key_with_value (GckSession *session, GckObject *key, CK_OBJECT_CLASS klass, const gchar *value)
{
	GckAttributes *attrs;
	GckAttribute *attr;
	gulong check;

	attrs = gck_object_get (key, NULL, NULL, CKA_CLASS, CKA_VALUE, GCK_INVALID);
	g_assert (attrs);

	if (!gck_attributes_find_ulong (attrs, CKA_CLASS, &check))
		g_assert_not_reached ();
	g_assert (check == klass);

	attr = gck_attributes_find (attrs, CKA_VALUE);
	g_assert (attr);
	g_assert (!gck_attribute_is_invalid (attr));
	egg_assert_cmpsize (attr->length, ==, strlen (value));
	g_assert (memcmp (attr->value, value, attr->length) == 0);

	gck_attributes_unref (attrs);
}

static gboolean
authenticate_object (GckSlot *module, GckObject *object, gchar *label, gchar **password)
{
	g_assert (GCK_IS_MODULE (module));
	g_assert (GCK_IS_OBJECT (object));
	g_assert (password);
	g_assert (!*password);

	*password = g_strdup ("booo");
	return TRUE;
}

static void
test_encrypt (Test *test, gconstpointer unused)
{
	GckMechanism mech = { CKM_MOCK_CAPITALIZE, NULL, 0 };
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GckObject *key;
	guchar *output;
	gsize n_output;

	/* Find the right key */
	key = find_key (test->session, CKA_ENCRYPT, CKM_MOCK_CAPITALIZE);
	g_assert (key);

	/* Simple one */
	output = gck_session_encrypt (test->session, key, CKM_MOCK_CAPITALIZE, (const guchar*)"blah blah", 10, &n_output, NULL, &error);
	g_assert_no_error (error);
	g_assert (output);
	g_assert (n_output == 10);
	g_assert_cmpstr ((gchar*)output, ==, "BLAH BLAH");
	g_free (output);

	/* Asynchronous one */
	gck_session_encrypt_async (test->session, key, &mech, (const guchar*)"second chance", 14, NULL, fetch_async_result, &result);

	egg_test_wait_until (500);
	g_assert (result != NULL);

	/* Get the result */
	output = gck_session_encrypt_finish (test->session, result, &n_output, &error);
	g_assert_no_error (error);
	g_assert (output);
	g_assert (n_output == 14);
	g_assert_cmpstr ((gchar*)output, ==, "SECOND CHANCE");
	g_free (output);

	g_object_unref (result);
	g_object_unref (key);
}

static void
test_decrypt (Test *test, gconstpointer unused)
{
	GckMechanism mech = { CKM_MOCK_CAPITALIZE, NULL, 0 };
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GckObject *key;
	guchar *output;
	gsize n_output;

	/* Find the right key */
	key = find_key (test->session, CKA_DECRYPT, CKM_MOCK_CAPITALIZE);
	g_assert (key);

	/* Simple one */
	output = gck_session_decrypt (test->session, key, CKM_MOCK_CAPITALIZE, (const guchar*)"FRY???", 7, &n_output, NULL, &error);
	g_assert_no_error (error);
	g_assert (output);
	g_assert (n_output == 7);
	g_assert_cmpstr ((gchar*)output, ==, "fry???");
	g_free (output);

	/* Asynchronous one */
	gck_session_decrypt_async (test->session, key, &mech, (const guchar*)"FAT CHANCE", 11, NULL, fetch_async_result, &result);

	egg_test_wait_until (500);
	g_assert (result != NULL);

	/* Get the result */
	output = gck_session_decrypt_finish (test->session, result, &n_output, &error);
	g_assert_no_error (error);
	g_assert (output);
	g_assert (n_output == 11);
	g_assert_cmpstr ((gchar*)output, ==, "fat chance");
	g_free (output);

	g_object_unref (result);
	g_object_unref (key);
}

static void
test_login_context_specific (Test *test, gconstpointer unused)
{
	/* The test module won't let us sign without doing a login, check that */

	GError *error = NULL;
	GckObject *key;
	guchar *output;
	gsize n_output;

	/* Find the right key */
	key = find_key (test->session, CKA_SIGN, CKM_MOCK_PREFIX);
	g_assert (key);

	/* Simple one */
	output = gck_session_sign (test->session, key, CKM_MOCK_PREFIX, (const guchar*)"TV Monster", 11, &n_output, NULL, &error);
	g_assert_error (error, GCK_ERROR, CKR_USER_NOT_LOGGED_IN);
	g_assert (output == NULL);

	g_object_unref (key);
}

static void
test_sign (Test *test, gconstpointer unused)
{
	GckMechanism mech = { CKM_MOCK_PREFIX, "my-prefix:", 10 };
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GckObject *key;
	guchar *output;
	gsize n_output;

	/* Enable auto-login on this test->session, see previous test */
	g_signal_connect (test->module, "authenticate-object", G_CALLBACK (authenticate_object), NULL);

	/* Find the right key */
	key = find_key (test->session_with_auth, CKA_SIGN, CKM_MOCK_PREFIX);
	g_assert (key);

	/* Simple one */
	output = gck_session_sign (test->session_with_auth, key, CKM_MOCK_PREFIX, (const guchar*)"Labarbara", 10, &n_output, NULL, &error);
	g_assert_no_error (error);
	g_assert (output);
	g_assert_cmpuint (n_output, ==, 24);
	g_assert_cmpstr ((gchar*)output, ==, "signed-prefix:Labarbara");
	g_free (output);

	/* Asynchronous one */
	gck_session_sign_async (test->session_with_auth, key, &mech, (const guchar*)"Conrad", 7, NULL, fetch_async_result, &result);

	egg_test_wait_until (500);
	g_assert (result != NULL);

	/* Get the result */
	output = gck_session_sign_finish (test->session_with_auth, result, &n_output, &error);
	g_assert_no_error (error);
	g_assert (output);
	g_assert_cmpuint (n_output, ==, 17);
	g_assert_cmpstr ((gchar*)output, ==, "my-prefix:Conrad");
	g_free (output);

	g_object_unref (result);
	g_object_unref (key);
}

static void
test_verify (Test *test, gconstpointer unused)
{
	GckMechanism mech = { CKM_MOCK_PREFIX, "my-prefix:", 10 };
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GckObject *key;
	gboolean ret;

	/* Enable auto-login on this session, shouldn't be needed */
	g_signal_connect (test->module, "authenticate-object", G_CALLBACK (authenticate_object), NULL);

	/* Find the right key */
	key = find_key (test->session, CKA_VERIFY, CKM_MOCK_PREFIX);
	g_assert (key);

	/* Simple one */
	ret = gck_session_verify (test->session, key, CKM_MOCK_PREFIX, (const guchar*)"Labarbara", 10,
	                           (const guchar*)"signed-prefix:Labarbara", 24, NULL, &error);
	g_assert_no_error (error);
	g_assert (ret);

	/* Failure one */
	ret = gck_session_verify_full (test->session, key, &mech, (const guchar*)"Labarbara", 10,
	                                (const guchar*)"my-prefix:Loborboro", 20, NULL, &error);
	g_assert (error != NULL);
	g_assert (!ret);
	g_clear_error (&error);

	/* Asynchronous one */
	gck_session_verify_async (test->session, key, &mech, (const guchar*)"Labarbara", 10,
	                           (const guchar*)"my-prefix:Labarbara", 20, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);
	ret = gck_session_verify_finish (test->session, result, &error);
	g_assert_no_error (error);
	g_assert (ret);
	g_object_unref (result);

	/* Asynchronous failure */
	result = NULL;
	gck_session_verify_async (test->session, key, &mech, (const guchar*)"Labarbara", 10,
	                           (const guchar*)"my-prefix:Labarxoro", 20, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);
	ret = gck_session_verify_finish (test->session, result, &error);
	g_assert (error != NULL);
	g_assert (!ret);
	g_clear_error (&error);
	g_object_unref (result);

	g_object_unref (key);
}

static void
test_generate_key_pair (Test *test, gconstpointer unused)
{
	GckMechanism mech = { CKM_MOCK_GENERATE, "generate", 9 };
	GckAttributes *pub_attrs, *prv_attrs;
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GckObject *pub_key, *prv_key;
	gboolean ret;

	pub_attrs = gck_attributes_new ();
	gck_attributes_add_ulong (pub_attrs, CKA_CLASS, CKO_PUBLIC_KEY);
	prv_attrs = gck_attributes_new ();
	gck_attributes_add_ulong (prv_attrs, CKA_CLASS, CKO_PRIVATE_KEY);

	/* Full One*/
	ret = gck_session_generate_key_pair_full (test->session, &mech, pub_attrs, prv_attrs,
	                                           &pub_key, &prv_key, NULL, &error);
	g_assert_no_error (error);
	g_assert (ret);
	g_object_unref (pub_key);
	g_object_unref (prv_key);

	/* Failure one */
	mech.type = 0;
	pub_key = prv_key = NULL;
	ret = gck_session_generate_key_pair_full (test->session, &mech, pub_attrs, prv_attrs,
	                                           &pub_key, &prv_key, NULL, &error);
	g_assert (error != NULL);
	g_assert (!ret);
	g_clear_error (&error);
	g_assert (pub_key == NULL);
	g_assert (prv_key == NULL);

	/* Asynchronous one */
	mech.type = CKM_MOCK_GENERATE;
	gck_session_generate_key_pair_async (test->session, &mech, pub_attrs, prv_attrs, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);
	ret = gck_session_generate_key_pair_finish (test->session, result, &pub_key, &prv_key, &error);
	g_assert_no_error (error);
	g_assert (ret);
	g_object_unref (result);
	g_object_unref (pub_key);
	g_object_unref (prv_key);

	/* Asynchronous failure */
	result = NULL;
	mech.type = 0;
	pub_key = prv_key = NULL;
	gck_session_generate_key_pair_async (test->session, &mech, pub_attrs, prv_attrs, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);
	ret = gck_session_generate_key_pair_finish (test->session, result, &pub_key, &prv_key, &error);
	g_assert (error != NULL);
	g_assert (!ret);
	g_clear_error (&error);
	g_object_unref (result);
	g_assert (pub_key == NULL);
	g_assert (prv_key == NULL);

	gck_attributes_unref (pub_attrs);
	gck_attributes_unref (prv_attrs);
}

static void
test_wrap_key (Test *test, gconstpointer unused)
{
	GckMechanism mech = { CKM_MOCK_WRAP, "wrap", 4 };
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GckObject *wrapper, *wrapped;
	gpointer output;
	gsize n_output;

	wrapper = find_key (test->session, CKA_WRAP, 0);
	wrapped = find_key_with_value (test->session, "value");

	/* Simple One */
	output = gck_session_wrap_key (test->session, wrapper, CKM_MOCK_WRAP, wrapped, &n_output, NULL, &error);
	g_assert_no_error (error);
	g_assert (output);
	egg_assert_cmpsize (n_output, ==, 5);
	g_assert (memcmp (output, "value", 5) == 0);
	g_free (output);

	/* Full One*/
	output = gck_session_wrap_key_full (test->session, wrapper, &mech, wrapped, &n_output, NULL, &error);
	g_assert_no_error (error);
	g_assert (output);
	egg_assert_cmpsize (n_output, ==, 5);
	g_assert (memcmp (output, "value", 5) == 0);
	g_free (output);

	/* Failure one */
	mech.type = 0;
	n_output = 0;
	output = gck_session_wrap_key_full (test->session, wrapper, &mech, wrapped, &n_output, NULL, &error);
	g_assert (error != NULL);
	g_assert (!output);
	g_clear_error (&error);
	egg_assert_cmpsize (n_output, ==, 0);

	/* Asynchronous one */
	mech.type = CKM_MOCK_WRAP;
	gck_session_wrap_key_async (test->session, wrapper, &mech, wrapped, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);
	output = gck_session_wrap_key_finish (test->session, result, &n_output, &error);
	g_assert_no_error (error);
	g_assert (output);
	egg_assert_cmpsize (n_output, ==, 5);
	g_assert (memcmp (output, "value", 5) == 0);
	g_object_unref (result);
	g_free (output);

	/* Asynchronous failure */
	result = NULL;
	mech.type = 0;
	n_output = 0;
	gck_session_wrap_key_async (test->session, wrapper, &mech, wrapped, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);
	output = gck_session_wrap_key_finish (test->session, result, &n_output, &error);
	g_assert (error != NULL);
	g_assert (!output);
	g_clear_error (&error);
	egg_assert_cmpsize (n_output, ==, 0);
	g_object_unref (result);

	g_object_unref (wrapper);
	g_object_unref (wrapped);
}

static void
test_unwrap_key (Test *test, gconstpointer unused)
{
	GckMechanism mech = { CKM_MOCK_WRAP, "wrap", 4 };
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GckObject *wrapper, *unwrapped;
	GckAttributes *attrs;

	wrapper = find_key (test->session, CKA_UNWRAP, 0);
	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_SECRET_KEY);

	/* Full One*/
	unwrapped = gck_session_unwrap_key_full (test->session, wrapper, &mech, "special", 7, attrs, NULL, &error);
	g_assert_no_error (error);
	g_assert (GCK_IS_OBJECT (unwrapped));
	check_key_with_value (test->session, unwrapped, CKO_SECRET_KEY, "special");
	g_object_unref (unwrapped);

	/* Failure one */
	mech.type = 0;
	unwrapped = gck_session_unwrap_key_full (test->session, wrapper, &mech, "special", 7, attrs, NULL, &error);
	g_assert (error != NULL);
	g_assert (!unwrapped);
	g_clear_error (&error);

	/* Asynchronous one */
	mech.type = CKM_MOCK_WRAP;
	gck_session_unwrap_key_async (test->session, wrapper, &mech, "special", 7, attrs, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);
	unwrapped = gck_session_unwrap_key_finish (test->session, result, &error);
	g_assert_no_error (error);
	g_assert (GCK_IS_OBJECT (unwrapped));
	check_key_with_value (test->session, unwrapped, CKO_SECRET_KEY, "special");
	g_object_unref (unwrapped);
	g_object_unref (result);

	/* Asynchronous failure */
	result = NULL;
	mech.type = 0;
	gck_session_unwrap_key_async (test->session, wrapper, &mech, "special", 6, attrs, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);
	unwrapped = gck_session_unwrap_key_finish (test->session, result, &error);
	g_assert (error != NULL);
	g_assert (!unwrapped);
	g_clear_error (&error);
	g_object_unref (result);

	g_object_unref (wrapper);
	gck_attributes_unref (attrs);
}

static void
test_derive_key (Test *test, gconstpointer unused)
{
	GckMechanism mech = { CKM_MOCK_DERIVE, "derive", 6 };
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GckObject *wrapper, *derived;
	GckAttributes *attrs;

	wrapper = find_key (test->session, CKA_DERIVE, 0);
	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_SECRET_KEY);

	/* Full One*/
	derived = gck_session_derive_key_full (test->session, wrapper, &mech, attrs, NULL, &error);
	g_assert_no_error (error);
	g_assert (GCK_IS_OBJECT (derived));
	check_key_with_value (test->session, derived, CKO_SECRET_KEY, "derived");
	g_object_unref (derived);

	/* Failure one */
	mech.type = 0;
	derived = gck_session_derive_key_full (test->session, wrapper, &mech, attrs, NULL, &error);
	g_assert (error != NULL);
	g_assert (!derived);
	g_clear_error (&error);

	/* Asynchronous one */
	mech.type = CKM_MOCK_DERIVE;
	gck_session_derive_key_async (test->session, wrapper, &mech, attrs, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);
	derived = gck_session_derive_key_finish (test->session, result, &error);
	g_assert_no_error (error);
	g_assert (GCK_IS_OBJECT (derived));
	check_key_with_value (test->session, derived, CKO_SECRET_KEY, "derived");
	g_object_unref (derived);
	g_object_unref (result);

	/* Asynchronous failure */
	result = NULL;
	mech.type = 0;
	gck_session_derive_key_async (test->session, wrapper, &mech, attrs, NULL, fetch_async_result, &result);
	egg_test_wait_until (500);
	g_assert (result != NULL);
	derived = gck_session_derive_key_finish (test->session, result, &error);
	g_assert (error != NULL);
	g_assert (!derived);
	g_clear_error (&error);
	g_object_unref (result);

	g_object_unref (wrapper);
	gck_attributes_unref (attrs);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gck/crypto/encrypt", Test, NULL, setup, test_encrypt, teardown);
	g_test_add ("/gck/crypto/decrypt", Test, NULL, setup, test_decrypt, teardown);
	g_test_add ("/gck/crypto/login_context_specific", Test, NULL, setup, test_login_context_specific, teardown);
	g_test_add ("/gck/crypto/sign", Test, NULL, setup, test_sign, teardown);
	g_test_add ("/gck/crypto/verify", Test, NULL, setup, test_verify, teardown);
	g_test_add ("/gck/crypto/generate_key_pair", Test, NULL, setup, test_generate_key_pair, teardown);
	g_test_add ("/gck/crypto/wrap_key", Test, NULL, setup, test_wrap_key, teardown);
	g_test_add ("/gck/crypto/unwrap_key", Test, NULL, setup, test_unwrap_key, teardown);
	g_test_add ("/gck/crypto/derive_key", Test, NULL, setup, test_derive_key, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
