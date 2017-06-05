#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "test-suite.h"

#include <glib.h>

#include "gp11-test.h"

static GP11Module *module = NULL;
static GP11Slot *slot = NULL;
static GP11Session *session = NULL;

DEFINE_SETUP(crypto_session)
{
	GError *err = NULL;
	GList *slots;
	
	/* Successful load */
	module = gp11_module_initialize (".libs/libgp11-test-module.so", NULL, &err);
	SUCCESS_RES (module, err);
	
	slots = gp11_module_get_slots (module, TRUE);
	g_assert (slots != NULL);
	
	slot = GP11_SLOT (slots->data);
	g_object_ref (slot);
	gp11_list_unref_free (slots);

	session = gp11_slot_open_session (slot, 0, &err);
	SUCCESS_RES(session, err); 
}

DEFINE_TEARDOWN(crypto_session)
{
	g_object_unref (session); 
	g_object_unref (slot);
	g_object_unref (module);
}

static void 
fetch_async_result (GObject *source, GAsyncResult *result, gpointer user_data)
{
	*((GAsyncResult**)user_data) = result;
	g_object_ref (result);
	testing_wait_stop ();
}

static GP11Object*
find_key (GP11Session *session, CK_ATTRIBUTE_TYPE method, CK_MECHANISM_TYPE mech)
{
	GList *objects, *l;
	GP11Object *object = NULL;
	CK_MECHANISM_TYPE_PTR mechs;
	gsize n_mechs;
	
	objects = gp11_session_find_objects (session, NULL, method, GP11_BOOLEAN, TRUE, GP11_INVALID);
	g_assert (objects);
	
	for (l = objects; l; l = g_list_next (l)) {
		gp11_object_set_session (l->data, session);
		if (mech) {
			mechs = gp11_object_get_data (l->data, CKA_ALLOWED_MECHANISMS, &n_mechs, NULL);
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
	
	gp11_list_unref_free (objects);
	return object;
}

static GP11Object*
find_key_with_value (GP11Session *session, const gchar *value)
{
	GList *objects;
	GP11Object *object;

	objects = gp11_session_find_objects (session, NULL, CKA_VALUE, GP11_STRING, value, GP11_INVALID);
	g_assert (objects);

	object = g_object_ref (objects->data);
	gp11_list_unref_free (objects);
	return object;
}

static void
check_key_with_value (GP11Session *session, GP11Object *key, CK_OBJECT_CLASS klass, const gchar *value)
{
	GP11Attributes *attrs;
	GP11Attribute *attr;
	gulong check;

	gp11_object_set_session (key, session);
	attrs = gp11_object_get (key, NULL, CKA_CLASS, CKA_VALUE, GP11_INVALID);
	g_assert (attrs);

	if (!gp11_attributes_find_ulong (attrs, CKA_CLASS, &check))
		g_assert_not_reached ();
	g_assert (check == klass);

	attr = gp11_attributes_find (attrs, CKA_VALUE);
	g_assert (attr);
	g_assert (!gp11_attribute_is_invalid (attr));
	g_assert_cmpsize (attr->length, ==, strlen (value));
	g_assert (memcmp (attr->value, value, attr->length) == 0);

	gp11_attributes_unref (attrs);
}

static gboolean
authenticate_object (GP11Slot *module, GP11Object *object, gchar *label, gchar **password)
{
	g_assert (GP11_IS_MODULE (module));
	g_assert (GP11_IS_OBJECT (object));
	g_assert (password);
	g_assert (!*password);
	
	*password = g_strdup ("booo");
	return TRUE;
}

DEFINE_TEST(encrypt)
{
	GP11Mechanism *mech;
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GP11Object *key;
	guchar *output;
	gsize n_output;

	mech = gp11_mechanism_new (CKM_CAPITALIZE);

	/* Find the right key */
	key = find_key (session, CKA_ENCRYPT, CKM_CAPITALIZE);
	g_assert (key);
	
	/* Simple one */
	output = gp11_session_encrypt (session, key, CKM_CAPITALIZE, (const guchar*)"blah blah", 10, &n_output, &error);
	SUCCESS_RES (output, error);
	g_assert (n_output == 10);
	g_assert_cmpstr ((gchar*)output, ==, "BLAH BLAH");
	g_free (output);
	
	/* Full one */
	output = gp11_session_encrypt_full (session, key, mech, (const guchar*)"blah blah", 10, &n_output, NULL, &error);
	SUCCESS_RES (output, error);
	g_assert (n_output == 10);
	g_assert_cmpstr ((gchar*)output, ==, "BLAH BLAH");
	g_free (output);
	
	/* Asynchronous one */
	gp11_session_encrypt_async (session, key, mech, (const guchar*)"second chance", 14, NULL, fetch_async_result, &result);

	testing_wait_until (500);
	g_assert (result != NULL);
	
	/* Get the result */
	output = gp11_session_encrypt_finish (session, result, &n_output, &error);
	SUCCESS_RES (output, error);
	g_assert (n_output == 14);
	g_assert_cmpstr ((gchar*)output, ==, "SECOND CHANCE");
	g_free (output);

	gp11_mechanism_unref (mech);
	g_object_unref (result);
	g_object_unref (key);
}

DEFINE_TEST(decrypt)
{
	GP11Mechanism *mech;
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GP11Object *key;
	guchar *output;
	gsize n_output;

	mech = gp11_mechanism_new (CKM_CAPITALIZE);

	/* Find the right key */
	key = find_key (session, CKA_DECRYPT, CKM_CAPITALIZE);
	g_assert (key);
	
	/* Simple one */
	output = gp11_session_decrypt (session, key, CKM_CAPITALIZE, (const guchar*)"FRY???", 7, &n_output, &error);
	SUCCESS_RES (output, error);
	g_assert (n_output == 7);
	g_assert_cmpstr ((gchar*)output, ==, "fry???");
	g_free (output);
	
	/* Full one */
	output = gp11_session_decrypt_full (session, key, mech, (const guchar*)"TENNIS instructor", 18, &n_output, NULL, &error);
	SUCCESS_RES (output, error);
	g_assert (n_output == 18);
	g_assert_cmpstr ((gchar*)output, ==, "tennis instructor");
	g_free (output);
	
	/* Asynchronous one */
	gp11_session_decrypt_async (session, key, mech, (const guchar*)"FAT CHANCE", 11, NULL, fetch_async_result, &result);

	testing_wait_until (500);
	g_assert (result != NULL);

	/* Get the result */
	output = gp11_session_decrypt_finish (session, result, &n_output, &error);
	SUCCESS_RES (output, error);
	g_assert (n_output == 11);
	g_assert_cmpstr ((gchar*)output, ==, "fat chance");
	g_free (output);

	gp11_mechanism_unref (mech);
	g_object_unref (result);
	g_object_unref (key);
}

DEFINE_TEST(login_context_specific)
{
	/* The test module won't let us sign without doing a login, check that */
	
	GError *error = NULL;
	GP11Object *key;
	guchar *output;
	gsize n_output;

	/* Find the right key */
	key = find_key (session, CKA_SIGN, CKM_PREFIX);
	g_assert (key);
	
	/* Simple one */
	output = gp11_session_sign (session, key, CKM_PREFIX, (const guchar*)"TV Monster", 11, &n_output, &error);
	g_assert (error && error->code == CKR_USER_NOT_LOGGED_IN);
	FAIL_RES (output, error);
	g_assert (output == NULL);
	
	g_object_unref (key);
}

DEFINE_TEST(sign)
{
	GP11Mechanism *mech;
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GP11Object *key;
	guchar *output;
	gsize n_output;

	mech = gp11_mechanism_new_with_param (CKM_PREFIX, "my-prefix:", 10);

	/* Enable auto-login on this session, see previous test */
	gp11_module_set_auto_authenticate (module, TRUE);
	g_signal_connect (module, "authenticate-object", G_CALLBACK (authenticate_object), NULL);

	/* Find the right key */
	key = find_key (session, CKA_SIGN, CKM_PREFIX);
	g_assert (key);
	
	/* Simple one */
	output = gp11_session_sign (session, key, CKM_PREFIX, (const guchar*)"Labarbara", 10, &n_output, &error);
	SUCCESS_RES (output, error);
	g_assert_cmpuint (n_output, ==, 24);
	g_assert_cmpstr ((gchar*)output, ==, "signed-prefix:Labarbara");
	g_free (output);
	
	/* Full one */
	output = gp11_session_sign_full (session, key, mech, (const guchar*)"Labarbara", 10, &n_output, NULL, &error);
	SUCCESS_RES (output, error);
	g_assert_cmpuint (n_output, ==, 20);
	g_assert_cmpstr ((gchar*)output, ==, "my-prefix:Labarbara");
	g_free (output);
	
	/* Asynchronous one */
	gp11_session_sign_async (session, key, mech, (const guchar*)"Conrad", 7, NULL, fetch_async_result, &result);

	testing_wait_until (500);
	g_assert (result != NULL);

	/* Get the result */
	output = gp11_session_sign_finish (session, result, &n_output, &error);
	SUCCESS_RES (output, error);
	g_assert_cmpuint (n_output, ==, 17);
	g_assert_cmpstr ((gchar*)output, ==, "my-prefix:Conrad");
	g_free (output);

	gp11_mechanism_unref (mech);
	g_object_unref (result);
	g_object_unref (key);
}

DEFINE_TEST(verify)
{
	GP11Mechanism *mech;
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GP11Object *key;
	gboolean ret;

	mech = gp11_mechanism_new_with_param (CKM_PREFIX, "my-prefix:", 10);

	/* Enable auto-login on this session, shouldn't be needed */
	gp11_module_set_auto_authenticate (module, TRUE);
	g_signal_connect (module, "authenticate-object", G_CALLBACK (authenticate_object), NULL);

	/* Find the right key */
	key = find_key (session, CKA_VERIFY, CKM_PREFIX);
	g_assert (key);
	
	/* Simple one */
	ret = gp11_session_verify (session, key, CKM_PREFIX, (const guchar*)"Labarbara", 10, 
	                           (const guchar*)"signed-prefix:Labarbara", 24, &error);
	SUCCESS_RES (ret, error);
	
	/* Full one */
	ret = gp11_session_verify_full (session, key, mech, (const guchar*)"Labarbara", 10,
	                                (const guchar*)"my-prefix:Labarbara", 20, NULL, &error);
	SUCCESS_RES (ret, error);

	/* Failure one */
	ret = gp11_session_verify_full (session, key, mech, (const guchar*)"Labarbara", 10,
	                                (const guchar*)"my-prefix:Loborboro", 20, NULL, &error);
	FAIL_RES (ret, error);

	/* Asynchronous one */
	gp11_session_verify_async (session, key, mech, (const guchar*)"Labarbara", 10,
	                           (const guchar*)"my-prefix:Labarbara", 20, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	ret = gp11_session_verify_finish (session, result, &error);
	SUCCESS_RES (ret, error);
	g_object_unref (result);
	
	/* Asynchronous failure */
	result = NULL;
	gp11_session_verify_async (session, key, mech, (const guchar*)"Labarbara", 10,
	                           (const guchar*)"my-prefix:Labarxoro", 20, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	ret = gp11_session_verify_finish (session, result, &error);
	FAIL_RES (ret, error);
	g_object_unref (result);

	gp11_mechanism_unref (mech);
	g_object_unref (key);
}

DEFINE_TEST(generate_key_pair)
{
	GP11Attributes *pub_attrs, *prv_attrs;
	GP11Mechanism *mech;
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GP11Object *pub_key, *prv_key;
	gboolean ret;

	mech = gp11_mechanism_new_with_param (CKM_GENERATE, "generate", 9);

	pub_attrs = gp11_attributes_new ();
	gp11_attributes_add_ulong (pub_attrs, CKA_CLASS, CKO_PUBLIC_KEY);
	prv_attrs = gp11_attributes_new ();
	gp11_attributes_add_ulong (prv_attrs, CKA_CLASS, CKO_PRIVATE_KEY);

	/* Full One*/
	ret = gp11_session_generate_key_pair_full (session, mech, pub_attrs, prv_attrs,
	                                           &pub_key, &prv_key, NULL, &error);
	SUCCESS_RES (ret, error);
	g_object_unref (pub_key);
	g_object_unref (prv_key);

	/* Failure one */
	mech->type = 0;
	pub_key = prv_key = NULL;
	ret = gp11_session_generate_key_pair_full (session, mech, pub_attrs, prv_attrs,
	                                           &pub_key, &prv_key, NULL, &error);
	FAIL_RES (ret, error);
	g_assert (pub_key == NULL);
	g_assert (prv_key == NULL);

	/* Asynchronous one */
	mech->type = CKM_GENERATE;
	gp11_session_generate_key_pair_async (session, mech, pub_attrs, prv_attrs, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	ret = gp11_session_generate_key_pair_finish (session, result, &pub_key, &prv_key, &error);
	SUCCESS_RES (ret, error);
	g_object_unref (result);
	g_object_unref (pub_key);
	g_object_unref (prv_key);

	/* Asynchronous failure */
	result = NULL;
	mech->type = 0;
	pub_key = prv_key = NULL;
	gp11_session_generate_key_pair_async (session, mech, pub_attrs, prv_attrs, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	ret = gp11_session_generate_key_pair_finish (session, result, &pub_key, &prv_key, &error);
	FAIL_RES (ret, error);
	g_object_unref (result);
	g_assert (pub_key == NULL);
	g_assert (prv_key == NULL);

	gp11_mechanism_unref (mech);
	gp11_attributes_unref (pub_attrs);
	gp11_attributes_unref (prv_attrs);
}

DEFINE_TEST(wrap_key)
{
	GP11Mechanism *mech;
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GP11Object *wrapper, *wrapped;
	gpointer output;
	gsize n_output;

	mech = gp11_mechanism_new_with_param (CKM_WRAP, "wrap", 4);
	wrapper = find_key (session, CKA_WRAP, 0);
	wrapped = find_key_with_value (session, "value");

	/* Simple One */
	output = gp11_session_wrap_key (session, wrapper, CKM_WRAP, wrapped, &n_output, &error);
	SUCCESS_RES (output, error);
	g_assert (output);
	g_assert_cmpsize (n_output, ==, 5);
	g_assert (memcmp (output, "value", 5) == 0);
	g_free (output);

	/* Full One*/
	output = gp11_session_wrap_key_full (session, wrapper, mech, wrapped, &n_output, NULL, &error);
	SUCCESS_RES (output, error);
	g_assert_cmpsize (n_output, ==, 5);
	g_assert (memcmp (output, "value", 5) == 0);
	g_free (output);

	/* Failure one */
	mech->type = 0;
	n_output = 0;
	output = gp11_session_wrap_key_full (session, wrapper, mech, wrapped, &n_output, NULL, &error);
	FAIL_RES (output, error);
	g_assert_cmpsize (n_output, ==, 0);

	/* Asynchronous one */
	mech->type = CKM_WRAP;
	gp11_session_wrap_key_async (session, wrapper, mech, wrapped, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	output = gp11_session_wrap_key_finish (session, result, &n_output, &error);
	SUCCESS_RES (output, error);
	g_assert_cmpsize (n_output, ==, 5);
	g_assert (memcmp (output, "value", 5) == 0);
	g_object_unref (result);
	g_free (output);

	/* Asynchronous failure */
	result = NULL;
	mech->type = 0;
	n_output = 0;
	gp11_session_wrap_key_async (session, wrapper, mech, wrapped, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	output = gp11_session_wrap_key_finish (session, result, &n_output, &error);
	FAIL_RES (output, error);
	g_assert_cmpsize (n_output, ==, 0);
	g_object_unref (result);

	g_object_unref (wrapper);
	g_object_unref (wrapped);
	gp11_mechanism_unref (mech);
}

DEFINE_TEST(unwrap_key)
{
	GP11Mechanism *mech;
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GP11Object *wrapper, *unwrapped;
	GP11Attributes *attrs;

	mech = gp11_mechanism_new_with_param (CKM_WRAP, "wrap", 4);
	wrapper = find_key (session, CKA_UNWRAP, 0);
	attrs = gp11_attributes_newv (CKA_CLASS, GP11_ULONG, CKO_SECRET_KEY, GP11_INVALID);

	/* Simple One */
	unwrapped = gp11_session_unwrap_key (session, wrapper, CKM_WRAP, "special", 7, &error,
	                                     CKA_CLASS, GP11_ULONG, CKO_SECRET_KEY, GP11_INVALID);
	SUCCESS_RES (unwrapped, error);
	g_assert (GP11_IS_OBJECT (unwrapped));
	check_key_with_value (session, unwrapped, CKO_SECRET_KEY, "special");
	g_object_unref (unwrapped);

	/* Full One*/
	unwrapped = gp11_session_unwrap_key_full (session, wrapper, mech, "special", 7, attrs, NULL, &error);
	SUCCESS_RES (unwrapped, error);
	g_assert (GP11_IS_OBJECT (unwrapped));
	check_key_with_value (session, unwrapped, CKO_SECRET_KEY, "special");
	g_object_unref (unwrapped);

	/* Failure one */
	mech->type = 0;
	unwrapped = gp11_session_unwrap_key_full (session, wrapper, mech, "special", 7, attrs, NULL, &error);
	FAIL_RES (unwrapped, error);

	/* Asynchronous one */
	mech->type = CKM_WRAP;
	gp11_session_unwrap_key_async (session, wrapper, mech, "special", 7, attrs, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	unwrapped = gp11_session_unwrap_key_finish (session, result, &error);
	SUCCESS_RES (unwrapped, error);
	g_assert (GP11_IS_OBJECT (unwrapped));
	check_key_with_value (session, unwrapped, CKO_SECRET_KEY, "special");
	g_object_unref (unwrapped);
	g_object_unref (result);

	/* Asynchronous failure */
	result = NULL;
	mech->type = 0;
	gp11_session_unwrap_key_async (session, wrapper, mech, "special", 6, attrs, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	unwrapped = gp11_session_unwrap_key_finish (session, result, &error);
	FAIL_RES (unwrapped, error);
	g_object_unref (result);

	g_object_unref (wrapper);
	gp11_attributes_unref (attrs);
	gp11_mechanism_unref (mech);
}

DEFINE_TEST(derive_key)
{
	GP11Mechanism *mech;
	GError *error = NULL;
	GAsyncResult *result = NULL;
	GP11Object *wrapper, *derived;
	GP11Attributes *attrs;

	mech = gp11_mechanism_new_with_param (CKM_DERIVE, "derive", 6);
	wrapper = find_key (session, CKA_DERIVE, 0);
	attrs = gp11_attributes_newv (CKA_CLASS, GP11_ULONG, CKO_SECRET_KEY, GP11_INVALID);

	/* Simple One */
	derived = gp11_session_derive_key (session, wrapper, CKM_DERIVE, &error,
	                                   CKA_CLASS, GP11_ULONG, CKO_SECRET_KEY, GP11_INVALID);
	SUCCESS_RES (derived, error);
	g_assert (GP11_IS_OBJECT (derived));
g_printerr ("derived is: %lu", gp11_object_get_handle (derived));
	check_key_with_value (session, derived, CKO_SECRET_KEY, "derived");
	g_object_unref (derived);

	/* Full One*/
	derived = gp11_session_derive_key_full (session, wrapper, mech, attrs, NULL, &error);
	SUCCESS_RES (derived, error);
	g_assert (GP11_IS_OBJECT (derived));
	check_key_with_value (session, derived, CKO_SECRET_KEY, "derived");
	g_object_unref (derived);

	/* Failure one */
	mech->type = 0;
	derived = gp11_session_derive_key_full (session, wrapper, mech, attrs, NULL, &error);
	FAIL_RES (derived, error);

	/* Asynchronous one */
	mech->type = CKM_DERIVE;
	gp11_session_derive_key_async (session, wrapper, mech, attrs, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	derived = gp11_session_derive_key_finish (session, result, &error);
	SUCCESS_RES (derived, error);
	g_assert (GP11_IS_OBJECT (derived));
	check_key_with_value (session, derived, CKO_SECRET_KEY, "derived");
	g_object_unref (derived);
	g_object_unref (result);

	/* Asynchronous failure */
	result = NULL;
	mech->type = 0;
	gp11_session_derive_key_async (session, wrapper, mech, attrs, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	derived = gp11_session_derive_key_finish (session, result, &error);
	FAIL_RES (derived, error);
	g_object_unref (result);

	g_object_unref (wrapper);
	gp11_attributes_unref (attrs);
	gp11_mechanism_unref (mech);
}
