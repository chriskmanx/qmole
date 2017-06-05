#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "test-suite.h"

#include <glib.h>

#include "gp11-test.h"

static GP11Module *module = NULL;
static GP11Slot *slot = NULL;
static GP11Session *session = NULL;
static GP11Object *object = NULL;

DEFINE_SETUP(prep_object)
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
	
	/* Our module always exports a token object with this */
	object = gp11_object_from_handle (slot, 2);
	g_assert (object != NULL);
}

DEFINE_TEARDOWN(prep_object)
{
	g_object_unref (object);
	g_object_unref (session); 
	g_object_unref (slot);
	g_object_unref (module);
}

DEFINE_TEST(object_props)
{
	GP11Slot *sl;
	GP11Module *mod;
	CK_OBJECT_HANDLE handle;
	g_object_get (object, "slot", &sl, "module", &mod, "handle", &handle, NULL);
	g_assert (slot == sl);
	g_object_unref (sl);
	g_assert (module == mod);
	g_object_unref (mod);
	g_assert (handle == 2);
}

DEFINE_TEST(object_equals_hash)
{
	GP11Slot *other_slot;
	GP11Object *other_object;
	GObject *obj;
	guint hash;
	
	hash = gp11_object_hash (object);
	g_assert (hash != 0);
	
	g_assert (gp11_object_equal (object, object));
	
	other_slot = g_object_new (GP11_TYPE_SLOT, "module", module, "handle", 5895, NULL);
	other_object = gp11_object_from_handle (other_slot, gp11_object_get_handle (object));
	g_assert (!gp11_object_equal (object, other_object));
	g_object_unref (other_slot);
	g_object_unref (other_object);
	
	obj = g_object_new (G_TYPE_OBJECT, NULL);
	g_assert (!gp11_object_equal (object, obj));
	g_object_unref (obj);

	other_object = gp11_object_from_handle (slot, 383838);
	g_assert (!gp11_object_equal (object, other_object));
	g_object_unref (other_object);
	
	other_object = gp11_object_from_handle (slot, gp11_object_get_handle (object));
	g_assert (gp11_object_equal (object, other_object));
	g_object_unref (other_object);
}

static void 
fetch_async_result (GObject *source, GAsyncResult *result, gpointer user_data)
{
	*((GAsyncResult**)user_data) = result;
	g_object_ref (result);
	testing_wait_stop ();
}

DEFINE_TEST(create_object)
{
	GAsyncResult *result = NULL;
	GP11Attributes *attrs;
	GP11Object *object;
	CK_OBJECT_HANDLE last_handle;
	GError *err = NULL;
	
	/* Using simple */
	object = gp11_session_create_object (session, &err, 
	                                     CKA_CLASS, GP11_ULONG, CKO_DATA,
	                                     CKA_LABEL, GP11_STRING, "TEST LABEL",
	                                     CKA_TOKEN, GP11_BOOLEAN, CK_FALSE,
	                                     CKA_VALUE, 4UL, "BLAH",
	                                     GP11_INVALID);
	SUCCESS_RES (object, err);
	g_assert (GP11_IS_OBJECT (object));
	
	if (object) {
		last_handle = gp11_object_get_handle (object);
		g_object_unref (object);
	}
	
	/* Using full */
	attrs = gp11_attributes_newv (CKA_CLASS, GP11_ULONG, CKO_DATA,
	                              CKA_LABEL, GP11_STRING, "TEST LABEL",
	                              CKA_TOKEN, GP11_BOOLEAN, CK_FALSE,
	                              CKA_VALUE, 4UL, "BLAH",
	                              GP11_INVALID);
	
	object = gp11_session_create_object_full (session, attrs, NULL, &err);
	g_assert (GP11_IS_OBJECT (object));
	SUCCESS_RES (object, err);
	
	if (object) {
		g_assert (last_handle != gp11_object_get_handle (object));
		last_handle = gp11_object_get_handle (object);
		g_object_unref (object);
	}

	/* Using async */
	gp11_session_create_object_async (session, attrs, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	
	object = gp11_session_create_object_finish (session, result, &err);
	g_object_unref (result);
	SUCCESS_RES (object, err);
	g_assert (GP11_IS_OBJECT (object));
	
	if (object)
		g_object_unref (object);
	gp11_attributes_unref (attrs);
}

DEFINE_TEST(destroy_object)
{
	GAsyncResult *result = NULL;
	GP11Object *object;
	GError *err = NULL;
	gboolean ret;
	
	/* Using simple */
	object = gp11_session_create_object (session, &err, 
	                                     CKA_CLASS, GP11_ULONG, CKO_DATA,
	                                     CKA_LABEL, GP11_STRING, "TEST OBJECT",
	                                     CKA_TOKEN, GP11_BOOLEAN, CK_TRUE,
	                                     GP11_INVALID);
	SUCCESS_RES (object, err);
	g_assert (GP11_IS_OBJECT (object));
	
	if (!object)
		return;
	
	ret = gp11_object_destroy (object, &err);
	SUCCESS_RES (ret, err);
	g_object_unref (object);

	/* Using full */
	object = gp11_session_create_object (session, &err, 
	                                     CKA_CLASS, GP11_ULONG, CKO_DATA,
	                                     CKA_LABEL, GP11_STRING, "TEST OBJECT",
	                                     CKA_TOKEN, GP11_BOOLEAN, CK_TRUE,
	                                     GP11_INVALID);
	SUCCESS_RES (object, err);
	g_assert (GP11_IS_OBJECT (object));
	
	if (!object)
		return;
	
	ret = gp11_object_destroy_full (object, NULL, &err);
	SUCCESS_RES (ret, err);
	g_object_unref (object);
	
	/* Using async */
	object = gp11_session_create_object (session, &err, 
	                                     CKA_CLASS, GP11_ULONG, CKO_DATA,
	                                     CKA_LABEL, GP11_STRING, "TEST OBJECT",
	                                     CKA_TOKEN, GP11_BOOLEAN, CK_TRUE,
	                                     GP11_INVALID);
	SUCCESS_RES (object, err);
	g_assert (GP11_IS_OBJECT (object));
	
	if (!object)
		return;
	
	/* Using async */
	gp11_object_destroy_async (object, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	ret = gp11_object_destroy_finish (object, result, &err);
	g_object_unref (result);
	SUCCESS_RES (object, err);
	g_object_unref (object);
}

DEFINE_TEST(get_attributes)
{
	GAsyncResult *result = NULL;
	GP11Attributes *attrs, *attrs_ret;
	GError *err = NULL;
	gulong klass;
	gchar *value = NULL;
	
	/* Simple */
	attrs = gp11_object_get (object, &err, CKA_CLASS, CKA_LABEL, GP11_INVALID);
	SUCCESS_RES (attrs, err);
	if (attrs != NULL) {
		g_assert (gp11_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == CKO_DATA);
		g_assert (gp11_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "TEST LABEL") == 0);
		g_free (value); value = NULL;
	}
	gp11_attributes_unref (attrs);

	/* Full */
	attrs = gp11_attributes_new_empty (CKA_CLASS, CKA_LABEL, GP11_INVALID);
	attrs_ret = gp11_object_get_full (object, attrs, NULL, &err);
	SUCCESS_RES (attrs_ret, err);
	if (attrs_ret != NULL) {
		g_assert (attrs_ret == attrs);
		g_assert (gp11_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == CKO_DATA);
		g_assert (gp11_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "TEST LABEL") == 0);
		g_free (value); value = NULL;
	}
	gp11_attributes_unref (attrs);

	/* Async */
	attrs = gp11_attributes_new_empty (CKA_CLASS, CKA_LABEL, GP11_INVALID);
	gp11_object_get_async (object, attrs, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	attrs_ret = gp11_object_get_finish (object, result, &err);
	g_object_unref (result);
	SUCCESS_RES (attrs, err);
	if (attrs != NULL) {
		g_assert (attrs_ret == attrs);
		g_assert (gp11_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == CKO_DATA);
		g_assert (gp11_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "TEST LABEL") == 0);
		g_free (value); value = NULL;
	}
	gp11_attributes_unref (attrs);
}

DEFINE_TEST(get_data_attribute)
{
	GAsyncResult *result = NULL;
	CK_OBJECT_CLASS_PTR klass;
	gsize n_data;
	GError *err = NULL;
	
	/* Simple */
	klass = gp11_object_get_data (object, CKA_CLASS, &n_data, &err);
	SUCCESS_RES (klass, err);
	if (klass != NULL) {
		g_assert (n_data == sizeof (CK_OBJECT_CLASS));
		g_assert (*klass == CKO_DATA);
		g_free (klass);
	}

	/* Full */
	klass = gp11_object_get_data_full (object, CKA_CLASS, NULL, NULL, &n_data, &err);
	SUCCESS_RES (klass, err);
	if (klass != NULL) {
		g_assert (n_data == sizeof (CK_OBJECT_CLASS));
		g_assert (*klass == CKO_DATA);
		g_free (klass);
	}

	/* Async */
	gp11_object_get_data_async (object, CKA_CLASS, NULL, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	klass = gp11_object_get_data_finish (object, result, &n_data, &err);
	g_object_unref (result);
	SUCCESS_RES (klass, err);
	if (klass != NULL) {
		g_assert (n_data == sizeof (CK_OBJECT_CLASS));
		g_assert (*klass == CKO_DATA);
		g_free (klass);
	}

}

DEFINE_TEST(set_attributes)
{
	GAsyncResult *result = NULL;
	GP11Attributes *attrs, *templ;
	GError *err = NULL;
	gulong klass;
	gchar *value = NULL;
	gboolean ret;
	
	/* Simple */
	ret = gp11_object_set (object, &err, 
	                       CKA_CLASS, GP11_ULONG, 5, 
	                       CKA_LABEL, GP11_STRING, "CHANGE ONE", 
	                       GP11_INVALID);
	SUCCESS_RES (ret, err);
	if (ret) {
		attrs = gp11_object_get (object, &err, CKA_CLASS, CKA_LABEL, GP11_INVALID);
		g_assert (gp11_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == 5);
		g_assert (gp11_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "CHANGE ONE") == 0);
		g_free (value); value = NULL;
		gp11_attributes_unref (attrs);
	}

	templ = gp11_attributes_newv (CKA_CLASS, GP11_ULONG, 6,
	                              CKA_LABEL, GP11_STRING, "CHANGE TWO",
	                              GP11_INVALID);
	
	/* Full */
	ret = gp11_object_set_full (object, templ, NULL, &err);
	gp11_attributes_unref (templ);
	SUCCESS_RES (ret, err);
	if (ret) {
		attrs = gp11_object_get (object, &err, CKA_CLASS, CKA_LABEL, GP11_INVALID);
		g_assert (gp11_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == 6);
		g_assert (gp11_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "CHANGE TWO") == 0);
		g_free (value); value = NULL;
		gp11_attributes_unref (attrs);
	}

	templ = gp11_attributes_newv (CKA_CLASS, GP11_ULONG, 7,
	                              CKA_LABEL, GP11_STRING, "CHANGE THREE",
	                              GP11_INVALID);

	/* Async */
	gp11_object_set_async (object, templ, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	ret = gp11_object_set_finish (object, result, &err);
	g_object_unref (result);
	SUCCESS_RES (ret, err);
	if (ret) {
		attrs = gp11_object_get (object, &err, CKA_CLASS, CKA_LABEL, GP11_INVALID);
		g_assert (gp11_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == 7);
		g_assert (gp11_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "CHANGE THREE") == 0);
		g_free (value); value = NULL;
		gp11_attributes_unref (attrs);
	}
}

DEFINE_TEST(find_objects)
{
	GAsyncResult *result = NULL;
	GP11Attributes *templ;
	GList *objects;
	GP11Object *testobj;
	GError *err = NULL;
	
	testobj = gp11_session_create_object (session, &err, 
	                                      CKA_CLASS, GP11_ULONG, CKO_DATA,
	                                      CKA_LABEL, GP11_STRING, "UNIQUE LABEL",
	                                      GP11_INVALID);
	g_object_unref (testobj);

	testobj = gp11_session_create_object (session, &err, 
	                                      CKA_CLASS, GP11_ULONG, CKO_DATA,
	                                      CKA_LABEL, GP11_STRING, "OTHER LABEL",
	                                      GP11_INVALID);
	g_object_unref (testobj);

	/* Simple, "TEST LABEL" */
	objects = gp11_session_find_objects (session, &err, CKA_LABEL, GP11_STRING, "UNIQUE LABEL", GP11_INVALID);
	SUCCESS_RES (objects, err);
	g_assert (g_list_length (objects) == 1);
	gp11_list_unref_free (objects);

	/* Full, All */
	templ = gp11_attributes_new ();
	objects = gp11_session_find_objects_full (session, templ, NULL, &err);
	SUCCESS_RES (objects, err);
	g_assert (g_list_length (objects) > 1);
	gp11_list_unref_free (objects);
	
	/* Async, None */
	gp11_attributes_add_string (templ, CKA_LABEL, "blah blah");
	gp11_session_find_objects_async (session, templ, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	objects = gp11_session_find_objects_finish (session, result, &err);
	g_object_unref (result);
	g_assert (objects == NULL);
	gp11_list_unref_free (objects);
}

DEFINE_TEST(explicit_sessions)
{
	GP11Session *sess;
	GAsyncResult *result = NULL;
	CK_OBJECT_CLASS_PTR klass;
	GError *err = NULL;
	gsize n_data;

	/* Set an explicit session */
	gp11_object_set_session (object, session);
	sess = gp11_object_get_session (object);
	g_assert (sess == session);
	g_object_unref (sess);
	g_object_get (object, "session", &sess, NULL);
	g_assert (sess == session);
	g_object_unref (sess);
	
	/* Simple */
	klass = gp11_object_get_data (object, CKA_CLASS, &n_data, &err);
	SUCCESS_RES (klass, err);
	if (klass != NULL) {
		g_assert (n_data == sizeof (CK_OBJECT_CLASS));
		g_assert (*klass == CKO_DATA);
		g_free (klass);
	}

	/* Async */
	gp11_object_get_data_async (object, CKA_CLASS, NULL, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	klass = gp11_object_get_data_finish (object, result, &n_data, &err);
	g_object_unref (result);
	SUCCESS_RES (klass, err);
	if (klass != NULL) {
		g_assert (n_data == sizeof (CK_OBJECT_CLASS));
		g_assert (*klass == CKO_DATA);
		g_free (klass);
	}

	/* Set it to null and make sure taht works */
	gp11_object_set_session (object, NULL);
	g_assert (gp11_object_get_session (object) == NULL);
	g_object_get (object, "session", &sess, NULL);
	g_assert (sess == NULL);

	/* Test property settor */
	g_object_set (object, "session", session, NULL);
	sess = gp11_object_get_session (object);
	g_assert (sess == session);
	gp11_object_set_session (object, NULL);
	g_object_unref (sess);
}
