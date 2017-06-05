#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "test-suite.h"

#include <glib.h>

#include "gck-test.h"

static GckModule *module = NULL;
static GckSlot *slot = NULL;
static GckSession *session = NULL;
static GckObject *object = NULL;

DEFINE_SETUP(prep_object)
{
	GError *err = NULL;
	GList *slots;

	/* Successful load */
	module = gck_module_initialize (".libs/libmock-test-module.so", NULL, 0, &err);
	SUCCESS_RES (module, err);

	slots = gck_module_get_slots (module, TRUE);
	g_assert (slots != NULL);

	slot = GCK_SLOT (slots->data);
	g_object_ref (slot);
	gck_list_unref_free (slots);

	session = gck_slot_open_session (slot, 0, NULL, &err);
	SUCCESS_RES(session, err);

	/* Our module always exports a token object with this */
	object = gck_object_from_handle (session, 2);
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
	GckSession *sess;
	GckModule *mod;
	CK_OBJECT_HANDLE handle;
	g_object_get (object, "session", &sess, "module", &mod, "handle", &handle, NULL);
	g_assert (session == sess);
	g_object_unref (sess);
	g_assert (module == mod);
	g_object_unref (mod);
	g_assert (handle == 2);
}

DEFINE_TEST(object_equals_hash)
{
	GckSlot *other_slot;
	GckSession *other_session;
	GckObject *other_object;
	GObject *obj;
	GError *err = NULL;
	guint hash;

	hash = gck_object_hash (object);
	g_assert (hash != 0);

	g_assert (gck_object_equal (object, object));

	other_slot = g_object_new (GCK_TYPE_SLOT, "module", module, "handle", GCK_MOCK_SLOT_TWO_ID, NULL);
	other_session = gck_slot_open_session (other_slot, 0, NULL, &err);
	SUCCESS_RES (other_session, err);
	other_object = gck_object_from_handle (other_session, gck_object_get_handle (object));
	g_assert (!gck_object_equal (object, other_object));
	g_object_unref (other_slot);
	g_object_unref (other_session);
	g_object_unref (other_object);

	obj = g_object_new (G_TYPE_OBJECT, NULL);
	g_assert (!gck_object_equal (object, obj));
	g_object_unref (obj);

	other_object = gck_object_from_handle (session, 383838);
	g_assert (!gck_object_equal (object, other_object));
	g_object_unref (other_object);

	other_object = gck_object_from_handle (session, gck_object_get_handle (object));
	g_assert (gck_object_equal (object, other_object));
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
	GckAttributes *attrs;
	GckObject *object;
	CK_OBJECT_HANDLE last_handle;
	GError *err = NULL;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_DATA);
	gck_attributes_add_string (attrs, CKA_LABEL, "TEST LABEL");
	gck_attributes_add_boolean (attrs, CKA_TOKEN, CK_FALSE);
	gck_attributes_add_data (attrs, CKA_VALUE, "BLAH", 4);

	object = gck_session_create_object (session, attrs, NULL, &err);
	g_assert (GCK_IS_OBJECT (object));
	SUCCESS_RES (object, err);

	last_handle = gck_object_get_handle (object);
	g_object_unref (object);

	/* Using async */
	gck_session_create_object_async (session, attrs, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	object = gck_session_create_object_finish (session, result, &err);
	g_object_unref (result);
	SUCCESS_RES (object, err);
	g_assert (GCK_IS_OBJECT (object));

	g_assert (last_handle != gck_object_get_handle (object));
	g_object_unref (object);

	gck_attributes_unref (attrs);
}

DEFINE_TEST(destroy_object)
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
	object = gck_session_create_object (session, attrs, NULL, &err);
	SUCCESS_RES (object, err);
	g_assert (GCK_IS_OBJECT (object));

	ret = gck_object_destroy (object, NULL, &err);
	SUCCESS_RES (ret, err);
	g_object_unref (object);

	/* Using async */
	object = gck_session_create_object (session, attrs, NULL, &err);
	SUCCESS_RES (object, err);
	g_assert (GCK_IS_OBJECT (object));

	/* Using async */
	gck_object_destroy_async (object, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	ret = gck_object_destroy_finish (object, result, &err);
	g_object_unref (result);
	SUCCESS_RES (object, err);
	g_object_unref (object);
}

DEFINE_TEST(get_attributes)
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
	attrs = gck_object_get (object, NULL, &err, CKA_CLASS, CKA_LABEL, GCK_INVALID);
	SUCCESS_RES (attrs, err);
	if (attrs != NULL) {
		g_assert (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == CKO_DATA);
		g_assert (gck_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "TEST LABEL") == 0);
		g_free (value); value = NULL;
	}
	gck_attributes_unref (attrs);

	/* Full */
	attrs = gck_object_get_full (object, attr_types, G_N_ELEMENTS (attr_types), NULL, &err);
	SUCCESS_RES (attrs, err);
	if (attrs != NULL) {
		g_assert (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == CKO_DATA);
		g_assert (gck_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "TEST LABEL") == 0);
		g_free (value); value = NULL;
	}
	gck_attributes_unref (attrs);

	/* Async */
	gck_object_get_async (object, attr_types, G_N_ELEMENTS (attr_types), NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	attrs = gck_object_get_finish (object, result, &err);
	g_object_unref (result);
	SUCCESS_RES (attrs, err);
	if (attrs != NULL) {
		g_assert (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == CKO_DATA);
		g_assert (gck_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "TEST LABEL") == 0);
		g_free (value); value = NULL;
	}
	gck_attributes_unref (attrs);
}

DEFINE_TEST(get_data_attribute)
{
	GAsyncResult *result = NULL;
	CK_OBJECT_CLASS_PTR klass;
	gsize n_data;
	GError *err = NULL;

	/* Simple */
	klass = gck_object_get_data (object, CKA_CLASS, NULL, &n_data, &err);
	SUCCESS_RES (klass, err);
	if (klass != NULL) {
		g_assert (n_data == sizeof (CK_OBJECT_CLASS));
		g_assert (*klass == CKO_DATA);
		g_free (klass);
	}

	/* Full */
	klass = gck_object_get_data_full (object, CKA_CLASS, NULL, NULL, &n_data, &err);
	SUCCESS_RES (klass, err);
	if (klass != NULL) {
		g_assert (n_data == sizeof (CK_OBJECT_CLASS));
		g_assert (*klass == CKO_DATA);
		g_free (klass);
	}

	/* Async */
	gck_object_get_data_async (object, CKA_CLASS, NULL, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	klass = gck_object_get_data_finish (object, result, &n_data, &err);
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
	GckAttributes *attrs, *templ;
	GError *err = NULL;
	gulong klass;
	gchar *value = NULL;
	gboolean ret;

	templ = gck_attributes_new ();
	gck_attributes_add_ulong (templ, CKA_CLASS, 6);
	gck_attributes_add_string (templ, CKA_LABEL, "CHANGE TWO");

	/* Full */
	ret = gck_object_set (object, templ, NULL, &err);
	gck_attributes_unref (templ);
	SUCCESS_RES (ret, err);
	if (ret) {
		attrs = gck_object_get (object, NULL, &err, CKA_CLASS, CKA_LABEL, GCK_INVALID);
		g_assert (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == 6);
		g_assert (gck_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "CHANGE TWO") == 0);
		g_free (value); value = NULL;
		gck_attributes_unref (attrs);
	}

	templ = gck_attributes_new ();
	gck_attributes_add_ulong (templ, CKA_CLASS, 7);
	gck_attributes_add_string (templ, CKA_LABEL, "CHANGE THREE");

	/* Async */
	gck_object_set_async (object, templ, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	ret = gck_object_set_finish (object, result, &err);
	g_object_unref (result);
	SUCCESS_RES (ret, err);
	if (ret) {
		attrs = gck_object_get (object, NULL, &err, CKA_CLASS, CKA_LABEL, GCK_INVALID);
		g_assert (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) && klass == 7);
		g_assert (gck_attributes_find_string (attrs, CKA_LABEL, &value) && strcmp (value, "CHANGE THREE") == 0);
		g_free (value); value = NULL;
		gck_attributes_unref (attrs);
	}
}

DEFINE_TEST(find_objects)
{
	GAsyncResult *result = NULL;
	GckAttributes *templ, *attrs;
	GList *objects;
	GckObject *testobj;
	GError *err = NULL;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_DATA);
	gck_attributes_add_string (attrs, CKA_LABEL, "UNIQUE LABEL");
	testobj = gck_session_create_object (session, attrs, NULL, &err);
	gck_attributes_unref (attrs);
	g_object_unref (testobj);

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_DATA);
	gck_attributes_add_string (attrs, CKA_LABEL, "OTHER LABEL");
	testobj = gck_session_create_object (session, attrs, NULL, &err);
	gck_attributes_unref (attrs);
	g_object_unref (testobj);

	/* Simple, "TEST LABEL" */
	attrs = gck_attributes_new ();
	gck_attributes_add_string (attrs, CKA_LABEL, "UNIQUE LABEL");
	objects = gck_session_find_objects (session, attrs, NULL, &err);
	SUCCESS_RES (objects, err);
	g_assert (g_list_length (objects) == 1);
	gck_list_unref_free (objects);
	gck_attributes_unref (attrs);

	/* Full, All */
	templ = gck_attributes_new ();
	objects = gck_session_find_objects (session, templ, NULL, &err);
	SUCCESS_RES (objects, err);
	g_assert (g_list_length (objects) > 1);
	gck_list_unref_free (objects);

	/* Async, None */
	gck_attributes_add_string (templ, CKA_LABEL, "blah blah");
	gck_session_find_objects_async (session, templ, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	objects = gck_session_find_objects_finish (session, result, &err);
	g_object_unref (result);
	g_assert (objects == NULL);
	gck_list_unref_free (objects);
}
