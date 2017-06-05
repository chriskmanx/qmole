
#include <glib.h>
#include <string.h>

#include "test-suite.h"
#include "gck-test.h"
#include "gck-private.h"

static GList *modules = NULL;

DEFINE_SETUP(enumerator)
{
	GckModule *module;
	GError *err = NULL;

	/* Successful load */
	module = gck_module_initialize (".libs/libmock-test-module.so", NULL, 0, &err);
	SUCCESS_RES (module, err);

	modules = g_list_append (NULL, module);
}

DEFINE_TEARDOWN(enumerator)
{
	gck_list_unref_free (modules);
	modules = NULL;
}

DEFINE_TEST(enumerator_create)
{
	GckEnumerator *en;

	en = _gck_enumerator_new (modules, 0, NULL, NULL);
	g_assert (GCK_IS_ENUMERATOR (en));
	g_object_unref (en);
}

DEFINE_TEST(enumerator_next)
{
	GError *error = NULL;
	GckEnumerator *en;
	GckObject *obj;

	en = _gck_enumerator_new (modules, 0, NULL, NULL);
	g_assert (GCK_IS_ENUMERATOR (en));

	obj = gck_enumerator_next (en, NULL, &error);
	g_assert (GCK_IS_OBJECT (obj));

	g_object_unref (obj);
	g_object_unref (en);
}

DEFINE_TEST(enumerator_next_and_resume)
{
	GError *error = NULL;
	GckEnumerator *en;
	GckObject *obj, *obj2;

	en = _gck_enumerator_new (modules, 0, NULL, NULL);
	g_assert (GCK_IS_ENUMERATOR (en));

	obj = gck_enumerator_next (en, NULL, &error);
	SUCCESS_RES (obj, error);
	g_assert (GCK_IS_OBJECT (obj));

	obj2 = gck_enumerator_next (en, NULL, &error);
	SUCCESS_RES (obj2, error);
	g_assert (GCK_IS_OBJECT (obj2));

	g_assert (!gck_object_equal (obj, obj2));

	g_object_unref (obj);
	g_object_unref (obj2);
	g_object_unref (en);
}

DEFINE_TEST(enumerator_next_n)
{
	GError *error = NULL;
	GckEnumerator *en;
	GList *objects, *l;

	en = _gck_enumerator_new (modules, 0, NULL, NULL);
	g_assert (GCK_IS_ENUMERATOR (en));

	objects = gck_enumerator_next_n (en, -1, NULL, &error);
	SUCCESS_RES (objects, error);
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
	testing_wait_stop ();
}

DEFINE_TEST(enumerator_next_async)
{
	GAsyncResult *result = NULL;
	GError *error = NULL;
	GckEnumerator *en;
	GList *objects, *l;

	en = _gck_enumerator_new (modules, 0, NULL, NULL);
	g_assert (GCK_IS_ENUMERATOR (en));

	gck_enumerator_next_async (en, -1, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result);

	objects = gck_enumerator_next_finish (en, result, &error);
	SUCCESS_RES (objects, error);
	g_assert_cmpint (g_list_length (objects), ==, 5);
	for (l = objects; l; l = g_list_next (l))
		g_assert (GCK_IS_OBJECT (l->data));

	g_object_unref (result);
	gck_list_unref_free (objects);
	g_object_unref (en);
}

DEFINE_TEST(enumerator_attributes)
{
	GckAttributes *attrs;
	GError *error = NULL;
	GckEnumerator *en;
	GList *objects;

	attrs = gck_attributes_new ();
	gck_attributes_add_string (attrs, CKA_LABEL, "Private Capitalize Key");
	en = _gck_enumerator_new (modules, 0, NULL, attrs);
	g_assert (GCK_IS_ENUMERATOR (en));
	gck_attributes_unref (attrs);

	objects = gck_enumerator_next_n (en, -1, NULL, &error);
	SUCCESS_RES (objects, error);
	g_assert_cmpint (g_list_length (objects), ==, 1);
	g_assert (GCK_IS_OBJECT (objects->data));

	gck_list_unref_free (objects);
	g_object_unref (en);
}

DEFINE_TEST(enumerator_token_match)
{
	GckTokenInfo *token;
	GError *error = NULL;
	GckEnumerator *en;
	GList *objects;

	token = g_new0 (GckTokenInfo, 1);
	token->label = g_strdup ("Invalid token name");
	en = _gck_enumerator_new (modules, 0, token, NULL);
	g_assert (GCK_IS_ENUMERATOR (en));

	objects = gck_enumerator_next_n (en, -1, NULL, &error);
	g_assert_cmpint (g_list_length (objects), ==, 0);
	g_assert (error == NULL);

	gck_list_unref_free (objects);
	g_object_unref (en);
}
