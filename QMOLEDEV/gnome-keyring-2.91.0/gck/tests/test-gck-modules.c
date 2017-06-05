
#include <glib.h>
#include <string.h>

#include "test-suite.h"
#include "gck-test.h"

static GList *modules = NULL;

DEFINE_SETUP(modules)
{
	GckModule *module;
	GError *err = NULL;

	/* Successful load */
	module = gck_module_initialize (".libs/libmock-test-module.so", NULL, 0, &err);
	SUCCESS_RES (module, err);

	modules = g_list_append (NULL, module);
}

DEFINE_TEARDOWN(modules)
{
	gck_list_unref_free (modules);
	modules = NULL;
}

DEFINE_TEST(modules_enumerate_objects)
{
	GckAttributes *attrs;
	GError *error = NULL;
	GckEnumerator *en;
	GList *objects;

	attrs = gck_attributes_new ();
	gck_attributes_add_string (attrs, CKA_LABEL, "Private Capitalize Key");
	en = gck_modules_enumerate_objects (modules, attrs, 0);
	g_assert (GCK_IS_ENUMERATOR (en));
	gck_attributes_unref (attrs);

	objects = gck_enumerator_next_n (en, -1, NULL, &error);
	SUCCESS_RES (objects, error);
	g_assert_cmpint (g_list_length (objects), ==, 1);
	g_assert (GCK_IS_OBJECT (objects->data));

	gck_list_unref_free (objects);
	g_object_unref (en);
}


DEFINE_TEST(modules_token_for_uri)
{
	GckSlot *slot;
	GError *error = NULL;

	slot = gck_modules_token_for_uri (modules, "pkcs11:token=TEST%20LABEL", &error);
	g_assert (GCK_IS_SLOT (slot));

	g_object_unref (slot);
}

DEFINE_TEST(modules_token_for_uri_not_found)
{
	GckSlot *slot;
	GError *error = NULL;

	slot = gck_modules_token_for_uri (modules, "pkcs11:token=UNKNOWN", &error);
	g_assert (slot == NULL);
	g_assert (error == NULL);
}

DEFINE_TEST(modules_token_for_uri_error)
{
	GckSlot *slot;
	GError *error = NULL;

	slot = gck_modules_token_for_uri (modules, "http://invalid.uri", &error);
	g_assert (slot == NULL);
	g_assert (error != NULL);
	g_assert (g_error_matches (error, GCK_URI_ERROR, GCK_URI_BAD_PREFIX));
	g_error_free (error);
}

DEFINE_TEST(modules_object_for_uri)
{
	GckObject *object;
	GError *error = NULL;

	object = gck_modules_object_for_uri (modules, "pkcs11:object=Public%20Capitalize%20Key;objecttype=public", 0, &error);
	g_assert (GCK_IS_OBJECT (object));
	g_object_unref (object);
}

DEFINE_TEST(modules_object_for_uri_not_found)
{
	GckObject *object;
	GError *error = NULL;

	object = gck_modules_object_for_uri (modules, "pkcs11:object=Unknown%20Label", 0, &error);
	g_assert (object == NULL);
	g_assert (error == NULL);
}

DEFINE_TEST(modules_object_for_uri_error)
{
	GckObject *object;
	GError *error = NULL;

	object = gck_modules_object_for_uri (modules, "http://invalid.uri", 0, &error);
	g_assert (object == NULL);
	g_assert (error != NULL);
	g_assert (g_error_matches (error, GCK_URI_ERROR, GCK_URI_BAD_PREFIX));
	g_error_free (error);
}

DEFINE_TEST(modules_objects_for_uri)
{
	GList *objects;
	GError *error = NULL;

	objects = gck_modules_objects_for_uri (modules, "pkcs11:token=TEST%20LABEL", 0, &error);
	g_assert (objects);
	g_assert (!error);
	g_assert_cmpint (g_list_length (objects), ==, 5);

	gck_list_unref_free (objects);
}

DEFINE_TEST(modules_enumerate_uri)
{
	GckEnumerator *en;
	GList *objects;
	GError *error = NULL;

	en = gck_modules_enumerate_uri (modules, "pkcs11:token=TEST%20LABEL", 0, &error);
	g_assert (GCK_IS_ENUMERATOR (en));
	g_assert (!error);

	objects = gck_enumerator_next_n (en, -1, NULL, &error);
	g_assert_cmpint (g_list_length (objects), ==, 5);
	g_assert (!error);

	g_object_unref (en);
	gck_list_unref_free (objects);
}
