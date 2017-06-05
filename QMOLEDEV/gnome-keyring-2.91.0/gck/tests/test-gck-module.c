
#include <glib.h>
#include <string.h>

#include "test-suite.h"
#include "gck-test.h"

static GckModule *module = NULL;

DEFINE_SETUP(load_module)
{
	GError *err = NULL;

	/* Successful load */
	module = gck_module_initialize (".libs/libmock-test-module.so", NULL, 0, &err);
	SUCCESS_RES (module, err);
}

DEFINE_TEARDOWN(load_module)
{
	g_object_unref (module);
}

DEFINE_TEST(invalid_modules)
{
	GckModule *invalid;
	GError *err = NULL;

	/* Shouldn't be able to load modules */
	invalid = gck_module_initialize ("blah-blah-non-existant", NULL, 0, &err);
	FAIL_RES (invalid, err);

	/* Shouldn't be able to load any file successfully */
	invalid = gck_module_initialize ("/usr/lib/libm.so", NULL, 0, &err);
	FAIL_RES (invalid, err);
}

DEFINE_TEST(module_equals_hash)
{
	GckModule *other;
	GObject *obj;
	guint hash;

	hash = gck_module_hash (module);
	g_assert (hash != 0);

	g_assert (gck_module_equal (module, module));

	other = gck_module_new (gck_module_get_functions (module), 0);
	obj = g_object_new (G_TYPE_OBJECT, NULL);

	g_assert (gck_module_equal (module, other));

	/* TODO: Could do with another test for inequality */
	g_assert (!gck_module_equal (module, obj));

	g_object_unref (other);
	g_object_unref (obj);
}

DEFINE_TEST(module_props)
{
	gchar *path;

	g_object_get (module, "path", &path, NULL);
	g_assert (path != NULL && "no module-path");
	g_assert (strcmp (".libs/libmock-test-module.so", path) == 0 && "module path wrong");
	g_free (path);
}

DEFINE_TEST(module_info)
{
	GckModuleInfo *info;

	info = gck_module_get_info (module);
	g_assert (info != NULL && "no module info");

	g_assert (info->pkcs11_version_major == CRYPTOKI_VERSION_MAJOR && "wrong major version");
	g_assert (info->pkcs11_version_minor == CRYPTOKI_VERSION_MINOR && "wrong minor version");
	g_assert (strcmp ("TEST MANUFACTURER", info->manufacturer_id) == 0);
	g_assert (strcmp ("TEST LIBRARY", info->library_description) == 0);
	g_assert (0 == info->flags);
	g_assert (45 == info->library_version_major);
	g_assert (145 == info->library_version_minor);

	gck_module_info_free (info);
}

#if 0
static int n_objects = 0;
static GckObject *last_object = NULL;

static gboolean
for_each_object (GckObject *object, gpointer user_data)
{
	g_assert (GCK_IS_OBJECT (object));
	g_assert_cmpstr ("blah", ==, user_data);
	g_assert (user_data);

	if (last_object)
		g_object_unref (last_object);
	last_object = g_object_ref (object);

	++n_objects;

	return TRUE;
}

static gboolean
for_first_object (GckObject *object, gpointer user_data)
{
	g_assert (GCK_IS_OBJECT (object));
	g_assert_cmpstr ("first", ==, user_data);
	g_assert (user_data);

	if (last_object)
		g_object_unref (last_object);
	last_object = g_object_ref (object);

	++n_objects;

	return FALSE;
}
#endif

DEFINE_TEST(module_enumerate)
{
#if 0
	GckSession *session;
	GckAttributes *attrs;
	gboolean ret;
	GList *modules;

	modules = g_list_prepend (NULL, g_object_ref (module));

	attrs = gck_attributes_new ();
	ret = gck_modules_enumerate_objects (modules, attrs, 0, NULL, for_first_object, "first", NULL);
	g_assert (ret);
	g_assert_cmpint (n_objects, ==, 1);
	g_assert (GCK_IS_OBJECT (last_object));
	gck_attributes_unref (attrs);

	session = gck_object_get_session (last_object);
	g_assert (GCK_IS_SESSION (session));
	g_object_unref (session);

	g_object_unref (last_object);
	last_object = NULL;
	n_objects = 0;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_PRIVATE_KEY);
	ret = gck_modules_enumerate_objects (modules, attrs, 0, NULL, for_each_object, "blah", NULL);
	g_assert (ret);
	g_assert_cmpint (n_objects, ==, 2);
	g_assert (GCK_IS_OBJECT (last_object));
	gck_attributes_unref (attrs);

	session = gck_object_get_session (last_object);
	g_assert (GCK_IS_SESSION (session));
	g_object_unref (session);

	g_object_unref (last_object);
	last_object = NULL;
	n_objects = 0;

	gck_list_unref_free (modules);
#endif
}
