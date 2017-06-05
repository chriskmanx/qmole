
#include <glib.h>
#include <string.h>

#include "test-suite.h"
#include "gp11-test.h"

static GP11Module *module = NULL;

DEFINE_SETUP(load_module)
{
	GError *err = NULL;

	/* Successful load */
	module = gp11_module_initialize (".libs/libgp11-test-module.so", NULL, &err);
	SUCCESS_RES (module, err);
}

DEFINE_TEARDOWN(load_module)
{
	g_object_unref (module);
}

DEFINE_TEST(invalid_modules)
{
	GP11Module *invalid;
	GError *err = NULL;
	
	/* Shouldn't be able to load modules */
	invalid = gp11_module_initialize ("blah-blah-non-existant", NULL, &err);
	FAIL_RES (invalid, err);

	/* Shouldn't be able to load any file successfully */ 
	invalid = gp11_module_initialize ("/usr/lib/libm.so", NULL, &err);
	FAIL_RES (invalid, err);
}

DEFINE_TEST(module_equals_hash)
{
	GP11Module *other;
	GObject *obj;
	guint hash;
	
	hash = gp11_module_hash (module);
	g_assert (hash != 0);
	
	g_assert (gp11_module_equal (module, module));
	
	other = gp11_module_new (gp11_module_get_functions (module));
	obj = g_object_new (G_TYPE_OBJECT, NULL);
	
	g_assert (gp11_module_equal (module, other));
	
	/* TODO: Could do with another test for inequality */
	g_assert (!gp11_module_equal (module, obj));
	
	g_object_unref (other);
	g_object_unref (obj);
}

DEFINE_TEST(module_props)
{
	gchar *path;

	g_object_get (module, "path", &path, NULL);
	g_assert (path != NULL && "no module-path");
	g_assert (strcmp (".libs/libgp11-test-module.so", path) == 0 && "module path wrong");
	g_free (path);
}

DEFINE_TEST(module_info)
{
	GP11ModuleInfo *info;
	
	info = gp11_module_get_info (module);
	g_assert (info != NULL && "no module info");
	
	g_assert (info->pkcs11_version_major == CRYPTOKI_VERSION_MAJOR && "wrong major version"); 
	g_assert (info->pkcs11_version_minor == CRYPTOKI_VERSION_MINOR && "wrong minor version"); 
	g_assert (strcmp ("TEST MANUFACTURER", info->manufacturer_id) == 0);
	g_assert (strcmp ("TEST LIBRARY", info->library_description) == 0);
	g_assert (0 == info->flags);
	g_assert (45 == info->library_version_major);
	g_assert (145 == info->library_version_minor);
	
	gp11_module_info_free (info);
}

static int n_objects = 0;
static GP11Object *last_object = NULL;

static gboolean
for_each_object (GP11Object *object, gpointer user_data)
{
	g_assert (GP11_IS_OBJECT (object));
	g_assert_cmpstr ("blah", ==, user_data);
	g_assert (user_data);
	
	if (last_object)
		g_object_unref (last_object);
	last_object = g_object_ref (object);
	
	++n_objects;
	
	return TRUE;
}

static gboolean
for_first_object (GP11Object *object, gpointer user_data)
{
	g_assert (GP11_IS_OBJECT (object));
	g_assert_cmpstr ("first", ==, user_data);
	g_assert (user_data);
	
	if (last_object)
		g_object_unref (last_object);
	last_object = g_object_ref (object);
	
	++n_objects;
	
	return FALSE;
}

DEFINE_TEST(module_enumerate)
{
	GP11Session *session;
	GP11Attributes *attrs;
	gboolean ret;
	
	attrs = gp11_attributes_new ();
	ret = gp11_module_enumerate_objects_full (module, attrs, NULL, for_first_object, "first", NULL);
	g_assert (ret);
	g_assert_cmpint (n_objects, ==, 1);
	g_assert (GP11_IS_OBJECT (last_object));
	gp11_attributes_unref (attrs);
	
	session = gp11_object_get_session (last_object);
	g_assert (GP11_IS_SESSION (session));
	g_object_unref (session);
	
	g_object_unref (last_object);
	last_object = NULL;
	n_objects = 0;

	ret = gp11_module_enumerate_objects (module, for_each_object, "blah", 
	                                     CKA_CLASS, GP11_ULONG, CKO_PRIVATE_KEY,
	                                     GP11_INVALID);
	g_assert (ret);
	g_assert_cmpint (n_objects, ==, 2);
	g_assert (GP11_IS_OBJECT (last_object));
	
	session = gp11_object_get_session (last_object);
	g_assert (GP11_IS_SESSION (session));
	g_object_unref (session);
	
	g_object_unref (last_object);
	last_object = NULL;
	n_objects = 0;
}
