/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-gck-module.c - the GObject PKCS#11 wrapper library

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

#include <errno.h>
#include <glib.h>
#include <string.h>

#include "gck/gck.h"
#include "gck/gck-test.h"

typedef struct {
	GckModule *module;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	GError *err = NULL;

	/* Successful load */
	test->module = gck_module_initialize (BUILDDIR "/.libs/libmock-test-module.so", &err);
	g_assert_no_error (err);
	g_assert (test->module);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->module);
}

static void
test_invalid_modules (Test *test, gconstpointer unused)
{
	GckModule *invalid;
	GError *err = NULL;

	/* Shouldn't be able to load modules */
	invalid = gck_module_initialize ("blah-blah-non-existant", &err);
	g_assert (invalid == NULL);
	g_assert_error (err, GCK_ERROR, CKR_GCK_MODULE_PROBLEM);

	g_clear_error (&err);

	/* Shouldn't be able to load any file successfully */
	invalid = gck_module_initialize ("/usr/lib/libm.so", &err);
	g_assert (invalid == NULL);
	g_assert_error (err, GCK_ERROR, CKR_GCK_MODULE_PROBLEM);
}

static void
test_module_equals_hash (Test *test, gconstpointer unused)
{
	GckModule *other;
	GObject *obj;
	guint hash;

	hash = gck_module_hash (test->module);
	g_assert (hash != 0);

	g_assert (gck_module_equal (test->module, test->module));

	other = gck_module_new (gck_module_get_functions (test->module));
	obj = g_object_new (G_TYPE_OBJECT, NULL);

	g_assert (gck_module_equal (test->module, other));

	/* TODO: Could do with another test for inequality */
	g_assert (!gck_module_equal (test->module, obj));

	g_object_unref (other);
	g_object_unref (obj);
}

static void
test_module_props (Test *test, gconstpointer unused)
{
	gchar *path;

	g_object_get (test->module, "path", &path, NULL);
	g_assert (path != NULL && "no module-path");
	g_assert (strcmp (BUILDDIR "/.libs/libmock-test-module.so", path) == 0 && "module path wrong");
	g_free (path);
}

static void
test_module_info (Test *test, gconstpointer unused)
{
	GckModuleInfo *info;

	info = gck_module_get_info (test->module);
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

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gck/module/invalid_modules", Test, NULL, setup, test_invalid_modules, teardown);
	g_test_add ("/gck/module/module_equals_hash", Test, NULL, setup, test_module_equals_hash, teardown);
	g_test_add ("/gck/module/module_props", Test, NULL, setup, test_module_props, teardown);
	g_test_add ("/gck/module/module_info", Test, NULL, setup, test_module_info, teardown);

	return g_test_run ();
}
