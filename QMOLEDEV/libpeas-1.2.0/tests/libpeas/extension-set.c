/*
 * extension-set.c
 * This file is part of libpeas
 *
 * Copyright (C) 2010 - Garrett Regier
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>

#include <glib.h>
#include <libpeas/peas.h>

#include "testing/testing.h"

typedef struct _TestFixture TestFixture;

struct _TestFixture {
  PeasEngine *engine;
};

/* Have dependencies before the plugin that requires them */
static const gchar *loadable_plugins[] = {
  "loadable", "has-dep", "self-dep"
};

static void
extension_added_cb (PeasExtensionSet *extension_set,
                    PeasPluginInfo   *info,
                    PeasExtension    *extension,
                    gint             *active)
{
  ++(*active);
}

static void
extension_removed_cb (PeasExtensionSet *extension_set,
                      PeasPluginInfo   *info,
                      PeasExtension    *extension,
                      gint             *active)
{
  --(*active);
}

static void
sync_active_extensions (PeasExtensionSet *extension_set,
                        gint             *active)
{
  *active = 0;

  g_signal_connect (extension_set,
                    "extension-added",
                    G_CALLBACK (extension_added_cb),
                    active);
  g_signal_connect (extension_set,
                    "extension-removed",
                    G_CALLBACK (extension_removed_cb),
                    active);
}

static void
test_setup (TestFixture   *fixture,
            gconstpointer  data)
{
  fixture->engine = testing_engine_new ();
}

static void
test_teardown (TestFixture   *fixture,
               gconstpointer  data)
{
  testing_engine_free (fixture->engine);
}

static void
test_runner (TestFixture   *fixture,
             gconstpointer  data)
{
  ((void (*) (PeasEngine *engine)) data) (fixture->engine);
}

static void
test_extension_set_create_valid (PeasEngine *engine)
{
  PeasExtensionSet *extension_set;

  extension_set = peas_extension_set_new (engine,
                                          PEAS_TYPE_ACTIVATABLE,
                                          "object", NULL,
                                          NULL);

  g_object_unref (extension_set);
}

static void
test_extension_set_create_invalid (PeasEngine *engine)
{
  PeasExtensionSet *extension_set;

  testing_util_push_log_hook ("*assertion `G_TYPE_IS_INTERFACE (*)' failed");
  testing_util_push_log_hook ("*type 'PeasActivatable' has no property named 'invalid-property'");

  /* Invalid GType */
  extension_set = peas_extension_set_new (engine, G_TYPE_INVALID, NULL);
  g_assert (!PEAS_IS_EXTENSION_SET (extension_set));


  /* GObject but not a GInterface */
  extension_set = peas_extension_set_new (engine, G_TYPE_OBJECT, NULL);
  g_assert (!PEAS_IS_EXTENSION_SET (extension_set));


  /* Interface does not have an 'invalid-property' property */
  extension_set = peas_extension_set_new (engine,
                                          PEAS_TYPE_ACTIVATABLE,
                                          "invalid-property", "does-not-exist",
                                          NULL);
  g_assert (!PEAS_IS_EXTENSION_SET (extension_set));
}

static void
test_extension_set_activate (PeasEngine *engine)
{
  gint i, active;
  PeasPluginInfo *info;
  PeasExtensionSet *extension_set;

  extension_set = peas_extension_set_new (engine,
                                          PEAS_TYPE_ACTIVATABLE,
                                          "object", NULL,
                                          NULL);

  sync_active_extensions (extension_set, &active);

  for (i = 0; i < G_N_ELEMENTS (loadable_plugins); ++i)
    {
      g_assert_cmpint (active, ==, i);

      info = peas_engine_get_plugin_info (engine, loadable_plugins[i]);

      g_assert (peas_engine_load_plugin (engine, info));
    }

  /* Load a plugin that does not provide a PeasActivatable */
  info = peas_engine_get_plugin_info (engine, "extension-c");
  g_assert (peas_engine_load_plugin (engine, info));

  g_assert_cmpint (active, ==, G_N_ELEMENTS (loadable_plugins));

  g_object_unref (extension_set);

  g_assert_cmpint (active, ==, 0);
}

static void
test_extension_set_deactivate (PeasEngine *engine)
{
  gint i, active;
  PeasPluginInfo *info;
  PeasExtensionSet *extension_set;

  extension_set = peas_extension_set_new (engine,
                                          PEAS_TYPE_ACTIVATABLE,
                                          "object", NULL,
                                          NULL);

  sync_active_extensions (extension_set, &active);

  test_extension_set_activate (engine);

  /* Unload the plugin that does not provide a PeasActivatable */
  info = peas_engine_get_plugin_info (engine, "extension-c");
  g_assert (peas_engine_unload_plugin (engine, info));

  /* To keep deps in order */
  for (i = G_N_ELEMENTS (loadable_plugins); i > 0; --i)
    {
      g_assert_cmpint (active, ==, i);

      info = peas_engine_get_plugin_info (engine, loadable_plugins[i - 1]);

      g_assert (peas_engine_unload_plugin (engine, info));
    }

  g_assert_cmpint (active, ==, 0);

  g_object_unref (extension_set);
}

static void
test_extension_set_get_extension (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;
  PeasExtensionSet *extension_set;

  info = peas_engine_get_plugin_info (engine, loadable_plugins[0]);

  extension_set = peas_extension_set_new (engine,
                                          PEAS_TYPE_ACTIVATABLE,
                                          "object", NULL,
                                          NULL);

  g_assert (peas_extension_set_get_extension (extension_set, info) == NULL);
  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_extension_set_get_extension (extension_set, info);

  g_assert (PEAS_IS_ACTIVATABLE (extension));

  g_object_unref (extension_set);
}

static void
test_extension_set_call_valid (PeasEngine *engine)
{
  PeasExtensionSet *extension_set;

  test_extension_set_activate (engine);

  extension_set = peas_extension_set_new (engine,
                                          PEAS_TYPE_ACTIVATABLE,
                                          "object", NULL,
                                          NULL);

  g_assert (peas_extension_set_call (extension_set, "activate", NULL));

  g_object_unref (extension_set);
}

static void
test_extension_set_call_invalid (PeasEngine *engine)
{
  PeasExtensionSet *extension_set;

  test_extension_set_activate (engine);

  testing_util_push_log_hook ("*Method 'PeasActivatable.invalid' not found*");

  extension_set = peas_extension_set_new (engine,
                                          PEAS_TYPE_ACTIVATABLE,
                                          "object", NULL,
                                          NULL);

  g_assert (!peas_extension_set_call (extension_set, "invalid", NULL));

  g_object_unref (extension_set);
}

static void
test_extension_set_foreach (PeasEngine *engine)
{
  PeasExtensionSet *extension_set;
  gint active = 0;

  test_extension_set_activate (engine);

  extension_set = peas_extension_set_new (engine,
                                          PEAS_TYPE_ACTIVATABLE,
                                          "object", NULL,
                                          NULL);

  peas_extension_set_foreach (extension_set,
                              (PeasExtensionSetForeachFunc) extension_added_cb,
                              &active);

  g_assert_cmpint (active, ==, G_N_ELEMENTS (loadable_plugins));

  g_object_unref (extension_set);
}

int
main (int    argc,
      char **argv)
{
  g_test_init (&argc, &argv, NULL);

  g_type_init ();

#define TEST(path, ftest) \
  g_test_add ("/extension-set/" path, TestFixture, \
              (gpointer) test_extension_set_##ftest, \
              test_setup, test_runner, test_teardown)

  TEST ("create-valid", create_valid);
  TEST ("create-invalid", create_invalid);

  TEST ("activate", activate);
  TEST ("deactivate", deactivate);

  TEST ("get-extension", get_extension);

  TEST ("call-valid", call_valid);
  TEST ("call-invalid", call_invalid);

  TEST ("foreach", foreach);

#undef TEST

  return testing_run_tests ();
}
