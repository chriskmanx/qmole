/*
 * engine.c
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

#include "libpeas/peas-engine-priv.h"

#include "testing/testing.h"

typedef struct _TestFixture TestFixture;

struct _TestFixture {
  PeasEngine *engine;
};

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
test_engine_new (PeasEngine *engine)
{
  PeasEngine *new_engine;

  new_engine = peas_engine_new ();

  g_assert (engine != NULL);
  g_assert (new_engine != NULL);

  /* Does not return the same engine */
  g_assert (engine != new_engine);
  /* peas_engine_new() sets the default engine */
  g_assert (engine == peas_engine_get_default ());

  g_object_unref (new_engine);
}

static void
test_engine_dispose (PeasEngine *engine)
{
  /* Yes this really has failed before */
  g_object_run_dispose (G_OBJECT (engine));
  g_object_run_dispose (G_OBJECT (engine));
  g_object_run_dispose (G_OBJECT (engine));

  /* Seems we have to explicitly unref it
   * because testing_engine_free() does not
   */
  g_object_unref (G_OBJECT (engine));
}

static void
test_engine_get_default (void)
{
  g_assert (peas_engine_get_default () == peas_engine_get_default ());

  g_object_unref (peas_engine_get_default ());
}

static void
test_engine_load_plugin (PeasEngine *engine)
{
  PeasPluginInfo *info;

  info = peas_engine_get_plugin_info (engine, "loadable");

  g_assert (peas_engine_load_plugin (engine, info));
  g_assert (peas_plugin_info_is_loaded (info));

  /* Check that we can load a plugin that is already loaded */
  g_assert (peas_engine_load_plugin (engine, info));
}

static void
test_engine_load_plugin_with_dep (PeasEngine *engine)
{
  PeasPluginInfo *info;

  info = peas_engine_get_plugin_info (engine, "has-dep");

  g_assert (peas_engine_load_plugin (engine, info));
  g_assert (peas_plugin_info_is_loaded (info));

  info = peas_engine_get_plugin_info (engine, "loadable");

  g_assert (peas_plugin_info_is_loaded (info));
}

static void
test_engine_load_plugin_with_self_dep (PeasEngine *engine)
{
  PeasPluginInfo *info;

  info = peas_engine_get_plugin_info (engine, "self-dep");

  g_assert (peas_engine_load_plugin (engine, info));
  g_assert (peas_plugin_info_is_loaded (info));
}

static void
test_engine_load_plugin_with_nonexistent_dep (PeasEngine *engine)
{
  GError *error = NULL;
  PeasPluginInfo *info;

  testing_util_push_log_hook ("Could not find plugin 'does-not-exist'*");

  info = peas_engine_get_plugin_info (engine, "nonexistent-dep");

  g_assert (!peas_engine_load_plugin (engine, info));
  g_assert (!peas_plugin_info_is_loaded (info));
  g_assert (!peas_plugin_info_is_available (info, &error));
  g_assert_error (error, PEAS_PLUGIN_INFO_ERROR,
                  PEAS_PLUGIN_INFO_ERROR_DEP_NOT_FOUND);

  g_error_free (error);
}

static void
test_engine_unload_plugin (PeasEngine *engine)
{
  PeasPluginInfo *info;

  info = peas_engine_get_plugin_info (engine, "loadable");

  /* Check that we can unload a plugin that is not loaded */
  g_assert (peas_engine_unload_plugin (engine, info));

  test_engine_load_plugin (engine);

  g_assert (peas_engine_unload_plugin (engine, info));
  g_assert (!peas_plugin_info_is_loaded (info));
}

static void
test_engine_unload_plugin_with_dep (PeasEngine *engine)
{
  PeasPluginInfo *info;

  test_engine_load_plugin_with_dep (engine);

  info = peas_engine_get_plugin_info (engine, "loadable");

  g_assert (peas_engine_unload_plugin (engine, info));
  g_assert (!peas_plugin_info_is_loaded (info));

  info = peas_engine_get_plugin_info (engine, "has-dep");

  g_assert (!peas_plugin_info_is_loaded (info));
}

static void
test_engine_unload_plugin_with_self_dep (PeasEngine *engine)
{
  PeasPluginInfo *info;

  test_engine_load_plugin_with_self_dep (engine);

  info = peas_engine_get_plugin_info (engine, "self-dep");

  g_assert (peas_engine_unload_plugin (engine, info));
  g_assert (!peas_plugin_info_is_loaded (info));
}

static void
test_engine_unavailable_plugin (PeasEngine *engine)
{
  PeasPluginInfo *info;

  testing_util_push_log_hook ("Could not find plugin 'does-not-exist'*");

  info = peas_engine_get_plugin_info (engine, "unavailable");

  g_assert (!peas_engine_load_plugin (engine, info));
  g_assert (!peas_plugin_info_is_loaded (info));
  g_assert (!peas_plugin_info_is_available (info, NULL));
}

static void
test_engine_not_loadable_plugin (PeasEngine *engine)
{
  GError *error = NULL;
  PeasPluginInfo *info;

  testing_util_push_log_hook ("*libnot-loadable.so: cannot open shared "
                              "object file: No such file or directory");
  testing_util_push_log_hook ("Could not load plugin module: 'not-loadable'");
  testing_util_push_log_hook ("Error loading plugin 'not-loadable'");

  info = peas_engine_get_plugin_info (engine, "not-loadable");

  g_assert (!peas_engine_load_plugin (engine, info));
  g_assert (!peas_plugin_info_is_loaded (info));
  g_assert (!peas_plugin_info_is_available (info, &error));
  g_assert_error (error, PEAS_PLUGIN_INFO_ERROR,
                  PEAS_PLUGIN_INFO_ERROR_LOADING_FAILED);

  g_error_free (error);
}

static void
load_plugin_cb (PeasEngine     *engine,
                PeasPluginInfo *info,
                gint           *loaded)
{
  /* PeasEngine:load is not stopped if loading fails */
  if (peas_plugin_info_is_loaded (info))
    ++(*loaded);
}

static void
unload_plugin_cb (PeasEngine     *engine,
                  PeasPluginInfo *info,
                  gint           *loaded)
{
  --(*loaded);
}

static void
notify_loaded_plugins_cb (PeasEngine   *engine,
                          GParamSpec   *pspec,
                          gchar      ***loaded_plugins)
{
  if (*loaded_plugins != NULL)
    g_strfreev (*loaded_plugins);

  *loaded_plugins = peas_engine_get_loaded_plugins (engine);
}

static void
test_engine_loaded_plugins (PeasEngine *engine)
{
  PeasPluginInfo *info;
  gint loaded = 0;
  gchar **load_plugins;
  gchar **loaded_plugins = NULL;

  testing_util_push_log_hook ("Could not find plugin 'does-not-exist'*");

  g_signal_connect_after (engine,
                          "load-plugin",
                          G_CALLBACK (load_plugin_cb),
                          (gpointer) &loaded);
  g_signal_connect_after (engine,
                          "unload-plugin",
                          G_CALLBACK (unload_plugin_cb),
                          (gpointer) &loaded);
  g_signal_connect (engine,
                    "notify::loaded-plugins",
                    G_CALLBACK (notify_loaded_plugins_cb),
                    (gpointer) &loaded_plugins);


  /* Need to cause the plugin to be unavailable */
  info = peas_engine_get_plugin_info (engine, "unavailable");
  g_assert (!peas_engine_load_plugin (engine, info));

  info = peas_engine_get_plugin_info (engine, "loadable");

  g_object_notify (G_OBJECT (engine), "loaded-plugins");


  /* Unload all plugins */
  peas_engine_set_loaded_plugins (engine, NULL);
  g_assert_cmpint (loaded, ==, 0);
  g_assert (loaded_plugins != NULL);
  g_assert (loaded_plugins[0] == NULL);

  load_plugins = g_new0 (gchar *, 1);
  peas_engine_set_loaded_plugins (engine, (const gchar **) load_plugins);
  g_strfreev (load_plugins);

  g_assert_cmpint (loaded, ==, 0);
  g_assert (loaded_plugins != NULL);
  g_assert (loaded_plugins[0] == NULL);


  /* Load a plugin */
  load_plugins = g_new0 (gchar *, 2);
  load_plugins[0] = g_strdup ("loadable");
  peas_engine_set_loaded_plugins (engine, (const gchar **) load_plugins);
  g_strfreev (load_plugins);

  g_assert_cmpint (loaded, ==, 1);
  g_assert (loaded_plugins != NULL);
  g_assert_cmpstr (loaded_plugins[0], ==, "loadable");
  g_assert (loaded_plugins[1] == NULL);


  /* Try to load an unavailable plugin */
  load_plugins = g_new0 (gchar *, 2);
  load_plugins[0] = g_strdup ("unavailable");
  peas_engine_set_loaded_plugins (engine, (const gchar **) load_plugins);
  g_strfreev (load_plugins);

  g_assert_cmpint (loaded, ==, 0);
  g_assert (loaded_plugins != NULL);
  g_assert (loaded_plugins[0] == NULL);


  g_assert (peas_engine_load_plugin (engine, info));

  g_assert_cmpint (loaded, ==, 1);
  g_assert (loaded_plugins != NULL);
  g_assert_cmpstr (loaded_plugins[0], ==, "loadable");
  g_assert (loaded_plugins[1] == NULL);

  g_object_unref (engine);

  g_assert_cmpint (loaded, ==, 0);
  g_assert (loaded_plugins != NULL);
  g_assert_cmpstr (loaded_plugins[0], ==, "loadable");
  g_assert (loaded_plugins[1] == NULL);

  g_strfreev (loaded_plugins);
}

static void
test_engine_nonexistent_loader (PeasEngine *engine)
{
  GError *error = NULL;
  PeasPluginInfo *info;

  testing_util_push_log_hook ("Could not find loader 'does-not-exist'*");

  info = peas_engine_get_plugin_info (engine, "nonexistent-loader");
  peas_engine_enable_loader (engine, "does-not-exist");

  g_assert (!peas_engine_load_plugin (engine, info));
  g_assert (!peas_plugin_info_is_loaded (info));
  g_assert (!peas_plugin_info_is_available (info, &error));
  g_assert_error (error, PEAS_PLUGIN_INFO_ERROR,
                  PEAS_PLUGIN_INFO_ERROR_LOADER_NOT_FOUND);

  g_error_free (error);
}

static void
test_engine_disabled_loader (PeasEngine *engine)
{
  PeasPluginInfo *info;
  GError *error = NULL;

  testing_util_push_log_hook ("Could not find loader 'disabled'*");

  info = peas_engine_get_plugin_info (engine, "disabled-loader");

  g_assert (!peas_engine_load_plugin (engine, info));
  g_assert (!peas_plugin_info_is_loaded (info));
  g_assert (!peas_plugin_info_is_available (info, &error));
  g_assert_error (error, PEAS_PLUGIN_INFO_ERROR,
                  PEAS_PLUGIN_INFO_ERROR_LOADER_NOT_FOUND);

  g_error_free (error);
}

static void
test_engine_enable_loader_multiple_times (PeasEngine *engine)
{
  peas_engine_enable_loader (engine, "C");
}

static void
test_engine_nonexistent_search_path (PeasEngine *engine)
{
  /* We use /nowhere as it is also used in configure.ac */
  peas_engine_add_search_path (engine, "/nowhere", NULL);
}


static void
test_engine_shutdown (void)
{
  /* Should be able to shutdown multiple times */
  peas_engine_shutdown ();
  peas_engine_shutdown ();

  /* Cannot create an engine because libpeas has been shutdown */
  if (g_test_trap_fork (0, G_TEST_TRAP_SILENCE_STDOUT | G_TEST_TRAP_SILENCE_STDERR))
    {
      peas_engine_new ();
      exit (0);
    }
  g_test_trap_assert_failed ();
  g_test_trap_assert_stderr ("*libpeas cannot create a plugin "
                              "engine as it has been shutdown*");
}

int
main (int    argc,
      char **argv)
{
  g_test_init (&argc, &argv, NULL);

  g_type_init ();

#define TEST(path, ftest) \
  g_test_add ("/engine/" path, TestFixture, \
              (gpointer) test_engine_##ftest, \
              test_setup, test_runner, test_teardown)

#define TEST_FUNC(path, ftest) \
  g_test_add_func ("/engine/" path, test_engine_##ftest)

  TEST ("new", new);
  TEST ("dispose", dispose);
  TEST_FUNC ("get-default", get_default);

  TEST ("load-plugin", load_plugin);
  TEST ("load-plugin-with-dep", load_plugin_with_dep);
  TEST ("load-plugin-with-self-dep", load_plugin_with_self_dep);
  TEST ("load-plugin-with-nonexistent-dep", load_plugin_with_nonexistent_dep);

  TEST ("unload-plugin", unload_plugin);
  TEST ("unload-plugin-with-dep", unload_plugin_with_dep);
  TEST ("unload-plugin-with-self-dep", unload_plugin_with_self_dep);

  TEST ("unavailable-plugin", unavailable_plugin);
  TEST ("not-loadable-plugin", not_loadable_plugin);

  TEST ("loaded-plugins", loaded_plugins);

  TEST ("nonexistent-loader", nonexistent_loader);
  TEST ("disabled-loader", disabled_loader);
  TEST ("enable-loader-multiple-times", enable_loader_multiple_times);

  TEST ("nonexistent-search-path", nonexistent_search_path);

  /* MUST be last */
  TEST_FUNC ("shutdown", shutdown);

#undef TEST

  return testing_run_tests ();
}
