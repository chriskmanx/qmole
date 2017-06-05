/*
 * extension-c.c
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

#include "libpeas/peas.h"

#include "testing/testing-extension.h"
#include "introspection/introspection-callable.h"

static void
test_extension_c_instance_refcount (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;

  info = peas_engine_get_plugin_info (engine, "extension-c");

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_CALLABLE,
                                            NULL);

  g_assert (PEAS_IS_EXTENSION (extension));

  /* The refcount of the returned object should be 1:
   *  - one ref for the PeasExtension
   */
  g_assert_cmpint (extension->ref_count, ==, 1);

  g_object_unref (extension);
}

static void
test_extension_c_plugin_info (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;

  info = peas_engine_get_plugin_info (engine, "loadable");

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            PEAS_TYPE_ACTIVATABLE,
                                            NULL);

  g_assert (PEAS_IS_EXTENSION_BASE (extension));
  g_assert (peas_extension_base_get_plugin_info (PEAS_EXTENSION_BASE (extension)) == info);

  g_object_unref (extension);
}

static void
test_extension_c_nonexistent (PeasEngine *engine)
{
  PeasPluginInfo *info;

  testing_util_push_log_hook ("*extension-c-nonexistent*No such file*");
  testing_util_push_log_hook ("Could not load*'extension-c-nonexistent'");
  testing_util_push_log_hook ("Error loading plugin 'extension-c-nonexistent'");

  info = peas_engine_get_plugin_info (engine, "extension-c-nonexistent");

  g_assert (!peas_engine_load_plugin (engine, info));
}

int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);
  g_type_init ();

  /* Only test the basics */
  EXTENSION_TESTS_INIT (c);

  /* We still need to add the callable tests
   * because of peas_extension_call()
   */
  EXTENSION_TESTS_CALLABLE (c);

  EXTENSION_TEST (c, "instance-refcount", instance_refcount);
  EXTENSION_TEST (c, "plugin-info", plugin_info);
  EXTENSION_TEST (c, "nonexistent", nonexistent);

  return testing_run_tests ();
}
