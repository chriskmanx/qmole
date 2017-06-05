/*
 * testing-extensin.c
 * This file is part of libpeas
 *
 * Copyright (C) 2011 - Garrett Regier
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
#include <stdio.h>
#include <string.h>

#include <glib.h>
#include <libpeas/peas.h>

#include "testing.h"
#include "testing-extension.h"

#include "introspection-callable.h"
#include "introspection-properties.h"
#include "introspection-unimplemented.h"

static gchar *extension_plugin;

void
testing_extension_set_plugin_ (const gchar *plugin)
{
  extension_plugin = (gchar *) plugin;
}

void
testing_extension_test_setup_ (TestingExtensionFixture_ *fixture,
                               gconstpointer             data)
{
  fixture->engine = testing_engine_new ();
}

void
testing_extension_test_teardown_ (TestingExtensionFixture_ *fixture,
                                  gconstpointer             data)
{
  testing_engine_free (fixture->engine);
}

void
testing_extension_test_runner_  (TestingExtensionFixture_ *fixture,
                                 gconstpointer             data)
{
  ((void (*) (PeasEngine *engine)) data) (fixture->engine);
}

void
testing_extension_garbage_collect_ (PeasEngine *engine)
{
  PeasPluginInfo *info;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  /* See that we can collect the garbage when no plugins are loaded */
  peas_engine_garbage_collect (engine);

  g_assert (peas_engine_load_plugin (engine, info));

  peas_engine_garbage_collect (engine);
}

void
testing_extension_provides_valid_ (PeasEngine *engine)
{
  PeasPluginInfo *info;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  g_assert (peas_engine_load_plugin (engine, info));

  g_assert (peas_engine_provides_extension (engine, info,
                                            INTROSPECTION_TYPE_CALLABLE));
}

void
testing_extension_provides_invalid_ (PeasEngine *engine)
{
  PeasPluginInfo *info;

  testing_util_push_log_hook ("*assertion `G_TYPE_IS_INTERFACE (*)' failed");

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  /* Not loaded */
  g_assert (!peas_engine_provides_extension (engine, info,
                                             INTROSPECTION_TYPE_CALLABLE));

  g_assert (peas_engine_load_plugin (engine, info));

  /* Invalid GType */
  peas_engine_provides_extension (engine, info, G_TYPE_INVALID);


  /* GObject but not a GInterface */
  peas_engine_provides_extension (engine, info, PEAS_TYPE_ENGINE);


  /* Does not implement this GType */
  g_assert (!peas_engine_provides_extension (engine, info,
                                             INTROSPECTION_TYPE_UNIMPLEMENTED));
}

void
testing_extension_create_valid_ (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_CALLABLE,
                                            NULL);

  g_assert (PEAS_IS_EXTENSION (extension));
  g_assert (INTROSPECTION_IS_CALLABLE (extension));

  g_object_unref (extension);
}

void
testing_extension_create_invalid_ (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;

  testing_util_push_log_hook ("*assertion `peas_plugin_info_is_loaded (*)' failed");
  testing_util_push_log_hook ("*assertion `G_TYPE_IS_INTERFACE (*)' failed");
  testing_util_push_log_hook ("*does not provide a 'IntrospectionUnimplemented' extension");
  testing_util_push_log_hook ("*type 'IntrospectionCallable' has no property named 'invalid-property'");

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  /* Not loaded */
  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_CALLABLE,
                                            NULL);
  g_assert (!PEAS_IS_EXTENSION (extension));

  g_assert (peas_engine_load_plugin (engine, info));

  /* Invalid GType */
  extension = peas_engine_create_extension (engine, info, G_TYPE_INVALID, NULL);
  g_assert (!PEAS_IS_EXTENSION (extension));


  /* GObject but not a GInterface */
  extension = peas_engine_create_extension (engine, info, PEAS_TYPE_ENGINE, NULL);
  g_assert (!PEAS_IS_EXTENSION (extension));


  /* Does not implement this GType */
  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_UNIMPLEMENTED,
                                            NULL);
  g_assert (!PEAS_IS_EXTENSION (extension));

  /* Interface does not have an 'invalid-property' property */
  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_CALLABLE,
                                            "invalid-property", "does-not-exist",
                                            NULL);
  g_assert (!PEAS_IS_EXTENSION (extension));
}

void
testing_extension_reload_ (PeasEngine *engine)
{
  gint i;
  PeasPluginInfo *info;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  for (i = 0; i < 3; ++i)
    {
      g_assert (peas_engine_load_plugin (engine, info));
      g_assert (peas_engine_unload_plugin (engine, info));
    }
}

void
testing_extension_call_no_args_ (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;
  IntrospectionCallable *callable;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_CALLABLE,
                                            NULL);

  callable = INTROSPECTION_CALLABLE (extension);

  g_assert (peas_extension_call (extension, "call_no_args"));
  introspection_callable_call_no_args (callable);

  g_object_unref (extension);
}

void
testing_extension_call_with_return_ (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;
  IntrospectionCallable *callable;
  const gchar *return_val = NULL;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_CALLABLE,
                                            NULL);

  callable = INTROSPECTION_CALLABLE (extension);

  g_assert (peas_extension_call (extension, "call_with_return", &return_val));
  g_assert_cmpstr (return_val, ==, "Hello, World!");

  return_val = NULL;

  return_val = introspection_callable_call_with_return (callable);
  g_assert_cmpstr (return_val, ==, "Hello, World!");

  g_object_unref (extension);
}

void
testing_extension_call_single_arg_ (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;
  IntrospectionCallable *callable;
  gboolean called = FALSE;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_CALLABLE,
                                            NULL);

  callable = INTROSPECTION_CALLABLE (extension);

  g_assert (peas_extension_call (extension, "call_single_arg", &called));
  g_assert (called);

  called = FALSE;

  introspection_callable_call_single_arg (callable, &called);
  g_assert (called);

  g_object_unref (extension);
}

void
testing_extension_call_multi_args_ (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;
  IntrospectionCallable *callable;
  gint in, out, inout;
  gint inout_saved;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_CALLABLE,
                                            NULL);

  callable = INTROSPECTION_CALLABLE (extension);

  in = g_random_int ();
  inout = g_random_int ();
  inout_saved = inout;

  introspection_callable_call_multi_args (callable, in, &out, &inout);
  
  g_assert_cmpint (inout_saved, ==, out);
  g_assert_cmpint (in, ==, inout);

  g_object_unref (extension);
}

void
testing_extension_properties_construct_only_ (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;
  gchar *construct_only;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_PROPERTIES,
                                            "construct-only", "my-construct-only",
                                            NULL);

  g_object_get (extension, "construct-only", &construct_only, NULL);
  g_assert_cmpstr (construct_only, ==, "my-construct-only");
  g_free (construct_only);

  g_object_unref (extension);
}

void
testing_extension_properties_read_only_ (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;
  gchar *read_only;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_PROPERTIES,
                                            NULL);

  g_object_get (extension, "read-only", &read_only, NULL);
  g_assert_cmpstr (read_only, ==, "read-only");
  g_free (read_only);

  g_object_unref (extension);
}

void
testing_extension_properties_write_only_ (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_PROPERTIES,
                                            NULL);

  g_object_set (extension, "write-only", "my-write-only", NULL);

  g_object_unref (extension);
}

void
testing_extension_properties_readwrite_ (PeasEngine *engine)
{
  PeasPluginInfo *info;
  PeasExtension *extension;
  gchar *readwrite;

  info = peas_engine_get_plugin_info (engine, extension_plugin);

  g_assert (peas_engine_load_plugin (engine, info));

  extension = peas_engine_create_extension (engine, info,
                                            INTROSPECTION_TYPE_PROPERTIES,
                                            NULL);

  g_object_get (extension, "readwrite", &readwrite, NULL);
  g_assert_cmpstr (readwrite, ==, "readwrite");
  g_free (readwrite);

  g_object_set (extension, "readwrite", "my-readwrite", NULL);

  g_object_get (extension, "readwrite", &readwrite, NULL);
  g_assert_cmpstr (readwrite, ==, "my-readwrite");
  g_free (readwrite);

  g_object_unref (extension);
}
