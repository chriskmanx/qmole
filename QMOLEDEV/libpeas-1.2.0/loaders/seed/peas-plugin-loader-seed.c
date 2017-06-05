/*
 * peas-plugin-loader-seed.c
 * This file is part of libpeas
 *
 * Copyright (C) 2009 - Steve Frécinaux
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

#include <seed.h>
#include <JavaScriptCore/JavaScript.h>

#include "peas-plugin-loader-seed.h"
#include "peas-extension-seed.h"

G_DEFINE_TYPE (PeasPluginLoaderSeed, peas_plugin_loader_seed, PEAS_TYPE_PLUGIN_LOADER);

typedef struct {
  SeedContext context;
  SeedObject extensions;
} SeedInfo;

static SeedEngine *seed = NULL;

static gchar *
get_script_for_plugin_info (PeasPluginInfo *info)
{
  gchar *basename;
  gchar *filename;
  gchar *script = NULL;
  GError *error = NULL;

  basename = g_strconcat (peas_plugin_info_get_module_name (info), ".js", NULL);
  filename = g_build_filename (peas_plugin_info_get_module_dir (info), basename, NULL);

  g_debug ("Seed script filename is '%s'", filename);

  g_file_get_contents (filename, &script, NULL, &error);

  g_free (basename);
  g_free (filename);

  if (error != NULL)
    {
      g_warning ("Error: %s", error->message);
      g_error_free (error);
    }

  return script;
}

static gboolean
peas_plugin_loader_seed_load (PeasPluginLoader *loader,
                              PeasPluginInfo   *info)
{
  PeasPluginLoaderSeed *sloader = PEAS_PLUGIN_LOADER_SEED (loader);
  SeedContext context;
  gchar *script;
  SeedException exc = NULL;
  SeedObject global, extensions;
  SeedInfo *sinfo;

  script = get_script_for_plugin_info (info);

  if (script == NULL)
    return FALSE;

  context = seed_context_create (seed->group, NULL);

  seed_prepare_global_context (context);
  seed_simple_evaluate (context, script, &exc);

  g_free (script);

  if (exc)
    {
      gchar *exc_string = seed_exception_to_string (context, exc);
      g_warning ("Seed Exception: %s", exc_string);
      g_free (exc_string);
      seed_context_unref (context);
      return FALSE;
    }

  global = seed_context_get_global_object (context);
  extensions = seed_object_get_property (context, global, "extensions");

  if (seed_value_is_object (context, extensions))
    {
      sinfo = (SeedInfo *) g_slice_new (SeedInfo);
      sinfo->context = context;
      sinfo->extensions = extensions;
      seed_value_protect (context, extensions);

      g_hash_table_insert (sloader->loaded_plugins, info, sinfo);
      return TRUE;
    }
  else
    {
      g_warning ("extensions is not an object in plugin '%s'",
                 peas_plugin_info_get_module_name (info));
      seed_context_unref (context);
      return FALSE;
    }
}

static gboolean
peas_plugin_loader_seed_provides_extension  (PeasPluginLoader *loader,
                                             PeasPluginInfo   *info,
                                             GType             exten_type)
{
  PeasPluginLoaderSeed *sloader = PEAS_PLUGIN_LOADER_SEED (loader);
  SeedInfo *sinfo;
  SeedValue extension;

  sinfo = (SeedInfo *) g_hash_table_lookup (sloader->loaded_plugins, info);

  extension = seed_object_get_property (sinfo->context,
                                        sinfo->extensions,
                                        g_type_name (exten_type));
  return extension && seed_value_is_object (sinfo->context, extension);
}

static PeasExtension *
peas_plugin_loader_seed_create_extension (PeasPluginLoader *loader,
                                          PeasPluginInfo   *info,
                                          GType             exten_type,
                                          guint             n_parameters,
                                          GParameter       *parameters)
{
  PeasPluginLoaderSeed *sloader = PEAS_PLUGIN_LOADER_SEED (loader);
  SeedInfo *sinfo;
  SeedValue extension_ctor, extension;
  guint i, j;
  SeedValue value;
  GValue gvalue = { 0 };

  sinfo = (SeedInfo *) g_hash_table_lookup (sloader->loaded_plugins, info);

  /* FIXME: instantiate new object and pass the parameters */
  extension_ctor = seed_object_get_property (sinfo->context,
                                             sinfo->extensions,
                                             g_type_name (exten_type));
  if (!extension_ctor ||
      seed_value_is_undefined (sinfo->context, extension_ctor) ||
      seed_value_is_null (sinfo->context, extension_ctor))
    return NULL;

  if (!seed_value_is_object (sinfo->context, extension_ctor))
    {
      g_warning ("Extension '%s' in plugin '%s' is not a Seed object",
                 g_type_name (exten_type), peas_plugin_info_get_module_name (info));
      return NULL;
    }

  /* Instantiate the ctor object into a new specific object. */
  extension = JSObjectCallAsConstructor (sinfo->context, extension_ctor, 0, NULL, NULL);

  if (extension == NULL)
    {
#ifndef PEAS_DISABLE_DEPRECATED_FEATURES
      gchar **property_names;

      g_warning ("DEPRECATION WARNING: Extension '%s' in plugin '%s' is not a valid "
                 "constructor function. Support for extension initialization by array "
                 "copy will be dropped soon.",
                 g_type_name (exten_type), peas_plugin_info_get_module_name (info));

      extension = seed_make_object (sinfo->context, NULL, NULL);
      property_names = seed_object_copy_property_names (sinfo->context,
                                                        extension_ctor);
      for (i = 0; property_names[i] != NULL; i++)
        {
          SeedValue value;
          value = seed_object_get_property (sinfo->context,
                                            extension_ctor,
                                            property_names[i]);
          seed_object_set_property (sinfo->context,
                                    extension,
                                    property_names[i],
                                    value);
        }

        g_strfreev (property_names);
#else
      g_warning ("Extension '%s' in plugin '%s' is not a valid constructor",
                 g_type_name (exten_type), peas_plugin_info_get_module_name (info));
      return NULL;
#endif
    }

  /* Set the properties as well, cannot use
   * g_object_set_property() because it may be construct-only
   */
  for (i = 0; i < n_parameters; i++)
    {
      gchar *key;

      /* We want to normalize the property names to have a '_' instead of the
       * conventional '-', to make them accessible through this.property_name */
      key = g_strdup (parameters[i].name);
      for (j = 0; key[j] != '\0'; j++)
        {
          if (key[j] == '-')
            key[j] = '_';
        }

      value = seed_value_from_gvalue (sinfo->context,
                                      &parameters[i].value,
                                      NULL);
      seed_object_set_property (sinfo->context,
                                extension,
                                key,
                                value);

      g_free (key);
    }

  /* Set the plugin info as an attribute of the instance */
  g_value_init (&gvalue, PEAS_TYPE_PLUGIN_INFO);
  g_value_set_boxed (&gvalue, info);

  value = seed_value_from_gvalue (sinfo->context, &gvalue, NULL);
  seed_object_set_property (sinfo->context, extension, "plugin_info", value);

  g_value_unset (&gvalue);

  return peas_extension_seed_new (exten_type, sinfo->context, extension);
}

static void
peas_plugin_loader_seed_unload (PeasPluginLoader *loader,
                                PeasPluginInfo   *info)
{
  PeasPluginLoaderSeed *sloader = PEAS_PLUGIN_LOADER_SEED (loader);

  g_hash_table_remove (sloader->loaded_plugins, info);
}

static void
peas_plugin_loader_seed_garbage_collect (PeasPluginLoader *loader)
{
  seed_context_collect (seed->context);
}

static void
destroy_seed_info (SeedInfo *info)
{
  seed_value_unprotect (info->context, info->extensions);
  seed_context_unref (info->context);

  g_slice_free (SeedInfo, info);
}

static void
peas_plugin_loader_seed_init (PeasPluginLoaderSeed *sloader)
{
  /* This is somewhat buggy as the seed engine cannot be reinitialized
   * and is shared among instances (esp wrt module paths), but apparently there
   * is no way to avoid having it shared... */
  if (!seed)
    {
      seed = seed_init (NULL, NULL);

      /* Reverse the log handle Seed uses.
       * The one place that Seed uses this shouldn't make a
       * difference.
       */
      g_log_set_handler ("GLib-GObject", G_LOG_LEVEL_WARNING,
                         g_log_default_handler, 0);
    }

  sloader->loaded_plugins = g_hash_table_new_full (g_direct_hash, g_direct_equal,
                                                   NULL,
                                                   (GDestroyNotify) destroy_seed_info);
}

static void
peas_plugin_loader_seed_finalize (GObject *object)
{
  PeasPluginLoaderSeed *sloader = PEAS_PLUGIN_LOADER_SEED (object);

  g_hash_table_destroy (sloader->loaded_plugins);

  G_OBJECT_CLASS (peas_plugin_loader_seed_parent_class)->finalize (object);
}

static void
peas_plugin_loader_seed_class_init (PeasPluginLoaderSeedClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  PeasPluginLoaderClass *loader_class = PEAS_PLUGIN_LOADER_CLASS (klass);

  object_class->finalize = peas_plugin_loader_seed_finalize;

  loader_class->load = peas_plugin_loader_seed_load;
  loader_class->provides_extension = peas_plugin_loader_seed_provides_extension;
  loader_class->create_extension = peas_plugin_loader_seed_create_extension;
  loader_class->unload = peas_plugin_loader_seed_unload;
  loader_class->garbage_collect = peas_plugin_loader_seed_garbage_collect;
}

G_MODULE_EXPORT void
peas_register_types (PeasObjectModule *module)
{
  peas_object_module_register_extension_type (module,
                                              PEAS_TYPE_PLUGIN_LOADER,
                                              PEAS_TYPE_PLUGIN_LOADER_SEED);
}
