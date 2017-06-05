/*
 * peas-plugin-loader-c.c
 * This file is part of libpeas
 *
 * Copyright (C) 2008 - Jesse van den Kieboom
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

#include <string.h>
#include <gmodule.h>

#include "peas-plugin-loader-c.h"
#include <libpeas/peas-object-module.h>
#include <libpeas/peas-extension-base.h>

struct _PeasPluginLoaderCPrivate {
  GHashTable *loaded_plugins;
};

G_DEFINE_TYPE (PeasPluginLoaderC, peas_plugin_loader_c, PEAS_TYPE_PLUGIN_LOADER);

G_MODULE_EXPORT void
peas_register_types (PeasObjectModule *module)
{
  peas_object_module_register_extension_type (module,
                                              PEAS_TYPE_PLUGIN_LOADER,
                                              PEAS_TYPE_PLUGIN_LOADER_C);
}

static gboolean
peas_plugin_loader_c_load (PeasPluginLoader *loader,
                           PeasPluginInfo   *info)
{
  PeasPluginLoaderC *cloader = PEAS_PLUGIN_LOADER_C (loader);
  PeasObjectModule *module;
  const gchar *module_name;

  module_name = peas_plugin_info_get_module_name (info);
  module = (PeasObjectModule *) g_hash_table_lookup (cloader->priv->loaded_plugins,
                                                     module_name);

  if (module == NULL)
    {
      /* Force all C modules to be resident in case they
       * use libraries that do not deal well with reloading
       */
      module = peas_object_module_new (module_name,
                                       peas_plugin_info_get_module_dir (info),
                                       TRUE);

      g_hash_table_insert (cloader->priv->loaded_plugins,
                           g_strdup (module_name), module);
    }

  if (!g_type_module_use (G_TYPE_MODULE (module)))
    {
      g_warning ("Could not load plugin module: '%s'", module_name);
      g_object_unref (module);
      g_hash_table_remove (cloader->priv->loaded_plugins, module_name);
      return FALSE;
    }

  return TRUE;
}

static gboolean
peas_plugin_loader_c_provides_extension  (PeasPluginLoader *loader,
                                          PeasPluginInfo   *info,
                                          GType             exten_type)
{
  PeasPluginLoaderC *cloader = PEAS_PLUGIN_LOADER_C (loader);
  PeasObjectModule *module;
  const gchar *module_name;

  module_name = peas_plugin_info_get_module_name (info);
  module = (PeasObjectModule *) g_hash_table_lookup (cloader->priv->loaded_plugins,
                                                     module_name);

  return peas_object_module_provides_object (module, exten_type);
}

static PeasExtension *
peas_plugin_loader_c_create_extension (PeasPluginLoader *loader,
                                       PeasPluginInfo   *info,
                                       GType             exten_type,
                                       guint             n_parameters,
                                       GParameter       *parameters)
{
  PeasPluginLoaderC *cloader = PEAS_PLUGIN_LOADER_C (loader);
  PeasObjectModule *module;
  GParameter *exten_parameters;
  gpointer instance;
  const gchar *module_name;

  module_name = peas_plugin_info_get_module_name (info);
  module = (PeasObjectModule *) g_hash_table_lookup (cloader->priv->loaded_plugins,
                                                     module_name);

  /* We want to add a "plugin-info" property so we can pass it to
   * the extension if it inherits from PeasExtensionBase. No need to
   * actually "duplicate" the GValues, a memcpy is sufficient as the
   * source GValues are longer lived than our local copy.
   */
  exten_parameters = g_new (GParameter, n_parameters + 1);
  memcpy (exten_parameters, parameters, sizeof (GParameter) * n_parameters);

  /* Initialize our additional property.
   * If the instance does not have a plugin-info property
   * then PeasObjectModule will remove the property.
   */
  exten_parameters[n_parameters].name = g_intern_static_string ("plugin-info");
  memset (&exten_parameters[n_parameters].value, 0, sizeof (GValue));
  g_value_init (&exten_parameters[n_parameters].value, PEAS_TYPE_PLUGIN_INFO);
  g_value_set_boxed (&exten_parameters[n_parameters].value, info);

  instance = peas_object_module_create_object (module,
                                               exten_type,
                                               n_parameters + 1,
                                               exten_parameters);

  g_value_unset (&exten_parameters[n_parameters].value);
  g_free (exten_parameters);

  if (instance == NULL)
    return NULL;

  g_return_val_if_fail (G_IS_OBJECT (instance), NULL);
  g_return_val_if_fail (G_TYPE_CHECK_INSTANCE_TYPE (instance, exten_type), NULL);

  /* As we do not instantiate a PeasExtensionWrapper, we have to remember
   * somehow which interface we are instantiating, to make it possible to use
   * the deprecated peas_extension_call() method.
   */
  g_object_set_data (instance, "peas-extension-type", GUINT_TO_POINTER (exten_type));

  return instance;
}

static void
peas_plugin_loader_c_unload (PeasPluginLoader *loader,
                             PeasPluginInfo   *info)
{
  PeasPluginLoaderC *cloader = PEAS_PLUGIN_LOADER_C (loader);
  PeasObjectModule *module;
  const gchar *module_name;

  module_name = peas_plugin_info_get_module_name (info);
  module = (PeasObjectModule *) g_hash_table_lookup (cloader->priv->loaded_plugins,
                                                     module_name);

  g_type_module_unuse (G_TYPE_MODULE (module));
}

static void
peas_plugin_loader_c_init (PeasPluginLoaderC *self)
{
  self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self,
                                            PEAS_TYPE_PLUGIN_LOADER_C,
                                            PeasPluginLoaderCPrivate);

  /* loaded_plugins maps PeasPluginInfo:module-name to a PeasObjectModule */
  self->priv->loaded_plugins = g_hash_table_new_full (g_str_hash,
                                                      g_str_equal,
                                                      g_free,
                                                      NULL);
}

static void
peas_plugin_loader_c_finalize (GObject *object)
{
  PeasPluginLoaderC *cloader = PEAS_PLUGIN_LOADER_C (object);

  g_hash_table_destroy (cloader->priv->loaded_plugins);

  G_OBJECT_CLASS (peas_plugin_loader_c_parent_class)->finalize (object);
}

static void
peas_plugin_loader_c_class_init (PeasPluginLoaderCClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  PeasPluginLoaderClass *loader_class = PEAS_PLUGIN_LOADER_CLASS (klass);

  object_class->finalize = peas_plugin_loader_c_finalize;

  loader_class->load = peas_plugin_loader_c_load;
  loader_class->unload = peas_plugin_loader_c_unload;
  loader_class->provides_extension = peas_plugin_loader_c_provides_extension;
  loader_class->create_extension = peas_plugin_loader_c_create_extension;

  g_type_class_add_private (object_class, sizeof (PeasPluginLoaderCPrivate));
}
