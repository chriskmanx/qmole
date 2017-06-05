/*
 * peas-plugin-loader-gjs.c
 * This file is part of libpeas
 *
 * Copyright (C) 2011 - Garrett Regier, Steve Frécinaux
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

#include <gjs/gjs-module.h>
#include <gi/object.h>
#include <gi/repo.h>
#include <gi/value.h>

#include "peas-plugin-loader-gjs.h"
#include "peas-extension-gjs.h"

typedef struct {
  JSObject *extensions;
  GjsContext *context;
} GjsInfo;

G_DEFINE_TYPE (PeasPluginLoaderGjs, peas_plugin_loader_gjs, PEAS_TYPE_PLUGIN_LOADER);

static gchar *
get_script_filename_for_plugin_info (PeasPluginInfo *info)
{
  gchar *basename;
  gchar *filename;

  basename = g_strconcat (peas_plugin_info_get_module_name (info), ".js", NULL);
  filename = g_build_filename (peas_plugin_info_get_module_dir (info), basename, NULL);

  g_free (basename);

  return filename;
}

static gboolean
peas_plugin_loader_gjs_load (PeasPluginLoader *loader,
                             PeasPluginInfo   *info)
{
  PeasPluginLoaderGjs *gloader = PEAS_PLUGIN_LOADER_GJS (loader);
  gchar **search_paths;
  const gchar *version;
  GjsContext *context;
  gchar *filename;
  GjsInfo *ginfo;
  GError *error = NULL;
  JSContext *js_context;
  JSObject *global;
  jsval extensions;

  filename = get_script_filename_for_plugin_info (info);

  g_debug ("GJS script filename is '%s'", filename);

  search_paths = g_new (gchar *, 2);
  search_paths[0] = g_strdup (peas_plugin_info_get_module_dir (info));
  search_paths[1] = NULL;

  version = gjs_context_scan_file_for_js_version (filename);

  context = g_object_new (GJS_TYPE_CONTEXT,
                          "search-path", search_paths,
                          "js-version", version,
                          NULL);

  gjs_context_eval_file (context, filename, NULL, &error);

  g_strfreev (search_paths);
  g_free (filename);

  if (error != NULL)
    {
      g_warning ("Error: %s", error->message);
      g_error_free (error);
      g_object_unref (context);
      return FALSE;
    }

  js_context = gjs_context_get_native_context (context);
  global = JS_GetGlobalObject (js_context);

  if (!JS_GetProperty (js_context, global, "extensions", &extensions))
    {
      g_warning ("Error: could not find extensions");
      return FALSE;
    }

  if (!JSVAL_IS_OBJECT (extensions) || JSVAL_IS_NULL (extensions))
    {
      g_warning ("Error: 'extensions' is of invalid type '%s'",
                 gjs_get_type_name (extensions));
      return FALSE;
    }

  ginfo = g_slice_new (GjsInfo);
  ginfo->context = g_object_ref (context);
  ginfo->extensions = JSVAL_TO_OBJECT (extensions);
  JS_AddObjectRoot (js_context, &ginfo->extensions);

  g_hash_table_insert (gloader->loaded_plugins, info, ginfo);

  g_object_unref (context);

  return TRUE;
}

static gboolean
peas_plugin_loader_gjs_provides_extension  (PeasPluginLoader *loader,
                                            PeasPluginInfo   *info,
                                            GType             exten_type)
{
  PeasPluginLoaderGjs *gloader = PEAS_PLUGIN_LOADER_GJS (loader);
  GjsInfo *ginfo;
  JSContext *js_context;
  jsval extension;

  ginfo = g_hash_table_lookup (gloader->loaded_plugins, info);

  js_context = gjs_context_get_native_context (ginfo->context);

  return JS_GetProperty (js_context, ginfo->extensions,
                         g_type_name (exten_type), &extension) &&
         JSVAL_IS_OBJECT (extension) && !JSVAL_IS_NULL (extension);
}

static PeasExtension *
peas_plugin_loader_gjs_create_extension (PeasPluginLoader *loader,
                                         PeasPluginInfo   *info,
                                         GType             exten_type,
                                         guint             n_parameters,
                                         GParameter       *parameters)
{
  PeasPluginLoaderGjs *gloader = PEAS_PLUGIN_LOADER_GJS (loader);
  GjsInfo *ginfo;
  JSContext *js_context;
  jsval extension_ctor;
  JSObject *extension;
  guint i;
  jsval js_value;
  GValue gvalue = { 0 };

  ginfo = g_hash_table_lookup (gloader->loaded_plugins, info);

  js_context = gjs_context_get_native_context (ginfo->context);

  if (!JS_GetProperty (js_context, ginfo->extensions,
                       g_type_name (exten_type), &extension_ctor) ||
      JSVAL_IS_VOID (extension_ctor) || JSVAL_IS_NULL (extension_ctor))
    return NULL;

  if (!JSVAL_IS_OBJECT (extension_ctor))
    {
      g_warning ("Extension '%s' in plugin '%s' in not a valid constructor object",
                 g_type_name (exten_type),
                 peas_plugin_info_get_module_name (info));
      return NULL;
    }

  /* Instantiate the extension ctor object to a new specific object. */
  extension = JS_New (js_context, JSVAL_TO_OBJECT (extension_ctor), 0, NULL);

  if (!extension)
    {
      g_warning ("Extension '%s' in plugin '%s' is not a valid constructor object",
                 g_type_name (exten_type),
                 peas_plugin_info_get_module_name (info));
      return NULL;
    }

  /* Cannot use g_object_set_property()
   * because the property may be construct-only
   */
  for (i = 0; i < n_parameters; i++)
    {
      guint j;
      gchar *prop_name;

      prop_name = g_strdup (parameters[i].name);
      for (j = 0; prop_name[j] != '\0'; ++j)
        {
          if (prop_name[j] == '-')
            prop_name[j] = '_';
        }

      if (!gjs_value_from_g_value (js_context, &js_value, &parameters[i].value))
        {
          g_warning ("Error: failed to convert GValue to jsval for property '%s'", prop_name);
        }
      else if (!JS_SetProperty (js_context, extension, prop_name, &js_value))
        {
          g_warning ("Error: failed to set property '%s'", prop_name);
        }

      g_free (prop_name);
    }


  /* Set the plugin info as an attribute of the instance */
  g_value_init (&gvalue, PEAS_TYPE_PLUGIN_INFO);
  g_value_set_boxed (&gvalue, info);

  if (!gjs_value_from_g_value (js_context, &js_value, &gvalue))
    {
      g_warning ("Error: failed to convert PeasPluginInfo GValue to jsvalue");
    }
  else if (!JS_SetProperty (js_context, extension, "plugin_info", &js_value))
    {
      g_warning ("Error: failed to set property 'plugin_info'");
    }

  g_value_unset (&gvalue);

  return peas_extension_gjs_new (exten_type, js_context, extension);
}

static void
peas_plugin_loader_gjs_unload (PeasPluginLoader *loader,
                               PeasPluginInfo   *info)
{
  PeasPluginLoaderGjs *gloader = PEAS_PLUGIN_LOADER_GJS (loader);

  g_hash_table_remove (gloader->loaded_plugins, info);
}

static void
garbage_collect (PeasPluginInfo *info,
                 GjsInfo        *ginfo)
{
  JS_GC (gjs_context_get_native_context (ginfo->context));
}

static void
peas_plugin_loader_gjs_garbage_collect (PeasPluginLoader *loader)
{
  PeasPluginLoaderGjs *gloader = PEAS_PLUGIN_LOADER_GJS (loader);

  g_hash_table_foreach (gloader->loaded_plugins,
                        (GHFunc) garbage_collect,
                        NULL);
}

static void
peas_plugin_loader_gjs_finalize (GObject *object)
{
  PeasPluginLoaderGjs *gloader = PEAS_PLUGIN_LOADER_GJS (object);

  g_hash_table_destroy (gloader->loaded_plugins);

  G_OBJECT_CLASS (peas_plugin_loader_gjs_parent_class)->finalize (object);
}

static void
peas_plugin_loader_gjs_class_init (PeasPluginLoaderGjsClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  PeasPluginLoaderClass *loader_class = PEAS_PLUGIN_LOADER_CLASS (klass);

  gobject_class->finalize = peas_plugin_loader_gjs_finalize;

  loader_class->load = peas_plugin_loader_gjs_load;
  loader_class->provides_extension = peas_plugin_loader_gjs_provides_extension;
  loader_class->create_extension = peas_plugin_loader_gjs_create_extension;
  loader_class->unload = peas_plugin_loader_gjs_unload;
  loader_class->garbage_collect = peas_plugin_loader_gjs_garbage_collect;
}

static void
destroy_gjs_info (GjsInfo *ginfo)
{
  JSContext *js_context;

  js_context = gjs_context_get_native_context (ginfo->context);

  JS_RemoveObjectRoot (js_context, &ginfo->extensions);
  g_object_unref (ginfo->context);

  g_slice_free (GjsInfo, ginfo);
}

static void
peas_plugin_loader_gjs_init (PeasPluginLoaderGjs *gloader)
{
  gloader->loaded_plugins = g_hash_table_new_full (g_direct_hash,
                                                   g_direct_equal,
                                                   NULL,
                                                   (GDestroyNotify) destroy_gjs_info);
}

G_MODULE_EXPORT void
peas_register_types (PeasObjectModule *module)
{
  peas_object_module_register_extension_type (module,
                                              PEAS_TYPE_PLUGIN_LOADER,
                                              PEAS_TYPE_PLUGIN_LOADER_GJS);
}
