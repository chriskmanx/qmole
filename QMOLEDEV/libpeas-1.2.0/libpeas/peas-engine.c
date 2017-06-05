/*
 * peas-engine.c
 * This file is part of libpeas
 *
 * Copyright (C) 2002-2005 Paolo Maggi
 * Copyright (C) 2009 Steve Frécinaux
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

#include "peas-i18n.h"
#include "peas-engine.h"
#include "peas-engine-priv.h"
#include "peas-plugin-info-priv.h"
#include "peas-plugin-loader.h"
#include "peas-object-module.h"
#include "peas-extension.h"
#include "peas-dirs.h"
#include "peas-debug.h"
#include "peas-helpers.h"

/**
 * SECTION:peas-engine
 * @short_description: Engine at the heart of the Peas plugin system.
 * @see_also: #PeasPluginInfo
 *
 * The #PeasEngine is the object which manages the plugins.
 *
 * Its role is twofold:
 * <itemizedlist>
 *   <listitem>
 *     <para>it will fetch all the information about the available plugins
 *     from all the registered plugin directories;</para>
 *   </listitem>
 *   <listitem>
 *     <para>it will provide you an API to load, control and unload your
 *     plugins and their extensions from within your application.</para>
 *   </listitem>
 * </itemizedlist>
 **/
G_DEFINE_TYPE (PeasEngine, peas_engine, G_TYPE_OBJECT);

static PeasEngine *default_engine = NULL;
static gboolean shutdown = FALSE;
static GHashTable *loaders = NULL;

/* Signals */
enum {
  LOAD_PLUGIN,
  UNLOAD_PLUGIN,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

/* Properties */
enum {
  PROP_0,
  PROP_PLUGIN_LIST,
  PROP_LOADED_PLUGINS
};

typedef struct _LoaderInfo LoaderInfo;

struct _LoaderInfo {
  PeasPluginLoader *loader;
  PeasObjectModule *module;
};

typedef struct _SearchPath {
  gchar *module_dir;
  gchar *data_dir;
} SearchPath;

struct _PeasEnginePrivate {
  GList *search_paths;

  GList *plugin_list;

  guint in_dispose : 1;
};

static void peas_engine_load_plugin_real   (PeasEngine     *engine,
                                            PeasPluginInfo *info);
static void peas_engine_unload_plugin_real (PeasEngine     *engine,
                                            PeasPluginInfo *info);

static void
load_plugin_info (PeasEngine  *engine,
                  const gchar *filename,
                  const gchar *module_dir,
                  const gchar *data_dir)
{
  PeasPluginInfo *info;
  const gchar *module_name;

  info = _peas_plugin_info_new (filename,
                                module_dir,
                                data_dir);

  if (info == NULL)
    {
      g_warning ("Error loading '%s'", filename);
      return;
    }

  /* If a plugin with this name has already been loaded
   * drop this one (user plugins override system plugins) */
  module_name = peas_plugin_info_get_module_name (info);
  if (peas_engine_get_plugin_info (engine, module_name) != NULL)
    _peas_plugin_info_unref (info);
  else
    {
      engine->priv->plugin_list = g_list_prepend (engine->priv->plugin_list,
                                                  info);

      g_object_notify (G_OBJECT (engine), "plugin-list");
    }
}

static void
load_dir_real (PeasEngine  *engine,
               const gchar *module_dir,
               const gchar *data_dir,
               guint        recursions)
{
  GError *error = NULL;
  GDir *d;
  const gchar *dirent;

  g_debug ("Loading %s/*.plugin...", module_dir);

  d = g_dir_open (module_dir, 0, &error);

  if (!d)
    {
      g_debug ("%s", error->message);
      g_error_free (error);
      return;
    }

  while ((dirent = g_dir_read_name (d)))
    {
      gchar *filename = g_build_filename (module_dir, dirent, NULL);

      if (g_file_test (filename, G_FILE_TEST_IS_DIR))
        {
          if (recursions > 0)
            load_dir_real (engine, filename, data_dir, recursions - 1);
        }
      else if (g_str_has_suffix (dirent, ".plugin"))
        {
          load_plugin_info (engine, filename, module_dir, data_dir);
        }

      g_free (filename);
    }

  g_dir_close (d);
}

/**
 * peas_engine_rescan_plugins:
 * @engine: A #PeasEngine.
 *
 * Rescan all the registered directories to find new or updated plugins.
 *
 * Calling this function will make the newly installed plugin infos
 * be loaded by the engine, so the new plugins can be used without
 * restarting the application.
 */
void
peas_engine_rescan_plugins (PeasEngine *engine)
{
  GList *item;

  g_return_if_fail (PEAS_IS_ENGINE (engine));

  if (engine->priv->search_paths == NULL)
    {
      g_debug ("No search paths where provided");
      return;
    }

  g_object_freeze_notify (G_OBJECT (engine));

  /* Go and read everything from the provided search paths */
  for (item = engine->priv->search_paths; item != NULL; item = item->next)
    {
      SearchPath *sp = (SearchPath *) item->data;
      load_dir_real (engine, sp->module_dir, sp->data_dir, 1);
    }

  g_object_thaw_notify (G_OBJECT (engine));
}

/**
 * peas_engine_add_search_path:
 * @engine: A #PeasEngine.
 * @module_dir: the plugin module directory.
 * @data_dir: (allow-none): the plugin data directory.
 *
 * This function appends a search path to the list of paths where to
 * look for plugins.
 *
 * A so-called "search path" actually consists of both a
 * module directory (where the shared libraries or language modules
 * lie) and a data directory (where the plugin data is).
 *
 * The plugin will be able to use a correct data dir depending on
 * where it is installed, hence allowing to keep the plugin agnostic
 * when it comes to installation location: the same plugin can be
 * installed either in the system path or in the user's home directory,
 * without taking other special care than using
 * peas_plugin_info_get_data_dir() when looking for its data files.
 *
 * If @data_dir is %NULL, then it is set to the same value as
 * @module_dir.
 */
void
peas_engine_add_search_path (PeasEngine *engine,
                             const gchar *module_dir,
                             const gchar *data_dir)
{
  SearchPath *sp;

  g_return_if_fail (PEAS_IS_ENGINE (engine));
  g_return_if_fail (module_dir != NULL);

  sp = g_slice_new (SearchPath);
  sp->module_dir = g_strdup (module_dir);
  sp->data_dir = g_strdup (data_dir ? data_dir : module_dir);

  /* Appending to a list is bad, but is easier to handle wrt refreshing
   * the plugin list. */
  engine->priv->search_paths = g_list_append (engine->priv->search_paths, sp);

  g_object_freeze_notify (G_OBJECT (engine));
  load_dir_real (engine, sp->module_dir, sp->data_dir, 1);
  g_object_thaw_notify (G_OBJECT (engine));
}

static guint
hash_lowercase (gconstpointer data)
{
  gchar *lowercase;
  guint ret;

  lowercase = g_ascii_strdown ((const gchar *) data, -1);
  ret = g_str_hash (lowercase);
  g_free (lowercase);

  return ret;
}

static gboolean
equal_lowercase (const gchar *a,
                 const gchar *b)
{
  return g_ascii_strcasecmp (a, b) == 0;
}

static void
loader_destroy (LoaderInfo *info)
{
  if (!info)
    return;

  if (info->loader)
    g_object_unref (info->loader);

  g_free (info);
}

static void
peas_engine_init (PeasEngine *engine)
{
  engine->priv = G_TYPE_INSTANCE_GET_PRIVATE (engine,
                                              PEAS_TYPE_ENGINE,
                                              PeasEnginePrivate);

  engine->priv->in_dispose = FALSE;
}

static void
loader_garbage_collect (const gchar *id,
                        LoaderInfo  *info)
{
  if (info != NULL && info->loader != NULL)
    peas_plugin_loader_garbage_collect (info->loader);
}

/**
 * peas_engine_garbage_collect:
 * @engine: A #PeasEngine.
 *
 * This function triggers garbage collection on all the loaders currently
 * owned by the #PeasEngine.  This can be used to force the loaders to destroy
 * managed objects that still hold references to objects that are about to
 * disappear.
 */
void
peas_engine_garbage_collect (PeasEngine *engine)
{
  g_return_if_fail (PEAS_IS_ENGINE (engine));

  g_hash_table_foreach (loaders,
                        (GHFunc) loader_garbage_collect,
                        NULL);
}

static GObject *
peas_engine_constructor (GType                  type,
                         guint                  n_construct_params,
                         GObjectConstructParam *construct_params)
{
  GObject *object;

  /* We don't support calling PeasEngine API without module support */
  if (!g_module_supported ())
    {
      g_error ("libpeas is not able to create the "
               "plugins engine as modules are not supported.");
    }

  if (shutdown)
    {
      g_error ("libpeas cannot create a plugin engine "
               "as it has been shutdown.");
    }

  object = G_OBJECT_CLASS (peas_engine_parent_class)->constructor (type,
                                                                   n_construct_params,
                                                                   construct_params);

  if (default_engine == NULL)
    {
      default_engine = PEAS_ENGINE (object);
      g_object_add_weak_pointer (object, (gpointer *) &default_engine);
    }

  return object;
}

static void
peas_engine_set_property (GObject      *object,
                          guint         prop_id,
                          const GValue *value,
                          GParamSpec   *pspec)
{
  PeasEngine *engine = PEAS_ENGINE (object);

  switch (prop_id)
    {
    case PROP_LOADED_PLUGINS:
      peas_engine_set_loaded_plugins (engine,
                                      (const gchar **) g_value_get_boxed (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
peas_engine_get_property (GObject    *object,
                          guint       prop_id,
                          GValue     *value,
                          GParamSpec *pspec)
{
  PeasEngine *engine = PEAS_ENGINE (object);

  switch (prop_id)
    {
    case PROP_PLUGIN_LIST:
      g_value_set_pointer (value,
                           (gpointer) peas_engine_get_plugin_list (engine));
      break;
    case PROP_LOADED_PLUGINS:
      g_value_take_boxed (value,
                          (gconstpointer) peas_engine_get_loaded_plugins (engine));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
peas_engine_dispose (GObject *object)
{
  PeasEngine *engine = PEAS_ENGINE (object);
  GList *item;

  engine->priv->in_dispose = TRUE;

  /* First unload all the plugins */
  for (item = engine->priv->plugin_list; item; item = item->next)
    {
      PeasPluginInfo *info = PEAS_PLUGIN_INFO (item->data);

      if (peas_plugin_info_is_loaded (info))
        peas_engine_unload_plugin (engine, info);
    }

  G_OBJECT_CLASS (peas_engine_parent_class)->dispose (object);
}

static void
peas_engine_finalize (GObject *object)
{
  PeasEngine *engine = PEAS_ENGINE (object);
  GList *item;

  /* free the infos */
  for (item = engine->priv->plugin_list; item; item = item->next)
    _peas_plugin_info_unref (PEAS_PLUGIN_INFO (item->data));

  /* free the search path list */
  for (item = engine->priv->search_paths; item; item = item->next)
    {
      SearchPath *sp = (SearchPath *) item->data;

      g_free (sp->module_dir);
      g_free (sp->data_dir);
      g_slice_free (SearchPath, sp);
    }

  g_list_free (engine->priv->plugin_list);
  g_list_free (engine->priv->search_paths);

  G_OBJECT_CLASS (peas_engine_parent_class)->finalize (object);
}

static void
peas_engine_class_init (PeasEngineClass *klass)
{
  GType the_type = G_TYPE_FROM_CLASS (klass);
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->constructor = peas_engine_constructor;
  object_class->set_property = peas_engine_set_property;
  object_class->get_property = peas_engine_get_property;
  object_class->dispose = peas_engine_dispose;
  object_class->finalize = peas_engine_finalize;

  klass->load_plugin = peas_engine_load_plugin_real;
  klass->unload_plugin = peas_engine_unload_plugin_real;

  /**
   * PeasEngine:plugin-list:
   *
   * The list of found plugins.
   *
   * This will be modified when peas_engine_rescan_plugins() is called.
   *
   * Note that the list belongs to the engine and should not be modified
   * or freed.
   */
  g_object_class_install_property (object_class,
                                   PROP_PLUGIN_LIST,
                                   g_param_spec_pointer ("plugin-list",
                                                         "Plugin list",
                                                         "The list of found plugins",
                                                         G_PARAM_READABLE |
                                                         G_PARAM_STATIC_STRINGS));

  /**
   * PeasEngine:loaded-plugins:
   *
   * The list of loaded plugins.
   *
   * This will be modified when peas_engine_load_plugin() or
   * peas_engine_unload_plugin() is called.
   *
   * This can be used with GSettings to save the loaded plugins by binding
   * to this property after instantiating the engine by doing:
   * |[
   *   g_settings_bind (gsettings_object,
   *                    LOADED_PLUGINS_KEY,
   *                    engine,
   *                    "loaded-plugins",
   *                    G_SETTINGS_BIND_DEFAULT);
   * ]|
   *
   * Note: notify will not be called when the engine is being destroyed.
   */
  g_object_class_install_property (object_class,
                                   PROP_LOADED_PLUGINS,
                                   g_param_spec_boxed ("loaded-plugins",
                                                       "Loaded plugins",
                                                       "The list of loaded plugins",
                                                       G_TYPE_STRV,
                                                       G_PARAM_READWRITE |
                                                       G_PARAM_STATIC_STRINGS));

  /**
   * PeasEngine::load-plugin:
   * @engine: A #PeasEngine.
   * @info: A #PeasPluginInfo.
   *
   * The load-plugin signal is emitted when a plugin is being loaded.
   *
   * The plugin is being loaded in the default handler. Hence, if you want to
   * perform some action before the plugin is loaded, you should use
   * g_signal_connect(), but if you want to perform some action *after* the
   * plugin is loaded (the most common case), you should use
   * g_signal_connect_after().
   */
  signals[LOAD_PLUGIN] =
    g_signal_new ("load-plugin",
                  the_type,
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (PeasEngineClass, load_plugin),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__BOXED,
                  G_TYPE_NONE,
                  1,
                  PEAS_TYPE_PLUGIN_INFO |
                  G_SIGNAL_TYPE_STATIC_SCOPE);

  /**
   * PeasEngine::unload-plugin:
   * @engine: A #PeasEngine.
   * @info: A #PeasPluginInfo.
   *
   * The unload-plugin signal is emitted when a plugin is being unloaded.
   *
   * The plugin is being unloaded in the default handler. Hence, if you want
   * to perform some action before the plugin is unloaded (the most common
   * case), you should use g_signal_connect(), but if you want to perform some
   * action after the plugin is unloaded (the most common case), you should
   * use g_signal_connect_after().
   */
  signals[UNLOAD_PLUGIN] =
    g_signal_new ("unload-plugin",
                  the_type,
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (PeasEngineClass, unload_plugin),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__BOXED,
                  G_TYPE_NONE,
                  1, PEAS_TYPE_PLUGIN_INFO |
                  G_SIGNAL_TYPE_STATIC_SCOPE);

  g_type_class_add_private (klass, sizeof (PeasEnginePrivate));

  /* We are doing some global initialization here as there is currently no
   * global init function for libpeas. */
  peas_debug_init ();

  /* mapping from loadername -> loader object */
  loaders = g_hash_table_new_full (hash_lowercase,
                                   (GEqualFunc) equal_lowercase,
                                   (GDestroyNotify) g_free,
                                   (GDestroyNotify) loader_destroy);

  /* The C plugin loader is always enabled */
  g_hash_table_insert (loaders, g_strdup ("C"), g_new0 (LoaderInfo, 1));
}

static PeasObjectModule *
try_to_open_loader_module (const gchar *loader_id,
                           gboolean     in_subdir)
{
  gchar *tmp_dirname;
  gchar *loader_dirname;
  gchar *loader_basename;
  PeasObjectModule *module;

  if (in_subdir)
    {
      tmp_dirname = peas_dirs_get_plugin_loaders_dir ();
      loader_dirname = g_build_filename (tmp_dirname, loader_id, NULL);
      g_free (tmp_dirname);
    }
  else
    {
      loader_dirname = peas_dirs_get_plugin_loaders_dir ();
    }

  /* Let's build the expected filename of the requested plugin loader */
  loader_basename = g_strdup_printf ("lib%sloader.%s", loader_id, G_MODULE_SUFFIX);

  g_debug ("Loading loader '%s': '%s/%s'", loader_id, loader_dirname, loader_basename);

  module = peas_object_module_new (loader_basename, loader_dirname, TRUE);

  g_free (loader_basename);
  g_free (loader_dirname);

  if (!g_type_module_use (G_TYPE_MODULE (module)))
    {
      g_object_unref (module);
      module = NULL;
    }

  return module;
}

static PeasPluginLoader *
get_plugin_loader (PeasEngine     *engine,
                   PeasPluginInfo *info)
{
  LoaderInfo *loader_info;
  gchar *loader_id;

  loader_info = (LoaderInfo *) g_hash_table_lookup (loaders, info->loader);

  /* The loader has not been enabled. */
  if (loader_info == NULL)
    return NULL;

  /* The loader has already been loaded. */
  if (loader_info->loader != NULL)
    return loader_info->loader;

  /* We need to ensure we use the lowercase loader_id */
  loader_id = g_ascii_strdown (info->loader, -1);

  loader_info->module = try_to_open_loader_module (loader_id, FALSE);
  if (loader_info->module == NULL)
    loader_info->module = try_to_open_loader_module (loader_id, TRUE);

  g_free (loader_id);

  if (loader_info->module == NULL)
    {
      g_hash_table_insert (loaders, g_strdup (info->loader), NULL);
      return NULL;
    }

  loader_info->loader = PEAS_PLUGIN_LOADER (
        peas_object_module_create_object (loader_info->module,
                                          PEAS_TYPE_PLUGIN_LOADER,
                                          0, NULL));

  g_type_module_unuse (G_TYPE_MODULE (loader_info->module));

  if (loader_info->loader == NULL ||
      !peas_plugin_loader_initialize (loader_info->loader))
    {
      g_warning ("Loader '%s' is not a valid PeasPluginLoader instance",
                 info->loader);

      /* This will cause the loader to be unreffed if it exists */
      g_hash_table_insert (loaders, g_strdup (info->loader), NULL);
      return NULL;
    }

  return loader_info->loader;
}

/**
 * peas_engine_enable_loader:
 * @engine: A #PeasEngine.
 * @loader_id: The id of the loader to enable.
 *
 * Enable a loader, enables a loader for plugins.
 * The C plugin loader is always enabled.
 *
 * For instance, the following code will enable python plugin
 * from being loaded:
 * |[
 * peas_engine_enable_loader (engine, "python");
 * ]|
 *
 * Note: plugin loaders are shared across #PeasEngines so enabling
 *       a loader on one #PeasEngine will enable it on all #PeasEngines.
 **/
void
peas_engine_enable_loader (PeasEngine  *engine,
                           const gchar *loader_id)
{
  g_return_if_fail (PEAS_IS_ENGINE (engine));
  g_return_if_fail (loader_id != NULL && *loader_id != '\0');

  if (g_hash_table_lookup_extended (loaders, loader_id, NULL, NULL))
    return;

  /* The loader is loaded in get_plugin_loader() */
  g_hash_table_insert (loaders, g_strdup (loader_id), g_new0 (LoaderInfo, 1));
}

/**
 * peas_engine_get_plugin_list:
 * @engine: A #PeasEngine.
 *
 * Returns the list of #PeasPluginInfo known to the engine.
 *
 * Returns: (transfer none) (element-type Peas.PluginInfo): a #GList of
 * #PeasPluginInfo. Note that the list belongs to the engine and should
 * not be freed.
 **/
const GList *
peas_engine_get_plugin_list (PeasEngine *engine)
{
  g_return_val_if_fail (PEAS_IS_ENGINE (engine), NULL);

  return engine->priv->plugin_list;
}

static gint
compare_plugin_info_and_name (PeasPluginInfo *info,
                              const gchar    *module_name)
{
  return strcmp (peas_plugin_info_get_module_name (info), module_name);
}

/**
 * peas_engine_get_plugin_info:
 * @engine: A #PeasEngine.
 * @plugin_name: A plugin name.
 *
 * Gets the #PeasPluginInfo corresponding with @plugin_name,
 * or %NULL if @plugin_name was not found.
 *
 * Returns: the #PeasPluginInfo corresponding with
 * a given plugin module name.
 */
PeasPluginInfo *
peas_engine_get_plugin_info (PeasEngine  *engine,
                             const gchar *plugin_name)
{
  GList *l;

  g_return_val_if_fail (PEAS_IS_ENGINE (engine), NULL);
  g_return_val_if_fail (plugin_name != NULL, NULL);

  l = g_list_find_custom (engine->priv->plugin_list,
                          plugin_name,
                          (GCompareFunc) compare_plugin_info_and_name);

  return l == NULL ? NULL : (PeasPluginInfo *) l->data;
}

static gboolean
load_plugin (PeasEngine     *engine,
             PeasPluginInfo *info)
{
  const gchar **dependencies;
  PeasPluginInfo *dep_info;
  guint i;
  PeasPluginLoader *loader;

  if (peas_plugin_info_is_loaded (info))
    return TRUE;

  if (!peas_plugin_info_is_available (info, NULL))
    return FALSE;

  /* We set the plugin info as loaded before trying to load the dependencies,
   * to make sure we won't have an infinite loop. */
  info->loaded = TRUE;

  dependencies = peas_plugin_info_get_dependencies (info);
  for (i = 0; dependencies[i] != NULL; i++)
    {
      dep_info = peas_engine_get_plugin_info (engine, dependencies[i]);
      if (!dep_info)
        {
          g_warning ("Could not find plugin '%s' for plugin '%s'",
                     dependencies[i], peas_plugin_info_get_module_name (info));
          g_set_error (&info->error,
                       PEAS_PLUGIN_INFO_ERROR,
                       PEAS_PLUGIN_INFO_ERROR_DEP_NOT_FOUND,
                       _("Dependency '%s' was not found"),
                       dependencies[i]);
          goto error;
        }

      if (!peas_engine_load_plugin (engine, dep_info))
        {
          g_set_error (&info->error,
                       PEAS_PLUGIN_INFO_ERROR,
                       PEAS_PLUGIN_INFO_ERROR_LOADING_FAILED,
                       _("Dependency '%s' failed to load"),
                       peas_plugin_info_get_name (dep_info));
          goto error;
        }
    }

  loader = get_plugin_loader (engine, info);

  if (loader == NULL)
    {
      g_warning ("Could not find loader '%s' for plugin '%s'",
                 info->loader, peas_plugin_info_get_module_name (info));
      g_set_error (&info->error,
                   PEAS_PLUGIN_INFO_ERROR,
                   PEAS_PLUGIN_INFO_ERROR_LOADER_NOT_FOUND,
                   _("Plugin loader '%s' was not found"),
                   info->loader);
      goto error;
    }

  if (!peas_plugin_loader_load (loader, info))
    {
      g_warning ("Error loading plugin '%s'",
                 peas_plugin_info_get_module_name (info));
      g_set_error (&info->error,
                   PEAS_PLUGIN_INFO_ERROR,
                   PEAS_PLUGIN_INFO_ERROR_LOADING_FAILED,
                   _("Failed to load"));
      goto error;
    }

  g_debug ("Loaded plugin '%s'", peas_plugin_info_get_module_name (info));

  return TRUE;

error:

  info->loaded = FALSE;
  info->available = FALSE;

  return FALSE;
}

static void
peas_engine_load_plugin_real (PeasEngine     *engine,
                              PeasPluginInfo *info)
{
  if (load_plugin (engine, info))
    g_object_notify (G_OBJECT (engine), "loaded-plugins");
}

/**
 * peas_engine_load_plugin:
 * @engine: A #PeasEngine.
 * @info: A #PeasPluginInfo.
 *
 * Loads the plugin corresponding to @info if it's not currently loaded.
 * Emits the "load-plugin" signal; loading the plugin
 * actually occurs in the default signal handler.
 *
 * Returns: whether the plugin has been successfully loaded.
 */
gboolean
peas_engine_load_plugin (PeasEngine     *engine,
                         PeasPluginInfo *info)
{
  g_return_val_if_fail (PEAS_IS_ENGINE (engine), FALSE);
  g_return_val_if_fail (info != NULL, FALSE);

  if (!peas_plugin_info_is_available (info, NULL))
    return FALSE;

  if (peas_plugin_info_is_loaded (info))
    return TRUE;

  g_signal_emit (engine, signals[LOAD_PLUGIN], 0, info);

  return peas_plugin_info_is_loaded (info);
}

static void
peas_engine_unload_plugin_real (PeasEngine     *engine,
                                PeasPluginInfo *info)
{
  GList *item;
  const gchar *module_name;
  PeasPluginLoader *loader;

  if (!peas_plugin_info_is_loaded (info) ||
      !peas_plugin_info_is_available (info, NULL))
    return;

  /* We set the plugin info as unloaded before trying to unload the
   * dependants, to make sure we won't have an infinite loop. */
  info->loaded = FALSE;

  /* First unload all the dependant plugins */
  module_name = peas_plugin_info_get_module_name (info);
  for (item = engine->priv->plugin_list; item; item = item->next)
    {
      PeasPluginInfo *other_info = PEAS_PLUGIN_INFO (item->data);

      if (!peas_plugin_info_is_loaded (other_info))
        continue;

      if (peas_plugin_info_has_dependency (other_info, module_name))
         peas_engine_unload_plugin (engine, other_info);
    }

  /* find the loader and tell it to gc and unload the plugin */
  loader = get_plugin_loader (engine, info);

  peas_plugin_loader_garbage_collect (loader);
  peas_plugin_loader_unload (loader, info);

  g_debug ("Unloaded plugin '%s'", peas_plugin_info_get_module_name (info));

  if (!engine->priv->in_dispose)
    g_object_notify (G_OBJECT (engine), "loaded-plugins");
}

/**
 * peas_engine_unload_plugin:
 * @engine: A #PeasEngine.
 * @info: A #PeasPluginInfo.
 *
 * Unloads the plugin corresponding to @info.
 * Emits the "unload-plugin" signal; unloading the plugin
 * actually occurs in the default signal handler.
 *
 * Returns: whether the plugin has been successfully unloaded.
 */
gboolean
peas_engine_unload_plugin (PeasEngine     *engine,
                           PeasPluginInfo *info)
{
  g_return_val_if_fail (PEAS_IS_ENGINE (engine), FALSE);
  g_return_val_if_fail (info != NULL, FALSE);

  if (!peas_plugin_info_is_loaded (info))
    return TRUE;

  g_signal_emit (engine, signals[UNLOAD_PLUGIN], 0, info);

  return !peas_plugin_info_is_loaded (info);
}

/**
 * peas_engine_provides_extension:
 * @engine: A #PeasEngine.
 * @info: A #PeasPluginInfo.
 * @extension_type: The extension #GType.
 *
 * Returns if @info provides an extension for @extension_type.
 * If the @info is not loaded than %FALSE will always be returned.
 *
 * Returns: if @info provides an extension for @extension_type.
 */
gboolean
peas_engine_provides_extension (PeasEngine     *engine,
                                PeasPluginInfo *info,
                                GType           extension_type)
{
  PeasPluginLoader *loader;

  g_return_val_if_fail (PEAS_IS_ENGINE (engine), FALSE);
  g_return_val_if_fail (info != NULL, FALSE);
  g_return_val_if_fail (G_TYPE_IS_INTERFACE (extension_type), FALSE);

  if (!peas_plugin_info_is_loaded (info))
    return FALSE;

  loader = get_plugin_loader (engine, info);
  return peas_plugin_loader_provides_extension (loader, info, extension_type);
}

/**
 * peas_engine_create_extensionv:
 * @engine: A #PeasEngine.
 * @info: A loaded #PeasPluginInfo.
 * @extension_type: The implemented extension #GType.
 * @n_parameters: the length of the @parameters array.
 * @parameters: (allow-none) (array length=n_parameters):
 *   an array of #GParameter.
 *
 * If the plugin identified by @info implements the @extension_type interface,
 * then this function will return a new instance of this implementation,
 * wrapped in a new #PeasExtension instance. Otherwise, it will return %NULL.
 *
 * See peas_engine_create_extension() for more information.
 *
 * Returns: (transfer full): a new instance of #PeasExtension wrapping
 * the @extension_type instance, or %NULL.
 *
 * Rename to: peas_engine_create_extension
 */
PeasExtension *
peas_engine_create_extensionv (PeasEngine     *engine,
                               PeasPluginInfo *info,
                               GType           extension_type,
                               guint           n_parameters,
                               GParameter     *parameters)
{
  PeasPluginLoader *loader;
  PeasExtension *extension;

  g_return_val_if_fail (PEAS_IS_ENGINE (engine), NULL);
  g_return_val_if_fail (info != NULL, NULL);
  g_return_val_if_fail (peas_plugin_info_is_loaded (info), NULL);
  g_return_val_if_fail (G_TYPE_IS_INTERFACE (extension_type), FALSE);

  loader = get_plugin_loader (engine, info);
  extension = peas_plugin_loader_create_extension (loader, info, extension_type,
                                                   n_parameters, parameters);

  if (!G_TYPE_CHECK_INSTANCE_TYPE (extension, extension_type))
    {
      g_warning ("Plugin '%s' does not provide a '%s' extension",
                 peas_plugin_info_get_module_name (info),
                 g_type_name (extension_type));
      return NULL;
    }

  return extension;
}

/**
 * peas_engine_create_extension_valist: (skip)
 * @engine: A #PeasEngine.
 * @info: A loaded #PeasPluginInfo.
 * @extension_type: The implemented extension #GType.
 * @first_property: the name of the first property.
 * @var_args: the value of the first property, followed optionally by more
 *   name/value pairs, followed by %NULL.
 *
 * If the plugin identified by @info implements the @extension_type interface,
 * then this function will return a new instance of this implementation,
 * wrapped in a new #PeasExtension instance. Otherwise, it will return %NULL.
 *
 * See peas_engine_create_extension() for more information.
 *
 * Returns: a new instance of #PeasExtension wrapping
 * the @extension_type instance, or %NULL.
 */
PeasExtension *
peas_engine_create_extension_valist (PeasEngine     *engine,
                                     PeasPluginInfo *info,
                                     GType           extension_type,
                                     const gchar    *first_property,
                                     va_list         var_args)
{
  gpointer type_struct;
  guint n_parameters;
  GParameter *parameters;
  PeasExtension *exten;

  g_return_val_if_fail (PEAS_IS_ENGINE (engine), NULL);
  g_return_val_if_fail (info != NULL, NULL);
  g_return_val_if_fail (peas_plugin_info_is_loaded (info), NULL);
  g_return_val_if_fail (G_TYPE_IS_INTERFACE (extension_type), FALSE);

  type_struct = _g_type_struct_ref (extension_type);

  if (!_valist_to_parameter_list (extension_type, type_struct, first_property,
                                  var_args, &parameters, &n_parameters))
    {
      /* Already warned */
      _g_type_struct_unref (extension_type, type_struct);
      return NULL;
    }

  exten = peas_engine_create_extensionv (engine, info, extension_type,
                                         n_parameters, parameters);

  while (n_parameters-- > 0)
    g_value_unset (&parameters[n_parameters].value);
  g_free (parameters);

  _g_type_struct_unref (extension_type, type_struct);

  return exten;
}

/**
 * peas_engine_create_extension: (skip)
 * @engine: A #PeasEngine.
 * @info: A loaded #PeasPluginInfo.
 * @extension_type: The implemented extension #GType.
 * @first_property: the name of the first property.
 * @...: the value of the first property, followed optionally by more
 *   name/value pairs, followed by %NULL.
 *
 * If the plugin identified by @info implements the @extension_type interface,
 * then this function will return a new instance of this implementation,
 * wrapped in a new #PeasExtension instance. Otherwise, it will return %NULL.
 *
 * When creating the new instance of the @extension_type subtype, the
 * provided construct properties will be passed to the extension construction
 * handler (exactly like if you had called g_object_new() yourself).
 *
 * The new extension instance produced by this function will always be
 * returned wrapped in a #PeasExtension proxy, following the current libpeas
 * principle of never giving you the actual object (also because it might as
 * well *not* be an actual object).
 *
 * Returns: a new instance of #PeasExtension wrapping
 * the @extension_type instance, or %NULL.
 */
PeasExtension *
peas_engine_create_extension (PeasEngine     *engine,
                              PeasPluginInfo *info,
                              GType           extension_type,
                              const gchar    *first_property,
                              ...)
{
  va_list var_args;
  PeasExtension *exten;

  g_return_val_if_fail (PEAS_IS_ENGINE (engine), NULL);
  g_return_val_if_fail (info != NULL, NULL);
  g_return_val_if_fail (peas_plugin_info_is_loaded (info), NULL);
  g_return_val_if_fail (G_TYPE_IS_INTERFACE (extension_type), FALSE);

  va_start (var_args, first_property);
  exten = peas_engine_create_extension_valist (engine, info, extension_type,
                                               first_property, var_args);
  va_end (var_args);

  return exten;
}

/**
 * peas_engine_get_loaded_plugins:
 * @engine: A #PeasEngine.
 *
 * Returns the list of the names of all the loaded plugins, or an array
 * containing a single %NULL element if there is no plugin currently loaded.
 *
 * Please note that the returned array is a newly allocated one: you will need
 * to free it using g_strfreev().
 *
 * Returns: (transfer full) (array zero-terminated=1): A newly-allocated
 * %NULL-terminated array of strings.
 */
gchar **
peas_engine_get_loaded_plugins (PeasEngine *engine)
{
  GArray *array;
  GList *pl;

  g_return_val_if_fail (PEAS_IS_ENGINE (engine), NULL);

  array = g_array_new (TRUE, FALSE, sizeof (gchar *));

  for (pl = engine->priv->plugin_list; pl; pl = pl->next)
    {
      PeasPluginInfo *info = (PeasPluginInfo *) pl->data;
      gchar *module_name;

      if (peas_plugin_info_is_loaded (info))
        {
          module_name = g_strdup (peas_plugin_info_get_module_name (info));
          g_array_append_val (array, module_name);
        }
    }

  return (gchar **) g_array_free (array, FALSE);
}

static gboolean
string_in_strv (const gchar  *needle,
                const gchar **haystack)
{
  guint i;

  if (haystack == NULL)
    return FALSE;

  for (i = 0; haystack[i] != NULL; i++)
    if (strcmp (haystack[i], needle) == 0)
      return TRUE;
  return FALSE;
}

/**
 * peas_engine_set_loaded_plugins:
 * @engine: A #PeasEngine.
 * @plugin_names: (allow-none) (array zero-terminated=1): A %NULL-terminated
 *  array of plugin names, or %NULL.
 *
 * Sets the list of loaded plugins for @engine. When this function is called,
 * the #PeasEngine will load all the plugins whose names are in @plugin_names,
 * and ensures all other active plugins are unloaded.
 *
 * If @plugin_names is %NULL, all plugins will be unloaded.
 */
void
peas_engine_set_loaded_plugins (PeasEngine   *engine,
                                const gchar **plugin_names)
{
  GList *pl;

  g_return_if_fail (PEAS_IS_ENGINE (engine));

  for (pl = engine->priv->plugin_list; pl; pl = pl->next)
    {
      PeasPluginInfo *info = (PeasPluginInfo *) pl->data;
      const gchar *module_name;
      gboolean is_loaded;
      gboolean to_load;

      if (!peas_plugin_info_is_available (info, NULL))
        continue;

      module_name = peas_plugin_info_get_module_name (info);
      is_loaded = peas_plugin_info_is_loaded (info);

      to_load = string_in_strv (module_name, plugin_names);

      if (!is_loaded && to_load)
        g_signal_emit (engine, signals[LOAD_PLUGIN], 0, info);
      else if (is_loaded && !to_load)
        g_signal_emit (engine, signals[UNLOAD_PLUGIN], 0, info);
    }
}

/**
 * peas_engine_new:
 *
 * Return a new instance of #PeasEngine.
 * If no default #PeasEngine has been instantiated yet,
 * the first call of this function will set the default
 * engine as the new instance of #PeasEngine.
 *
 * Returns: a new instance of #PeasEngine.
 */
PeasEngine *
peas_engine_new (void)
{
  return PEAS_ENGINE (g_object_new (PEAS_TYPE_ENGINE, NULL));
}

/**
 * peas_engine_get_default:
 *
 * Return the existing instance of #PeasEngine or a subclass of it.
 * If no #PeasEngine subclass has been instantiated yet, the first call
 * of this function will return a new instance of #PeasEngine.
 *
 * Returns: (transfer none): the existing instance of #PeasEngine.
 */
PeasEngine *
peas_engine_get_default (void)
{
  /* peas_engine_new() will cause the default to be set for us */
  if (default_engine == NULL)
    return peas_engine_new ();

  return default_engine;
}

/*
 * peas_engine_shutdown:
 *
 * Frees memory shared by PeasEngines.
 * No libpeas API should be called after calling this.
 */
void
peas_engine_shutdown (void)
{
  if (shutdown)
    return;

  shutdown = TRUE;

  if (loaders != NULL)
    {
      g_hash_table_destroy (loaders);
      loaders = NULL;
    }
}
