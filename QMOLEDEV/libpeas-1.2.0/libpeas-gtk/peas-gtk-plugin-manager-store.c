/*
 * peas-gtk-plugin-manager-store.c
 * This file is part of libpeas
 *
 * Copyright (C) 2002 Paolo Maggi and James Willcox
 * Copyright (C) 2003-2006 Paolo Maggi, Paolo Borelli
 * Copyright (C) 2007-2009 Paolo Maggi, Paolo Borelli, Steve Frécinaux
 * Copyright (C) 2010 Garrett Regier
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

#include <libpeas/peas-plugin-info.h>

#include "peas-gtk-plugin-manager-store.h"

static const GType ColumnTypes[] = {
  G_TYPE_BOOLEAN, /* Enabled */
  G_TYPE_BOOLEAN, /* Enabled Visible */
  G_TYPE_OBJECT,  /* Pixbuf Icon */
  G_TYPE_STRING,  /* Stock Icon */
  G_TYPE_BOOLEAN, /* Icon Visible */
  G_TYPE_STRING,  /* Info */
  G_TYPE_BOOLEAN, /* Info Visible */
  /* To avoid having to unref it all the time */
  G_TYPE_POINTER  /* PeasPluginInfo */
};

/* G_STATIC_ASSERT is in glib >= 2.20 and we only depend on 2.18.0 */
#ifdef G_STATIC_ASSERT
G_STATIC_ASSERT (G_N_ELEMENTS (ColumnTypes) == PEAS_GTK_PLUGIN_MANAGER_STORE_N_COLUMNS);
#endif

struct _PeasGtkPluginManagerStorePrivate {
  PeasEngine *engine;
};

/* Properties */
enum {
  PROP_0,
  PROP_ENGINE
};

G_DEFINE_TYPE (PeasGtkPluginManagerStore, peas_gtk_plugin_manager_store, GTK_TYPE_LIST_STORE);

static void
update_plugin (PeasGtkPluginManagerStore *store,
               GtkTreeIter               *iter,
               PeasPluginInfo            *info)
{
  gboolean loaded;
  gboolean available;
  gboolean builtin;
  gchar *markup;
  const gchar *icon_name;
  GdkPixbuf *icon_pixbuf = NULL;

  loaded = peas_plugin_info_is_loaded (info);
  available = peas_plugin_info_is_available (info, NULL);
  builtin = peas_plugin_info_is_builtin (info);

  if (peas_plugin_info_get_description (info) == NULL)
    {
      markup = g_markup_printf_escaped ("<b>%s</b>",
                                        peas_plugin_info_get_name (info));
    }
  else
    {
      markup = g_markup_printf_escaped ("<b>%s</b>\n%s",
                                        peas_plugin_info_get_name (info),
                                        peas_plugin_info_get_description (info));
    }

  if (!available)
    {
      icon_name = GTK_STOCK_DIALOG_ERROR;
    }
  else
    {
      gchar *icon_filename;

      icon_name = peas_plugin_info_get_icon_name (info);
      icon_filename = g_build_filename (peas_plugin_info_get_data_dir (info),
                                        icon_name,
                                        NULL);

      if (!gtk_icon_theme_has_icon (gtk_icon_theme_get_default (), icon_name))
        icon_name = "libpeas-plugin";

      /* Prevent warning for the common case that icon_filename
       * does not exist but warn when it is a directory
       */
      if (g_file_test (icon_filename, G_FILE_TEST_EXISTS))
        {
          GError *error = NULL;
          gint width, height;

          /* Attempt to load the icon scaled to the correct size */
          if (!gtk_icon_size_lookup (GTK_ICON_SIZE_SMALL_TOOLBAR,
                                     &width, &height))
            {
              icon_pixbuf = gdk_pixbuf_new_from_file (icon_filename, &error);
            }
          else
            {
              icon_pixbuf = gdk_pixbuf_new_from_file_at_size (icon_filename,
                                                              width, height,
                                                              &error);
            }

          if (error == NULL)
            icon_name = NULL;
          else
            {
              g_warning ("Error while loading icon: %s", error->message);
              g_error_free (error);
            }
        }

      g_free (icon_filename);
    }

  gtk_list_store_set (GTK_LIST_STORE (store), iter,
    PEAS_GTK_PLUGIN_MANAGER_STORE_ENABLED_COLUMN,        loaded,
    PEAS_GTK_PLUGIN_MANAGER_STORE_CAN_ENABLE_COLUMN,     !builtin && available,
    PEAS_GTK_PLUGIN_MANAGER_STORE_ICON_PIXBUF_COLUMN,    icon_pixbuf,
    PEAS_GTK_PLUGIN_MANAGER_STORE_ICON_NAME_COLUMN,      icon_name,
    PEAS_GTK_PLUGIN_MANAGER_STORE_ICON_VISIBLE_COLUMN,   !available,
    PEAS_GTK_PLUGIN_MANAGER_STORE_INFO_COLUMN,           markup,
    PEAS_GTK_PLUGIN_MANAGER_STORE_INFO_SENSITIVE_COLUMN, available && (!builtin || loaded),
    PEAS_GTK_PLUGIN_MANAGER_STORE_PLUGIN_COLUMN,         info,
    -1);

  if (icon_pixbuf != NULL)
    g_object_unref (icon_pixbuf);

  g_free (markup);
}

static void
plugin_loaded_toggled_cb (PeasEngine                *engine,
                          PeasPluginInfo            *info,
                          PeasGtkPluginManagerStore *store)
{
  GtkTreeIter iter;

  if (peas_gtk_plugin_manager_store_get_iter_from_plugin (store, &iter, info))
    update_plugin (store, &iter, info);
}

static gint
model_name_sort_func (PeasGtkPluginManagerStore *store,
                      GtkTreeIter               *iter1,
                      GtkTreeIter               *iter2,
                      gpointer                   user_data)
{
  PeasPluginInfo *info1;
  PeasPluginInfo *info2;

  info1 = peas_gtk_plugin_manager_store_get_plugin (store, iter1);
  info2 = peas_gtk_plugin_manager_store_get_plugin (store, iter2);

  return g_utf8_collate (peas_plugin_info_get_name (info1),
                         peas_plugin_info_get_name (info2));
}

static void
peas_gtk_plugin_manager_store_init (PeasGtkPluginManagerStore *store)
{
  store->priv = G_TYPE_INSTANCE_GET_PRIVATE (store,
                                             PEAS_GTK_TYPE_PLUGIN_MANAGER_STORE,
                                             PeasGtkPluginManagerStorePrivate);

  gtk_list_store_set_column_types (GTK_LIST_STORE (store),
                                   PEAS_GTK_PLUGIN_MANAGER_STORE_N_COLUMNS,
                                   (GType *) ColumnTypes);

  /* Sort on the plugin names */
  gtk_tree_sortable_set_default_sort_func (GTK_TREE_SORTABLE (store),
                                           (GtkTreeIterCompareFunc) model_name_sort_func,
                                           NULL, NULL);
  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (store),
                                        GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID,
                                        GTK_SORT_ASCENDING);
}

static void
peas_gtk_plugin_manager_store_set_property (GObject      *object,
                                            guint         prop_id,
                                            const GValue *value,
                                            GParamSpec   *pspec)
{
  PeasGtkPluginManagerStore *store = PEAS_GTK_PLUGIN_MANAGER_STORE (object);

  switch (prop_id)
    {
    case PROP_ENGINE:
      store->priv->engine = g_value_get_object (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
peas_gtk_plugin_manager_store_get_property (GObject    *object,
                                            guint       prop_id,
                                            GValue     *value,
                                            GParamSpec *pspec)
{
  PeasGtkPluginManagerStore *store = PEAS_GTK_PLUGIN_MANAGER_STORE (object);

  switch (prop_id)
    {
    case PROP_ENGINE:
      g_value_set_object (value, store->priv->engine);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
peas_gtk_plugin_manager_store_constructed (GObject *object)
{
  PeasGtkPluginManagerStore *store = PEAS_GTK_PLUGIN_MANAGER_STORE (object);

  if (store->priv->engine == NULL)
    store->priv->engine = peas_engine_get_default ();

  g_object_ref (store->priv->engine);

  g_signal_connect_after (store->priv->engine,
                          "load-plugin",
                          G_CALLBACK (plugin_loaded_toggled_cb),
                          store);
  g_signal_connect_after (store->priv->engine,
                          "unload-plugin",
                          G_CALLBACK (plugin_loaded_toggled_cb),
                          store);

  peas_gtk_plugin_manager_store_reload (store);

  if (G_OBJECT_CLASS (peas_gtk_plugin_manager_store_parent_class)->constructed != NULL)
    G_OBJECT_CLASS (peas_gtk_plugin_manager_store_parent_class)->constructed (object);
}

static void
peas_gtk_plugin_manager_store_dispose (GObject *object)
{
  PeasGtkPluginManagerStore *store = PEAS_GTK_PLUGIN_MANAGER_STORE (object);

  if (store->priv->engine != NULL)
    {
      g_signal_handlers_disconnect_by_func (store->priv->engine,
                                            plugin_loaded_toggled_cb,
                                            store);

      g_object_unref (store->priv->engine);
      store->priv->engine = NULL;
    }

  G_OBJECT_CLASS (peas_gtk_plugin_manager_store_parent_class)->dispose (object);
}

static void
peas_gtk_plugin_manager_store_class_init (PeasGtkPluginManagerStoreClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->set_property = peas_gtk_plugin_manager_store_set_property;
  object_class->get_property = peas_gtk_plugin_manager_store_get_property;
  object_class->constructed = peas_gtk_plugin_manager_store_constructed;
  object_class->dispose = peas_gtk_plugin_manager_store_dispose;

  /*
   * PeasGtkPLuginManagerStore:engine:
   *
   * The #PeasEngine this store is attached to.
   */
  g_object_class_install_property (object_class,
                                   PROP_ENGINE,
                                   g_param_spec_object ("engine",
                                                        "engine",
                                                        "The PeasEngine this store is attached to",
                                                        PEAS_TYPE_ENGINE,
                                                        G_PARAM_READWRITE |
                                                        G_PARAM_CONSTRUCT_ONLY |
                                                        G_PARAM_STATIC_STRINGS));

  g_type_class_add_private (object_class, sizeof (PeasGtkPluginManagerStorePrivate));
}

/*
 * peas_gtk_plugin_manager_store_new:
 * @engine: (allow-none): A #PeasEngine, or %NULL.
 *
 * Creates a new plugin manager store for the given #PeasEngine.
 *
 * If @engine is %NULL, then the default engine will be used.
 *
 * Returns: the new #PeasGtkPluginManagerStore.
 */
PeasGtkPluginManagerStore  *
peas_gtk_plugin_manager_store_new (PeasEngine *engine)
{
  g_return_val_if_fail (engine == NULL || PEAS_IS_ENGINE (engine), NULL);

  return PEAS_GTK_PLUGIN_MANAGER_STORE (g_object_new (PEAS_GTK_TYPE_PLUGIN_MANAGER_STORE,
                                                      "engine", engine,
                                                      NULL));
}

/*
 * peas_gtk_plugin_manager_store_reload:
 * @store: A #PeasGtkPluginManagerStore.
 *
 * Reloads the list of plugins.
 */
void
peas_gtk_plugin_manager_store_reload (PeasGtkPluginManagerStore *store)
{
  GtkListStore *list_store;
  const GList *plugins;
  GtkTreeIter iter;

  g_return_if_fail (PEAS_GTK_IS_PLUGIN_MANAGER_STORE (store));

  list_store = GTK_LIST_STORE (store);

  gtk_list_store_clear (list_store);

  plugins = peas_engine_get_plugin_list (store->priv->engine);

  while (plugins != NULL)
    {
      PeasPluginInfo *info;

      info = PEAS_PLUGIN_INFO (plugins->data);

      if (!peas_plugin_info_is_hidden (info))
        {
          gtk_list_store_append (list_store, &iter);
          update_plugin (store, &iter, info);
        }

      plugins = plugins->next;
    }
}

/*
 * peas_gtk_plugin_manager_store_set_enabled:
 * @store: A #PeasGtkPluginManagerStore.
 * @iter: A #GtkTreeIter.
 * @enabled: If the plugin should be enabled.
 *
 * Sets if the plugin at @iter should be enabled.
 */
void
peas_gtk_plugin_manager_store_set_enabled (PeasGtkPluginManagerStore *store,
                                           GtkTreeIter               *iter,
                                           gboolean                   enabled)
{
  PeasPluginInfo *info;
  gboolean success = TRUE;

  g_return_if_fail (PEAS_GTK_IS_PLUGIN_MANAGER_STORE (store));
  g_return_if_fail (iter != NULL);
  g_return_if_fail (peas_gtk_plugin_manager_store_can_enable (store, iter));

  info = peas_gtk_plugin_manager_store_get_plugin (store, iter);
  g_return_if_fail (info != NULL);

  if (enabled)
    {
      /* load the plugin */
      if (!peas_engine_load_plugin (store->priv->engine, info))
        success = FALSE;
    }
  else
    {
      /* unload the plugin */
      if (!peas_engine_unload_plugin (store->priv->engine, info))
        success = FALSE;
    }

  if (success)
    update_plugin (store, iter, info);
}

/*
 * peas_gtk_plugin_manager_store_set_enabled:
 * @store: A #PeasGtkPluginManagerStore.
 * @iter: A #GtkTreeIter.
 *
 * Returns if the plugin at @iter is enabled.
 *
 * Returns: if the plugin at @iter is enabled.
 */
gboolean
peas_gtk_plugin_manager_store_get_enabled (PeasGtkPluginManagerStore *store,
                                           GtkTreeIter               *iter)
{
  GValue value = { 0 };
  gboolean enabled;

  g_return_val_if_fail (PEAS_GTK_IS_PLUGIN_MANAGER_STORE (store), FALSE);
  g_return_val_if_fail (iter != NULL, FALSE);

  gtk_tree_model_get_value (GTK_TREE_MODEL (store), iter,
                            PEAS_GTK_PLUGIN_MANAGER_STORE_ENABLED_COLUMN, &value);

  g_return_val_if_fail (G_VALUE_HOLDS_BOOLEAN (&value), FALSE);
  enabled = g_value_get_boolean (&value);

  g_value_unset (&value);

  return enabled;
}

/*
 * peas_gtk_plugin_manager_store_set_all_enabled:
 * @store: A #PeasGtkPluginManagerStore.
 * @enabled: If all the plugins should be enabled.
 *
 * Sets if all the plugins should be enabled.
 */
void
peas_gtk_plugin_manager_store_set_all_enabled (PeasGtkPluginManagerStore *store,
                                               gboolean                   enabled)
{
  GtkTreeModel *model;
  GtkTreeIter iter;

  g_return_if_fail (PEAS_GTK_IS_PLUGIN_MANAGER_STORE (store));

  model = GTK_TREE_MODEL (store);

  if (!gtk_tree_model_get_iter_first (model, &iter))
    return;

  do
    {
      if (peas_gtk_plugin_manager_store_can_enable (store, &iter))
        peas_gtk_plugin_manager_store_set_enabled (store, &iter, enabled);
    }
  while (gtk_tree_model_iter_next (model, &iter));
}

/*
 * peas_gtk_plugin_manager_store_toggle_enabled:
 * @store: A #PeasGtkPluginManagerStore.
 * @iter: A #GtkTreeIter.
 *
 * Toggles the if the plugin should should be enabled.
 */
void
peas_gtk_plugin_manager_store_toggle_enabled (PeasGtkPluginManagerStore *store,
                                              GtkTreeIter               *iter)
{
  gboolean enabled;

  g_return_if_fail (PEAS_GTK_IS_PLUGIN_MANAGER_STORE (store));
  g_return_if_fail (iter != NULL);

  enabled = peas_gtk_plugin_manager_store_get_enabled (store, iter);

  peas_gtk_plugin_manager_store_set_enabled (store, iter, !enabled);
}

/*
 * peas_gtk_plugin_manager_store_can_enabled:
 * @store: A #PeasGtkPluginManagerStore.
 * @iter: A #GtkTreeIter.
 *
 * Returns if the plugin at @iter can be enabled.
 * Note: that while a plugin may be enableable there are other factors
 * that can cause it to not be enabled.
 *
 * Returns: if the plugin can be enabled.
 */
gboolean
peas_gtk_plugin_manager_store_can_enable (PeasGtkPluginManagerStore *store,
                                          GtkTreeIter               *iter)
{
  GValue value = { 0 };
  gboolean can_enable;

  g_return_val_if_fail (PEAS_GTK_IS_PLUGIN_MANAGER_STORE (store), FALSE);
  g_return_val_if_fail (iter != NULL, FALSE);

  gtk_tree_model_get_value (GTK_TREE_MODEL (store), iter,
                            PEAS_GTK_PLUGIN_MANAGER_STORE_CAN_ENABLE_COLUMN, &value);

  g_return_val_if_fail (G_VALUE_HOLDS_BOOLEAN (&value), FALSE);
  can_enable = g_value_get_boolean (&value);

  g_value_unset (&value);

  return can_enable;
}

/*
 * peas_gtk_plugin_manager_store_get_plugin:
 * @store: A #PeasGtkPluginManagerStore.
 * @iter: A #GtkTreeIter.
 *
 * Returns the plugin at @iter.
 *
 * Returns: the plugin at @iter.
 */
PeasPluginInfo *
peas_gtk_plugin_manager_store_get_plugin (PeasGtkPluginManagerStore *store,
                                          GtkTreeIter               *iter)
{
  GValue value = { 0 };
  PeasPluginInfo *info;

  g_return_val_if_fail (PEAS_GTK_IS_PLUGIN_MANAGER_STORE (store), NULL);
  g_return_val_if_fail (iter != NULL, NULL);

  gtk_tree_model_get_value (GTK_TREE_MODEL (store), iter,
                            PEAS_GTK_PLUGIN_MANAGER_STORE_PLUGIN_COLUMN, &value);

  g_return_val_if_fail (G_VALUE_HOLDS_POINTER (&value), NULL);
  info = g_value_get_pointer (&value);

  g_value_unset (&value);

  /* We register it as a pointer instead
   * of a boxed so no need to unref it
   */
  return info;
}

/*
 * peas_gtk_plugin_manager_store_get_iter_from_plugin:
 * @store: A #PeasGtkPluginManagerStore.
 * @iter: A #GtkTreeIter.
 * @info: A #PeasPluginInfo.
 *
 * Sets @iter to the @info.
 *
 * Returns: if @iter was set.
 */
gboolean
peas_gtk_plugin_manager_store_get_iter_from_plugin (PeasGtkPluginManagerStore *store,
                                                    GtkTreeIter               *iter,
                                                    const PeasPluginInfo      *info)
{
  GtkTreeModel *model = GTK_TREE_MODEL (store);
  gboolean found = FALSE;

  g_return_val_if_fail (PEAS_GTK_IS_PLUGIN_MANAGER_STORE (store), FALSE);
  g_return_val_if_fail (iter != NULL, FALSE);
  g_return_val_if_fail (info != NULL, FALSE);

  if (gtk_tree_model_get_iter_first (model, iter))
    {
      PeasPluginInfo *current_info;

      do
        {
          current_info = peas_gtk_plugin_manager_store_get_plugin (store, iter);

          found = (info == current_info);
        }
      while (!found && gtk_tree_model_iter_next (model, iter));
    }

  return found;
}
