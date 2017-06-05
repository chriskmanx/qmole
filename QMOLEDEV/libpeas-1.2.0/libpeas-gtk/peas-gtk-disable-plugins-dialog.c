/*
 * peas-gtk-disable-plugins-dialog.c
 * This file is part of libpeas
 *
 * Copyright (C) 2011 Garrett Regier
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

#include <libpeas/peas-i18n.h>
#include <libpeas/peas-plugin-info.h>

#include "peas-gtk-disable-plugins-dialog.h"

enum {
  PLUGIN_INFO_NAME_COLUMN = 0
};

struct _PeasGtkDisablePluginsDialogPrivate {
  PeasPluginInfo *plugin_info;
  GList *dep_plugins;
};

/* Properties */
enum {
  PROP_0,
  PROP_PLUGIN_INFO,
  PROP_DEPENDANT_PLUGINS
};

G_DEFINE_TYPE (PeasGtkDisablePluginsDialog,
               peas_gtk_disable_plugins_dialog,
               GTK_TYPE_MESSAGE_DIALOG);

static gint
model_name_sort_func (GtkListStore *store,
                      GtkTreeIter  *iter1,
                      GtkTreeIter  *iter2,
                      gpointer      user_data)
{
  gchar *name1, *name2;
  gint retval;

  gtk_tree_model_get (GTK_TREE_MODEL (store), iter1,
                      PLUGIN_INFO_NAME_COLUMN, &name1,
                      -1);

  gtk_tree_model_get (GTK_TREE_MODEL (store), iter1,
                      PLUGIN_INFO_NAME_COLUMN, &name2,
                      -1);

  retval = g_utf8_collate (name1, name2);

  g_free (name1);
  g_free (name2);

  return retval;
}

static void
build_multiple_dependant_plugins (PeasGtkDisablePluginsDialog *dialog)
{
  gchar *message;
  GtkWidget *message_area;
  GtkWidget *sw;
  GtkListStore *store;
  GList *dep_plugin;
  GtkWidget *tree_view;
  GtkCellRenderer *cell;

  message = g_strconcat ("<span weight=\"bold\" size=\"larger\">",
                         _("Additional plugins must be disabled"),
                         "</span>", NULL);

  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), message);
  g_free (message);

  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
      _("The following plugins depend on '%s' and will also be disabled:"),
      peas_plugin_info_get_name (dialog->priv->plugin_info));

  message_area = gtk_message_dialog_get_message_area (GTK_MESSAGE_DIALOG (dialog));

  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (sw),
                                       GTK_SHADOW_IN);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
                                  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start (GTK_BOX (message_area), sw, TRUE, TRUE, 0);

  store = gtk_list_store_new (1, G_TYPE_STRING);

  /* Sort on the plugin names */
  gtk_tree_sortable_set_default_sort_func (GTK_TREE_SORTABLE (store),
                                           (GtkTreeIterCompareFunc) model_name_sort_func,
                                           NULL, NULL);
  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (store),
                                        GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID,
                                        GTK_SORT_ASCENDING);

  for (dep_plugin = dialog->priv->dep_plugins; dep_plugin != NULL;
       dep_plugin = dep_plugin->next)
    {
      PeasPluginInfo *plugin = (PeasPluginInfo *) dep_plugin->data;
      GtkTreeIter iter;

      gtk_list_store_append (store, &iter);
      gtk_list_store_set (store, &iter,
                          PLUGIN_INFO_NAME_COLUMN, peas_plugin_info_get_name (plugin),
                          -1);
    }

  tree_view = gtk_tree_view_new_with_model (GTK_TREE_MODEL (store));
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (tree_view), FALSE);
  gtk_tree_view_set_enable_search (GTK_TREE_VIEW (tree_view), FALSE);
  gtk_widget_set_size_request (tree_view, 260, 120);
  gtk_container_add (GTK_CONTAINER (sw), tree_view);

  cell = gtk_cell_renderer_text_new ();
  gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (tree_view),
                                               0, _("Plugins"),
                                               cell,
                                               "text", PLUGIN_INFO_NAME_COLUMN,
                                               NULL);

  g_object_unref (store);

  gtk_widget_show_all (sw);
}

static void
build_single_dependant_plugin (PeasGtkDisablePluginsDialog *dialog)
{
  gchar *message;

  message = g_strconcat ("<span weight=\"bold\" size=\"larger\">",
                         _("An additional plugin must be disabled"),
                         "</span>", NULL);

  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), message);
  g_free (message);

  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
      _("The '%s' plugin depends on the '%s' plugin.\n"
        "If you disable '%s', '%s' will also be disabled."),
      peas_plugin_info_get_name (dialog->priv->plugin_info),
      peas_plugin_info_get_name (dialog->priv->dep_plugins->data),
      peas_plugin_info_get_name (dialog->priv->plugin_info),
      peas_plugin_info_get_name (dialog->priv->dep_plugins->data));
}

static void
peas_gtk_disable_plugins_dialog_init (PeasGtkDisablePluginsDialog *dialog)
{
  dialog->priv = G_TYPE_INSTANCE_GET_PRIVATE (dialog,
                                              PEAS_GTK_TYPE_DISABLE_PLUGINS_DIALOG,
                                              PeasGtkDisablePluginsDialogPrivate);

  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         _("Disable Plugins"), GTK_RESPONSE_OK);
}

static void
peas_gtk_disable_plugins_dialog_set_property (GObject      *object,
                                              guint         prop_id,
                                              const GValue *value,
                                              GParamSpec   *pspec)
{
  PeasGtkDisablePluginsDialog *dialog = PEAS_GTK_DISABLE_PLUGINS_DIALOG (object);

  switch (prop_id)
    {
    case PROP_PLUGIN_INFO:
      dialog->priv->plugin_info = g_value_get_pointer (value);
      break;
    case PROP_DEPENDANT_PLUGINS:
      dialog->priv->dep_plugins = g_value_get_pointer (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
peas_gtk_disable_plugins_dialog_get_property (GObject    *object,
                                              guint       prop_id,
                                              GValue     *value,
                                              GParamSpec *pspec)
{
  PeasGtkDisablePluginsDialog *dialog = PEAS_GTK_DISABLE_PLUGINS_DIALOG (object);

  switch (prop_id)
    {
    case PROP_PLUGIN_INFO:
      g_value_set_pointer (value, dialog->priv->plugin_info);
      break;
    case PROP_DEPENDANT_PLUGINS:
      g_value_set_pointer (value, dialog->priv->dep_plugins);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
peas_gtk_disable_plugins_dialog_constructed (GObject *object)
{
  PeasGtkDisablePluginsDialog *dialog = PEAS_GTK_DISABLE_PLUGINS_DIALOG (object);

  if (dialog->priv->dep_plugins->next == NULL)
    build_single_dependant_plugin (dialog);
  else
    build_multiple_dependant_plugins (dialog);

  if (G_OBJECT_CLASS (peas_gtk_disable_plugins_dialog_parent_class)->constructed != NULL)
    G_OBJECT_CLASS (peas_gtk_disable_plugins_dialog_parent_class)->constructed (object);
}

static void
peas_gtk_disable_plugins_dialog_finalize (GObject *object)
{
  PeasGtkDisablePluginsDialog *dialog = PEAS_GTK_DISABLE_PLUGINS_DIALOG (object);

  g_list_free (dialog->priv->dep_plugins);

  G_OBJECT_CLASS (peas_gtk_disable_plugins_dialog_parent_class)->finalize (object);
}

static void
peas_gtk_disable_plugins_dialog_class_init (PeasGtkDisablePluginsDialogClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->get_property = peas_gtk_disable_plugins_dialog_get_property;
  object_class->set_property = peas_gtk_disable_plugins_dialog_set_property;
  object_class->constructed = peas_gtk_disable_plugins_dialog_constructed;
  object_class->finalize = peas_gtk_disable_plugins_dialog_finalize;

  g_object_class_install_property (object_class,
                                   PROP_PLUGIN_INFO,
                                   g_param_spec_pointer ("plugin-info",
                                                         "Plugin Information",
                                                         "Plugin that is being disabled",
                                                         G_PARAM_READWRITE |
                                                         G_PARAM_CONSTRUCT_ONLY |
                                                         G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (object_class,
                                   PROP_DEPENDANT_PLUGINS,
                                   g_param_spec_pointer ("dependant-plugins",
                                                         "Dependant plugins",
                                                         "Dependant plugins",
                                                         G_PARAM_READWRITE |
                                                         G_PARAM_CONSTRUCT_ONLY |
                                                         G_PARAM_STATIC_STRINGS));

  g_type_class_add_private (object_class, sizeof (PeasGtkDisablePluginsDialogPrivate));
}

/*
 * peas_gtk_disable_plugins_dialog_new:
 * @parent: transient window.
 * @info: the #PeasPluginInfo being disabled.
 * @dep_plugins: (transfer container) (element-type Peas.PluginInfo):
 *  list of plugins that are dependant on @info.
 *
 * Creates a new #PeasGtkDisablePluginsDialog.
 *
 * Returns: the new #PeasGtkDisablePluginsDialog.
 */
GtkWidget  *
peas_gtk_disable_plugins_dialog_new (GtkWindow      *parent,
                                     PeasPluginInfo *info,
                                     GList          *dep_plugins)
{
  return GTK_WIDGET (g_object_new (PEAS_GTK_TYPE_DISABLE_PLUGINS_DIALOG,
                                   "transient-for", parent,
                                   "plugin-info", info,
                                   "dependant-plugins", dep_plugins,
                                   "message-type", GTK_MESSAGE_QUESTION,
                                   NULL));
}
