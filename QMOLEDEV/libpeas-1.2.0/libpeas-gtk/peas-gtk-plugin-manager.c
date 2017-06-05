/*
 * peas-plugin-manager.c
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

#include <string.h>
#include <girepository.h>

#ifdef OS_OSX
#include <Carbon/Carbon.h>
#endif

#include <libpeas/peas-engine.h>
#include <libpeas/peas-plugin-info.h>
#include <libpeas/peas-i18n.h>

#include "peas-gtk-plugin-manager.h"
#include "peas-gtk-plugin-manager-view.h"
#include "peas-gtk-configurable.h"

/**
 * SECTION:peas-gtk-plugin-manager
 * @short_description: Management GUI for plugins.
 *
 * The #PeasGtkPluginManager is a widget that can be used to manage plugins,
 * i.e. load or unload them, and see some pieces of information.
 *
 * <inlinegraphic fileref="peas-gtk-plugin-manager.png" format="PNG" />
 *
 * The only thing you need to do as an application writer if you wish to use
 * the manager to configure your plugins is to instantiate it using
 * peas_gtk_plugin_manager_new() and pack it into another widget or a window
 * (as in the screenshot above).
 *
 **/

struct _PeasGtkPluginManagerPrivate {
  PeasEngine *engine;

  GtkWidget *sw;
  GtkWidget *view;

  GtkWidget *about;

  GtkWidget *about_button;
  GtkWidget *configure_button;
};

/* Properties */
enum {
  PROP_0,
  PROP_ENGINE,
  PROP_VIEW
};

G_DEFINE_TYPE (PeasGtkPluginManager, peas_gtk_plugin_manager, GTK_TYPE_BOX);

static gboolean
plugin_is_configurable (PeasGtkPluginManager *pm,
                        PeasPluginInfo       *info)
{
  if (info == NULL || !peas_plugin_info_is_loaded (info))
    return FALSE;

  return peas_engine_provides_extension (pm->priv->engine,
                                         info,
                                         PEAS_GTK_TYPE_CONFIGURABLE);
}

static void
update_button_sensitivity (PeasGtkPluginManager *pm,
                           PeasPluginInfo       *info)
{
  gtk_widget_set_sensitive (pm->priv->about_button, info != NULL);
  gtk_widget_set_sensitive (pm->priv->configure_button,
                            plugin_is_configurable (pm, info));
}

static void
show_about_cb (GtkWidget            *widget,
               PeasGtkPluginManager *pm)
{
  PeasGtkPluginManagerView *view;
  PeasPluginInfo *info;

  view = PEAS_GTK_PLUGIN_MANAGER_VIEW (pm->priv->view);

  info = peas_gtk_plugin_manager_view_get_selected_plugin (view);
  g_return_if_fail (info != NULL);

  /* if there is another about dialog already open destroy it */
  if (pm->priv->about)
    gtk_widget_destroy (pm->priv->about);

  pm->priv->about = GTK_WIDGET (g_object_new (GTK_TYPE_ABOUT_DIALOG,
                                              "program-name", peas_plugin_info_get_name (info),
                                              "copyright", peas_plugin_info_get_copyright (info),
                                              "authors", peas_plugin_info_get_authors (info),
                                              "comments", peas_plugin_info_get_description (info),
                                              "website", peas_plugin_info_get_website (info),
                                              "logo-icon-name", peas_plugin_info_get_icon_name (info),
                                              "version", peas_plugin_info_get_version (info),
                                              NULL));

  gtk_window_set_destroy_with_parent (GTK_WINDOW (pm->priv->about), TRUE);

  g_signal_connect (pm->priv->about,
                    "response",
                    G_CALLBACK (gtk_widget_destroy),
                    NULL);
  g_signal_connect (pm->priv->about,
                    "destroy",
                    G_CALLBACK (gtk_widget_destroyed),
                    &pm->priv->about);

  gtk_window_set_transient_for (GTK_WINDOW (pm->priv->about),
                                GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (pm))));
  gtk_widget_show (pm->priv->about);
}

static void
help_button_cb (GtkWidget      *button,
                PeasPluginInfo *info)
{
  const gchar *help_uri;
#ifndef OS_OSX
  GError *error = NULL;
  GtkWindow *toplevel;
  GtkWidget *error_dlg;
  GtkWindowGroup *wg;
#endif

  g_return_if_fail (peas_plugin_info_get_help_uri (info) != NULL);

  help_uri = peas_plugin_info_get_help_uri (info);

#ifdef OS_OSX
  [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:[NSString stringWithUTF8String:help_uri]]];
#else

  gtk_show_uri (NULL,
                help_uri,
                GDK_CURRENT_TIME,
                &error);

  if (error == NULL)
    return;

  g_debug ("PeasGtkPluginManager: could not show help uri: '%s'", help_uri);

  toplevel = GTK_WINDOW (gtk_widget_get_toplevel (button));
  error_dlg = gtk_message_dialog_new (toplevel,
                                      0,
                                      GTK_MESSAGE_ERROR,
                                      GTK_BUTTONS_CLOSE,
                                      _("There was an error displaying the help."));

  g_signal_connect (error_dlg,
                    "response",
                    G_CALLBACK (gtk_widget_destroy), NULL);

  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (error_dlg),
                                            "%s", error->message);

  if (gtk_window_has_group (toplevel))
    {
      wg = gtk_window_get_group (toplevel);
    }
  else
    {
      wg = gtk_window_group_new ();
      gtk_window_group_add_window (wg, toplevel);
    }

  gtk_window_group_add_window (wg, GTK_WINDOW (error_dlg));

  gtk_window_set_modal (GTK_WINDOW (error_dlg), TRUE);
  gtk_widget_show_all (error_dlg);

  g_error_free (error);
#endif
}

static void
show_configure_cb (GtkWidget            *widget,
                   PeasGtkPluginManager *pm)
{
  PeasGtkPluginManagerView *view = PEAS_GTK_PLUGIN_MANAGER_VIEW (pm->priv->view);
  PeasPluginInfo *info;
  PeasExtension *exten;
  GtkWindow *toplevel;
  GtkWidget *conf_widget = NULL;
  GtkWidget *conf_dlg;
  GtkWidget *vbox;
  GtkWindowGroup *wg;

  info = peas_gtk_plugin_manager_view_get_selected_plugin (view);
  g_return_if_fail (info != NULL);

  exten = peas_engine_create_extension (pm->priv->engine, info, PEAS_GTK_TYPE_CONFIGURABLE, NULL);
  g_return_if_fail (PEAS_IS_EXTENSION (exten));

  conf_widget = peas_gtk_configurable_create_configure_widget (PEAS_GTK_CONFIGURABLE (exten));
  g_object_unref (exten);

  g_return_if_fail (GTK_IS_WIDGET (conf_widget));
  g_return_if_fail (!gtk_widget_is_toplevel (conf_widget));

  toplevel = GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (pm)));

  conf_dlg = gtk_dialog_new_with_buttons (peas_plugin_info_get_name (info),
                                          toplevel,
                                          0,
                                          GTK_STOCK_CLOSE,
                                          GTK_RESPONSE_CLOSE,
                                          NULL);

  vbox = gtk_dialog_get_content_area (GTK_DIALOG (conf_dlg));
  gtk_box_pack_start (GTK_BOX (vbox), conf_widget, TRUE, TRUE, 0);

  if (peas_plugin_info_get_help_uri (info) != NULL)
    {
      GtkWidget *hbuttonbox;
      GtkWidget *help_button;

      hbuttonbox = gtk_dialog_get_action_area (GTK_DIALOG (conf_dlg));
      help_button = gtk_button_new_from_stock (GTK_STOCK_HELP);
      gtk_container_add (GTK_CONTAINER (hbuttonbox), help_button);
      gtk_button_box_set_child_secondary (GTK_BUTTON_BOX (hbuttonbox),
                                          help_button, TRUE);

      g_signal_connect (help_button,
                        "clicked",
                        G_CALLBACK (help_button_cb),
                        info);
    }

  if (gtk_window_has_group (toplevel))
    {
      wg = gtk_window_get_group (toplevel);
    }
  else
    {
      wg = gtk_window_group_new ();
      gtk_window_group_add_window (wg, toplevel);
    }

  gtk_window_group_add_window (wg, GTK_WINDOW (conf_dlg));

  gtk_window_set_transient_for (GTK_WINDOW (conf_dlg), toplevel);
  gtk_window_set_modal (GTK_WINDOW (conf_dlg), TRUE);
  gtk_widget_show_all (conf_dlg);

  g_signal_connect (conf_dlg,
                    "response",
                    G_CALLBACK (gtk_widget_destroy),
                    NULL);
}

static void
plugin_loaded_toggled_cb (PeasEngine           *engine,
                          PeasPluginInfo       *info,
                          PeasGtkPluginManager *pm)
{
  PeasGtkPluginManagerView *view;
  PeasPluginInfo *selected;

  view = PEAS_GTK_PLUGIN_MANAGER_VIEW (pm->priv->view);
  selected = peas_gtk_plugin_manager_view_get_selected_plugin (view);

  if (selected == info)
    update_button_sensitivity (pm, info);
}

static void
selection_changed_cb (PeasGtkPluginManager *pm)
{
  PeasGtkPluginManagerView *view;
  PeasPluginInfo *info;

  view = PEAS_GTK_PLUGIN_MANAGER_VIEW (pm->priv->view);
  info = peas_gtk_plugin_manager_view_get_selected_plugin (view);

  update_button_sensitivity (pm, info);
}

static void
populate_popup_cb (PeasGtkPluginManagerView *view,
                   GtkMenu                  *menu,
                   PeasGtkPluginManager     *pm)
{
  PeasPluginInfo *info;
  GtkWidget *item;

  info = peas_gtk_plugin_manager_view_get_selected_plugin (view);

  if (info == NULL)
    return;

  item = gtk_image_menu_item_new_from_stock (GTK_STOCK_PREFERENCES, NULL);
  g_signal_connect (item, "activate", G_CALLBACK (show_configure_cb), pm);
  gtk_widget_set_sensitive (item, plugin_is_configurable (pm, info));
  gtk_menu_shell_prepend (GTK_MENU_SHELL (menu), item);

  item = gtk_image_menu_item_new_from_stock (GTK_STOCK_ABOUT, NULL);
  g_signal_connect (item, "activate", G_CALLBACK (show_about_cb), pm);
  gtk_menu_shell_prepend (GTK_MENU_SHELL (menu), item);
}

static void
peas_gtk_plugin_manager_init (PeasGtkPluginManager *pm)
{
  GtkWidget *hbuttonbox;

  pm->priv = G_TYPE_INSTANCE_GET_PRIVATE (pm,
                                          PEAS_GTK_TYPE_PLUGIN_MANAGER,
                                          PeasGtkPluginManagerPrivate);

  /* If we are using a PeasGtkPluginManager, we know for sure we will be using
     libpeas-gtk, so let's load the typelib for it here. */
  g_irepository_require (g_irepository_get_default (),
                         "PeasGtk", "1.0", 0, NULL);

  gtk_box_set_spacing (GTK_BOX (pm), 6);
  gtk_orientable_set_orientation (GTK_ORIENTABLE (pm),
                                  GTK_ORIENTATION_VERTICAL);

  gtk_widget_push_composite_child ();

  pm->priv->sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (pm->priv->sw),
                                  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (pm->priv->sw),
                                       GTK_SHADOW_IN);
  gtk_box_pack_start (GTK_BOX (pm), pm->priv->sw, TRUE, TRUE, 0);

  hbuttonbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
  gtk_box_set_spacing (GTK_BOX (hbuttonbox), 6);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (hbuttonbox), GTK_BUTTONBOX_END);
  gtk_box_pack_start (GTK_BOX (pm), hbuttonbox, FALSE, FALSE, 0);

  pm->priv->about_button = gtk_button_new_from_stock (GTK_STOCK_ABOUT);
  gtk_container_add (GTK_CONTAINER (hbuttonbox), pm->priv->about_button);

  pm->priv->configure_button = gtk_button_new_from_stock (GTK_STOCK_PREFERENCES);
  gtk_container_add (GTK_CONTAINER (hbuttonbox), pm->priv->configure_button);

  gtk_widget_pop_composite_child ();

  /* setup a window of a sane size. */
  gtk_widget_set_size_request (GTK_WIDGET (pm->priv->sw), 270, 100);

  g_signal_connect (pm->priv->about_button,
                    "clicked",
                    G_CALLBACK (show_about_cb),
                    pm);
  g_signal_connect (pm->priv->configure_button,
                    "clicked",
                    G_CALLBACK (show_configure_cb),
                    pm);
}

static void
peas_gtk_plugin_manager_set_property (GObject      *object,
                                      guint         prop_id,
                                      const GValue *value,
                                      GParamSpec   *pspec)
{
  PeasGtkPluginManager *pm = PEAS_GTK_PLUGIN_MANAGER (object);

  switch (prop_id)
    {
    case PROP_ENGINE:
      pm->priv->engine = g_value_get_object (value);
      break;
    case PROP_VIEW:
      pm->priv->view = g_value_get_object (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
peas_gtk_plugin_manager_get_property (GObject    *object,
                                      guint       prop_id,
                                      GValue     *value,
                                      GParamSpec *pspec)
{
  PeasGtkPluginManager *pm = PEAS_GTK_PLUGIN_MANAGER (object);

  switch (prop_id)
    {
    case PROP_ENGINE:
      g_value_set_object (value, pm->priv->engine);
      break;
    case PROP_VIEW:
      g_value_set_object (value, peas_gtk_plugin_manager_get_view (pm));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
peas_gtk_plugin_manager_constructed (GObject *object)
{
  PeasGtkPluginManager *pm = PEAS_GTK_PLUGIN_MANAGER (object);
  GtkTreeSelection *selection;

  if (pm->priv->engine == NULL)
    pm->priv->engine = peas_engine_get_default ();

  g_object_ref (pm->priv->engine);

  /* When we create the manager, we always rescan the plugins directory
   * Must come after the view has connected to notify::plugin-list
   */
  peas_engine_rescan_plugins (pm->priv->engine);

  /* For the view to behave as expected, we must ensure it uses the same
   * engine as the manager itself.
   */
  if (pm->priv->view != NULL)
    {
      PeasEngine *engine;

      g_object_get (pm->priv->view,
                    "engine", &engine,
                    NULL);

      g_warn_if_fail (engine == pm->priv->engine);

      if (engine != pm->priv->engine)
        {
          g_object_unref (pm->priv->view);
          pm->priv->view = NULL;
        }

      g_object_unref (engine);
    }

  if (pm->priv->view == NULL)
    pm->priv->view = peas_gtk_plugin_manager_view_new (pm->priv->engine);

  gtk_widget_push_composite_child ();
  gtk_container_add (GTK_CONTAINER (pm->priv->sw), pm->priv->view);
  gtk_widget_pop_composite_child ();

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (pm->priv->view));

  g_signal_connect_swapped (selection,
                            "changed",
                            G_CALLBACK (selection_changed_cb),
                            pm);
  g_signal_connect_swapped (pm->priv->view,
                            "cursor-changed",
                            G_CALLBACK (selection_changed_cb),
                            pm);
  g_signal_connect (pm->priv->view,
                    "populate-popup",
                    G_CALLBACK (populate_popup_cb),
                    pm);
  g_signal_connect_after (pm->priv->engine,
                          "load-plugin",
                          G_CALLBACK (plugin_loaded_toggled_cb),
                          pm);
  g_signal_connect_after (pm->priv->engine,
                          "unload-plugin",
                          G_CALLBACK (plugin_loaded_toggled_cb),
                          pm);

  /* Update the button sensitivity */
  selection_changed_cb (pm);

  if (G_OBJECT_CLASS (peas_gtk_plugin_manager_parent_class)->constructed != NULL)
    G_OBJECT_CLASS (peas_gtk_plugin_manager_parent_class)->constructed (object);
}

static void
peas_gtk_plugin_manager_dispose (GObject *object)
{
  PeasGtkPluginManager *pm = PEAS_GTK_PLUGIN_MANAGER (object);

  if (pm->priv->engine != NULL)
    {
      g_signal_handlers_disconnect_by_func (pm->priv->engine,
                                            plugin_loaded_toggled_cb,
                                            pm);

      g_object_unref (pm->priv->engine);
      pm->priv->engine = NULL;
    }

  G_OBJECT_CLASS (peas_gtk_plugin_manager_parent_class)->dispose (object);
}

static void
peas_gtk_plugin_manager_class_init (PeasGtkPluginManagerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->set_property = peas_gtk_plugin_manager_set_property;
  object_class->get_property = peas_gtk_plugin_manager_get_property;
  object_class->constructed = peas_gtk_plugin_manager_constructed;
  object_class->dispose = peas_gtk_plugin_manager_dispose;

  /**
   * PeasGtkPluginManager:engine:
   *
   * The #PeasEngine this manager is attached to.
   */
  g_object_class_install_property (object_class,
                                   PROP_ENGINE,
                                   g_param_spec_object ("engine",
                                                        "engine",
                                                        "The PeasEngine this manager is attached to",
                                                        PEAS_TYPE_ENGINE,
                                                        G_PARAM_READWRITE |
                                                        G_PARAM_CONSTRUCT_ONLY |
                                                        G_PARAM_STATIC_STRINGS));

  /**
   * PeasGtkPluginManager:view:
   *
   * The #PeasGtkPluginManagerView shown in the #PeasGtkPluginManager.
   */
  g_object_class_install_property (object_class,
                                   PROP_VIEW,
                                   g_param_spec_object ("view",
                                                        "view",
                                                        "The view shown in the manager",
                                                        PEAS_GTK_TYPE_PLUGIN_MANAGER_VIEW,
                                                        G_PARAM_READWRITE |
                                                        G_PARAM_CONSTRUCT_ONLY |
                                                        G_PARAM_STATIC_STRINGS));

  g_type_class_add_private (object_class, sizeof (PeasGtkPluginManagerPrivate));
}

/**
 * peas_gtk_plugin_manager_new:
 * @engine: (allow-none): A #PeasEngine, or %NULL.
 *
 * Creates a new plugin manager for the given #PeasEngine.
 *
 * If @engine is %NULL, then the default engine will be used.
 *
 * Returns: the new #PeasGtkPluginManager.
 */
GtkWidget *
peas_gtk_plugin_manager_new (PeasEngine *engine)
{
  g_return_val_if_fail (engine == NULL || PEAS_IS_ENGINE (engine), NULL);

  return GTK_WIDGET (g_object_new (PEAS_GTK_TYPE_PLUGIN_MANAGER,
                                   "engine", engine,
                                   NULL));
}

/**
 * peas_gtk_plugin_manager_get_view:
 * @pm: A #PeasGtkPluginManager.
 *
 * Returns the #PeasGtkPluginManagerView of @pm.
 *
 * Returns: (transfer none): the #GtkTreeView of @pm.
 */
GtkWidget *
peas_gtk_plugin_manager_get_view (PeasGtkPluginManager *pm)
{
  g_return_val_if_fail (PEAS_GTK_IS_PLUGIN_MANAGER (pm), NULL);

  return pm->priv->view;
}
