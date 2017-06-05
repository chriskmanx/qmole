/*
 * testing.c
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
#include <girepository.h>
#include <testing-util.h>

#include "testing.h"

void
testing_init (void)
{
  GError *error = NULL;
  static gboolean initialized = FALSE;

  if (initialized)
    return;

  testing_util_init ();

  g_irepository_prepend_search_path (BUILDDIR "/libpeas-gtk");

  g_irepository_require (g_irepository_get_default (), "PeasGtk", "1.0", 0, &error);
  g_assert_no_error (error);

  initialized = TRUE;
}

PeasEngine *
testing_engine_new (void)
{
  PeasEngine *engine;

  testing_init ();

  /* Must be after requiring typelibs */
  engine = testing_util_engine_new ();

  peas_engine_add_search_path (engine, BUILDDIR "/tests/libpeas-gtk/plugins",
                                       SRCDIR   "/tests/libpeas-gtk/plugins");

  return engine;
}

PeasPluginInfo *
testing_get_plugin_info_for_iter (PeasGtkPluginManagerView *view,
                                  GtkTreeIter              *iter)
{
  GtkTreeSelection *selection;
  GtkTreeIter selected_iter;
  gboolean had_selection;
  PeasPluginInfo *info;

  /* This is annoying but the only way to get the plugin info
   * is to ask the view for the info of the selected plugin
   */

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));

  had_selection = gtk_tree_selection_get_selected (selection,
                                                   NULL, &selected_iter);

  gtk_tree_selection_select_iter (selection, iter);

  info = peas_gtk_plugin_manager_view_get_selected_plugin (view);

  if (had_selection)
    gtk_tree_selection_select_iter (selection, &selected_iter);

  return info;
}

gboolean
testing_get_iter_for_plugin_info (PeasGtkPluginManagerView *view,
                                  PeasPluginInfo           *info,
                                  GtkTreeIter              *iter)
{
  GtkTreeModel *model;
  GtkTreeIter pos_iter;

  model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));

  g_assert (gtk_tree_model_get_iter_first (model, &pos_iter));

  do
    {
      if (testing_get_plugin_info_for_iter (view, &pos_iter) == info)
        {
          if (iter != NULL)
            *iter = pos_iter;

          return TRUE;
        }
    }
  while (gtk_tree_model_iter_next (model, &pos_iter));

  return FALSE;
}

static gboolean
delete_event_cb (GtkWidget *window,
                 GdkEvent  *event,
                 GtkWidget *widget)
{
  gtk_main_quit ();

  return TRUE;
}

/* This takes a gpointer for ease of debugging */
void
testing_show_widget (gpointer widget)
{
  GtkWidget *window;

  g_assert (GTK_IS_WIDGET (widget));

  widget = gtk_widget_get_toplevel (GTK_WIDGET (widget));

  if (gtk_widget_is_toplevel (GTK_WIDGET (widget)))
    window = widget;
  else
    {
      window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      gtk_container_add (GTK_CONTAINER (window), GTK_WIDGET (widget));
    }

  gtk_window_set_default_size (GTK_WINDOW (window), 200, 100);

  gtk_window_set_has_resize_grip (GTK_WINDOW (window), FALSE);

  gtk_widget_show_all (window);

  g_signal_connect (window,
                    "delete-event",
                    G_CALLBACK (delete_event_cb),
                    widget);

  gtk_main ();
}
