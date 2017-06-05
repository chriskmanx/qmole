/*
 * plugin-manager-view.c
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

#include <glib.h>
#include <gtk/gtk.h>
#include <libpeas/peas.h>
#include <libpeas-gtk/peas-gtk.h>

#include "testing/testing.h"

typedef struct _TestFixture TestFixture;

struct _TestFixture {
  PeasEngine *engine;
  GtkTreeView *tree_view;
  PeasGtkPluginManagerView *view;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkListStore *store;
};

static void
notify_model_cb (GtkTreeView *view,
                 GParamSpec  *pspec,
                 TestFixture *fixture)
{
  fixture->model = gtk_tree_view_get_model (fixture->tree_view);

  if (GTK_IS_TREE_MODEL_FILTER (fixture->model))
    {
      GtkTreeModelFilter *filter = GTK_TREE_MODEL_FILTER (fixture->model);
      fixture->store = GTK_LIST_STORE (gtk_tree_model_filter_get_model (filter));
    }
  else
    {
      fixture->store = GTK_LIST_STORE (fixture->model);
    }
}

static void
test_setup (TestFixture   *fixture,
            gconstpointer  data)
{
  fixture->engine = testing_engine_new ();
  fixture->tree_view = GTK_TREE_VIEW (peas_gtk_plugin_manager_view_new (NULL));
  fixture->view = PEAS_GTK_PLUGIN_MANAGER_VIEW (fixture->tree_view);
  fixture->selection = gtk_tree_view_get_selection (fixture->tree_view);

  g_signal_connect (fixture->view,
                    "notify::model",
                    G_CALLBACK (notify_model_cb),
                    fixture);

  /* Set the model and store */
  g_object_notify (G_OBJECT (fixture->tree_view), "model");

  g_object_ref_sink (fixture->tree_view);
}

static void
test_teardown (TestFixture   *fixture,
               gconstpointer  data)
{
  gtk_widget_destroy (GTK_WIDGET (fixture->tree_view));
  g_object_unref (fixture->tree_view);

  testing_engine_free (fixture->engine);
}

static void
test_runner (TestFixture   *fixture,
             gconstpointer  data)
{
  ((void (*) (TestFixture *)) data) (fixture);
}

/* Based on code from peas-gtk-manager-view.h */
static void
convert_iter_to_child_iter (PeasGtkPluginManagerView *view,
                            GtkTreeIter              *iter)
{
  GtkTreeModel *model;
  GtkTreeIter child_iter;

  if (peas_gtk_plugin_manager_view_get_show_builtin (view))
    return;

  model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));

  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER (model),
                                                    &child_iter, iter);

  *iter = child_iter;
}

static gboolean
model_has_builtin (TestFixture *fixture)
{
  GtkTreeIter iter;
  gboolean found = FALSE;

  if (gtk_tree_model_get_iter_first (fixture->model, &iter))
    {
      do
        {
          if (peas_plugin_info_is_builtin (testing_get_plugin_info_for_iter (fixture->view, &iter)))
            found = TRUE;
        }
      while (!found && gtk_tree_model_iter_next (fixture->model, &iter));
    }

  return found;
}

static void
test_gtk_plugin_manager_view_selection (TestFixture *fixture)
{
  GtkTreeIter iter;
  PeasPluginInfo *info;

  info = peas_gtk_plugin_manager_view_get_selected_plugin (fixture->view);
  g_assert (info == NULL);

  gtk_tree_model_get_iter_first (fixture->model, &iter);

  gtk_tree_selection_select_iter (fixture->selection, &iter);

  info = peas_gtk_plugin_manager_view_get_selected_plugin (fixture->view);
  g_assert (info != NULL);
}

static void
test_gtk_plugin_manager_view_show_builtin (TestFixture *fixture)
{
  peas_gtk_plugin_manager_view_set_show_builtin (fixture->view, TRUE);

  g_assert (model_has_builtin (fixture));
}

static void
test_gtk_plugin_manager_view_hide_builtin (TestFixture *fixture)
{
  /* Should this be here given its already the default? */
  peas_gtk_plugin_manager_view_set_show_builtin (fixture->view, FALSE);

  g_assert (!model_has_builtin (fixture));
}

static void
test_gtk_plugin_manager_view_reload (TestFixture *fixture)
{
  GtkTreeIter iter;
  PeasPluginInfo *removed_info, *selected_info;

  g_assert (gtk_tree_model_get_iter_first (fixture->model, &iter));
  gtk_tree_selection_select_iter (fixture->selection, &iter);
  removed_info = testing_get_plugin_info_for_iter (fixture->view, &iter);

  convert_iter_to_child_iter (fixture->view, &iter);
  gtk_list_store_remove (fixture->store, &iter);

  g_assert (gtk_tree_model_get_iter_first (fixture->model, &iter));
  gtk_tree_selection_select_iter (fixture->selection, &iter);
  selected_info = testing_get_plugin_info_for_iter (fixture->view, &iter);

  g_object_notify (G_OBJECT (fixture->engine), "plugin-list");

  g_assert (gtk_tree_selection_get_selected (fixture->selection, NULL, &iter));
  g_assert (testing_get_plugin_info_for_iter (fixture->view, &iter) == selected_info);

  g_assert (gtk_tree_model_get_iter_first (fixture->model, &iter));
  g_assert (testing_get_plugin_info_for_iter (fixture->view, &iter) == removed_info);
}

static void
test_gtk_plugin_manager_view_enable_plugin (TestFixture *fixture)
{
  GtkTreeIter iter;
  GtkTreePath *path;
  GtkTreeViewColumn *column;
  PeasPluginInfo *info;

  g_assert (gtk_tree_model_get_iter_first (fixture->model, &iter));
  info = testing_get_plugin_info_for_iter (fixture->view, &iter);
  path = gtk_tree_model_get_path (fixture->model, &iter);
  column = gtk_tree_view_get_column (fixture->tree_view, 0);

  g_assert (!peas_plugin_info_is_loaded (info));
  gtk_tree_view_row_activated (fixture->tree_view, path, column);
  g_assert (peas_plugin_info_is_loaded (info));

  gtk_tree_path_free (path);
}

static void
test_gtk_plugin_manager_view_enable_builtin_plugin (TestFixture *fixture)
{
  GtkTreeIter iter;
  GtkTreePath *path;
  GtkTreeViewColumn *column;
  PeasPluginInfo *info;

  peas_gtk_plugin_manager_view_set_show_builtin (fixture->view, TRUE);

  info = peas_engine_get_plugin_info (fixture->engine, "builtin");

  testing_get_iter_for_plugin_info (fixture->view, info, &iter);

  path = gtk_tree_model_get_path (fixture->model, &iter);
  column = gtk_tree_view_get_column (fixture->tree_view, 0);

  g_assert (!peas_plugin_info_is_loaded (info));
  gtk_tree_view_row_activated (fixture->tree_view, path, column);
  g_assert (!peas_plugin_info_is_loaded (info));

  gtk_tree_path_free (path);
}

int
main (int    argc,
      char **argv)
{
  gtk_test_init (&argc, &argv, NULL);

  g_type_init ();

#define TEST(path, ftest) \
  g_test_add ("/gtk/plugin-manager-view/" path, TestFixture, \
              (gpointer) test_gtk_plugin_manager_view_##ftest, \
              test_setup, test_runner, test_teardown)

  TEST ("selection", selection);

  TEST ("show-builtin", show_builtin);
  TEST ("hide-builtin", hide_builtin);

  TEST ("reload", reload);

  TEST ("enable-plugin", enable_plugin);
  TEST ("enable-builtin-plugin", enable_builtin_plugin);

#undef TEST

  return testing_run_tests ();
}
