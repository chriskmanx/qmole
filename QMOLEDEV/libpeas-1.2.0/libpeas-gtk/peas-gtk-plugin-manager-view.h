/*
 * peas-plugin-manager-view.h
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

#ifndef __PEAS_GTK_PLUGIN_MANAGER_VIEW_H__
#define __PEAS_GTK_PLUGIN_MANAGER_VIEW_H__

#include <gtk/gtk.h>
#include <libpeas/peas-engine.h>

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define PEAS_GTK_TYPE_PLUGIN_MANAGER_VIEW             (peas_gtk_plugin_manager_view_get_type())
#define PEAS_GTK_PLUGIN_MANAGER_VIEW(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj), PEAS_GTK_TYPE_PLUGIN_MANAGER_VIEW, PeasGtkPluginManagerView))
#define PEAS_GTK_PLUGIN_MANAGER_VIEW_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass), PEAS_GTK_TYPE_PLUGIN_MANAGER_VIEW, PeasGtkPluginManagerViewClass))
#define PEAS_GTK_IS_PLUGIN_MANAGER_VIEW(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj), PEAS_GTK_TYPE_PLUGIN_MANAGER_VIEW))
#define PEAS_GTK_IS_PLUGIN_MANAGER_VIEW_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass), PEAS_GTK_TYPE_PLUGIN_MANAGER_VIEW))
#define PEAS_GTK_PLUGIN_MANAGER_VIEW_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj), PEAS_GTK_TYPE_PLUGIN_MANAGER_VIEW, PeasGtkPluginManagerViewClass))

typedef struct _PeasGtkPluginManagerView        PeasGtkPluginManagerView;
typedef struct _PeasGtkPluginManagerViewClass   PeasGtkPluginManagerViewClass;
typedef struct _PeasGtkPluginManagerViewPrivate PeasGtkPluginManagerViewPrivate;

/**
 * PeasGtkPluginManagerView:
 *
 * The #PeasGtkPluginManagerView structure contains only private data
 * and should only be accessed using the provided API.
 */
struct _PeasGtkPluginManagerView {
  GtkTreeView parent;

  /*< private > */
  PeasGtkPluginManagerViewPrivate *priv;
};

/**
 * PeasGtkPluginManagerViewClass:
 * @parent_class: The parent class.
 * @populate_popup: Signal class handler for the
 *                  #PeasGtkPluginManagerView::populate-popup signal.
 *
 * The class structure for #PeasGtkPluginManagerView.
 */
struct _PeasGtkPluginManagerViewClass {
  GtkTreeViewClass parent_class;

  void  (*populate_popup)   (PeasGtkPluginManagerView *view,
                             GtkMenu                  *menu);

  /*< private >*/
  gpointer padding[8];
};

GType           peas_gtk_plugin_manager_view_get_type             (void) G_GNUC_CONST;
GtkWidget      *peas_gtk_plugin_manager_view_new                  (PeasEngine               *engine);

#ifndef PEAS_DISABLE_DEPRECATED
void            peas_gtk_plugin_manager_view_set_show_builtin     (PeasGtkPluginManagerView *view,
                                                                   gboolean                  show_builtin);
gboolean        peas_gtk_plugin_manager_view_get_show_builtin     (PeasGtkPluginManagerView *view);
#endif

void            peas_gtk_plugin_manager_view_set_selected_plugin  (PeasGtkPluginManagerView *view,
                                                                   PeasPluginInfo           *info);
PeasPluginInfo *peas_gtk_plugin_manager_view_get_selected_plugin  (PeasGtkPluginManagerView *view);

G_END_DECLS

#endif /* __PEAS_GTK_PLUGIN_MANAGER_VIEW_H__  */
