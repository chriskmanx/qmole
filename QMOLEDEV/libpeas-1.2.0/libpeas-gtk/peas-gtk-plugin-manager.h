/*
 * peas-gtk-plugin-manager.h
 * This file is part of libpeas
 *
 * Copyright (C) 2005-2009 Paolo Maggi, Paolo Borelli
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

#ifndef __PEAS_GTK_PLUGIN_MANAGER_H__
#define __PEAS_GTK_PLUGIN_MANAGER_H__

#include <gtk/gtk.h>
#include <libpeas/peas-engine.h>

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define PEAS_GTK_TYPE_PLUGIN_MANAGER            (peas_gtk_plugin_manager_get_type())
#define PEAS_GTK_PLUGIN_MANAGER(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), PEAS_GTK_TYPE_PLUGIN_MANAGER, PeasGtkPluginManager))
#define PEAS_GTK_PLUGIN_MANAGER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), PEAS_GTK_TYPE_PLUGIN_MANAGER, PeasGtkPluginManagerClass))
#define PEAS_GTK_IS_PLUGIN_MANAGER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), PEAS_GTK_TYPE_PLUGIN_MANAGER))
#define PEAS_GTK_IS_PLUGIN_MANAGER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), PEAS_GTK_TYPE_PLUGIN_MANAGER))
#define PEAS_GTK_PLUGIN_MANAGER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), PEAS_GTK_TYPE_PLUGIN_MANAGER, PeasGtkPluginManagerClass))

typedef struct _PeasGtkPluginManager        PeasGtkPluginManager;
typedef struct _PeasGtkPluginManagerClass   PeasGtkPluginManagerClass;
typedef struct _PeasGtkPluginManagerPrivate PeasGtkPluginManagerPrivate;

/**
 * PeasGtkPluginManager:
 *
 * The #PeasGtkPluginManager structure contains only private data
 * and should only be accessed using the provided API.
 */
struct _PeasGtkPluginManager
{
  GtkBox box;

  /*< private > */
  PeasGtkPluginManagerPrivate *priv;
};

/**
 * PeasGtkPluginManagerClass:
 * @parent_class: The parent class.
 *
 * The class structure for #PeasGtkPluginManager.
 */
struct _PeasGtkPluginManagerClass
{
  GtkBoxClass parent_class;

  /*< private >*/
  gpointer padding[8];
};

GType       peas_gtk_plugin_manager_get_type  (void)  G_GNUC_CONST;
GtkWidget  *peas_gtk_plugin_manager_new       (PeasEngine           *engine);

GtkWidget  *peas_gtk_plugin_manager_get_view  (PeasGtkPluginManager *pm);

G_END_DECLS

#endif /* __PEAS_GTK_PLUGIN_MANAGER_H__  */
