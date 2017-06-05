/*
 * peas-plugin-manager-store.h
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

#ifndef __PEAS_GTK_PLUGIN_MANAGER_STORE_H__
#define __PEAS_GTK_PLUGIN_MANAGER_STORE_H__

#include <gtk/gtk.h>
#include <libpeas/peas-engine.h>
#include <libpeas/peas-plugin-info.h>

G_BEGIN_DECLS

typedef enum {
  PEAS_GTK_PLUGIN_MANAGER_STORE_ENABLED_COLUMN = 0,
  PEAS_GTK_PLUGIN_MANAGER_STORE_CAN_ENABLE_COLUMN,
  PEAS_GTK_PLUGIN_MANAGER_STORE_ICON_PIXBUF_COLUMN,
  PEAS_GTK_PLUGIN_MANAGER_STORE_ICON_NAME_COLUMN,
  PEAS_GTK_PLUGIN_MANAGER_STORE_ICON_VISIBLE_COLUMN,
  PEAS_GTK_PLUGIN_MANAGER_STORE_INFO_COLUMN,
  PEAS_GTK_PLUGIN_MANAGER_STORE_INFO_SENSITIVE_COLUMN,
  PEAS_GTK_PLUGIN_MANAGER_STORE_PLUGIN_COLUMN,
  PEAS_GTK_PLUGIN_MANAGER_STORE_N_COLUMNS
} PeasGtkPluginManagerStoreColumns;

/*
 * Type checking and casting macros
 */
#define PEAS_GTK_TYPE_PLUGIN_MANAGER_STORE            (peas_gtk_plugin_manager_store_get_type())
#define PEAS_GTK_PLUGIN_MANAGER_STORE(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), PEAS_GTK_TYPE_PLUGIN_MANAGER_STORE, PeasGtkPluginManagerStore))
#define PEAS_GTK_PLUGIN_MANAGER_STORE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), PEAS_GTK_TYPE_PLUGIN_MANAGER_STORE, PeasGtkPluginManagerStoreClass))
#define PEAS_GTK_IS_PLUGIN_MANAGER_STORE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), PEAS_GTK_TYPE_PLUGIN_MANAGER_STORE))
#define PEAS_GTK_IS_PLUGIN_MANAGER_STORE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), PEAS_GTK_TYPE_PLUGIN_MANAGER_STORE))
#define PEAS_GTK_PLUGIN_MANAGER_STORE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), PEAS_GTK_TYPE_PLUGIN_MANAGER_STORE, PeasGtkPluginManagerStoreClass))

typedef struct _PeasGtkPluginManagerStore         PeasGtkPluginManagerStore;
typedef struct _PeasGtkPluginManagerStoreClass    PeasGtkPluginManagerStoreClass;
typedef struct _PeasGtkPluginManagerStorePrivate  PeasGtkPluginManagerStorePrivate;

struct _PeasGtkPluginManagerStore {
  GtkListStore parent;

  /*< private > */
  PeasGtkPluginManagerStorePrivate *priv;
};

struct _PeasGtkPluginManagerStoreClass {
  GtkListStoreClass parent_class;
};

GType                       peas_gtk_plugin_manager_store_get_type              (void) G_GNUC_CONST;
PeasGtkPluginManagerStore  *peas_gtk_plugin_manager_store_new                   (PeasEngine                *engine);

void                        peas_gtk_plugin_manager_store_reload                (PeasGtkPluginManagerStore *store);

void                        peas_gtk_plugin_manager_store_set_enabled           (PeasGtkPluginManagerStore *store,
                                                                                 GtkTreeIter               *iter,
                                                                                 gboolean                   enabled);
gboolean                    peas_gtk_plugin_manager_store_get_enabled           (PeasGtkPluginManagerStore *store,
                                                                                 GtkTreeIter               *iter);
void                        peas_gtk_plugin_manager_store_set_all_enabled       (PeasGtkPluginManagerStore *store,
                                                                                 gboolean                  enabled);
void                        peas_gtk_plugin_manager_store_toggle_enabled        (PeasGtkPluginManagerStore *store,
                                                                                 GtkTreeIter               *iter);

gboolean                    peas_gtk_plugin_manager_store_can_enable            (PeasGtkPluginManagerStore *store,
                                                                                 GtkTreeIter               *iter);

PeasPluginInfo             *peas_gtk_plugin_manager_store_get_plugin            (PeasGtkPluginManagerStore *store,
                                                                                 GtkTreeIter               *iter);

gboolean                    peas_gtk_plugin_manager_store_get_iter_from_plugin  (PeasGtkPluginManagerStore *store,
                                                                                 GtkTreeIter               *iter,
                                                                                 const PeasPluginInfo      *info);
G_END_DECLS

#endif /* __PEAS_GTK_PLUGIN_MANAGER_STORE_H__  */
