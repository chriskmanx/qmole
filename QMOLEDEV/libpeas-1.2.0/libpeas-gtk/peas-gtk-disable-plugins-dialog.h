/*
 * peas-gtk-disable-plugins-dialog.h
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

#ifndef __PEAS_GTK_DISABLE_PLUGINS_DIALOG_H__
#define __PEAS_GTK_DISABLE_PLUGINS_DIALOG_H__

#include <gtk/gtk.h>
#include <libpeas/peas-plugin-info.h>

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define PEAS_GTK_TYPE_DISABLE_PLUGINS_DIALOG            (peas_gtk_disable_plugins_dialog_get_type())
#define PEAS_GTK_DISABLE_PLUGINS_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), PEAS_GTK_TYPE_DISABLE_PLUGINS_DIALOG, PeasGtkDisablePluginsDialog))
#define PEAS_GTK_DISABLE_PLUGINS_DIALOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), PEAS_GTK_TYPE_DISABLE_PLUGINS_DIALOG, PeasGtkDisablePluginsDialogClass))
#define PEAS_GTK_IS_DISABLE_PLUGINS_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), PEAS_GTK_TYPE_DISABLE_PLUGINS_DIALOG))
#define PEAS_GTK_IS_DISABLE_PLUGINS_DIALOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), PEAS_GTK_TYPE_DISABLE_PLUGINS_DIALOG))
#define PEAS_GTK_DISABLE_PLUGINS_DIALOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), PEAS_GTK_TYPE_DISABLE_PLUGINS_DIALOG, PeasGtkDisablePluginsDialogClass))

typedef struct _PeasGtkDisablePluginsDialog         PeasGtkDisablePluginsDialog;
typedef struct _PeasGtkDisablePluginsDialogClass    PeasGtkDisablePluginsDialogClass;
typedef struct _PeasGtkDisablePluginsDialogPrivate  PeasGtkDisablePluginsDialogPrivate;

struct _PeasGtkDisablePluginsDialog {
  GtkMessageDialog parent;

  /*< private > */
  PeasGtkDisablePluginsDialogPrivate *priv;
};

struct _PeasGtkDisablePluginsDialogClass {
  GtkMessageDialogClass parent_class;
};

GType      peas_gtk_disable_plugins_dialog_get_type (void) G_GNUC_CONST;

GtkWidget *peas_gtk_disable_plugins_dialog_new      (GtkWindow      *parent,
                                                     PeasPluginInfo *info,
                                                     GList          *dep_plugins);

G_END_DECLS

#endif /* __PEAS_GTK_DISABLE_PLUGINS_DIALOG_H__  */
