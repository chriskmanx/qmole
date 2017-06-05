/*
 * peas-gtk-configurable.h
 * This file is part of libpeas
 *
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

#ifndef __PEAS_GTK_CONFIGURABLE_H__
#define __PEAS_GTK_CONFIGURABLE_H__

#include <gtk/gtk.h>

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define PEAS_GTK_TYPE_CONFIGURABLE            (peas_gtk_configurable_get_type ())
#define PEAS_GTK_CONFIGURABLE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), PEAS_GTK_TYPE_CONFIGURABLE, PeasGtkConfigurable))
#define PEAS_GTK_CONFIGURABLE_IFACE(obj)      (G_TYPE_CHECK_CLASS_CAST ((obj), PEAS_GTK_TYPE_CONFIGURABLE, PeasGtkConfigurableInterface))
#define PEAS_GTK_IS_CONFIGURABLE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), PEAS_GTK_TYPE_CONFIGURABLE))
#define PEAS_GTK_CONFIGURABLE_GET_IFACE(obj)  (G_TYPE_INSTANCE_GET_INTERFACE ((obj), PEAS_GTK_TYPE_CONFIGURABLE, PeasGtkConfigurableInterface))

/**
 * PeasGtkConfigurable:
 *
 * Interface for configurable plugins.
 */
typedef struct _PeasGtkConfigurable           PeasGtkConfigurable; /* dummy typedef */
typedef struct _PeasGtkConfigurableInterface  PeasGtkConfigurableInterface;

/**
 * PeasGtkConfigurableInterface:
 * @g_iface: The parent interface.
 * @create_configure_widget: Creates the configure widget for the plugin.
 *
 * Provides an interface for configurable plugins.
 */
struct _PeasGtkConfigurableInterface
{
  GTypeInterface g_iface;

  GtkWidget  *(*create_configure_widget)  (PeasGtkConfigurable  *configurable);
};

GType       peas_gtk_configurable_get_type                (void)  G_GNUC_CONST;
GtkWidget  *peas_gtk_configurable_create_configure_widget (PeasGtkConfigurable  *configurable);

G_END_DECLS

#endif /* __PEAS_GTK_PLUGIN_MANAGER_H__  */
