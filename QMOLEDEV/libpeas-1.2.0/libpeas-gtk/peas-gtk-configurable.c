/*
 * peas-gtk-configurable.c
 * This file is part of libpeas
 *
 * Copyright (C) 2009-2010 Steve Frécinaux
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

#include "peas-gtk-configurable.h"

/**
 * SECTION:peas-gtk-configurable
 * @short_description: Interface for providing a plugin configuration UI.
 *
 * The #PeasGtkConfigurable interface will allow a plugin to provide a
 * graphical interface for the user to configure the plugin through the
 * #PeasGtkPluginManager: the #PeasGtkPluginManager will make its
 * “Configure Plugin” button active when the selected plugin implements
 * the #PeasGtkConfigurable interface.
 *
 * To allow plugin configuration from the #PeasGtkPluginManager, the
 * plugin writer will just need to implement the
 * PeasGtkConfigurableInterface.create_configure_widget() method.
 **/

G_DEFINE_INTERFACE(PeasGtkConfigurable, peas_gtk_configurable, G_TYPE_OBJECT)

static void
peas_gtk_configurable_default_init (PeasGtkConfigurableInterface *iface)
{
}

/**
 * peas_gtk_configurable_create_configure_widget:
 * @configurable: A #PeasGtkConfigurable
 *
 * Creates the configure widget for the plugin. The returned widget
 * should allow configuring all the relevant aspects of the plugin, and should
 * allow instant-apply, as promoted by the Gnome Human Interface Guidelines.
 *
 * #PeasGtkPluginManager will embed the returned widget into a dialog box,
 * but you shouldn't take this behaviour for granted as other implementations
 * of a plugin manager UI might do otherwise.
 *
 * This method should always return a valid #GtkWidget instance, never %NULL.
 *
 * Returns: (transfer full): A #GtkWidget used for configuration.
 */
GtkWidget *
peas_gtk_configurable_create_configure_widget (PeasGtkConfigurable *configurable)
{
  PeasGtkConfigurableInterface *iface;

  g_return_val_if_fail (PEAS_GTK_IS_CONFIGURABLE (configurable), FALSE);

  iface = PEAS_GTK_CONFIGURABLE_GET_IFACE (configurable);

  if (G_LIKELY (iface->create_configure_widget != NULL))
    return iface->create_configure_widget (configurable);

  /* Default implementation */
  return NULL;
}
