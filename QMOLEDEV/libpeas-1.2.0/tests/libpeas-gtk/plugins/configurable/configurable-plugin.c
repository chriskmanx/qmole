/*
 * configurable-plugin.c
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
#include <glib-object.h>
#include <gmodule.h>

#include <libpeas/peas.h>
#include <libpeas-gtk/peas-gtk.h>

#include "configurable-plugin.h"

static void peas_gtk_configurable_iface_init (PeasGtkConfigurableInterface *iface);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (TestingConfigurablePlugin,
                                testing_configurable_plugin,
                                PEAS_TYPE_EXTENSION_BASE,
                                0,
                                G_IMPLEMENT_INTERFACE_DYNAMIC (PEAS_GTK_TYPE_CONFIGURABLE,
                                                               peas_gtk_configurable_iface_init))

static void
testing_configurable_plugin_init (TestingConfigurablePlugin *configurable)
{
}

static void
testing_configurable_plugin_class_init (TestingConfigurablePluginClass *klass)
{
}

static GtkWidget *
testing_configurable_plugin_create_configure_widget (PeasGtkConfigurable *configurable)
{
  return gtk_label_new ("Hello, World!");
}

static void
peas_gtk_configurable_iface_init (PeasGtkConfigurableInterface *iface)
{
  iface->create_configure_widget = testing_configurable_plugin_create_configure_widget;
}

static void
testing_configurable_plugin_class_finalize (TestingConfigurablePluginClass *klass)
{
}

G_MODULE_EXPORT void
peas_register_types (PeasObjectModule *module)
{
  testing_configurable_plugin_register_type (G_TYPE_MODULE (module));

  peas_object_module_register_extension_type (module,
                                              PEAS_GTK_TYPE_CONFIGURABLE,
                                              TESTING_TYPE_CONFIGURABLE_PLUGIN);
}
