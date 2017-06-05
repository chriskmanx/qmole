/*
 * peasdemo-hello-world-configurable.c
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

#include <glib.h>
#include <glib-object.h>
#include <gmodule.h>
#include <gtk/gtk.h>

#include <libpeas/peas.h>
#include <libpeas-gtk/peas-gtk.h>

#include "peasdemo-hello-world-configurable.h"

static void peas_gtk_configurable_iface_init (PeasGtkConfigurableInterface *iface);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (PeasDemoHelloWorldConfigurable,
                                peasdemo_hello_world_configurable,
                                PEAS_TYPE_EXTENSION_BASE,
                                0,
                                G_IMPLEMENT_INTERFACE_DYNAMIC (PEAS_GTK_TYPE_CONFIGURABLE,
                                                               peas_gtk_configurable_iface_init))

static void
peasdemo_hello_world_configurable_init (PeasDemoHelloWorldConfigurable *plugin)
{
  g_debug ("%s", G_STRFUNC);
}

static GtkWidget *
peasdemo_hello_world_configurable_create_configure_widget (PeasGtkConfigurable *configurable)
{
  g_debug ("%s", G_STRFUNC);

  return gtk_label_new ("This is a configuration dialog for the HelloWorld plugin.");
}

static void
peasdemo_hello_world_configurable_class_init (PeasDemoHelloWorldConfigurableClass *klass)
{
}

static void
peas_gtk_configurable_iface_init (PeasGtkConfigurableInterface *iface)
{
  iface->create_configure_widget = peasdemo_hello_world_configurable_create_configure_widget;
}

static void
peasdemo_hello_world_configurable_class_finalize (PeasDemoHelloWorldConfigurableClass *klass)
{
}

void
peasdemo_hello_world_configurable_register (GTypeModule *module)
{
  peasdemo_hello_world_configurable_register_type (module);
}
