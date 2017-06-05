/*
 * second-time.c
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

#include "second-time.h"

static void peas_activatable_iface_init     (PeasActivatableInterface    *iface);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (PeasDemoSecondTime,
                                peasdemo_second_time,
                                PEAS_TYPE_EXTENSION_BASE,
                                0,
                                G_IMPLEMENT_INTERFACE_DYNAMIC (PEAS_TYPE_ACTIVATABLE,
                                                               peas_activatable_iface_init))

enum {
  PROP_0,
  PROP_OBJECT
};

static void
peasdemo_second_time_set_property (GObject      *object,
                                   guint         prop_id,
                                   const GValue *value,
                                   GParamSpec   *pspec)
{
  PeasDemoSecondTime *plugin = PEASDEMO_SECOND_TIME (object);

  switch (prop_id)
    {
    case PROP_OBJECT:
      plugin->window = GTK_WIDGET (g_value_dup_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
peasdemo_second_time_get_property (GObject    *object,
                                   guint       prop_id,
                                   GValue     *value,
                                   GParamSpec *pspec)
{
  PeasDemoSecondTime *plugin = PEASDEMO_SECOND_TIME (object);

  switch (prop_id)
    {
    case PROP_OBJECT:
      g_value_set_object (value, plugin->window);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}


static void
peasdemo_second_time_init (PeasDemoSecondTime *plugin)
{
  g_debug ("%s", G_STRFUNC);
}

static void
peasdemo_second_time_finalize (GObject *object)
{
  PeasDemoSecondTime *plugin = PEASDEMO_SECOND_TIME (object);

  g_debug ("%s", G_STRFUNC);

  g_object_unref (plugin->label);
  g_object_unref (plugin->window);

  G_OBJECT_CLASS (peasdemo_second_time_parent_class)->finalize (object);
}

static GtkBox *
get_box (GtkWidget *window)
{
  return GTK_BOX (gtk_bin_get_child (GTK_BIN (window)));
}

static void
peasdemo_second_time_activate (PeasActivatable *activatable)
{
  PeasDemoSecondTime *plugin = PEASDEMO_SECOND_TIME (activatable);

  g_debug ("%s", G_STRFUNC);

  plugin->label = gtk_label_new ("A second time!");
  gtk_box_pack_start (get_box (plugin->window), plugin->label, 1, 1, 0);
  gtk_widget_show (plugin->label);
  g_object_ref (plugin->label);
}

static void
peasdemo_second_time_deactivate (PeasActivatable *activatable)
{
  PeasDemoSecondTime *plugin = PEASDEMO_SECOND_TIME (activatable);

  g_debug ("%s", G_STRFUNC);

  gtk_container_remove (GTK_CONTAINER (get_box (plugin->window)), plugin->label);
}

static void
peasdemo_second_time_class_init (PeasDemoSecondTimeClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->set_property = peasdemo_second_time_set_property;
  object_class->get_property = peasdemo_second_time_get_property;
  object_class->finalize = peasdemo_second_time_finalize;

  g_object_class_override_property (object_class, PROP_OBJECT, "object");
}

static void
peas_activatable_iface_init (PeasActivatableInterface *iface)
{
  iface->activate = peasdemo_second_time_activate;
  iface->deactivate = peasdemo_second_time_deactivate;
}

static void
peasdemo_second_time_class_finalize (PeasDemoSecondTimeClass *klass)
{
}

G_MODULE_EXPORT void
peas_register_types (PeasObjectModule *module)
{
  peasdemo_second_time_register_type (G_TYPE_MODULE (module));

  peas_object_module_register_extension_type (module,
                                              PEAS_TYPE_ACTIVATABLE,
                                              PEASDEMO_TYPE_SECOND_TIME);
}
