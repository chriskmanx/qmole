/*
 * builtin-plugin.c
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

#include "builtin-plugin.h"
#include "builtin-configurable.h"

struct _TestingBuiltinPluginPrivate {
  GObject *object;
};

static void peas_activatable_iface_init (PeasActivatableInterface *iface);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (TestingBuiltinPlugin,
                                testing_builtin_plugin,
                                PEAS_TYPE_EXTENSION_BASE,
                                0,
                                G_IMPLEMENT_INTERFACE_DYNAMIC (PEAS_TYPE_ACTIVATABLE,
                                                               peas_activatable_iface_init))

enum {
  PROP_0,
  PROP_OBJECT
};

static void
testing_builtin_plugin_set_property (GObject      *object,
                                     guint         prop_id,
                                     const GValue *value,
                                     GParamSpec   *pspec)
{
  TestingBuiltinPlugin *plugin = TESTING_BUILTIN_PLUGIN (object);

  switch (prop_id)
    {
    case PROP_OBJECT:
      plugin->priv->object = g_value_dup_object (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
testing_builtin_plugin_get_property (GObject    *object,
                                     guint       prop_id,
                                     GValue     *value,
                                     GParamSpec *pspec)
{
  TestingBuiltinPlugin *plugin = TESTING_BUILTIN_PLUGIN (object);

  switch (prop_id)
    {
    case PROP_OBJECT:
      g_value_set_object (value, plugin->priv->object);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
testing_builtin_plugin_init (TestingBuiltinPlugin *plugin)
{
  plugin->priv = G_TYPE_INSTANCE_GET_PRIVATE (plugin,
                                              TESTING_TYPE_BUILTIN_PLUGIN,
                                              TestingBuiltinPluginPrivate);
}

static void
testing_builtin_plugin_activate (PeasActivatable *activatable)
{
}

static void
testing_builtin_plugin_deactivate (PeasActivatable *activatable)
{
}

static void
testing_builtin_plugin_class_init (TestingBuiltinPluginClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->set_property = testing_builtin_plugin_set_property;
  object_class->get_property = testing_builtin_plugin_get_property;

  g_object_class_override_property (object_class, PROP_OBJECT, "object");

  g_type_class_add_private (klass, sizeof (TestingBuiltinPluginPrivate));
}

static void
peas_activatable_iface_init (PeasActivatableInterface *iface)
{
  iface->activate = testing_builtin_plugin_activate;
  iface->deactivate = testing_builtin_plugin_deactivate;
}

static void
testing_builtin_plugin_class_finalize (TestingBuiltinPluginClass *klass)
{
}

G_MODULE_EXPORT void
peas_register_types (PeasObjectModule *module)
{
  testing_builtin_plugin_register_type (G_TYPE_MODULE (module));
  testing_builtin_configurable_register (G_TYPE_MODULE (module));

  peas_object_module_register_extension_type (module,
                                              PEAS_TYPE_ACTIVATABLE,
                                              TESTING_TYPE_BUILTIN_PLUGIN);
  peas_object_module_register_extension_type (module,
                                              PEAS_GTK_TYPE_CONFIGURABLE,
                                              TESTING_TYPE_BUILTIN_CONFIGURABLE);
}
