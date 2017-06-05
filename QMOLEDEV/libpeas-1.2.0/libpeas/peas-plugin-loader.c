/*
 * peas-plugin-loader.c
 * This file is part of libpeas
 *
 * Copyright (C) 2008 - Jesse van den Kieboom
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

#include "peas-plugin-loader.h"

G_DEFINE_ABSTRACT_TYPE (PeasPluginLoader, peas_plugin_loader, G_TYPE_OBJECT);

static void
peas_plugin_loader_init (PeasPluginLoader *loader)
{
}

static void
peas_plugin_loader_class_init (PeasPluginLoaderClass *klass)
{
}

gboolean
peas_plugin_loader_initialize (PeasPluginLoader *loader)
{
  PeasPluginLoaderClass *klass;

  g_return_val_if_fail (PEAS_IS_PLUGIN_LOADER (loader), FALSE);

  klass = PEAS_PLUGIN_LOADER_GET_CLASS (loader);

  if (klass->initialize != NULL)
    return klass->initialize (loader);

  return TRUE;
}

gboolean
peas_plugin_loader_load (PeasPluginLoader *loader,
                         PeasPluginInfo   *info)
{
  PeasPluginLoaderClass *klass;

  g_return_val_if_fail (PEAS_IS_PLUGIN_LOADER (loader), FALSE);

  klass = PEAS_PLUGIN_LOADER_GET_CLASS (loader);
  g_return_val_if_fail (klass->load != NULL, FALSE);

  return klass->load (loader, info);
}

void
peas_plugin_loader_unload (PeasPluginLoader *loader,
                           PeasPluginInfo   *info)
{
  PeasPluginLoaderClass *klass;

  g_return_if_fail (PEAS_IS_PLUGIN_LOADER (loader));

  klass = PEAS_PLUGIN_LOADER_GET_CLASS (loader);
  g_return_if_fail (klass->unload != NULL);

  klass->unload (loader, info);
}

gboolean
peas_plugin_loader_provides_extension (PeasPluginLoader *loader,
                                       PeasPluginInfo   *info,
                                       GType             ext_type)
{
  PeasPluginLoaderClass *klass;

  g_return_val_if_fail (PEAS_IS_PLUGIN_LOADER (loader), FALSE);

  klass = PEAS_PLUGIN_LOADER_GET_CLASS (loader);
  g_return_val_if_fail (klass->provides_extension != NULL, FALSE);

  return klass->provides_extension (loader, info, ext_type);
}

PeasExtension *
peas_plugin_loader_create_extension (PeasPluginLoader *loader,
                                     PeasPluginInfo   *info,
                                     GType             ext_type,
                                     guint             n_parameters,
                                     GParameter       *parameters)
{
  PeasPluginLoaderClass *klass;

  g_return_val_if_fail (PEAS_IS_PLUGIN_LOADER (loader), NULL);

  klass = PEAS_PLUGIN_LOADER_GET_CLASS (loader);
  g_return_val_if_fail (klass->create_extension != NULL, NULL);

  return klass->create_extension (loader, info, ext_type, n_parameters, parameters);
}

void
peas_plugin_loader_garbage_collect (PeasPluginLoader *loader)
{
  PeasPluginLoaderClass *klass;

  g_return_if_fail (PEAS_IS_PLUGIN_LOADER (loader));

  klass = PEAS_PLUGIN_LOADER_GET_CLASS (loader);

  if (klass->garbage_collect != NULL)
    klass->garbage_collect (loader);
}
