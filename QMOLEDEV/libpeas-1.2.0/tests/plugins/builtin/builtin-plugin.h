/*
 * builtin-plugin.h
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

#ifndef __TESTING_BUILTIN_PLUGIN_H__
#define __TESTING_BUILTIN_PLUGIN_H__

#include <libpeas/peas.h>

G_BEGIN_DECLS

#define TESTING_TYPE_BUILTIN_PLUGIN         (testing_builtin_plugin_get_type ())
#define TESTING_BUILTIN_PLUGIN(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), TESTING_TYPE_BUILTIN_PLUGIN, TestingBuiltinPlugin))
#define TESTING_BUILTIN_PLUGIN_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), TESTING_TYPE_BUILTIN_PLUGIN, TestingBuiltinPlugin))
#define TESTING_IS_BUILTIN_PLUGIN(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), TESTING_TYPE_BUILTIN_PLUGIN))
#define TESTING_IS_BUILTIN_PLUGIN_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), TESTING_TYPE_BUILTIN_PLUGIN))
#define TESTING_BUILTIN_PLUGIN_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), TESTING_TYPE_BUILTIN_PLUGIN, TestingBuiltinPluginClass))

typedef struct _TestingBuiltinPlugin         TestingBuiltinPlugin;
typedef struct _TestingBuiltinPluginClass    TestingBuiltinPluginClass;
typedef struct _TestingBuiltinPluginPrivate  TestingBuiltinPluginPrivate;

struct _TestingBuiltinPlugin {
  PeasExtensionBase parent_instance;

  TestingBuiltinPluginPrivate *priv;
};

struct _TestingBuiltinPluginClass {
  PeasExtensionBaseClass parent_class;
};

GType                 testing_builtin_plugin_get_type (void) G_GNUC_CONST;
G_MODULE_EXPORT void  peas_register_types             (PeasObjectModule *module);

G_END_DECLS

#endif /* __TESTING_BUILTIN_PLUGIN_H__ */
