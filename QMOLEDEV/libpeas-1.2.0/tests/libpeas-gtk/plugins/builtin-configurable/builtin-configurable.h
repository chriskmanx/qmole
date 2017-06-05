/*
 * builtin-configurable.h
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

#ifndef __TESTING_BUILTIN_CONFIGURABLE_H__
#define __TESTING_BUILTIN_CONFIGURABLE_H__

#include <libpeas/peas.h>

G_BEGIN_DECLS

#define TESTING_TYPE_BUILTIN_CONFIGURABLE         (testing_builtin_configurable_get_type ())
#define TESTING_BUILTIN_CONFIGURABLE(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), TESTING_TYPE_BUILTIN_CONFIGURABLE, TestingBuiltinConfigurable))
#define TESTING_BUILTIN_CONFIGURABLE_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), TESTING_TYPE_BUILTIN_CONFIGURABLE, TestingBuiltinConfigurable))
#define TESTING_IS_BUILTIN_CONFIGURABLE(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), TESTING_TYPE_BUILTIN_CONFIGURABLE))
#define TESTING_IS_BUILTIN_CONFIGURABLE_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), TESTING_TYPE_BUILTIN_CONFIGURABLE))
#define TESTING_BUILTIN_CONFIGURABLE_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), TESTING_TYPE_BUILTIN_CONFIGURABLE, TestingBuiltinConfigurableClass))

typedef struct _TestingBuiltinConfigurable         TestingBuiltinConfigurable;
typedef struct _TestingBuiltinConfigurableClass    TestingBuiltinConfigurableClass;

struct _TestingBuiltinConfigurable {
  PeasExtensionBase parent_instance;
};

struct _TestingBuiltinConfigurableClass {
  PeasExtensionBaseClass parent_class;
};

GType testing_builtin_configurable_get_type (void) G_GNUC_CONST;
void  testing_builtin_configurable_register (GTypeModule *module);

G_END_DECLS

#endif /* __TESTING_BUILTIN_CONFIGURABLE_H__ */
