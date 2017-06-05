/*
 * peasdemo-hello-world-configurable.h
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

#ifndef __PEASDEMO_HELLO_WORLD_CONFIGURABLE_H__
#define __PEASDEMO_HELLO_WORLD_CONFIGURABLE_H__

#include <gtk/gtk.h>
#include <libpeas/peas.h>

G_BEGIN_DECLS

#define PEASDEMO_TYPE_HELLO_WORLD_CONFIGURABLE         (peasdemo_hello_world_configurable_get_type ())
#define PEASDEMO_HELLO_WORLD_CONFIGURABLE(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), PEASDEMO_TYPE_HELLO_WORLD_CONFIGURABLE, PeasDemoHelloWorldConfigurable))
#define PEASDEMO_HELLO_WORLD_CONFIGURABLE_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), PEASDEMO_TYPE_HELLO_WORLD_CONFIGURABLE, PeasDemoHelloWorldConfigurable))
#define PEASDEMO_IS_HELLO_WORLD_CONFIGURABLE(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), PEASDEMO_TYPE_HELLO_WORLD_CONFIGURABLE))
#define PEASDEMO_IS_HELLO_WORLD_CONFIGURABLE_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), PEASDEMO_TYPE_HELLO_WORLD_CONFIGURABLE))
#define PEASDEMO_HELLO_WORLD_CONFIGURABLE_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), PEASDEMO_TYPE_HELLO_WORLD_CONFIGURABLE, PeasDemoHelloWorldConfigurableClass))

typedef struct _PeasDemoHelloWorldConfigurable      PeasDemoHelloWorldConfigurable;
typedef struct _PeasDemoHelloWorldConfigurableClass PeasDemoHelloWorldConfigurableClass;

struct _PeasDemoHelloWorldConfigurable {
  PeasExtensionBase parent;
};

struct _PeasDemoHelloWorldConfigurableClass {
  PeasExtensionBaseClass parent_class;
};

GType   peasdemo_hello_world_configurable_get_type  (void) G_GNUC_CONST;
void    peasdemo_hello_world_configurable_register  (GTypeModule *module);

G_END_DECLS

#endif /* __PEASDEMO_HELLO_WORLD_CONFIGURABLE_H__ */
