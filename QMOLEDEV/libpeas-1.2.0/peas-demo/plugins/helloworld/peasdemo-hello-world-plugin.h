/*
 * peasdemo-hello-world-plugin.h
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

#ifndef __PEASDEMO_HELLO_WORLD_PLUGIN_H__
#define __PEASDEMO_HELLO_WORLD_PLUGIN_H__

#include <gtk/gtk.h>
#include <libpeas/peas.h>

G_BEGIN_DECLS

#define PEASDEMO_TYPE_HELLO_WORLD_PLUGIN         (peasdemo_hello_world_plugin_get_type ())
#define PEASDEMO_HELLO_WORLD_PLUGIN(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), PEASDEMO_TYPE_HELLO_WORLD_PLUGIN, PeasDemoHelloWorldPlugin))
#define PEASDEMO_HELLO_WORLD_PLUGIN_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), PEASDEMO_TYPE_HELLO_WORLD_PLUGIN, PeasDemoHelloWorldPlugin))
#define PEASDEMO_IS_HELLO_WORLD_PLUGIN(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), PEASDEMO_TYPE_HELLO_WORLD_PLUGIN))
#define PEASDEMO_IS_HELLO_WORLD_PLUGIN_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), PEASDEMO_TYPE_HELLO_WORLD_PLUGIN))
#define PEASDEMO_HELLO_WORLD_PLUGIN_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), PEASDEMO_TYPE_HELLO_WORLD_PLUGIN, PeasDemoHelloWorldPluginClass))

typedef struct _PeasDemoHelloWorldPlugin       PeasDemoHelloWorldPlugin;
typedef struct _PeasDemoHelloWorldPluginClass  PeasDemoHelloWorldPluginClass;

struct _PeasDemoHelloWorldPlugin {
  PeasExtensionBase parent_instance;

  GtkWidget *window;
  GtkWidget *label;
};

struct _PeasDemoHelloWorldPluginClass {
  PeasExtensionBaseClass parent_class;
};

GType                 peasdemo_hello_world_plugin_get_type        (void) G_GNUC_CONST;
G_MODULE_EXPORT void  peas_register_types                         (PeasObjectModule *module);

G_END_DECLS

#endif /* __PEASDEMO_HELLO_WORLD_PLUGIN_H__ */
