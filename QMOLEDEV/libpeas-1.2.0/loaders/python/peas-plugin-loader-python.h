/*
 * peas-plugin-loader-python.h
 * This file is part of libpeas
 *
 * Copyright (C) 2008 - Jesse van den Kieboom
 * Copyright (C) 2009 - Steve Frécinaux
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

#ifndef __PEAS_PLUGIN_LOADER_PYTHON_H__
#define __PEAS_PLUGIN_LOADER_PYTHON_H__

#include <libpeas/peas-plugin-loader.h>
#include <libpeas/peas-object-module.h>

G_BEGIN_DECLS

#define PEAS_TYPE_PLUGIN_LOADER_PYTHON             (peas_plugin_loader_python_get_type ())
#define PEAS_PLUGIN_LOADER_PYTHON(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), PEAS_TYPE_PLUGIN_LOADER_PYTHON, PeasPluginLoaderPython))
#define PEAS_PLUGIN_LOADER_PYTHON_CONST(obj)       (G_TYPE_CHECK_INSTANCE_CAST ((obj), PEAS_TYPE_PLUGIN_LOADER_PYTHON, PeasPluginLoaderPython const))
#define PEAS_PLUGIN_LOADER_PYTHON_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), PEAS_TYPE_PLUGIN_LOADER_PYTHON, PeasPluginLoaderPythonClass))
#define PEAS_IS_PLUGIN_LOADER_PYTHON(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), PEAS_TYPE_PLUGIN_LOADER_PYTHON))
#define PEAS_IS_PLUGIN_LOADER_PYTHON_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), PEAS_TYPE_PLUGIN_LOADER_PYTHON))
#define PEAS_PLUGIN_LOADER_PYTHON_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), PEAS_TYPE_PLUGIN_LOADER_PYTHON, PeasPluginLoaderPythonClass))

typedef struct _PeasPluginLoaderPython         PeasPluginLoaderPython;
typedef struct _PeasPluginLoaderPythonClass    PeasPluginLoaderPythonClass;
typedef struct _PeasPluginLoaderPythonPrivate  PeasPluginLoaderPythonPrivate;

struct _PeasPluginLoaderPython {
  PeasPluginLoader parent;

  PeasPluginLoaderPythonPrivate *priv;
};

struct _PeasPluginLoaderPythonClass {
  PeasPluginLoaderClass parent_class;
};

GType                    peas_plugin_loader_python_get_type  (void) G_GNUC_CONST;

/* All the loaders must implement this function */
G_MODULE_EXPORT void     peas_register_types                 (PeasObjectModule *module);

G_END_DECLS

#endif /* __PEAS_PLUGIN_LOADER_PYTHON_H__ */

