/*
 * peas-object-module.h
 * This file is part of libpeas
 *
 * Copyright (C) 2003 Marco Pesenti Gritti
 * Copyright (C) 2003, 2004 Christian Persch
 * Copyright (C) 2005-2007 Paolo Maggi
 * Copyright (C) 2008 Jesse van den Kieboom
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

#ifndef __PEAS_OBJECT_MODULE_H__
#define __PEAS_OBJECT_MODULE_H__

#include <glib-object.h>
#include <gmodule.h>

G_BEGIN_DECLS

#define PEAS_TYPE_OBJECT_MODULE             (peas_object_module_get_type ())
#define PEAS_OBJECT_MODULE(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), PEAS_TYPE_OBJECT_MODULE, PeasObjectModule))
#define PEAS_OBJECT_MODULE_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), PEAS_TYPE_OBJECT_MODULE, PeasObjectModuleClass))
#define PEAS_IS_OBJECT_MODULE(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), PEAS_TYPE_OBJECT_MODULE))
#define PEAS_IS_OBJECT_MODULE_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), PEAS_TYPE_OBJECT_MODULE))
#define PEAS_OBJECT_MODULE_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj), PEAS_TYPE_OBJECT_MODULE, PeasObjectModuleClass))

typedef struct _PeasObjectModule         PeasObjectModule;
typedef struct _PeasObjectModuleClass    PeasObjectModuleClass;
typedef struct _PeasObjectModulePrivate  PeasObjectModulePrivate;

/**
 * PeasFactoryFunc:
 * @n_parameters: The number of paramteters.
 * @parameters: (array length=n_parameters): The parameters.
 * @user_data: Optional data to be passed to the function, or %NULL.
 *
 * A #PeasFactoryFunc is a factory function which will instanciate a new
 * extension of a given type. g_object_newv() is such a function.
 *
 * It is used with peas_object_module_register_extension_factory().
 *
 * Return value: (transfer full): The created object.
 */
typedef GObject *(*PeasFactoryFunc)   (guint          n_parameters,
                                       GParameter    *parameters,
                                       gpointer       user_data);

/**
 * PeasObjectModule:
 *
 * The #PeasObjectModule structure contains only private data and should only
 * be accessed using the provided API.
 */
struct _PeasObjectModule {
  GTypeModule parent;

  PeasObjectModulePrivate *priv;
};

/**
 * PeasObjectModuleClass:
 * @parent_class: The parent class.
 *
 * The class structure for #PeasObjectModule.
 */
struct _PeasObjectModuleClass {
  GTypeModuleClass parent_class;

  /*< private >*/
  gpointer padding[8];
};

GType               peas_object_module_get_type               (void) G_GNUC_CONST;
PeasObjectModule   *peas_object_module_new                    (const gchar      *module_name,
                                                               const gchar      *path,
                                                               gboolean          resident);

GObject            *peas_object_module_create_object          (PeasObjectModule *module,
                                                               GType             interface,
                                                               guint             n_parameters,
                                                               GParameter       *parameters);
gboolean            peas_object_module_provides_object        (PeasObjectModule *module,
                                                               GType             interface);

const gchar        *peas_object_module_get_path               (PeasObjectModule *module);
const gchar        *peas_object_module_get_module_name        (PeasObjectModule *module);

GModule            *peas_object_module_get_library            (PeasObjectModule *module);

void                peas_object_module_register_extension_factory
                                                              (PeasObjectModule *module,
                                                               GType             iface_type,
                                                               PeasFactoryFunc   factory_func,
                                                               gpointer          user_data,
                                                               GDestroyNotify    destroy_func);
void                peas_object_module_register_extension_type
                                                              (PeasObjectModule *module,
                                                               GType             iface_type,
                                                               GType             extension_type);

G_END_DECLS

#endif /* __PEAS_OBJECT_MODULE_H__ */
