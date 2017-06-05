/*
 * peas-extension-base.h
 * This file is part of libpeas
 *
 * Copyright (C) 2002-2005 - Paolo Maggi
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

#ifndef __PEAS_EXTENSION_BASE_H__
#define __PEAS_EXTENSION_BASE_H__

#include <glib-object.h>

#include "peas-plugin-info.h"

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define PEAS_TYPE_EXTENSION_BASE            (peas_extension_base_get_type())
#define PEAS_EXTENSION_BASE(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), PEAS_TYPE_EXTENSION_BASE, PeasExtensionBase))
#define PEAS_EXTENSION_BASE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), PEAS_TYPE_EXTENSION_BASE, PeasExtensionBaseClass))
#define PEAS_IS_EXTENSION_BASE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), PEAS_TYPE_EXTENSION_BASE))
#define PEAS_IS_EXTENSION_BASE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), PEAS_TYPE_EXTENSION_BASE))
#define PEAS_EXTENSION_BASE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), PEAS_TYPE_EXTENSION_BASE, PeasExtensionBaseClass))

/**
 * PeasExtensionBase:
 *
 * Base class for C extensions.
 */
typedef struct _PeasExtensionBase        PeasExtensionBase;
typedef struct _PeasExtensionBaseClass   PeasExtensionBaseClass;
typedef struct _PeasExtensionBasePrivate PeasExtensionBasePrivate;

struct _PeasExtensionBase {
  GObject parent;

  PeasExtensionBasePrivate *priv;
};

/**
 * PeasExtensionBaseClass:
 * @parent_class: The parent class.
 *
 * The class structure of #PeasExtensionBase.
 */
struct _PeasExtensionBaseClass {
  GObjectClass parent_class;

  /*< private >*/
  gpointer padding[8];
};

/*
 * Public methods
 */
GType            peas_extension_base_get_type         (void)  G_GNUC_CONST;

PeasPluginInfo  *peas_extension_base_get_plugin_info  (PeasExtensionBase *extbase);
gchar           *peas_extension_base_get_data_dir     (PeasExtensionBase *extbase);

G_END_DECLS

#endif /* __PEAS_EXTENSION_BASE_H__ */
