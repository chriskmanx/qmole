/*
 * peas-extension-wrapper.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010-2011 - Steve Frécinaux
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

#ifndef __PEAS_EXTENSION_WRAPPER_H__
#define __PEAS_EXTENSION_WRAPPER_H__

#include <glib-object.h>
#include <girepository.h>

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define PEAS_TYPE_EXTENSION_WRAPPER             (peas_extension_wrapper_get_type ())
#define PEAS_EXTENSION_WRAPPER(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj), PEAS_TYPE_EXTENSION_WRAPPER, PeasExtensionWrapper))
#define PEAS_IS_EXTENSION_WRAPPER(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj), PEAS_TYPE_EXTENSION_WRAPPER))
#define PEAS_EXTENSION_WRAPPER_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass), PEAS_TYPE_EXTENSION_WRAPPER, PeasExtensionWrapperClass))
#define PEAS_IS_EXTENSION_WRAPPER_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), PEAS_TYPE_EXTENSION_WRAPPER))
#define PEAS_EXTENSION_WRAPPER_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj), PEAS_TYPE_EXTENSION_WRAPPER, PeasExtensionWrapperClass))

typedef struct _PeasExtensionWrapper      PeasExtensionWrapper;
typedef struct _PeasExtensionWrapperClass PeasExtensionWrapperClass;

struct _PeasExtensionWrapper {
  GObject parent;

  /*< private >*/
  GType exten_type;
  gboolean constructed;
};

struct _PeasExtensionWrapperClass {
  GObjectClass parent_class;

  /*< private >*/
  gboolean   (*call)                      (PeasExtensionWrapper *exten,
                                           const gchar          *method,
                                           GIArgument           *args,
                                           GIArgument           *return_value);
};

/*
 * Public methods
 */
GType        peas_extension_wrapper_get_type    (void)  G_GNUC_CONST;

GType        peas_extension_wrapper_get_extension_type
                                                (PeasExtensionWrapper *exten);

gboolean     peas_extension_wrapper_callv       (PeasExtensionWrapper *exten,
                                                 const gchar          *method_name,
                                                 GIArgument           *args,
                                                 GIArgument           *return_value);

G_END_DECLS

#endif /* __PEAS_EXTENSION_WRAPPER_H__ */
