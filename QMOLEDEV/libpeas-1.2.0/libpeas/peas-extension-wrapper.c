/*
 * peas-extension.c
 * This file is part of libpeas
 *
 * Copyright (C) 2010 Steve Frécinaux
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

#include "peas-extension-wrapper.h"
#include "peas-introspection.h"

G_DEFINE_ABSTRACT_TYPE (PeasExtensionWrapper, peas_extension_wrapper, G_TYPE_OBJECT);

static void
peas_extension_wrapper_init (PeasExtensionWrapper *exten)
{
}

static void
peas_extension_wrapper_constructed (GObject *object)
{
  PeasExtensionWrapper *exten = PEAS_EXTENSION_WRAPPER (object);

  exten->constructed = TRUE;

  if (G_OBJECT_CLASS (peas_extension_wrapper_parent_class)->constructed != NULL)
    G_OBJECT_CLASS (peas_extension_wrapper_parent_class)->constructed (object);
}

static void
peas_extension_wrapper_class_init (PeasExtensionWrapperClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->constructed = peas_extension_wrapper_constructed;

  /* Don't add properties as they could shadow the instance's
   * and C plugins would not have the property.
   */
}

GType
peas_extension_wrapper_get_extension_type (PeasExtensionWrapper *exten)
{
  g_return_val_if_fail (PEAS_IS_EXTENSION_WRAPPER (exten), G_TYPE_INVALID);

  return exten->exten_type;
}

gboolean
peas_extension_wrapper_callv (PeasExtensionWrapper *exten,
                              const gchar          *method_name,
                              GIArgument           *args,
                              GIArgument           *return_value)
{
  PeasExtensionWrapperClass *klass;

  g_return_val_if_fail (PEAS_IS_EXTENSION_WRAPPER (exten), FALSE);
  g_return_val_if_fail (method_name != NULL, FALSE);

  klass = PEAS_EXTENSION_WRAPPER_GET_CLASS (exten);
  return klass->call (exten, method_name, args, return_value);
}
