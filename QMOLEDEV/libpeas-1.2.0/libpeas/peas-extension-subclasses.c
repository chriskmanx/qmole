/*
 * peas-extension-subclasses.c
 * This file is part of libpeas
 *
 * Copyright (C) 2010 - Steve Frécinaux
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

#include <string.h>
#include <girepository.h>
#include <girffi.h>
#include "peas-extension-wrapper.h"
#include "peas-extension-subclasses.h"
#include "peas-introspection.h"

typedef struct _MethodImpl {
  GICallableInfo *info;
  gchar *method_name;
  ffi_cif cif;
  ffi_closure *closure;
  guint struct_offset;
} MethodImpl;

static GQuark
method_impl_quark (void)
{
  static GQuark quark = 0;

  if (quark == 0)
    quark = g_quark_from_static_string ("PeasExtensionInterfaceImplementation");

  return quark;
}

static void
handle_method_impl (ffi_cif  *cif,
                    gpointer  result,
                    gpointer *args,
                    gpointer  data)
{
  MethodImpl *impl = (MethodImpl *) data;
  GIArgInfo arg_info;
  GITypeInfo type_info;
  GITypeInfo return_type_info;
  gint n_args, i;
  PeasExtensionWrapper *instance;
  GIArgument *arguments;
  GIArgument return_value;

  instance = *((PeasExtensionWrapper **) args[0]);
  g_assert (PEAS_IS_EXTENSION_WRAPPER (instance));

  n_args = g_callable_info_get_n_args (impl->info);
  g_return_if_fail (n_args >= 1);
  arguments = g_newa (GIArgument, n_args-1);

  for (i = 1; i < n_args; i++)
    {
      g_callable_info_load_arg (impl->info, i, &arg_info);
      g_arg_info_load_type (&arg_info, &type_info);

      if (g_arg_info_get_direction (&arg_info) == GI_DIRECTION_IN)
        peas_gi_pointer_to_argument (&type_info, args[i], &arguments[i-1]);
      else
        arguments[i-1].v_pointer = *((gpointer **) args[i]);
    }

  peas_extension_wrapper_callv (instance, impl->method_name, arguments, &return_value);

  g_callable_info_load_return_type (impl->info, &return_type_info);
  if (g_type_info_get_tag (&return_type_info) != GI_TYPE_TAG_VOID)
    peas_gi_argument_to_pointer (&return_type_info, &return_value, result);
}

static void
create_native_closure (GIInterfaceInfo *iface_info,
                       GIVFuncInfo     *vfunc_info,
                       MethodImpl      *impl)
{
  GIFunctionInfo *invoker_info;
  GIStructInfo *struct_info;
  GIFieldInfo *field_info;
  GITypeInfo *type_info;
  GICallbackInfo *callback_info;
  guint n_fields, i;
  gboolean found_field_info;

  invoker_info = g_vfunc_info_get_invoker (vfunc_info);
  if (invoker_info == NULL)
    {
      g_debug ("No invoker for VFunc '%s.%s'",
               g_base_info_get_name (iface_info),
               g_base_info_get_name (vfunc_info));
      return;
    }

  struct_info = g_interface_info_get_iface_struct (iface_info);
  n_fields = g_struct_info_get_n_fields (struct_info);

  found_field_info = FALSE;
  for (i = 0; i < n_fields; i++)
    {
      field_info = g_struct_info_get_field (struct_info, i);

      if (strcmp (g_base_info_get_name (field_info),
                  g_base_info_get_name (vfunc_info)) == 0)
        {
          found_field_info = TRUE;
          break;
        }

      g_base_info_unref (field_info);
    }

  if (!found_field_info)
    {
      g_debug ("No struct field for VFunc '%s.%s'",
               g_base_info_get_name (iface_info),
               g_base_info_get_name (vfunc_info));
      g_base_info_unref (struct_info);
      g_base_info_unref (invoker_info);
      return;
    }

  type_info = g_field_info_get_type (field_info);
  g_assert (g_type_info_get_tag (type_info) == GI_TYPE_TAG_INTERFACE);

  callback_info = g_type_info_get_interface (type_info);
  g_assert (g_base_info_get_type (callback_info) == GI_INFO_TYPE_CALLBACK);

  impl->info = g_base_info_ref (callback_info);
  impl->method_name = g_strdup (g_base_info_get_name (invoker_info));
  impl->closure = g_callable_info_prepare_closure (callback_info, &impl->cif,
                                                   handle_method_impl, impl);
  impl->struct_offset = g_field_info_get_offset (field_info);

  g_base_info_unref (callback_info);
  g_base_info_unref (type_info);
  g_base_info_unref (field_info);
  g_base_info_unref (struct_info);
  g_base_info_unref (invoker_info);
}

static void
implement_interface_methods (gpointer iface,
                             GType    proxy_type)
{
  GType exten_type = G_TYPE_FROM_INTERFACE (iface);
  GIInterfaceInfo *iface_info;
  guint n_vfuncs, i;
  MethodImpl *impls;

  g_debug ("Implementing interface '%s' for proxy type '%s'",
           g_type_name (exten_type), g_type_name (proxy_type));

  iface_info = g_irepository_find_by_gtype (NULL, exten_type);
  g_return_if_fail (iface_info != NULL);
  g_return_if_fail (g_base_info_get_type (iface_info) == GI_INFO_TYPE_INTERFACE);

  n_vfuncs = g_interface_info_get_n_vfuncs (iface_info);

  impls = g_type_get_qdata (exten_type, method_impl_quark ());

  if (impls == NULL)
    {
      impls = g_new0 (MethodImpl, n_vfuncs);

      for (i = 0; i < n_vfuncs; i++)
        {
          GIVFuncInfo *vfunc_info;
          vfunc_info = g_interface_info_get_vfunc (iface_info, i);
          create_native_closure (iface_info, vfunc_info, &impls[i]);
          g_base_info_unref ((GIBaseInfo *) vfunc_info);
        }

      g_type_set_qdata (exten_type, method_impl_quark (), impls);
    }

  for (i = 0; i < n_vfuncs; i++)
    {
      gpointer *method_ptr;

      if (impls[i].closure == NULL)
        continue;

      method_ptr = G_STRUCT_MEMBER_P (iface, impls[i].struct_offset);
      *method_ptr = impls[i].closure;

      g_debug ("Implemented '%s.%s' at %d (%p) with %p",
               g_type_name (exten_type), impls[i].method_name,
               impls[i].struct_offset, method_ptr, impls[i].closure);
    }

  g_base_info_unref (iface_info);

  g_debug ("Implemented interface '%s' for '%s' proxy",
           g_type_name (exten_type), g_type_name (proxy_type));
}

static gpointer
get_parent_class (GObject *object)
{
  return g_type_class_peek (g_type_parent (G_TYPE_FROM_INSTANCE (object)));
}

static void
extension_subclass_set_property (GObject      *object,
                                 guint         prop_id,
                                 const GValue *value,
                                 GParamSpec   *pspec)
{
  PeasExtensionWrapper *exten = PEAS_EXTENSION_WRAPPER (object);

  /* This will have already been set on the real instance */
  if ((pspec->flags & G_PARAM_CONSTRUCT_ONLY) != 0)
    return;

  /* Setting will fail if we are not constructed yet */
  if ((pspec->flags & G_PARAM_CONSTRUCT) != 0 && !exten->constructed)
    return;

  g_debug ("Setting '%s:%s'",
           G_OBJECT_TYPE_NAME (object),
           g_param_spec_get_name (pspec));

  G_OBJECT_CLASS (get_parent_class (object))->set_property (object, prop_id,
                                                            value, pspec);
}

static void
extension_subclass_get_property (GObject    *object,
                                 guint       prop_id,
                                 GValue     *value,
                                 GParamSpec *pspec)
{
  g_debug ("Getting '%s:%s'",
           G_OBJECT_TYPE_NAME (object),
           g_param_spec_get_name (pspec));

  G_OBJECT_CLASS (get_parent_class (object))->get_property (object, prop_id,
                                                            value, pspec);
}

static void
extension_subclass_init (GObjectClass *klass,
                         GType         exten_type)
{
  guint n_props, i;
  gpointer iface_vtable;
  GParamSpec **properties;

  g_debug ("Initializing class '%s'", G_OBJECT_CLASS_NAME (klass));

  iface_vtable = g_type_default_interface_peek (exten_type);
  properties = g_object_interface_list_properties (iface_vtable, &n_props);

  if (n_props > 0)
    {
      klass->set_property = extension_subclass_set_property;
      klass->get_property = extension_subclass_get_property;

      for (i = 0; i < n_props; ++i)
        {
          const gchar *property_name = g_param_spec_get_name (properties[i]);

          g_object_class_override_property (klass, i + 1, property_name);

          g_debug ("Overrided '%s:%s' for '%s' proxy",
                   g_type_name (exten_type), property_name,
                   G_OBJECT_CLASS_NAME (klass));
        }
    }

  g_free (properties);

  g_debug ("Initialized class '%s'", G_OBJECT_CLASS_NAME (klass));
}

static void
extension_subclass_instance_init (GObject *instance)
{
  g_debug ("Initializing new instance of '%s'", G_OBJECT_TYPE_NAME (instance));
}

GType
peas_extension_register_subclass (GType parent_type,
                                  GType extension_type)
{
  gchar *type_name;
  GType the_type;

  type_name = g_strdup_printf ("%s+%s",
                               g_type_name (parent_type),
                               g_type_name (extension_type));

  the_type = g_type_from_name (type_name);

  if (the_type == G_TYPE_INVALID)
    {
      GTypeQuery query;
      GTypeInfo type_info = {
        0,
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) extension_subclass_init,
        (GClassFinalizeFunc) NULL,
        GSIZE_TO_POINTER (extension_type),
        0,
        0,
        (GInstanceInitFunc) extension_subclass_instance_init,
        NULL
      };
      GInterfaceInfo iface_info = {
        (GInterfaceInitFunc) implement_interface_methods,
        (GInterfaceFinalizeFunc) NULL,
        NULL
      };

      g_debug ("Registering new type '%s'", type_name);

      g_type_query (parent_type, &query);
      type_info.class_size = query.class_size;
      type_info.instance_size = query.instance_size;

      the_type = g_type_register_static (parent_type, type_name, &type_info, 0);

      iface_info.interface_data = GSIZE_TO_POINTER (the_type);

      g_type_add_interface_static (the_type, extension_type, &iface_info);
    }

  g_free (type_name);

  return the_type;
}
