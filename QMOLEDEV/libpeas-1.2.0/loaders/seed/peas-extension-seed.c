/*
 * peas-extension-seed.c
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

#include "peas-extension-seed.h"
#include <libpeas/peas-introspection.h>
#include <libpeas/peas-extension-subclasses.h>
#include <girepository.h>

G_DEFINE_TYPE (PeasExtensionSeed, peas_extension_seed, PEAS_TYPE_EXTENSION_WRAPPER);

typedef struct {
  GITypeInfo type_info;
  gpointer ptr;
} OutArg;


static void
peas_extension_seed_init (PeasExtensionSeed *sexten)
{
}

static gchar *
convert_property_name (const gchar *pname)
{
  gint i;
  gchar *prop_name;

  prop_name = g_strdup (pname);

  for (i = 0; prop_name[i] != '\0'; ++i)
    {
      if (prop_name[i] == '-')
        prop_name[i] = '_';
    }

  return prop_name;
}

static void
peas_extension_seed_get_property (GObject    *object,
                                  guint       prop_id,
                                  GValue     *value,
                                  GParamSpec *pspec)
{
  PeasExtensionSeed *sexten = PEAS_EXTENSION_SEED (object);
  SeedValue seed_value;
  SeedException exc = NULL;
  gchar *prop_name;

  /* Don't add properties as they could shadow the instance's */

  prop_name = convert_property_name (pspec->name);

  seed_value = seed_object_get_property (sexten->js_context, sexten->js_object,
                                         prop_name);

  g_free (prop_name);


  seed_value_to_gvalue (sexten->js_context, seed_value,
                        pspec->value_type, value, &exc);

  if (exc != NULL)
    {
      gchar *exc_string;

      exc_string = seed_exception_to_string (sexten->js_context, exc);
      g_warning ("Seed Exception: %s", exc_string);

      g_free (exc_string);
    }
}

static void
peas_extension_seed_set_property (GObject      *object,
                                  guint         prop_id,
                                  const GValue *value,
                                  GParamSpec   *pspec)
{
  PeasExtensionSeed *sexten = PEAS_EXTENSION_SEED (object);
  SeedValue seed_value;
  SeedException exc = NULL;
  gchar *prop_name;

  /* Don't add properties as they could shadow the instance's */

  seed_value = seed_value_from_gvalue (sexten->js_context,
                                       (GValue *) value, &exc);

  if (exc != NULL)
    {
      gchar *exc_string;

      exc_string = seed_exception_to_string (sexten->js_context, exc);
      g_warning ("Seed Exception: %s", exc_string);

      g_free (exc_string);
      return;
    }

  prop_name = convert_property_name (pspec->name);

  seed_object_set_property (sexten->js_context, sexten->js_object,
                            prop_name, seed_value);

  g_free (prop_name);
}

static void
peas_extension_seed_dispose (GObject *object)
{
  PeasExtensionSeed *sexten = PEAS_EXTENSION_SEED (object);

  if (sexten->js_object != NULL)
    {
      seed_value_unprotect (sexten->js_context, sexten->js_object);
      seed_context_unref (sexten->js_context);

      sexten->js_object = NULL;
      sexten->js_context = NULL;
    }
}

static gboolean
peas_extension_seed_call (PeasExtensionWrapper *exten,
                          const gchar          *method_name,
                          GIArgument           *args,
                          GIArgument           *retval)
{
  PeasExtensionSeed *sexten = PEAS_EXTENSION_SEED (exten);
  GType exten_type;
  SeedValue js_method;
  GICallableInfo *func_info;
  gint n_args, i;
  SeedValue *js_in_args;
  OutArg *out_args;
  SeedValue js_ret, val;
  SeedException exc = NULL;
  gchar *exc_string;
  gint n_in_args = 0;
  gint n_out_args = 0;
  GIArgument argument;

  g_return_val_if_fail (sexten->js_context != NULL, FALSE);
  g_return_val_if_fail (sexten->js_object != NULL, FALSE);

  exten_type = peas_extension_wrapper_get_extension_type (exten);

  /* Fetch the JS method we want to call */
  js_method = seed_object_get_property (sexten->js_context,
                                        sexten->js_object,
                                        method_name);
  if (seed_value_is_undefined (sexten->js_context, js_method))
    {
      g_warning ("Method '%s.%s' is not defined",
                 g_type_name (exten_type), method_name);
      return FALSE;
    }

  /* We want to display an error if the method is defined but is not a function. */
  if (!seed_value_is_function (sexten->js_context, js_method))
    {
      g_warning ("Method '%s.%s' is not a function",
                 g_type_name (exten_type), method_name);
      return FALSE;
    }

  /* Prepare the arguments */
  func_info = peas_gi_get_method_info (exten_type, method_name);
  if (func_info == NULL)
    return FALSE;

  n_args = g_callable_info_get_n_args (func_info);
  g_return_val_if_fail (n_args >= 0, FALSE);

  js_in_args = g_newa (SeedValue, n_args);
  out_args = g_newa (OutArg, n_args + 1);

  /* We put the return value first in the out tuple, as it seems to be
   * the common behaviour for GI-based bindings */
  g_callable_info_load_return_type (func_info, &out_args[0].type_info);
  if (g_type_info_get_tag (&out_args[0].type_info) != GI_TYPE_TAG_VOID)
    out_args[n_out_args++].ptr = &retval->v_pointer;

  /* Handle the other arguments */
  for (i = 0; i < n_args && exc == NULL; i++)
    {
      GIArgInfo arg_info;
      GIDirection direction;

      g_callable_info_load_arg (func_info, i, &arg_info);
      direction = g_arg_info_get_direction (&arg_info);
      g_arg_info_load_type (&arg_info, &out_args[n_out_args].type_info);

      if (direction == GI_DIRECTION_IN)
        {
          js_in_args[n_in_args++] = seed_value_from_gi_argument (sexten->js_context,
                                                                 &args[i],
                                                                 &out_args[n_out_args].type_info,
                                                                 &exc);
        }

      if (direction == GI_DIRECTION_INOUT)
        {
          GIArgument arg;

          peas_gi_pointer_to_argument (&out_args[n_out_args].type_info,
                                       args[i].v_pointer, &arg);
          js_in_args[n_in_args++] = seed_value_from_gi_argument (sexten->js_context,
                                                                 &arg,
                                                                 &out_args[n_out_args].type_info,
                                                                 &exc);
        }

      if (direction == GI_DIRECTION_OUT || direction == GI_DIRECTION_INOUT)
        out_args[n_out_args++].ptr = args[i].v_pointer;
    }
  if (exc != NULL)
    goto cleanup;

  js_ret = seed_object_call (sexten->js_context,
                             js_method,
                             sexten->js_object,
                             n_in_args,
                             js_in_args,
                             &exc);
  if (exc != NULL)
    goto cleanup;

  if (n_out_args == 1)
    {
      if (seed_value_to_gi_argument (sexten->js_context, js_ret,
                                     &out_args[0].type_info, &argument, &exc))
        {
          peas_gi_argument_to_pointer (&out_args[0].type_info,
                                       &argument, out_args[0].ptr);
        }
    }
  else if (n_out_args > 0 && seed_value_is_object (sexten->js_context, js_ret))
    {
      for (i = 0; i < n_out_args && exc == NULL; i++)
        {
          val = seed_object_get_property_at_index (sexten->js_context, js_ret,
                                                   i, exc);

          if (exc == NULL &&
              seed_value_to_gi_argument (sexten->js_context, val,
                                         &out_args[i].type_info,
                                         &argument, &exc))
            {
              peas_gi_argument_to_pointer (&out_args[i].type_info,
                                           &argument, out_args[i].ptr);
            }
        }
    }

cleanup:

  g_base_info_unref ((GIBaseInfo *) func_info);

  if (exc == NULL)
    return TRUE;

  exc_string = seed_exception_to_string (sexten->js_context, exc);
  g_warning ("Seed Exception: %s", exc_string);
  g_free (exc_string);
  return FALSE;
}

static void
peas_extension_seed_class_init (PeasExtensionSeedClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  PeasExtensionWrapperClass *extension_class = PEAS_EXTENSION_WRAPPER_CLASS (klass);

  object_class->set_property = peas_extension_seed_set_property;
  object_class->get_property = peas_extension_seed_get_property;
  object_class->dispose = peas_extension_seed_dispose;

  extension_class->call = peas_extension_seed_call;
}

GObject *
peas_extension_seed_new (GType       exten_type,
                         SeedContext js_context,
                         SeedObject  js_object)
{
  PeasExtensionSeed *sexten;
  GType real_type;

  g_return_val_if_fail (js_context != NULL, NULL);
  g_return_val_if_fail (js_object != NULL, NULL);

  real_type = peas_extension_register_subclass (PEAS_TYPE_EXTENSION_SEED, exten_type);
  sexten = PEAS_EXTENSION_SEED (g_object_new (real_type, NULL));

  sexten->js_context = js_context;
  sexten->js_object = js_object;
  PEAS_EXTENSION_WRAPPER (sexten)->exten_type = exten_type;

  seed_context_ref (sexten->js_context);
  seed_value_protect (sexten->js_context, sexten->js_object);

  return G_OBJECT (sexten);
}
