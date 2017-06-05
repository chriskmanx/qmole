/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gparser.c parse DBus description files
 *
 * Copyright (C) 2003, 2005  Red Hat, Inc.
 *
 * Licensed under the Academic Free License version 2.1
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include <config.h>

#include "dbus-gparser.h"
#include "dbus/dbus-glib-lowlevel.h"
#include "dbus-gidl.h"
#include "dbus-gobject.h"
#include "dbus/dbus-signature.h"
#include <string.h>

#define ELEMENT_IS(name) (strcmp (element_name, (name)) == 0)

typedef struct
{
  const char  *name;
  const char **retloc;
} LocateAttr;

static gboolean
locate_attributes (const char  *element_name,
                   const char **attribute_names,
                   const char **attribute_values,
                   GError     **error,
                   const char  *first_attribute_name,
                   const char **first_attribute_retloc,
                   ...)
{
  va_list args;
  const char *name;
  const char **retloc;
  int n_attrs;
#define MAX_ATTRS 24
  LocateAttr attrs[MAX_ATTRS];
  gboolean retval;
  int i;

  g_return_val_if_fail (first_attribute_name != NULL, FALSE);
  g_return_val_if_fail (first_attribute_retloc != NULL, FALSE);

  retval = TRUE;

  n_attrs = 1;
  attrs[0].name = first_attribute_name;
  attrs[0].retloc = first_attribute_retloc;
  *first_attribute_retloc = NULL;
  
  va_start (args, first_attribute_retloc);

  name = va_arg (args, const char*);
  retloc = va_arg (args, const char**);

  while (name != NULL)
    {
      if (retloc == NULL)
        {
          va_end (args);
          return FALSE;
        }

      g_assert (n_attrs < MAX_ATTRS);
      
      attrs[n_attrs].name = name;
      attrs[n_attrs].retloc = retloc;
      n_attrs += 1;
      *retloc = NULL;      

      name = va_arg (args, const char*);
      retloc = va_arg (args, const char**);
    }

  va_end (args);

  i = 0;
  while (attribute_names[i])
    {
      int j;
      gboolean found;

      found = FALSE;
      j = 0;
      while (j < n_attrs)
        {
          if (strcmp (attrs[j].name, attribute_names[i]) == 0)
            {
              retloc = attrs[j].retloc;

              if (*retloc != NULL)
                {
                  g_set_error (error,
                               G_MARKUP_ERROR,
                               G_MARKUP_ERROR_PARSE,
                               "Attribute \"%s\" repeated twice on the same <%s> element",
                               attrs[j].name, element_name);
                  retval = FALSE;
                  goto out;
                }

              *retloc = attribute_values[i];
              found = TRUE;
            }

          ++j;
        }

      if (!found)
        {
          /* We want to passthrough namespaced XML nodes that we don't know anything about. */
          if (strchr (attribute_names[i], ':') == NULL)
            {
              g_set_error (error,
                           G_MARKUP_ERROR,
                           G_MARKUP_ERROR_PARSE,
                           "Attribute \"%s\" is invalid on <%s> element in this context",
                           attribute_names[i], element_name);
              retval = FALSE;
              goto out;
            }
        }

      ++i;
    }

 out:
  return retval;
}

#if 0
static gboolean
check_no_attributes (const char  *element_name,
                     const char **attribute_names,
                     const char **attribute_values,
                     GError     **error)
{
  if (attribute_names[0] != NULL)
    {
      g_set_error (error,
                   G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "Attribute \"%s\" is invalid on <%s> element in this context",
                   attribute_names[0], element_name);
      return FALSE;
    }

  return TRUE;
}
#endif

struct Parser
{
  int refcount;

  NodeInfo *result; /* Filled in when we pop the last node */
  GSList *node_stack;
  InterfaceInfo *interface;
  MethodInfo *method;
  SignalInfo *signal;
  PropertyInfo *property;
  ArgInfo *arg;
  gboolean in_annotation;
  guint unknown_namespaced_depth;
};

Parser*
parser_new (void)
{
  Parser *parser;

  parser = g_new0 (Parser, 1);

  parser->refcount = 1;

  return parser;
}

Parser *
parser_ref (Parser *parser)
{
  parser->refcount += 1;

  return parser;
}

void
parser_unref (Parser *parser)
{
  parser->refcount -= 1;
  if (parser->refcount == 0)
    {
      if (parser->result)
        node_info_unref (parser->result);

      g_free (parser);
    }
}

gboolean
parser_check_doctype (Parser      *parser,
                      const char  *doctype,
                      GError     **error)
{
  g_return_val_if_fail (error == NULL || *error == NULL, FALSE);
  
  if (strcmp (doctype, "node") != 0)
    {
      g_set_error (error,
                   G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "D-BUS description file has the wrong document type %s, use node or interface",
                   doctype);
      return FALSE;
    }
  else
    return TRUE;
}

static gboolean
parse_node (Parser      *parser,
            const char  *element_name,
            const char **attribute_names,
            const char **attribute_values,
            GError     **error)
{
  const char *name;
  NodeInfo *node;
  
  if (parser->interface ||
      parser->method ||
      parser->signal ||
      parser->property ||
      parser->arg ||
      parser->in_annotation)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "Can't put <%s> element here",
                   element_name);
      return FALSE;      
    }

  name = NULL;
  if (!locate_attributes (element_name, attribute_names,
                          attribute_values, error,
                          "name", &name,
                          NULL))
    return FALSE;

  /* Only the root node can have no name */
  if (parser->node_stack != NULL && name == NULL)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute required on <%s> element ",
                   "name", element_name);
      return FALSE;
    }

  /* Root element name must be absolute */
  if (parser->node_stack == NULL && name && *name != '/')
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute on <%s> element must be an absolute object path, \"%s\" not OK",
                   "name", element_name, name);
      return FALSE;
    }

  /* Other element names must not be absolute */
  if (parser->node_stack != NULL && name && *name == '/')
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute on <%s> element must not be an absolute object path, \"%s\" starts with /",
                   "name", element_name, name);
      return FALSE;
    }
  
  node = node_info_new (name);

  if (parser->node_stack != NULL)
    {
      node_info_add_node (parser->node_stack->data,
                          node);
    }
  
  parser->node_stack = g_slist_prepend (parser->node_stack,
                                        node);
  
  return TRUE;
}

static gboolean
parse_interface (Parser      *parser,
                 const char  *element_name,
                 const char **attribute_names,
                 const char **attribute_values,
                 GError     **error)
{
  const char *name;
  InterfaceInfo *iface;
  NodeInfo *top;
  
  if (parser->interface ||
      parser->method ||
      parser->signal ||
      parser->property ||
      parser->arg ||
      parser->in_annotation ||
      (parser->node_stack == NULL))
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "Can't put <%s> element here",
                   element_name);
      return FALSE;      
    }

  name = NULL;
  if (!locate_attributes (element_name, attribute_names,
                          attribute_values, error,
                          "name", &name,
                          NULL))
    return FALSE;

  if (name == NULL)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute required on <%s> element ",
                   "name", element_name);
      return FALSE;
    }

  top = parser->node_stack->data;
  
  iface = interface_info_new (name);
  node_info_add_interface (top, iface);
  interface_info_unref (iface);

  parser->interface = iface;
  
  return TRUE;
}

static gboolean
parse_method (Parser      *parser,
              const char  *element_name,
              const char **attribute_names,
              const char **attribute_values,
              GError     **error)
{
  const char *name;
  MethodInfo *method;
  
  if (parser->interface == NULL ||
      parser->node_stack == NULL ||
      parser->method ||
      parser->signal ||
      parser->property ||
      parser->in_annotation ||
      parser->arg)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "Can't put <%s> element here",
                   element_name);
      return FALSE;      
    }

  name = NULL;
  if (!locate_attributes (element_name, attribute_names,
                          attribute_values, error,
                          "name", &name,
                          NULL))
    return FALSE;

  if (name == NULL)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute required on <%s> element ",
                   "name", element_name);
      return FALSE;
    }

  method = method_info_new (name);
  interface_info_add_method (parser->interface, method);
  method_info_unref (method);

  parser->method = method;
  
  return TRUE;
}

static gboolean
parse_signal (Parser      *parser,
              const char  *element_name,
              const char **attribute_names,
              const char **attribute_values,
              GError     **error)
{
  const char *name;
  SignalInfo *signal;
  
  if (parser->interface == NULL ||
      parser->node_stack == NULL ||
      parser->signal ||
      parser->method ||
      parser->property ||
      parser->in_annotation ||
      parser->arg)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "Can't put <%s> element here",
                   element_name);
      return FALSE;      
    }

  name = NULL;
  if (!locate_attributes (element_name, attribute_names,
                          attribute_values, error,
                          "name", &name,
                          NULL))
    return FALSE;

  if (name == NULL)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute required on <%s> element ",
                   "name", element_name);
      return FALSE;
    }

  signal = signal_info_new (name);
  interface_info_add_signal (parser->interface, signal);
  signal_info_unref (signal);

  parser->signal = signal;
  
  return TRUE;
}

static gboolean
validate_signature (const char *str,
		    const char *element_name,
		    GError    **error)
{
  DBusError derror;

  dbus_error_init (&derror);
  
  if (!dbus_signature_validate (str, &derror))
    {
      dbus_set_g_error (error, &derror);
      return FALSE;
    }
  return TRUE;
}

static gboolean
parse_property (Parser      *parser,
                const char  *element_name,
                const char **attribute_names,
                const char **attribute_values,
                GError     **error)
{
  const char *name;
  const char *access;
  const char *type;
  PropertyInfo *property;
  PropertyAccessFlags access_flags;
  
  if (parser->interface == NULL ||
      parser->node_stack == NULL ||
      parser->signal ||
      parser->method ||
      parser->property ||
      parser->in_annotation ||
      parser->arg)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "Can't put <%s> element here",
                   element_name);
      return FALSE;      
    }

  name = NULL;
  if (!locate_attributes (element_name, attribute_names,
                          attribute_values, error,
                          "name", &name,
                          "access", &access,
                          "type", &type,
                          NULL))
    return FALSE;

  if (name == NULL)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute required on <%s> element ",
                   "name", element_name);
      return FALSE;
    }

  if (access == NULL)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute required on <%s> element ",
                   "access", element_name);
      return FALSE;
    }

  if (type == NULL)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute required on <%s> element ",
                   "type", element_name);
      return FALSE;
    }

  if (!validate_signature (type, element_name, error))
    return FALSE;

  access_flags = 0;
  if (strcmp (access, "readwrite") == 0)
    access_flags = PROPERTY_READ | PROPERTY_WRITE;
  else if (strcmp (access, "read") == 0)
    access_flags = PROPERTY_READ;
  else if (strcmp (access, "write") == 0)
    access_flags = PROPERTY_WRITE;
  else
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "access=\"%s\" must have value readwrite, read, or write on %s\n",
                   access, element_name);
      return FALSE;
    }
  
  property = property_info_new (name, type, access_flags);
  interface_info_add_property (parser->interface, property);
  property_info_unref (property);

  parser->property = property;
  
  return TRUE;
}

static gboolean
parse_arg (Parser      *parser,
           const char  *element_name,
           const char **attribute_names,
           const char **attribute_values,
           GError     **error)
{
  const char *name;
  const char *type;
  const char *direction;
  ArgDirection dir;
  ArgInfo *arg;
  char *generated_name;
  
  if (!(parser->method || parser->signal) ||
      parser->node_stack == NULL ||
      parser->property ||
      parser->in_annotation ||
      parser->arg)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "Can't put <%s> element here",
                   element_name);
      return FALSE;      
    }

  name = NULL;
  if (!locate_attributes (element_name, attribute_names,
                          attribute_values, error,
                          "name", &name,
                          "type", &type,
                          "direction", &direction,
                          NULL))
    return FALSE;

  /* name can be null for args */
  
  if (type == NULL)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute required on <%s> element ",
                   "type", element_name);
      return FALSE;
    }

  if (direction == NULL)
    {
      /* methods default to in, signal to out */
      if (parser->method)
        direction = "in";
      else if (parser->signal)
        direction = "out";
      else
        g_assert_not_reached ();
    }

  dir = ARG_INVALID;
  
  if (strcmp (direction, "in") == 0)
    dir = ARG_IN;
  else if (strcmp (direction, "out") == 0)
    dir = ARG_OUT;
  
  if (dir == ARG_INVALID ||
      (parser->signal && dir == ARG_IN))
    {
      if (parser->signal)
        g_set_error (error, G_MARKUP_ERROR,
                     G_MARKUP_ERROR_PARSE,
                     "Signals must have direction=\"out\" (just omit the direction attribute)");
      else
        g_set_error (error, G_MARKUP_ERROR,
                     G_MARKUP_ERROR_PARSE,
                     "\"%s\" attribute on <%s> has value \"in\" or \"out\"",
                     "direction", element_name);
      return FALSE;
    }

  if (!validate_signature (type, element_name, error))
    return FALSE;

  generated_name = NULL;
  if (name == NULL)
    generated_name = g_strdup_printf ("arg%d",
                                      parser->method ?
                                      method_info_get_n_args (parser->method) :
                                      signal_info_get_n_args (parser->signal));
                                      
  arg = arg_info_new (name ? name : generated_name, dir, type);
  if (parser->method)
    method_info_add_arg (parser->method, arg);
  else if (parser->signal)
    signal_info_add_arg (parser->signal, arg);
  else
    g_assert_not_reached ();

  g_free (generated_name);
  
  arg_info_unref (arg);

  parser->arg = arg;
  
  return TRUE;
}

static gboolean
parse_annotation (Parser      *parser,
		  const char  *element_name,
		  const char **attribute_names,
		  const char **attribute_values,
		  GError     **error)
{
  const char *name;
  const char *value;
  
  if (!(parser->method || parser->interface || parser->arg ||
        parser->property || parser->signal) ||
      parser->node_stack == NULL ||
      parser->in_annotation)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "Can't put <%s> element here",
                   element_name);
      return FALSE;      
    }

  name = NULL;
  if (!locate_attributes (element_name, attribute_names,
                          attribute_values, error,
                          "name", &name,
                          "value", &value,
                          NULL))
    return FALSE;

  if (name == NULL)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute required on <%s> element ",
                   "name", element_name);
      return FALSE;
    }
  if (value == NULL)
    {
      g_set_error (error, G_MARKUP_ERROR,
                   G_MARKUP_ERROR_PARSE,
                   "\"%s\" attribute required on <%s> element ",
                   "value", element_name);
      return FALSE;
    }

  if (parser->arg)
    arg_info_add_annotation (parser->arg, name, value);
  else if (parser->method)
    method_info_add_annotation (parser->method, name, value);
  else if (parser->interface)
    interface_info_add_annotation (parser->interface, name, value);
  else
    g_assert_not_reached ();

  parser->in_annotation = TRUE;

  return TRUE;
}

gboolean
parser_start_element (Parser      *parser,
                      const char  *element_name,
                      const char **attribute_names,
                      const char **attribute_values,
                      GError     **error)
{
  g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

  if (ELEMENT_IS ("node"))
    {
      if (!parse_node (parser, element_name, attribute_names,
                       attribute_values, error))
        return FALSE;
    }
  else if (ELEMENT_IS ("interface"))
    {
      if (!parse_interface (parser, element_name, attribute_names,
                            attribute_values, error))
        return FALSE;
    }
  else if (ELEMENT_IS ("method"))
    {
      if (!parse_method (parser, element_name, attribute_names,
                         attribute_values, error))
        return FALSE;
    }
  else if (ELEMENT_IS ("signal"))
    {
      if (!parse_signal (parser, element_name, attribute_names,
                         attribute_values, error))
        return FALSE;
    }
  else if (ELEMENT_IS ("property"))
    {
      if (!parse_property (parser, element_name, attribute_names,
                           attribute_values, error))
        return FALSE;
    }
  else if (ELEMENT_IS ("arg"))
    {
      if (!parse_arg (parser, element_name, attribute_names,
                      attribute_values, error))
        return FALSE;
    }
  else if (ELEMENT_IS ("annotation"))
    {
      if (!parse_annotation (parser, element_name, attribute_names,
			     attribute_values, error))
        return FALSE;
    }
  else
    {
      if (strchr (element_name, ':') != NULL)
        /* Passthrough XML-namespaced nodes */
        parser->unknown_namespaced_depth += 1;
      else if (parser->unknown_namespaced_depth == 0)
        {
          g_set_error (error, G_MARKUP_ERROR,
                       G_MARKUP_ERROR_PARSE,
                       "Element <%s> not recognized",
                       element_name);
          return FALSE;
        }
    }
  
  return TRUE;
}

gboolean
parser_end_element (Parser      *parser,
                    const char  *element_name,
                    GError     **error)
{
  g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

  if (ELEMENT_IS ("interface"))
    {
      parser->interface = NULL;
    }
  else if (ELEMENT_IS ("method"))
    {
      parser->method = NULL;
    }
  else if (ELEMENT_IS ("signal"))
    {
      parser->signal = NULL;
    }
  else if (ELEMENT_IS ("property"))
    {
      parser->property = NULL;
    }
  else if (ELEMENT_IS ("arg"))
    {
      parser->arg = NULL;
    }
  else if (ELEMENT_IS ("annotation"))
    {
      parser->in_annotation = FALSE;
    }
  else if (ELEMENT_IS ("node"))
    {
      NodeInfo *top;

      g_assert (parser->node_stack != NULL);
      top = parser->node_stack->data;

      parser->node_stack = g_slist_remove (parser->node_stack,
                                           top);

      if (parser->node_stack == NULL)
        parser->result = top; /* We are done, store the result */      
    }
  else if (strchr (element_name, ':') != NULL)
    {
      /* Passthrough XML-namespaced nodes */
      parser->unknown_namespaced_depth -= 1;
    }
  else if (parser->unknown_namespaced_depth > 0)
    {
		/* pass through unknown elements underneath a namespace */
    }
  else
    g_assert_not_reached (); /* should have had an error on start_element */
  
  return TRUE;
}

gboolean
parser_content (Parser      *parser,
                const char  *content,
                int          len,
                GError     **error)
{
  g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

  /* FIXME check that it's all whitespace */
  
  return TRUE;
}

gboolean
parser_finished (Parser      *parser,
                 GError     **error)
{
  g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

  return TRUE;
}

NodeInfo*
parser_get_nodes (Parser *parser)
{
  return parser->result;
}
