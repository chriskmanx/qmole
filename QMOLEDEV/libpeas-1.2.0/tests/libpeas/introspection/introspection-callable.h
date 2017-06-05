/*
 * introspection-callable.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010 - Garrett Regier
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

#ifndef __INTROSPECTION_CALLABLE_H__
#define __INTROSPECTION_CALLABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define INTROSPECTION_TYPE_CALLABLE             (introspection_callable_get_type ())
#define INTROSPECTION_CALLABLE(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), INTROSPECTION_TYPE_CALLABLE, IntrospectionCallable))
#define INTROSPECTION_CALLABLE_IFACE(obj)       (G_TYPE_CHECK_CLASS_CAST ((obj), INTROSPECTION_TYPE_CALLABLE, IntrospectionCallableInterface))
#define INTROSPECTION_IS_CALLABLE(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), INTROSPECTION_TYPE_CALLABLE))
#define INTROSPECTION_CALLABLE_GET_IFACE(obj)   (G_TYPE_INSTANCE_GET_INTERFACE ((obj), INTROSPECTION_TYPE_CALLABLE, IntrospectionCallableInterface))

typedef struct _IntrospectionCallable           IntrospectionCallable; /* dummy typedef */
typedef struct _IntrospectionCallableInterface  IntrospectionCallableInterface;

struct _IntrospectionCallableInterface {
  GTypeInterface g_iface;

  /* Virtual public methods */
  const gchar *(*call_with_return) (IntrospectionCallable *callable);
  void         (*call_no_args)     (IntrospectionCallable *callable);
  void         (*call_single_arg)  (IntrospectionCallable *callable,
                                    gboolean              *called);
  void         (*call_multi_args)  (IntrospectionCallable *callable,
                                    gint                   in,
                                    gint                  *out,
                                    gint                  *inout);

  /* libpeas must have an invoker to implement an interface's vfunc */
  void         (*no_invoker_)      (IntrospectionCallable *callable);
};

/*
 * Public methods
 */
GType        introspection_callable_get_type         (void) G_GNUC_CONST;

const gchar *introspection_callable_call_with_return (IntrospectionCallable *callable);

void         introspection_callable_call_no_args     (IntrospectionCallable *callable);
void         introspection_callable_call_single_arg  (IntrospectionCallable *callable,
                                                      gboolean              *called);
void         introspection_callable_call_multi_args  (IntrospectionCallable *callable,
                                                      gint                   in,
                                                      gint                  *out,
                                                      gint                  *inout);

G_END_DECLS

#endif /* __INTROSPECTION_CALLABLE_H__ */
