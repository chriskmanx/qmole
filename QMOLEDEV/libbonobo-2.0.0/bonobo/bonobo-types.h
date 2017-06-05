/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * GRuntime types for CORBA Objects.
 *
 * Authors:
 *   Martin Baulig (baulig@suse.de)
 *
 * Copyright 2001 SuSE Linux AG.
 */
#ifndef _BONOBO_TYPES_H_
#define _BONOBO_TYPES_H_

#include <stdarg.h>
#include <glib/gmacros.h>
#include <glib-object.h>
#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-arg.h>

G_BEGIN_DECLS

GType bonobo_corba_object_type_register_static      (const gchar           *name,
                                                     const CORBA_TypeCode   tc,
						     gboolean               is_bonobo_unknown) G_GNUC_CONST;

GType bonobo_unknown_get_type                       (void) G_GNUC_CONST;
GType bonobo_corba_any_get_type                     (void) G_GNUC_CONST;
GType bonobo_corba_object_get_type                  (void) G_GNUC_CONST;
GType bonobo_corba_typecode_get_type                (void) G_GNUC_CONST;
GType bonobo_corba_exception_get_type               (void) G_GNUC_CONST;

#define BONOBO_TYPE_UNKNOWN                         (bonobo_unknown_get_type ())
#define BONOBO_TYPE_CORBA_ANY                       (bonobo_corba_any_get_type ())
#define BONOBO_TYPE_CORBA_OBJECT                    (bonobo_corba_object_get_type ())
#define BONOBO_TYPE_CORBA_TYPECODE                  (bonobo_corba_typecode_get_type ())
#define BONOBO_TYPE_CORBA_EXCEPTION                 (bonobo_corba_exception_get_type ())

#define BONOBO_TYPE_STATIC_UNKNOWN                  (bonobo_unknown_get_type () | G_SIGNAL_TYPE_STATIC_SCOPE)
#define BONOBO_TYPE_STATIC_CORBA_ANY                (bonobo_corba_any_get_type () | G_SIGNAL_TYPE_STATIC_SCOPE)
#define BONOBO_TYPE_STATIC_CORBA_OBJECT             (bonobo_corba_object_get_type () | G_SIGNAL_TYPE_STATIC_SCOPE)
#define BONOBO_TYPE_STATIC_CORBA_TYPECODE           (bonobo_corba_typecode_get_type () | G_SIGNAL_TYPE_STATIC_SCOPE)
#define BONOBO_TYPE_STATIC_CORBA_EXCEPTION          (bonobo_corba_exception_get_type () | G_SIGNAL_TYPE_STATIC_SCOPE)

#define BONOBO_VALUE_HOLDS_UNKNOWN(value)           (G_TYPE_CHECK_VALUE_TYPE ((value), BONOBO_TYPE_UNKNOWN))
#define BONOBO_VALUE_HOLDS_CORBA_ANY(value)         (G_TYPE_CHECK_VALUE_TYPE ((value), BONOBO_TYPE_CORBA_ANY))
#define BONOBO_VALUE_HOLDS_CORBA_OBJECT(value)      (G_TYPE_CHECK_VALUE_TYPE ((value), BONOBO_TYPE_CORBA_OBJECT))
#define BONOBO_VALUE_HOLDS_CORBA_TYPECODE(value)    (G_TYPE_CHECK_VALUE_TYPE ((value), BONOBO_TYPE_CORBA_TYPECODE))
#define BONOBO_VALUE_HOLDS_CORBA_EXCEPTION(value)   (G_TYPE_CHECK_VALUE_TYPE ((value), BONOBO_TYPE_CORBA_EXCEPTION))

Bonobo_Unknown           bonobo_value_get_unknown         (const GValue *value);
BonoboArg               *bonobo_value_get_corba_any       (const GValue *value);
CORBA_Object             bonobo_value_get_corba_object    (const GValue *value);
CORBA_TypeCode           bonobo_value_get_corba_typecode  (const GValue *value);
const CORBA_Environment *bonobo_value_get_corba_exception (const GValue *value);

void bonobo_value_set_corba_object       (GValue                      *value,
                                          const CORBA_Object           object);

void bonobo_value_set_unknown            (GValue                      *value,
                                          const Bonobo_Unknown         unknown);

void bonobo_value_set_corba_any          (GValue                      *value,
                                          const CORBA_any             *any);

void bonobo_value_set_corba_typecode     (GValue                      *value,
                                          const CORBA_TypeCode         tc);

void bonobo_value_set_corba_environment  (GValue                      *value,
                                          const CORBA_Environment     *ev);

void       bonobo_closure_invoke_va_list (GClosure            *closure,
					  GValue              *return_value,
					  va_list              var_args);

void       bonobo_closure_invoke         (GClosure            *closure,
					  GType                return_type,
					  ...);

GClosure * bonobo_closure_store          (GClosure            *closure,
					  GClosureMarshal      default_marshal);
						     
G_END_DECLS

#endif
