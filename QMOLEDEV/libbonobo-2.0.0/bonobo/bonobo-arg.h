/*
 * bonobo-arg.h Bonobo argument support:
 *
 *  A thin wrapper of CORBA_any's with macros
 * to assist in handling values safely.
 *
 * Author:
 *    Michael Meeks (michael@helixcode.com)
 *
 * Copyright 2000, Helix Code, Inc.
 */
#ifndef __BONOBO_ARG_H__
#define __BONOBO_ARG_H__

#include <bonobo/Bonobo.h>

#include <glib-object.h>

G_BEGIN_DECLS

typedef CORBA_any      BonoboArg;
typedef CORBA_TypeCode BonoboArgType;

#define BONOBO_ARG_NULL     TC_null
#define BONOBO_ARG_BOOLEAN  TC_CORBA_boolean
#define BONOBO_ARG_SHORT    TC_CORBA_short
#define BONOBO_ARG_INT      TC_CORBA_long
#define BONOBO_ARG_LONG     TC_CORBA_long
#define BONOBO_ARG_LONGLONG TC_CORBA_long_long
#define BONOBO_ARG_FLOAT    TC_CORBA_float
#define BONOBO_ARG_DOUBLE   TC_CORBA_double
#define BONOBO_ARG_CHAR     TC_CORBA_char
#define BONOBO_ARG_STRING   TC_CORBA_string

#ifdef __GNUC__
#	define BONOBO_ARG_GET_GENERAL(a,c,t,e)   (g_assert (bonobo_arg_type_is_equal ((a)->_type, c, e)),\
					          *((t *)((a)->_value)))
#	define BONOBO_ARG_SET_GENERAL(a,v,c,t,e) (g_assert (bonobo_arg_type_is_equal ((a)->_type, c, e)),\
					          *((t *)((a)->_value)) = (t)(v))
#else
#	define BONOBO_ARG_GET_GENERAL(a,c,t,e)   (*((t *)((a)->_value)))
#	define BONOBO_ARG_SET_GENERAL(a,v,c,t,e) (*((t *)((a)->_value)) = (v))
#endif

#define BONOBO_ARG_GET_BOOLEAN(a)   (BONOBO_ARG_GET_GENERAL (a, TC_CORBA_boolean, CORBA_boolean, NULL))
#define BONOBO_ARG_SET_BOOLEAN(a,v) (BONOBO_ARG_SET_GENERAL (a, v, TC_CORBA_boolean, CORBA_boolean, NULL))

#define BONOBO_ARG_GET_SHORT(a)     (BONOBO_ARG_GET_GENERAL (a, TC_CORBA_short, CORBA_short, NULL))
#define BONOBO_ARG_SET_SHORT(a,v)   (BONOBO_ARG_SET_GENERAL (a, v, TC_CORBA_short, CORBA_short, NULL))
#define BONOBO_ARG_GET_INT(a)       (BONOBO_ARG_GET_GENERAL (a, TC_CORBA_long, CORBA_long, NULL))
#define BONOBO_ARG_SET_INT(a,v)     (BONOBO_ARG_SET_GENERAL (a, v, TC_CORBA_long, CORBA_long, NULL))
#define BONOBO_ARG_GET_LONG(a)      (BONOBO_ARG_GET_GENERAL (a, TC_CORBA_long, CORBA_long, NULL))
#define BONOBO_ARG_SET_LONG(a,v)    (BONOBO_ARG_SET_GENERAL (a, v, TC_CORBA_long, CORBA_long, NULL))
#define BONOBO_ARG_GET_LONGLONG(a)  (BONOBO_ARG_GET_GENERAL (a, TC_CORBA_long_long, CORBA_long_long, NULL))
#define BONOBO_ARG_SET_LONGLONG(a,v) (BONOBO_ARG_SET_GENERAL (a, v, TC_CORBA_long_long, CORBA_long_long, NULL))

#define BONOBO_ARG_GET_FLOAT(a)     (BONOBO_ARG_GET_GENERAL (a, TC_CORBA_float, CORBA_float, NULL))
#define BONOBO_ARG_SET_FLOAT(a,v)   (BONOBO_ARG_SET_GENERAL (a, v, TC_CORBA_float, CORBA_float, NULL))

#define BONOBO_ARG_GET_DOUBLE(a)    (BONOBO_ARG_GET_GENERAL (a, TC_CORBA_double, CORBA_double, NULL))
#define BONOBO_ARG_SET_DOUBLE(a,v)  (BONOBO_ARG_SET_GENERAL (a, v, TC_CORBA_double, CORBA_double, NULL))

#define BONOBO_ARG_GET_CHAR(a)      (BONOBO_ARG_GET_GENERAL (a, TC_CORBA_char, CORBA_char, NULL))
#define BONOBO_ARG_SET_CHAR(a,v)    (BONOBO_ARG_SET_GENERAL (a, v, TC_CORBA_char, CORBA_char, NULL))

#ifdef __GNUC__
#define BONOBO_ARG_GET_STRING(a)    (g_assert ((a)->_type->kind == CORBA_tk_string),\
				     *((CORBA_char **)(a->_value)))
#define BONOBO_ARG_SET_STRING(a,v)  (g_assert ((a)->_type->kind == CORBA_tk_string), CORBA_free (*(char **)a->_value),\
				     *((CORBA_char **)(a->_value)) = CORBA_string_dup ((v)?(v):""))
#else
#define BONOBO_ARG_GET_STRING(a)    (*((CORBA_char **)(a->_value)))
#define BONOBO_ARG_SET_STRING(a,v)  (CORBA_free (*(char **)a->_value),\
				     *((CORBA_char **)(a->_value)) = CORBA_string_dup ((v)?(v):""))
#endif

BonoboArg    *bonobo_arg_new             (BonoboArgType      t);

BonoboArg    *bonobo_arg_new_from        (BonoboArgType      t,
					  gconstpointer      data);

void          bonobo_arg_release         (BonoboArg         *arg);

BonoboArg    *bonobo_arg_copy            (const BonoboArg   *arg);

void          bonobo_arg_from_gvalue     (BonoboArg         *a, 
					   const GValue      *value);
BonoboArgType bonobo_arg_type_from_gtype (GType              t);

void          bonobo_arg_to_gvalue       (GValue            *value, 
					  const BonoboArg   *arg);

GType         bonobo_arg_type_to_gtype   (BonoboArgType      id);

gboolean      bonobo_arg_is_equal        (const BonoboArg   *a, 
					  const BonoboArg   *b, 
					  CORBA_Environment *opt_ev);

gboolean      bonobo_arg_type_is_equal   (BonoboArgType      a, 
					  BonoboArgType      b,
					  CORBA_Environment *opt_ev);

G_END_DECLS

#endif /* ! __BONOBO_ARG_H__ */
