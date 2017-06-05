/*
 * bonobo-property-bag-client.c: C sugar for property bags.
 *
 * Author:
 *   Dietmar Maurer (dietmar@ximian.com)
 *   Michael Meeks  (michael@ximian.com)
 *   Nat Friedman   (nat@ximian.com)
 *
 * Copyright 2001 Ximian, Inc.
 */
#ifndef __BONOBO_PROPERTY_BAG_CLIENT_H__
#define __BONOBO_PROPERTY_BAG_CLIENT_H__

#include <bonobo/bonobo-property-bag.h>

G_BEGIN_DECLS

CORBA_TypeCode
bonobo_pbclient_get_type                 (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gchar *
bonobo_pbclient_get_string               (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gchar *
bonobo_pbclient_get_default_string       (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gchar *
bonobo_pbclient_get_string_with_default  (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gchar              *defval,
					  gboolean           *def);
gint16 
bonobo_pbclient_get_short                (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gint16 
bonobo_pbclient_get_default_short        (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gint16 
bonobo_pbclient_get_short_with_default   (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gint16              defval,
					  gboolean           *def);
guint16 
bonobo_pbclient_get_ushort               (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
guint16 
bonobo_pbclient_get_default_ushort       (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
guint16 
bonobo_pbclient_get_ushort_with_default  (Bonobo_PropertyBag  bag,
					  const char         *key,
					  guint16             defval,
					  gboolean           *def);
gint32 
bonobo_pbclient_get_long                 (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gint32 
bonobo_pbclient_get_default_long         (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gint32 
bonobo_pbclient_get_long_with_default    (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gint32              defval,
					  gboolean           *def);
guint32 
bonobo_pbclient_get_ulong                (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
guint32 
bonobo_pbclient_get_default_ulong        (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
guint32 
bonobo_pbclient_get_ulong_with_default   (Bonobo_PropertyBag  bag,
					  const char         *key,
					  guint32             defval,
					  gboolean           *def);
gfloat 
bonobo_pbclient_get_float                (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gfloat 
bonobo_pbclient_get_default_float        (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gfloat 
bonobo_pbclient_get_float_with_default   (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gfloat              defval,
					  gboolean           *def);
gdouble 
bonobo_pbclient_get_double               (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gdouble 
bonobo_pbclient_get_default_double       (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gdouble 
bonobo_pbclient_get_double_with_default  (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gdouble             defval,
					  gboolean           *def);
gboolean
bonobo_pbclient_get_boolean              (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gboolean
bonobo_pbclient_get_default_boolean      (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gboolean 
bonobo_pbclient_get_boolean_with_default (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gboolean            defval,
					  gboolean           *def);
gchar
bonobo_pbclient_get_char                 (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gchar
bonobo_pbclient_get_default_char         (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
gchar 
bonobo_pbclient_get_char_with_default    (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gchar               defval,
					  gboolean           *def);
CORBA_any *
bonobo_pbclient_get_value                (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_TypeCode      opt_tc,
					  CORBA_Environment  *opt_ev);

CORBA_any *
bonobo_pbclient_get_default_value        (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_TypeCode      opt_tc,
					  CORBA_Environment  *opt_ev);

void
bonobo_pbclient_set_string               (Bonobo_PropertyBag  bag,
					  const char         *key,
					  const char         *value,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_set_short                (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gint16              value,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_set_ushort               (Bonobo_PropertyBag  bag,
					  const char         *key,
					  guint16             value,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_set_long                 (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gint32              value,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_set_ulong                (Bonobo_PropertyBag  bag,
					  const char         *key,
					  guint32             value,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_set_float                (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gfloat              value,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_set_double               (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gdouble             value,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_set_boolean              (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gboolean            value,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_set_char                 (Bonobo_PropertyBag  bag,
					  const char         *key,
					  gchar               value,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_set_value                (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_any          *value,
					  CORBA_Environment  *opt_ev);
char *
bonobo_pbclient_get_doc_title            (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
char *
bonobo_pbclient_get_doc                  (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
GList *
bonobo_pbclient_get_keys                 (Bonobo_PropertyBag  bag,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_free_keys                (GList *key_list);

Bonobo_PropertyFlags
bonobo_pbclient_get_flags                (Bonobo_PropertyBag  bag,
					  const char         *key,
					  CORBA_Environment  *opt_ev);
void
bonobo_pbclient_set                      (Bonobo_PropertyBag  bag,
					  CORBA_Environment  *opt_ev,
					  const char         *first_prop,
					  ...);
void
bonobo_pbclient_get                      (Bonobo_PropertyBag  bag,
					  CORBA_Environment  *opt_ev,
					  const char         *first_prop,
					  ...);
char *
bonobo_pbclient_setv                     (Bonobo_PropertyBag  bag,
					  CORBA_Environment  *ev,
					  const char         *first_prop,
					  va_list             var_args);
char *
bonobo_pbclient_getv                     (Bonobo_PropertyBag  bag,
					  CORBA_Environment  *ev,
					  const char         *first_prop,
					  va_list             var_args);

/* just to be compatible */

#define bonobo_property_bag_client_setv                                       \
bonobo_pbclient_setv    
#define bonobo_property_bag_client_getv                                       \
bonobo_pbclient_getv
#define bonobo_property_bag_client_get_property_type                          \
bonobo_pbclient_get_type
#define bonobo_property_bag_client_get_value_gboolean                         \
bonobo_pbclient_get_boolean
#define bonobo_property_bag_client_get_value_gint                             \
bonobo_pbclient_get_long
#define bonobo_property_bag_client_get_value_glong                            \
bonobo_pbclient_get_long 
#define bonobo_property_bag_client_get_value_gfloat                           \
bonobo_pbclient_get_float 
#define bonobo_property_bag_client_get_value_gdouble                          \
bonobo_pbclient_get_double
#define bonobo_property_bag_client_get_value_string                           \
bonobo_pbclient_get_string

#define bonobo_property_bag_client_get_value_any(pb, name, ev)                \
bonobo_pbclient_get_value (pb, name, NULL, ev);

#define bonobo_property_bag_client_get_default_gboolean                       \
bonobo_pbclient_get_default_boolean
#define bonobo_property_bag_client_get_default_gint                           \
bonobo_pbclient_get_default_long
#define bonobo_property_bag_client_get_default_glong                          \
bonobo_pbclient_get_default_long
#define bonobo_property_bag_client_get_default_gfloat                         \
bonobo_pbclient_get_default_float
#define bonobo_property_bag_client_get_default_gdouble                        \
bonobo_pbclient_get_default_double
#define bonobo_property_bag_client_get_default_string                         \
bonobo_pbclient_get_default_string

#define bonobo_property_bag_client_get_default_any(pb, name, ev)              \
bonobo_pbclient_get_default_value (pb, name, NULL, ev)

#define bonobo_property_bag_client_set_value_gboolean                         \
bonobo_pbclient_set_boolean   
#define bonobo_property_bag_client_set_value_gint                             \
bonobo_pbclient_set_long
#define bonobo_property_bag_client_set_value_glong                            \
bonobo_pbclient_set_long
#define bonobo_property_bag_client_set_value_gfloat                           \
bonobo_pbclient_set_float
#define bonobo_property_bag_client_set_value_gdouble                          \
bonobo_pbclient_set_double
#define bonobo_property_bag_client_set_value_string                           \
bonobo_pbclient_set_string
#define bonobo_property_bag_client_set_value_any                              \
bonobo_pbclient_set_value 

#define bonobo_property_bag_client_get_docstring                              \
bonobo_pbclient_get_doc_title
#define bonobo_property_bag_client_get_flags	                              \
bonobo_pbclient_get_flags


G_END_DECLS

#endif /* __BONOBO_PROPERTY_BAG_CLIENT_H__ */
