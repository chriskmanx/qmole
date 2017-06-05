
#ifndef ___gtk_marshal_MARSHAL_H__
#define ___gtk_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* OBJECT:VOID (./marshalers.list:1) */
extern void _gtk_marshal_OBJECT__VOID (GClosure     *closure,
                                       GValue       *return_value,
                                       guint         n_param_values,
                                       const GValue *param_values,
                                       gpointer      invocation_hint,
                                       gpointer      marshal_data);

/* VOID:ENUM,OBJECT,DOUBLE (./marshalers.list:2) */
extern void _gtk_marshal_VOID__ENUM_OBJECT_DOUBLE (GClosure     *closure,
                                                   GValue       *return_value,
                                                   guint         n_param_values,
                                                   const GValue *param_values,
                                                   gpointer      invocation_hint,
                                                   gpointer      marshal_data);

G_END_DECLS

#endif /* ___gtk_marshal_MARSHAL_H__ */

