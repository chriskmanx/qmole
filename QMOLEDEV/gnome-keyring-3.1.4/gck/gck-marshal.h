
#ifndef ___gck_marshal_MARSHAL_H__
#define ___gck_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* BOOLEAN:STRING,POINTER (gck-marshal.list:1) */
extern void _gck_marshal_BOOLEAN__STRING_POINTER (GClosure     *closure,
                                                  GValue       *return_value,
                                                  guint         n_param_values,
                                                  const GValue *param_values,
                                                  gpointer      invocation_hint,
                                                  gpointer      marshal_data);

/* BOOLEAN:OBJECT,STRING,POINTER (gck-marshal.list:2) */
extern void _gck_marshal_BOOLEAN__OBJECT_STRING_POINTER (GClosure     *closure,
                                                         GValue       *return_value,
                                                         guint         n_param_values,
                                                         const GValue *param_values,
                                                         gpointer      invocation_hint,
                                                         gpointer      marshal_data);

/* BOOLEAN:ULONG (gck-marshal.list:3) */
extern void _gck_marshal_BOOLEAN__ULONG (GClosure     *closure,
                                         GValue       *return_value,
                                         guint         n_param_values,
                                         const GValue *param_values,
                                         gpointer      invocation_hint,
                                         gpointer      marshal_data);

G_END_DECLS

#endif /* ___gck_marshal_MARSHAL_H__ */

