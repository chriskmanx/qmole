
#ifndef ___gcr_marshal_MARSHAL_H__
#define ___gcr_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* BOOLEAN:INT (gcr-marshal.list:1) */
extern void _gcr_marshal_BOOLEAN__INT (GClosure     *closure,
                                       GValue       *return_value,
                                       guint         n_param_values,
                                       const GValue *param_values,
                                       gpointer      invocation_hint,
                                       gpointer      marshal_data);

/* BOOLEAN:BOXED (gcr-marshal.list:2) */
extern void _gcr_marshal_BOOLEAN__BOXED (GClosure     *closure,
                                         GValue       *return_value,
                                         guint         n_param_values,
                                         const GValue *param_values,
                                         gpointer      invocation_hint,
                                         gpointer      marshal_data);

/* VOID:STRING,BOXED (gcr-marshal.list:3) */
extern void _gcr_marshal_VOID__STRING_BOXED (GClosure     *closure,
                                             GValue       *return_value,
                                             guint         n_param_values,
                                             const GValue *param_values,
                                             gpointer      invocation_hint,
                                             gpointer      marshal_data);

/* VOID:BOXED (gcr-marshal.list:4) */
#define _gcr_marshal_VOID__BOXED	g_cclosure_marshal_VOID__BOXED

/* VOID:STRING (gcr-marshal.list:5) */
#define _gcr_marshal_VOID__STRING	g_cclosure_marshal_VOID__STRING

G_END_DECLS

#endif /* ___gcr_marshal_MARSHAL_H__ */

