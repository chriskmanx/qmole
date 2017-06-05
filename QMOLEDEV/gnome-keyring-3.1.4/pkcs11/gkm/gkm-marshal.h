
#ifndef __gkm_marshal_MARSHAL_H__
#define __gkm_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* BOOLEAN:VOID (gkm-marshal.list:1) */
extern void gkm_marshal_BOOLEAN__VOID (GClosure     *closure,
                                       GValue       *return_value,
                                       guint         n_param_values,
                                       const GValue *param_values,
                                       gpointer      invocation_hint,
                                       gpointer      marshal_data);

/* VOID:STRING,ULONG (gkm-marshal.list:2) */
extern void gkm_marshal_VOID__STRING_ULONG (GClosure     *closure,
                                            GValue       *return_value,
                                            guint         n_param_values,
                                            const GValue *param_values,
                                            gpointer      invocation_hint,
                                            gpointer      marshal_data);

/* VOID:OBJECT,ULONG (gkm-marshal.list:3) */
extern void gkm_marshal_VOID__OBJECT_ULONG (GClosure     *closure,
                                            GValue       *return_value,
                                            guint         n_param_values,
                                            const GValue *param_values,
                                            gpointer      invocation_hint,
                                            gpointer      marshal_data);

G_END_DECLS

#endif /* __gkm_marshal_MARSHAL_H__ */

