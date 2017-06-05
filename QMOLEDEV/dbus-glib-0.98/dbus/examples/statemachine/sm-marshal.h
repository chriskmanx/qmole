
#ifndef __sm_marshal_MARSHAL_H__
#define __sm_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* VOID:STRING,BOXED (./sm-marshal.list:1) */
extern void sm_marshal_VOID__STRING_BOXED (GClosure     *closure,
                                           GValue       *return_value,
                                           guint         n_param_values,
                                           const GValue *param_values,
                                           gpointer      invocation_hint,
                                           gpointer      marshal_data);

G_END_DECLS

#endif /* __sm_marshal_MARSHAL_H__ */

