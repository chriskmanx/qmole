
#ifndef __gconf_marshal_MARSHAL_H__
#define __gconf_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* VOID:POINTER (gconfmarshal.list:1) */
#define gconf_marshal_VOID__POINTER	g_cclosure_marshal_VOID__POINTER

/* VOID:STRING,POINTER (gconfmarshal.list:2) */
extern void gconf_marshal_VOID__STRING_POINTER (GClosure     *closure,
                                                GValue       *return_value,
                                                guint         n_param_values,
                                                const GValue *param_values,
                                                gpointer      invocation_hint,
                                                gpointer      marshal_data);

G_END_DECLS

#endif /* __gconf_marshal_MARSHAL_H__ */

