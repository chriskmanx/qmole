
#ifndef __gq_marshal_MARSHAL_H__
#define __gq_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* VOID:VOID (gq-marshal.list:1) */
#define gq_marshal_VOID__VOID	g_cclosure_marshal_VOID__VOID

/* VOID:INT,INT,INT,INT (gq-marshal.list:2) */
extern void gq_marshal_VOID__INT_INT_INT_INT (GClosure     *closure,
                                              GValue       *return_value,
                                              guint         n_param_values,
                                              const GValue *param_values,
                                              gpointer      invocation_hint,
                                              gpointer      marshal_data);

extern void gq_marshal_VOID__INT_INT (GClosure     *closure,
		GValue       *return_value,
		guint         n_param_values,
		const GValue *param_values,
		gpointer      invocation_hint,
		gpointer      marshal_data);

/* VOID:DOUBLE (gq-marshal.list:3) */
#define gq_marshal_VOID__DOUBLE	g_cclosure_marshal_VOID__DOUBLE

G_END_DECLS

#endif /* __gq_marshal_MARSHAL_H__ */

/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
