
#ifndef __go__MARSHAL_H__
#define __go__MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* BOOLEAN:OBJECT (../../../../goffice/goffice/utils/go-marshalers.list:23) */
extern void go__BOOLEAN__OBJECT (GClosure     *closure,
                                 GValue       *return_value,
                                 guint         n_param_values,
                                 const GValue *param_values,
                                 gpointer      invocation_hint,
                                 gpointer      marshal_data);

/* BOOLEAN:POINTER (../../../../goffice/goffice/utils/go-marshalers.list:24) */
extern void go__BOOLEAN__POINTER (GClosure     *closure,
                                  GValue       *return_value,
                                  guint         n_param_values,
                                  const GValue *param_values,
                                  gpointer      invocation_hint,
                                  gpointer      marshal_data);

/* STRING:POINTER (../../../../goffice/goffice/utils/go-marshalers.list:25) */
extern void go__STRING__POINTER (GClosure     *closure,
                                 GValue       *return_value,
                                 guint         n_param_values,
                                 const GValue *param_values,
                                 gpointer      invocation_hint,
                                 gpointer      marshal_data);

/* VOID:INT,BOOLEAN,BOOLEAN,BOOLEAN (../../../../goffice/goffice/utils/go-marshalers.list:26) */
extern void go__VOID__INT_BOOLEAN_BOOLEAN_BOOLEAN (GClosure     *closure,
                                                   GValue       *return_value,
                                                   guint         n_param_values,
                                                   const GValue *param_values,
                                                   gpointer      invocation_hint,
                                                   gpointer      marshal_data);

G_END_DECLS

#endif /* __go__MARSHAL_H__ */

