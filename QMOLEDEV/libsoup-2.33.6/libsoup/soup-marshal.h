
#ifndef __soup_marshal_MARSHAL_H__
#define __soup_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* NONE:BOXED (./soup-marshal.list:1) */
#define soup_marshal_VOID__BOXED	g_cclosure_marshal_VOID__BOXED
#define soup_marshal_NONE__BOXED	soup_marshal_VOID__BOXED

/* NONE:INT (./soup-marshal.list:2) */
#define soup_marshal_VOID__INT	g_cclosure_marshal_VOID__INT
#define soup_marshal_NONE__INT	soup_marshal_VOID__INT

/* NONE:NONE (./soup-marshal.list:3) */
#define soup_marshal_VOID__VOID	g_cclosure_marshal_VOID__VOID
#define soup_marshal_NONE__NONE	soup_marshal_VOID__VOID

/* NONE:OBJECT (./soup-marshal.list:4) */
#define soup_marshal_VOID__OBJECT	g_cclosure_marshal_VOID__OBJECT
#define soup_marshal_NONE__OBJECT	soup_marshal_VOID__OBJECT

/* NONE:OBJECT,OBJECT (./soup-marshal.list:5) */
extern void soup_marshal_VOID__OBJECT_OBJECT (GClosure     *closure,
                                              GValue       *return_value,
                                              guint         n_param_values,
                                              const GValue *param_values,
                                              gpointer      invocation_hint,
                                              gpointer      marshal_data);
#define soup_marshal_NONE__OBJECT_OBJECT	soup_marshal_VOID__OBJECT_OBJECT

/* NONE:OBJECT,POINTER (./soup-marshal.list:6) */
extern void soup_marshal_VOID__OBJECT_POINTER (GClosure     *closure,
                                               GValue       *return_value,
                                               guint         n_param_values,
                                               const GValue *param_values,
                                               gpointer      invocation_hint,
                                               gpointer      marshal_data);
#define soup_marshal_NONE__OBJECT_POINTER	soup_marshal_VOID__OBJECT_POINTER

/* NONE:BOXED,BOXED (./soup-marshal.list:7) */
extern void soup_marshal_VOID__BOXED_BOXED (GClosure     *closure,
                                            GValue       *return_value,
                                            guint         n_param_values,
                                            const GValue *param_values,
                                            gpointer      invocation_hint,
                                            gpointer      marshal_data);
#define soup_marshal_NONE__BOXED_BOXED	soup_marshal_VOID__BOXED_BOXED

/* NONE:OBJECT,OBJECT,BOOLEAN (./soup-marshal.list:8) */
extern void soup_marshal_VOID__OBJECT_OBJECT_BOOLEAN (GClosure     *closure,
                                                      GValue       *return_value,
                                                      guint         n_param_values,
                                                      const GValue *param_values,
                                                      gpointer      invocation_hint,
                                                      gpointer      marshal_data);
#define soup_marshal_NONE__OBJECT_OBJECT_BOOLEAN	soup_marshal_VOID__OBJECT_OBJECT_BOOLEAN

/* NONE:STRING,BOXED (./soup-marshal.list:9) */
extern void soup_marshal_VOID__STRING_BOXED (GClosure     *closure,
                                             GValue       *return_value,
                                             guint         n_param_values,
                                             const GValue *param_values,
                                             gpointer      invocation_hint,
                                             gpointer      marshal_data);
#define soup_marshal_NONE__STRING_BOXED	soup_marshal_VOID__STRING_BOXED

/* NONE:STRING,STRING (./soup-marshal.list:10) */
extern void soup_marshal_VOID__STRING_STRING (GClosure     *closure,
                                              GValue       *return_value,
                                              guint         n_param_values,
                                              const GValue *param_values,
                                              gpointer      invocation_hint,
                                              gpointer      marshal_data);
#define soup_marshal_NONE__STRING_STRING	soup_marshal_VOID__STRING_STRING

G_END_DECLS

#endif /* __soup_marshal_MARSHAL_H__ */

