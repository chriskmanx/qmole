
#ifndef ___gtksourceview_marshal_MARSHAL_H__
#define ___gtksourceview_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* VOID:VOID (gtksourceview-marshal.list:1) */
#define _gtksourceview_marshal_VOID__VOID	g_cclosure_marshal_VOID__VOID

/* VOID:BOOLEAN (gtksourceview-marshal.list:2) */
#define _gtksourceview_marshal_VOID__BOOLEAN	g_cclosure_marshal_VOID__BOOLEAN

/* VOID:BOOLEAN,INT (gtksourceview-marshal.list:3) */
extern void _gtksourceview_marshal_VOID__BOOLEAN_INT (GClosure     *closure,
                                                      GValue       *return_value,
                                                      guint         n_param_values,
                                                      const GValue *param_values,
                                                      gpointer      invocation_hint,
                                                      gpointer      marshal_data);

/* VOID:BOXED (gtksourceview-marshal.list:4) */
#define _gtksourceview_marshal_VOID__BOXED	g_cclosure_marshal_VOID__BOXED

/* VOID:BOXED,BOXED (gtksourceview-marshal.list:5) */
extern void _gtksourceview_marshal_VOID__BOXED_BOXED (GClosure     *closure,
                                                      GValue       *return_value,
                                                      guint         n_param_values,
                                                      const GValue *param_values,
                                                      gpointer      invocation_hint,
                                                      gpointer      marshal_data);

/* VOID:INT,INT (gtksourceview-marshal.list:6) */
extern void _gtksourceview_marshal_VOID__INT_INT (GClosure     *closure,
                                                  GValue       *return_value,
                                                  guint         n_param_values,
                                                  const GValue *param_values,
                                                  gpointer      invocation_hint,
                                                  gpointer      marshal_data);

/* VOID:STRING (gtksourceview-marshal.list:7) */
#define _gtksourceview_marshal_VOID__STRING	g_cclosure_marshal_VOID__STRING

/* BOOLEAN:POINTER (gtksourceview-marshal.list:8) */
extern void _gtksourceview_marshal_BOOLEAN__POINTER (GClosure     *closure,
                                                     GValue       *return_value,
                                                     guint         n_param_values,
                                                     const GValue *param_values,
                                                     gpointer      invocation_hint,
                                                     gpointer      marshal_data);

/* BOOLEAN:VOID (gtksourceview-marshal.list:9) */
extern void _gtksourceview_marshal_BOOLEAN__VOID (GClosure     *closure,
                                                  GValue       *return_value,
                                                  guint         n_param_values,
                                                  const GValue *param_values,
                                                  gpointer      invocation_hint,
                                                  gpointer      marshal_data);

/* BOOLEAN:OBJECT (gtksourceview-marshal.list:10) */
extern void _gtksourceview_marshal_BOOLEAN__OBJECT (GClosure     *closure,
                                                    GValue       *return_value,
                                                    guint         n_param_values,
                                                    const GValue *param_values,
                                                    gpointer      invocation_hint,
                                                    gpointer      marshal_data);

/* BOOLEAN:OBJECT,OBJECT (gtksourceview-marshal.list:11) */
extern void _gtksourceview_marshal_BOOLEAN__OBJECT_OBJECT (GClosure     *closure,
                                                           GValue       *return_value,
                                                           guint         n_param_values,
                                                           const GValue *param_values,
                                                           gpointer      invocation_hint,
                                                           gpointer      marshal_data);

/* VOID:ENUM,INT (gtksourceview-marshal.list:12) */
extern void _gtksourceview_marshal_VOID__ENUM_INT (GClosure     *closure,
                                                   GValue       *return_value,
                                                   guint         n_param_values,
                                                   const GValue *param_values,
                                                   gpointer      invocation_hint,
                                                   gpointer      marshal_data);

/* VOID:OBJECT,BOXED,POINTER (gtksourceview-marshal.list:13) */
extern void _gtksourceview_marshal_VOID__OBJECT_BOXED_POINTER (GClosure     *closure,
                                                               GValue       *return_value,
                                                               guint         n_param_values,
                                                               const GValue *param_values,
                                                               gpointer      invocation_hint,
                                                               gpointer      marshal_data);

/* BOOL:OBJECT,BOXED,OBJECT (gtksourceview-marshal.list:14) */
extern void _gtksourceview_marshal_BOOLEAN__OBJECT_BOXED_OBJECT (GClosure     *closure,
                                                                 GValue       *return_value,
                                                                 guint         n_param_values,
                                                                 const GValue *param_values,
                                                                 gpointer      invocation_hint,
                                                                 gpointer      marshal_data);
#define _gtksourceview_marshal_BOOL__OBJECT_BOXED_OBJECT	_gtksourceview_marshal_BOOLEAN__OBJECT_BOXED_OBJECT

/* VOID:BOXED,POINTER (gtksourceview-marshal.list:15) */
extern void _gtksourceview_marshal_VOID__BOXED_POINTER (GClosure     *closure,
                                                        GValue       *return_value,
                                                        guint         n_param_values,
                                                        const GValue *param_values,
                                                        gpointer      invocation_hint,
                                                        gpointer      marshal_data);

G_END_DECLS

#endif /* ___gtksourceview_marshal_MARSHAL_H__ */

