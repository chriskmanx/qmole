
#ifndef __claws_marshal_MARSHAL_H__
#define __claws_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* NONE:POINTER (claws-marshal.list:1) */
#define claws_marshal_VOID__POINTER	g_cclosure_marshal_VOID__POINTER
#define claws_marshal_NONE__POINTER	claws_marshal_VOID__POINTER

/* NONE:INT,POINTER (claws-marshal.list:2) */
extern void claws_marshal_VOID__INT_POINTER (GClosure     *closure,
                                             GValue       *return_value,
                                             guint         n_param_values,
                                             const GValue *param_values,
                                             gpointer      invocation_hint,
                                             gpointer      marshal_data);
#define claws_marshal_NONE__INT_POINTER	claws_marshal_VOID__INT_POINTER

/* NONE:OBJECT,OBJECT (claws-marshal.list:3) */
extern void claws_marshal_VOID__OBJECT_OBJECT (GClosure     *closure,
                                               GValue       *return_value,
                                               guint         n_param_values,
                                               const GValue *param_values,
                                               gpointer      invocation_hint,
                                               gpointer      marshal_data);
#define claws_marshal_NONE__OBJECT_OBJECT	claws_marshal_VOID__OBJECT_OBJECT

/* NONE:INT,INT,BOXED (claws-marshal.list:4) */
extern void claws_marshal_VOID__INT_INT_BOXED (GClosure     *closure,
                                               GValue       *return_value,
                                               guint         n_param_values,
                                               const GValue *param_values,
                                               gpointer      invocation_hint,
                                               gpointer      marshal_data);
#define claws_marshal_NONE__INT_INT_BOXED	claws_marshal_VOID__INT_INT_BOXED

/* NONE:INT,INT (claws-marshal.list:5) */
extern void claws_marshal_VOID__INT_INT (GClosure     *closure,
                                         GValue       *return_value,
                                         guint         n_param_values,
                                         const GValue *param_values,
                                         gpointer      invocation_hint,
                                         gpointer      marshal_data);
#define claws_marshal_NONE__INT_INT	claws_marshal_VOID__INT_INT

/* NONE:INT (claws-marshal.list:6) */
#define claws_marshal_VOID__INT	g_cclosure_marshal_VOID__INT
#define claws_marshal_NONE__INT	claws_marshal_VOID__INT

/* NONE:VOID (claws-marshal.list:7) */
#define claws_marshal_VOID__VOID	g_cclosure_marshal_VOID__VOID
#define claws_marshal_NONE__VOID	claws_marshal_VOID__VOID

/* NONE:VOID,VOID (claws-marshal.list:8) */
extern void claws_marshal_VOID__VOID_VOID (GClosure     *closure,
                                           GValue       *return_value,
                                           guint         n_param_values,
                                           const GValue *param_values,
                                           gpointer      invocation_hint,
                                           gpointer      marshal_data);
#define claws_marshal_NONE__VOID_VOID	claws_marshal_VOID__VOID_VOID

/* NONE:ENUM,FLOAT,BOOLEAN (claws-marshal.list:9) */
extern void claws_marshal_VOID__ENUM_FLOAT_BOOLEAN (GClosure     *closure,
                                                    GValue       *return_value,
                                                    guint         n_param_values,
                                                    const GValue *param_values,
                                                    gpointer      invocation_hint,
                                                    gpointer      marshal_data);
#define claws_marshal_NONE__ENUM_FLOAT_BOOLEAN	claws_marshal_VOID__ENUM_FLOAT_BOOLEAN

/* NONE:ENUM,FLOAT (claws-marshal.list:10) */
extern void claws_marshal_VOID__ENUM_FLOAT (GClosure     *closure,
                                            GValue       *return_value,
                                            guint         n_param_values,
                                            const GValue *param_values,
                                            gpointer      invocation_hint,
                                            gpointer      marshal_data);
#define claws_marshal_NONE__ENUM_FLOAT	claws_marshal_VOID__ENUM_FLOAT

/* NONE:POINTER,INT (claws-marshal.list:11) */
extern void claws_marshal_VOID__POINTER_INT (GClosure     *closure,
                                             GValue       *return_value,
                                             guint         n_param_values,
                                             const GValue *param_values,
                                             gpointer      invocation_hint,
                                             gpointer      marshal_data);
#define claws_marshal_NONE__POINTER_INT	claws_marshal_VOID__POINTER_INT

/* NONE:POINTER,POINTER,POINTER (claws-marshal.list:12) */
extern void claws_marshal_VOID__POINTER_POINTER_POINTER (GClosure     *closure,
                                                         GValue       *return_value,
                                                         guint         n_param_values,
                                                         const GValue *param_values,
                                                         gpointer      invocation_hint,
                                                         gpointer      marshal_data);
#define claws_marshal_NONE__POINTER_POINTER_POINTER	claws_marshal_VOID__POINTER_POINTER_POINTER

/* NONE:ENUM (claws-marshal.list:13) */
#define claws_marshal_VOID__ENUM	g_cclosure_marshal_VOID__ENUM
#define claws_marshal_NONE__ENUM	claws_marshal_VOID__ENUM

G_END_DECLS

#endif /* __claws_marshal_MARSHAL_H__ */

