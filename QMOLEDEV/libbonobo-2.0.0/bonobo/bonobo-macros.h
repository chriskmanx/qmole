/**
 * Useful macros.
 *
 * Author:
 *   Darin Adler <darin@bentspoon.com>
 *
 * Copyright 2001 Ben Tea Spoons, Inc.
 */
#ifndef _BONOBO_MACROS_H_
#define _BONOBO_MACROS_H_

#include <glib/gmacros.h>

G_BEGIN_DECLS

/* Macros for defining classes.  Ideas taken from Nautilus and GOB. */

/* Define the boilerplate type stuff to reduce typos and code size.  Defines
 * the get_type method and the parent_class static variable. */

#define BONOBO_BOILERPLATE(type, type_as_function, corba_type,		\
			   parent_type, parent_type_macro,		\
			   register_type_macro)				\
static void type_as_function ## _class_init    (type ## Class *klass);	\
static void type_as_function ## _instance_init (type          *object);	\
static parent_type ## Class *parent_class = NULL;			\
static void								\
type_as_function ## _class_init_trampoline (gpointer klass,		\
					    gpointer data)		\
{									\
	parent_class = g_type_class_ref (parent_type_macro);		\
	type_as_function ## _class_init (klass);			\
}									\
GType									\
type_as_function ## _get_type (void)					\
{									\
	static GType object_type = 0;					\
	if (object_type == 0) {						\
		static const GTypeInfo object_info = {			\
		    sizeof (type ## Class),				\
		    NULL,		/* base_init */			\
		    NULL,		/* base_finalize */		\
		    type_as_function ## _class_init_trampoline,		\
		    NULL,		/* class_finalize */		\
		    NULL,               /* class_data */		\
		    sizeof (type),					\
		    0,                  /* n_preallocs */		\
		    (GInstanceInitFunc) type_as_function ## _instance_init \
		};							\
		object_type = register_type_macro			\
			(type, type_as_function, corba_type,		\
			 parent_type, parent_type_macro);		\
	}								\
	return object_type;						\
}

/* Just call the parent handler.  This assumes that there is a variable
 * named parent_class that points to the (duh!) parent class.  Note that
 * this macro is not to be used with things that return something, use
 * the _WITH_DEFAULT version for that */
#define BONOBO_CALL_PARENT(parent_class_cast, name, args)		\
	((parent_class_cast(parent_class)->name != NULL) ?		\
	 parent_class_cast(parent_class)->name args : (void)0)

/* Same as above, but in case there is no implementation, it evaluates
 * to def_return */
#define BONOBO_CALL_PARENT_WITH_DEFAULT(parent_class_cast,		\
					name, args, def_return)		\
	((parent_class_cast(parent_class)->name != NULL) ?		\
	 parent_class_cast(parent_class)->name args : def_return)


#define BONOBO_CLASS_BOILERPLATE(type, type_as_function,		\
				 parent_type, parent_type_macro)	\
	BONOBO_BOILERPLATE(type, type_as_function, type,		\
			   parent_type, parent_type_macro,		\
			   BONOBO_REGISTER_TYPE)
#define BONOBO_REGISTER_TYPE(type, type_as_function, corba_type,	\
			     parent_type, parent_type_macro)		\
	bonobo_type_unique (parent_type_macro, NULL, NULL, 0,		\
			    &object_info, #type)

#define BONOBO_CLASS_BOILERPLATE_FULL(type, type_as_function,		\
				      corba_type,			\
				      parent_type, parent_type_macro)	\
	BONOBO_BOILERPLATE(type, type_as_function, corba_type,		\
			   parent_type, parent_type_macro,		\
			   BONOBO_REGISTER_TYPE_FULL)
#define BONOBO_REGISTER_TYPE_FULL(type, type_as_function, corba_type,	\
				  parent_type, parent_type_macro)	\
	bonobo_type_unique (parent_type_macro,				\
			    POA_##corba_type##__init,			\
			    POA_##corba_type##__fini,			\
			    G_STRUCT_OFFSET (type##Class, epv),		\
			    &object_info, #type)

G_END_DECLS

#endif /* _BONOBO_MACROS_H_ */
