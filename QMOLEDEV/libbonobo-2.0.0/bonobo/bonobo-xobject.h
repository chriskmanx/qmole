/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-xobject.h: Modified Bonobo Unknown interface base implementation
 *
 * Authors:
 *   Michael Meeks (michael@ximian.com)
 *
 * Copyright 2000 Ximian, Inc.
 */
#ifndef _BONOBO_X_OBJECT_H_
#define _BONOBO_X_OBJECT_H_

#include <bonobo/bonobo-object.h>

#ifndef BONOBO_DISABLE_DEPRECATED

G_BEGIN_DECLS

/* Compatibility code */
#define BONOBO_TYPE_X_OBJECT        BONOBO_TYPE_OBJECT
#define BONOBO_X_OBJECT_TYPE        BONOBO_TYPE_X_OBJECT /* deprecated, you should use BONOBO_TYPE_X_OBJECT */
#define BONOBO_X_OBJECT(o)          BONOBO_OBJECT (o)
#define BONOBO_X_OBJECT_CLASS(k)    BONOBO_OBJECT_CLASS (k)
#define BONOBO_IS_X_OBJECT(o)       BONOBO_IS_OBJECT (o)
#define BONOBO_IS_X_OBJECT_CLASS(k) BONOBO_IS_OBJECT_CLASS (k)

/*
 * Compatibility macros to convert between types,
 * use bonobo_object (), it's more foolproof.
 */
#define BONOBO_X_OBJECT_HEADER_SIZE BONOBO_OBJECT_HEADER_SIZE
#define BONOBO_X_OBJECT_GET_SERVANT(o) ((PortableServer_Servant)&(o)->servant)
#define BONOBO_X_SERVANT_GET_OBJECT(o) ((BonoboXObject *)((guchar *)(o)				\
					     - BONOBO_X_OBJECT_HEADER_SIZE			\
					     - sizeof (struct CORBA_Object_struct)	\
					     - sizeof (gpointer) * 4))

#define BonoboXObject            BonoboObject
#define BonoboXObjectClass       BonoboObjectClass
#define bonobo_x_object          bonobo_object
#define BonoboXObjectPOAFn       BonoboObjectPOAFn
#define bonobo_x_object_get_type bonobo_object_get_type
#define bonobo_x_type_unique     bonobo_type_unique
#define bonobo_x_type_setup      bonobo_type_setup

#define BONOBO_X_TYPE_FUNC_FULL(class_name, corba_name, parent, prefix)       \
GType                                                                         \
prefix##_get_type (void)                                                      \
{                                                                             \
	GType ptype;                                                          \
	static GType type = 0;                                                \
                                                                              \
	if (type == 0) {                                                      \
		static GTypeInfo info = {                                     \
			sizeof (class_name##Class),                           \
			(GBaseInitFunc) NULL,                                 \
			(GBaseFinalizeFunc) NULL,                             \
			(GClassInitFunc) prefix##_class_init,                 \
			NULL, NULL,                                           \
			sizeof (class_name),                                  \
			0,                                                    \
			(GInstanceInitFunc) prefix##_init                     \
		};                                                            \
		ptype = (parent);                                             \
		type = bonobo_x_type_unique (ptype,                           \
			POA_##corba_name##__init, POA_##corba_name##__fini,   \
			G_STRUCT_OFFSET (class_name##Class, epv),             \
			&info, #class_name);                                  \
	}                                                                     \
	return type;                                                          \
}
 
#define BONOBO_X_TYPE_FUNC(class_name, parent, prefix)                        \
GType                                                                         \
prefix##_get_type (void)                                                      \
{                                                                             \
	GType ptype;                                                          \
	static GType type = 0;                                                \
                                                                              \
	if (type == 0) {                                                      \
		static GTypeInfo info = {                                     \
			sizeof (class_name##Class),                           \
			(GBaseInitFunc) NULL,                                 \
			(GBaseFinalizeFunc) NULL,                             \
			(GClassInitFunc) prefix##_class_init,                 \
			NULL, NULL,                                           \
			sizeof (class_name),                                  \
			0,                                                    \
			(GInstanceInitFunc) prefix##_init                     \
		};                                                            \
		ptype = (parent);                                             \
		type = bonobo_x_type_unique (ptype, NULL, NULL, 0,            \
					     &info, #class_name);             \
	}                                                                     \
	return type;                                                          \
}

G_END_DECLS

#endif /* BONOBO_DISABLE_DEPRECATED */

#endif
