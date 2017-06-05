/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * Bonobo Unknown interface base implementation
 *
 * Authors:
 *   Miguel de Icaza (miguel@kernel.org)
 *   Michael Meeks (michael@helixcode.com)
 *
 * Copyright 1999,2001 Ximian, Inc.
 */
#ifndef _BONOBO_OBJECT_H_
#define _BONOBO_OBJECT_H_

#include <bonobo-activation/bonobo-activation.h>

#include <glib-object.h>
#include <bonobo/Bonobo.h>
#include <bonobo/bonobo-macros.h>

G_BEGIN_DECLS

#undef BONOBO_OBJECT_DEBUG
 
#define BONOBO_TYPE_OBJECT        (bonobo_object_get_type ())
#define BONOBO_OBJECT_TYPE        BONOBO_TYPE_OBJECT /* deprecated, you should use BONOBO_TYPE_OBJECT */
#define BONOBO_OBJECT(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_OBJECT, BonoboObject))
#define BONOBO_OBJECT_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_OBJECT, BonoboObjectClass))
#define BONOBO_IS_OBJECT(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_OBJECT))
#define BONOBO_IS_OBJECT_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_OBJECT))
#define BONOBO_OBJECT_GET_CLASS(o)(G_TYPE_INSTANCE_GET_CLASS ((o), BONOBO_TYPE_OBJECT, BonoboObjectClass))

#define BONOBO_OBJREF(o)          (bonobo_object_corba_objref(BONOBO_OBJECT(o)))

typedef void  (*BonoboObjectPOAFn) (PortableServer_Servant servant,
				    CORBA_Environment     *ev);

typedef struct _BonoboObjectPrivate BonoboObjectPrivate;

typedef struct {
	GObject              base;             /* pointer + guint + pointer */
	BonoboObjectPrivate *priv;             /* pointer */
	guint                object_signature; /* guint   */
} BonoboObjectHeader;

#define BONOBO_OBJECT_HEADER_SIZE (sizeof (BonoboObjectHeader))
#define BONOBO_OBJECT_SIGNATURE   0xaef2
#define BONOBO_SERVANT_SIGNATURE  0x2fae

typedef struct {
	/* A GObject and its signature of type BonoboObjectHeader */
	GObject              base;             /* pointer + guint + pointer */
	BonoboObjectPrivate *priv;             /* pointer */
	guint                object_signature; /* guint   */

	/* A Servant and its signature - same memory layout */
	POA_Bonobo_Unknown   servant;          /* pointer + pointer */
	guint                dummy;            /* guint   */
	Bonobo_Unknown       corba_objref;     /* pointer */
	guint                servant_signature;
} BonoboObject;

typedef struct {
	GObjectClass parent_class;

	/* signals. */
	void         (*destroy)          (BonoboObject *object);
	void         (*system_exception) (BonoboObject *object,
					  CORBA_Object  cobject,
					  CORBA_Environment *ev);

	BonoboObjectPOAFn          poa_init_fn;
	BonoboObjectPOAFn          poa_fini_fn;

	POA_Bonobo_Unknown__vepv       *vepv;

	/* The offset of this class' additional epv */
	int                             epv_struct_offset;

	PortableServer_ServantBase__epv base_epv;
	POA_Bonobo_Unknown__epv         epv;

	gpointer                        dummy[4];
} BonoboObjectClass;

GType                    bonobo_object_get_type               (void) G_GNUC_CONST;
void                     bonobo_object_add_interface          (BonoboObject           *object,
							       BonoboObject           *newobj);
BonoboObject            *bonobo_object_query_local_interface  (BonoboObject           *object,
							       const char             *repo_id);
Bonobo_Unknown           bonobo_object_query_interface        (BonoboObject           *object,
							       const char             *repo_id,
							       CORBA_Environment      *opt_ev);
Bonobo_Unknown           bonobo_object_corba_objref           (BonoboObject           *object);

/*
 * Gnome Object Life Cycle
 */
Bonobo_Unknown           bonobo_object_dup_ref                (Bonobo_Unknown          object,
							       CORBA_Environment      *opt_ev);
Bonobo_Unknown           bonobo_object_release_unref          (Bonobo_Unknown          object,
							       CORBA_Environment      *opt_ev);
gpointer                 bonobo_object_ref                    (gpointer                obj);
void                     bonobo_object_idle_unref             (gpointer                obj);
gpointer                 bonobo_object_unref                  (gpointer                obj);
void                     bonobo_object_set_immortal           (BonoboObject           *object,
							       gboolean                immortal);
gpointer                 bonobo_object_trace_refs             (gpointer                obj,
							       const char             *fn,
							       int                     line,
							       gboolean                ref);

#ifdef BONOBO_OBJECT_DEBUG
#	define           bonobo_object_ref(o)   bonobo_object_trace_refs ((o),G_GNUC_PRETTY_FUNCTION,__LINE__,TRUE);
#	define           bonobo_object_unref(o) bonobo_object_trace_refs ((o),G_GNUC_PRETTY_FUNCTION,__LINE__,FALSE);
#endif	/* BONOBO_OBJECT_DEBUG */
void                     bonobo_object_dump_interfaces        (BonoboObject *object);

/*
 * Error checking
 */
void                     bonobo_object_check_env              (BonoboObject           *object,
							       CORBA_Object            corba_object,
							       CORBA_Environment      *ev);

#define BONOBO_OBJECT_CHECK(o,c,e)				\
			G_STMT_START {				\
			if ((e)->_major != CORBA_NO_EXCEPTION)	\
				bonobo_object_check_env(o,c,e);	\
			} G_STMT_END

/*
 * Others
 */

gboolean  bonobo_unknown_ping           (Bonobo_Unknown     object,
					 CORBA_Environment *opt_ev);
void      bonobo_object_list_unref_all  (GList            **list);
void      bonobo_object_slist_unref_all (GSList           **list);


/* Detects the pointer type and returns the object reference - magic. */
BonoboObject *bonobo_object (gpointer p);
/* The same thing but faster - has a double evaluate */
#define       bonobo_object_fast(o) \
	((((BonoboObjectHeader *)(o))->object_signature == BONOBO_OBJECT_SIGNATURE) ? \
	 (BonoboObject *)(o) : (BonoboObject *)(((guchar *) (o)) - BONOBO_OBJECT_HEADER_SIZE))

/* Compat */
#define       bonobo_object_from_servant(s) ((BonoboObject *)(((guchar *) (s)) - BONOBO_OBJECT_HEADER_SIZE))
#define       bonobo_object_get_servant(o)  ((PortableServer_Servant)((guchar *)(o) + BONOBO_OBJECT_HEADER_SIZE))


/* Use G_STRUCT_OFFSET to calc. epv_struct_offset */
GType          bonobo_type_unique (GType             parent_type,
				   BonoboObjectPOAFn init_fn,
				   BonoboObjectPOAFn fini_fn,
				   int               epv_struct_offset,
				   const GTypeInfo  *info,
				   const gchar      *type_name);

gboolean       bonobo_type_setup  (GType             type,
				   BonoboObjectPOAFn init_fn,
				   BonoboObjectPOAFn fini_fn,
				   int               epv_struct_offset);

#define BONOBO_TYPE_FUNC_FULL(class_name, corba_name, parent, prefix)         \
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
		type = bonobo_type_unique (ptype,                             \
			POA_##corba_name##__init, POA_##corba_name##__fini,   \
			G_STRUCT_OFFSET (class_name##Class, epv),             \
			&info, #class_name);                                  \
	}                                                                     \
	return type;                                                          \
}
 
#define BONOBO_TYPE_FUNC(class_name, parent, prefix)                        \
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
		type = bonobo_type_unique (ptype, NULL, NULL, 0,              \
				           &info, #class_name);               \
	}                                                                     \
	return type;                                                          \
}

G_END_DECLS

#endif
