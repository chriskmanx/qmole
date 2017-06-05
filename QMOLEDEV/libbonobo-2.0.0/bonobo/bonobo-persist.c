/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * bonobo-persist.c: a persistance interface
 *
 * Author:
 *   Miguel de Icaza (miguel@kernel.org)
 *
 * Copyright 1999 Ximian, Inc.
 */
#include <config.h>
#include <string.h>
#include <glib-object.h>
#include <gobject/gmarshal.h>
#include <bonobo/bonobo-persist.h>

#define PARENT_TYPE BONOBO_TYPE_OBJECT

/* Parent object class */
static GObjectClass *bonobo_persist_parent_class;

#define CLASS(o) BONOBO_PERSIST_CLASS(G_OBJECT_GET_CLASS (o))

struct _BonoboPersistPrivate
{
	gchar *iid;
	gboolean dirty;
};

static inline BonoboPersist *
bonobo_persist_from_servant (PortableServer_Servant servant)
{
	return BONOBO_PERSIST (bonobo_object_from_servant (servant));
}

static Bonobo_Persist_ContentTypeList *
impl_Bonobo_Persist_getContentTypes (PortableServer_Servant servant,
				     CORBA_Environment     *ev)
{
	BonoboPersist *persist = bonobo_persist_from_servant (servant);

	return CLASS (persist)->get_content_types (persist, ev);
}

static CORBA_char*
impl_Bonobo_Persist_getIId (PortableServer_Servant   servant,
			    CORBA_Environment       *ev)
{
	BonoboPersist *persist = bonobo_persist_from_servant (servant);

	return CORBA_string_dup (persist->priv->iid);
}

static CORBA_boolean
impl_Bonobo_Persist_isDirty (PortableServer_Servant   servant,
			     CORBA_Environment       *ev)
{
	BonoboPersist *persist = bonobo_persist_from_servant (servant);

	return persist->priv->dirty;
}

static void
bonobo_persist_finalize (GObject *object)
{
	BonoboPersist *persist = BONOBO_PERSIST (object);

	if (persist->priv)
	{
		g_free (persist->priv->iid);
		g_free (persist->priv);
		persist->priv = 0;
	}
	
	bonobo_persist_parent_class->finalize (object);
}

static void
bonobo_persist_class_init (BonoboPersistClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;
	POA_Bonobo_Persist__epv *epv = &klass->epv;

	bonobo_persist_parent_class = g_type_class_peek_parent (klass);

	/* Override and initialize methods */
	object_class->finalize = bonobo_persist_finalize;

	epv->getContentTypes = impl_Bonobo_Persist_getContentTypes;
	epv->getIId = impl_Bonobo_Persist_getIId;
	epv->isDirty = impl_Bonobo_Persist_isDirty;
}

static void
bonobo_persist_init (GObject *object)
{
	BonoboPersist *persist = BONOBO_PERSIST (object);
	persist->priv = g_new0 (BonoboPersistPrivate, 1);
}

BONOBO_TYPE_FUNC_FULL (BonoboPersist, 
		       Bonobo_Persist,
		       PARENT_TYPE,
		       bonobo_persist);

/**
 * bonobo_persist_generate_content_types:
 * @num: the number of content types specified
 * @...: the content types (as strings)
 *
 * Returns: a ContentTypeList containing the given ContentTypes
 **/
Bonobo_Persist_ContentTypeList *
bonobo_persist_generate_content_types (int num, ...)
{
	Bonobo_Persist_ContentTypeList *types;
	va_list ap;
	char *type;
	int i;

	types = Bonobo_Persist_ContentTypeList__alloc ();
	CORBA_sequence_set_release (types, TRUE);
	types->_length = types->_maximum = num;
	types->_buffer = CORBA_sequence_Bonobo_Persist_ContentType_allocbuf (num);

	va_start (ap, num);
	for (i = 0; i < num; i++) {
		type = va_arg (ap, char *);
		types->_buffer[i] = CORBA_string_alloc (strlen (type) + 1);
		strcpy (types->_buffer[i], type);
	}
	va_end (ap);

	return types;
}

/**
 * bonobo_persist_construct:
 * @persist: A BonoboPersist
 * @iid: OAF IID of the object this interface is aggregated to
 *
 * Initializes the BonoboPersist object. You should only use this
 * method in derived implementations, because a BonoboPersist instance
 * doesn't make a lot of sense, but the iid private field has to be
 * set at construction time.
 */
BonoboPersist *
bonobo_persist_construct (BonoboPersist *persist,
			  const gchar   *iid)
{
	g_return_val_if_fail (persist != NULL, NULL);
	g_return_val_if_fail (BONOBO_IS_PERSIST (persist), NULL);

	g_return_val_if_fail (iid != NULL, NULL);

	persist->priv->iid = g_strdup (iid);

	return persist;
}

/**
 * bonobo_persist_set_dirty:
 * @persist: A BonoboPersist
 * @dirty: A flag indicating the dirty status of this object.
 *
 * Sets the dirty status of the interface which is reported via
 * the isDirty method.
 */
void
bonobo_persist_set_dirty (BonoboPersist *persist, gboolean dirty)
{
	persist->priv->dirty = dirty;
}
