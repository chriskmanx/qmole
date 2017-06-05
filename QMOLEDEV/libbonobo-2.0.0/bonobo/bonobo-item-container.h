/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-item-container.h: a generic container for monikers.
 *
 * Author:
 *   Miguel de Icaza (miguel@kernel.org)
 *
 * Copyright 1999 Helix Code, Inc.
 */

#ifndef _BONOBO_ITEM_CONTAINER_H_
#define _BONOBO_ITEM_CONTAINER_H_


#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-moniker.h>

G_BEGIN_DECLS
 
#define BONOBO_TYPE_ITEM_CONTAINER        (bonobo_item_container_get_type ())
#define BONOBO_ITEM_CONTAINER_TYPE        BONOBO_TYPE_ITEM_CONTAINER /* deprecated, you should use BONOBO_TYPE_ITEM_CONTAINER */
#define BONOBO_ITEM_CONTAINER(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_ITEM_CONTAINER, BonoboItemContainer))
#define BONOBO_ITEM_CONTAINER_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_ITEM_CONTAINER, BonoboItemContainerClass))
#define BONOBO_IS_ITEM_CONTAINER(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_ITEM_CONTAINER))
#define BONOBO_IS_ITEM_CONTAINER_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_ITEM_CONTAINER))

typedef GList BonoboClientSiteList;

typedef struct _BonoboItemContainerPrivate BonoboItemContainerPrivate;

typedef struct {
	BonoboObject base;

	BonoboItemContainerPrivate *priv;
} BonoboItemContainer;

typedef struct {
	BonoboObjectClass parent_class;

	POA_Bonobo_ItemContainer__epv epv;

	Bonobo_Unknown (*get_object) (BonoboItemContainer *item_container,
				      CORBA_char          *item_name,
				      CORBA_boolean        only_if_exists,
				      CORBA_Environment   *ev);
} BonoboItemContainerClass;

GType                bonobo_item_container_get_type       (void) G_GNUC_CONST;
BonoboItemContainer *bonobo_item_container_new            (void);

void                 bonobo_item_container_add            (BonoboItemContainer *container,
							   const char          *name, 
							   BonoboObject        *object);

void                 bonobo_item_container_remove_by_name (BonoboItemContainer *container,
							   const char          *name);

G_END_DECLS

#endif

