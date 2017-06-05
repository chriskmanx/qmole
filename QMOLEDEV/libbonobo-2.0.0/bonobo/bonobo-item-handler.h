/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-item-handler.h: a generic ItemContainer handler for monikers.
 *
 * Author:
 *   Miguel de Icaza (miguel@kernel.org)
 *
 * Copyright 1999, 2000 Helix Code, Inc.
 * Copyright 1999, 2000 Miguel de Icaza
 */

#ifndef _BONOBO_ITEM_HANDLER_H_
#define _BONOBO_ITEM_HANDLER_H_


#include <bonobo/bonobo-object.h>

G_BEGIN_DECLS
 
#define BONOBO_TYPE_ITEM_HANDLER        (bonobo_item_handler_get_type ())
#define BONOBO_ITEM_HANDLER_TYPE        BONOBO_TYPE_ITEM_HANDLER /* deprecated, you should use BONOBO_TYPE_ITEM_HANDLER */
#define BONOBO_ITEM_HANDLER(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_ITEM_HANDLER, BonoboItemHandler))
#define BONOBO_ITEM_HANDLER_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_ITEM_HANDLER, BonoboItemHandlerClass))
#define BONOBO_IS_ITEM_HANDLER(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_ITEM_HANDLER))
#define BONOBO_IS_ITEM_HANDLER_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_ITEM_HANDLER))

typedef struct _BonoboItemHandlerPrivate BonoboItemHandlerPrivate;
typedef struct _BonoboItemHandler        BonoboItemHandler;

typedef Bonobo_ItemContainer_ObjectNames *(*BonoboItemHandlerEnumObjectsFn)
	(BonoboItemHandler *h, gpointer data, CORBA_Environment *);

typedef Bonobo_Unknown (*BonoboItemHandlerGetObjectFn)
	(BonoboItemHandler *h, const char *item_name, gboolean only_if_exists,
	 gpointer data, CORBA_Environment *ev);

struct _BonoboItemHandler {
	BonoboObject base;

	POA_Bonobo_ItemContainer__epv epv;

	BonoboItemHandlerPrivate      *priv;
};

typedef struct {
	BonoboObjectClass parent_class;

	POA_Bonobo_ItemContainer__epv epv;
} BonoboItemHandlerClass;

GType                bonobo_item_handler_get_type    (void) G_GNUC_CONST;
BonoboItemHandler   *bonobo_item_handler_new         (BonoboItemHandlerEnumObjectsFn enum_objects,
						      BonoboItemHandlerGetObjectFn   get_object,
						      gpointer                       user_data);

BonoboItemHandler   *bonobo_item_handler_new_closure (GClosure *enum_objects,
						      GClosure *get_object);

BonoboItemHandler   *bonobo_item_handler_construct   (BonoboItemHandler *handler,
						      GClosure          *enum_objects,
						      GClosure          *get_object);

/* Utility functions that can be used by getObject routines */
typedef struct {
	char *key;
	char *value;
} BonoboItemOption;

GSList *bonobo_item_option_parse (const char *option_string);
void    bonobo_item_options_free (GSList *options);

G_END_DECLS

#endif
