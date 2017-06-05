/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-persist.h: a persistance interface
 *
 * Author:
 *   Miguel de Icaza (miguel@kernel.org)
 *
 * Copyright 1999 Helix Code, Inc.
 */
#ifndef _BONOBO_PERSIST_H_
#define _BONOBO_PERSIST_H_

#include <bonobo/bonobo-object.h>

G_BEGIN_DECLS

#define BONOBO_TYPE_PERSIST        (bonobo_persist_get_type ())
#define BONOBO_PERSIST_TYPE        BONOBO_TYPE_PERSIST /* deprecated, you should use BONOBO_TYPE_PERSIST */
#define BONOBO_PERSIST(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_PERSIST, BonoboPersist))
#define BONOBO_PERSIST_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_PERSIST, BonoboPersistClass))
#define BONOBO_IS_PERSIST(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_PERSIST))
#define BONOBO_IS_PERSIST_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_PERSIST))

typedef struct _BonoboPersistPrivate BonoboPersistPrivate;

typedef struct {
	BonoboObject object;

	BonoboPersistPrivate *priv;
} BonoboPersist;

typedef struct {
	BonoboObjectClass      parent_class;

	POA_Bonobo_Persist__epv epv;

	Bonobo_Persist_ContentTypeList *
	                      (*get_content_types) (BonoboPersist     *persist,
						    CORBA_Environment *ev);
} BonoboPersistClass;

GType                           bonobo_persist_get_type (void) G_GNUC_CONST;

Bonobo_Persist_ContentTypeList *bonobo_persist_generate_content_types (int num,
								       ...);

BonoboPersist                  *bonobo_persist_construct (BonoboPersist *persist,
							  const gchar   *iid);

void				bonobo_persist_set_dirty (BonoboPersist *persist,
							  gboolean dirty);

G_END_DECLS

#endif /* _BONOBO_PERSIST_H_ */
