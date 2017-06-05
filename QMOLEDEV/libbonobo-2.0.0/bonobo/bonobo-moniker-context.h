/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-moniker-context.c: A global moniker interface
 *
 * Author:
 *	Michael Meeks (michael@helixcode.com)
 *
 * Copyright (C) 2000, Helix Code, Inc.
 */
#ifndef _BONOBO_MONIKER_CONTEXT_H_
#define _BONOBO_MONIKER_CONTEXT_H_

#include <bonobo/bonobo-object.h>

G_BEGIN_DECLS

typedef struct _BonoboMonikerContextPrivate BonoboMonikerContextPrivate;

typedef struct {
	BonoboObject parent;

	BonoboMonikerContextPrivate *priv;
} BonoboMonikerContext;

typedef struct {
	BonoboObjectClass parent;

	POA_Bonobo_MonikerContext__epv epv;
} BonoboMonikerContextClass;

BonoboObject *bonobo_moniker_context_new (void);

G_END_DECLS

#endif /* _BONOBO_MONIKER_CONTEXT_H_ */

