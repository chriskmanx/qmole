/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-context.h: Handle Global Component contexts.
 *
 * Author:
 *     Michael Meeks (michael@helixcode.com)
 *
 * Copyright 2000 Helix Code, Inc.
 */
#ifndef _BONOBO_CONTEXT_H_
#define _BONOBO_CONTEXT_H_

#include <bonobo/bonobo-object.h>

Bonobo_Unknown bonobo_context_get (const CORBA_char  *context_name,
				   CORBA_Environment *opt_ev);

void           bonobo_context_add (const CORBA_char  *context_name,
				   Bonobo_Unknown     context);

/* emits a 'last_unref' signal */
BonoboObject  *bonobo_context_running_get (void);

void           bonobo_running_context_auto_exit_unref (BonoboObject *object);

#endif /* _BONOBO_CONTEXT_H_ */
