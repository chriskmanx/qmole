/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-persist-client.h: Client-side utility functions dealing with persistancy
 *
 * Author:
 *   ÉRDI Gergõ <cactus@cactus.rulez.org>
 *
 * Copyright 2001 Gergõ Érdi
 */
#ifndef _BONOBO_PERSIST_CLIENT_H_
#define _BONOBO_PERSIST_CLIENT_H_

#include <bonobo/bonobo-object.h>

G_BEGIN_DECLS

void bonobo_object_save_to_stream (Bonobo_Unknown     object,
				   Bonobo_Stream      stream,
				   CORBA_Environment *opt_ev);
Bonobo_Unknown bonobo_object_from_stream (Bonobo_Stream      stream,
					  CORBA_Environment *opt_ev);

G_END_DECLS

#endif /* _BONOBO_PERSIST_CLIENT_H_ */
