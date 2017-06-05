/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-persist-client.c: Client-side utility functions dealing with persistancy
 *
 * Author:
 *   ÉRDI Gergõ <cactus@cactus.rulez.org>
 *
 * Copyright 2001 Gergõ Érdi
 */

#include <bonobo/bonobo-persist-client.h>

#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-stream-client.h>
#include <bonobo/bonobo-moniker-util.h>

void
bonobo_object_save_to_stream (Bonobo_Unknown     object,
			      Bonobo_Stream      stream,
			      CORBA_Environment *opt_ev)
{
	char                           *iid = 0;
	CORBA_Environment               my_ev;
	Bonobo_PersistStream            pstream = CORBA_OBJECT_NIL;

	CORBA_exception_init (&my_ev);
	pstream = Bonobo_Unknown_queryInterface (object, "IDL:Bonobo/PersistStream:1.0", &my_ev);
	CORBA_exception_free (&my_ev);

	if (!pstream) {
		bonobo_exception_set (opt_ev, ex_Bonobo_Moniker_InterfaceNotFound);
		goto out;
	}

	CORBA_exception_init (&my_ev);
	iid = Bonobo_Persist_getIId (pstream, &my_ev);
	bonobo_stream_client_write_string (stream, iid, TRUE, &my_ev);
	if (BONOBO_EX (&my_ev)) {
		if (opt_ev)
			bonobo_exception_set (opt_ev, BONOBO_EX_REPOID (&my_ev));
		CORBA_exception_free (&my_ev);
		goto out;
	}

	if (opt_ev) {
		Bonobo_PersistStream_save (pstream, stream, "", opt_ev);
	} else {
		Bonobo_PersistStream_save (pstream, stream, "", opt_ev);
		CORBA_exception_free (&my_ev);
	}

 out:
	g_free (iid);
	if (pstream != CORBA_OBJECT_NIL) {
		CORBA_exception_init (&my_ev);
		Bonobo_Unknown_unref (pstream, &my_ev);
		CORBA_exception_free (&my_ev);
	}
}


Bonobo_Unknown
bonobo_object_from_stream (Bonobo_Stream      stream,
			   CORBA_Environment *opt_ev)
{
	char                 *iid = 0;
	CORBA_Environment     my_ev, *ev;
	Bonobo_PersistStream  pstream = CORBA_OBJECT_NIL;

	CORBA_exception_init (&my_ev);

	if (opt_ev)
		ev = opt_ev;
	else
		ev = &my_ev;

	bonobo_stream_client_read_string (stream, &iid, ev);
	if (BONOBO_EX (ev)) {
		goto out;
	}
	
	pstream = bonobo_get_object (iid, "IDL:Bonobo/PersistStream:1.0",
				     ev);
	if (BONOBO_EX (ev)) {
		pstream = CORBA_OBJECT_NIL;
		goto out;
	}
	
	Bonobo_PersistStream_load (pstream, stream, "", ev);
	
 out:
	CORBA_exception_free (&my_ev);
	g_free (iid);
	
	return pstream;
}
