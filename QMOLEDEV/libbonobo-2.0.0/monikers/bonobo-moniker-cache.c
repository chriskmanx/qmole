/*
 * bonobo-moniker-cache.c: 
 *
 * Author:
 *	Dietmar Maurer (dietmar@helixcode.com)
 *
 * Copyright 2000 Ximian, Inc.
 */
#include <config.h>
#include <string.h>
#include <bonobo/bonobo-exception.h>

#include "bonobo-moniker-std.h"
#include "bonobo-stream-cache.h"

Bonobo_Unknown
bonobo_moniker_cache_resolve (BonoboMoniker               *moniker,
			      const Bonobo_ResolveOptions *options,
			      const CORBA_char            *requested_interface,
			      CORBA_Environment           *ev)
{
	Bonobo_Moniker parent;
	BonoboStream *stream;
	Bonobo_Stream in_stream;

	if (!strcmp (requested_interface, "IDL:Bonobo/Stream:1.0")) {

		parent = bonobo_moniker_get_parent (moniker, ev);

		if (BONOBO_EX (ev) || parent == CORBA_OBJECT_NIL)
			return CORBA_OBJECT_NIL;
	
		in_stream = Bonobo_Moniker_resolve (parent, options, 
						    "IDL:Bonobo/Stream:1.0",
						    ev);

		if (BONOBO_EX (ev) || in_stream == CORBA_OBJECT_NIL) {
			bonobo_object_release_unref (parent, NULL);
			return CORBA_OBJECT_NIL;
		}

		bonobo_object_release_unref (parent, ev);

		if (BONOBO_EX (ev))
			return CORBA_OBJECT_NIL;
	       
		stream = bonobo_stream_cache_create (in_stream, ev);

		if (BONOBO_EX (ev) || stream == CORBA_OBJECT_NIL) {
			bonobo_object_release_unref (in_stream, NULL);
			return CORBA_OBJECT_NIL;
		}

		bonobo_object_release_unref (in_stream, ev);

		if (BONOBO_EX (ev))
			return CORBA_OBJECT_NIL;
	      
		return CORBA_Object_duplicate (BONOBO_OBJREF (stream), ev);
	}

	return CORBA_OBJECT_NIL; /* use the extender */
}
