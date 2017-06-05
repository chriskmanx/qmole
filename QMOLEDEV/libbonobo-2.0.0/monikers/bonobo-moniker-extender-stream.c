/*
 * bonobo-moniker-extender-stream.c: 
 *
 * Author:
 *	Dietmar Maurer (dietmar@helixcode.com)
 *
 * Copyright 2000, Ximian, Inc.
 */
#include <config.h>
#include <bonobo/bonobo-storage.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-moniker.h>
#include <bonobo/bonobo-moniker-extender.h>
#include <bonobo/bonobo-moniker-util.h>

#include "bonobo-moniker-std.h"

static gchar *
get_stream_type (Bonobo_Stream stream, CORBA_Environment *ev)
{
	Bonobo_StorageInfo *info;
	gchar              *type;

	g_return_val_if_fail (stream != CORBA_OBJECT_NIL, NULL);

	info = Bonobo_Stream_getInfo (stream, Bonobo_FIELD_CONTENT_TYPE, ev);
	
	if (BONOBO_EX (ev))
		return NULL;

	type = g_strdup (info->content_type);

	CORBA_free (info);

	return type;
}

Bonobo_Unknown
bonobo_stream_extender_resolve (BonoboMonikerExtender       *extender,
				const Bonobo_Moniker         m,
				const Bonobo_ResolveOptions *options,
				const CORBA_char            *display_name,
				const CORBA_char            *requested_interface,
				CORBA_Environment           *ev)
{
	const char    *mime_type;
	char          *requirements;
	Bonobo_Unknown object;
	Bonobo_Unknown stream;
	Bonobo_Persist persist;

	g_warning ("Stream extender: '%s'", display_name);

	if (!m)
		return CORBA_OBJECT_NIL;

	stream = Bonobo_Moniker_resolve (m, options, "IDL:Bonobo/Stream:1.0", ev);

	if (!stream)
		return CORBA_OBJECT_NIL;

	mime_type = get_stream_type (stream, ev);
	if (!mime_type)
		goto unref_stream_exception;

	requirements = g_strdup_printf (
		"bonobo:supported_mime_types.has ('%s') AND repo_ids.has ('%s') AND "
		"repo_ids.has ('IDL:Bonobo/PersistStream:1.0')",
		mime_type, requested_interface);
		
	object = bonobo_activation_activate (requirements, NULL, 0, NULL, ev);
	g_warning ("Attempt activate object satisfying '%s': %p",
		   requirements, object);
	g_free (requirements);

	if (ev->_major != CORBA_NO_EXCEPTION)
		goto unref_stream_exception;
		
	if (object == CORBA_OBJECT_NIL) {
		g_warning ("Can't find object satisfying requirements");
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_Moniker_InterfaceNotFound, NULL);
		goto unref_stream_exception;
	}

	persist = Bonobo_Unknown_queryInterface (
		object, "IDL:Bonobo/PersistStream:1.0", ev);

	if (ev->_major != CORBA_NO_EXCEPTION)
		goto unref_object_exception;

	if (persist != CORBA_OBJECT_NIL) {
		Bonobo_PersistStream_load (
			persist, stream, (const Bonobo_Persist_ContentType) mime_type, ev);

		bonobo_object_release_unref (persist, ev);
		bonobo_object_release_unref (stream, ev);

		return bonobo_moniker_util_qi_return (
			object, requested_interface, ev);
	}

 unref_object_exception:
	bonobo_object_release_unref (object, ev);

 unref_stream_exception:
	bonobo_object_release_unref (stream, ev);

	return CORBA_OBJECT_NIL;
}
