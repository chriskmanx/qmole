/**
 * gnome-stream-client.c: Helper routines to access a Bonobo_Stream CORBA object
 *
 * Authors:
 *   Nat Friedman    (nat@nat.org)
 *   Miguel de Icaza (miguel@kernel.org).
 *   Michael Meekss  (michael@helixcode.com)
 *
 * Copyright 1999,2000 Helix Code, Inc.
 */
#ifndef _BONOBO_STREAM_CLIENT_H_
#define _BONOBO_STREAM_CLIENT_H_

#include <bonobo/Bonobo.h>

void       bonobo_stream_client_write        (const Bonobo_Stream stream,
					      const void         *buffer,
					      const size_t        size,
					      CORBA_Environment  *ev);

guint8    *bonobo_stream_client_read         (const Bonobo_Stream stream,
					      const size_t        size,
					      CORBA_long         *length_read,
					      CORBA_Environment  *ev);

void       bonobo_stream_client_write_string (const Bonobo_Stream stream,
					      const char         *str,
					      const gboolean      terminate,
					      CORBA_Environment  *ev);

void       bonobo_stream_client_printf       (const Bonobo_Stream stream,
					      const gboolean      terminate,
					      CORBA_Environment  *ev,
					      const char         *fmt, ...);

CORBA_long bonobo_stream_client_read_string  (const Bonobo_Stream stream,
					      char              **str,
					      CORBA_Environment  *ev);

CORBA_long bonobo_stream_client_get_length   (const Bonobo_Stream stream,
					      CORBA_Environment  *ev);

#endif /* _BONOBO_STREAM_CLIENT_H_ */
