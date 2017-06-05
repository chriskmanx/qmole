/* -*- mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */

#include <stdlib.h>
#include <string.h>

#include <bonobo/bonobo-stream-client.h>
#include <bonobo/bonobo-stream-memory.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-main.h>

#define BUFSIZE 100

static gboolean
test_read_write (BonoboStream *memstream)
{
	CORBA_Environment ev;
	char *write_string = "This is the BonoboStreamMem test application";
	char *read_string = 0;
	Bonobo_Stream stream;

	stream = bonobo_object_corba_objref (BONOBO_OBJECT (memstream));
	
	CORBA_exception_init (&ev);
	printf ("\tWriting '%s' to stream\n", write_string);
	bonobo_stream_client_write_string (stream,
					   write_string, TRUE,
					   &ev);
	if (BONOBO_EX (&ev))
		printf ("\tWrite failed");
	CORBA_exception_free (&ev);

	CORBA_exception_init (&ev);
	Bonobo_Stream_seek (stream, 0, Bonobo_Stream_SeekSet, &ev);
	CORBA_exception_free (&ev);

	CORBA_exception_init (&ev);
	bonobo_stream_client_read_string (stream,
					  &read_string,
					  &ev);
	if (BONOBO_EX (&ev))
		printf ("\tRead failed");
	else
		printf ("\tRead '%s' from stream\n", read_string);
	CORBA_exception_free (&ev);

	if (!strcmp (read_string, write_string))
		return TRUE;
	else
		return FALSE;
}

int main (int argc, char *argv [])
{
	BonoboStream *memstream;
	guint8       *buffer;

	free (malloc (8));
	
	if (!bonobo_init (&argc, argv))
		g_error ("bonobo_init failed");
    
	printf ("Creating a stream in memory from scratch "
		"(size: %d bytes)\n", BUFSIZE);	
	memstream = bonobo_stream_mem_create (NULL,
					      BUFSIZE,
					      FALSE, TRUE);
	if (test_read_write (memstream))
		printf ("Passed\n");
	else
		printf ("Failed\n");
	bonobo_object_unref (memstream);


	printf ("Creating a stream in memory from pre-allocated buffer "
		"(size: %d bytes)\n", BUFSIZE);
	buffer = g_new0 (guint8, BUFSIZE);
	memstream = bonobo_stream_mem_create (buffer,
					      BUFSIZE,
					      FALSE, TRUE);
	if (test_read_write (memstream))
		printf ("Passed\n");
	else
		printf ("Failed\n");
	bonobo_object_unref (memstream);	

	return bonobo_debug_shutdown ();
}
