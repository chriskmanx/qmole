/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-storage.c: Storage manipulation.
 *
 * Authors:
 *   Dietmar Maurer (dietmar@maurer-it.com)
 *
 * Copyright 2001 Ximian, Inc.
 */
#include <config.h>
#include <gmodule.h>

#include <bonobo/bonobo-storage.h>
#include <bonobo/bonobo-exception.h>

static void
copy_stream (Bonobo_Stream src, Bonobo_Stream dest, CORBA_Environment *ev) 
{
	Bonobo_Stream_iobuf *buf;

	do {
		Bonobo_Stream_read (src, 4096, &buf, ev);
		if (BONOBO_EX (ev)) 
			break;

		if (buf->_length == 0) {
			CORBA_free (buf);
			break;
		}

		Bonobo_Stream_write (dest, buf, ev);
		CORBA_free (buf);
		if (BONOBO_EX (ev)) 
			break;

	} while (1);

	if (BONOBO_EX (ev)) /* we must return a Bonobo_Storage exception */
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION, 
				     ex_Bonobo_Storage_IOError, NULL);
}

/**
 * bonobo_storage_copy_to:
 * @src: the source storage
 * @dest: the destination storage
 * @ev: CORBA exception environment
 * 
 * Implements a pure CORBA method for copying one storage into
 * another, this is used by several BonoboStorage implemetations
 * where a fast case localy copy cannot work.
 **/
void
bonobo_storage_copy_to (Bonobo_Storage src, Bonobo_Storage dest,
			CORBA_Environment *ev) 
{
	Bonobo_Storage new_src, new_dest;
	Bonobo_Stream src_stream, dest_stream;
	Bonobo_Storage_DirectoryList *list;
	gint i;

	if ((src == CORBA_OBJECT_NIL) || (dest == CORBA_OBJECT_NIL) || !ev) {
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION, 
				     ex_Bonobo_Storage_IOError, NULL);
		return;
	}

	list = Bonobo_Storage_listContents (src, "", 
					    Bonobo_FIELD_CONTENT_TYPE |
					    Bonobo_FIELD_TYPE,
					    ev);
	if (BONOBO_EX (ev))
		return;

	for (i = 0; i <list->_length; i++) {

		if (list->_buffer[i].type == Bonobo_STORAGE_TYPE_DIRECTORY) {

			new_dest = Bonobo_Storage_openStorage
				(dest, list->_buffer[i].name, 
				 Bonobo_Storage_CREATE | 
				 Bonobo_Storage_FAILIFEXIST, ev);

			if (BONOBO_EX (ev)) 
				break;

			Bonobo_Storage_setInfo (new_dest, "",
						&list->_buffer[i],
						Bonobo_FIELD_CONTENT_TYPE,
						ev);

			if (BONOBO_EX (ev)) {
				bonobo_object_release_unref (new_dest, NULL);
				break;
			}

			new_src = Bonobo_Storage_openStorage
				(src, list->_buffer[i].name, 
				 Bonobo_Storage_READ, ev);
			
			if (BONOBO_EX (ev)) {
				bonobo_object_release_unref (new_dest, NULL);
				break;
			}

			bonobo_storage_copy_to (new_src, new_dest, ev);
			
			bonobo_object_release_unref (new_src, NULL);
			bonobo_object_release_unref (new_dest, NULL);

			if (BONOBO_EX (ev))
				break;

		} else {
			dest_stream = Bonobo_Storage_openStream 
				(dest, list->_buffer[i].name,
				 Bonobo_Storage_CREATE | 
				 Bonobo_Storage_FAILIFEXIST, ev);

			if (BONOBO_EX (ev))
				break;

			Bonobo_Stream_setInfo (dest_stream,
					       &list->_buffer[i],
					       Bonobo_FIELD_CONTENT_TYPE,
					       ev);

			if (BONOBO_EX (ev)) {
				CORBA_exception_set (ev, CORBA_USER_EXCEPTION, 
						     ex_Bonobo_Storage_IOError,
						     NULL);
				bonobo_object_release_unref (dest_stream,
							     NULL);
				break;
			}

			src_stream = Bonobo_Storage_openStream 
				(src, list->_buffer[i].name,
				 Bonobo_Storage_READ, ev);

			if (BONOBO_EX (ev)) {
				bonobo_object_release_unref (dest_stream, 
							     NULL);
				break;
			}

			copy_stream (src_stream, dest_stream, ev);

			bonobo_object_release_unref (src_stream, NULL);
			bonobo_object_release_unref (dest_stream, NULL);

			if (BONOBO_EX (ev))
				break;
		}
	}

	CORBA_free (list);
}
