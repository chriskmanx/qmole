/*
 * CORBA corbaloc test
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Author: Elliot Lee <sopwith@redhat.com>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <orbit/orbit.h> 

static gboolean
IOP_ObjectKey_equal (ORBit_ObjectKey *a,
		     ORBit_ObjectKey *b);

gboolean
IOP_profile_equal (CORBA_Object obj1, CORBA_Object obj2,
		   gpointer d1, gpointer d2);

static gboolean
IOP_ObjectKey_equal (ORBit_ObjectKey *a,
		     ORBit_ObjectKey *b)
{
	if (a->_length != b->_length)
		return FALSE;

	if (memcmp (a->_buffer, b->_buffer, a->_length))
		return FALSE;

	return TRUE;
}

CORBA_Object ORBit_corbaloc_to_object (CORBA_ORB          orb,
                                       const gchar       *corbaloc,
                                       CORBA_Environment *ev);
 
#define ABORT_IF_EXCEPTION(_ev, _message)                    \
if ((_ev)->_major != CORBA_NO_EXCEPTION) {                   \
  g_error("%s: %s", _message, CORBA_exception_id (_ev));     \
  CORBA_exception_free (_ev);                                \
  abort();                                                   \
}

/* this implementation realizes a full comparison of profiles and
 * differs from the original one found in corba-object.c.*/ 
static gboolean
object_equal (gconstpointer a, gconstpointer b)
{
	GSList *cur1, *cur2;
	CORBA_Object _obj = (CORBA_Object) a;
	CORBA_Object other_object = (CORBA_Object) b;

	g_assert (_obj->object_key && other_object->object_key);

	if (!IOP_ObjectKey_equal (_obj->object_key, other_object->object_key))
		return FALSE;
	
	for (cur1 = _obj->profile_list; cur1; cur1 = cur1->next) {
		gboolean  equal_found = FALSE;
	
		for (cur2 = other_object->profile_list; cur2; cur2 = cur2->next) {
			if (IOP_profile_equal (_obj, other_object,
					       cur1->data, cur2->data)) {
				equal_found = TRUE;
			}
		}

		if (!equal_found)
			return FALSE;
	}

	return TRUE;
}


/**
 *
 */
static void
export_object_to_stream (CORBA_ORB          orb,
			 CORBA_Object       obj,
			 FILE              *stream,
			 CORBA_Environment *ev)
{
        CORBA_char *objref = NULL;
 
        /* write objref to file */
         
        objref = CORBA_ORB_object_to_string (orb, obj, ev);
	if ((ev)->_major != CORBA_NO_EXCEPTION) return;
 
        /* print ior to terminal */
        fprintf (stream, "%s\n", objref);
        fflush (stream);
 
        CORBA_free (objref);
}

struct ObjrefPair 
{
	gchar *corbaloc;
	gchar *ior; 	
};


static const struct ObjrefPair objref_str[] = 
{
	/* OMG corbaloc */ 
	{ "corbaloc:iiop:localhost:9999/NameService",
	  "IOR:010000000100000000000000010000000000000028000000010100000a0000006c6f63616c686f7374000f270b0000004e616d65536572766963650000000000" 
	},
	{ "corbaloc:iiop:1.2@localhost/NameService",
	  "IOR:010000000100000000000000010000000000000028000000010102000a0000006c6f63616c686f737400f90a0b0000004e616d65536572766963650000000000"
	},
	
	/* OMG multi profile */
	{ "corbaloc:iiop:gnome.org:8888,iiop:gnome.de:7777/NameService",
	  "IOR:010000000100000000000000020000000000000028000000010100000a000000676e6f6d652e6f726700b8220b0000004e616d6553657276696365000000000000000000280000000101000009000000676e6f6d652e64650000611e0b0000004e616d65536572766963650000000000"
	},
	{ "corbaloc::gnome.org:8888,:gnome.de:7777/NameService",
	  "IOR:010000000100000000000000020000000000000028000000010100000a000000676e6f6d652e6f726700b8220b0000004e616d6553657276696365000000000000000000280000000101000009000000676e6f6d652e64650000611e0b0000004e616d65536572766963650000000000"
	},
	{ "corbaloc::gnome.org,:gnome.de/NameService",
	  "IOR:010000000100000000000000020000000000000028000000010100000a000000676e6f6d652e6f726700f90a0b0000004e616d6553657276696365000000000000000000280000000101000009000000676e6f6d652e64650000f90a0b0000004e616d65536572766963650000000000"
	},
	
	/* common corbaloc */
	{ "iiop:1.2@home.de/objkey",
	  "IOR:0100000001000000000000000100000000000000240000000101020008000000686f6d652e646500f90af90a060000006f626a6b6579727600000000"
	},
	{ "iiop:gnome.org:8787/NameService",
	  "IOR:010000000100000000000000010000000000000028000000010100000a000000676e6f6d652e6f72670053220b0000004e616d65536572766963650000000000"
	},
	{ "iiop:gnome.org/NameService",
	  "IOR:010000000100000000000000010000000000000028000000010100000a000000676e6f6d652e6f726700f90a0b0000004e616d65536572766963650000000000"
	},
	{ "iiop://gnome.org/NameService",
	  "IOR:010000000100000000000000010000000000000028000000010100000a000000676e6f6d652e6f726700f90a0b0000004e616d65536572766963650000000000"
	},

	{ "corbaloc:iiops:localhost:8885/NameService",
	  "IOR:01000000010000000000000001000000000000003e000000010101000a0000006c6f63616c686f73740000000b0000004e616d65536572766963650001000000140000000e000000010101007f00000006000000b522"
	},
	
	/* verify url-decoding */ 
	{ "corbaloc:iiops:localhost:8885/%4e%61%6d%65%53%65%72%76%69%63%65",
	  "IOR:01000000010000000000000001000000000000003e000000010101000a0000006c6f63616c686f73740000000b0000004e616d65536572766963650001000000140000000e000000010101007f00000006000000b522"
	},

	{ "corbaloc:ssliop:localhost:8885/NameService",
	  "IOR:01000000010000000000000001000000000000003e000000010101000a0000006c6f63616c686f73740000000b0000004e616d65536572766963650001000000140000000e000000010101007f00000006000000b522"
	},

	{ "corbaloc:uiop:/tmp/orbit-frehberg-3333:/NameService",
	  "IOR:01000000010000000000000001000000caaedfba3300000001010200190000002f746d702f6f726269742d66726568626572672d33333333006300000b0000004e616d6553657276696365"
	},

	{ "corbaloc:uiop:/tmp/orbit-frehberg-3333:,iiop:localhost:88/NameService",
	  "IOR:01000000010000000000000002000000caaedfba3300000001010200190000002f746d702f6f726269742d66726568626572672d33333333006300000b0000004e616d6553657276696365000000000028000000010101650a0000006c6f63616c686f737400b8220b0000004e616d65536572766963650000000000"
	},

	{ NULL,
	  NULL
	}
};

int main (int argc, char *argv[]) {
	glong   i = 0;
	
			      
	CORBA_Object  client = CORBA_OBJECT_NIL;
	CORBA_Object  verify = CORBA_OBJECT_NIL;

	CORBA_Environment ev;
	CORBA_ORB orb;
	
	g_thread_init (NULL);

	CORBA_exception_init(&ev);
	orb = CORBA_ORB_init(&argc, argv, "orbit-local-orb", &ev);

	g_print ("running... \n");

	for (i = 0; objref_str[i].corbaloc; ++i)
	{
		gchar *corbaloc = objref_str[i].corbaloc;
		gchar *ior      = objref_str[i].ior;

		g_print ("verify \"%s\"\n", corbaloc);
		
		/* bind to object */
		client = CORBA_ORB_string_to_object(orb, corbaloc, &ev);
		ABORT_IF_EXCEPTION (&ev, "resolving corbloc failed");
		g_assert (client!=NULL);
		
		if (ior) 
		{
			verify = CORBA_ORB_string_to_object (orb, ior, &ev);
			ABORT_IF_EXCEPTION (&ev, "de-stringify object failed");
			g_assert (verify!=NULL);
			
			if (!object_equal(client, verify))
			{			
				export_object_to_stream (orb, client, stderr, &ev);
				g_error ("verification failed\n");
			}

			CORBA_Object_release (verify, &ev);
			ABORT_IF_EXCEPTION (&ev, "release failed");
		}
		else
		{
			export_object_to_stream (orb, client, stderr, &ev);
		}

		/* release initial object reference */
		CORBA_Object_release(client, &ev);
		ABORT_IF_EXCEPTION (&ev, "release failed");
	}

	/* shutdown ORB, shutdown IO channels */
	CORBA_ORB_shutdown (orb, FALSE, &ev);
	ABORT_IF_EXCEPTION(&ev, "ORB shutdown ...");

	/* destroy local ORB */
	CORBA_ORB_destroy(orb, &ev);
	ABORT_IF_EXCEPTION (&ev, "destroying local ORB raised exception");

	return 0;
}
