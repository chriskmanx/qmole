/*
 * CORBA GIOP timeout test
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
 * Author: Jules Colding <colding@omesc.com>
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <glib.h>
#include <glib/gstdio.h>

#include "timeout.h"

/*
 * This method will return an ORB which is so initialized
 * as to set a specific GIOP timeout value.
 *
 * orb_name     : Name of return ORB or empty string
 *
 * timeout      : GIOP timeout in milliseconds
 *
 * Return value : Initialized ORB or CORBA::ORB::_nil()
 *
 */
static CORBA_ORB
create_timeout_orb(const char *orb_name,
		   const char *timeout,
		   CORBA_Environment *ev)
{
	CORBA_ORB orb = CORBA_OBJECT_NIL;
	char timeout_str[128] = { '\0' };
	int argc = 0;
	char **argv = NULL;

	// sanity checks
	if (!orb_name)
		return CORBA_OBJECT_NIL;
	if (!timeout)
		return CORBA_OBJECT_NIL;

	snprintf (timeout_str, sizeof(timeout_str), "--GIOPTimeoutMSEC=%s", timeout);

	argc = 2;
	argv = (char**)malloc (sizeof(char*) * argc);
	if (!argv)
		return CORBA_OBJECT_NIL;
	memset ((void*)argv, 0, argc);

	//  dummy argument
	argv[0] = "timeout-client";

	// Set a timeout limit for GIOP operations
	argv[1] = timeout_str;

	// initialize the ORB
	orb = CORBA_ORB_init (&argc, argv, (char*)orb_name, ev);
	if (ev->_major != CORBA_NO_EXCEPTION)
		orb = CORBA_OBJECT_NIL;

	free (argv);

	return orb;
}

static CORBA_Object
object_ref_from_file(CORBA_ORB orb,
		     gchar *filename,
		     CORBA_Environment *ev)
{
	CORBA_Object obj = CORBA_OBJECT_NIL;
	CORBA_char *objref = NULL;
	FILE *file = NULL;
	struct stat st;
	size_t c = 0;

	file = g_fopen(filename, "r");
	if (!file)
		return CORBA_OBJECT_NIL;

	if (g_stat(filename, &st))
		goto out;

	objref = g_malloc0(st.st_size + 1);
	if (!objref)
		goto out;

	// must work even if sizeof(char) != sizeof(CORBA_char) (should be impossible, I know, but bad things happen...)
	c = fread((void*)objref, sizeof(CORBA_char), (size_t)(st.st_size/sizeof(CORBA_char)), file);
	if (c != st.st_size)
		goto out;

	obj = (CORBA_Object)CORBA_ORB_string_to_object(orb, objref, ev);

out:
	g_free(objref);
	fclose(file);

	return obj;
}

static Timeout
get_timeout_ref(CORBA_ORB orb)
{
	Timeout obj = CORBA_OBJECT_NIL;
	CORBA_boolean cb = CORBA_FALSE;
	CORBA_Environment ev[1];

	CORBA_exception_init (ev);

	obj = object_ref_from_file(orb, "timeout-server.iorfile", ev);
	if (ev->_major != CORBA_NO_EXCEPTION) {
		g_print ("object_ref_from_file(): %s\n", CORBA_exception_id (ev));
		obj = CORBA_OBJECT_NIL;
		goto out;
	}

	cb = CORBA_Object_is_nil((CORBA_Object)obj, ev);
	if (ev->_major != CORBA_NO_EXCEPTION) {
		g_print ("create_timeout_orb(): %s\n", CORBA_exception_id (ev));
		obj = CORBA_OBJECT_NIL;
		goto out;
	}
	if (cb) {
		g_print ("Could not get Timeout reference\n");
		goto out;
	}

out:
	CORBA_exception_free (ev);

	return obj;
}

int
main (int argc, char *argv[])
{
	Timeout timeout_obj = CORBA_OBJECT_NIL;
	CORBA_Environment ev;
	CORBA_ORB orb;
	int retv = EXIT_FAILURE;

	g_thread_init (NULL);

	CORBA_exception_init (&ev);

	/* create timeout orb */
	orb = create_timeout_orb ("orbit-io-thread", "2000", &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("create_timeout_orb(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}

	/* get ref */
	if (argc == 1)
		timeout_obj = get_timeout_ref(orb);
	else if (argc == 2)
		timeout_obj = (Timeout)CORBA_ORB_string_to_object (orb, argv[1], &ev);
	else {
		g_print ("ERROR, usage: %s [ior]\n", argv[0]);
	}
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("CORBA_ORB_string_to_object(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}

	/*
	 * test GIOP timeout
	 */

	g_print ("Provoking timeout exception... ");
	Timeout_ping (timeout_obj, 3, &ev);
	if (ev._major == CORBA_NO_EXCEPTION) {
		g_print ("ERROR: Timeout exception expected\n");
		goto out;
	} else {
		if (strcmp (CORBA_exception_id (&ev), ex_CORBA_TIMEOUT)) {
			g_print ("Timeout_ping(): %s\n", CORBA_exception_id (&ev));
			goto out;
		}
	}
	CORBA_exception_free (&ev);
	CORBA_exception_init (&ev);
	g_print ("OK\n");

	g_print ("Testing reacquired connection with no server delay... ");
	Timeout_ping (timeout_obj, 0, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("Timeout_ping(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}
	g_print ("OK\n");

	/* test no timeout but with a small delay */
	g_print ("Testing with small server delay... ");
	Timeout_ping (timeout_obj, 1, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("Timeout_ping(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}
	g_print ("OK\n");

	g_print ("Provoking timeout exception... ");
	Timeout_ping (timeout_obj, 3, &ev);
	if (ev._major == CORBA_NO_EXCEPTION) {
		g_print ("ERROR: Timeout exception expected\n");

		goto out;
	} else {
		if (strcmp (CORBA_exception_id (&ev), ex_CORBA_TIMEOUT)) {
			g_print ("Timeout_ping(): %s\n", CORBA_exception_id (&ev));
			goto out;
		}
	}
	CORBA_exception_free (&ev);
	CORBA_exception_init (&ev);
	g_print ("OK\n");

	g_print ("Testing reacquired connection with no server delay... ");
	Timeout_ping (timeout_obj, 0, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("Timeout_ping(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}
	g_print ("OK\n");
	
	retv = EXIT_SUCCESS;

out:
	g_print ("Shutting down GIOP timeout tests... ");

	/* release object reference */
	CORBA_Object_release(timeout_obj, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("CORBA_Object_release(): %s\n", CORBA_exception_id (&ev));
		retv = EXIT_FAILURE;
		goto fast_out;
	}

	/* shutdown ORB, shutdown IO channels */
	CORBA_ORB_shutdown (orb, FALSE, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("CORBA_ORB_shutdown(): %s\n", CORBA_exception_id (&ev));
		retv = EXIT_FAILURE;
		goto fast_out;
	}

	/* destroy local ORB */
	CORBA_ORB_destroy(orb, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("CORBA_ORB_destroy(): %s\n", CORBA_exception_id (&ev));
		retv = EXIT_FAILURE;
		goto fast_out;
	}

	g_print ("OK\n");

fast_out:
	CORBA_exception_free (&ev);

	if (retv == EXIT_FAILURE)
		g_print ("Some GIOP timeout tests failed\n");
	else
		g_print ("All GIOP timeout tests passed OK\n");

	return retv;
}
