/*
 * CORBA emopty test
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

#include "empty.h"

static Empty empty_client;

#define ABORT_IF_EXCEPTION(_ev, _message)                    \
if ((_ev)->_major != CORBA_NO_EXCEPTION) {                   \
  g_error("%s: %s", _message, CORBA_exception_id (_ev));     \
  CORBA_exception_free (_ev);                                \
  abort();                                                   \
}

int
main (int argc, char *argv[])
{
	CORBA_Environment ev;
	CORBA_ORB orb;
	int i;

	int niters = 100;

	CORBA_exception_init(&ev);
	orb = CORBA_ORB_init(&argc, argv, "orbit-local-orb", &ev);
	ABORT_IF_EXCEPTION(&ev, "ORB init ...");

#if 0
	for(i = 0; i < (sizeof(theblah) - 1); i++)
		theblah[i] = 'a';
	theblah[sizeof(theblah) - 1] = '\0';
#endif

	if(argc < 2) {
		printf("Need a binding ID thing as argv[1]\n");
		return 1;
	}

	if(argc == 3)
		niters = atoi(argv[2]);

	/* bind to object */
	empty_client = CORBA_ORB_string_to_object(orb, argv[1], &ev);
	ABORT_IF_EXCEPTION(&ev, "Cannot bind to object ...");

	printf("corba = %d, empty = %d, foobar = %d\n",
	       CORBA_Object_is_a(empty_client, "IDL:CORBA/Object:1.0", &ev),
	       CORBA_Object_is_a(empty_client, "IDL:Empty:1.0", &ev),
	       CORBA_Object_is_a(empty_client, "IDL:Foo/Bar:1.0", &ev));

	for(i = 0; i < niters; i++) {
		Empty_doNothing(empty_client, &ev);
		ABORT_IF_EXCEPTION(&ev, "doNothing() ...");
	}

	/* release initial object reference */
	CORBA_Object_release(empty_client, &ev);
	ABORT_IF_EXCEPTION(&ev, "release empty_client ...");
	
	/* shutdown ORB, shutdown IO channels */
	CORBA_ORB_shutdown (orb, FALSE, &ev);
	ABORT_IF_EXCEPTION(&ev, "ORB shutdown ...");

	/* destroy local ORB */
	CORBA_ORB_destroy (orb, &ev);
	ABORT_IF_EXCEPTION(&ev, "ORB destroy ...");

	return 0;
}
