/*
 * CORBA echo tests
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
#include <signal.h>
#include "echo.h"
#include "echo-share.h"

/*public*/ gboolean echo_opt_quiet = 0;

int
main (int argc, char *argv[])
{
	FILE *iorfile;
	CORBA_Environment ev;
	CORBA_ORB orb;
	Echo echo_client = CORBA_OBJECT_NIL;
	char *retval;

	signal(SIGINT, exit);
	signal(SIGTERM, exit);

	CORBA_exception_init(&ev);
	orb = CORBA_ORB_init(&argc, argv, "orbit-local-orb", &ev);
	g_assert(ev._major == CORBA_NO_EXCEPTION);

	echo_srv_start_poa(orb, &ev);
	g_assert(ev._major == CORBA_NO_EXCEPTION);
	echo_client = echo_srv_start_object(&ev);
	retval = CORBA_ORB_object_to_string(orb, echo_client, &ev);
	g_assert(ev._major == CORBA_NO_EXCEPTION);

	iorfile = fopen ("echo-server.iorfile", "w");
	fprintf(iorfile, "%s\n", retval);
	fclose(iorfile);

	fprintf(stdout, "%s\n", retval);

	CORBA_free(retval);

	CORBA_ORB_run (orb, &ev);

	echo_srv_finish_object(&ev);
	echo_srv_finish_poa(&ev);
	CORBA_exception_free(&ev);

	return 0;
}
