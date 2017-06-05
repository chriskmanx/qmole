/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkm-rpc-daemon-standalone.c - A sample daemon.

   Copyright (C) 2008, Stef Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "pkcs11/pkcs11.h"

#include "gkm-rpc-layer.h"

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include <dlfcn.h>
#include <pthread.h>

#define SOCKET_PATH "/tmp/gkm-rpc-daemon.sock"

/* Sample configuration for loading NSS remotely */
static CK_C_INITIALIZE_ARGS p11_init_args = {
	NULL,
        NULL,
        NULL,
        NULL,
        CKF_OS_LOCKING_OK,
        "init-string = configdir='/tmp' certPrefix='' keyPrefix='' secmod='/tmp/secmod.db' flags="
};

static int is_running = 1;

static int
usage (void)
{
	fprintf (stderr, "usage: gkm-rpc-daemon pkcs11-module\n");
	exit (2);
}

int
main (int argc, char *argv[])
{
	CK_C_GetFunctionList func_get_list;
	CK_FUNCTION_LIST_PTR funcs;
	void *module;
	fd_set read_fds;
	int sock, ret;
	CK_RV rv;

	/* The module to load is the argument */
	if (argc != 2)
		usage();

	/* Load the library */
	module = dlopen(argv[1], RTLD_NOW);
	if(!module) {
		fprintf (stderr, "couldn't open library: %s: %s\n", argv[1], dlerror());
		exit (1);
	}

	/* Lookup the appropriate function in library */
	func_get_list = (CK_C_GetFunctionList)dlsym (module, "C_GetFunctionList");
	if (!func_get_list) {
		fprintf (stderr, "couldn't find C_GetFunctionList in library: %s: %s\n", argv[1], dlerror());
		exit (1);
	}

	/* Get the function list */
	rv = (func_get_list) (&funcs);
	if (rv != CKR_OK || !funcs) {
		fprintf (stderr, "couldn't get function list from C_GetFunctionList in libary: %s: 0x%08x\n",
		         argv[1], (int)rv);
		exit (1);
	}

	/* RPC layer expects initialized module */
	rv = (funcs->C_Initialize) (&p11_init_args);
	if (rv != CKR_OK) {
		fprintf (stderr, "couldn't initialize module: %s: 0x%08x\n", argv[1], (int)rv);
		exit (1);
	}

	gkm_rpc_layer_initialize (funcs);
	sock = gkm_rpc_layer_startup (SOCKET_PATH);
	if (sock == -1)
		exit (1);

	is_running = 1;
	while (is_running) {
		FD_ZERO (&read_fds);
		FD_SET (sock, &read_fds);
		ret = select (sock + 1, &read_fds, NULL, NULL, NULL);
		if (ret < 0) {
			if (errno == EINTR)
				continue;
			fprintf (stderr, "error watching socket: %s\n", strerror (errno));
			exit (1);
		}

		if (FD_ISSET (sock, &read_fds))
			gkm_rpc_layer_accept ();
	}

	gkm_rpc_layer_shutdown ();
	gkm_rpc_layer_uninitialize ();

	rv = (funcs->C_Finalize) (NULL);
	if (rv != CKR_OK)
		fprintf (stderr, "couldn't finalize module: %s: 0x%08x\n", argv[1], (int)rv);

	dlclose(module);

	return 0;
}
