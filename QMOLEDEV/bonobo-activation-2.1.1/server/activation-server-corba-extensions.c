/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  bonobo_activation
 *
 *  Copyright (C) 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this library; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Authors: Maciej Stachowiak <mjs@eazel.com>
 *
 */

/* bonobo-activation-corba-extensions.c - CORBA utility functions
 */

#include <config.h>
#include <stdio.h>
#include <string.h>

#include "server.h"

#include "activation-server-corba-extensions.h"
#include <bonobo-activation/bonobo-activation-i18n.h>

char *
activation_server_CORBA_Context_get_value (CORBA_Context         ctx, 
                                           const char           *propname,
                                           const CORBA_char     *exception_if_fail,
                                           CORBA_Environment    *ev)
{
	CORBA_NVList nvout;
	char *retval;
        CORBA_Environment local_ev;

        CORBA_exception_init (&local_ev);

        retval = NULL;

	CORBA_Context_get_values (ctx, NULL, 0, (char *) propname, &nvout, &local_ev);

	if (local_ev._major == CORBA_NO_EXCEPTION) {
		if (nvout->list->len > 0) {
			CORBA_NamedValue *nv;
                        int i;

                        nv = NULL;

                        for (i = 0; i < nvout->list->len; i++) {
                                nv = &g_array_index (nvout->list, 
                                                     CORBA_NamedValue, i);
                                if (!strcmp (nv->name, propname)) break;
                        }

                        if (i < nvout->list->len)
                                retval = g_strdup (*(char **) nv->argument._value);
		} 

		CORBA_NVList_free (nvout, &local_ev);
	} else {                
                if (exception_if_fail != NULL) {
                        CORBA_exception_set (ev, 
                                             CORBA_USER_EXCEPTION,
                                             exception_if_fail,
                                             NULL);
                }
        }

        CORBA_exception_free (&local_ev);

	return retval;
}


