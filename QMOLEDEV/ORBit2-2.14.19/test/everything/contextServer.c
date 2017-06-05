/*
 * CORBA C language mapping tests
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
 * Author: Michael Meeks <michael@ximian.com>
 */

#include "everything.h"
#include "constants.h"
#include <stdio.h>

static  
CORBA_Object
ContextServer_opWithContext (PortableServer_Servant _servant,
			     const CORBA_Object     inArg,
			     CORBA_Object          *inoutArg,
			     CORBA_Object          *outArg,
			     CORBA_Context          ctx,
			     CORBA_Environment     *ev)
{
	CORBA_NVList      nvout;
	CORBA_NamedValue *nv;
	char             *val;
	int               i;

	CORBA_Context_get_values (ctx, NULL, 0, "", &nvout, ev);

	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (nvout->list->len == 2);

	for (i = 0; i < nvout->list->len; i++) {
		nv = &g_array_index (nvout->list, 
				     CORBA_NamedValue, 0);
		if (!strcmp (nv->name, "bar")) {
			val = * (char **) nv->argument._value;
			g_assert (!strcmp (val, "baaaa"));
		} else if (!strcmp (nv->name, "foo")) {
			val = * (char **) nv->argument._value;
			g_assert (!strcmp (val, "foo2"));
		} else
			g_error ("Unknown context property '%s'", nv->name);
	}

	CORBA_NVList_free (nvout, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	*outArg = CORBA_OBJECT_NIL;

	return CORBA_Object_duplicate (inArg, ev);
}

POA_test_ContextServer__epv ContextServer_epv = {
	NULL,
	ContextServer_opWithContext
};

PortableServer_ServantBase__epv ContextServer_base_epv = {NULL, simple_finalize, NULL};
POA_test_ContextServer__vepv ContextServer_vepv = { &ContextServer_base_epv, &ContextServer_epv };
