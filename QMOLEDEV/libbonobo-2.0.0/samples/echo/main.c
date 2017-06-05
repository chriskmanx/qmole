/*
 * main.c: Startup code for the Echo Bonobo Component.
 *
 * Author:
 *   Miguel de Icaza (miguel@ximian.com)
 *
 * (C) 1999, 2001 Ximian, Inc. http://www.ximian.com
 */
#include <config.h>

#include <libbonobo.h>

#include "Bonobo_Sample_Echo.h"
#include "echo.h"

static BonoboObject *
echo_factory (BonoboGenericFactory *this_factory,
	      const char           *iid,
	      gpointer              user_data)
{
	return g_object_new (ECHO_TYPE, NULL);
}

BONOBO_ACTIVATION_FACTORY ("OAFIID:Bonobo_Sample_Echo_Factory",
			   "Sample Echo component factory", "1.0",
			   echo_factory, NULL);
