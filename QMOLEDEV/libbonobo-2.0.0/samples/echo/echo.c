/*
 * echo.c: Implements a Bonobo Echo server
 *
 * Author:
 *   Miguel de Icaza (miguel@ximian.com)
 *
 * This file is here to show what are the basic steps
 * neccessary to create a Bonobo Component.
 */
#include <config.h>
#include <libbonobo.h>

/*
 * This pulls the CORBA definitions for the Demo::Echo server
 */
#include "Bonobo_Sample_Echo.h"

/*
 * This pulls the definition for the BonoboObject (Gtk Type)
 */
#include "echo.h"

/*
 * A pointer to our parent object class
 */
static GObjectClass *echo_parent_class;

/*
 * Implemented GObject::finalize
 */
static void
echo_object_finalize (GObject *object)
{
	Echo *echo = ECHO (object);

	g_free (echo->instance_data);
	
	echo_parent_class->finalize (object);
}

/*
 * CORBA Demo::Echo::echo method implementation
 */
static void
impl_demo_echo_echo (PortableServer_Servant  servant,
		     const CORBA_char       *string,
		     CORBA_Environment      *ev)
{
	Echo *echo = ECHO (bonobo_object (servant));
									 
	printf ("Echo message received: %s (echo instance data: %s)\n",
		string, echo->instance_data);
}

static void
echo_class_init (EchoClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;
	POA_Bonobo_Sample_Echo__epv *epv = &klass->epv;

	echo_parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = echo_object_finalize;

	epv->echo = impl_demo_echo_echo;
}

static void
echo_init (Echo *echo)
{
	static int i = 0;

	echo->instance_data = g_strdup_printf ("Hello %d!", i++);
}

BONOBO_TYPE_FUNC_FULL (
	Echo,                /* Glib class name */
	Bonobo_Sample_Echo,  /* CORBA interface name */
	BONOBO_TYPE_OBJECT,  /* parent type */
	echo);               /* local prefix ie. 'echo'_class_init */
