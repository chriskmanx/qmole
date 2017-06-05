#include <stdio.h>
#include <unistd.h>

#include "everything.h"
#include "constants.h"

extern PortableServer_POA global_poa;

typedef struct {
	POA_test_PingPongServer baseServant;

	CORBA_Object            registered;
} test_PingPongServer_Servant;

static void
PingPongServer_set (PortableServer_Servant    servant,
		    CORBA_Object              object,
		    const CORBA_char          *name,
		    CORBA_Environment        *ev)
{
	test_PingPongServer_Servant *this;

	this = (test_PingPongServer_Servant *) servant;

	this->registered = CORBA_Object_duplicate (object, ev);
}

static CORBA_Object
PingPongServer_get (PortableServer_Servant    servant,
		    const CORBA_char          *name,
		    CORBA_Environment        *ev)
{
	test_PingPongServer_Servant *this;

	this = (test_PingPongServer_Servant *) servant;

	return CORBA_Object_duplicate (this->registered, ev);
}

static void
PingPongServer_opSleep (PortableServer_Servant  servant,
			const char             *large_string,
			CORBA_Environment      *ev)
{
	/* Don't process the buffer - it should fill up at the other end */
	g_usleep (10000);
}

static void
PingPongServer_opOneWay (PortableServer_Servant servant,
			 const CORBA_long       l,
			 CORBA_Environment     *ev)
{
	/* Do nothing, but try and confuse the queue */
	link_main_iteration (FALSE);
}

static void
PingPongServer_opOneWayCallback (PortableServer_Servant servant,
				 test_PingPongServer    remote_obj,
				 CORBA_Environment     *ev)
{
	static int depth = 0;

	depth++;
	if (depth % 400 == 0 && depth > 1)
		fprintf (stderr, " recursion depth %d\n", depth);

	g_assert (ORBit_small_get_connection_status (remote_obj)
		  != ORBIT_CONNECTION_IN_PROC);
	
	/* While this is blocking, loads more incoming
	 * calls will trash our stack - quite possibly */
	test_PingPongServer_opRoundTrip (remote_obj, ev);

	depth--;
}

static void
PingPongServer_opRoundTrip (PortableServer_Servant servant,
			    CORBA_Environment     *ev)
{
	/* do nothing, but forces a round-trip */
}

static CORBA_long
PingPongServer_pingPong (PortableServer_Servant    servant,
			 const test_PingPongServer replyTo,
			 const CORBA_long          idx,
			 CORBA_Environment        *ev)
{
	CORBA_long   ret;
	CORBA_Object me;

	me = PortableServer_POA_servant_to_reference (
		global_poa, servant, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	test_PingPongServer_opOneWay (replyTo, 3, ev);

	if (idx > 0)
		ret = test_PingPongServer_pingPong (replyTo, me, idx - 1, ev);
	else
		ret = 0;

	CORBA_Object_release (me, ev);

	return ret;
}

static void
ping_pong_finalize (PortableServer_Servant servant,
		    CORBA_Environment     *ev)
{
	test_PingPongServer_Servant *this;

	this = (test_PingPongServer_Servant *) servant;

	CORBA_Object_release (this->registered, ev);

	g_free (servant);
}

PortableServer_ServantBase__epv PingPongServer_base_epv = {
	NULL, ping_pong_finalize, NULL
};

POA_test_PingPongServer__epv PingPongServer_epv = {
	NULL,
	PingPongServer_opSleep,
	PingPongServer_opOneWay,
	PingPongServer_opOneWayCallback,
	PingPongServer_opRoundTrip,
	PingPongServer_pingPong,
	PingPongServer_set,
	PingPongServer_get
};

POA_test_PingPongServer__vepv PingPongServer_vepv = {
	&PingPongServer_base_epv,
	&PingPongServer_epv
};

static POA_test_PingPongServer *
create_ping_pong_servant (void)
{
	CORBA_Environment ev[1];
	test_PingPongServer_Servant *servant;
	
	servant = g_new0 (test_PingPongServer_Servant, 1);
	servant->baseServant.vepv = &PingPongServer_vepv;

	servant->registered = CORBA_OBJECT_NIL;

	CORBA_exception_init (ev);
	POA_test_PingPongServer__init (servant, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_exception_free (ev);

	return (POA_test_PingPongServer *) servant;
};
