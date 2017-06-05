#include "config.h"
#include <orbit/orbit.h>

void
CORBA_Request_add_arg (CORBA_Request            _obj,
		       const CORBA_char        *name,
		       const CORBA_TypeCode     arg_type,
		       const CORBA_OpaqueValue  value,
		       const CORBA_long         len,
		       const CORBA_Flags        arg_flags,
		       CORBA_Environment       *ev)
{
}

void
CORBA_Request_invoke(CORBA_Request _obj,
		     const CORBA_Flags invoke_flags,
		     CORBA_Environment * ev)
{
}

void
CORBA_Request_delete(CORBA_Request _obj, CORBA_Environment * ev)
{
}

void
CORBA_Request_send(CORBA_Request _obj, const CORBA_Flags invoke_flags,
		   CORBA_Environment * ev)
{
}

void
CORBA_Request_get_response(CORBA_Request _obj,
			   CORBA_Environment * ev)
{
}

CORBA_boolean
CORBA_Request_poll_response(CORBA_Request _obj,
			    CORBA_Environment * ev)
{
  return CORBA_FALSE;
}
