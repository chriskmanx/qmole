#include "config.h"
#include <orbit/orbit.h>
#include <string.h>

CORBA_sequence_CORBA_octet*
ORBit_sequence_CORBA_octet_dup (const CORBA_sequence_CORBA_octet *in)
{
	CORBA_sequence_CORBA_octet *retval =
		CORBA_sequence_CORBA_octet__alloc ();

	*retval = *in;

	if (in->_buffer) {
		retval->_buffer = ORBit_alloc_simple (in->_length);
		memcpy (retval->_buffer, in->_buffer, in->_length);
		retval->_release = CORBA_TRUE;
	}

	return retval;
}
