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
 * Author: Phil Dawes <philipd@users.sourceforge.net>
 */


#include "everything.h"
#include "constants.h"
#include <stdio.h>

static test_StrSeq *
SequenceServer_opStrSeq (PortableServer_Servant _servant,
			 const test_StrSeq * inArg,
			 test_StrSeq * inoutArg,
			 test_StrSeq ** outArg,
			 CORBA_Environment * ev)
{
	test_StrSeq *retn;
	guint i;
	for (i=0;i<inArg->_length;i++)
		g_assert(strcmp(inArg->_buffer[i],constants_SEQ_STRING_IN[i]) == 0);
  
	for (i=0;i<inoutArg->_length;i++)
		g_assert(strcmp(inoutArg->_buffer[i],constants_SEQ_STRING_INOUT_IN[i]) == 0);

	if (CORBA_sequence_get_release (inoutArg))
		CORBA_free(inoutArg->_buffer);

	inoutArg->_buffer = CORBA_sequence_CORBA_string_allocbuf(2);
	inoutArg->_length = 2;
	CORBA_sequence_set_release(inoutArg, CORBA_TRUE);
  
	for (i=0;i<inoutArg->_length;i++)
		inoutArg->_buffer[i] = CORBA_string_dup(constants_SEQ_STRING_INOUT_OUT[i]);
  
	*outArg = CORBA_sequence_CORBA_string__alloc();
	(*outArg)->_buffer = CORBA_sequence_CORBA_string_allocbuf(2);
	(*outArg)->_length = 2;
	CORBA_sequence_set_release(*outArg, CORBA_TRUE);

	for (i=0;i<(*outArg)->_length;i++)
		(*outArg)->_buffer[i] = CORBA_string_dup(constants_SEQ_STRING_OUT[i]);

	retn = CORBA_sequence_CORBA_string__alloc();
	retn->_buffer = CORBA_sequence_CORBA_string_allocbuf(2);
	retn->_length = 2;
	CORBA_sequence_set_release(retn, CORBA_TRUE);

	for (i=0;i<retn->_length;i++)
		retn->_buffer[i] = CORBA_string_dup(constants_SEQ_STRING_RETN[i]);

	return retn;
}

static test_BoundedStructSeq *
SequenceServer_opBoundedStructSeq (PortableServer_Servant _servant,
				   const test_BoundedStructSeq * inArg,
				   test_BoundedStructSeq * inoutArg,
				   test_BoundedStructSeq ** outArg,
				   CORBA_Environment * ev)
{
	test_BoundedStructSeq *retn;
	CORBA_long i;
	for (i = 0; i < inArg->_length; i++)
		g_assert (strcmp (inArg->_buffer[i].a.a, constants_SEQ_STRING_IN[i]) == 0);
  
	for (i = 0; i < inoutArg->_length; i++)
		g_assert(strcmp(inoutArg->_buffer[i].a.a,constants_SEQ_STRING_INOUT_IN[i]) == 0);

	if (CORBA_sequence_get_release (inoutArg))
		CORBA_free (inoutArg->_buffer);

	inoutArg->_buffer = CORBA_sequence_test_CompoundStruct_allocbuf (2);
	inoutArg->_length = 2;
	CORBA_sequence_set_release (inoutArg, CORBA_TRUE);
  
	for (i = 0; i < inoutArg->_length; i++)
		inoutArg->_buffer[i].a.a = CORBA_string_dup (constants_SEQ_STRING_INOUT_OUT[i]);
  
	*outArg = CORBA_sequence_test_CompoundStruct__alloc ();
	(*outArg)->_buffer = CORBA_sequence_test_CompoundStruct_allocbuf (2);
	(*outArg)->_length = 2;
	CORBA_sequence_set_release (*outArg, CORBA_TRUE);

	for (i = 0; i < (*outArg)->_length; i++)
		(*outArg)->_buffer[i].a.a = CORBA_string_dup (constants_SEQ_STRING_OUT[i]);
  
  
	retn = CORBA_sequence_test_CompoundStruct__alloc ();
	retn->_buffer = CORBA_sequence_test_CompoundStruct_allocbuf (2);
	retn->_length = 2;
	CORBA_sequence_set_release (retn, CORBA_TRUE);

	for (i = 0; i < retn->_length; i++)
		retn->_buffer[i].a.a = CORBA_string_dup (constants_SEQ_STRING_RETN[i]);

	return retn;
}

static test_LongSeq *
SequenceServer_opMassiveSeq (PortableServer_Servant servant,
			     CORBA_Environment     *ev)
{
	test_LongSeq *retn;
	CORBA_long i;
	CORBA_long n = 400000;

	retn = test_LongSeq__alloc ();
	retn->_buffer = test_LongSeq_allocbuf (n);
	retn->_length = n;
	CORBA_sequence_set_release (retn, CORBA_TRUE);

	for (i = 0; i < retn->_length; i++)
		retn->_buffer[i] = i;

	return retn;
}

static test_AnySeq *
SequenceServer_opAnySeq (PortableServer_Servant servant,
                         CORBA_Environment     *ev)
{
	test_AnySeq *retn;
	CORBA_long i;
	CORBA_long n = 1000;

	retn = test_AnySeq__alloc ();
	retn->_buffer = test_AnySeq_allocbuf (n);
	retn->_length = n;
	CORBA_sequence_set_release (retn, CORBA_TRUE);

	for (i = 0; i < retn->_length; i++) {
		if (i < 500)
			retn->_buffer[i]._type = TC_void;
		else
			retn->_buffer[i]._type = TC_null;
		retn->_buffer[i]._value = NULL;
		retn->_buffer[i]._release = CORBA_FALSE;
        }
	return retn;
}

POA_test_SequenceServer__epv SequenceServer_epv = {
	NULL,
	SequenceServer_opStrSeq,
	SequenceServer_opBoundedStructSeq,
	SequenceServer_opMassiveSeq,
	SequenceServer_opAnySeq
};

PortableServer_ServantBase__epv SequenceServer_base_epv = {NULL, simple_finalize, NULL};
POA_test_SequenceServer__vepv SequenceServer_vepv = { &SequenceServer_base_epv, &SequenceServer_epv };
