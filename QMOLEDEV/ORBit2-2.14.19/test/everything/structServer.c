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

static test_FixedLengthStruct
StructServer_opFixed (PortableServer_Servant        servant,
		      const test_FixedLengthStruct *inArg,
		      test_FixedLengthStruct       *inoutArg,
		      test_FixedLengthStruct       *outArg,
		      CORBA_Environment            *ev)
{
	test_FixedLengthStruct retval;
	g_assert (inArg->a == constants_SHORT_IN);
	g_assert (inoutArg->a == constants_SHORT_INOUT_IN);
  
	inoutArg->a = constants_SHORT_INOUT_OUT;
	outArg->a   = constants_SHORT_OUT;
	retval.a    = constants_SHORT_RETN;

	return retval;
}



static test_VariableLengthStruct*
StructServer_opVariable (PortableServer_Servant          servant,
			const test_VariableLengthStruct *inArg,
			test_VariableLengthStruct       *inoutArg,
			test_VariableLengthStruct      **outArg,
			CORBA_Environment               *ev)
{
	test_VariableLengthStruct *retval;
	g_assert (!strcmp (inArg->a,constants_STRING_IN));
	g_assert (!strcmp (inoutArg->a,constants_STRING_INOUT_IN));
  
	*outArg = test_VariableLengthStruct__alloc ();
	retval  = test_VariableLengthStruct__alloc ();
  
	CORBA_free (inoutArg->a);

	inoutArg->a  = CORBA_string_dup (constants_STRING_INOUT_OUT);
	(*outArg)->a = CORBA_string_dup (constants_STRING_OUT);
	retval->a    = CORBA_string_dup (constants_STRING_RETN);
  
	return retval;
}

static test_CompoundStruct *
StructServer_opCompound (PortableServer_Servant     servant,
			 const test_CompoundStruct *inArg,
			 test_CompoundStruct       *inoutArg,
			 test_CompoundStruct      **outArg,
			 CORBA_Environment         *ev)
{
	test_CompoundStruct *retval;
	g_assert (!strcmp (inArg->a.a,constants_STRING_IN));
	g_assert (!strcmp (inoutArg->a.a,constants_STRING_INOUT_IN));
  
	*outArg = test_CompoundStruct__alloc ();
	retval  = test_CompoundStruct__alloc ();
  
	CORBA_free (inoutArg->a.a);

	inoutArg->a.a = CORBA_string_dup (constants_STRING_INOUT_OUT);
	(*outArg)->a.a = CORBA_string_dup (constants_STRING_OUT);
	retval->a.a = CORBA_string_dup (constants_STRING_RETN);
  
	return retval;
}

static test_AlignHoleStruct
StructServer_opAlignHole (PortableServer_Servant     servant,
			 const test_AlignHoleStruct *inArg,
			 test_AlignHoleStruct       *inoutArg,
			 test_AlignHoleStruct      *outArg,
			 CORBA_Environment         *ev)
{
	test_AlignHoleStruct retval;
	g_assert (inArg->a.a == constants_DOUBLE_IN);
	g_assert (inArg->a.b == constants_OCTET_IN);
	g_assert (inArg->b == constants_CHAR_IN);

	g_assert (inoutArg->a.a == constants_DOUBLE_INOUT_IN);
	g_assert (inoutArg->a.b == constants_OCTET_INOUT_IN);
	g_assert (inoutArg->b == constants_CHAR_INOUT_IN);
  
	inoutArg->a.a = constants_DOUBLE_INOUT_OUT;
	inoutArg->a.b = constants_OCTET_INOUT_OUT;
	inoutArg->b = constants_CHAR_INOUT_OUT;

	outArg->a.a = constants_DOUBLE_OUT;
	outArg->a.b = constants_OCTET_OUT;
	outArg->b = constants_CHAR_OUT;

	retval.a.a = constants_DOUBLE_RETN;
	retval.a.b = constants_OCTET_RETN;
	retval.b = constants_CHAR_RETN;

	return retval;
}

static void
StructServer_opObjectStruct (PortableServer_Servant   servant,
			     const test_ObjectStruct *inArg,
			     CORBA_Environment       *ev)
{
	CORBA_Object    objref;
	test_StructAny *val;

	objref = CORBA_Object_duplicate (inArg->serv, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	val = test_StructServer_opStructAny (inArg->serv, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	
	CORBA_free (val);

	CORBA_Object_release (objref, ev);
}

static test_StructAny *
StructServer_opStructAny (PortableServer_Servant servant,
			  CORBA_Environment     *ev)
{
	test_StructAny   *a = test_StructAny__alloc ();
	static CORBA_long l;

	a->a = CORBA_string_dup (constants_STRING_IN);

	l = constants_LONG_IN;
	a->b._release = FALSE;
	a->b._value = &l;
	a->b._type  = TC_CORBA_long;

	return a;
}

POA_test_StructServer__epv StructServer_epv = {
	NULL,
	StructServer_opFixed,
	StructServer_opVariable,
	StructServer_opCompound,
	StructServer_opAlignHole,
	StructServer_opObjectStruct,
	StructServer_opStructAny
};

PortableServer_ServantBase__epv StructServer_base_epv = {NULL, simple_finalize, NULL};
POA_test_StructServer__vepv StructServer_vepv = {&StructServer_base_epv,&BasicServer_epv,&StructServer_epv};

POA_test_StructServer StructServer_servant = {NULL,&StructServer_vepv};  /* Singleton */
