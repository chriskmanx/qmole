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



static CORBA_long opAnyLong_inout = constants_LONG_INOUT_OUT;
static CORBA_long opAnyLong_out = constants_LONG_OUT;
static CORBA_long opAnyLong_retn = constants_LONG_RETN;

static CORBA_any *
AnyServer_opAnyStrSeq (PortableServer_Servant _servant,
		       CORBA_Environment * ev)
{
	CORBA_any   *retn;
	test_StrSeq *seq;
	int          i;
	
	seq = test_StrSeq__alloc();
	seq->_length = 16;
	seq->_buffer = CORBA_sequence_CORBA_string_allocbuf (seq->_length);
	seq->_release = TRUE;

	for (i = 0; i < seq->_length; i++)
		seq->_buffer [i] = CORBA_string_dup ("Foo");

	retn = CORBA_any_alloc ();
	retn->_type = (CORBA_TypeCode) CORBA_Object_duplicate (
		(CORBA_Object) TC_test_StrSeq, ev);
	retn->_value = seq;
	retn->_release = TRUE;

	return retn;
}

static CORBA_any *
AnyServer_opAnyLong (PortableServer_Servant _servant,
		     const CORBA_any * inArg,
		     CORBA_any * inoutArg,
		     CORBA_any ** outArg,
		     CORBA_Environment * ev)
{
  CORBA_any *retn;  

  g_assert(CORBA_TypeCode_equal(inArg->_type,TC_CORBA_long,ev));
  g_assert(*(CORBA_long*)inArg->_value == constants_LONG_IN);

  g_assert(CORBA_TypeCode_equal(inoutArg->_type,TC_CORBA_long,ev));
  g_assert(*(CORBA_long*)inoutArg->_value == constants_LONG_INOUT_IN);

  if(CORBA_any_get_release(inoutArg)){
	CORBA_free(inoutArg->_value);
	CORBA_Object_release((CORBA_Object)inoutArg->_type, ev);
  }


  inoutArg->_type = (CORBA_TypeCode)TC_CORBA_long;
  inoutArg->_value = &opAnyLong_inout; 
  CORBA_any_set_release(inoutArg, CORBA_FALSE);

  *outArg = CORBA_any_alloc();
  (*outArg)->_type = (CORBA_TypeCode)TC_CORBA_long;
  (*outArg)->_value = &opAnyLong_out;
  CORBA_any_set_release(*outArg, CORBA_FALSE);

  retn = CORBA_any_alloc();
  retn->_type = (CORBA_TypeCode)TC_CORBA_long;
  retn->_value = &opAnyLong_retn;
  CORBA_any_set_release(retn, CORBA_FALSE);

  return retn;
}


static
CORBA_any *
AnyServer_opAnyString(PortableServer_Servant _servant,
					  const CORBA_any * inArg,
					  CORBA_any * inoutArg,
					  CORBA_any ** outArg,
					  CORBA_Environment * ev){
  CORBA_any *retn;  

  g_assert(CORBA_TypeCode_equal(inArg->_type,TC_CORBA_string,ev));
  g_assert(strcmp(*(CORBA_char **)inArg->_value,constants_STRING_IN) == 0);

  g_assert(CORBA_TypeCode_equal(inoutArg->_type,TC_CORBA_string,ev) );
  g_assert(strcmp(*(CORBA_char **)inoutArg->_value,constants_STRING_INOUT_IN) == 0);

  if(CORBA_any_get_release(inoutArg)){
	CORBA_free(inoutArg->_value);
	CORBA_Object_release((CORBA_Object)inoutArg->_type, ev);
  }

  inoutArg->_type = (CORBA_TypeCode)TC_CORBA_string;
  inoutArg->_value = &constants_STRING_INOUT_OUT; 
  CORBA_any_set_release(inoutArg, CORBA_FALSE);

  *outArg = CORBA_any_alloc();
  (*outArg)->_type = (CORBA_TypeCode)TC_CORBA_string;
  (*outArg)->_value = &constants_STRING_OUT;
  CORBA_any_set_release(*outArg, CORBA_FALSE);

  retn = CORBA_any_alloc();
  retn->_type = (CORBA_TypeCode)TC_CORBA_string;
  retn->_value = &constants_STRING_RETN;
  CORBA_any_set_release(retn, CORBA_FALSE);

  return retn;
}


static test_VariableLengthStruct inoutArgStruct;
static test_VariableLengthStruct outArgStruct;
static test_VariableLengthStruct retnStruct;

static
CORBA_any *
AnyServer_opAnyStruct(PortableServer_Servant _servant,
					  const CORBA_any * inArg,
					  CORBA_any * inoutArg,
					  CORBA_any ** outArg,
					  CORBA_Environment * ev){
  CORBA_any *retn;  

  g_assert(CORBA_TypeCode_equal(inArg->_type,TC_test_VariableLengthStruct,ev));
  g_assert(strcmp((*(test_VariableLengthStruct*)inArg->_value).a,constants_STRING_IN) == 0);

  g_assert(CORBA_TypeCode_equal(inoutArg->_type,TC_test_VariableLengthStruct,ev) );
  g_assert(strcmp((*(test_VariableLengthStruct*)inoutArg->_value).a,constants_STRING_INOUT_IN) == 0);

  if(CORBA_any_get_release(inoutArg)){
	CORBA_free(inoutArg->_value);
	CORBA_Object_release((CORBA_Object)inoutArg->_type, ev);
  }

  inoutArg->_type = (CORBA_TypeCode)TC_test_VariableLengthStruct;
  inoutArgStruct.a = (char *)constants_STRING_INOUT_OUT;
  inoutArg->_value = &inoutArgStruct;
  CORBA_any_set_release(inoutArg, CORBA_FALSE);

  *outArg = CORBA_any_alloc();
  (*outArg)->_type = (CORBA_TypeCode)TC_test_VariableLengthStruct;
  outArgStruct.a = (char *)constants_STRING_OUT;
  (*outArg)->_value = &outArgStruct;
  CORBA_any_set_release(*outArg, CORBA_FALSE);

  retn = CORBA_any_alloc();
  retn->_type = (CORBA_TypeCode)TC_test_VariableLengthStruct;
  retnStruct.a = (char *)constants_STRING_RETN;
  retn->_value = &retnStruct;
  CORBA_any_set_release(retn, CORBA_FALSE);

  return retn;
}


CORBA_TypeCode retntypecode = TC_test_VariableLengthStruct;

static
CORBA_TypeCode 
AnyServer_opTypeCode (PortableServer_Servant servant,
		      const CORBA_TypeCode   inArg,
		      CORBA_TypeCode        *inoutArg,
		      CORBA_TypeCode        *outArg,
		      CORBA_Environment     *ev)
{
	g_assert (CORBA_TypeCode_equal (inArg, TC_test_ArrayUnion, ev));
	g_assert (CORBA_TypeCode_equal (*inoutArg, TC_test_AnyServer, ev));
  
	CORBA_Object_release ((CORBA_Object)*inoutArg, ev);
	*inoutArg = (CORBA_TypeCode) CORBA_Object_duplicate (
		(CORBA_Object) TC_test_TestException, ev);
	*outArg = TC_test_AnEnum;

	return TC_test_VariableLengthStruct; 
}

POA_test_AnyServer__epv AnyServer_epv = {
	NULL,
	AnyServer_opAnyStrSeq,
	AnyServer_opAnyLong,
	AnyServer_opAnyString,
	AnyServer_opAnyStruct,
	AnyServer_opTypeCode
};

PortableServer_ServantBase__epv AnyServer_base_epv = {NULL, simple_finalize, NULL};
POA_test_AnyServer__vepv AnyServer_vepv = { &AnyServer_base_epv, &AnyServer_epv };
