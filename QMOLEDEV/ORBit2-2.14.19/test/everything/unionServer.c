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

#include <stdio.h>
#include <string.h>

#include "everything.h"
#include "constants.h"

static  
test_FixedLengthUnion
UnionServer_opFixed(PortableServer_Servant _servant,
					 const test_FixedLengthUnion *inArg,
					test_FixedLengthUnion *inoutArg,
					 test_FixedLengthUnion *outArg,
					 CORBA_Environment * ev){
  test_FixedLengthUnion retval;
  g_assert(inArg->_d == 'a');
  g_assert(inArg->_u.x == constants_LONG_IN);
  
  g_assert(inoutArg->_d == 'b');
  g_assert(inoutArg->_u.y == 't');

  inoutArg->_u.z = TRUE;
  inoutArg->_d = 'c';

  outArg->_u.x = constants_LONG_OUT;
  outArg->_d = 'a';

  retval._u.z = FALSE;
  retval._d = 'd';
  return retval;
}

static
test_VariableLengthUnion *
UnionServer_opVariable(PortableServer_Servant _servant,
					   const test_VariableLengthUnion * inArg,
					   test_VariableLengthUnion * inoutArg,
					   test_VariableLengthUnion ** outArg,
					   CORBA_Environment * ev){
  test_VariableLengthUnion *retval;
  
  g_assert(inArg->_d == 1);
  g_assert(inArg->_u.x == constants_LONG_IN);
  
  g_assert(inoutArg->_d == 2);
  g_assert(strcmp(inoutArg->_u.y,constants_STRING_INOUT_IN)==0);

  CORBA_free(inoutArg->_u.y);
  inoutArg->_u.z = TRUE;
  inoutArg->_d = 3;
  
  *outArg = test_VariableLengthUnion__alloc();
  (*outArg)->_u.x = constants_LONG_OUT;
  (*outArg)->_d = 1;

  retval = test_VariableLengthUnion__alloc();
  retval->_u.z = FALSE;
  retval->_d = 4;
  return retval;
}

static test_EnumUnion
UnionServer_opMisc (PortableServer_Servant    servant,
		    const test_unionSeq      *inSeq,
		    const test_BooleanUnion  *inArg,
		    test_ArrayUnion         **outArg,
		    CORBA_Environment        *ev)
{
	test_EnumUnion retval;
	int            i;

	g_assert (inSeq->_length == 3);
	g_assert (inSeq->_buffer [0]._d == 4);
	g_assert (inSeq->_buffer [0]._u.z == CORBA_TRUE);
	g_assert (inSeq->_buffer [1]._d == 2);
	g_assert (!strcmp (inSeq->_buffer [1]._u.y, "blah"));
	g_assert (inSeq->_buffer [2]._d == 55);
	g_assert (inSeq->_buffer [2]._u.w == constants_LONG_IN);

	g_assert (inArg->_d == 1);
	g_assert (!strcmp (inArg->_u.y, "blah de blah"));

	(*outArg) = test_ArrayUnion__alloc ();
	(*outArg)->_d = 22;
	for (i = 0; i < 20; i++) {
		char *tmp;

		tmp = g_strdup_printf ("Numero %d", i);
		(*outArg)->_u.d [i] = CORBA_string_dup (tmp);
		g_free (tmp);
	}

	retval._d   = test_EnumUnion_red;
	retval._u.x = constants_LONG_IN;

	return retval;
}

static test_FixedLengthUnionArray_slice *
UnionServer_opFixedLengthUnionArray(PortableServer_Servant _servant,
				    const test_FixedLengthUnionArray inArg,
				    test_FixedLengthUnionArray inoutArg,
				    test_FixedLengthUnionArray outArg,
				    CORBA_Environment *ev)
{
  test_FixedLengthUnionArray_slice *retn;

  g_assert (inArg[0]._d == 'a');
  g_assert (inArg[0]._u.x == constants_LONG_IN);
  g_assert (inArg[1]._d == 'b');
  g_assert (inArg[1]._u.y == constants_CHAR_IN);
  g_assert (inArg[2]._d == 'c');
  g_assert (inArg[3]._d == 'e');
  g_assert (inArg[3]._u.v.a == constants_SHORT_IN);

  g_assert (inoutArg[0]._d == 'a');
  g_assert (inoutArg[0]._u.x == constants_LONG_INOUT_IN);
  g_assert (inoutArg[1]._d == 'b');
  g_assert (inoutArg[1]._u.y == constants_CHAR_INOUT_IN);
  g_assert (inoutArg[2]._d == 'c');
  g_assert (inoutArg[3]._d == 'e');
  g_assert (inoutArg[3]._u.v.a == constants_SHORT_INOUT_IN);

  inoutArg[0]._d = 'a';
  inoutArg[0]._u.x = constants_LONG_INOUT_OUT;
  inoutArg[1]._d = 'b';
  inoutArg[1]._u.y = constants_CHAR_INOUT_OUT;
  inoutArg[2]._d = 'c';
  inoutArg[3]._d = 'e';
  inoutArg[3]._u.v.a = constants_SHORT_INOUT_OUT;

  outArg[0]._d = 'a';
  outArg[0]._u.x = constants_LONG_OUT;
  outArg[1]._d = 'b';
  outArg[1]._u.y = constants_CHAR_OUT;
  outArg[2]._d = 'c';
  outArg[3]._d = 'e';
  outArg[3]._u.v.a = constants_SHORT_OUT;

  retn = test_FixedLengthUnionArray__alloc();

  retn[0]._d = 'a';
  retn[0]._u.x = constants_LONG_RETN;
  retn[1]._d = 'b';
  retn[1]._u.y = constants_CHAR_RETN;
  retn[2]._d = 'c';
  retn[3]._d = 'e';
  retn[3]._u.v.a = constants_SHORT_RETN;

  return retn;
}

PortableServer_ServantBase__epv UnionServer_base_epv = {NULL, simple_finalize, NULL};

POA_test_UnionServer__epv UnionServer_epv = {
	NULL,
	UnionServer_opFixed,
	UnionServer_opVariable,
	UnionServer_opMisc,
	UnionServer_opFixedLengthUnionArray
};

POA_test_UnionServer__vepv UnionServer_vepv = {&UnionServer_base_epv, &UnionServer_epv};

POA_test_UnionServer UnionServer_servant = {NULL, &UnionServer_vepv};  /* Singleton */
