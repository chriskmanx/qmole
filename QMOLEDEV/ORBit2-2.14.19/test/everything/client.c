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

#include <glib.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "everything.h"
#include "constants.h"
#include "orb-core/orb-core-private.h"
#include "orbit-imodule.h"
#include "orbit/orbit.h"

#define NUM_RUNS 1
#define NUM_THREADS 8

#undef TIMING_RUN

#ifdef TIMING_RUN
#  define d_print(a)
#else
#  define d_print(a) if (!thread_tests && a != NULL) g_print(a)
#endif

extern CORBA_ORB global_orb;
gboolean         in_proc;
gboolean         thread_safe = FALSE;
gboolean         thread_tests = FALSE;

/* Ugly- but hey */
#define _IN_CLIENT_
#include "server.c"
#undef  _IN_CLIENT_

static void
testConst (void)
{
	d_print ("Testing constants...\n");
	g_assert (test_CONST_CHAR == 't');
	g_assert (test_CONST_LONG == 0x12345678);
	g_assert (test_CONST_LONG_LONG == 0x12345678);
	g_assert (!strcmp (test_CONST_STRING, "ConstString"));
	g_assert (test_CONST_FLOAT == 1234.56);
	g_assert (test_CONST_DOUBLE == 1234.5678);
	g_assert (test_CONST_LONG_DOUBLE == 1234.567891);
	g_assert (test_FAVORITE_SOUP == test_veggie);
	g_assert (test_HORRIBLE_SOUP == test_oxtail);
}

static void
testSequenceHelpers (void)
{
	CORBA_long           l    = 0;
	test_BoundedLongSeq *lseq = NULL;

	d_print ("Testing ORBit_sequence helpers...\n");

	lseq = ORBit_sequence_alloc (TC_test_BoundedLongSeq, 2);
	g_assert (lseq != NULL);
	g_assert (lseq->_length ==  2);
	g_assert (lseq->_maximum >= 2);

	ORBit_sequence_index (lseq, 0) = 0;
	ORBit_sequence_index (lseq, 1) = 0;
	
	l = 1;
	ORBit_sequence_append (lseq, &l);
	l++;
	ORBit_sequence_append (lseq, &l);
	
	g_assert (lseq->_length == 4);
	g_assert (ORBit_sequence_index (lseq, 2) == 1);
	g_assert (ORBit_sequence_index (lseq, 3) == 2);

        ORBit_sequence_remove(lseq, 2);
	g_assert (lseq->_length == 3);
	g_assert (ORBit_sequence_index (lseq, 2) == 2);

	ORBit_sequence_set_size (lseq, 100);
	ORBit_sequence_set_size (lseq, 0);

	CORBA_free (lseq);

	/* test incremental reallocation of memory */
	{
		CORBA_long i = 0;
		CORBA_long j = 0;

		/* 16 times double capacity of buffer */
		CORBA_long MAX_APPEND = 1<<16; 
		/* generic sequence<octet> test */ 
		CORBA_sequence_CORBA_octet *oseq = NULL;
		
		oseq = ORBit_sequence_alloc (TC_CORBA_sequence_CORBA_octet, 0);
		
		g_assert (oseq != NULL);
		g_assert (oseq->_length == 0);
		g_assert (oseq->_maximum >= 0);

		for (i = 0; i < MAX_APPEND; ++i)
		{
			CORBA_octet oct = (CORBA_octet) (i % 109);
			/* _append does shallow copy */ 
			ORBit_sequence_append (oseq, &oct); /* realloc */ 
			g_assert (i+1==oseq->_length);
			
			/* infrequent validation sequence values have
			 * been re-located correctly */
			if (i % 367 == 0) 
			for (j = 0; j < oseq->_length; ++j /* prim */ )
			{
				CORBA_octet j_check = (CORBA_octet) (j % 109);
				CORBA_octet j_value = ORBit_sequence_index (oseq, j);
				g_assert (j_value==j_check);
			}
		}
		
		CORBA_free (oseq);
	}

	/* test concat operation: concat two sequences of different
	 * size. The concatinated sequence must contain values of both
	 * in correct order. */
	{
		CORBA_octet oct = 0;
		CORBA_long i = 0;
		CORBA_long j = 0;
		CORBA_long a = 0;
		CORBA_long b = 0;
		CORBA_sequence_CORBA_octet *aseq = NULL;
		CORBA_sequence_CORBA_octet *bseq = NULL;

		aseq = ORBit_sequence_alloc (TC_CORBA_sequence_CORBA_octet, 0);
		bseq = ORBit_sequence_alloc (TC_CORBA_sequence_CORBA_octet, 0);
		
		for (b = 0; b < 200; ++b) {
			ORBit_sequence_concat (aseq, bseq);
			
			a = 0;
			for (i = 0; i < b; ++i)
				for (j = 0; j < i; ++j, ++a)
					g_assert (ORBit_sequence_index (aseq, a) == j % 128);
			
			oct = b % 128;
			ORBit_sequence_append (bseq, &oct);
		}
		
		CORBA_free (aseq);
		CORBA_free (bseq);
	}
	
}

static void
testAttribute (test_BasicServer   objref,
	       CORBA_Environment *ev)
{  	
	CORBA_char *val;
	CORBA_long  lval;

	d_print ("Testing attributes...\n");

	val = test_BasicServer__get_foo (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);  
	g_assert (strcmp (val, constants_STRING_RETN)==0);	
	CORBA_free (val);

	test_BasicServer__set_foo (objref, constants_STRING_IN, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	lval = test_BasicServer__get_bah (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);  
	g_assert (lval == constants_LONG_RETN);
}

static void
testString (test_BasicServer   objref,
	    CORBA_Environment *ev)
{
	const CORBA_char *in;
	CORBA_char *inout, *out, *retn; 

	d_print ("Testing strings...\n");

	in = constants_STRING_IN;
	inout = CORBA_string_dup (constants_STRING_INOUT_IN);
	retn = test_BasicServer_opString (objref, in, &inout, &out, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
  
	g_assert (strcmp (out, constants_STRING_OUT)==0);
	g_assert (strcmp (retn, constants_STRING_RETN)==0);	
	g_assert (strcmp (in, constants_STRING_IN)==0);
	g_assert (strcmp (inout, constants_STRING_INOUT_OUT)==0);

	test_BasicServer_opOneWay (objref, constants_STRING_IN, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
  
	CORBA_free (inout);
	CORBA_free (out);
	CORBA_free (retn);
}

static void
testLong (test_BasicServer   objref,
	  CORBA_Environment *ev)
{
	CORBA_long inArg, inoutArg, outArg, retn;

	d_print ("Testing longs...\n");

	inArg = constants_LONG_IN;
	inoutArg = constants_LONG_INOUT_IN;
	retn = test_BasicServer_opLong (objref, inArg, &inoutArg, &outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (inArg == constants_LONG_IN);
	g_assert (inoutArg == constants_LONG_INOUT_OUT);
	g_assert (outArg == constants_LONG_OUT);
	g_assert (retn == constants_LONG_RETN);
}

static void
testLongLong (test_BasicServer   objref,
	      CORBA_Environment *ev)
{
	CORBA_long_long inArg, inoutArg, outArg, retn;

	d_print ("Testing long longs...\n");

	inArg = constants_LONG_LONG_IN;
	inoutArg = constants_LONG_LONG_INOUT_IN;
	retn = test_BasicServer_opLongLong (objref, inArg, &inoutArg, &outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (inArg == constants_LONG_LONG_IN);
	g_assert (inoutArg == constants_LONG_LONG_INOUT_OUT);
	g_assert (outArg == constants_LONG_LONG_OUT);
	g_assert (retn == constants_LONG_LONG_RETN);
}

static void
testFloat (test_BasicServer   objref,
	   CORBA_Environment *ev)
{
	CORBA_float inArg, inoutArg, outArg, retn;

	d_print ("Testing floats...\n");

	inArg = constants_FLOAT_IN;
	inoutArg = constants_FLOAT_INOUT_IN;
	retn = test_BasicServer_opFloat (objref, inArg, &inoutArg, &outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (inArg == constants_FLOAT_IN);
	g_assert (inoutArg == constants_FLOAT_INOUT_OUT);
	g_assert (outArg == constants_FLOAT_OUT);
	g_assert (retn == constants_FLOAT_RETN);
}

static void
testDouble (test_BasicServer   objref,
	    CORBA_Environment *ev)
{
	CORBA_double inArg, inoutArg, outArg, retn;

	d_print ("Testing doubles...\n");

	inArg = constants_DOUBLE_IN;
	inoutArg = constants_DOUBLE_INOUT_IN;
	retn = test_BasicServer_opDouble (objref, inArg, &inoutArg, &outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (inArg == constants_DOUBLE_IN);
	g_assert (inoutArg == constants_DOUBLE_INOUT_OUT);
	g_assert (outArg == constants_DOUBLE_OUT);
	g_assert (retn == constants_DOUBLE_RETN);
}

static void
testLongDouble (test_BasicServer   objref,
		CORBA_Environment *ev)
{
	CORBA_long_double inArg, inoutArg, outArg, retn;

	d_print ("Testing long doubles...\n");

	inArg = constants_LONG_DOUBLE_IN;
	inoutArg = constants_LONG_DOUBLE_INOUT_IN;
	retn = test_BasicServer_opLongDouble (objref, inArg, &inoutArg, &outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (inArg == constants_LONG_DOUBLE_IN);
	g_assert (inoutArg == constants_LONG_DOUBLE_INOUT_OUT);
	g_assert (outArg == constants_LONG_DOUBLE_OUT);
	g_assert (retn == constants_LONG_DOUBLE_RETN);
}

static void
testEnum (test_BasicServer   objref,
	  CORBA_Environment *ev)
{
	test_AnEnum inArg, inoutArg, outArg, retn;

	d_print ("Testing enums...\n");

	inArg = test_ENUM_IN;
	inoutArg = test_ENUM_INOUT_IN;
	retn = test_BasicServer_opEnum (objref, inArg, &inoutArg, &outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (inArg == test_ENUM_IN);
	g_assert (inoutArg == test_ENUM_INOUT_OUT);
	g_assert (outArg == test_ENUM_OUT);
	g_assert (retn == test_ENUM_RETN);
}

static void
testException (test_BasicServer   objref,
	       CORBA_Environment *ev)
{
	test_TestException *ex;
	CORBA_Environment  *cpyev;

	d_print ("Testing exceptions...\n");

	test_BasicServer_opException (CORBA_OBJECT_NIL, ev);
	g_assert (ev->_major == CORBA_SYSTEM_EXCEPTION);
	g_assert (strcmp (CORBA_exception_id (ev), ex_CORBA_INV_OBJREF) == 0);
	CORBA_exception_free (ev);

	test_BasicServer_opException (objref, ev);
  
	g_assert (ev->_major == CORBA_USER_EXCEPTION);
	g_assert (strcmp (CORBA_exception_id (ev), ex_test_TestException) == 0);
	ex = CORBA_exception_value (ev);
	g_assert (strcmp (ex->reason, constants_STRING_IN) == 0);
	g_assert (ex->number == constants_LONG_IN);
	g_assert (ex->aseq._length == 1);
	g_assert (ex->aseq._buffer[0] == constants_LONG_IN);

	cpyev = CORBA_exception__copy (ev);
	CORBA_exception_free (ev);
	ev = cpyev;

	g_assert (ev->_major == CORBA_USER_EXCEPTION);
	g_assert (strcmp (CORBA_exception_id (ev), ex_test_TestException) == 0);
/*	FIXME: we can't do this until we get exception data from
	the typelib - and make sure we register all system types
	there too */
/*	ex = CORBA_exception_value (ev);
	g_assert (strcmp (ex->reason, constants_STRING_IN) == 0);
	g_assert (ex->number == constants_LONG_IN);
	g_assert (ex->aseq._length == 1);
	g_assert (ex->aseq._buffer[0] == constants_LONG_IN);*/

	CORBA_free (cpyev);
}

static void
testBoolAlign (test_BasicServer   objref,
	       CORBA_Environment *ev)
{
	char *inoutArg;

	d_print ("Testing bool arg. alignment ...\n");

	inoutArg = CORBA_string_dup("foo");
	test_BasicServer_testBoolString (objref, TRUE, "retout", &inoutArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static CORBA_TypeCode
find_tc (CORBA_sequence_CORBA_TypeCode *tcs, 
	 const char                    *repo_id)
{
	int i;

	for (i = 0; i < tcs->_length; i++)
		if (!strcmp (tcs->_buffer [i]->repo_id, repo_id))
			return tcs->_buffer [i];

	return NULL;
}

static void
testIInterface (test_TestFactory   factory, 
		CORBA_Environment *ev)
{
	CORBA_TypeCode    tc;
	test_StructServer objref;
	CORBA_char       *type_id;
	ORBit_IInterface *iinterface;
	CORBA_sequence_CORBA_TypeCode *tcs;

	d_print ("Testing IInterface code...\n");
	objref = test_TestFactory_getStructServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	/* Check nil check is working ! */
	g_assert (CORBA_Object_is_nil  (CORBA_OBJECT_NIL, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (!CORBA_Object_is_nil (objref, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	/* Check non_existant is working ! */
	g_assert (!CORBA_Object_non_existent (objref, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	/* Ensure that we go over the wire at least once */
	g_assert (CORBA_Object_is_a (objref, "IDL:orbit/test/StructServer:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (CORBA_Object_is_a (objref, "IDL:orbit/test/BasicServer:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	/* Scripting stuff */

	/* Get real type id */
	g_assert ( (type_id = ORBit_small_get_type_id (objref, ev)));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (!strcmp (type_id, "IDL:orbit/test/StructServer:1.0"));
	CORBA_free (type_id);

	/* Get interface data */
	iinterface = ORBit_small_get_iinterface (
		objref, "foo_bar_jelly", ev);
	g_assert (ev->_major != CORBA_NO_EXCEPTION);
	g_assert (iinterface == NULL);
	g_assert (!strcmp (ev->_id, ex_ORBit_NoIInterface));
	CORBA_exception_free (ev);

	iinterface = ORBit_small_get_iinterface (
		objref, "IDL:orbit/test/StructServer:1.0", ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (iinterface != NULL);
	g_assert (!strcmp (iinterface->tc->repo_id, "IDL:orbit/test/StructServer:1.0"));
	CORBA_free (iinterface);

	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

#define TYPELIB_NAME "./Everything_module"

	if (!ORBit_small_load_typelib (TYPELIB_NAME))
		g_warning ("Failed to load '" TYPELIB_NAME "'");
	iinterface = ORBit_small_get_iinterface ( 
		objref, "IDL:orbit/test/StructServer:1.0", ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (iinterface != NULL);
	CORBA_free (iinterface);

	tcs = ORBit_small_get_types (TYPELIB_NAME);
	g_assert (find_tc (tcs, "IDL:orbit/test/Soup:1.0"));
	g_assert (find_tc (tcs, "IDL:orbit/test/EnumUnion/Colour:1.0"));
	g_assert (find_tc (tcs, "IDL:orbit/test/ArrayUnion:1.0"));
	
	tc = find_tc (tcs, "IDL:orbit/test/StrSeq:1.0");
	g_assert (!strcmp (tc->repo_id, "IDL:orbit/test/StrSeq:1.0"));
	g_assert (tc->kind == CORBA_tk_alias);
	g_assert (tc->subtypes[0]->kind);

	CORBA_free (tcs);

	/* test subnames for unions correctly handle multiple case
	 * labels pointing at the same sub type. */
	tc = TC_test_FixedLengthUnion;
	g_assert(tc->sub_parts == 5);
	g_assert(!strcmp(tc->subnames[0], "x"));
	g_assert(!strcmp(tc->subnames[1], "y"));
	g_assert(!strcmp(tc->subnames[2], "z"));
	g_assert(!strcmp(tc->subnames[3], "z"));
	g_assert(!strcmp(tc->subnames[4], "v"));
}

static void
testIsA (test_TestFactory   factory, 
	 CORBA_Environment *ev)
{
	test_DerivedServer ds;
 
	d_print ("Testing is_a ...\n");

	g_assert (CORBA_Object_is_a (factory, "IDL:CORBA/Object:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (CORBA_Object_is_a (factory, "IDL:omg.org/CORBA/Object:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	ds = test_TestFactory_getDerivedServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	g_assert (CORBA_Object_is_a (ds, "IDL:orbit/test/DerivedServer:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (CORBA_Object_is_a (ds, "IDL:orbit/test/C1:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (CORBA_Object_is_a (ds, "IDL:orbit/test/B1:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (CORBA_Object_is_a (ds, "IDL:orbit/test/B2:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (CORBA_Object_is_a (ds, "IDL:orbit/test/BaseServer:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (CORBA_Object_is_a (factory, "IDL:CORBA/Object:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release (ds, ev);
}

static void
testFixedLengthStruct (test_TestFactory   factory, 
		       CORBA_Environment *ev)
{
	test_StructServer objref;
	test_FixedLengthStruct inArg, inoutArg, outArg, retn;
	CORBA_char *ior;

	d_print ("Testing struct code ...\n");
	ior = test_TestFactory_getStructServerIOR (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
  
	objref = CORBA_ORB_string_to_object (global_orb, ior, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (objref != CORBA_OBJECT_NIL);
	CORBA_free (ior);


	inArg.a = constants_SHORT_IN;
	inoutArg.a = constants_SHORT_INOUT_IN;
  
	retn = test_StructServer_opFixed (objref, &inArg, &inoutArg, &outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
  
	g_assert (inArg.a == constants_SHORT_IN);
	g_assert (inoutArg.a == constants_SHORT_INOUT_OUT);
	g_assert (outArg.a == constants_SHORT_OUT);
	g_assert (retn.a == constants_SHORT_RETN);
	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testVariableLengthStruct (test_TestFactory   factory, 
			  CORBA_Environment *ev)
{
  test_StructServer objref;
  test_VariableLengthStruct inArg, inoutArg, *outArg, *retn;
  d_print ("Testing variable length structs...\n");
  objref = test_TestFactory_getStructServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  inArg.a = (CORBA_char*)constants_STRING_IN;  /* const cast */
  inoutArg.a = CORBA_string_dup (constants_STRING_INOUT_IN);
  
  retn = test_StructServer_opVariable (objref, &inArg, &inoutArg, &outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
  
  g_assert (strcmp (inArg.a, constants_STRING_IN)==0);
  g_assert (strcmp (inoutArg.a, constants_STRING_INOUT_OUT)==0);
  g_assert (strcmp (outArg->a, constants_STRING_OUT)==0);
  g_assert (strcmp (retn->a, constants_STRING_RETN)==0);	

  CORBA_free (inoutArg.a);
  CORBA_free (outArg);
  CORBA_free (retn);
  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
}


static void
testCompoundStruct (test_TestFactory   factory, 
		    CORBA_Environment *ev)
{
  test_StructServer objref;
  test_CompoundStruct inArg, inoutArg, *outArg, *retn;
  d_print ("Testing compound structs...\n");
  objref = test_TestFactory_getStructServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  inArg.a.a = CORBA_string_dup (constants_STRING_IN);
  inoutArg.a.a = CORBA_string_dup (constants_STRING_INOUT_IN);
  
  retn = test_StructServer_opCompound (objref, &inArg, &inoutArg, &outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
  
  g_assert (strcmp (inArg.a.a, constants_STRING_IN)==0);
  g_assert (strcmp (inoutArg.a.a, constants_STRING_INOUT_OUT)==0);
  g_assert (strcmp (outArg->a.a, constants_STRING_OUT)==0);
  g_assert (strcmp (retn->a.a, constants_STRING_RETN)==0);	
  
  CORBA_free (inArg.a.a);
  CORBA_free (inoutArg.a.a);
  CORBA_free (outArg);
  CORBA_free (retn);  
  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testAlignHoleStruct (test_TestFactory   factory, 
		    CORBA_Environment *ev)
{
  test_StructServer objref;
  test_AlignHoleStruct inArg, inoutArg, outArg, retn;
  d_print ("Testing structs with aligning holes...\n");
  objref = test_TestFactory_getStructServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  inArg.a.a = constants_DOUBLE_IN;
  inArg.a.b = constants_OCTET_IN;
  inArg.b = constants_CHAR_IN;

  inoutArg.a.a = constants_DOUBLE_INOUT_IN;
  inoutArg.a.b = constants_OCTET_INOUT_IN;
  inoutArg.b = constants_CHAR_INOUT_IN;
  
  memset(&outArg, 0, sizeof(outArg));

  retn = test_StructServer_opAlignHole (objref, &inArg, &inoutArg, &outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
  
  g_assert (inArg.a.a == constants_DOUBLE_IN);
  g_assert (inArg.a.b == constants_OCTET_IN);
  g_assert (inArg.b == constants_CHAR_IN);

  g_assert (inoutArg.a.a == constants_DOUBLE_INOUT_OUT);
  g_assert (inoutArg.a.b == constants_OCTET_INOUT_OUT);
  g_assert (inoutArg.b == constants_CHAR_INOUT_OUT);

  g_assert (outArg.a.a == constants_DOUBLE_OUT);
  g_assert (outArg.a.b == constants_OCTET_OUT);
  g_assert (outArg.b == constants_CHAR_OUT);

  g_assert (retn.a.a == constants_DOUBLE_RETN);	
  g_assert (retn.a.b == constants_OCTET_RETN);	
  g_assert (retn.b == constants_CHAR_RETN);	
  
  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testObjectStruct (test_TestFactory   factory, 
		  CORBA_Environment *ev)
{
  test_StructServer objref;
  test_ObjectStruct inArg;

  d_print ("Testing object structs...\n");
  objref = test_TestFactory_getStructServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  inArg.serv = objref;

  test_StructServer_opObjectStruct (objref, &inArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

}

static void
testStructAny (test_TestFactory   factory, 
	       CORBA_Environment *ev)
{
	test_StructServer objref;
	test_StructAny   *a;

	d_print ("Testing 'any' structs...\n");
	objref = test_TestFactory_getStructServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	a = test_StructServer_opStructAny (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	g_assert (!strcmp (a->a, constants_STRING_IN));
	g_assert (* (CORBA_long *)a->b._value == constants_LONG_IN);
  
	CORBA_free (a);  

	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testUnboundedSequence (test_TestFactory   factory, 
		       CORBA_Environment *ev)
{
  test_SequenceServer objref;
  test_StrSeq *outArg = NULL, inArg, inoutArg, *retn;
  /*  test_LongSeq *long_retn; */
  guint i;
  d_print ("Testing unbounded sequences...\n");
  objref = test_TestFactory_getSequenceServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
  
  inArg._buffer = CORBA_sequence_CORBA_string_allocbuf (2);
  inArg._length = 2;
  CORBA_sequence_set_release (&inArg, CORBA_TRUE);

  for (i=0;i<inArg._length;i++){
	inArg._buffer[i] = CORBA_string_dup (constants_SEQ_STRING_IN[i]);
  }

  inoutArg._buffer = CORBA_sequence_CORBA_string_allocbuf (2);
  inoutArg._length = 2;
  CORBA_sequence_set_release (&inoutArg, CORBA_TRUE);

  for (i=0;i<inoutArg._length;i++){
	inoutArg._buffer[i] = CORBA_string_dup (constants_SEQ_STRING_INOUT_IN[i]);
  }

  retn = test_SequenceServer_opStrSeq (objref, &inArg, &inoutArg, &outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
  
  for (i=0;i<inArg._length;i++)
	g_assert (strcmp (inArg._buffer[i], constants_SEQ_STRING_IN[i]) == 0);

  for (i=0;i<inoutArg._length;i++)
	g_assert (strcmp (inoutArg._buffer[i], constants_SEQ_STRING_INOUT_OUT[i]) == 0);

  for (i=0;i<outArg->_length;i++)
	g_assert (strcmp (outArg->_buffer[i], constants_SEQ_STRING_OUT[i]) == 0);

  for (i=0;i<retn->_length;i++)
	g_assert (strcmp (retn->_buffer[i], constants_SEQ_STRING_RETN[i]) == 0);

  g_warning ("FIXME: opMassiveSeq fails - due to max. size check");
  /*  long_retn = test_SequenceServer_opMassiveSeq(objref, ev);
      g_assert (ev->_major == CORBA_NO_EXCEPTION);
      CORBA_free (long_retn); */

  CORBA_free (inArg._buffer);
  CORBA_free (inoutArg._buffer);
  CORBA_free (outArg);
  CORBA_free (retn);

  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testAnySequence (test_TestFactory   factory, 
                 CORBA_Environment *ev)
{
    test_AnySeq *any_retn, *copy;
    test_SequenceServer objref;

    d_print ("Testing sequence<any>...\n");
    objref = test_TestFactory_getSequenceServer (factory, ev);
    g_assert (ev->_major == CORBA_NO_EXCEPTION);
    
    any_retn = test_SequenceServer_opAnySeq (objref, ev);
    g_assert (ev->_major == CORBA_NO_EXCEPTION);
    copy = ORBit_copy_value (any_retn, TC_test_AnySeq);
    CORBA_free (any_retn);

    CORBA_Object_release (objref, ev);
    g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testBoundedSequence (test_TestFactory   factory, 
		     CORBA_Environment *ev)
{
  test_SequenceServer objref;
  test_BoundedStructSeq inArg, inoutArg, *outArg, *retn;
  guint i;
  d_print ("Testing bounded sequences...\n");
  objref = test_TestFactory_getSequenceServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  
  inArg._buffer  = CORBA_sequence_test_CompoundStruct_allocbuf (2);
  inArg._length  = 2;
  inArg._maximum = 2;
  CORBA_sequence_set_release (&inoutArg, CORBA_TRUE);

  for (i=0;i<inArg._length;i++){
	inArg._buffer[i].a.a = CORBA_string_dup (constants_SEQ_STRING_IN[i]);
  }

  inoutArg._buffer  = CORBA_sequence_test_CompoundStruct_allocbuf (2);
  inoutArg._length  = 2;
  inoutArg._maximum = 2;
  CORBA_sequence_set_release (&inoutArg, CORBA_TRUE);

  for (i=0;i<inoutArg._length;i++){
	inoutArg._buffer[i].a.a = CORBA_string_dup (constants_SEQ_STRING_INOUT_IN[i]);
  }

  retn = test_SequenceServer_opBoundedStructSeq (objref, &inArg, &inoutArg, &outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
  

  for (i=0;i<inArg._length;i++){
	g_assert (strcmp (inArg._buffer[i].a.a, constants_SEQ_STRING_IN[i]) == 0);
  }

  for (i=0;i<inoutArg._length;i++){
	g_assert (strcmp (inoutArg._buffer[i].a.a, constants_SEQ_STRING_INOUT_OUT[i]) == 0);
  }

  for (i=0;i<outArg->_length;i++){
	g_assert (strcmp (outArg->_buffer[i].a.a, constants_SEQ_STRING_OUT[i]) == 0);
  }

  for (i=0;i<retn->_length;i++){
	g_assert (strcmp (retn->_buffer[i].a.a, constants_SEQ_STRING_RETN[i]) == 0);
  }

  CORBA_free (inArg._buffer);
  CORBA_free (inoutArg._buffer);
  CORBA_free (outArg);
  CORBA_free (retn);
  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testFixedLengthUnion (test_TestFactory   factory, 
		      CORBA_Environment *ev)
{
  test_UnionServer objref;
  test_FixedLengthUnion inArg, inoutArg, outArg, retn;
  d_print ("Testing fixed length unions...\n");
  objref = test_TestFactory_getUnionServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  inArg._u.x = constants_LONG_IN;
  inArg._d = 'a';

  inoutArg._u.y = 't';
  inoutArg._d = 'b';
  
  retn = test_UnionServer_opFixed (objref, &inArg, &inoutArg, &outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  g_assert (inArg._d == 'a');
  g_assert (inArg._u.x == constants_LONG_IN);
  g_assert (inoutArg._d == 'c');
  g_assert (inoutArg._u.z == TRUE);
  g_assert (outArg._d == 'a');
  g_assert (outArg._u.x == constants_LONG_OUT);
  g_assert (retn._d == 'd');
  g_assert (retn._u.z == FALSE);

  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testVariableLengthUnion (test_TestFactory   factory, 
			 CORBA_Environment *ev)
{
  test_UnionServer objref;
  test_VariableLengthUnion inArg, inoutArg, *outArg, *retn;
  d_print ("Testing variable length unions...\n");
  objref = test_TestFactory_getUnionServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  inArg._u.x = constants_LONG_IN;
  inArg._d = 1;

  inoutArg._u.y = CORBA_string_dup (constants_STRING_INOUT_IN);
  inoutArg._d = 2;
  
  retn = test_UnionServer_opVariable (objref, &inArg, &inoutArg, &outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  g_assert (inArg._d == 1);
  g_assert (inArg._u.x == constants_LONG_IN);
  g_assert (inoutArg._d == 3);
  g_assert (inoutArg._u.z == TRUE);
  g_assert (outArg->_d == 1);
  g_assert (outArg->_u.x == constants_LONG_OUT);
  g_assert (retn->_d == 4);
  g_assert (retn->_u.z == FALSE);

  CORBA_free (outArg);
  CORBA_free (retn);

  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testMiscUnions (test_TestFactory   factory, 
	        CORBA_Environment *ev)
{
	test_UnionServer   obj;
	test_EnumUnion     retn;
	test_unionSeq      inSeq;
	test_VariableLengthUnion inSeq_buffer[3];
	test_BooleanUnion  inArg;
	test_ArrayUnion   *outArg;
	int                i;

	d_print ("Testing misc type unions...\n");
	obj = test_TestFactory_getUnionServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	inSeq._length  = inSeq._maximum = 3;
	inSeq._buffer  = inSeq_buffer;
	inSeq._release = CORBA_FALSE;
	inSeq._buffer [0]._d   = 4;
	inSeq._buffer [0]._u.z = CORBA_TRUE;
	inSeq._buffer [1]._d   = 2;
	inSeq._buffer [1]._u.y = "blah";
	inSeq._buffer [2]._d   = 55;
	inSeq._buffer [2]._u.w = constants_LONG_IN;

	inArg._d   = 1;
	inArg._u.y = "blah de blah";

	retn = test_UnionServer_opMisc (obj, &inSeq, &inArg, &outArg, ev);

	g_assert (inSeq._length == 3);
	g_assert (inSeq._buffer [0]._d == 4);
	g_assert (inSeq._buffer [0]._u.z == CORBA_TRUE);
	g_assert (inSeq._buffer [1]._d == 2);
	g_assert (!strcmp (inSeq._buffer [1]._u.y, "blah"));
	g_assert (inSeq._buffer [2]._d == 55);
	g_assert (inSeq._buffer [2]._u.w == constants_LONG_IN);
	g_assert (inArg._d == 1);
	g_assert (!strcmp (inArg._u.y, "blah de blah"));

	g_assert (outArg->_d == 22);
	for (i = 0; i < 20; i++) {
		char *tmp;

		tmp = g_strdup_printf ("Numero %d", i);
		g_assert (!strcmp (outArg->_u.d [i], tmp));
		g_free (tmp);
	}

	g_assert (retn._d == test_EnumUnion_red);
	g_assert (retn._u.x == constants_LONG_IN);

	CORBA_free (outArg);

	CORBA_Object_release (obj, ev);
  	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testUnionArray (test_TestFactory   factory, 
	        CORBA_Environment *ev)
{
	test_UnionServer                  obj;
	test_FixedLengthUnionArray_slice *retn;
	test_FixedLengthUnionArray        inArg;
	test_FixedLengthUnionArray        inoutArg;
	test_FixedLengthUnionArray        outArg;

	d_print ("Testing union array...\n");
	obj = test_TestFactory_getUnionServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	inArg[0]._d = 'a';
	inArg[0]._u.x = constants_LONG_IN;
	inArg[1]._d = 'b';
	inArg[1]._u.y = constants_CHAR_IN;
	inArg[2]._d = 'c';
	inArg[3]._d = 'e';
	inArg[3]._u.v.a = constants_SHORT_IN;

	inoutArg[0]._d = 'a';
	inoutArg[0]._u.x = constants_LONG_INOUT_IN;
	inoutArg[1]._d = 'b';
	inoutArg[1]._u.y = constants_CHAR_INOUT_IN;
	inoutArg[2]._d = 'c';
	inoutArg[3]._d = 'e';
	inoutArg[3]._u.v.a = constants_SHORT_INOUT_IN;
	
	retn = test_UnionServer_opFixedLengthUnionArray (obj, inArg, inoutArg, outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	g_assert (inArg[0]._d == 'a');
	g_assert (inArg[0]._u.x == constants_LONG_IN);
	g_assert (inArg[1]._d == 'b');
	g_assert (inArg[1]._u.y == constants_CHAR_IN);
	g_assert (inArg[2]._d == 'c');
	g_assert (inArg[3]._d == 'e');
	g_assert (inArg[3]._u.v.a == constants_SHORT_IN);

	g_assert (inoutArg[0]._d == 'a');
	g_assert (inoutArg[0]._u.x == constants_LONG_INOUT_OUT);
	g_assert (inoutArg[1]._d == 'b');
	g_assert (inoutArg[1]._u.y == constants_CHAR_INOUT_OUT);
	g_assert (inoutArg[2]._d == 'c');
	g_assert (inoutArg[3]._d == 'e');
	g_assert (inoutArg[3]._u.v.a == constants_SHORT_INOUT_OUT);
	
	g_assert (outArg[0]._d == 'a');
	g_assert (outArg[0]._u.x == constants_LONG_OUT);
	g_assert (outArg[1]._d == 'b');
	g_assert (outArg[1]._u.y == constants_CHAR_OUT);
	g_assert (outArg[2]._d == 'c');
	g_assert (outArg[3]._d == 'e');
	g_assert (outArg[3]._u.v.a == constants_SHORT_OUT);
	
	g_assert (retn[0]._d == 'a');
	g_assert (retn[0]._u.x == constants_LONG_RETN);
	g_assert (retn[1]._d == 'b');
	g_assert (retn[1]._u.y == constants_CHAR_RETN);
	g_assert (retn[2]._d == 'c');
	g_assert (retn[3]._d == 'e');
	g_assert (retn[3]._u.v.a == constants_SHORT_RETN);
	
	CORBA_free (retn);
	
	CORBA_Object_release (obj, ev);
  	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testLongArray (test_ArrayServer   objref,
	       CORBA_Environment *ev)
{
  int i;
  test_LongArray inArg, inoutArg, outArg;
  test_LongArray_slice *retn;

  for (i=0;i<test_SequenceLen;i++)
	inArg[i] = constants_SEQ_LONG_IN[i];

  for (i=0;i<test_SequenceLen;i++)
	inoutArg[i] = constants_SEQ_LONG_INOUT_IN[i];

  retn = test_ArrayServer_opLongArray (objref, inArg, inoutArg, outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  for (i=0;i<test_SequenceLen;i++)
	g_assert (inArg[i]==constants_SEQ_LONG_IN[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (inoutArg[i]==constants_SEQ_LONG_INOUT_OUT[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (outArg[i]==constants_SEQ_LONG_OUT[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (retn[i]==constants_SEQ_LONG_RETN[i]);

  CORBA_free (retn);
}

static void
testOctetArray (test_ArrayServer   objref,
		CORBA_Environment *ev)
{
  int i;
  test_OctetArray inArg, inoutArg, outArg;
  test_OctetArray_slice *retn;

  for (i=0;i<test_SequenceLen;i++)
	inArg[i] = constants_SEQ_OCTET_IN[i];

  for (i=0;i<test_SequenceLen;i++)
	inoutArg[i] = constants_SEQ_OCTET_INOUT_IN[i];

  retn = test_ArrayServer_opOctetArray (objref, inArg, inoutArg, outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  for (i=0;i<test_SequenceLen;i++)
	g_assert (inArg[i]==constants_SEQ_OCTET_IN[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (inoutArg[i]==constants_SEQ_OCTET_INOUT_OUT[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (outArg[i]==constants_SEQ_OCTET_OUT[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (retn[i]==constants_SEQ_OCTET_RETN[i]);

  CORBA_free (retn);
}

static void
testFixedLengthStructArray (test_ArrayServer   objref,
			    CORBA_Environment *ev)
{
  int i;
  test_FixedLengthStructArray inArg, inoutArg, outArg;
  test_FixedLengthStructArray_slice *retn;

  for (i=0;i<test_SequenceLen;i++)
	inArg[i].a = constants_SEQ_OCTET_IN[i];

  for (i=0;i<test_SequenceLen;i++)
	inoutArg[i].a = constants_SEQ_OCTET_INOUT_IN[i];

  retn = test_ArrayServer_opFixedLengthStructArray (objref, inArg, inoutArg, outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  for (i=0;i<test_SequenceLen;i++)
	g_assert (inArg[i].a==constants_SEQ_OCTET_IN[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (inoutArg[i].a==constants_SEQ_OCTET_INOUT_OUT[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (outArg[i].a==constants_SEQ_OCTET_OUT[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (retn[i].a==constants_SEQ_OCTET_RETN[i]);

  CORBA_free (retn);
}

static void
testFixedLengthArray (test_TestFactory   factory, 
		      CORBA_Environment *ev)
{
  
  test_ArrayServer objref;
  d_print ("Testing arrays with fixed length members...\n");
  objref = test_TestFactory_getArrayServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  testLongArray (objref, ev);
  testOctetArray (objref, ev);
  testFixedLengthStructArray (objref, ev);

  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testStrArray (test_ArrayServer   objref,
	      CORBA_Environment *ev)
{
  test_StrArray inArg, inoutArg;
  test_StrArray_slice *outArg, *retn;
  test_StrArrayMultiDimensional_slice *multidim;
  int i, n0, n1, n2;
  
  for (i=0;i<test_SequenceLen;i++)
	inArg[i] = CORBA_string_dup (constants_SEQ_STRING_IN[i]);

  for (i=0;i<test_SequenceLen;i++)
	inoutArg[i] = CORBA_string_dup (constants_SEQ_STRING_INOUT_IN[i]);

  retn = test_ArrayServer_opStrArray (objref, inArg, inoutArg, &outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  for (i=0;i<test_SequenceLen;i++)
	CORBA_free (inArg[i]); 

  for (i=0;i<test_SequenceLen;i++)
        CORBA_free (inoutArg[i]);

  CORBA_free (outArg);
  CORBA_free (retn);
  
  multidim = test_StrArrayMultiDimensional__alloc ();
  for (n0 = 0; n0 < 2; n0++) {
	for (n1 = 0; n1 < 3; n1++) {
	  for (n2 = 0; n2 < 5; n2++) {
		multidim[n0][n1][n2] = CORBA_string_dup (constants_SEQ_STRING_INOUT_IN[0]);
	  }
	}
  }
  CORBA_free (multidim);

}

static void
testAlignHoleStructArray (test_ArrayServer   objref,
			  CORBA_Environment *ev)
{
  int i;
  test_AlignHoleStructArray inArg, inoutArg, outArg;
  test_AlignHoleStructArray_slice *retn;

  for (i=0;i<test_SequenceLen;i++)
	inArg[i].a.a = constants_SEQ_OCTET_IN[i];
  for (i=0;i<test_SequenceLen;i++)
	inArg[i].a.b = constants_SEQ_OCTET_IN[i];
  for (i=0;i<test_SequenceLen;i++)
	inArg[i].b = constants_SEQ_OCTET_IN[i];

  for (i=0;i<test_SequenceLen;i++)
	inoutArg[i].a.a = constants_SEQ_OCTET_INOUT_IN[i];
  for (i=0;i<test_SequenceLen;i++)
	inoutArg[i].a.b = constants_SEQ_OCTET_INOUT_IN[i];
  for (i=0;i<test_SequenceLen;i++)
	inoutArg[i].b = constants_SEQ_OCTET_INOUT_IN[i];

  retn = test_ArrayServer_opAlignHoleStructArray (objref, inArg, inoutArg, outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  for (i=0;i<test_SequenceLen;i++)
	g_assert (inArg[i].a.a==constants_SEQ_OCTET_IN[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (inArg[i].a.b==constants_SEQ_OCTET_IN[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (inArg[i].b==(CORBA_char)constants_SEQ_OCTET_IN[i]);

  for (i=0;i<test_SequenceLen;i++)
	g_assert (inoutArg[i].a.a==constants_SEQ_OCTET_INOUT_OUT[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (inoutArg[i].a.b==constants_SEQ_OCTET_INOUT_OUT[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (inoutArg[i].b==(CORBA_char)constants_SEQ_OCTET_INOUT_OUT[i]);

  for (i=0;i<test_SequenceLen;i++)
	g_assert (outArg[i].a.a==constants_SEQ_OCTET_OUT[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (outArg[i].a.b==constants_SEQ_OCTET_OUT[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (outArg[i].b==(CORBA_char)constants_SEQ_OCTET_OUT[i]);

  for (i=0;i<test_SequenceLen;i++)
	g_assert (retn[i].a.a==constants_SEQ_OCTET_RETN[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (retn[i].a.b==constants_SEQ_OCTET_RETN[i]);
  for (i=0;i<test_SequenceLen;i++)
	g_assert (retn[i].b==(CORBA_char)constants_SEQ_OCTET_RETN[i]);

  CORBA_free (retn);
}

static void
testVariableLengthArray (test_TestFactory   factory, 
			 CORBA_Environment *ev)
{
  test_ArrayServer objref;

  d_print ("Testing arrays with variable length members...\n");
  objref = test_TestFactory_getArrayServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  testStrArray (objref, ev);
  testAlignHoleStructArray (objref, ev);

  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testAnyLong (test_TestFactory   factory, 
	     CORBA_Environment *ev)
{
  test_AnyServer objref;
  CORBA_any inArg, inoutArg, *outArg, *retn;
  CORBA_long tmp, tmp1;

  d_print ("Testing any with longs...\n");
  objref = test_TestFactory_getAnyServer (factory, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  tmp = constants_LONG_IN;
  inArg._type = (CORBA_TypeCode)TC_CORBA_long;
  inArg._value = &tmp;
  CORBA_any_set_release (&inArg, CORBA_FALSE);

  inoutArg._type = (CORBA_TypeCode)TC_CORBA_long;
  tmp1 = constants_LONG_INOUT_IN;
  inoutArg._value = &tmp1;
  CORBA_any_set_release (&inoutArg, CORBA_FALSE);
    
  retn = test_AnyServer_opAnyLong (objref, &inArg, &inoutArg, &outArg, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);

  g_assert ( CORBA_TypeCode_equal (inArg._type, TC_CORBA_long, ev) );
  g_assert (* (CORBA_long*)inArg._value == constants_LONG_IN);

  g_assert ( CORBA_TypeCode_equal (inoutArg._type, TC_CORBA_long, ev) );
  g_assert (* (CORBA_long*)inoutArg._value == constants_LONG_INOUT_OUT);

  g_assert ( CORBA_TypeCode_equal (outArg->_type, TC_CORBA_long, ev) );
  g_assert (* (CORBA_long*)outArg->_value == constants_LONG_OUT);

  g_assert ( CORBA_TypeCode_equal (retn->_type, TC_CORBA_long, ev) );
  g_assert (* (CORBA_long*)retn->_value == constants_LONG_RETN);

  if (CORBA_any_get_release (&inArg)){
	CORBA_free (inArg._value);
	CORBA_Object_release ((CORBA_Object)inArg._type, ev);
  }

  if (CORBA_any_get_release (&inoutArg)){
	CORBA_free (inoutArg._value);
	CORBA_Object_release ((CORBA_Object)inoutArg._type, ev);
  }

  CORBA_free (outArg);
  CORBA_free (retn);

  CORBA_Object_release (objref, ev);
  g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testAnyString (test_TestFactory   factory, 
	       CORBA_Environment *ev)
{
	test_AnyServer objref;
	CORBA_any inArg, inoutArg, *outArg, *retn;

	d_print ("Testing any with strings...\n");
	objref = test_TestFactory_getAnyServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	inArg._type = (CORBA_TypeCode)TC_CORBA_string;
	inArg._value = &constants_STRING_IN;
	CORBA_any_set_release (&inArg, CORBA_FALSE);

	inoutArg._type = (CORBA_TypeCode)TC_CORBA_string;
	inoutArg._value = &constants_STRING_INOUT_IN;
	CORBA_any_set_release (&inoutArg, CORBA_FALSE);
  
	retn = test_AnyServer_opAnyString (objref, &inArg, &inoutArg, &outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	g_assert (CORBA_TypeCode_equal (inArg._type, TC_CORBA_string, ev));
	g_assert (strcmp (* (CORBA_char **)inArg._value, constants_STRING_IN) == 0);

	g_assert (CORBA_TypeCode_equal (inoutArg._type, TC_CORBA_string, ev) );
	g_assert (strcmp (* (CORBA_char **)inoutArg._value, constants_STRING_INOUT_OUT) == 0);

	g_assert (CORBA_TypeCode_equal (outArg->_type, TC_CORBA_string, ev) );
	g_assert (strcmp (* (CORBA_char **)outArg->_value, constants_STRING_OUT) == 0);

	g_assert (CORBA_TypeCode_equal (retn->_type, TC_CORBA_string, ev) );
	g_assert (strcmp (* (CORBA_char **)retn->_value, constants_STRING_RETN) == 0);

	if (CORBA_any_get_release (&inArg)){
		CORBA_free (inArg._value);
		CORBA_Object_release ((CORBA_Object)inArg._type, ev);
	}

	if (CORBA_any_get_release (&inoutArg)){
		CORBA_free (inoutArg._value);
		CORBA_Object_release ((CORBA_Object)inoutArg._type, ev);
	}

	CORBA_free (outArg);
	CORBA_free (retn);

	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testAnyStrSeq (test_TestFactory   factory, 
	       CORBA_Environment *ev)
{
	test_AnyServer objref;
	CORBA_any     *retn;

	d_print ("Testing any with string sequences ...\n");

	objref = test_TestFactory_getAnyServer(factory,ev);
	g_assert(ev->_major == CORBA_NO_EXCEPTION);

	retn = test_AnyServer_opAnyStrSeq (objref, ev);
	g_assert(ev->_major == CORBA_NO_EXCEPTION);

	CORBA_free (retn);

	CORBA_Object_release (objref, ev);
}

static void
testAnyStruct (test_TestFactory   factory, 
	       CORBA_Environment *ev)
{
	test_AnyServer objref;
	CORBA_any inArg, inoutArg, *outArg, *retn;
	test_VariableLengthStruct inArgStruct; 
	test_VariableLengthStruct * inoutArgStruct;

	d_print ("Testing any with structs...\n");

	objref = test_TestFactory_getAnyServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	inoutArgStruct = test_VariableLengthStruct__alloc ();
	inArgStruct.a= (CORBA_char*)constants_STRING_IN; /* const cast */
	inArg._type = (CORBA_TypeCode)TC_test_VariableLengthStruct;
	inArg._value = &inArgStruct;
	CORBA_any_set_release (&inArg, CORBA_FALSE);


	inoutArgStruct->a = CORBA_string_dup (constants_STRING_INOUT_IN);
	inoutArg._type = (CORBA_TypeCode)TC_test_VariableLengthStruct;
	inoutArg._value = inoutArgStruct;
	CORBA_any_set_release (&inoutArg, CORBA_TRUE);

	retn = test_AnyServer_opAnyStruct (objref, &inArg, &inoutArg, &outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

  
	g_assert (CORBA_TypeCode_equal (inArg._type, TC_test_VariableLengthStruct, ev));
	g_assert (strcmp ((* (test_VariableLengthStruct*)inArg._value).a, constants_STRING_IN) == 0);

	g_assert (CORBA_TypeCode_equal (inoutArg._type, TC_test_VariableLengthStruct, ev) );
	g_assert (strcmp ((* (test_VariableLengthStruct*)inoutArg._value).a, constants_STRING_INOUT_OUT) == 0);

	g_assert (CORBA_TypeCode_equal (outArg->_type, TC_test_VariableLengthStruct, ev) );
	g_assert (strcmp ((* (test_VariableLengthStruct*)outArg->_value).a, constants_STRING_OUT) == 0);

	g_assert (CORBA_TypeCode_equal (retn->_type, TC_test_VariableLengthStruct, ev) );
	g_assert (strcmp ((* (test_VariableLengthStruct*)retn->_value).a, constants_STRING_RETN) == 0);


	if (CORBA_any_get_release (&inArg)){
		/* This shouldn't be called */
		CORBA_free (inArg._value);
		CORBA_Object_release ((CORBA_Object)inArg._type, ev);
	}

	if (CORBA_any_get_release (&inoutArg)){
		CORBA_free (inoutArg._value);
		CORBA_Object_release ((CORBA_Object)inoutArg._type, ev);
	}

	CORBA_free (outArg);
	CORBA_free (retn);

	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

}

static void
testSequenceOfAny (test_TestFactory   factory, 
		   CORBA_Environment *ev)
{
	/* this test just checks the memory management for seq of any */

	test_AnySeq anyseq;
	int i;

	d_print ("Testing Sequence of Any...\n");

	anyseq._buffer  = CORBA_sequence_CORBA_any_allocbuf (2);
	anyseq._length  = 2;
	CORBA_sequence_set_release (&anyseq, CORBA_TRUE);
  
	for (i = 0; i < anyseq._length; i++) {
		anyseq._buffer [i]._type = (CORBA_TypeCode) TC_CORBA_string;
		anyseq._buffer [i]._value = &constants_STRING_IN;
		CORBA_any_set_release (
			&anyseq._buffer[i], CORBA_FALSE);
	}
  
	CORBA_free (anyseq._buffer);
}

static void
testAnyException (test_TestFactory   factory, 
		  CORBA_Environment *ev)
{
	CORBA_any *inArg; 
	test_TestException *testex;

	d_print ("Testing Any with exception...\n");

	inArg = CORBA_any__alloc ();
	testex = test_TestException__alloc ();  
	inArg->_type = (CORBA_TypeCode) TC_test_TestException;
	inArg->_value = testex;
	CORBA_any_set_release (inArg, CORBA_TRUE);

	CORBA_free (inArg);
}

static void
testAnyEquivalence (test_TestFactory   factory, 
		    CORBA_Environment *ev)
{
	test_unionSeq *aseq, *bseq;
	CORBA_any     *a, *b;

	d_print ("Testing Anys equivalence...\n");

	a = CORBA_any__alloc ();
	b = CORBA_any__alloc ();

	a->_type = b->_type = (CORBA_TypeCode) TC_test_unionSeq;

	aseq = test_unionSeq__alloc ();
	bseq = test_unionSeq__alloc ();
	aseq->_length  = aseq->_maximum = 3;
	bseq->_length  = bseq->_maximum = 3;
	aseq->_buffer  = test_unionSeq_allocbuf (3);
	bseq->_buffer  = test_unionSeq_allocbuf (3);
	bseq->_release = aseq->_release = CORBA_TRUE;
	bseq->_buffer [0]._d   = aseq->_buffer [0]._d   = 4;
	bseq->_buffer [0]._u.z = aseq->_buffer [0]._u.z = CORBA_TRUE;
	bseq->_buffer [1]._d   = aseq->_buffer [1]._d   = 2;
	aseq->_buffer [1]._u.y = CORBA_string_dup ("blah");
	bseq->_buffer [1]._u.y = CORBA_string_dup ("blah");

	bseq->_buffer [2]._d   = aseq->_buffer [2]._d   = 55;
	bseq->_buffer [2]._u.w = aseq->_buffer [2]._u.w = constants_LONG_IN;

	a->_value = aseq;
	b->_value = bseq;

	CORBA_any_set_release (a, CORBA_TRUE);
	CORBA_any_set_release (b, CORBA_TRUE);

	g_assert (ORBit_any_equivalent (a, b, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	bseq->_buffer [0]._u.z = CORBA_FALSE;

	g_assert (!ORBit_any_equivalent (a, b, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_free (b);
	b = NULL;
	bseq = NULL;

	b = ORBit_copy_value (a, TC_CORBA_any);

	g_assert (b != NULL);
	g_assert (ORBit_any_equivalent (a, b, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	bseq = (test_unionSeq *) b->_value;
	bseq->_buffer [0]._u.z = CORBA_FALSE;

	g_assert (!ORBit_any_equivalent (a, b, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_free (a);
	CORBA_free (b);
}

static void
testTypeCode (test_TestFactory   factory, 
	      CORBA_Environment *ev)
{
	test_AnyServer objref;
	CORBA_TypeCode inArg, inoutArg, outArg, retn;

	d_print ("Testing TypeCodes...\n");
	objref = test_TestFactory_getAnyServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	inArg = TC_test_ArrayUnion;
	inoutArg = TC_test_AnyServer;
  
	retn = test_AnyServer_opTypeCode (objref, inArg, &inoutArg, &outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	g_assert (CORBA_TypeCode_equal (inArg, TC_test_ArrayUnion, ev));
	g_assert (CORBA_TypeCode_equal (inoutArg, TC_test_TestException, ev));
	g_assert (CORBA_TypeCode_equal (outArg, TC_test_AnEnum, ev));  
	g_assert (CORBA_TypeCode_equal (retn, TC_test_VariableLengthStruct, ev));

	CORBA_Object_release ((CORBA_Object)inArg, ev);
	CORBA_Object_release ((CORBA_Object)inoutArg, ev);
	CORBA_Object_release ((CORBA_Object)outArg, ev);
	CORBA_Object_release ((CORBA_Object)retn, ev);

	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testContext (test_TestFactory   factory, 
	     CORBA_Environment *ev)
{
	test_ContextServer objref;
	CORBA_Object inArg, inoutArg, outArg, retn;
	CORBA_Context ctx;

	d_print ("Testing Contexts...\n");

	objref = test_TestFactory_getContextServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	inArg = inoutArg = outArg = retn = CORBA_OBJECT_NIL;

	CORBA_Context_create_child (CORBA_OBJECT_NIL, "Whatever", &ctx, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Context_set_one_value (ctx, "foo", "foo1", ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Context_set_one_value (ctx, "foo", "foo2", ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Context_set_one_value (ctx, "bar", "baaaa", ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	retn = test_ContextServer_opWithContext (
		objref, inArg, &inoutArg, &outArg, ctx, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release (retn, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Object_release (inArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Object_release (inoutArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Object_release (outArg, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release ( (CORBA_Object)ctx, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

#define SEQ_SIZE 4096
static test_StrSeq *
make_large_str_seq (void)
{
	test_StrSeq *seq = test_StrSeq__alloc ();
	int i;
	static const char base_data[] = "This is a longish test string it could go on for ever and ever";

	seq->_buffer = test_StrSeq_allocbuf (SEQ_SIZE);

	for (i = 0; i < SEQ_SIZE; i++) {
		int len = 3 + (int) ((sizeof (base_data) - 3.0) * rand () / (RAND_MAX + 1.0));
		seq->_buffer [i] = CORBA_string_dup (base_data);
		seq->_buffer [i] [len] = '\0';
	}

	seq->_length = seq->_maximum = SEQ_SIZE;
	seq->_release = CORBA_TRUE;

	return seq;
}

static void
testMisc (test_TestFactory   factory, 
	  CORBA_Environment *ev)
{
	CORBA_char       *foo;
	CORBA_Context     ctx;
	test_BasicServer  objref;

	d_print ("Testing Misc bits...\n");

	if (!in_proc) {
		/* Invoke a BasicServer method on a TestFactory */
		foo = test_BasicServer__get_foo (factory, ev);
		g_assert (ev->_major == CORBA_SYSTEM_EXCEPTION);
		g_assert (!strcmp (ev->_id, "IDL:omg.org/CORBA/BAD_OPERATION:1.0"));
		CORBA_exception_free (ev);
	}

	g_assert (ORBit_copy_value (NULL, TC_CORBA_boolean) == NULL);

	objref = test_TestFactory_getBasicServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (objref != CORBA_OBJECT_NIL);
	g_assert (CORBA_Object_is_a (objref, "IDL:orbit/test/BasicServer:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	{ /* IOR stringification */
		CORBA_char *ior;
		CORBA_Object o2;

		ior = CORBA_ORB_object_to_string (global_orb, factory, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);

		o2 = CORBA_ORB_string_to_object (global_orb, ior, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);
		
		CORBA_free (ior);

		g_assert (o2 == factory);
		CORBA_Object_release (o2, ev);
	}
	
	test_BasicServer_noImplement (objref, ev);
	g_assert (ev->_major == CORBA_SYSTEM_EXCEPTION);
	g_assert (!strcmp (ev->_id, "IDL:omg.org/CORBA/NO_IMPLEMENT:1.0"));
	CORBA_exception_free (ev);

	if (!in_proc) {
		test_StrSeq *seq;
		
		seq = make_large_str_seq ();
		test_BasicServer_testLargeStringSeq (objref, seq, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);
		CORBA_free (seq);
	}

	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
		
	/* Check we are building full type data */
	if (strcmp (TC_test_ObjectStruct->subtypes [0]->repo_id, 
		    "IDL:orbit/test/DerivedServer:1.0"))
		g_warning ("Martin's bug needs fixing");
	
	/* Check a daft one seen in CORBA_exception_free */
	CORBA_exception_set (ev, CORBA_USER_EXCEPTION, 
			     ex_test_SimpleException, NULL);
	CORBA_exception_free (ev);
	g_assert (ev->_id == NULL);
	g_assert (ev->_any._value == NULL);

	/* TypeCode equal */
	g_assert (CORBA_TypeCode_equal (
		TC_CORBA_string, TC_CORBA_string, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (!CORBA_TypeCode_equal (
		TC_test_StrSeq, TC_test_AnotherStrSeq, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	/* TypeCode equivalent */
	g_assert (CORBA_TypeCode_equivalent (
		TC_CORBA_string, TC_CORBA_string, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (CORBA_TypeCode_equivalent (
		TC_test_StrSeq, TC_test_AnotherStrSeq, ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	/*
	 * dead reference check
	 */
	if (!in_proc) {
		test_DeadReferenceObj obj;

		obj = test_TestFactory_createDeadReferenceObj (factory, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);
		g_assert (obj != CORBA_OBJECT_NIL);

		test_DeadReferenceObj_test (obj, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);

		CORBA_Object_release (obj, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);
	} else { /* Lets do some things on a de-activated ref */
		test_BasicServer         obj;
		PortableServer_ObjectId *oid;
		POA_test_BasicServer    *servant;

		servant = SIMPLE_SERVANT_NEW (BasicServer);
		obj = create_object (global_poa, servant, ev);

		oid = PortableServer_POA_servant_to_id (
			global_poa, servant, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);
		PortableServer_POA_deactivate_object (
			global_poa, oid, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);
	
		CORBA_free (oid);

		test_BasicServer_opException (obj, ev);
		g_assert (ev->_major == CORBA_SYSTEM_EXCEPTION);
		g_assert (!strcmp (ev->_id, ex_CORBA_OBJECT_NOT_EXIST));
		CORBA_exception_free (ev);

		test_BasicServer_opOneWay (obj, "Foo", ev);
		g_assert (ev->_major == CORBA_SYSTEM_EXCEPTION);
		g_assert (!strcmp (ev->_id, ex_CORBA_OBJECT_NOT_EXIST));
		CORBA_exception_free (ev);

		CORBA_Object_release (obj, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);
	}

	/* Check the ORB cleans up contexts properly */
	CORBA_ORB_get_default_context (
		global_orb, &ctx, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (ctx != CORBA_OBJECT_NIL);
	CORBA_Object_release ((CORBA_Object) ctx, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	/* Check that various bonobo hooks work */
	g_assert (ORBit_small_get_servant (NULL) == NULL);
	if (in_proc) {
		g_assert (ORBit_small_get_servant (factory));
		g_assert (ORBit_small_get_connection_status (factory) ==
			  ORBIT_CONNECTION_IN_PROC);
	} else {
		g_assert (!ORBit_small_get_servant (factory));
		g_assert (ORBit_small_get_connection_status (factory) ==
			  ORBIT_CONNECTION_CONNECTED);
	}
}

static void
testIOR (test_TestFactory   factory, 
	 CORBA_Environment *ev)
{
	int i, count;
	CORBA_Object objref;

	d_print ("Testing IOR marshalling ...\n");

	objref = test_TestFactory_getBasicServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	/* Check to see that the ORB correctly marshals various obj references */
	count = test_BasicServer_getObjectCount (objref, ev);
	for (i = 0; i < count; i++)
	{
		CORBA_Object test_obj;
		test_obj = test_BasicServer_getObject (objref, i, ev);
		if (ev->_major != CORBA_NO_EXCEPTION)
			g_error ("Error demarshalling object number %d: %s",
				 i, CORBA_exception_id (ev));
	}

	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testLifeCycle (test_TestFactory   factory, 
	       CORBA_Environment *ev)
{
	test_LifeCycleServer objref;
	ORBit_IMethod       *method;

	d_print ("Testing LifeCycle bits...\n");

	objref = test_TestFactory_createLifeCycleServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
		
	d_print (" pre gnome 2.4 stubs ...\n");

	if (in_proc) {
		method = &test_LifeCycleServer__iinterface.methods._buffer[1];
		g_assert (!strcmp (method->name, "deactivateUnrefOnReturn"));
	} else {
		method = &test_LifeCycleServer__iinterface.methods._buffer[0];
		g_assert (!strcmp (method->name, "deactivateOnReturn"));
	}
	
	ORBit_small_invoke_stub (objref, method, NULL, NULL, NULL, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	if (!in_proc)
		CORBA_Object_release (objref, ev);

	d_print (" post gnome 2.4 stubs ...\n");

	objref = test_TestFactory_createLifeCycleServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	test_LifeCycleServer_deactivateOnReturn (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release (objref, ev);
}

static volatile int done = 0;

static void
test_BasicServer_opExceptionA_cb (CORBA_Object          object, 
				  ORBit_IMethod        *m_data, 
				  ORBitAsyncQueueEntry *aqe, 
				  gpointer              user_data, 
				  CORBA_Environment    *ev)
{
	test_TestException *ex;

	/* Not a broken connection */
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	ORBit_small_demarshal_async (aqe, NULL, NULL, ev);

	g_assert (ev->_major == CORBA_USER_EXCEPTION);
	g_assert (strcmp (CORBA_exception_id (ev), ex_test_TestException) == 0);

	ex = CORBA_exception_value (ev);
	g_assert (strcmp (ex->reason, constants_STRING_IN) == 0);
	g_assert (ex->number == constants_LONG_IN);
	g_assert (ex->aseq._length == 1);
	g_assert (ex->aseq._buffer[0] == constants_LONG_IN);

	done = 1;
}


static void
test_BasicServer_opExceptionA (CORBA_Object       obj, 
			       CORBA_Environment *ev)
{
	ORBit_IMethod *m_data;

	m_data = &test_BasicServer__iinterface.methods._buffer [ORBIT_IMETHODS_INDEX(test_BasicServer_opException)];
	/* if this failed, we re-ordered the IDL ... */
	g_assert (!strcmp (m_data->name, "opException"));

	ORBit_small_invoke_async (
		obj, m_data, test_BasicServer_opExceptionA_cb, 
		NULL, NULL, NULL, ev);
}

static void
test_BasicServer_opStringA_cb (CORBA_Object          object, 
			       ORBit_IMethod        *m_data, 
			       ORBitAsyncQueueEntry *aqe, 
			       gpointer              user_data, 
			       CORBA_Environment    *ev)
{
	CORBA_char  *inout_str = NULL, *out_str, *ret_str;
	CORBA_char **out_str_shim = &out_str;

	gpointer args[3];
	gpointer ret    = &ret_str;

	args[0] = NULL;
	args[1] = &inout_str;
	args[2] = &out_str_shim;

	/* Not a broken connection */
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	ORBit_small_demarshal_async (aqe, ret, args, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
  
	g_assert (!strcmp (inout_str, constants_STRING_INOUT_OUT));
	g_assert (!strcmp (out_str, constants_STRING_OUT));
	g_assert (!strcmp (ret_str, constants_STRING_RETN));

	CORBA_free (inout_str);
	CORBA_free (out_str);
	CORBA_free (ret_str);

	done = 1;
}

static void
test_BasicServer_opStringA (CORBA_Object       obj, 
			    const CORBA_char  *in_str, 
			    const CORBA_char  *inout_str, 
			    CORBA_Environment *ev)
{
	gpointer args[3];
	ORBit_IMethod *m_data;

	args[0] = &in_str;
	args[1] = &inout_str;
	args[2] = NULL;

	m_data = &test_BasicServer__iinterface.methods._buffer[ORBIT_IMETHODS_INDEX(test_BasicServer_opString)];
	/* if this failed, we re-ordered the IDL ... */
	g_assert (!strcmp (m_data->name, "opString"));

	ORBit_small_invoke_async (
		obj, m_data, test_BasicServer_opStringA_cb, 
		NULL, args, NULL, ev);
}

static void
wait_until_done (void)
{
	while (!done)
		g_main_context_iteration (NULL, TRUE);
}

static void
testAsync (test_TestFactory   factory, 
	   CORBA_Environment *ev)
{
	test_BasicServer objref;

	d_print ("Testing Async invocations ...\n");
	objref = test_TestFactory_getBasicServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	if (in_proc) {
		g_assert (objref->profile_list == NULL);
		g_assert (ORBit_small_get_connection_ref (objref) == NULL);
	}
			      
	done = 0;
	test_BasicServer_opStringA (
		objref, constants_STRING_IN, 
		constants_STRING_INOUT_IN, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	/* While waiting do some normal methods */
	testString (objref, ev);

	wait_until_done ();

	done = 0;
	test_BasicServer_opExceptionA (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	wait_until_done ();

	CORBA_Object_release (objref, ev);  
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
broken_cb (LinkConnection *connection, gboolean *broken)
{
	*broken = TRUE;
}

static void
testPingPong (test_TestFactory   factory,
	      gboolean           thread_tests,
	      CORBA_Environment *ev)
{
	test_PingPongServer r_objref, l_objref, objref;
	CORBA_unsigned_long before_remote_hash;
	CORBA_unsigned_long after_remote_hash;

#if NUM_THREADS > 0
	if (thread_tests) {
		static volatile int warned = 0;
		if (!warned++)
			g_warning ("No thread available to handle incoming requests");
		return;
	}
#endif

	d_print ("Testing ping pong invocations ...\n");
	r_objref = test_TestFactory_createPingPongServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	l_objref = TestFactory_createPingPongServer (NULL, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Object_release (l_objref, ev); /* only want 1 ref */
	g_assert (ORBit_small_get_servant (l_objref) != NULL);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	
	before_remote_hash = CORBA_Object_hash (l_objref, 0, ev);


	test_PingPongServer_pingPong (r_objref, l_objref, 64, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	
	d_print ("Testing ping pong reg / lookup ...\n");
	test_PingPongServer_set (r_objref, l_objref, "Foo", ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	
	objref = test_PingPongServer_get (r_objref, "Foo", ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (ORBit_small_get_servant (objref) != NULL);
	g_assert (l_objref == objref);

	after_remote_hash = CORBA_Object_hash (l_objref, 0, ev);

	d_print ("Testing hashing\n");
	g_assert (before_remote_hash == after_remote_hash);

	d_print ("Testing equivalence\n");
	g_assert (CORBA_Object_is_equivalent (
		l_objref, objref, ev));
	g_assert (CORBA_Object_is_equivalent (
		CORBA_OBJECT_NIL, CORBA_OBJECT_NIL, ev));
	g_assert (CORBA_Object_is_equivalent (
		l_objref, l_objref, ev));
	g_assert (CORBA_Object_is_equivalent (
		r_objref, r_objref, ev));
	g_assert (!CORBA_Object_is_equivalent (
		r_objref, l_objref, ev));
	g_assert (!CORBA_Object_is_equivalent (
		l_objref, r_objref, ev));
	g_assert (!CORBA_Object_is_equivalent (
		l_objref, CORBA_OBJECT_NIL, ev));
	g_assert (!CORBA_Object_is_equivalent (
		CORBA_OBJECT_NIL, l_objref, ev));

#if 0
	/* Test blocking bits - try to blow the remote guy's stack */
	if (!in_proc) {
		int i;

		d_print ("Testing client limiting of stack smash on remote server\n");
		for (i = 0; i < 10000; i++) {
			test_PingPongServer_opOneWayCallback (r_objref, l_objref, ev);
			g_assert (ev->_major == CORBA_NO_EXCEPTION);
		}
		test_PingPongServer_opRoundTrip (r_objref, ev);
	}
#endif

	if (!in_proc) {
		int i;
		ORBitConnection *cnx = ORBit_small_get_connection_ref (r_objref);
		const char *base =
			"This string is in order to provide some "
			"more bulky data on the wire, the larger "
			"the better. When the socket buffer fills "
			"we start exercising the linc buffering code "
			"and hopefully we get an exception somewhere "
			"indicating that the buffer is full and that "
			"the write has failed & that the connection is "
			"now saturated ";
		char *str;
		gboolean broken = FALSE;

		g_assert (cnx != NULL);
		ORBit_connection_set_max_buffer (cnx, 10000);

		str = g_strdup (base);
		for (i = 0; i < 4; i++) {
			char *new;
			new = g_strconcat (str, str, NULL);
			g_free (str);
			str = new;
		}

		d_print ("Testing non blocking IO ...\n");

		ORBit_small_listen_for_broken (
			r_objref, G_CALLBACK (broken_cb),
			&broken);

		for (i = 0; i < 100; i++) {
			test_PingPongServer_opSleep (
				r_objref, str, ev);
			if (broken)
				break;
		}

		g_free (str);

		/* If this blows - perhaps you just have a strange
		 * system scheduler */
/*		g_assert (broken); */
		CORBA_exception_free (ev);
		ORBit_small_connection_unref (cnx);

		ORBit_connection_set_max_buffer (cnx, 0);
	}

	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release (l_objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release (r_objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testPolicy (test_TestFactory   factory,
	    gboolean           thread_tests,
	    CORBA_Environment *ev)
{
	ORBitPolicy *policy;
	test_PingPongServer r_objref, l_objref;

	if (thread_tests || !thread_safe)
		return;

	d_print ("Testing policy code ...\n");
	r_objref = test_TestFactory_createPingPongServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	l_objref = TestFactory_createPingPongServer (NULL, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Object_release (l_objref, ev); /* only want 1 ref */
	g_assert (ORBit_small_get_servant (l_objref) != NULL);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	
	policy = ORBit_policy_new (ORBIT_TYPE_POLICY_EX,
				   "allow", global_poa, NULL);
	ORBit_object_set_policy (r_objref, policy);
	test_PingPongServer_pingPong (r_objref, l_objref, 64, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	ORBit_object_set_policy (r_objref, NULL);
	ORBit_policy_unref (policy);

	CORBA_Object_release (l_objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release (r_objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
dummy_cb (LinkConnection *connection, gboolean *invoked)
{
	*invoked = TRUE;
}

static void
testSegv (test_TestFactory   factory, 
	  CORBA_Environment *ev)
{
	gboolean broken = FALSE;
	gboolean invoked = FALSE;

	if (in_proc) 
		return;

	d_print ("Testing Fatal invocations ...\n");

	g_assert (ORBit_small_listen_for_broken (
						 factory, G_CALLBACK (broken_cb), &broken) ==
		  ORBIT_CONNECTION_CONNECTED);

	g_assert (ORBit_small_listen_for_broken (
						 factory, G_CALLBACK (dummy_cb), &invoked) ==
		  ORBIT_CONNECTION_CONNECTED);

	g_assert (ORBit_small_unlisten_for_broken (
						   factory, G_CALLBACK (dummy_cb)) ==
		  ORBIT_CONNECTION_CONNECTED);
	g_assert (!CORBA_Object_non_existent (factory, ev));

	test_TestFactory_segv (factory, "do it!", ev); 

#ifdef DO_HARDER_SEGV // unusual
	g_assert (ev->_major == CORBA_SYSTEM_EXCEPTION);
	g_assert (!strcmp (ev->_id, "IDL:omg.org/CORBA/COMM_FAILURE:1.0"));
	CORBA_exception_free (ev);

	g_assert (ORBit_small_get_connection_status (factory) ==
		  ORBIT_CONNECTION_DISCONNECTED);
	g_assert (broken);
	g_assert (!invoked);
#else
	if (ORBit_small_unlisten_for_broken (factory, G_CALLBACK (broken_cb)) !=
	    ORBIT_CONNECTION_CONNECTED)
		g_warning ("Unusual race in unlisten");
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
#endif
}

static void
test_initial_references (CORBA_ORB          orb,
			 CORBA_Environment *ev)
{
	CORBA_ORB_ObjectIdList *list;
	int                     i;

	fprintf (stderr, "\nInitial References:\n");

	list = CORBA_ORB_list_initial_services (orb, ev);

	g_assert (ev->_major == CORBA_NO_EXCEPTION && list && list->_length);

	for (i = 0; i < list->_length; i++) {
		CORBA_ORB_ObjectId id;
		CORBA_Object       obj;

		id = list->_buffer [i];

		g_assert (id);

		fprintf (stderr, "\t%s ... ", id);

		obj = CORBA_ORB_resolve_initial_references (orb, id, ev);

		g_assert (ev->_major == CORBA_NO_EXCEPTION && obj != CORBA_OBJECT_NIL);

		CORBA_Object_release (obj, ev);

		g_assert (ev->_major == CORBA_NO_EXCEPTION);

		fprintf (stderr, "okay\n");
	}

	CORBA_free (list);
}

#define TIME_TEST_RUNS 1000

static void
test_time_noop (test_TestFactory   factory, 
		CORBA_Environment *ev)
{
	int i;
	int old_flags;
	GTimer *timer;

	timer = g_timer_new ();
	g_timer_start (timer);
	for (i = 0; i < TIME_TEST_RUNS; i++)
		test_TestFactory_noOp (factory, ev);
	g_timer_stop (timer);
	fprintf (stderr, "In proc (fast) took %g msecs\n",
	g_timer_elapsed (timer, NULL) * 1000);

	old_flags = ORBit_small_flags;
	ORBit_small_flags |= ORBIT_SMALL_FORCE_GENERIC_MARSHAL;
	g_timer_reset (timer);
	g_timer_start (timer);
	for (i = 0; i < TIME_TEST_RUNS; i++)
		test_TestFactory_noOp (factory, ev);
	g_timer_stop (timer);
	fprintf (stderr, "In proc (slow) took %g msecs\n",
		 g_timer_elapsed (timer, NULL) * 1000);
	ORBit_small_flags = old_flags;

	g_timer_destroy (timer);
}

static void
test_basic_server (test_TestFactory   factory, 
		   CORBA_Environment *ev)
{
	test_BasicServer objref;

	objref = test_TestFactory_getBasicServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (objref != CORBA_OBJECT_NIL);
	g_assert (CORBA_Object_is_a (objref, "IDL:orbit/test/BasicServer:1.0", ev));
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	testAttribute  (objref, ev);
	testString     (objref, ev);
	testLong       (objref, ev);
	testLongLong   (objref, ev);
	testFloat      (objref, ev);
	testDouble     (objref, ev);
	testLongDouble (objref, ev);
	testEnum       (objref, ev);
	testException  (objref, ev);
	testBoolAlign  (objref, ev);

	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
}

static void
testDerivedServer (test_TestFactory   factory, 
		   CORBA_Environment *ev)
{
	CORBA_Object            obj;
	PortableServer_ServantBase *servant;
	PortableServer_ObjectId *oid;
	ORBit_POAObject             pobj;
	PortableServer_ServantBase__epv DerivedServer_base_epv = {NULL, simple_finalize, NULL};
	POA_test_BasicServer__vepv      DerivedServer_vepv = { &DerivedServer_base_epv, NULL };

	d_print ("Testing DerivedServer ...\n");

	servant = SIMPLE_SERVANT_NEW (DerivedServer);
	obj = create_object (global_poa, servant, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	g_assert (test_C1__classid != 0);
	g_assert (test_B1__classid != 0);
	g_assert (test_B2__classid != 0);
	g_assert (test_DerivedServer__classid != 0);

	pobj = (ORBit_POAObject) obj->adaptor_obj;
	g_assert (pobj->vepvmap_cache [test_DerivedServer__classid] != 0);
	g_assert (pobj->vepvmap_cache [test_C1__classid] != 0);
	g_assert (pobj->vepvmap_cache [test_B1__classid] != 0);
	g_assert (pobj->vepvmap_cache [test_B2__classid] != 0);

	CORBA_Object_release (obj, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	oid = PortableServer_POA_servant_to_id (global_poa, servant, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	PortableServer_POA_deactivate_object (global_poa, oid, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	
	CORBA_free (oid);	
}

static void
testNonExistent (test_TestFactory factory, CORBA_Environment *ev)
{
	CORBA_Object non_existent;
	const char *non_existent_ior =
		"IOR:010000001f00000049444c3a6f726269742f746573742f54657"
		"374466163746f72793a312e300000030000000054424f6400000001"
		"01020005000000554e495800000000160000006c6f63616c686f737"
		"42e6c6f63616c646f6d61696e0000002d0000002f746d702f6f7262"
		"69742d6d69636861656c2f6c696e632d363733322d302d373362323"
		"966373333316662390000000000000000caaedfba58000000010102"
		"002d0000002f746d702f6f726269742d6d69636861656c2f6c696e6"
		"32d363733322d302d37336232396637333331666239000000001c00"
		"000000000000331c40f8ba0fa828dc2928282828282808000000db7"
		"e269601000000480000000100000002000000050000001c00000000"
		"000000331c40f8ba0fa828dc2928282828282808000000db7e26960"
		"1000000140000000100000001000105000000000901010000000000";

	if (!in_proc)
		return;
	d_print ("Testing CORBA_Object_non_existent ...\n");

	non_existent = CORBA_ORB_string_to_object
		(global_orb, non_existent_ior, ev);
	g_assert (CORBA_Object_non_existent (non_existent, ev));
	CORBA_Object_release (non_existent, NULL);
}

static void
testWithException (test_TestFactory factory, CORBA_Environment *ev)
{
	int old_flags;
	CORBA_Object objref;

	CORBA_exception_set (ev, CORBA_SYSTEM_EXCEPTION,
			     ex_CORBA_OBJECT_NOT_EXIST, NULL);
	objref = test_TestFactory_getBasicServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	old_flags = ORBit_small_flags;
	ORBit_small_flags |= ORBIT_SMALL_FORCE_GENERIC_MARSHAL;

	CORBA_exception_set (ev, CORBA_SYSTEM_EXCEPTION,
			     ex_CORBA_OBJECT_NOT_EXIST, NULL);
	objref = test_TestFactory_getBasicServer (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Object_release (objref, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	ORBit_small_flags = old_flags;
}

static void
run_tests (test_TestFactory   factory, 
	   gboolean           thread_tests,
	   CORBA_Environment *ev)
{
	int i;

	for (i = 0; i < NUM_RUNS; i++) {
		testSequenceHelpers ();
		testConst ();
		test_basic_server (factory, ev);
		testIsA (factory, ev);
		testFixedLengthStruct (factory, ev);
		testVariableLengthStruct (factory, ev);
		testCompoundStruct (factory, ev);
		testAlignHoleStruct (factory, ev);
		testObjectStruct (factory, ev);
		testStructAny (factory, ev);
		testUnboundedSequence (factory, ev);
		testBoundedSequence (factory, ev);
		testAnySequence (factory, ev);
		testFixedLengthUnion (factory, ev);
		testVariableLengthUnion (factory, ev);
		testMiscUnions (factory, ev);
		testUnionArray (factory, ev);
		testFixedLengthArray (factory, ev);
		testVariableLengthArray (factory, ev);
		testAnyStrSeq (factory, ev);
		testAnyLong (factory, ev);
		testAnyString (factory, ev);
		testAnyStruct (factory, ev);
		testAnyException (factory, ev);
		testAnyEquivalence (factory, ev);
		testSequenceOfAny (factory, ev);
		testTypeCode (factory, ev);
		testContext (factory, ev);
		testIInterface (factory, ev);
		testDerivedServer (factory, ev);
		testNonExistent (factory, ev);
#ifndef TIMING_RUN
		if (!thread_tests)
			testAsync (factory, ev);
#endif
		if (!in_proc) {
			testPingPong (factory, thread_tests, ev);
			testPolicy (factory, thread_tests, ev);
		}
		testMisc (factory, ev);
		testIOR (factory, ev);
		testWithException (factory, ev);
		testLifeCycle (factory, ev);
	}
	
#if NUM_RUNS > 1
	g_warning ("Did '%d' iterations", i);
#endif
}

static gpointer
test_thread (gpointer data)
{
	CORBA_Environment ev[1];
	test_TestFactory factory = data;

	CORBA_exception_init (ev);
	run_tests (factory, TRUE, ev);
	CORBA_exception_free (ev);

	return data;
}

static void
run_threaded_tests (test_TestFactory   factory, 
		    CORBA_Environment *ev)
{
	int i;
	GError *error = NULL;
	GThread **threads;

	if (!NUM_THREADS)
		return;

	fprintf (stderr, "Testing with %d threads\n", NUM_THREADS);

	threads = g_new0 (GThread *, NUM_THREADS);

	for (i = 0; i < NUM_THREADS; i++) {
		threads [i] = g_thread_create
			( test_thread, factory, TRUE, &error);
		g_assert (!error);
	}

	for (i = 0; i < NUM_THREADS; i++) {
		if (!(g_thread_join (threads [i]) == factory))
			g_error ("Wierd thread join problem '%d'", i);
	}
}

static void
dump_protos (void)
{
	int enabled_count = 0;
	LinkProtocolInfo *info;

	for (info = link_protocol_all (); info->name; info++) {
		gboolean enabled;

		if ((enabled = ORBit_proto_use (info->name)))
			enabled_count++;

		fprintf (stderr, "Protocol %8s: %s\n",
			 info->name, 
			 enabled ? "enabled" : "disabled");
	}

	g_assert (enabled_count > 0);
}

static void
test_init (CORBA_Environment *ev)
{
	g_assert (CORBA_ORB_init (NULL, NULL, "", ev) == global_orb);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_ORB_destroy (global_orb, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Object_release ((CORBA_Object)global_orb, ev);
}

int
main (int argc, char *argv [])
{
	CORBA_Environment  ev[1];
	test_TestFactory   factory;
	ORBit_IInterfaces *interfaces = NULL;
	gboolean           gen_imodule = FALSE;
	char              *orb_name;
	int                i;

	g_thread_init (NULL);

	CORBA_exception_init (ev);

/* FIXME - make this work nicely sometime.
	global_orb = CORBA_ORB_init (&argc, argv, "", ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	CORBA_Object_release (global_orb, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
*/
	for (i = 0; i < argc; i++) {
		if (!strcmp (argv [i], "--gen-imodule"))
			gen_imodule = TRUE;
		if (!strcmp (argv [i], "--thread-safe"))
			thread_safe = TRUE;
		if (!strcmp (argv [i], "--thread-tests")) {
			thread_safe = TRUE;
			thread_tests = TRUE;
		}
	}

	if (thread_safe)
		orb_name = "orbit-local-orb";
	else
		orb_name = "orbit-local-non-threaded-orb";

	global_orb = CORBA_ORB_init (&argc, argv, orb_name, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	/*	if (thread_tests) {
		g_warning ("FIXME: testing only");
		link_set_io_thread (TRUE);
		} */

	if (gen_imodule) {
		CORBA_sequence_CORBA_TypeCode *typecodes = NULL;

		interfaces = ORBit_iinterfaces_from_file (
				TEST_SRCDIR "/everything.idl", NULL, &typecodes);
		g_assert (interfaces != NULL);
		g_assert (typecodes != NULL);

		CORBA_free (typecodes);

		init_iinterfaces (interfaces, ev);
	}

	test_init (ev);
	test_initial_references (global_orb, ev);

	free (malloc (8)); /* -lefence */

	dump_protos ();

	/* In Proc ... */
	in_proc = TRUE;

	fprintf (stderr, "\n --- In proc ---\n\n\n");
	factory = get_server (global_orb, ev);
	g_assert (factory->profile_list == NULL);
	g_assert (ORBit_object_get_connection (factory) == NULL);

	test_time_noop (factory, ev);
	run_tests (factory, FALSE, ev);
	if (thread_tests)
		g_warning ("FIXME: disabled in-proc threaded tests for now");
/*		run_threaded_tests (factory, ev); */

	CORBA_Object_release (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	factory = CORBA_OBJECT_NIL;

	fprintf (stderr, "\n\n --- Out of proc ---\n\n\n");
	in_proc = FALSE;

	{ /* read the ior from iorfile, and swizzle to an objref*/
		int   size;
		char  ior [1024];
		FILE *infile = fopen ("iorfile", "rb");

		if (!infile)
			g_error ("Start the server before running the client");

		size = fread (ior, 1, 1024, infile);
		fclose (infile);
		ior [size] = '\0';   /* insure that string is terminated correctly */

		factory = CORBA_ORB_string_to_object (global_orb, ior, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);

		if (CORBA_Object_non_existent (factory, ev))
			g_error  ("Can't contact the server");
		g_assert (ev->_major == CORBA_NO_EXCEPTION);
	}
	run_tests (factory, FALSE, ev);
	if (thread_tests)
		run_threaded_tests (factory, ev);
	testSegv (factory, ev);

	CORBA_Object_release (factory, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	if (gen_imodule)
		CORBA_free (interfaces);

	CORBA_Object_release ((CORBA_Object) global_poa, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	
	CORBA_ORB_destroy (global_orb, ev);
	CORBA_exception_free (ev);

	CORBA_Object_release ((CORBA_Object) global_orb, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_exception_free (ev);

	d_print ("All tests passed successfully\n");

	return 0;
}
