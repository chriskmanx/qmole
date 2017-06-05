#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <orbit/orbit.h>
#include "dynany.h"

#define CHECK_OK(ev)	g_assert ((ev)->_major == CORBA_NO_EXCEPTION)
#define CHECK_TYPE_MISMATCH(ev) \
	do { \
		g_assert ((ev)->_major == CORBA_USER_EXCEPTION && \
		          !strcmp ((ev)->_id, ex_DynamicAny_DynAny_TypeMismatch)); \
		CORBA_exception_free (ev); \
	} while (0)
#define CHECK_INVALID_VALUE(ev) \
	do { \
		g_assert ((ev)->_major == CORBA_USER_EXCEPTION && \
		          !strcmp ((ev)->_id, ex_DynamicAny_DynAny_InvalidValue)); \
		CORBA_exception_free (ev); \
	} while (0)
#define CHECK_OBJECT_NOT_EXIST(ev) \
	do { \
		g_assert ((ev)->_major == CORBA_SYSTEM_EXCEPTION && \
		          !strcmp ((ev)->_id, ex_CORBA_OBJECT_NOT_EXIST)); \
		CORBA_exception_free (ev); \
	} while (0)


static DynamicAny_DynAny
create_basic_dyn_any (CORBA_ORB orb,
				CORBA_TypeCode type,
				CORBA_Environment *ev)
{
	DynamicAny_DynAnyFactory f;
	DynamicAny_DynAny d;

	f = (DynamicAny_DynAnyFactory)
		CORBA_ORB_resolve_initial_references (
			orb, "DynAnyFactory", ev);
	CHECK_OK (ev);

	d = DynamicAny_DynAnyFactory_create_dyn_any_from_type_code (
		f, type, ev);
	CHECK_OK (ev);

	CORBA_Object_release ((CORBA_Object)f, ev);
	CHECK_OK (ev);

	return d;
}

static DynamicAny_DynAny
create_dyn_any (CORBA_ORB orb,
			  const CORBA_any *any,
			  CORBA_Environment *ev)
{
	DynamicAny_DynAnyFactory f;
	DynamicAny_DynAny d;

	f = (DynamicAny_DynAnyFactory)
		CORBA_ORB_resolve_initial_references (
			orb, "DynAnyFactory", ev);
	CHECK_OK (ev);

	d = DynamicAny_DynAnyFactory_create_dyn_any (
		f, any, ev);
	CHECK_OK (ev);

	CORBA_Object_release ((CORBA_Object)f, ev);
	CHECK_OK (ev);

	return d;
}

static int
double_equal (double a, double b)
{
	const double delta = 0.0001;

	if (fabs (a - b) < delta)
		return TRUE;
	else
		return FALSE;
}

static void
test_long (CORBA_ORB orb, CORBA_Environment *ev)
{
	DynamicAny_DynAny   dyn_any;
	CORBA_long     value;
	CORBA_TypeCode type;

	dyn_any = create_basic_dyn_any (orb, TC_CORBA_long, ev);
	CHECK_OK (ev);
	g_assert (dyn_any != CORBA_OBJECT_NIL);

	/* 1. Inserting */
	DynamicAny_DynAny_insert_long (dyn_any, 2, ev);
	CHECK_OK (ev);

	DynamicAny_DynAny_insert_boolean (dyn_any, TRUE, ev);
	CHECK_TYPE_MISMATCH (ev);

	DynamicAny_DynAny_insert_double (dyn_any, 1.3267, ev);
	CHECK_TYPE_MISMATCH (ev);

	DynamicAny_DynAny_insert_reference (dyn_any, (CORBA_Object) dyn_any, ev);
	CHECK_TYPE_MISMATCH (ev);

	/* 2. Getting */
	DynamicAny_DynAny_get_boolean (dyn_any, ev);
	CHECK_TYPE_MISMATCH (ev);

	DynamicAny_DynAny_get_double (dyn_any, ev);
	CHECK_TYPE_MISMATCH (ev);

	DynamicAny_DynAny_get_reference (dyn_any, ev);
	CHECK_TYPE_MISMATCH (ev);

	value = DynamicAny_DynAny_get_long (dyn_any, ev);
	CHECK_OK (ev);
	g_assert (value == 2);

	type = DynamicAny_DynAny_type (dyn_any, ev);
	CHECK_OK (ev);

	g_assert (CORBA_TypeCode_equal (type, TC_CORBA_long, ev));
	CHECK_OK (ev);

	CORBA_Object_release ((CORBA_Object) type, ev);
	CHECK_OK (ev);

	CORBA_Object_release ((CORBA_Object) dyn_any, ev);
	CHECK_OK (ev);
}

static void
test_string (CORBA_ORB orb, CORBA_Environment *ev)
{
	DynamicAny_DynAny dyn_any;
	CORBA_char  *value;
	const char   string[] = "Hello World";

	dyn_any = create_basic_dyn_any (orb, TC_CORBA_string, ev);
	CHECK_OK (ev);
	g_assert (dyn_any != CORBA_OBJECT_NIL);

	/* 1. Inserting */
	DynamicAny_DynAny_insert_string (dyn_any, (CORBA_char *)string, ev);
	CHECK_OK (ev);

	DynamicAny_DynAny_insert_boolean (dyn_any, TRUE, ev);
	CHECK_TYPE_MISMATCH (ev);

	DynamicAny_DynAny_insert_double (dyn_any, 1.3267, ev);
	CHECK_TYPE_MISMATCH (ev);

	/* 2. Getting */

	DynamicAny_DynAny_get_boolean (dyn_any, ev);
	CHECK_TYPE_MISMATCH (ev);

	DynamicAny_DynAny_get_double (dyn_any, ev);
	CHECK_TYPE_MISMATCH (ev);

	DynamicAny_DynAny_get_reference (dyn_any, ev);
	CHECK_TYPE_MISMATCH (ev);

	value = DynamicAny_DynAny_get_string (dyn_any, ev);
	CHECK_OK (ev);
	g_assert (!strcmp (value, string));
	CORBA_free (value);

	CORBA_Object_release ((CORBA_Object) dyn_any, ev);
	CHECK_OK (ev);
}

static void
test_copy (CORBA_ORB orb, CORBA_Environment *ev)
{
	DynamicAny_DynAny dyn_any;
	DynamicAny_DynAny dyn_any_copy;
	CORBA_any   *any;
	const char   string[] = "Hello World2";

	dyn_any = create_basic_dyn_any (orb, TC_CORBA_string, ev);
	CHECK_OK (ev);
	g_assert (dyn_any != CORBA_OBJECT_NIL);

	DynamicAny_DynAny_insert_string (dyn_any, (CORBA_char *)string, ev);
	CHECK_OK (ev);

	any = DynamicAny_DynAny_to_any (dyn_any, ev);
	CHECK_OK (ev);
	g_assert (any != NULL);

	dyn_any_copy = create_dyn_any (orb, any, ev);
	CHECK_OK (ev);
	g_assert (dyn_any_copy != NULL);
	CORBA_free (any);

	g_assert (DynamicAny_DynAny_equal (dyn_any_copy, dyn_any, ev));
	CHECK_OK (ev);

	{ /* Knock up an integer any */
		DynamicAny_DynAny int_any = create_basic_dyn_any (
			orb, TC_CORBA_long, ev);
		CHECK_OK (ev);

		DynamicAny_DynAny_insert_long (int_any, 57, ev);
		CHECK_OK (ev);

		any = DynamicAny_DynAny_to_any (int_any, ev);
		CHECK_OK (ev);

		CORBA_Object_release ((CORBA_Object) int_any, ev);
		CHECK_OK (ev);
	}

	DynamicAny_DynAny_from_any (dyn_any, any, ev);
	CHECK_TYPE_MISMATCH (ev);
	CORBA_free (any);

	DynamicAny_DynAny_assign (dyn_any, dyn_any_copy, ev);
	CHECK_OK (ev);

	g_assert (DynamicAny_DynAny_equal (dyn_any_copy, dyn_any, ev));
	CHECK_OK (ev);

	CORBA_Object_release ((CORBA_Object) dyn_any, ev);
	CHECK_OK (ev);

	CORBA_Object_release ((CORBA_Object) dyn_any_copy, ev);
	CHECK_OK (ev);
}

static void
test_sequence (CORBA_ORB orb, CORBA_Environment *ev)
{
	DynamicAny_DynAny      dyn_any;
	DynamicAny_DynSequence dyn_seq;
	int          i, len;

	dyn_any = create_basic_dyn_any (
		orb, TC_CORBA_sequence_CORBA_octet, ev);
	dyn_seq = (DynamicAny_DynSequence) dyn_any;
	CHECK_OK (ev);
	g_assert (dyn_any != CORBA_OBJECT_NIL);

	DynamicAny_DynAny_insert_long (dyn_any, 5, ev);
	CHECK_TYPE_MISMATCH (ev);

	DynamicAny_DynSequence_set_length (dyn_seq, 100, ev);
	CHECK_OK (ev);

	for (i = 0; i < 100; i++) {
		g_assert (DynamicAny_DynAny_seek (dyn_any, i, ev));
		CHECK_OK (ev);
		DynamicAny_DynAny_insert_octet (dyn_any, 100 - i, ev);
		CHECK_OK (ev);
	}

	len = DynamicAny_DynAny_component_count (dyn_any, ev);
	CHECK_OK (ev);
	g_assert (len == 100);

	len = DynamicAny_DynSequence_get_length (dyn_seq, ev);
	CHECK_OK (ev);
	g_assert (len == 100);

	/* Only growing the length for now */
	DynamicAny_DynAny_seek (dyn_any, -1, ev);
	CHECK_OK (ev);

	DynamicAny_DynSequence_set_length (dyn_seq, 150, ev);
	CHECK_OK (ev);

	len = DynamicAny_DynSequence_get_length (dyn_seq, ev);
	CHECK_OK (ev);
	g_assert (len == 150);

	DynamicAny_DynAny_insert_octet (dyn_any, 137, ev);
	CHECK_OK (ev);

	i = DynamicAny_DynAny_get_octet (dyn_any, ev);
	CHECK_OK (ev);

	g_assert (i == 137);

	DynamicAny_DynSequence_set_length (dyn_seq, 200, ev);
	CHECK_OK (ev);

	i = DynamicAny_DynAny_get_octet (dyn_any, ev);
	CHECK_OK (ev);

	g_assert (i == 137);

	len = DynamicAny_DynSequence_get_length (dyn_seq, ev);
	CHECK_OK (ev);
	g_assert (len == 200);

	{
		DynamicAny_AnySeq *seq;

		seq = DynamicAny_DynSequence_get_elements (dyn_seq, ev);
		CHECK_OK (ev);

		DynamicAny_DynSequence_set_elements (dyn_seq, seq, ev);
		CHECK_OK (ev);

		CORBA_free (seq);
	}	

	CORBA_Object_release ((CORBA_Object) dyn_any, ev);
	CHECK_OK (ev);
}

static void
test_array (CORBA_ORB orb, CORBA_Environment *ev)
{
	DynamicAny_DynAny dyn_any;
	CORBA_double d;
	int          i;

	dyn_any = create_basic_dyn_any (
		orb, TC_Test_MyArray, ev);
	CHECK_OK (ev);
	g_assert (dyn_any != CORBA_OBJECT_NIL);

	g_assert (DynamicAny_DynAny_seek (dyn_any, 99, ev));
	CHECK_OK (ev);

	DynamicAny_DynAny_insert_long (dyn_any, 2, ev);
	CHECK_TYPE_MISMATCH (ev);

	DynamicAny_DynAny_insert_double (dyn_any, 2.71828182845, ev);
	CHECK_OK (ev);

	/* 1. Insert */
	for (i = 0; i < 37; i+= 3) {
		d = i * 2.7 + 3.1;

		DynamicAny_DynAny_seek (dyn_any, i, ev);
		CHECK_OK (ev);

		DynamicAny_DynAny_insert_double (dyn_any, d, ev);
		CHECK_OK (ev);
	}

	/* 1. Extract */
	for (i = 0; i < 37; i+= 3) {
		d = i * 2.7 + 3.1;

		DynamicAny_DynAny_seek (dyn_any, i, ev);
		CHECK_OK (ev);

		g_assert (double_equal (DynamicAny_DynAny_get_double (dyn_any, ev), d));
		CHECK_OK (ev);
	}

	CORBA_Object_release ((CORBA_Object) dyn_any, ev);
	CHECK_OK (ev);
}

static const char *
enum_subnames_array [] = {
	"KIPPER",
	"BLOATER",
	"HERRING"
};

static void
test_enum (CORBA_ORB orb, CORBA_Environment *ev)
{
	int i;
	DynamicAny_DynEnum dyn_enum;

	dyn_enum = (DynamicAny_DynEnum)
		create_basic_dyn_any (
			orb, TC_Test_Fishy, ev);
	CHECK_OK (ev);
	g_assert (dyn_enum != CORBA_OBJECT_NIL);

	i = DynamicAny_DynEnum_get_as_ulong (dyn_enum, ev);
	CHECK_OK (ev);
	g_assert (i == 0);

	for (i = 0; i < sizeof (enum_subnames_array) / sizeof (const char *); i++) {

		DynamicAny_DynEnum_set_as_string (dyn_enum, enum_subnames_array [i], ev);
		CHECK_OK (ev);

		g_assert (DynamicAny_DynEnum_get_as_ulong (dyn_enum, ev) == i);
		CHECK_OK (ev);
	}

	for (i = 0; i < sizeof (enum_subnames_array) / sizeof (const char *); i++) {
		CORBA_char  *str;

		DynamicAny_DynEnum_set_as_ulong (dyn_enum, i, ev);
		CHECK_OK (ev);

		str = DynamicAny_DynEnum_get_as_string (dyn_enum, ev);
		CHECK_OK (ev);

		g_assert (!strcmp (str, enum_subnames_array [i]));
		CORBA_free (str);
	}

	CORBA_Object_release ((CORBA_Object) dyn_enum, ev);
	CHECK_OK (ev);
}

static const char *
union_subnames_array [] = {
	"tgw",
	"nut",
	"atl",
	"rmt",
	"ibid"
};

static CORBA_TypeCode
union_subtypes_array [] = {
	TC_CORBA_long,
	TC_CORBA_double,
	TC_CORBA_string,
	TC_Test_Fishy,
	TC_Test_OSeq
};

static void
test_union (CORBA_ORB orb, CORBA_Environment *ev)
{
	DynamicAny_DynUnion dyn_union;
	CORBA_TCKind        kind;

	dyn_union = (DynamicAny_DynUnion)
		create_basic_dyn_any (
			orb, TC_Test_English, ev);
	CHECK_OK (ev);
	g_assert (dyn_union != CORBA_OBJECT_NIL);

	kind = DynamicAny_DynUnion_discriminator_kind (dyn_union, ev);
	CHECK_OK (ev);
	g_assert (kind == CORBA_tk_ulong);

	CORBA_Object_release ((CORBA_Object) dyn_union, ev);
	CHECK_OK (ev);
}

static void
test_struct (CORBA_ORB orb, CORBA_Environment *ev)
{
	DynamicAny_DynAny dyn_any, sub_any;
	DynamicAny_DynStruct dyn_struct;
	DynamicAny_NameValuePairSeq *members;
	/* DynamicAny_NameDynAnyPairSeq *dyn_members; */
	CORBA_TCKind kind;
	CORBA_char  *str;
	CORBA_double dv = 1.23;
	const char  *test_str = "one is not amused";
	int i;

	dyn_any = create_basic_dyn_any (
		orb, TC_Test_Unions, ev);
	dyn_struct = (DynamicAny_DynStruct) dyn_any;
	CHECK_OK (ev);
	g_assert (dyn_any != CORBA_OBJECT_NIL);

	g_assert (!DynamicAny_DynStruct_seek (dyn_any, 6, ev));
	CHECK_OK (ev);

	for (i = 0; i < 5; i++) {
		DynamicAny_DynStruct_seek (dyn_any, i, ev);
		CHECK_OK (ev);

		kind = DynamicAny_DynStruct_current_member_kind (dyn_struct, ev);
		CHECK_OK (ev);

		g_assert (union_subtypes_array [i]->kind == kind);

		str = DynamicAny_DynStruct_current_member_name (dyn_struct, ev);
		CHECK_OK (ev);

		g_assert (!strcmp (union_subnames_array [i], str));
		CORBA_free (str);
	}

	g_assert (DynamicAny_DynStruct_seek (dyn_any, 0, ev));
	CHECK_OK (ev);

	DynamicAny_DynAny_insert_long (dyn_any, 345, ev);
	CHECK_OK (ev);

	g_assert (DynamicAny_DynAny_next (dyn_any, ev));
	CHECK_OK (ev);

	DynamicAny_DynAny_insert_double (dyn_any, dv, ev);
	CHECK_OK (ev);

	g_assert (DynamicAny_DynAny_next (dyn_any, ev));
	CHECK_OK (ev);

	DynamicAny_DynAny_insert_string (dyn_any, test_str, ev);
	CHECK_OK (ev);

	sub_any = DynamicAny_DynAny_current_component (dyn_any, ev);
	CHECK_OK (ev);

	str = DynamicAny_DynAny_get_string (sub_any, ev);
	CHECK_OK (ev);
	
	g_assert (!strcmp (test_str, str));

	CORBA_free (str);

	g_assert (DynamicAny_DynStruct_seek (dyn_any, 0, ev));
	CHECK_OK (ev);

	g_assert (DynamicAny_DynAny_get_long (dyn_any, ev) == 345);
	CHECK_OK (ev);

	g_assert (DynamicAny_DynAny_next (dyn_any, ev));
	CHECK_OK (ev);

	g_assert (double_equal (DynamicAny_DynAny_get_double (dyn_any, ev), dv));
	CHECK_OK (ev);

	g_assert (DynamicAny_DynAny_next (dyn_any, ev));
	CHECK_OK (ev);

	members = DynamicAny_DynStruct_get_members (dyn_struct, ev);
	g_assert (members != NULL);
	CHECK_OK (ev);

	DynamicAny_DynStruct_set_members (dyn_struct, members, ev);
	CHECK_OK (ev);

	CORBA_free (members);

/*	it's not clear how best to deal with this:

	dyn_members = DynamicAny_DynStruct_get_members_as_dyn_any (dyn_any, ev);
	g_assert (dyn_members != NULL);
	CHECK_OK (ev);

	DynamicAny_DynStruct_set_members_as_dyn_any (dyn_any, dyn_members, ev);
	CHECK_OK (ev);

	CORBA_free (dyn_members);*/

	CORBA_Object_release ((CORBA_Object) dyn_any, ev);
	CHECK_OK (ev);

	str = DynamicAny_DynAny_get_string (sub_any, ev);
	CHECK_OBJECT_NOT_EXIST (ev);

	CORBA_Object_release ((CORBA_Object) sub_any, ev);
	CHECK_OK (ev);
}

#define d_printf printf

int
main (int argc, char *argv[])
{
	CORBA_Environment ev;
	CORBA_ORB    orb;

	free (malloc (8));

	g_thread_init (NULL);

	CORBA_exception_init (&ev);
	orb = CORBA_ORB_init (NULL, NULL, "orbit-local-orb", &ev);
  
	/*
	 *  Since the API is entirely macro generated
	 * we only need to test a few cases.
	 */
	d_printf      ("Testing basic DynAny ...\n");
	d_printf      (" + long ops ...\n");
	test_long     (orb, &ev);

	d_printf      (" + string ops ...\n");
	test_string   (orb, &ev);

	d_printf      (" + copying ...\n");
	test_copy     (orb, &ev);

	d_printf      ("Testing DynSequence ...\n");
	test_sequence (orb, &ev);

	d_printf      ("Testing DynEnum ...\n");
	test_enum     (orb, &ev);

	d_printf      ("Testing DynUnion ...\n");
	test_union    (orb, &ev);

	d_printf      ("Testing DynArray...\n");
	test_array    (orb, &ev);

	d_printf      ("Testing DynStruct...\n");
	test_struct   (orb, &ev);

	CORBA_ORB_destroy (orb, &ev);
	CHECK_OK (&ev);
	CORBA_Object_release ((CORBA_Object) orb, &ev);
	CHECK_OK (&ev);

	d_printf ("all DynAny tests passed ok.\n");

	return 0;
}
