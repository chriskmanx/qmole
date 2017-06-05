/*
 * orbit-imodule-utils.c:
 *
 * Copyright (C) 2002 Sun Microsystems, Inc.
 *                    Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * Authors:
 *     Mark McLoughlin <mark@skynet.ie>
 *     Elliot Lee <sopwith@redhat.com
 */

#include <string.h>

#include <orbit/orbit.h>

#include "orbit-imodule-utils.h"
#include "orbit-imodule-libidl-utils.h"

static CORBA_StructMemberSeq *
ORBit_imodule_get_struct_members (GHashTable        *typecodes,
				  IDL_tree           tree,
				  CORBA_Environment *ev)
{
	CORBA_StructMemberSeq *members;
	IDL_tree               l;
	int                    num_members = 0;
	int                    i;

	g_return_val_if_fail (IDL_NODE_TYPE (tree) == IDLN_TYPE_STRUCT ||
			      IDL_NODE_TYPE (tree) == IDLN_EXCEPT_DCL, NULL);

	for (l = IDL_TYPE_STRUCT (tree).member_list; l; l = IDL_LIST (l).next)
		num_members += IDL_list_length (IDL_MEMBER (IDL_LIST (l).data).dcls);

	members = CORBA_StructMemberSeq__alloc ();

	members->_length  = members->_maximum = num_members;
	members->_buffer  = CORBA_StructMemberSeq_allocbuf (members->_length);
	members->_release = CORBA_TRUE;

	for (i = 0, l = IDL_TYPE_STRUCT (tree).member_list; l; l = IDL_LIST (l).next) {
		CORBA_TypeCode subtc;
		IDL_tree       dcl;

		subtc = ORBit_imodule_get_typecode (
				typecodes, IDL_MEMBER (IDL_LIST (l).data).type_spec);

		for (dcl = IDL_MEMBER (IDL_LIST (l).data).dcls; dcl;
		     dcl = IDL_LIST (dcl).next, i++) {
			CORBA_StructMember *member = &members->_buffer [i];
			CORBA_string        name;

			if (IDL_NODE_TYPE (dcl) == IDLN_IDENT)
				name = IDL_IDENT (dcl).str;
			else /* IDLN_TYPE_ARRAY */
				name = IDL_IDENT (IDL_TYPE_ARRAY (dcl).ident).str;

			member->name     = CORBA_string_dup (name);
			member->type     = (CORBA_TypeCode)
						CORBA_Object_duplicate ((CORBA_Object) subtc, ev);
			member->type_def = CORBA_OBJECT_NIL; /* Not used? */
		}

		CORBA_Object_release ((CORBA_Object) subtc, ev);
	}

	g_assert (i == num_members);

	return members;
}

static void
ORBit_imodule_jam_int (IDL_tree src, CORBA_TypeCode tc, gpointer dest)
{
	CORBA_long val = 0;

	switch (IDL_NODE_TYPE (src)) {
	case IDLN_BOOLEAN:
		val = IDL_BOOLEAN (src).value ? 1 : 0;
		break;
	case IDLN_CHAR:
		val = IDL_CHAR (src).value [0];
		break;
	case IDLN_INTEGER:
		val = IDL_INTEGER (src).value;
		break;
	default:
		g_assert_not_reached ();
		break;
	}

	switch (tc->kind) {
	case CORBA_tk_boolean:
	case CORBA_tk_char:
	case CORBA_tk_octet:
		*(CORBA_boolean *) dest = val;
		break;
	case CORBA_tk_short:
	case CORBA_tk_ushort:
		*(CORBA_short *) dest = val;
		break;
	case CORBA_tk_long:
	case CORBA_tk_ulong:
		*(CORBA_long *) dest = val;
		break;
	default:
		g_assert_not_reached ();
		break;
	}
}

static void
ORBit_imodule_setup_label_any (CORBA_TypeCode  tc,
			       IDL_tree        node,
			       CORBA_any      *label)
{
	if (!node) { /* default case */
		label->_type = TC_CORBA_octet;
		label->_value = ORBit_small_alloc (TC_CORBA_octet);
		*(CORBA_octet *) label->_value = -1;
		return;
	}

	label->_type = (CORBA_TypeCode)
				CORBA_Object_duplicate (
					(CORBA_Object) tc, NULL);
	label->_value = ORBit_small_alloc (tc);

	switch (IDL_NODE_TYPE (node)) {
	case IDLN_BOOLEAN:
	case IDLN_CHAR:
	case IDLN_INTEGER:
		ORBit_imodule_jam_int (node, tc, label->_value);
		break;

	case IDLN_FLOAT:
		g_assert (tc->kind == CORBA_tk_float);
		*(CORBA_float *) label->_value = IDL_FLOAT (node).value;
		break;
	case IDLN_BINOP:  /* drop through */
	case IDLN_UNARYOP: {
		IDL_tree val;

		if (IDL_NODE_TYPE (node) == IDLN_BINOP)
			val = _IDL_binop_eval (IDL_BINOP (node).op,
					       IDL_BINOP (node).left,
					       IDL_BINOP (node).right);
		else
			val = _IDL_unaryop_eval (IDL_BINOP (node).op,
					       IDL_UNARYOP (node).operand);

		ORBit_imodule_jam_int (val, tc, label->_value);
		IDL_tree_free (val);
		break;
	}
	case IDLN_IDENT: {
		CORBA_long val;

		g_assert (label->_type->kind == CORBA_tk_enum);
		for (val = 0; val < label->_type->sub_parts; val++)
			if (!strcmp (IDL_IDENT (node).str, label->_type->subnames [val]))
				break;
		g_assert (val < label->_type->sub_parts);
		*(CORBA_long *) label->_value = val;
		break;
	}
	default:
		g_assert_not_reached ();
		break;
	}
}

static CORBA_UnionMemberSeq *
ORBit_imodule_get_union_members (GHashTable        *typecodes,
				 IDL_tree           tree,
				 CORBA_TypeCode     switchtc,
				 CORBA_Environment *ev)
{
	CORBA_UnionMemberSeq *members;
	IDL_tree              l;
	int                   num_members = 0;
	int                   i;

	g_return_val_if_fail (IDL_NODE_TYPE (tree) == IDLN_TYPE_UNION, NULL);

	for (l = IDL_TYPE_UNION (tree).switch_body; l; l = IDL_LIST (l).next)
		num_members += IDL_list_length (IDL_CASE_STMT (IDL_LIST (l).data).labels);

	members = CORBA_UnionMemberSeq__alloc ();

	members->_length  = members->_maximum = num_members;
	members->_buffer  = CORBA_UnionMemberSeq_allocbuf (members->_length);
	members->_release = CORBA_TRUE;

	for (i = 0, l = IDL_TYPE_UNION (tree).switch_body; l; l = IDL_LIST (l).next) {
		CORBA_TypeCode subtc;
		IDL_tree       member, label, dcl;

		member = IDL_CASE_STMT (IDL_LIST (l).data).element_spec;
		g_assert (IDL_NODE_TYPE (member) == IDLN_MEMBER);

		subtc = ORBit_imodule_get_typecode (
				typecodes, IDL_MEMBER (member).type_spec);
		dcl = IDL_LIST (IDL_MEMBER (member).dcls).data;

		for (label = IDL_CASE_STMT (IDL_LIST (l).data).labels; label;
		     label = IDL_LIST (label).next, i++) {
			CORBA_UnionMember *umember = &members->_buffer [i];

			ORBit_imodule_setup_label_any (
				switchtc, IDL_LIST (label).data, &umember->label);

			umember->label._release = CORBA_TRUE;

			umember->name     = CORBA_string_dup (IDL_IDENT (dcl).str);
			umember->type     = (CORBA_TypeCode)
						CORBA_Object_duplicate ((CORBA_Object) subtc, ev);
			umember->type_def = CORBA_OBJECT_NIL; /* Not used? */
		}

		CORBA_Object_release ((CORBA_Object) subtc, ev);
	}

	g_assert (i == num_members);

	return members;
}

static CORBA_EnumMemberSeq *
ORBit_imodule_get_enum_members (IDL_tree           tree,
				CORBA_Environment *ev)
{
	CORBA_EnumMemberSeq *members;
	IDL_tree             l;
	int                  num_members = 0;
	int                  i;

	g_return_val_if_fail (IDL_NODE_TYPE (tree) == IDLN_TYPE_ENUM, NULL);

	num_members = IDL_list_length (IDL_TYPE_ENUM (tree).enumerator_list);

	members = CORBA_EnumMemberSeq__alloc ();

	members->_length  = members->_maximum = num_members;
	members->_buffer  = CORBA_EnumMemberSeq_allocbuf (members->_length);
	members->_release = CORBA_TRUE;

	for (i = 0, l = IDL_TYPE_ENUM (tree).enumerator_list; l; i++, l = IDL_LIST (l).next)
		members->_buffer [i] = CORBA_string_dup (IDL_IDENT (IDL_LIST (l).data).str);

	g_assert (i == num_members);

	return members;
}

static CORBA_TypeCode
ORBit_imodule_lookup_typecode (GHashTable *typecodes,
			       const char *repo_id)
{
	if (!typecodes)
		return CORBA_OBJECT_NIL;

	return (CORBA_TypeCode) CORBA_Object_duplicate (
					g_hash_table_lookup (typecodes, repo_id),
					NULL);
}

static void
ORBit_imodule_register_typecode (GHashTable     *typecodes,
				 const char     *repo_id,
				 CORBA_TypeCode  tc)
{
	g_return_if_fail (g_hash_table_lookup (typecodes, repo_id) == NULL);

	g_hash_table_insert (
		typecodes, g_strdup (repo_id),
		CORBA_Object_duplicate ((CORBA_Object) tc, NULL));
}

GHashTable *
ORBit_imodule_new_typecodes (void)
{
	return g_hash_table_new_full (
			g_str_hash, g_str_equal,
			g_free,
			(GDestroyNotify) CORBA_Object_release);
}

void
ORBit_imodule_free_typecodes (GHashTable *typecodes)
{
	g_hash_table_destroy (typecodes);
}

typedef struct {
	CORBA_sequence_CORBA_TypeCode *sequence;
	int                            iter;
} TypecodesHashIter;

static void
typecodes_hash_foreach (const char        *repo_id,
			CORBA_TypeCode     tc,
			TypecodesHashIter *iter)
{
	g_assert (iter->iter < iter->sequence->_length);

	iter->sequence->_buffer [iter->iter++] =
		(CORBA_TypeCode) CORBA_Object_duplicate ((CORBA_Object) tc, NULL);
}

CORBA_sequence_CORBA_TypeCode *
ORBit_imodule_get_typecodes_seq (GHashTable *typecodes)
{
	CORBA_sequence_CORBA_TypeCode *retval;
	TypecodesHashIter              iter;

	retval           = CORBA_sequence_CORBA_TypeCode__alloc ();
	retval->_length  = retval->_maximum = g_hash_table_size (typecodes);
	retval->_release = TRUE;
	retval->_buffer  = CORBA_sequence_CORBA_TypeCode_allocbuf (retval->_length);

	iter.sequence = retval;
	iter.iter     = 0;

	g_hash_table_foreach (typecodes, (GHFunc) typecodes_hash_foreach, &iter);

	g_assert (iter.iter == retval->_length);

	return retval;
}

IDL_tree
ORBit_imodule_get_typespec (IDL_tree tree)
{
	IDL_tree retval = NULL;

	if (!tree)
		return NULL;

	switch (IDL_NODE_TYPE (tree)) {
	case IDLN_TYPE_INTEGER:
	case IDLN_TYPE_FLOAT:
	case IDLN_TYPE_FIXED:
	case IDLN_TYPE_CHAR:
	case IDLN_TYPE_WIDE_CHAR:
	case IDLN_TYPE_STRING:
	case IDLN_TYPE_WIDE_STRING:
	case IDLN_TYPE_BOOLEAN:
	case IDLN_TYPE_OCTET:
	case IDLN_TYPE_ANY:
	case IDLN_TYPE_OBJECT:
	case IDLN_TYPE_ENUM:
	case IDLN_TYPE_SEQUENCE:
	case IDLN_TYPE_ARRAY:
	case IDLN_TYPE_STRUCT:
	case IDLN_TYPE_UNION:
	case IDLN_EXCEPT_DCL:
	case IDLN_FORWARD_DCL:
	case IDLN_INTERFACE:
	case IDLN_NATIVE:
 	case IDLN_TYPE_TYPECODE:
		retval = tree;
		break;
	case IDLN_TYPE_DCL:
		retval = ORBit_imodule_get_typespec (
				IDL_TYPE_DCL (tree).type_spec);
		break;
	case IDLN_PARAM_DCL:
		retval = ORBit_imodule_get_typespec (
				IDL_PARAM_DCL (tree).param_type_spec);
		break;
	case IDLN_MEMBER:
		retval = ORBit_imodule_get_typespec (
				IDL_MEMBER (tree).type_spec);
		break;
	case IDLN_LIST:
	case IDLN_IDENT:
		retval = ORBit_imodule_get_typespec (
				IDL_get_parent_node (tree, IDLN_ANY, NULL));
		break;
	default:
		g_error ("Cannot get typespec for %s",
				IDL_tree_type_names [IDL_NODE_TYPE (tree)]);
		break;
	}

	return retval;
}

static int
ORBit_imodule_find_c_align (IDL_tree node)
{
	int c_align = 1;

	node = ORBit_imodule_get_typespec (node);	

	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_INTEGER:
		switch (IDL_TYPE_INTEGER (node).f_type) {
		case IDL_INTEGER_TYPE_SHORT:
			c_align = ORBIT_ALIGNOF_CORBA_SHORT;
			break;
		case IDL_INTEGER_TYPE_LONG:
			c_align = ORBIT_ALIGNOF_CORBA_LONG;
			break;
		case IDL_INTEGER_TYPE_LONGLONG:
			c_align = ORBIT_ALIGNOF_CORBA_LONG_LONG;
			break;
		}
		break;
	case IDLN_TYPE_FLOAT:
		switch (IDL_TYPE_FLOAT (node).f_type) {
		case IDL_FLOAT_TYPE_FLOAT:
			c_align = ORBIT_ALIGNOF_CORBA_FLOAT;
			break;
		case IDL_FLOAT_TYPE_DOUBLE:
			c_align = ORBIT_ALIGNOF_CORBA_DOUBLE;
			break;
		case IDL_FLOAT_TYPE_LONGDOUBLE:
			c_align = ORBIT_ALIGNOF_CORBA_LONG_DOUBLE;
			break;
		}
		break;
	case IDLN_TYPE_ENUM:
		c_align = ORBIT_ALIGNOF_CORBA_LONG;
		break;
	case IDLN_TYPE_CHAR: /* drop through */
	case IDLN_TYPE_BOOLEAN:
	case IDLN_TYPE_OCTET:
		c_align = ORBIT_ALIGNOF_CORBA_CHAR;
		break;
	case IDLN_TYPE_WIDE_CHAR:
		c_align = ORBIT_ALIGNOF_CORBA_SHORT;
		break;
	case IDLN_TYPE_UNION: {
		IDL_tree l = IDL_TYPE_UNION (node).switch_body;

		c_align = ORBIT_ALIGNOF_CORBA_STRUCT;

		for (; l; l = IDL_LIST (l).next) {
			IDL_tree subtype = IDL_MEMBER (IDL_CASE_STMT (
					IDL_LIST (l).data).element_spec).type_spec;

			c_align = MAX (c_align, ORBit_imodule_find_c_align (subtype));
		}
		}
		break;
	case IDLN_EXCEPT_DCL: /* drop through */
	case IDLN_TYPE_STRUCT: {
		IDL_tree l = IDL_TYPE_STRUCT (node).member_list;
					
		for (; l; l = IDL_LIST (l).next) {
			IDL_tree member = IDL_MEMBER (IDL_LIST (l).data).type_spec;

			c_align = MAX (c_align, ORBit_imodule_find_c_align (member));
		}
		}
		break;
	case IDLN_TYPE_STRING: /* drop through */
	case IDLN_TYPE_WIDE_STRING:
	case IDLN_TYPE_OBJECT:
	case IDLN_TYPE_TYPECODE:
	case IDLN_FORWARD_DCL:
	case IDLN_INTERFACE:
		c_align = ORBIT_ALIGNOF_CORBA_POINTER;
		break;
	case IDLN_TYPE_ARRAY: {
		IDL_tree subtype;

		subtype = IDL_TYPE_DCL (
				IDL_get_parent_node (
					node, IDLN_TYPE_DCL, NULL)).type_spec;

		c_align = ORBit_imodule_find_c_align (subtype);
		}
		break;
	case IDLN_TYPE_SEQUENCE:
		c_align = MAX (MAX (ORBIT_ALIGNOF_CORBA_STRUCT,
				    ORBIT_ALIGNOF_CORBA_LONG),
				    ORBIT_ALIGNOF_CORBA_POINTER);
		break;
	case IDLN_TYPE_ANY:
		c_align =  MAX (ORBIT_ALIGNOF_CORBA_STRUCT,
			        ORBIT_ALIGNOF_CORBA_POINTER);
		break;
	default:
		g_error ("Can't find alignment %s\n", 
			 IDL_tree_type_names [IDL_NODE_TYPE (node)]);
		break;
	}

	return c_align;
}

CORBA_TypeCode
ORBit_imodule_get_typecode (GHashTable *typecodes,
			    IDL_tree    tree)
{
	CORBA_Environment env;
	CORBA_TypeCode    retval = CORBA_OBJECT_NIL;

	if (!tree)
		return CORBA_OBJECT_NIL;

	CORBA_exception_init (&env);

	switch (IDL_NODE_TYPE (tree)) {
	case IDLN_MEMBER:
		retval = ORBit_imodule_get_typecode (
				typecodes, IDL_MEMBER (tree).type_spec);
		break;
	case IDLN_TYPE_ANY:
		retval = (CORBA_TypeCode)
				CORBA_Object_duplicate (
					(CORBA_Object) TC_CORBA_any, NULL);
		break;
	case IDLN_TYPE_FLOAT:
		switch (IDL_TYPE_FLOAT (tree).f_type) {
		case IDL_FLOAT_TYPE_FLOAT:
			retval = TC_CORBA_float;
			break;
		case IDL_FLOAT_TYPE_DOUBLE:
			retval = TC_CORBA_double;
			break;
		case IDL_FLOAT_TYPE_LONGDOUBLE:
			retval = TC_CORBA_long_double;
			break;
		}
		break;
	case IDLN_TYPE_FIXED:
		retval = CORBA_ORB_create_fixed_tc (NULL, 
				IDL_INTEGER (IDL_TYPE_FIXED (tree).positive_int_const).value,
				IDL_INTEGER (IDL_TYPE_FIXED (tree).integer_lit).value, &env);
		break;
	case IDLN_TYPE_INTEGER:
		if (!IDL_TYPE_INTEGER (tree).f_signed)
			switch (IDL_TYPE_INTEGER (tree).f_type) {
			case IDL_INTEGER_TYPE_SHORT:
				retval = TC_CORBA_unsigned_short;
				break;
			case IDL_INTEGER_TYPE_LONGLONG:
				retval = TC_CORBA_unsigned_long_long;
				break;
			case IDL_INTEGER_TYPE_LONG:
				retval = TC_CORBA_unsigned_long;
				break;
			default:
				g_assert_not_reached ();
			}
		else
			switch (IDL_TYPE_INTEGER (tree).f_type) {
			case IDL_INTEGER_TYPE_SHORT:
				retval = TC_CORBA_short;
				break;
			case IDL_INTEGER_TYPE_LONGLONG:
				retval = TC_CORBA_long_long;
				break;
			case IDL_INTEGER_TYPE_LONG:
				retval = TC_CORBA_long;
				break;
			default:
				g_assert_not_reached ();
			}
		break;
	case IDLN_TYPE_STRING:
		retval = (CORBA_TypeCode)
				CORBA_Object_duplicate (
					(CORBA_Object) TC_CORBA_string, NULL);
		break;
	case IDLN_TYPE_WIDE_STRING:
		retval = (CORBA_TypeCode)
				CORBA_Object_duplicate (
					(CORBA_Object) TC_CORBA_wstring, NULL);
		break;
	case IDLN_TYPE_OCTET:
		retval = (CORBA_TypeCode)
				CORBA_Object_duplicate (
					(CORBA_Object) TC_CORBA_octet, NULL);
		break;
	case IDLN_TYPE_CHAR:
		retval = (CORBA_TypeCode)
				CORBA_Object_duplicate (
					(CORBA_Object) TC_CORBA_char, NULL);
		break;
	case IDLN_TYPE_WIDE_CHAR:
		retval = (CORBA_TypeCode)
				CORBA_Object_duplicate (
					(CORBA_Object) TC_CORBA_wchar, NULL);
		break;
	case IDLN_TYPE_BOOLEAN:
		retval = (CORBA_TypeCode)
				CORBA_Object_duplicate (
					(CORBA_Object) TC_CORBA_boolean, NULL);
		break;
  	case IDLN_TYPE_STRUCT:
		retval = ORBit_imodule_lookup_typecode (
				typecodes,
				IDL_IDENT (IDL_TYPE_STRUCT (tree).ident).repo_id);

		if (!retval) {
			CORBA_StructMemberSeq *members;

			members = ORBit_imodule_get_struct_members (
						typecodes, tree, &env);

			retval = CORBA_ORB_create_struct_tc (NULL,
					IDL_IDENT (IDL_TYPE_STRUCT (tree).ident).repo_id,
					IDL_IDENT (IDL_TYPE_STRUCT (tree).ident).str,
					members, &env);

			ORBit_imodule_register_typecode (
				typecodes,
				IDL_IDENT (IDL_TYPE_STRUCT (tree).ident).repo_id, retval);

			CORBA_free (members);
		}
		break;
	case IDLN_EXCEPT_DCL:
		retval = ORBit_imodule_lookup_typecode (
				typecodes,
				IDL_IDENT (IDL_EXCEPT_DCL (tree).ident).repo_id);

		if (!retval) {
			CORBA_StructMemberSeq *members;

			members = ORBit_imodule_get_struct_members (
						typecodes, tree, &env);

			retval = CORBA_ORB_create_exception_tc (NULL,
					IDL_IDENT (IDL_EXCEPT_DCL (tree).ident).repo_id,
					IDL_IDENT (IDL_EXCEPT_DCL (tree).ident).str,
					members, &env);

			ORBit_imodule_register_typecode (
				typecodes,
				IDL_IDENT (IDL_EXCEPT_DCL (tree).ident).repo_id, retval);

			CORBA_free (members);
		}
		break;
	case IDLN_TYPE_ARRAY: {
		CORBA_TypeCode subtc;
		IDL_tree       sizer;
		IDL_tree       type_dcl;

		sizer = IDL_list_nth (IDL_TYPE_ARRAY (tree).size_list,
				      IDL_list_length (IDL_TYPE_ARRAY (tree).size_list) - 1);
		g_assert (IDL_NODE_TYPE (IDL_LIST (sizer).data) == IDLN_INTEGER);

		type_dcl = IDL_NODE_UP (IDL_NODE_UP (tree));
		g_assert (IDL_NODE_TYPE (type_dcl) == IDLN_TYPE_DCL);

		subtc = ORBit_imodule_get_typecode (
				typecodes, IDL_TYPE_DCL (type_dcl).type_spec),
		retval = CORBA_ORB_create_array_tc (NULL,
				IDL_INTEGER (IDL_LIST (sizer).data).value,
				subtc, &env);
		retval->c_align = subtc->c_align;
		CORBA_Object_release ((CORBA_Object) subtc, NULL);

		for (sizer = IDL_LIST (sizer).prev; sizer; sizer = IDL_LIST (sizer).prev) {
			subtc = retval;
			retval = CORBA_ORB_create_array_tc (NULL,
					IDL_INTEGER (IDL_LIST (sizer).data).value,
					subtc, &env);
			retval->c_align = subtc->c_align;
			CORBA_Object_release ((CORBA_Object) subtc, NULL);
		}

		subtc = retval;
		retval = ORBit_imodule_create_alias_typecode (
				typecodes, IDL_TYPE_ARRAY (tree).ident, subtc);
		CORBA_Object_release ((CORBA_Object) subtc, NULL);
		}
		break;
	case IDLN_TYPE_UNION:
		retval = ORBit_imodule_lookup_typecode (
				typecodes,
				IDL_IDENT (IDL_TYPE_UNION (tree).ident).repo_id);
		
		if (!retval) {
			CORBA_UnionMemberSeq *members;
			CORBA_TypeCode        switchtc;

			switchtc = ORBit_imodule_get_typecode (
					typecodes, IDL_TYPE_UNION (tree).switch_type_spec);

			members = ORBit_imodule_get_union_members (
						typecodes, tree, switchtc, &env);
			retval = CORBA_ORB_create_union_tc (NULL,
					IDL_IDENT (IDL_TYPE_UNION (tree).ident).repo_id,
					IDL_IDENT (IDL_TYPE_UNION (tree).ident).str,
					switchtc, members, &env);

			CORBA_Object_release ((CORBA_Object) switchtc, NULL);

			ORBit_imodule_register_typecode (
				typecodes,
				IDL_IDENT (IDL_TYPE_UNION (tree).ident).repo_id, retval);

			CORBA_free (members);
		}
		break;
	case IDLN_TYPE_ENUM:
		retval = ORBit_imodule_lookup_typecode (
				typecodes,
				IDL_IDENT (IDL_TYPE_ENUM (tree).ident).repo_id);
		if (!retval) {
			CORBA_EnumMemberSeq *members;

			members = ORBit_imodule_get_enum_members (tree, &env);

			retval = CORBA_ORB_create_enum_tc (NULL,
					IDL_IDENT (IDL_TYPE_ENUM (tree).ident).repo_id,
					IDL_IDENT (IDL_TYPE_ENUM (tree).ident).str,
					members, &env);

			ORBit_imodule_register_typecode (
				typecodes,
				IDL_IDENT (IDL_TYPE_ENUM (tree).ident).repo_id, retval);

			CORBA_free (members);
		}
		break;
	case IDLN_IDENT:
		retval = ORBit_imodule_lookup_typecode (
				typecodes, IDL_IDENT (tree).repo_id);
		g_assert (retval != NULL);
		break;
	case IDLN_TYPE_SEQUENCE: {
		CORBA_TypeCode subtc;
		int            bound = 0;

		if (IDL_TYPE_SEQUENCE (tree).positive_int_const)
			bound = IDL_INTEGER (IDL_TYPE_SEQUENCE (tree).positive_int_const).value;

		subtc = ORBit_imodule_get_typecode (
				typecodes, IDL_TYPE_SEQUENCE (tree).simple_type_spec),
		retval = CORBA_ORB_create_sequence_tc (NULL,
				bound, subtc, &env);
		CORBA_Object_release ((CORBA_Object) subtc, NULL);
		/*
		 * FIXME: and what about recursive sequences?
		 */
		}
		break;
	case IDLN_FORWARD_DCL:
	case IDLN_INTERFACE:
		retval = ORBit_imodule_lookup_typecode (
				typecodes,
				IDL_IDENT (IDL_TYPE_ENUM (tree).ident).repo_id);
		if (!retval) {
			retval = CORBA_ORB_create_interface_tc (NULL,
					IDL_IDENT (IDL_TYPE_ENUM (tree).ident).repo_id,
					IDL_IDENT (IDL_TYPE_ENUM (tree).ident).str,
					&env);

			ORBit_imodule_register_typecode (
				typecodes,
				IDL_IDENT (IDL_TYPE_ENUM (tree).ident).repo_id, retval);
		}
		break;
	case IDLN_TYPE_OBJECT:
		retval = (CORBA_TypeCode)
				CORBA_Object_duplicate (
					(CORBA_Object) TC_CORBA_Object, NULL);
		break;
	case IDLN_TYPE_TYPECODE:
		retval = (CORBA_TypeCode)
				CORBA_Object_duplicate (
					(CORBA_Object) TC_CORBA_TypeCode, NULL);
		break;
	default:
		g_error ("We were asked to get a typecode for a %s",
			 IDL_tree_type_names [IDL_NODE_TYPE (tree)]);
		break;
	}

	if (retval && retval->c_align == 0)
		retval->c_align = ORBit_imodule_find_c_align (tree);

	if (env._major != CORBA_NO_EXCEPTION)
		g_warning ("ORBit_imodule_get_typecode: exception %s", env._id);

	CORBA_exception_free (&env);

	return retval;
}

CORBA_TypeCode
ORBit_imodule_create_alias_typecode (GHashTable    *typecodes,
				     IDL_tree       tree,
				     CORBA_TypeCode original_type)
{
	CORBA_Environment env;
	CORBA_TypeCode    retval;

	CORBA_exception_init (&env);

	g_return_val_if_fail (IDL_NODE_TYPE (tree) == IDLN_IDENT, NULL);
	g_return_val_if_fail (g_hash_table_lookup (typecodes,
						   IDL_IDENT (tree).repo_id) == NULL, NULL);

	retval = CORBA_ORB_create_alias_tc (NULL,
			IDL_IDENT (tree).repo_id,
			IDL_IDENT (tree).str,
			original_type, &env);

	ORBit_imodule_register_typecode (
			typecodes, IDL_IDENT (tree).repo_id, retval);

	if (env._major != CORBA_NO_EXCEPTION)
		g_warning ("ORBit_imodule_create_alias_typecode: exception %s", env._id);

	CORBA_exception_free (&env);

	return retval;
}

gboolean
ORBit_imodule_type_is_fixed_length (IDL_tree tree)
{
	gboolean is_fixed = TRUE;
	IDL_tree iter;
	IDL_tree typespec;

	typespec = ORBit_imodule_get_typespec (tree);

	switch (IDL_NODE_TYPE (typespec)) {
	case IDLN_TYPE_FLOAT:
	case IDLN_TYPE_INTEGER:
	case IDLN_TYPE_ENUM:
	case IDLN_TYPE_CHAR:
	case IDLN_TYPE_WIDE_CHAR:
	case IDLN_TYPE_OCTET:
	case IDLN_TYPE_BOOLEAN:
		is_fixed = TRUE;
		break;
	case IDLN_TYPE_SEQUENCE:
	case IDLN_TYPE_STRING:
	case IDLN_TYPE_WIDE_STRING:
	case IDLN_TYPE_OBJECT:
	case IDLN_FORWARD_DCL:
	case IDLN_INTERFACE:
	case IDLN_TYPE_ANY:
	case IDLN_NATIVE:
	case IDLN_TYPE_TYPECODE:
		is_fixed = FALSE;
		break;
	case IDLN_TYPE_UNION:
		for (iter = IDL_TYPE_UNION (typespec).switch_body;
		     iter; iter = IDL_LIST (iter).next)
			is_fixed &= ORBit_imodule_type_is_fixed_length (
					IDL_LIST (IDL_CASE_STMT (
						IDL_LIST (iter).data).element_spec).data);
		break;
	case IDLN_EXCEPT_DCL:
	case IDLN_TYPE_STRUCT:
		for (iter = IDL_TYPE_STRUCT (typespec).member_list;
		     iter; iter = IDL_LIST (iter).next)
			is_fixed &= ORBit_imodule_type_is_fixed_length (IDL_LIST (iter).data);
		break;
	case IDLN_TYPE_ARRAY:
		is_fixed = ORBit_imodule_type_is_fixed_length (
				IDL_TYPE_DCL (IDL_get_parent_node (
						typespec, IDLN_TYPE_DCL, NULL)).type_spec);
		break;
	case IDLN_TYPE_DCL:
		is_fixed = ORBit_imodule_type_is_fixed_length (
				IDL_TYPE_DCL (typespec).type_spec);
		break;
	case IDLN_IDENT:
	case IDLN_LIST:
		is_fixed = ORBit_imodule_type_is_fixed_length (IDL_NODE_UP (typespec));
		break;
	case IDLN_MEMBER:
		is_fixed = ORBit_imodule_type_is_fixed_length (IDL_MEMBER (typespec).type_spec);
		break;
	default:
		g_error ("Cannot determine if type %s is fixed-length",
				IDL_tree_type_names [IDL_NODE_TYPE (typespec)]);
		break;
	}

	return is_fixed;
}

static void
ORBit_imodule_traverse_helper (IDL_tree    tree,
			       GFunc       callback,
			       gpointer    user_data,
			       GHashTable *visited_nodes)
{
	IDL_tree curitem;

	if (g_hash_table_lookup (visited_nodes, tree))
		return;

	g_hash_table_insert (visited_nodes, tree, GINT_TO_POINTER (1));

	for (curitem = IDL_INTERFACE (tree).inheritance_spec; curitem;
	     curitem = IDL_LIST (curitem).next)
		ORBit_imodule_traverse_helper (
				IDL_get_parent_node (IDL_LIST (curitem).data, IDLN_INTERFACE, NULL),
				callback, user_data, visited_nodes);

	callback (tree, user_data);
}

void
ORBit_imodule_traverse_parents (IDL_tree tree,
				GFunc    callback,
				gpointer user_data)
{
	GHashTable *visited_nodes = g_hash_table_new (NULL, g_direct_equal);

	g_return_if_fail (tree != NULL);
	g_return_if_fail (callback != NULL);

	if (IDL_NODE_TYPE (tree) != IDLN_INTERFACE)
		tree = IDL_get_parent_node (tree, IDLN_INTERFACE, NULL);

	if (!tree)
		return;

	ORBit_imodule_traverse_helper (tree, callback, user_data, visited_nodes);

	g_hash_table_destroy (visited_nodes);
}
