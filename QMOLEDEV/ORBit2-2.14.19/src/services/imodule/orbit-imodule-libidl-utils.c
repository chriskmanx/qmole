/*
 * orbit-imodule-libidl-utils.c: cut and paste code from libIDL
 *
 * Copyright (C) 1998 - 2002, Andrew T. Veliath
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
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "orbit-imodule-libidl-utils.h"
		
static IDL_tree
IDL_binop_eval_integer (enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	IDL_tree p = NULL;

	g_assert (IDL_NODE_TYPE (a) == IDLN_INTEGER);

	switch (op) {
	case IDL_BINOP_MULT:
		p = IDL_integer_new (IDL_INTEGER (a).value * IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_DIV:
		if (IDL_INTEGER (b).value == 0) {
			g_error ("Divide by zero in constant expression");
			return NULL;
		}
		p = IDL_integer_new (IDL_INTEGER (a).value / IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_ADD:
		p = IDL_integer_new (IDL_INTEGER (a).value + IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_SUB:
		p = IDL_integer_new (IDL_INTEGER (a).value - IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_MOD:
		if (IDL_INTEGER (b).value == 0) {
			g_error ("Modulo by zero in constant expression");
			return NULL;
		}
		p = IDL_integer_new (IDL_INTEGER (a).value % IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_SHR:
		p = IDL_integer_new (IDL_INTEGER (a).value >> IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_SHL:
		p = IDL_integer_new (IDL_INTEGER (a).value << IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_AND:
		p = IDL_integer_new (IDL_INTEGER (a).value & IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_OR:
		p = IDL_integer_new (IDL_INTEGER (a).value | IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_XOR:
		p = IDL_integer_new (IDL_INTEGER (a).value ^ IDL_INTEGER (b).value);
		break;
	}

	return p;
}

static IDL_tree
IDL_binop_eval_float (enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	IDL_tree p = NULL;

	g_assert (IDL_NODE_TYPE (a) == IDLN_FLOAT);

	switch (op) {
	case IDL_BINOP_MULT:
		p = IDL_float_new (IDL_FLOAT (a).value * IDL_FLOAT (b).value);
		break;

	case IDL_BINOP_DIV:
		if (IDL_FLOAT (b).value == 0.0) {
			g_error ("Divide by zero in constant expression");
			return NULL;
		}
		p = IDL_float_new (IDL_FLOAT (a).value / IDL_FLOAT (b).value);
		break;

	case IDL_BINOP_ADD:
		p = IDL_float_new (IDL_FLOAT (a).value + IDL_FLOAT (b).value);
		break;

	case IDL_BINOP_SUB:
		p = IDL_float_new (IDL_FLOAT (a).value - IDL_FLOAT (b).value);
		break;

	default:
		break;
	}

	return p;
}

IDL_tree
_IDL_binop_eval (enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	g_assert (IDL_NODE_TYPE (a) == IDL_NODE_TYPE (b));

	switch (IDL_NODE_TYPE (a)) {
	case IDLN_INTEGER:
		return IDL_binop_eval_integer (op, a, b);
	case IDLN_FLOAT:
		return IDL_binop_eval_float (op, a, b);
	default:
		return NULL;
	}
}

static IDL_tree
IDL_unaryop_eval_integer (enum IDL_unaryop op, IDL_tree a)
{
	IDL_tree p = NULL;

	g_assert (IDL_NODE_TYPE (a) == IDLN_INTEGER);

	switch (op) {
	case IDL_UNARYOP_PLUS:
		p = IDL_integer_new (IDL_INTEGER (a).value);
		break;

	case IDL_UNARYOP_MINUS:
		p = IDL_integer_new (-IDL_INTEGER (a).value);
		break;

	case IDL_UNARYOP_COMPLEMENT:
		p = IDL_integer_new (~IDL_INTEGER (a).value);
		break;
	}
       
	return p;
}

static IDL_tree
IDL_unaryop_eval_fixed (enum IDL_unaryop op, IDL_tree a)
{
	IDL_tree p = NULL;

	g_assert (IDL_NODE_TYPE (a) == IDLN_FIXED);

	switch (op) {
	case IDL_UNARYOP_PLUS:
		p = IDL_fixed_new (IDL_FIXED (a).value);
		break;

	default:
		break;
	}
       
	return p;
}

static IDL_tree
IDL_unaryop_eval_float (enum IDL_unaryop op, IDL_tree a)
{
	IDL_tree p = NULL;

	g_assert (IDL_NODE_TYPE (a) == IDLN_FLOAT);

	switch (op) {
	case IDL_UNARYOP_PLUS:
		p = IDL_float_new (IDL_FLOAT (a).value);
		break;

	case IDL_UNARYOP_MINUS:
		p = IDL_float_new (-IDL_FLOAT (a).value);
		break;

	default:
		break;
	}
       
	return p;
}

IDL_tree
_IDL_unaryop_eval (enum IDL_unaryop op, IDL_tree a)
{
	switch (IDL_NODE_TYPE (a)) {
	case IDLN_INTEGER:
		return IDL_unaryop_eval_integer (op, a);
	case IDLN_FIXED:
		return IDL_unaryop_eval_fixed (op, a);
	case IDLN_FLOAT:
		return IDL_unaryop_eval_float (op, a);
	default:
		return NULL;
	}
}
