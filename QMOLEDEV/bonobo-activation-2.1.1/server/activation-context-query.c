/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  oafd: OAF CORBA dameon.
 *
 *  Copyright (C) 1999, 2000 Red Hat, Inc.
 *  Copyright (C) 1999, 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this library; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Authors: Elliot Lee <sopwith@redhat.com>,
 *
 */

/*
   Likely bugs: Forgetting to initialize QueryExprConst.needs_free = crash
*/
#include <stdio.h>
#include <string.h>

#include "activation-context-query.h"
#include "activation-context-query-parser.h"

static QueryExpr *
qexp_new (void)
{
	QueryExpr *retval = g_new (QueryExpr, 1);

	retval->have_cached_value = FALSE;
	retval->has_fields = TRUE;

	return retval;
}

void
qexp_free (QueryExpr * qexp)
{
	if (!qexp)
		return;

	switch (qexp->type) {
	case EXPR_FUNCTION:
                g_free (qexp->u.function_value.func_name);
		g_slist_foreach (qexp->u.function_value.arguments,
				 (GFunc) qexp_free, NULL);
		g_slist_free (qexp->u.function_value.arguments);
		break;
	case EXPR_VARIABLE:
		g_free (qexp->u.var_value);
		break;
	case EXPR_ID:
		g_free (qexp->u.id_value);
		break;
	case EXPR_BINOP:
		qexp_free (qexp->u.binop_value.op1);
		qexp_free (qexp->u.binop_value.op2);
		break;
	case EXPR_UNOP:
		qexp_free (qexp->u.unop_value.op);
		break;
	case EXPR_CONSTANT:
		if (qexp->u.constant_value.value_known) {
			switch (qexp->u.constant_value.type) {
			case CONST_STRING:
				g_free (qexp->u.constant_value.u.v_string);
				break;
			case CONST_STRINGV:
				g_strfreev (qexp->u.constant_value.
					    u.v_stringv);
				break;
			default:
				break;
			}
		}
		break;
	}

	g_free (qexp);
}

QueryExpr *
qexp_binop_new (QueryExpr * op1, int operand, QueryExpr * op2)
{
	QueryExpr *retval = qexp_new ();
	int optype;

	switch (operand) {
	case P_MULTIPLY:
		optype = OP_MULTIPLY;
		break;
	case P_DIVIDE:
		optype = OP_DIVIDE;
		break;
	case P_SUBTRACT:
		optype = OP_SUBTRACT;
		break;
	case P_ADD:
		optype = OP_ADD;
		break;
	case P_EQ:
		optype = OP_EQ;
		break;
	case P_NEQ:
		optype = OP_NEQ;
		break;
	case P_LEQ:
		optype = OP_LEQ;
		break;
	case P_GEQ:
		optype = OP_GEQ;
		break;
	case P_GT:
		optype = OP_GT;
		break;
	case P_LT:
		optype = OP_LT;
		break;
	case P_OR:
		optype = OP_OR;
		break;
	case P_AND:
		optype = OP_AND;
		break;
	case P_XOR:
		optype = OP_XOR;
		break;
	default:
		g_assert_not_reached ();
		optype = 0;
		break;
	}

	retval->u.binop_value.type = optype;
	retval->u.binop_value.op1 = op1;
	retval->u.binop_value.op2 = op2;

	retval->type = EXPR_BINOP;
	retval->has_fields = op1->has_fields || op2->has_fields;

	return retval;
}

QueryExpr *
qexp_unop_new (int operand, QueryExpr * op)
{
	QueryExpr *retval = qexp_new ();
	int optype;

	switch (operand) {
	case P_SUBTRACT:
		optype = OP_SUBTRACT;
		break;
	case P_NOT:
		optype = OP_NOT;
		break;
	default:
		g_assert_not_reached ();
		optype = 0;
		break;
	}

	retval->type = EXPR_UNOP;
	retval->u.unop_value.type = optype;
	retval->u.unop_value.op = op;

	retval->has_fields = op->has_fields;

	return retval;
}

QueryExpr *
qexp_function_new (char *name, GSList * exprlist)
{
	GSList *cur;
	QueryExpr *retval = qexp_new ();

	retval->type = EXPR_FUNCTION;
	retval->u.function_value.func_name = name;
	retval->u.function_value.arguments = exprlist;

        cur = exprlist;
        
	while (cur != NULL && !((QueryExpr *) cur->data)->has_fields) {
                cur = cur->next;
        }

	retval->has_fields = cur ? TRUE : FALSE;

	return retval;
}

QueryExpr *
qexp_variable_new (char *name)
{
	QueryExpr *retval = qexp_new ();

	retval->type = EXPR_VARIABLE;
	retval->u.var_value = name;

	retval->has_fields = FALSE;

	return retval;
}

QueryExpr *
qexp_id_new (char *name)
{
	QueryExpr *retval = qexp_new ();

	retval->type = EXPR_ID;
	retval->u.var_value = name;

	retval->has_fields = TRUE;

	return retval;
}

QueryExpr *
qexp_constant_new (QueryExprConst setme)
{
	QueryExpr *retval = qexp_new ();

	retval->type = EXPR_CONSTANT;
	retval->u.constant_value = setme;
	retval->u.constant_value.value_known = TRUE;

	retval->has_fields = FALSE;

	return retval;
}

void
qexp_dump (QueryExpr * exp)
{
	switch (exp->type) {
	case EXPR_FUNCTION:
		{
			GSList *cur;

			g_print ("%s(", exp->u.function_value.func_name);
			for (cur = exp->u.function_value.arguments; cur;
			     cur = cur->next) {
				qexp_dump (cur->data);
				if (cur->next)
					g_print (", ");
			}
			g_print (")");
		}
		break;
	case EXPR_VARIABLE:
		g_print ("$%s", exp->u.var_value);
		break;
	case EXPR_ID:
		g_print ("%s", exp->u.id_value);
		break;
	case EXPR_BINOP:
		{
			char *opc;

			g_print ("(");
			qexp_dump (exp->u.binop_value.op1);
			g_print (")");
			switch (exp->u.binop_value.type) {
			case OP_EQ:
				opc = "=";
				break;
			case OP_NEQ:
				opc = "!=";
				break;
			case OP_LEQ:
				opc = "<=";
				break;
			case OP_GEQ:
				opc = ">=";
				break;
			case OP_LT:
				opc = "<";
				break;
			case OP_GT:
				opc = ">";
				break;
			case OP_OR:
				opc = "||";
				break;
			case OP_AND:
				opc = "&&";
				break;
			case OP_MULTIPLY:
				opc = "*";
				break;
			case OP_DIVIDE:
				opc = "/";
				break;
			case OP_ADD:
				opc = "+";
				break;
			case OP_SUBTRACT:
				opc = "-";
				break;
			case OP_XOR:
				opc = "^";
				break;
			default:
				opc = NULL;
				break;
			}
			g_print (" %s (", opc);
			qexp_dump (exp->u.binop_value.op2);
			g_print (")");
		}
		break;
	case EXPR_UNOP:
		switch (exp->u.unop_value.type) {
		case OP_NOT:
			g_print ("~(");
			break;
		case OP_NEGATE:
			g_print ("-(");
			break;
		}
		qexp_dump (exp->u.unop_value.op);
		g_print (")");
		break;
	case EXPR_CONSTANT:
		qexp_constant_dump (&exp->u.constant_value);
		break;
	default:
		g_error ("Unknown exp type %d", exp->type);
		break;
	}
}

void
qexp_constant_dump (QueryExprConst * c)
{
	if (c->value_known) {
		switch (c->type) {
		case CONST_STRINGV:
			{
				int i;

				g_print ("[");
				for (i = 0; c->u.v_stringv[i]; i++) {
					g_print ("'%s'", c->u.v_stringv[i]);
					if (c->u.v_stringv[i + 1])
						g_print (", ");
				}
				g_print ("]");
			}
			break;
		case CONST_STRING:
			g_print ("'%s'", c->u.v_string);
			break;
		case CONST_NUMBER:
			g_print ("%f", c->u.v_number);
			break;
		case CONST_BOOLEAN:
			g_print ("%s", c->u.v_boolean ? "TRUE" : "FALSE");
			break;
		}
	} else
		g_print ("??");
}

/* Returns a value suitable for use in boolean expressions */
static gboolean
qexp_constant_bool (const QueryExprConst * c)
{
	if (c->value_known)
		switch (c->type) {
		case CONST_BOOLEAN:
			return c->u.v_boolean;
		case CONST_NUMBER:
			return (c->u.v_number != 0.0);
		case CONST_STRING:
			return (c->u.v_string != NULL);
		case CONST_STRINGV:
			return (c->u.v_stringv != NULL);
		}

	return FALSE;
}

gint
qexp_constant_compare (const QueryExprConst * c1, const QueryExprConst * c2)
{
	if (c1->value_known && c2->value_known) {
		g_return_val_if_fail (c1->type == c2->type, 0);

		switch (c1->type) {
		case CONST_STRING:
			return strcmp (c1->u.v_string, c2->u.v_string);
			break;
		case CONST_BOOLEAN:
			if (c1->u.v_boolean && !c2->u.v_boolean)
				return -1;
			else if (c2->u.v_boolean && !c1->u.v_boolean)
				return 1;
			else
				return 0;
			break;
		case CONST_NUMBER:
			{
				if (c2->u.v_number > c1->u.v_number)
					return 1;
				else if (c2->u.v_number < c1->u.v_number)
					return -1;
				else
					return 0;
			}
			break;
		default:
			g_assert_not_reached ();
			break;
		}
	} else if (c1->value_known)
		return -1;
	else if (c2->value_known)
		return 1;

	return 0;
}

#define qexp_constant_unuse(c) if ((c).needs_free && (c).value_known) qexp_constant_free(&(c))
static void
qexp_constant_free (const QueryExprConst * c)
{
	switch (c->type) {
	case CONST_STRING:
		g_free (c->u.v_string);
		break;
	case CONST_STRINGV:
		g_strfreev (c->u.v_stringv);
		break;
	default:
		break;
	}
}


/********************************************* Now the fun stuff *****************************************/

/******* handling functions *********/


typedef QueryExprConst (*QueryExprEvalFunc) (Bonobo_ServerInfo * si,
					     QueryExpr * e,
					     QueryContext * qctx);
/* A table of the functions that can be used in queries */
typedef struct
{
	const char *name;
	QueryExprEvalFunc efunc;
	int min_args, max_args;
}
QueryExprFuncInfo;

static QueryExprConst qexp_func_has_one (Bonobo_ServerInfo * si, QueryExpr * e,
					 QueryContext * qctx);
static QueryExprConst qexp_func_has_all (Bonobo_ServerInfo * si, QueryExpr * e,
					 QueryContext * qctx);
static QueryExprConst qexp_func_has (Bonobo_ServerInfo * si, QueryExpr * e,
				     QueryContext * qctx);

static QueryExprConst qexp_func_prefer_by_list_order (Bonobo_ServerInfo * si, QueryExpr * e,
                                                      QueryContext * qctx);

static QueryExprConst qexp_func_defined (Bonobo_ServerInfo * si, QueryExpr * e,
					 QueryContext * qctx);
static QueryExprConst qexp_func_max (Bonobo_ServerInfo * si, QueryExpr * e,
				     QueryContext * qctx);
static QueryExprConst qexp_func_min (Bonobo_ServerInfo * si, QueryExpr * e,
				     QueryContext * qctx);

static const QueryExprFuncInfo qexp_func_impls[] = {
	{"has_one", qexp_func_has_one, 2},
	{"has_all", qexp_func_has_all, 2},
	{"has", qexp_func_has, 2},
	{"prefer_by_list_order", qexp_func_prefer_by_list_order, 2},
	{"defined", qexp_func_defined, 1},
	{"max", qexp_func_max, 1},
	{"min", qexp_func_min, 1},
	{NULL}
};

static QueryExprConst
qexp_evaluate_function (Bonobo_ServerInfo * si, QueryExpr * e,
			QueryContext * qctx)
{
	QueryExprConst retval;
	const char *func_name;
	int i, n, max;
	const QueryExprFuncInfo *fi;

	func_name = e->u.function_value.func_name;

	for (i = 0; qexp_func_impls[i].name; i++) {
		if (!g_ascii_strcasecmp (func_name, qexp_func_impls[i].name))
			break;
	}

	fi = &qexp_func_impls[i];
	if (!fi->name) {
                retval.value_known = FALSE;
                retval.needs_free = FALSE;
		g_warning ("Invalid function name '%s'", func_name);
		return retval;
	}

	n = g_slist_length (e->u.function_value.arguments);

	max = fi->max_args;
	if (max < fi->min_args)
		max = fi->min_args;
	if ((n < fi->min_args) || (n > max)) {
		g_warning
			("Incorrect argument count to function '%s': got %d, need between %d and %d",
			 func_name, n, fi->min_args, max);
	}

	return fi->efunc (si, e, qctx);

}

static QueryExprConst
qexp_func_has_one (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	QueryExprConst retval, v1, v2;
	int i, j;
	gboolean found;
	char **check_one, **check_two;

	v1 = qexp_evaluate (si, e->u.function_value.arguments->data, qctx);
	v2 =
		qexp_evaluate (si, e->u.function_value.arguments->next->data,
			       qctx);

	retval.value_known = TRUE;

	if (!v1.value_known || !v2.value_known) {
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = FALSE;
	} else if (v1.type != CONST_STRINGV || v2.type != CONST_STRINGV) {
		retval.value_known = FALSE;
	} else {
		found = FALSE;

		check_one = v1.u.v_stringv;
		check_two = v2.u.v_stringv;

		for (i = j = 0; check_one[i]; i++) {
			for (j = 0; check_two[j]; j++) {
				if (!strcmp (check_one[i], check_two[j])) {
					found = TRUE;
					break;
				}
			}
		}

		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = found;
	}

	retval.needs_free = FALSE;
	qexp_constant_unuse (v1);
	qexp_constant_unuse (v2);

	return retval;
}

static QueryExprConst
qexp_func_has_all (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	QueryExprConst retval, v1, v2;
	int i, j;
	char **check_one, **check_two;

	v1 = qexp_evaluate (si, e->u.function_value.arguments->data, qctx);
	v2 =
		qexp_evaluate (si, e->u.function_value.arguments->next->data,
			       qctx);

	retval.value_known = TRUE;

	if (!v1.value_known || !v2.value_known) {
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = FALSE;
	} else if (v1.type != CONST_STRINGV || v2.type != CONST_STRINGV) {
		retval.value_known = FALSE;
	} else {
		check_one = v1.u.v_stringv;
		check_two = v2.u.v_stringv;

		for (i = j = 0; check_two[j] && check_one[i]; j++) {
			for (i = 0; check_one[i]; i++) {
				if (!strcmp (check_two[j], check_one[i]))
					break;
			}
		}

		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = check_one[i] ? TRUE : FALSE;
	}

	retval.needs_free = FALSE;

	qexp_constant_unuse (v1);
	qexp_constant_unuse (v2);

	return retval;
}

static QueryExprConst
qexp_func_has (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	QueryExprConst retval, v1, v2;
	char **check_one, *check_two;
	int i;

	v1 = qexp_evaluate (si, e->u.function_value.arguments->data, qctx);
	v2 =
		qexp_evaluate (si, e->u.function_value.arguments->next->data,
			       qctx);

	retval.value_known = TRUE;

	if (!v1.value_known || !v2.value_known) {
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = FALSE;
	} else if (v1.type != CONST_STRINGV || v2.type != CONST_STRING) {
		retval.value_known = FALSE;
	} else {
		check_one = v1.u.v_stringv;
		check_two = v2.u.v_string;

		for (i = 0; check_one[i]; i++) {
			if (!strcmp (check_one[i], check_two))
				break;
		}

		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = check_one[i] ? TRUE : FALSE;
	}

	retval.needs_free = FALSE;

	qexp_constant_unuse (v1);
	qexp_constant_unuse (v2);

	return retval;
}



static QueryExprConst
qexp_func_prefer_by_list_order (Bonobo_ServerInfo *si, 
                                QueryExpr *e, 
                                QueryContext *qctx)
{
	QueryExprConst retval, item, list;
	char **check_one, *check_two;
	int i;
        int position;

	item = qexp_evaluate (si, e->u.function_value.arguments->data, qctx);
	list = qexp_evaluate (si, e->u.function_value.arguments->next->data, qctx);

	retval.value_known = TRUE;

	if (!item.value_known || !list.value_known) {
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = FALSE;
	} else if (item.type != CONST_STRING || list.type != CONST_STRINGV) {
		retval.value_known = FALSE;
	} else {
                position = -1;
		
                check_one = list.u.v_stringv;
		check_two = item.u.v_string;

		for (i = 0; check_one[i] != NULL; i++) {
			if (position == -1 && 
                            strcmp (check_one[i], check_two) == 0) {
                                position = i;
                        }
		}

                if (position != -1) {
                        position = i - position;
                }

		retval.type = CONST_NUMBER;
		retval.u.v_number = position;
	}

	retval.needs_free = FALSE;

	qexp_constant_unuse (item);
	qexp_constant_unuse (list);

	return retval;
}


static QueryExprConst
qexp_func_defined (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	QueryExprConst retval, v1;

	v1 = qexp_evaluate (si, e->u.function_value.arguments->data, qctx);

	retval.value_known = TRUE;

	retval.type = CONST_BOOLEAN;

	retval.u.v_boolean = v1.value_known ? TRUE : FALSE;

	retval.needs_free = FALSE;

	qexp_constant_unuse (v1);

	return retval;
}


static QueryExprConst
qexp_func_max (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	int i;
	QueryExprConst max_val_so_far;

	max_val_so_far.value_known = FALSE;

	for (i = 0; i < qctx->nservers; i++) {
		QueryExprConst new_val;

		new_val =
			qexp_evaluate (qctx->sil[i],
				       e->u.function_value.arguments->data,
				       qctx);
		if (qexp_constant_compare (&max_val_so_far, &new_val) > 0)
			max_val_so_far = new_val;
	}

	/* The value of this function never changes on a per-record basis, so we never have to revaluate it */
	e->has_fields = FALSE;

	return max_val_so_far;
}

static QueryExprConst
qexp_func_min (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	int i;
	QueryExprConst min_val_so_far;

	min_val_so_far.value_known = FALSE;

	for (i = 0; i < qctx->nservers; i++) {
		QueryExprConst new_val;

		new_val =
			qexp_evaluate (qctx->sil[i],
				       e->u.function_value.arguments->data,
				       qctx);
		if (qexp_constant_compare (&min_val_so_far, &new_val) > 0)
			min_val_so_far = new_val;
	}

	/* see comment in qexp_func_max */
	e->has_fields = FALSE;

	return min_val_so_far;
}

/********** Variables *******/

static QueryExprConst
qexp_evaluate_variable (Bonobo_ServerInfo * si, QueryExpr * e,
			QueryContext * qctx)
{
	QueryExprConst retval;

	retval.value_known = FALSE;

	if (qctx->cctx) {
		CORBA_Environment ev;
		CORBA_NVList nvout;

		CORBA_exception_init (&ev);
		CORBA_Context_get_values (qctx->cctx, NULL, 0, e->u.var_value,
					  &nvout, &ev);

		/* FIXME bugzilla.eazel.com 2732: non-standard - I
                 * screwed up the NVList implementation in ORBit */

		if (ev._major == CORBA_NO_EXCEPTION) {
			if (nvout->list->len > 0) {
				CORBA_NamedValue *nv;
				retval.value_known = TRUE;
				retval.type = CONST_STRING;
				nv =
					&g_array_index (nvout->list,
							CORBA_NamedValue, 0);
				retval.u.v_string =
					g_strdup (*(char **) nv->
						  argument._value);
				retval.needs_free = TRUE;
			} else
				g_warning ("Unknown variable %s",
					   e->u.var_value);

			CORBA_NVList_free (nvout, &ev);
		} else
			g_warning ("Unknown variable %s", e->u.var_value);

		CORBA_exception_free (&ev);
	} else
		g_warning ("Unknown variable %s", e->u.var_value);

	return retval;
}

/********* fields ***********/
static QueryExprConst
qexp_evaluate_id (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	QueryExprConst retval;

	retval.value_known = retval.needs_free = FALSE;

	if (si) {
		retval.value_known = TRUE;
		retval.type = CONST_STRING;
		if (!g_ascii_strcasecmp (e->u.id_value, "location_info"))
			retval.u.v_string = si->location_info;
		else if (!g_ascii_strcasecmp (e->u.id_value, "server_type"))
			retval.u.v_string = si->server_type;
		else if (!g_ascii_strcasecmp (e->u.id_value, "iid"))
			retval.u.v_string = si->iid;
		else if (!g_ascii_strcasecmp (e->u.id_value, "username"))
			retval.u.v_string = si->username;
		else if (!g_ascii_strcasecmp (e->u.id_value, "hostname"))
			retval.u.v_string = si->hostname;
		else {
			int i;
			for (i = 0; i < si->props._length; i++) {
				if (!strcmp
				    (e->u.id_value,
				     si->props._buffer[i].name)) break;
			}

			retval.value_known = FALSE;

			if (i < si->props._length) {
				Bonobo_ActivationPropertyValue *av;

				av = &si->props._buffer[i].v;

				switch (av->_d) {
				case Bonobo_ACTIVATION_P_STRING:
					retval.type = CONST_STRING;
					retval.u.v_string =
						av->_u.value_string;
					break;
				case Bonobo_ACTIVATION_P_NUMBER:
					retval.type = CONST_NUMBER;
					retval.u.v_number =
						av->_u.value_number;
					break;
				case Bonobo_ACTIVATION_P_BOOLEAN:
					retval.type = CONST_BOOLEAN;
					retval.u.v_boolean =
						av->_u.value_boolean;
					break;
				case Bonobo_ACTIVATION_P_STRINGV:
					{
						/* FIXME bugzilla.eazel.com 2729: it would be nice to replace the
						 * NULL-terminated string arrays with
						 * CORBA_sequence_string all over 
                                                 */

						int i;
						retval.type = CONST_STRINGV;

						retval.needs_free = TRUE; 

						retval.u.v_stringv =
							g_malloc (sizeof
								  (char *) *
								  (av->
								   _u.value_stringv._length
								   + 1));
						for (i = 0;
						     i <
						     av->_u.
						     value_stringv._length;
						     i++) retval.
								u.v_stringv[i]
								=
								g_strdup
								(av->_u.value_stringv._buffer
								 [i]);
						retval.u.v_stringv[i] = NULL;
					}
					break;
				}

				retval.value_known = TRUE;
			} else if (qctx->id_evaluator)
				retval =
					qctx->id_evaluator (si, e->u.id_value,
							    qctx);
		}
	}

	return retval;
}

/********* binary operators *********/

static QueryExprConst
qexp_evaluate_binop (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	QueryExprConst retval, v1, v2;
	gboolean negate_result = FALSE;

	v2.value_known = FALSE;	/* To make sure that qexp_constant_unuse works properly if we short-circuit */

	retval.value_known = TRUE;
	retval.needs_free = FALSE;

	v1 = qexp_evaluate (si, e->u.binop_value.op1, qctx);

	/* Perform short-circuiting */
	switch (e->u.binop_value.type) {
	case OP_OR:
		if (v1.value_known && qexp_constant_bool (&v1)) {
			retval.type = CONST_BOOLEAN;
			retval.u.v_boolean = TRUE;
			qexp_constant_unuse (v1);
			return retval;
		}
		break;
	case OP_AND:
		if (v1.value_known && !qexp_constant_bool (&v1)) {
			retval.type = CONST_BOOLEAN;
			retval.u.v_boolean = FALSE;
			qexp_constant_unuse (v1);
			return retval;
		}
		break;
	default:
		break;
	}

	v2 = qexp_evaluate (si, e->u.binop_value.op2, qctx);

	retval.value_known = TRUE;

	switch (e->u.binop_value.type) {
	case OP_NEQ:
		negate_result = TRUE;
	case OP_EQ:
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = qexp_constant_compare (&v1, &v2) == 0;
		break;
	case OP_GEQ:
		negate_result = TRUE;
	case OP_LT:
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = qexp_constant_compare (&v1, &v2) < 0;
		break;
	case OP_LEQ:
		negate_result = TRUE;
	case OP_GT:
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = qexp_constant_compare (&v1, &v2) > 0;
		break;
	case OP_OR:
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = qexp_constant_bool (&v1)
			|| qexp_constant_bool (&v2);
		break;
	case OP_AND:
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = qexp_constant_bool (&v1)
			&& qexp_constant_bool (&v2);
		break;
	case OP_MULTIPLY:
		if (v1.type != CONST_NUMBER || v2.type != CONST_NUMBER)
			retval.value_known = FALSE;
		else {
			retval.type = CONST_NUMBER;
			retval.u.v_number = v1.u.v_number * v2.u.v_number;
		}
		break;
	case OP_DIVIDE:
		if (v1.type != CONST_NUMBER || v2.type != CONST_NUMBER)
			retval.value_known = FALSE;
		else {
			retval.type = CONST_NUMBER;
			if (v2.u.v_number == 0.0)
				retval.value_known = FALSE;
			else
				retval.u.v_number =
					v1.u.v_number / v2.u.v_number;
		}
		break;
	case OP_ADD:
		if (v1.type != CONST_NUMBER || v2.type != CONST_NUMBER)
			retval.value_known = FALSE;
		else {
			retval.type = CONST_NUMBER;
			retval.u.v_number = v1.u.v_number + v2.u.v_number;
		}
		break;
	case OP_SUBTRACT:
		if (v1.type != CONST_NUMBER || v2.type != CONST_NUMBER)
			retval.value_known = FALSE;
		else {
			retval.type = CONST_NUMBER;
			retval.u.v_number = v1.u.v_number - v2.u.v_number;
		}
		break;
	case OP_XOR:
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean =
			qexp_constant_bool (&v1) ^ qexp_constant_bool (&v2);
		break;
	}

	if (negate_result)
		retval.u.v_boolean = !retval.u.v_boolean;

	qexp_constant_unuse (v1);
	qexp_constant_unuse (v2);

	return retval;
}

/********** unary operators **********/
static QueryExprConst
qexp_evaluate_unop (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	QueryExprConst retval, v1;

	retval.value_known = TRUE;

	v1 = qexp_evaluate (si, e->u.unop_value.op, qctx);
	switch (e->u.unop_value.type) {
	case OP_NOT:
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean = !qexp_constant_bool (&v1);
		break;
	case OP_NEGATE:
		if (v1.type != CONST_NUMBER)
			retval.value_known = FALSE;
		else {
			retval.type = CONST_NUMBER;
			retval.u.v_number = -(v1.u.v_number);
		}
		break;
	}

	qexp_constant_unuse (v1);

	return retval;
}

/********** constants ************/
static QueryExprConst
qexp_evaluate_constant (Bonobo_ServerInfo * si, QueryExpr * e,
			QueryContext * qctx)
{
	return e->u.constant_value;
}

/***** The grand poobah of functions *****/
QueryExprConst
qexp_evaluate (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	QueryExprConst retval;

	if (e->have_cached_value) {
		retval = e->cached_value;
	} else {
		switch (e->type) {
		case EXPR_FUNCTION:
			retval = qexp_evaluate_function (si, e, qctx);
			break;
		case EXPR_VARIABLE:
			retval = qexp_evaluate_variable (si, e, qctx);
			break;
		case EXPR_ID:
			retval = qexp_evaluate_id (si, e, qctx);
			break;
		case EXPR_BINOP:
			retval = qexp_evaluate_binop (si, e, qctx);
			break;
		case EXPR_UNOP:
			retval = qexp_evaluate_unop (si, e, qctx);
			break;
		case EXPR_CONSTANT:
			retval = qexp_evaluate_constant (si, e, qctx);
			break;
		}
	}

	if (!e->has_fields) {
		e->cached_value = retval;
		e->have_cached_value = TRUE;
		retval.needs_free = FALSE;	/* We don't want people freeing our cached value... */
	}

	return retval;
}

gboolean
qexp_matches (Bonobo_ServerInfo * si, QueryExpr * e, QueryContext * qctx)
{
	QueryExprConst res;
	gboolean retval;

	res = qexp_evaluate (si, e, qctx);

	retval = qexp_constant_bool (&res);

	qexp_constant_unuse (res);

	return retval;
}

typedef struct
{
	QueryExpr **sexps;
	int nexps;
	QueryContext *qctx;
}
QexpSortData;


static int
qexp_sort_compare (gconstpointer a, gconstpointer b, gpointer user_data)
{
	Bonobo_ServerInfo *x = * (Bonobo_ServerInfo **) a;
	Bonobo_ServerInfo *y = * (Bonobo_ServerInfo **) b;
	QexpSortData * sort_data = user_data;
	int i;

	if (x == NULL) {
		return 1;
	}

	if (y == NULL) {
		return -1;
	}

	for (i = 0; i < sort_data->nexps; i++) {
		QueryExprConst cx, cy;
		int res;

		cx = qexp_evaluate (x, sort_data->sexps[i], sort_data->qctx);
		cy = qexp_evaluate (y, sort_data->sexps[i], sort_data->qctx);

		res = qexp_constant_compare (&cx, &cy);

		qexp_constant_unuse (cx);
		qexp_constant_unuse (cy);

		if (res > 0)
			return 1;
		else if (res < 0)
			return -1;
	}

	return 0;
}

void
qexp_sort (Bonobo_ServerInfo ** servers, int nservers, QueryExpr ** sexps,
	   int nexps, QueryContext * qctx)
{
	QexpSortData sort_data;

	sort_data.sexps = sexps;
	sort_data.nexps = nexps;
	sort_data.qctx = qctx;

	g_qsort_with_data (servers, nservers, sizeof (Bonobo_ServerInfo *),
	                   qexp_sort_compare, &sort_data);
}
