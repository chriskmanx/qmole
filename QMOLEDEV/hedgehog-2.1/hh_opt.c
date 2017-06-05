/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

/* This file implements algebraic optimizations, such as constant
   folding, let hoisting, inlining, etc.

   For the time being the only algebraic optimization implemented is
   constant folding of boolean expressions, but more will hopefully
   come. */

#define HH_COMPILER  1


#include "hh_common.h"
#include "hh_ast.h"


/* Note that the constant folder operates on three-value logic: true,
   false and don't know.  Hence `!hh_expr_is_true(x)' does not imply
   `hh_expr_is_false(x)', and vice versa. */

static int hh_expr_is_true(hh_ast_t *expr)
{
  return expr != NULL
    && ((expr->arity == HH_AST_INTEGER && expr->u.integer != 0)
	|| (expr->arity == HH_AST_UNSIGNED_INTEGER
	    && expr->u.unsigned_integer != 0)
	|| (expr->arity == HH_AST_SYMBOL && expr->u.symbol == hh_symbol_true));
}

static int hh_expr_is_false(hh_ast_t *expr)
{
  return expr != NULL
    && ((expr->arity == HH_AST_INTEGER && expr->u.integer == 0)
	|| (expr->arity == HH_AST_UNSIGNED_INTEGER
	    && expr->u.unsigned_integer == 0)
	|| expr->arity == HH_AST_NIL);
}


static hh_ast_t *hh_rec_fold(hh_ast_t *expr)
{
  int i, j;
  hh_signed_word_t sw;
  hh_ast_t *n;
  int n_true, n_false;

  if (expr == NULL)
    return NULL;

  switch (expr->arity) {
  case HH_AST_NIL:
  case HH_AST_STRING:
  case HH_AST_SYMBOL:
  case HH_AST_INTEGER:
  case HH_AST_UNSIGNED_INTEGER:
    return expr;
    break;
  case 2:
    /* Remove quoting from strings, integers, nil and t. */
    if (expr->u.ast[0]->arity == HH_AST_SYMBOL
	&& expr->u.ast[0]->u.symbol == hh_symbol_quote) {
      if (expr->u.ast[1]->arity == HH_AST_STRING
	  || expr->u.ast[1]->arity == HH_AST_INTEGER
	  || expr->u.ast[1]->arity == HH_AST_UNSIGNED_INTEGER
	  || expr->u.ast[1]->arity == HH_AST_NIL
	  || (expr->u.ast[1]->arity == HH_AST_SYMBOL
	      && expr->u.ast[1]->u.symbol == hh_symbol_true))
	return expr->u.ast[1];
      /* Otherwise return expr itself - we mustn't do any
	 simplifications into anything quoted. */
      return expr;
    }
    /*FALLTHROUGH*/
  default:
    /* Apply `hh_rec_fold' recursively bottom-up. */
    for (i = 0; i < expr->arity; i++)
      expr->u.ast[i] = hh_rec_fold(expr->u.ast[i]);

    if (expr->u.ast[0]->arity == HH_AST_SYMBOL) {
      /* Simplification of boolean if, not, and, or. */

      if (expr->u.ast[0]->u.symbol == hh_symbol_if) {
	if (expr->arity < 3 || expr->arity > 4)
	  hh_fatal(expr, "if expects 2 or 3 arguments");
	if (hh_expr_is_true(expr->u.ast[1]))
	  return expr->u.ast[2];
	if (hh_expr_is_false(expr->u.ast[1])) {
	  if (expr->arity == 4)
	    return expr->u.ast[3];
	  else {
	    n = hh_alloc_node(HH_AST_NIL);
	    hh_ast_copy_location(n, expr);
	    return n;
	  }
	}

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_not) {
	if (expr->arity != 2)
	  hh_fatal(expr, "not expects one argument");
	if (hh_expr_is_true(expr->u.ast[1])) {
	  n = hh_alloc_node(HH_AST_NIL);
	  hh_ast_copy_location(n, expr);
	  return n;
	}
	if (hh_expr_is_false(expr->u.ast[1])) {
	  n = hh_alloc_node(HH_AST_SYMBOL);
	  hh_ast_copy_location(n, expr);
	  n->u.symbol = hh_symbol_true;
	  return n;
	}

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_and) {

	/* Drop out all true subexpressions. */
	n_true = 0;
	n = NULL;
	for (i = 1; i < expr->arity; i++) {
	  if (hh_expr_is_true(expr->u.ast[i])) {
	    n_true++;
	    n = expr->u.ast[i];
	  } else
	    expr->u.ast[i - n_true] = expr->u.ast[i];
	}
	expr->arity -= n_true;
	if (expr->arity == 1) {
	  /* The value of `(and)' is true. */
	  if (n == NULL) {
	    expr->arity = HH_AST_SYMBOL;
	    expr->u.symbol = hh_symbol_true;
	  } else
	    expr = n;
	  return expr;
	}
	/* Check if there's a false.  If one is first, the whole expr
	   is false.  Otherwise cut the expression to the arity of the
	   first false. */
	if (hh_expr_is_false(expr->u.ast[1]))
	  return expr->u.ast[1];
	for (i = 2; i < expr->arity; i++)
	  if (hh_expr_is_false(expr->u.ast[i])) {
	    expr->arity = i + 1;
	    return expr;
	  }

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_or) {

	/* This is essentially a converse of the folding of ands. */
	n_false = 0;
	n = NULL;
	for (i = 1; i < expr->arity; i++) {
	  if (hh_expr_is_false(expr->u.ast[i])) {
	    n_false++;
	    n = expr->u.ast[i];
	  } else
	    expr->u.ast[i - n_false] = expr->u.ast[i];
	}
	expr->arity -= n_false;
	if (expr->arity == 1) {
	  /* The value of `(or)' is false. */
	  if (n == NULL)
	    expr->arity = HH_AST_NIL;
	  else
	    expr = n;
	  return expr;
	}
	/* Check if there's a true, etc...  See the folding of
	   ands. */
	if (hh_expr_is_true(expr->u.ast[1]))
	  return expr->u.ast[1];
	for (i = 2; i < expr->arity; i++)
	  if (hh_expr_is_true(expr->u.ast[i])) {
	    expr->arity = i + 1;
	    return expr;
	  }

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_add) {

	/* Simple constant folding of addition.  It can fold any
	   expression containing only integer constants, e.g.
	   (+ 1 (+ 2 -3)) is folded into zero.  But there are
	   limitations - for example if -3 were replaced by a
	   non-constant expression, nothing could be done to it. */
	sw = 0;
	for (i = j = 1; i < expr->arity; i++) {
	  if (expr->u.ast[i]->arity == HH_AST_INTEGER)
	    sw += expr->u.ast[i]->u.integer;
	  else
	    expr->u.ast[j++] = expr->u.ast[i];
	}
	HH_ASSERT(j <= i);
	if (sw != 0)
	  if (j == 1) {
	    expr->arity = HH_AST_INTEGER;
	    expr->u.integer = sw;
	  } else {
	    expr->u.ast[j] = hh_alloc_node(HH_AST_INTEGER);
	    hh_ast_copy_location(expr->u.ast[j], expr);
	    expr->u.ast[j]->arity = HH_AST_INTEGER;
	    expr->u.ast[j++]->u.integer = sw;
	    expr->arity -= i - j;
	  }
	else {
	  /* sw == 0 */
	  if (j == 2)
	    return expr->u.ast[1];
	  else if (j == 1) {
	    expr->arity = HH_AST_INTEGER;
	    expr->u.integer = 0;
	  } else
	    expr->arity -= i - j;
	}
	return expr;

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_eq) {

	if (expr->arity == 1) {
	make_true:
	  expr->arity = HH_AST_SYMBOL;
	  expr->u.symbol = hh_symbol_true;
	  return expr;
	}

	if (expr->arity == 3
	    && ((expr->u.ast[1]->arity == HH_AST_UNSIGNED_INTEGER
		 && expr->u.ast[2]->arity == HH_AST_UNSIGNED_INTEGER
		 && expr->u.ast[1]->u.unsigned_integer ==
		    expr->u.ast[2]->u.unsigned_integer)
		|| (expr->u.ast[1]->arity == HH_AST_INTEGER
		    && expr->u.ast[2]->arity == HH_AST_INTEGER
		    && expr->u.ast[1]->u.integer ==
		       expr->u.ast[2]->u.integer)))
	  goto make_true;

	if (expr->arity == 3
	    && ((expr->u.ast[1]->arity == HH_AST_UNSIGNED_INTEGER
		 && expr->u.ast[2]->arity == HH_AST_UNSIGNED_INTEGER
		 && expr->u.ast[1]->u.unsigned_integer !=
		    expr->u.ast[2]->u.unsigned_integer)
		|| (expr->u.ast[1]->arity == HH_AST_INTEGER
		    && expr->u.ast[2]->arity == HH_AST_INTEGER
		    && expr->u.ast[1]->u.integer !=
		    expr->u.ast[2]->u.integer))) {
	/* make_false: */
	  expr->arity = HH_AST_SYMBOL;
	  expr->u.symbol = hh_symbol_nil;
	  return expr;
	}

      }
    }
    return expr;
  }
}


hh_ast_t *hh_opt(hh_ast_t *expr)
{
  return hh_rec_fold(expr);
}
