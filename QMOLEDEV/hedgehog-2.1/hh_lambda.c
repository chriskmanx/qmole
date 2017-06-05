/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

/* This file lifts lambdas (`fn's and `def's inside other `def's to
   global functions and adds code that generates environments for them
   in the place where they are instantiated. */

#define HH_COMPILER  1

#include "hh_common.h"
#include "hh_ast.h"


/* How many variables may a closure contain at most?  This is limited
   by the maximum width of the tuple implementing closures. */

#define HH_MAX_ENV_SIZE  15


/* A description of the symbols in a closure. */

typedef hh_symbol_t *hh_env_t[HH_MAX_ENV_SIZE];


/* The linked list of `hh_lifted_t's make a stack used to handle
   nested `fn's in `fn's in `fn's... */

typedef struct hh_lifted_t {
  hh_ast_t *expr;
  hh_env_t env;
  struct hh_lifted_t *next;
} hh_lifted_t;

static hh_lifted_t *hh_lifted = NULL;


static hh_lifted_t *hh_push_lifted(hh_ast_t *expr)
{
  hh_lifted_t *l;

  l = malloc(sizeof(hh_lifted_t));
  if (l == NULL)
    hh_fatal(expr, "Malloc failed during lambda-lifting");
  l->expr = expr;
  memset(l->env, 0, sizeof(l->env));
  l->next = hh_lifted;
  hh_lifted = l;
  return l;
}


/* Some machinery to keep track of symbols in the scope.  No global
   symbols are kept here, since their placement is needed in code
   generation, not yet in lambda lifting. */

static hh_symbol_t *hh_scope[1024];
static int hh_scope_ix = 0;
static int hh_n_env_syms_in_scope = 0;


/* Put into `env' all those symbols which are referred from `expr'.
   Note that `expr's may define new local variables of parameters
   which may shadow references to outside `expr'.  `hh_new_locals' is
   used to help related bookkeeping. */

static void hh_lambda_scan(hh_ast_t *expr,
			   hh_env_t env,
			   int hh_new_locals)
{
  hh_ast_t *x, *list;
  int hh_scope_ix_bup, i;

  if (expr == NULL)
    return;

  switch (expr->arity) {
  case HH_AST_NIL:
  case HH_AST_INTEGER:
  case HH_AST_UNSIGNED_INTEGER:
  case HH_AST_STRING:
    return;

  case HH_AST_SYMBOL:
    /* Check where the symbol is defined.  If it is defined in the
       expression currently being recursively scanned, then do
       nothing.  If it is defined outside of it, then register it to
       be included in the environment.  If the symbol isn't defined
       there either, then it is assumed to be global (to be checked in
       code generation), and we don't do anything with it. */
    for (i = hh_scope_ix - 1; i >= hh_new_locals; i--)
      if (hh_scope[i] == expr->u.symbol)
	/* Found in newest scope. */
	return;
    for (i = hh_new_locals - 1; i >= 0; i--)
      if (hh_scope[i] == expr->u.symbol) {
	/* Found in surrounding environment. */
	for (i = 0; i < HH_MAX_ENV_SIZE; i++)
	  if (env[i] == expr->u.symbol)
	    /* The symbol is already in the environment requirements,
	       nothing to do. */
	    return;
	for (i = 0; i < HH_MAX_ENV_SIZE; i++)
	  if (env[i] == NULL) {
	    /* Found an empty slot in the env, put the requirement
	       here. */
	    env[i] = expr->u.symbol;
	    return;
	  }
	/* Environment full.  Barf. */
	hh_fatal(expr, "Too many variables required to be in the closure");
	HH_NOTREACHED;
      }
    /* Not found. */
    return;
    
  default:
    if (expr->u.ast[0]->arity == HH_AST_SYMBOL) {
      if (expr->u.ast[0]->u.symbol == hh_symbol_fn) {
	hh_scope_ix_bup = hh_scope_ix;

	/* Push argument list to scope. */
	if (expr->arity != 3)
	  hh_fatal(expr, "Incorrect `fn'");
	if (expr->u.ast[1]->arity > HH_AST_ATOMS_START)
	  hh_fatal(expr, "Incorrect `fn'");
	for (i = 0; i < expr->u.ast[1]->arity; i++) {
	  x = expr->u.ast[1]->u.ast[i];
	  if (x->arity != HH_AST_SYMBOL) {
	    /* Check for ellipsis. */
	    if (i == expr->u.ast[1]->arity - 1
		&& x->arity == 2
		&& x->u.ast[0]->arity == HH_AST_SYMBOL
		&& x->u.ast[0]->u.symbol == hh_symbol_ellipsis
		&& x->u.ast[1]->arity == HH_AST_SYMBOL)
	      x = x->u.ast[1];
	    else
	      hh_fatal(x, "Parameter name not a symbol");
	  }
	  hh_scope[hh_scope_ix++] = x->u.symbol;
	}

	expr = expr->u.ast[2];
      scan_list:
	/* Push local `def's in the list to the scope. */
	list = expr;
	while (list != NULL) {
	  HH_ASSERT(list->arity == 2);
	  x = list->u.ast[0];
	  if (x->arity >= 2
	      && x->arity < HH_AST_ATOMS_START
	      && x->u.ast[0]->arity == HH_AST_SYMBOL
	      && x->u.ast[0]->u.symbol == hh_symbol_def
	      && x->u.ast[1]->arity >= 1
	      && x->u.ast[1]->arity < HH_AST_ATOMS_START
	      && x->u.ast[1]->u.ast[0]->arity == HH_AST_SYMBOL)
	    hh_scope[hh_scope_ix++] = x->u.ast[1]->u.ast[0]->u.symbol;
	  list = list->u.ast[1];
	}

	/* Traverse the list again, this time calling `hh_lambda_scan'
	   recursively for all expressions in the list. */
	list = expr;
	while (list != NULL) {
	  HH_ASSERT(list->arity == 2);
	  x = list->u.ast[0];

	  hh_lambda_scan(x, env, hh_new_locals);

	  /* Check if `x' is a `set' of a local variable. */
	  if (x->arity == 3
	      && x->u.ast[0]->arity == HH_AST_SYMBOL
	      && x->u.ast[0]->u.symbol == hh_symbol_set
	      && x->u.ast[1]->arity == HH_AST_SYMBOL)
	    hh_scope[hh_scope_ix++] = x->u.ast[1]->u.symbol;

	  list = list->u.ast[1];
	}
	
	hh_scope_ix = hh_scope_ix_bup;
	return;

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_def) {

	hh_scope_ix_bup = hh_scope_ix;

	/* Push argument list to scope. */
	if (expr->arity != 3)
	  hh_fatal(expr, "Incorrect `def'");
	if (expr->u.ast[1]->arity > HH_AST_ATOMS_START)
	  hh_fatal(expr, "Incorrect `def'");
	for (i = 1; i < expr->u.ast[1]->arity; i++) {
	  x = expr->u.ast[1]->u.ast[i];
	  if (x->arity != HH_AST_SYMBOL) {
	    /* Check for ellipsis. */
	    if (i == expr->u.ast[1]->arity - 1
		&& x->arity == 2
		&& x->u.ast[0]->arity == HH_AST_SYMBOL
		&& x->u.ast[0]->u.symbol == hh_symbol_ellipsis
		&& x->u.ast[1]->arity == HH_AST_SYMBOL)
	      x = x->u.ast[1];
	    else
	      hh_fatal(x, "Parameter name not a symbol");
	  }
	  hh_scope[hh_scope_ix++] = x->u.symbol;
	}

	expr = expr->u.ast[2];
	goto scan_list;

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_do) {

	hh_scope_ix_bup = hh_scope_ix;

	if (expr->arity != 2)
	  hh_fatal(expr, "Malformed `do' body");
	expr = expr->u.ast[1];
	goto scan_list;

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_quote
		 || expr->u.ast[0]->u.symbol == hh_symbol_throw) {

	/* None of the symbols in the quoted form need to be included
	   in the closure, neither the catch tag. */
	return;

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_catch) {

	if (expr->arity != 4)
	  hh_fatal(expr, "`catch' expects exactly three arguments: "
		   "protected body, a symbol that represents the catch tag, "
		   "and the catching body");
	hh_lambda_scan(expr->u.ast[1], env, hh_new_locals);
	hh_lambda_scan(expr->u.ast[3], env, hh_new_locals);
	return;

      }
    }

    for (i = 0; i < expr->arity; i++)
      hh_lambda_scan(expr->u.ast[i], env, hh_new_locals);
    return;
  }
}


static hh_ast_t *hh_lambda_lift(hh_ast_t *expr); /* Forward decl. */

/* A utility function.  It is used to create an abstract syntax tree
   which creates the environment containing the `n_vars' values of
   symbols in `env'.  `expr' is passed in only as a place where to
   copy location data from (in order to allow descriptive error
   messages.  The created AST is essentially of the form
	(do (.make_new_env j)
	    (.put_new_env 0 x1)
	    (.put_new_env 1 x2)
	    ...)
 */

static hh_ast_t *hh_make_env(hh_ast_t *expr, int n_vars, hh_env_t env)
{
  hh_ast_t *x, *res;
  int i;

  HH_ASSERT(n_vars > 0);	/* Should be checked in the caller. */

  res = hh_alloc_node(2);
  hh_ast_copy_location(res, expr);
  res->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
  hh_ast_copy_location(res->u.ast[0], expr);
  res->u.ast[0]->u.symbol = hh_symbol_do;

  x = res->u.ast[1] = hh_alloc_node(2);
  hh_ast_copy_location(x, expr);

  /* Make the `(.make_new_env n_vars)' to `x->u.ast[0]'. */
  x->u.ast[0] = hh_alloc_node(2);
  hh_ast_copy_location(x->u.ast[0], expr);
  x->u.ast[0]->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
  hh_ast_copy_location(x->u.ast[0]->u.ast[0], expr);
  x->u.ast[0]->u.ast[0]->u.symbol = hh_symbol_make_new_env;
  x->u.ast[0]->u.ast[1] = hh_alloc_node(HH_AST_INTEGER);
  hh_ast_copy_location(x->u.ast[0]->u.ast[1], expr);
  /* Can't make 1-element tuples, so must the environment must have
     one excess value if there's only one value in it.  The excess
     slot should not have any effect.  HH_IMM_make_new_env initializes
     it to HH_NIL in any case. */
  x->u.ast[0]->u.ast[1]->u.integer = n_vars <= 1 ? 2 : n_vars;
  
  for (i = 0; i < HH_MAX_ENV_SIZE; i++)
    if (env[i] != NULL) {
      /* Make a cons to `x->u.ast[1]', move to that cons. */
      x->u.ast[1] = hh_alloc_node(2);
      x = x->u.ast[1];
      hh_ast_copy_location(x, expr);
      x->u.ast[1] = NULL;	/* The terminating NULL in case this
				   is the last one. */

      /* Make the `(.put_new_env ix value)' to `x->u.ast[0]'. */
      x->u.ast[0] = hh_alloc_node(3);
      hh_ast_copy_location(x->u.ast[0], expr);

      x->u.ast[0]->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
      hh_ast_copy_location(x->u.ast[0]->u.ast[0], expr);
      x->u.ast[0]->u.ast[0]->u.symbol = hh_symbol_put_new_env;

      x->u.ast[0]->u.ast[1] = hh_alloc_node(HH_AST_INTEGER);
      hh_ast_copy_location(x->u.ast[0]->u.ast[1], expr);
      x->u.ast[0]->u.ast[1]->u.integer = i;

      x->u.ast[0]->u.ast[2] = hh_alloc_node(HH_AST_SYMBOL);
      hh_ast_copy_location(x->u.ast[0]->u.ast[2], expr);
      x->u.ast[0]->u.ast[2]->u.symbol = env[i];

      /* Apply `hh_lambda_lift' recursively to the symbol part.
	 This is necessary for deeply nested lambdas, like
	   (do (set x 1)
	       (fn () (fn () x)))
       */
      x->u.ast[0]->u.ast[2] = hh_lambda_lift(x->u.ast[0]->u.ast[2]);

      n_vars--;
    }
  HH_ASSERT(n_vars == 0);

  return res;
}


/* This function rewrites the given expression so that any reference
   to the `hh_n_env_syms_in_scope' first variables in `hh_scope' is
   replaced by a `(.get_env ix)' from the corresponding index in the
   closure.

   If the rewrite procedure encounters any `fn's or `defs', they will
   be lifted to top level by pushing them to the `hh_lifted'-stack.
 */

static hh_ast_t *hh_lambda_lift(hh_ast_t *expr)
{
  hh_ast_t *x, *y, *list, *res, **resp;
  hh_symbol_t *sym;
  char buf[20];
  static unsigned int n_lambdas = 0;
  hh_lifted_t *lift;
  int i, j, hh_scope_ix_bup;

  /* XXX This should vanish, but it gives some leeway during
     development. */
  if (expr == NULL)
    return expr;

  switch (expr->arity) {
  case HH_AST_NIL:
  case HH_AST_INTEGER:
  case HH_AST_UNSIGNED_INTEGER:
  case HH_AST_STRING:
    return expr;

  case HH_AST_SYMBOL:
    /* Check if the symbol is defined to be in the environment.  If
       so, replace it with relevant environment reading expression. */
    for (i = hh_scope_ix - 1; i >= hh_n_env_syms_in_scope; i--)
      if (hh_scope[i] == expr->u.symbol)
	return expr;
    for (i = hh_n_env_syms_in_scope - 1; i >= 0; i--)
      if (hh_scope[i] == expr->u.symbol) {
	x = hh_alloc_node(2);
	hh_ast_copy_location(x, expr);
	x->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
	hh_ast_copy_location(x->u.ast[0], expr);
	x->u.ast[0]->u.symbol = hh_symbol_get_env;
	x->u.ast[1] = hh_alloc_node(HH_AST_INTEGER);
	hh_ast_copy_location(x->u.ast[1], expr);
	x->u.ast[1]->u.integer = i;
	return x;
      }
    /* Otherwise it is a global or some builtin.  Nothing needs to be
       done for it. */
    return expr;

  default:
    if (expr->u.ast[0]->arity == HH_AST_SYMBOL) {

      if (expr->u.ast[0]->u.symbol == hh_symbol_fn) {
	/* `expr' is a lambda function instantiation. */

	if (expr->arity != 3)
	  hh_fatal(expr, "`fn' corrupted");
	if (expr->u.ast[1]->arity > HH_AST_ATOMS_START)
	  hh_fatal(expr, "`fn' expected argument list");

	/* Create a new symbol. */
	sprintf(buf, "$.%d", n_lambdas++);
	sym = hh_ast_symbol(buf);

	/* Create a `def' of the lambda. */
	x = hh_alloc_node(expr->arity);
	hh_ast_copy_location(x, expr);
	x->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
	hh_ast_copy_location(x->u.ast[0], expr);
	x->u.ast[0]->u.symbol = hh_symbol_def;
	x->u.ast[1] = hh_alloc_node(expr->u.ast[1]->arity + 1);
	hh_ast_copy_location(x->u.ast[1], expr);
	x->u.ast[1]->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
	hh_ast_copy_location(x->u.ast[1]->u.ast[0], expr);
	x->u.ast[1]->u.ast[0]->u.symbol = sym;
	for (i = 0; i < expr->u.ast[1]->arity; i++)
	  x->u.ast[1]->u.ast[i + 1] = expr->u.ast[1]->u.ast[i];
	for (i = 2; i < expr->arity; i++)
	  x->u.ast[i] = expr->u.ast[i];

	/* Push the `def'd `fn' to the top-level. */
	lift = hh_push_lifted(x);

	/* Make `hh_lifted->env' contain the variables that the lambda
	   function refers to. */
	hh_lambda_scan(expr, lift->env, hh_scope_ix);

	for (j = i = 0; i < HH_MAX_ENV_SIZE; i++)
	  if (lift->env[i] != NULL)
	    j++;
	/* Now `j' has the closure size. */
	if (j == 0) {
	  /* Make `expr' a reference to the newly lifted function. */
	  expr->arity = HH_AST_SYMBOL;
	  expr->u.symbol = sym;
	  return expr;
	} else {
	  /* Create the equivalent of
	     (do (do (.make_new_env j)
	             (.put_new_env 0 x1)
		     (.put_new_env 1 x2)
		     ...)
		 (.bind_env sym))
	   */
	  hh_ast_t *y;		/* Helper variable. */

	  /* Make uppermost `do'. */
	  x = hh_alloc_node(2);
	  hh_ast_copy_location(x, expr);
	  x->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
	  hh_ast_copy_location(x->u.ast[0], expr);
	  x->u.ast[0]->u.symbol = hh_symbol_do;
	  /* Use `hh_make_env' to construct the code for creating the
	     new environment. */
	  x->u.ast[1] = hh_alloc_node(2);
	  x->u.ast[1]->u.ast[0] = hh_make_env(expr, j, lift->env);
	  /* Make the `(.bind_env sym)'. */
	  x->u.ast[1]->u.ast[1] = y = hh_alloc_node(2);
	  hh_ast_copy_location(y, expr);
	  y->u.ast[1] = NULL;	/* Terminating NULL for top-level do. */
	  y->u.ast[0] = hh_alloc_node(2);
	  y = y->u.ast[0];
	  hh_ast_copy_location(y, expr);
	  y->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
	  hh_ast_copy_location(y->u.ast[0], expr);
	  y->u.ast[0]->u.symbol = hh_symbol_bind_env;
	  y->u.ast[1] = hh_alloc_node(HH_AST_SYMBOL);
	  hh_ast_copy_location(y->u.ast[1], expr);
	  y->u.ast[1]->u.symbol = sym;

	  return x;
	}

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_def) {

	/* If we come here, then we have a `def' in a place where the
	   expression's value has some meaning.  since `def's return
	   value is undefined, barf. */
	hh_fatal(expr, "`def' encountered in a position where "
		 "an expression value is expected - `def' returns none");

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_do) {
	
	/* `do'-bodies handle the most complicated case of all:
	   mutually recursive local `def's.
	     (do (def (a ..) (b ..))
	         (def (b ..) (a ..))
                 ...)
           becomes
             (do ; Create mutable closures, no meaningful new_env.
	         (set a (.bind_env $.a))
	         (set b (.bind_env $.b))
		 ; Change `a's binding.
		 (do (do (.make_new_env ..)
		         (.put_new_env .. b))
		     (.change_env a))
		 ; Change `b's binding.
		 (do (do (.make_new_env ..)
		         (.put_new_env .. a))
		     (.change_env b))
		 ; Rest of body.
		 ...)

          where `$.a' and `$.b' are the lifted versions of `a' and
          `b', respectively. */

	if (expr->arity != 2)
	  hh_fatal(expr, "Malformed `do'");

	/* Create the uppermost `do'. */
	res = hh_alloc_node(2);
	hh_ast_copy_location(res, expr);
	res->u.ast[0] = expr->u.ast[0];	/* Reuse the `do' from `expr'. */
	resp = &res->u.ast[1];
	*resp = NULL;

	hh_scope_ix_bup = hh_scope_ix;

	/* Now follows a number of passes through the expression list
	   of `do'. */

	/* First pass: Push local `def's to the scope.  This is
	   necessary so that the `hh_lambda_scan' in the second pass
	   will see `def's later in the list. */
	list = expr->u.ast[1];
	while (list != NULL) {
	  HH_ASSERT(list->arity == 2);
	  x = list->u.ast[0];
	  if (x->arity >= 2
	      && x->arity < HH_AST_ATOMS_START
	      && x->u.ast[0]->arity == HH_AST_SYMBOL
	      && x->u.ast[0]->u.symbol == hh_symbol_def
	      && x->u.ast[1]->arity >= 1
	      && x->u.ast[1]->arity < HH_AST_ATOMS_START
	      && x->u.ast[1]->u.ast[0]->arity == HH_AST_SYMBOL)
	    hh_scope[hh_scope_ix++] = x->u.ast[1]->u.ast[0]->u.symbol;
	  list = list->u.ast[1];
	}

	/* Second pass: Lift the `def's and construct the
	   `(set a (.bind_env $.a))' for each of them. */
	list = expr->u.ast[1];
	while (list != NULL) {
	  HH_ASSERT(list->arity == 2);
	  x = list->u.ast[0];

	  /* Check if `x' is a `set' of a local variable. */
	  if (x->arity == 3
	      && x->u.ast[0]->arity == HH_AST_SYMBOL
	      && x->u.ast[0]->u.symbol == hh_symbol_set
	      && x->u.ast[1]->arity == HH_AST_SYMBOL) {
	    /* It is, add it to the scope, but first lift the body. */
	    x->u.ast[2] = hh_lambda_lift(x->u.ast[2]);
	    hh_scope[hh_scope_ix++] = x->u.ast[1]->u.symbol;

	  /* Otherwise, check if `x' is a local function definition. */
	  } else if (x->arity >= 2
		     && x->arity < HH_AST_ATOMS_START
		     && x->u.ast[0]->arity == HH_AST_SYMBOL
		     && x->u.ast[0]->u.symbol == hh_symbol_def
		     && x->u.ast[1]->arity >= 1
		     && x->u.ast[1]->arity < HH_AST_ATOMS_START
		     && x->u.ast[1]->u.ast[0]->arity == HH_AST_SYMBOL) {
	    /* Create a new symbol for the lifted local `def'. */
	    sprintf(buf, "$.%d", n_lambdas++);
	    sym = hh_ast_symbol(buf);

	    /* Create the `(set a (.bind_env $.a))' to `y'.

	       XXX Here would be a place for optimization: We could
	       lift and scan `x' before we come here and if it refers
	       to no values in the closure, we could merely create a
	       `(set a $.a)'. */
	    y = hh_alloc_node(3);
	    hh_ast_copy_location(y, x);
	    y->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);	/* The `set'. */
	    hh_ast_copy_location(y->u.ast[0], x);
	    y->u.ast[0]->u.symbol = hh_symbol_set;
	    y->u.ast[1] = hh_alloc_node(HH_AST_SYMBOL); /* The `a'. */
	    hh_ast_copy_location(y->u.ast[1], x);
	    y->u.ast[1]->u.symbol = x->u.ast[1]->u.ast[0]->u.symbol;
	    y->u.ast[2] = hh_alloc_node(2); /* The `(.bind_end $.a)' */
	    hh_ast_copy_location(y->u.ast[2], x);
	    y->u.ast[2]->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
	    hh_ast_copy_location(y->u.ast[2]->u.ast[0], x);
	    y->u.ast[2]->u.ast[0]->u.symbol = hh_symbol_bind_env;
	    y->u.ast[2]->u.ast[1] = hh_alloc_node(HH_AST_SYMBOL);
	    hh_ast_copy_location(y->u.ast[2]->u.ast[1], x);
	    y->u.ast[2]->u.ast[1]->u.symbol = sym;
	    /* Prepend the `y' to `resp'. */
	    *resp = hh_alloc_node(2);
	    hh_ast_copy_location(*resp, x);
	    (*resp)->u.ast[0] = y;
	    resp = &(*resp)->u.ast[1];
	    *resp = NULL;

	    /* Lift `def' in `x' to the top-level and scan it. */
	    lift = hh_push_lifted(x);
	    hh_lambda_scan(x, lift->env, hh_scope_ix);

	    /* Count to `j' the number of variables in the closure. */
	    for (j = i = 0; i < HH_MAX_ENV_SIZE; i++)
	      if (lift->env[i] != NULL)
		j++;

	    /* Write into `list->u.ast[0]' the environment rebinding
	       form. */
	    if (j == 0) {

	      /* Nothing to rewrite.  Put `nil' into `list->u.ast[0]'. */
	      list->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
	      list->u.ast[0]->u.symbol = hh_symbol_nil;

	    } else {

	      y = hh_alloc_node(2);
	      hh_ast_copy_location(y, x);
	      y->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL); /* Outer `do'. */
	      hh_ast_copy_location(y->u.ast[0], x);
	      y->u.ast[0]->u.symbol = hh_symbol_do;
	      y->u.ast[1] = hh_alloc_node(2);
	      hh_ast_copy_location(y->u.ast[1], x);
	      
	      y->u.ast[1]->u.ast[0] = hh_make_env(x, j, lift->env);
	      
	      /* The last `(.change_env ...)' into the `do'. */
	      y->u.ast[1]->u.ast[1] = hh_alloc_node(2);
	      hh_ast_copy_location(y->u.ast[1]->u.ast[1], x);
	      y->u.ast[1]->u.ast[1]->u.ast[0] = hh_alloc_node(2);
	      y->u.ast[1]->u.ast[1]->u.ast[1] = NULL;
	      list->u.ast[0] = y;
	      y = y->u.ast[1]->u.ast[1]->u.ast[0];
	      hh_ast_copy_location(y, x);
	      y->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
	      hh_ast_copy_location(y->u.ast[0], x);
	      y->u.ast[0]->u.symbol = hh_symbol_change_env;
	      y->u.ast[1] = hh_alloc_node(HH_AST_SYMBOL);
	      hh_ast_copy_location(y->u.ast[1], x);
	      y->u.ast[1]->u.symbol = x->u.ast[1]->u.ast[0]->u.symbol;

	    }

	    /* Rename the `(def (a ...) ...)' in `x' to
	       `(def ($.a ...) ...)'.  We can mutate here. */
	    x->u.ast[1]->u.ast[0]->u.symbol = sym;

	  } else {

	    /* `x' is neither `set' nor `def', so just perform a
	       recursive lambda lift for it. */
	    list->u.ast[0] = hh_lambda_lift(x);
	  }

	  list = list->u.ast[1];
	}

	/* Catenate the original (but slightly mutated) do-body to the
	   end of the newly created initial `(set ..)'s. */
	*resp = expr->u.ast[1];
	
	/* Restore scope to that of the surrounding expression, and
	   return the new do-list. */
	hh_scope_ix = hh_scope_ix_bup;
	return res;

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_set) {

	/* This is like anything else, but don't replace new local
	   variable.  Make some checks in the process. */
	if (expr->arity != 3)
	  hh_fatal(expr, "`set' expects exactly two arguments");
	if (expr->u.ast[1]->arity != HH_AST_SYMBOL)
	  hh_fatal(expr, "The second argument of `set' must be a symbol");
	if (expr->u.ast[1]->u.symbol->is_builtin
	    && !HH_NODE_IS_IN_PRELUDE(expr))
	  hh_fatal(expr, "Can't `set' builtin symbols except in the prelude");

	expr->u.ast[2] = hh_lambda_lift(expr->u.ast[2]);
	return expr;

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_quote
		 || expr->u.ast[0]->u.symbol == hh_symbol_throw) {

	/* Don't lift and replace symbols in quoted expressions, or a
	   catch tag. */
	return expr;

      } else if (expr->u.ast[0]->u.symbol == hh_symbol_catch) {

	if (expr->arity != 4)
	  hh_fatal(expr, "`catch' expects exactly three arguments: "
		   "protected body, a symbol that represents the catch tag, "
		   "and the catching body");
	expr->u.ast[1] = hh_lambda_lift(expr->u.ast[1]);
	expr->u.ast[3] = hh_lambda_lift(expr->u.ast[3]);
	return expr;

      }
    }

    /* The default is to recurse to all subexpressions. */
    for (i = 0; i < expr->arity; i++)
      expr->u.ast[i] = hh_lambda_lift(expr->u.ast[i]);
    return expr;
  }
}


/* Given a `(def (f a ...) ...)' on the top-level, do some checking,
   push the arguments to the scope, and perform lambda lifting in the
   `def's body.  Note that this may also be called when there are
   already some symbols in the scope, which is the case for already
   lifted functions. */

static hh_ast_t *hh_lambda_lift_def(hh_ast_t *expr)
{
  hh_ast_t *x, *args;
  int i;

  HH_ASSERT(expr->arity == 3); /* This should be ensured by the reader. */
  HH_ASSERT(expr->u.ast[0]->arity == HH_AST_SYMBOL);
  HH_ASSERT(expr->u.ast[0]->u.symbol == hh_symbol_def);

  /* Check the syntax of the argument list. */
  args = expr->u.ast[1];
  i = args->arity;
  if (i == HH_AST_SYMBOL)
    hh_fatal(args, "`def's of non-functions not yet implemented");
  if (i == HH_AST_NIL
      || i > HH_AST_ATOMS_START
      || args->u.ast[0]->arity != HH_AST_SYMBOL)
    hh_fatal(args, "Unrecognized form for `def'");
  if (i > 127)
    hh_fatal(args, "Too long argument list");
  if (expr->u.ast[2] == NULL)
    hh_fatal(expr, "`def' has no body");
  
  /* Put the argument list to the scope. */
  for (i = 1; i < args->arity; i++) {
    x = args->u.ast[i];
    if (x->arity != HH_AST_SYMBOL) {
      /* Check for ellipsis. */
      if (i == args->arity - 1
	  && x->arity == 2
	  && x->u.ast[0]->arity == HH_AST_SYMBOL
	  && x->u.ast[0]->u.symbol == hh_symbol_ellipsis
	  && x->u.ast[1]->arity == HH_AST_SYMBOL)
	x = x->u.ast[1];
      else
	hh_fatal(x, "Parameter name not a symbol");
    }
    hh_scope[hh_scope_ix++] = x->u.symbol;
  }
  
  /* Lift fn's out from the def's body.  Fake this by creating a
     temporary (do ...) around it so that we can reuse the
     functionality written for do-forms. */
  x = hh_alloc_node(2);
  hh_ast_copy_location(x, expr->u.ast[2]);
  x->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
  hh_ast_copy_location(x->u.ast[0], expr->u.ast[2]);
  x->u.ast[0]->u.symbol = hh_symbol_do;
  x->u.ast[1] = expr->u.ast[2];
  
  expr->u.ast[2] = hh_lambda_lift(x)->u.ast[1];

  return expr;
}


hh_ast_t *hh_lambda(hh_ast_t *list)
{
  hh_ast_t *list_head = list, **listp = &list_head, *expr;
  hh_lifted_t *tmp;
  int i;

  while (list != NULL) {
    HH_ASSERT(list->arity == 2);

    hh_n_env_syms_in_scope = hh_scope_ix = 0;
    expr = list->u.ast[0];

    if (expr->arity != HH_AST_NIL
	&& expr->arity < HH_AST_ATOMS_START
	&& expr->u.ast[0]->arity == HH_AST_SYMBOL
	&& expr->u.ast[0]->u.symbol == hh_symbol_def)
      expr = hh_lambda_lift_def(expr);
    else
      /* Something else than a top-level definition. */
      expr = hh_lambda_lift(expr);
    list->u.ast[0] = expr;

    /* Go to the next top-level expression. */
    listp = &list->u.ast[1];
    list = *listp;

    /* Move the contents of `hh_lifted'-stack to the list. */
    while (hh_lifted != NULL) {
      expr = hh_lifted->expr;

      HH_ASSERT(expr->arity >= 2);
      HH_ASSERT(expr->arity < HH_AST_ATOMS_START);
      HH_ASSERT(expr->u.ast[0]->arity == HH_AST_SYMBOL);
      HH_ASSERT(expr->u.ast[0]->u.symbol == hh_symbol_def);

      /* Push contents of the environment to the scope. */
      hh_scope_ix = 0;
      for (i = 0; i < HH_MAX_ENV_SIZE; i++)
	if (hh_lifted->env[i] != NULL)
	  hh_scope[hh_scope_ix++] = hh_lifted->env[i];
      hh_n_env_syms_in_scope = hh_scope_ix;

      /* Remove the lifted function from stack.  Note that
	 `hh_lambda_lift' may change `hh_lifted', so we must do this
	 here. */
      tmp = hh_lifted;
      hh_lifted = hh_lifted->next;
      HH_FREE(tmp);

      expr = hh_lambda_lift_def(expr);

      /* Chain `expr' to the top-level list of expressions. */
      list = hh_alloc_node(2);
      list->u.ast[0] = expr;
      list->u.ast[1] = *listp;
      *listp = list;
      listp = &list->u.ast[1];
      list = *listp;
    }
  }

  return list_head;
}
