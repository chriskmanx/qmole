/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

/* This file implements uses analysis, i.e. it sets the `is_used'-flag
   in all symbols reachable from the top-level program.

   XXX Currently this does not pay attention to local variables that
   in reality hide references to global functions.  For example if any
   reachable part of the code contains `(let ((hex ...)) ...)', then
   the byte code of `hex' defined in `stdlib.lisp' will be included
   even though in reality it won't be referred. */

#define HH_COMPILER  1

#include "hh_common.h"
#include "hh_ast.h"

static int has_changed;

static void hh_rec_uses(hh_ast_t *expr)
{
  int i;
  hh_ast_t *args;
  int n_args;

  if (expr == NULL)
    return;

  switch (expr->arity) {
  case HH_AST_NIL:
  case HH_AST_STRING:
  case HH_AST_INTEGER:
  case HH_AST_UNSIGNED_INTEGER:
    return;
  case HH_AST_SYMBOL:
    if (expr->u.symbol->is_used == 0) {
      expr->u.symbol->is_used = 1;
      has_changed = 1;
    }
    return;
  case 3:
    if (expr->u.ast[0]->arity == HH_AST_SYMBOL
	&& expr->u.ast[0]->u.symbol == hh_symbol_def) {
      args = expr->u.ast[1];

      /* Check the syntax of the argument list. */
      args = expr->u.ast[1];
      n_args = args->arity;
      if (n_args == HH_AST_SYMBOL)
	hh_fatal(args, "`def's of non-functions not yet implemented");
      if (n_args == HH_AST_NIL
	  || n_args > HH_AST_ATOMS_START
	  || args->u.ast[0]->arity != HH_AST_SYMBOL)
	hh_fatal(args, "Unrecognized form for `def'");
      if (n_args > 127)
	hh_fatal(args, "Too long argument list");

      if (args->u.ast[0]->u.symbol->is_used == 1) {
	/* The function is referred but it has not yet been traversed.
	   Traverse and mark traversed. */
	hh_rec_uses(expr->u.ast[2]);
	args->u.ast[0]->u.symbol->is_used = 2;
      }
      return;
    }
  default:
    /* Apply `hh_rec_uses' recursively bottom-up. */
    for (i = 1; i < expr->arity; i++)
      hh_rec_uses(expr->u.ast[i]);
    
    /* The head of the expression is treated a little smarter.  If it
       is any of the defined builtins, then *don't* traverse to the
       symbol. */
    if (expr->u.ast[0]->arity == HH_AST_SYMBOL) {
#define MODULE(name)  /* Nothing. */
#define MODULE_END    /* Nothing. */
#define BUILTIN(lisp_name, c_name, doc_string, args, code_gen)	\
      if (expr->u.ast[0]->u.symbol == hh_symbol_ ## c_name)	\
        return;
#include "hh_builtins.def"
    }

    /* The head is not any of the defined builtins, therefore traverse
       it too. */
    hh_rec_uses(expr->u.ast[0]);
  }
}


void hh_uses(hh_ast_t *prog)
{
  do {
    has_changed = 0;
    hh_rec_uses(prog);
  } while (has_changed);
}
