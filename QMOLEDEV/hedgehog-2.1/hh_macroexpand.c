/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

#define HH_COMPILER  1

#include "hh_common.h"
#include "hh_ast.h"
#include "hh_macroexpand.h"
#include <stdlib.h>


#define MAX_N_MACRO_VARS  16


static struct {
  hh_symbol_t *symbol;
  hh_ast_t *value;
} hh_macro_var_bindings[MAX_N_MACRO_VARS];

static int hh_n_macro_vars;


/* This is used during macro pattern matching to bind the macro
   variables to corresponding abstract syntax trees. */

static void hh_macro_var_bind(hh_ast_t *sym, hh_ast_t *val)
{
  int i;

  /* Check that this macro variable is not already bound -- that would
     be an error since macro patterns must be linear, i.e. all
     variables in them must appear only once in the pattern. */
  for (i = 0; i < hh_n_macro_vars; i++)
    if (hh_macro_var_bindings[i].symbol == sym->u.symbol)
      hh_fatal(sym, "Macro variable appears twice in the macro pattern");
  /* Ok, then perform the binding. */
  if (hh_n_macro_vars == MAX_N_MACRO_VARS)
    hh_fatal(sym, "Too many macro variables");
  hh_macro_var_bindings[hh_n_macro_vars].symbol = sym->u.symbol;
  hh_macro_var_bindings[hh_n_macro_vars].value = val;
  hh_n_macro_vars++;
}


/* This is used during macro instantiation to either look up the
   existing binding for the given macro variable, or if the macro
   variables is not already bound, then to create a new unique
   symbol. */

static hh_ast_t *hh_macro_var_lookup(hh_ast_t *sym)
{
  int i;
  static unsigned int n_auto_vars = 0;
  hh_ast_t *val;
  /* A temporary buffer to hold the name of the automatically
     generated variable name. */
  char buf[20];
  
  /* First check the existing macro variables. */
  for (i = 0; i < hh_n_macro_vars; i++)
    if (hh_macro_var_bindings[i].symbol == sym->u.symbol)
      return hh_macro_var_bindings[i].value;
  /* Not found, then create a new one. */
  if (hh_n_macro_vars == MAX_N_MACRO_VARS)
    hh_fatal(sym, "Too many macro variables");
  val = hh_alloc_node(HH_AST_SYMBOL);
  sprintf(buf, "$%d", n_auto_vars++);
  val->u.symbol = hh_ast_symbol(buf);
  hh_macro_var_bindings[hh_n_macro_vars].symbol = sym->u.symbol;
  hh_macro_var_bindings[hh_n_macro_vars].value = val;
  hh_n_macro_vars++;
  return val;
}


#define HH_IS_MACRO_VAR(sym)			\
  ((sym)->name[0] == '?')


static int hh_macro_match(hh_ast_t *pattern, hh_ast_t *target)
{
  hh_ast_t *n, *v;
  int i, n_subpatterns;

  switch (pattern->arity) {
  case HH_AST_NIL:
    if (target->arity == HH_AST_NIL)
      return 1;
    return 0;
    break;
  case HH_AST_STRING:
    if (target->arity == HH_AST_STRING
	&& pattern->u.string == target->u.string)
      return 1;
    return 0;
    break;
  case HH_AST_SYMBOL:
    if (HH_IS_MACRO_VAR(pattern->u.symbol)) {
      /* The target is a macro pattern variable. */
      hh_macro_var_bind(pattern, target);
      return 1;
    } else
      if (target->arity == HH_AST_SYMBOL
	  && pattern->u.symbol == target->u.symbol)
	return 1;
    return 0;
    break;
  case HH_AST_INTEGER:
    if (target->arity == HH_AST_INTEGER
	&& pattern->u.integer == target->u.integer)
      return 1;
    return 0;
    break;
  case HH_AST_UNSIGNED_INTEGER:
    if (target->arity == HH_AST_UNSIGNED_INTEGER
	&& pattern->u.unsigned_integer == target->u.unsigned_integer)
      return 1;
    return 0;
    break;
  default:
    /* Check that the target is also a list. */
    if (target->arity > HH_AST_ATOMS_START)
      return 0;

    /* First check whether we can make the ellipsis match. */
    n = pattern->u.ast[pattern->arity - 1];
    if (n->arity == 2
	&& n->u.ast[0]->arity == HH_AST_SYMBOL
	&& n->u.ast[0]->u.symbol == hh_symbol_ellipsis) {
      HH_ASSERT(n->u.ast[1]->arity == HH_AST_SYMBOL);
      if (!HH_IS_MACRO_VAR(n->u.ast[1]->u.symbol))
	hh_fatal(n->u.ast[1], "In macros ellipsis `...' must "
		 "be followed by a macro pattern variable");
      if (target->arity < pattern->arity
#if 1
	  /* This code allows ellipsis to match with an empty list. 
	     I don't yet know whether that is desired behavior. */
	  -1
#endif
	  )
	return 0;
      v = hh_alloc_node(target->arity - (pattern->arity - 1));
      hh_ast_copy_location(v, target);
      for (i = 0; i < target->arity - (pattern->arity - 1); i++)
	v->u.ast[i] = target->u.ast[i + pattern->arity - 1];
      hh_macro_var_bind(n->u.ast[1], v);
      n_subpatterns = pattern->arity - 1;
    } else {
      /* If there is no ellipsis, then the arities must be identical
	 and the last subpatterns must match. */
      if (target->arity != pattern->arity)
	return 0;
      n_subpatterns = pattern->arity;
    }
    /* Finally try to match all subpatterns to all the subtargets. */
    for (i = 0; i < n_subpatterns; i++)
      if (!hh_macro_match(pattern->u.ast[i], target->u.ast[i]))
	return 0;
    return 1;
    break;
  }
  HH_NOTREACHED;
}


static hh_ast_t *hh_macro_rewrite(hh_ast_t *replacement)
{
  hh_ast_t *n, *v = NULL;
  int i, arity;

  if (replacement == NULL)
    return NULL;

  switch (replacement->arity) {
  case HH_AST_SYMBOL:
    if (HH_IS_MACRO_VAR(replacement->u.symbol))
      return hh_macro_var_lookup(replacement);
  case HH_AST_NIL:
  case HH_AST_STRING:
  case HH_AST_INTEGER:
  case HH_AST_UNSIGNED_INTEGER:
    return replacement;
    break;
  default:
    arity = replacement->arity;
    n = replacement->u.ast[arity - 1];
    if (n != NULL
	&& n->arity == 2
	&& n->u.ast[0]->arity == HH_AST_SYMBOL
	&& n->u.ast[0]->u.symbol == hh_symbol_ellipsis) {
      HH_ASSERT(n->u.ast[1]->arity == HH_AST_SYMBOL);
      if (!HH_IS_MACRO_VAR(n->u.ast[1]->u.symbol))
	hh_fatal(n->u.ast[1], "In macros ellipsis `...' must "
		 "be followed by a macro pattern variable");
      v = hh_macro_var_lookup(n->u.ast[1]);
      if (v->arity > HH_AST_ATOMS_START)
	hh_fatal(n->u.ast[1], "In macro replacement ellipsis `...' must "
		 "be followed by a bound macro pattern variable");
      arity += (int) v->arity - 1;
    }
    if (replacement->u.ast[0]->arity == HH_AST_SYMBOL
	&& replacement->u.ast[0]->u.symbol == hh_symbol_macroquote) {
      if (replacement->arity != 2)
	hh_fatal(replacement, "Macro-quote (#') expects exactly one argument");
      /* Do nothing, n was already assigned replacement->u.ast[1]. */
    } else if (replacement->u.ast[0]->arity == HH_AST_SYMBOL
	       && replacement->u.ast[0]->u.symbol == hh_symbol_macroconcat) {
      hh_ast_t *m;
      char *s = NULL;
      int s_length = 0;

      n = hh_alloc_node(HH_AST_SYMBOL);
      hh_ast_copy_location(n, replacement);
      if (v == NULL)
	/* There is no ellipsis. */
	for (i = 1; i < arity; i++) {
	  m = hh_macro_rewrite(replacement->u.ast[i]);
	  if (m->arity != HH_AST_SYMBOL)
	    hh_fatal(m, "Macro token concatenator `##' applied "
		     "to a non-symbol");
	  s_length += strlen(m->u.symbol->name);
	  s = realloc(s, s_length + 1);
	  if (s == NULL)
	    hh_fatal(m, "Out of memory when concatenating `##' a new symbol");
	  if (i == 1)
	    strcpy(s, m->u.symbol->name);
	  else 
	    strcat(s, m->u.symbol->name);
	}
      else {
	/* There is ellipsis. */
	for (i = 1; i < replacement->arity - 1; i++) {
	  m = hh_macro_rewrite(replacement->u.ast[i]);
	  if (m->arity != HH_AST_SYMBOL)
	    hh_fatal(m, "Macro token concatenator `##' applied "
		     "to a non-symbol");
	  s_length += strlen(m->u.symbol->name);
	  s = realloc(s, s_length + 1);
	  if (s == NULL)
	    hh_fatal(m, "Out of memory when concatenating `##' a new symbol");
	  if (i == 1)
	    strcpy(s, m->u.symbol->name);
	  else 
	    strcat(s, m->u.symbol->name);
	}
	for (i = 0; i < arity - (replacement->arity - 1); i++) {
	  m = hh_macro_rewrite(v->u.ast[i]);
	  if (m->arity != HH_AST_SYMBOL)
	    hh_fatal(m, "Macro token concatenator `##' applied "
		     "to a non-symbol");
	  s_length += strlen(m->u.symbol->name);
	  s = realloc(s, s_length + 1);
	  if (s == NULL)
	    hh_fatal(m, "Out of memory when concatenating `##' a new symbol");
	  if (i == 1)
	    strcpy(s, m->u.symbol->name);
	  else 
	    strcat(s, m->u.symbol->name);
	}
      }
      if (s == NULL)
	hh_fatal(replacement, "Macro token concatenator `##' requires at least"
		 " one argument");
      n->u.symbol = hh_ast_symbol(s);
      free(s);
    } else {
      n = hh_alloc_node(arity);
      hh_ast_copy_location(n, replacement);
      if (v == NULL)
	/* There is no ellipsis. */
	for (i = 0; i < arity; i++)
	  n->u.ast[i] = hh_macro_rewrite(replacement->u.ast[i]);
      else {
	/* There is ellipsis. */
	for (i = 0; i < replacement->arity - 1; i++)
	  n->u.ast[i] = hh_macro_rewrite(replacement->u.ast[i]);
	for (i = 0; i < arity - (replacement->arity - 1); i++)
	  n->u.ast[i + replacement->arity - 1] = hh_macro_rewrite(v->u.ast[i]);
      }
    }
    return n;
  }
  HH_NOTREACHED;
}


/* This function performs a top-down macro expansion on the given list
   with the given macros in its macro scope. */

static hh_ast_t *hh_rec_macroexpand(hh_ast_t *expr,
				    hh_ast_t *macro_list,
				    int n_rec_expansions)
{
  hh_ast_t *m, *ml;
  int n_iter_expansions = 0, i;

  if (expr == NULL)
    return expr;
 rewrite_more:
  if (expr->arity > HH_AST_NIL
      && expr->arity < HH_AST_ATOMS_START
      && expr->u.ast[0]->arity == HH_AST_SYMBOL
      && expr->u.ast[0]->u.symbol == hh_symbol_defs)
    /* Don't try to expand an empty expression or macro definitions.
       Contents of macros are macro-expanded after the macro has been
       instantiated somewhere. */
    return expr;

  for (ml = macro_list; ml != NULL; ml = ml->u.ast[1]) {
    HH_ASSERT(ml->arity == 2);
    m = ml->u.ast[0];
    hh_n_macro_vars = 0;
    if (hh_macro_match(m->u.ast[1], expr)) {
      if (n_rec_expansions + n_iter_expansions++ > 100)
	hh_fatal(expr, "Too many nested macro expansions");
      if (m->u.ast[2]->u.ast[1] == HH_AST_NIL) {
	/* Single expression -body macro, iterate its expansion
	   in-place. */
	expr = hh_macro_rewrite(m->u.ast[2]->u.ast[0]);
	goto rewrite_more;
      } else {
	expr = hh_alloc_node(2);
	hh_ast_copy_location(expr, m);
	expr->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
	expr->u.ast[0]->u.symbol = hh_symbol_dot_do;
	expr->u.ast[1] = hh_macro_rewrite(m->u.ast[2]);
	return expr;
      }
    }
  }
  /* This expression didn't rewrite any further, but try its
     subexpressions. */
  if (n_iter_expansions > 0)
    n_rec_expansions++;
  if (expr->arity != HH_AST_NIL && expr->arity < HH_AST_ATOMS_START) {
    m = expr->u.ast[0];
    if (m->arity == HH_AST_SYMBOL
	&& (m->u.symbol == hh_symbol_def || m->u.symbol == hh_symbol_fn)) {
      if (expr->arity != 3)
	hh_fatal(m, "Too few arguments to %s", m->u.symbol->name);
      hh_macroexpand(&expr->u.ast[2], macro_list, n_rec_expansions);
    } else if (m->arity == HH_AST_SYMBOL 
	       && (m->u.symbol == hh_symbol_do 
		   || m->u.symbol == hh_symbol_dot_do))
      hh_macroexpand(&expr->u.ast[1], macro_list, n_rec_expansions);
    else {
      for (i = 0; i < expr->arity; i++) {
	expr->u.ast[i] = hh_rec_macroexpand(expr->u.ast[i],
					    macro_list,
					    n_rec_expansions);
	if (expr->u.ast[i] != HH_AST_NIL
	    && expr->u.ast[i]->arity == 2
	    && expr->u.ast[i]->u.ast[0]->arity == HH_AST_SYMBOL
	    && expr->u.ast[i]->u.ast[0]->u.symbol == hh_symbol_dot_do) {
	  expr->u.ast[i]->u.ast[0]->u.symbol = hh_symbol_do;
	  hh_macroexpand(&expr->u.ast[i]->u.ast[1], macro_list, 
			 n_rec_expansions);
	}
      }
    }
  }
  return expr;
}


void hh_macroexpand(hh_ast_t **list, hh_ast_t *macro_list,
		    int n_rec_expansions)
{
  hh_ast_t *n, *m;

  /* fprintf(stderr, "ENTER\n"); */
  
  while (*list != NULL) {
    n = *list;
    HH_ASSERT(n->arity == 2);
    
    /* fprintf(stderr, "TOP "); hh_ast_dump(n->u.ast[0]); fprintf(stderr, "\n"); */

    if (n->u.ast[0]->arity > HH_AST_NIL
	&& n->u.ast[0]->arity < HH_AST_ATOMS_START
	&& n->u.ast[0]->u.ast[0]->arity == HH_AST_SYMBOL
	&& n->u.ast[0]->u.ast[0]->u.symbol == hh_symbol_defs) {
      if (n->u.ast[0]->arity < 3)
	hh_fatal(n->u.ast[0], "Incorrect macro definition, should be like "
		 "`(def-syntax pattern replacement)");
      /* Move this macro definition from the program list to the macro
	 list and proceed to the next expression. */
      *list = n->u.ast[1];
      n->u.ast[1] = macro_list;
      macro_list = n;
    } else {
      m = hh_rec_macroexpand(n->u.ast[0], macro_list, n_rec_expansions);

      if (m != n->u.ast[0]) {
	if (m->arity == 2
	    && m->u.ast[0]->arity == HH_AST_SYMBOL
	    && m->u.ast[0]->u.symbol == hh_symbol_dot_do) {
	  /* fprintf(stderr, "FOO "); hh_ast_dump(m); fprintf(stderr, "\n"); */
	  *list = m->u.ast[1];
	  HH_ASSERT(m->u.ast[1] != HH_AST_NIL);
	  do {
	    m = m->u.ast[1];
	  } while (m->u.ast[1] != HH_AST_NIL);
	  m->u.ast[1] = n->u.ast[1];
	  /* fprintf(stderr, "FOO2 "); hh_ast_dump(m); fprintf(stderr, "\n"); */
	} else {
	  n->u.ast[0] = m;
	  list = &n->u.ast[1];
	}
      } else
	list = &n->u.ast[1];
    }
  }
  /* fprintf(stderr, "RETURN\n"); */
}
