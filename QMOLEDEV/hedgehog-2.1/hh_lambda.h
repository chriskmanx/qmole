/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */


#ifndef HH_INCL_LAMBDA
#define HH_INCL_LAMBDA  1


/* List all `fn's and `def's in non-top-level context into
   corresponding `def's in top-level context, add relevant closure
   generation and reading instructions. */
hh_ast_t *hh_lambda(hh_ast_t *expr);


/* Lambda-lifting prefixes all symbols it generates for lifted
   functions with `$.'. */
#define HH_SYMBOL_IS_LIFTED_LAMBDA(sym)			\
  ((sym)->name[0] == '$' && (sym)->name[1] == '.')


#endif /* !HH_INCL_LAMBDA */
