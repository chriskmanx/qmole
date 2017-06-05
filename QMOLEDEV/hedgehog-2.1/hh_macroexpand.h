/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */


#ifndef HH_INCL_MACROEXPAND
#define HH_INCL_MACROEXPAND  1


#include "hh_ast.h"


/* Perform macro expansion for the given list of definitions. */

void hh_macroexpand(hh_ast_t **list, hh_ast_t *macro_list,
		    int n_rec_expansions);


#endif /* !HH_INCL_MACROEXPAND */
