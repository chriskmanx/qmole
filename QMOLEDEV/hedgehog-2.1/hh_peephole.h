/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */


#ifndef HH_INCL_PEEPHOLE
#define HH_INCL_PEEPHOLE  1


#include "hh_common.h"
#include "hh_codegen.h"
#include "hh_interp.h"


/* Perform various peephole optimizations on the given program. */

hh_code_t *hh_peephole(hh_code_t *codes);


#endif /* !HH_INCL_PEEPHOLE */
