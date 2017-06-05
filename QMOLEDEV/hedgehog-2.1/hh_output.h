/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */


#ifndef HH_INCL_OUTPUT
#define HH_INCL_OUTPUT  1


#include "hh_common.h"
#include "hh_codegen.h"
#include "hh_interp.h"


/* Dump the given code sequence to the given file. */

typedef enum {
  HH_BYTECODE,
  HH_HEX,
  HH_HEX_C,
} hh_output_type_t;

void hh_output(hh_code_t *codes, FILE *code_fp, hh_output_type_t type,
	       int generate_debug_data, FILE *asm_fp);


/* Usable only during debugging. */

void hh_dump_codes(hh_code_t *codes);


#endif /* !HH_INCL_OUTPUT */
