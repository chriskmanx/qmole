/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004, 2005 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

#include "hh_common.h"
#include "hh_error.h"
#include "hh_interp.h"
#include "hh_data.h"
#include "hh_printf.h"


unsigned char hh_error_kind[HH_N_ERRORS] = {
  0,				/* HH_OK */
#define ERROR(code, kind, description_string) \
  kind,
#include "hh_error.def"
};


char *hh_error_string[HH_N_ERRORS] = {
  "OK",				/* HH_OK */
#define ERROR(code, kind, description_string) \
  description_string,
#include "hh_error.def"
};


void hh_error_print(hh_error_t error, void *aux_info)
{
  char *s;

  switch (hh_error_kind[error]) {
  case HH_SYSTEM_WARNING:
    s = "System warning";
    break;
  case HH_PROGRAM_WARNING:
    s = "Program warning";
    break;
  case HH_SYSTEM_FATAL:
    s = "Fatal system error";
    break;
  case HH_PROGRAM_FATAL:
    s = "Fatal program error";
    break;
  default:
    s = "Unknown error";
    break;
  }

  HH_PRINT("%s #%d: %s", s, (int) error, hh_error_string[error]);

#ifndef HH_SMALL
  if (aux_info == NULL)
    return;
  if (hh_error_kind[error] == HH_PROGRAM_FATAL
      || hh_error_kind[error] == HH_PROGRAM_WARNING) {
    hh_context_t *ctx = (hh_context_t *) aux_info;

    if (ctx->offending_value != 0) {
      HH_PRINT(", got ");
      hh_lisp_print_interpreter(ctx, ctx->offending_value, -1);
    }
    HH_PRINT(". pc = %06d, sp = %d.\n",
	     ctx->pc - (ctx->program + 12), 
	     ctx->sp - ctx->stack);
  }
#else
  HH_PRINT(".\n");
#endif
}
