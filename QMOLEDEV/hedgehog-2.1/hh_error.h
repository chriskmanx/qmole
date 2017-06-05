/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004, 2005 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

#ifndef HH_INCL_ERROR
#define HH_INCL_ERROR  1


/* Define the enumerated type for the kinds of errors.
 */
typedef enum {
  /* Recoverable warnings and notifications come here. */
  HH_SYSTEM_WARNING,
  HH_PROGRAM_WARNING,

  /* The program is not runnable for some reason, but the byte code
     interpreter and the operating system are still alive. */
  HH_PROGRAM_NORUN,

  /* This must be here. */
  HH_N_WARNING_KINDS,

  /* Irrecoverable fatal errors which require rebooting or something
     equally violent come here. */
  HH_SYSTEM_FATAL,
  HH_PROGRAM_FATAL
} hh_error_kind_t;


/* Define the enumerated type of all error codes.
 */
typedef enum {
  HH_OK = 0,
#define ERROR(code, kind, description_string) \
  code,
#include "hh_error.def"
#undef ERROR
  HH_N_ERRORS,
} hh_error_t;

extern unsigned char hh_error_kind[];

#define HH_ERROR_IS_FATAL(error)  (hh_error_kind[error] > HH_N_WARNING_KINDS)

/* HH_PRINT the error message into the given buffer.  `aux_info' could
   be a `hh_ctx_t *' in case of a run-time error in the byte code
   program.
 */
void hh_error_print(hh_error_t error, void *aux_info);


#endif  /* !HH_INCL_ERROR */
