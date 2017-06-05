/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004, 2005 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */


#ifndef HH_INTERP
#define HH_INTERP  1

#include "hh_common.h"
#include "hh_error.h"



/* Macros for unaligned MSB first memory reads.  `p' is assumed to be
   a valid pointer value of type `unsigned char *'. */

#define HH_GET_UINT32(p)			\
  (((hh_word_t) (p)[0] << 24)			\
   | ((hh_word_t) (p)[1] << 16) 		\
   | ((hh_word_t) (p)[2] << 8) 			\
   | (p)[3])

#define HH_GET_UINT24(p)			\
  (((hh_word_t) (p)[0] << 16)			\
   | ((hh_word_t) (p)[1] << 8)			\
   | (p)[2])

#define HH_GET_UINT16(p)  			\
  (((hh_word_t) (p)[0] << 8) | (p)[1])


/* Converses for writing. */

#define HH_PUT_UINT32(p, w)			\
  do {						\
    (p)[0] = (w) >> 24;				\
    (p)[1] = (w) >> 16;				\
    (p)[2] = (w) >> 8;				\
    (p)[3] = (w);				\
  } while (0)

#define HH_PUT_UINT24(p, w)			\
  do {						\
    (p)[0] = (w) >> 16;				\
    (p)[1] = (w) >> 8;				\
    (p)[2] = (w);				\
  } while (0)

#define HH_PUT_UINT16(p, w)			\
  do {						\
    (p)[0] = (w) >> 8;				\
    (p)[1] = (w);				\
  } while (0)


/* Instruction mnemonics. */

typedef enum {
#define INSN(mnemonic, flags, code)     HH_INSN_ ## mnemonic,
#define IMM(mnemonic, flags, code)	/* Nothing */
#define EXT_INSN(mnemonic, flags, code) /* Nothing */

#include "hh_insn.def"

  HH_NUMBER_OF_INSNS,

  HH_START_OF_EXT_INSNS = 255,	/* First HH_EXT_INSN_... will be 256. */

#define INSN(mnemonic, flags, code)	/* Nothing */
#define IMM(mnemonic, flags, code)	/* Nothing */
#define EXT_INSN(mnemonic, flags, code) HH_INSN_ ## mnemonic,

#include "hh_insn.def"

  HH_NUMBER_OF_EXT_INSNS,

} hh_insn_t;


typedef enum {
#define INSN(mnemonic, flags, code)     /* Nothing */
#define IMM(mnemonic, flags, code)      HH_IMM_ ## mnemonic,
#define EXT_INSN(mnemonic, flags, code) /* Nothing */

#include "hh_insn.def"

  HH_IMM_ext,
  HH_NUMBER_OF_IMMS,
} hh_imm_insn_t;


/* Internal state of a single HedgeHog interpreter execution.
 */

typedef struct hh_context_t {
  unsigned char *program;
  /* `heap' is the pointer to the current heap, and during collection,
     the to-space.  `old_heap' is correspondingly the from-space.  For
     various convenience and performance reasons allocation happens
     downwords starting from `heap + heap_n_words', but upwards during
     garbage collection.  The field `heap_free' tells how high the
     allocation took place during the last gc, and `heap_ptr' is the
     current allocation pointer during normal allocation. */
  hh_word_t *heap, *old_heap, *heap_ptr, *heap_free;
  hh_word_t heap_n_words;
  /* Pointer to the beginning of the constant pool. */
  hh_word_t *constant;
  /* The rest of this structure is not needed by the compiler. */
#ifndef HH_COMPILER
  unsigned char *pc;
  hh_word_t accu, env, new_env;
  hh_word_t *sp;
  hh_word_t stack_n_words;
#ifdef HH_UNIX
  /* This part is included if we are in a UNIX and support select(2).
     See the documentation of hh_interp_step below for information
     about these fields. */
  fd_set select_read_fds, select_write_fds;
  int select_max_fd;
  struct timeval select_timeout;
  int select_retval;
  unsigned char program_wants_to_select;
#endif
#ifdef HH_TESTING
  hh_word_t offending_value;
  unsigned char insn_trace_enabled;
  unsigned char gc_trace_enabled;
  /* If profiling is enabled, then this is a pointer to a `hh_word_t'
     array as large as the number of byte code insns in the entire
     program.  When executing an insn, the corresponding slot in the
     array is incremented.  If profiling is disabled, this is NULL. */
  hh_word_t *profile_data;
  hh_word_t redzone;
#endif
  /* The run-time stack is below.  `hh_context_allocate' allocates
     extra memory for this struct, and the stack grows there. */
  hh_word_t stack[1];
  /* Do not add any additional fields here! */
#endif /* !HH_COMPILER */
} hh_context_t;


/* The rest of this file is not needed by the compiler. */
#ifndef HH_COMPILER

/* Check the given program file is correct and executable on this byte
   code interpreter.  This function also fixes the byte order of the
   program file's constant pool, and therefore this function *MUST* be
   called prior to using the program for anything else.  `program'
   must be word-aligned.  Returns non-zero on success. */

hh_error_t hh_program_check(unsigned char *program, unsigned int n_bytes);


/* Allocate a Hedgehog execution context.  Note that it *must* be
   allocated with this function.  The `heap_n_words' tells the size of
   one semispace in the garbage-collectable dynamic heap.
   `stack_n_words' tells the number of words to be reserved for the
   stack.  If `enable_profiling' is non-zero and `HH_TESTING' is
   defined, allocate an array used for profiling.  This array is
   `HH_PRINT'ed in `hh_context_free'.  Returns NULL in case of
   HH_MALLOC failure. */

hh_context_t *hh_context_allocate(unsigned char *program,
				  unsigned long heap_n_words,
				  unsigned long stack_n_words,
				  int enable_profiling);

/* Free the Hedgehog execution context, including the call stack and
   the lisp heap(s).  If `HH_TESTING' is defined, then return the
   highest stack position used during the execution. */

long hh_context_free(hh_context_t *ctx);


/* Interprete the byte code program for the given number of `ticks'.
   Return `HH_OK' if the program did not exit.  Return
   `HH_ERROR_PROGRAM_EXITED', in which case the next call to interp
   will rerun the same program.  In case of error, return the error
   code and restore `ctx' to something that is possible to pass to
   `hh_error_fmt' as `aux_info'.

   I suggest using relatively large values for `n_ticks' for sake of
   reasonable performance, typically in the range of hundreds to
   thousands.

   If HH_UNIX is defined, hh_interp_step returns HH_OK and
   ctx->program_wants_to_select is non-zero, then the caller should issue
     ctx->select_retval = select(ctx->select_max_fd + 1,
                                 &ctx->select_read_fds,
                                 &ctx->select_write_fds,
				 NULL,
				 &ctx->select_timeout);
   and thereafter call hh_interp_step again.  This architecture of
   returning to the caller for select allows the programmers to merge
   several event loops to one and therby avoid a severe anti-pattern. */

hh_error_t hh_interp_step(hh_context_t *ctx, hh_signed_word_t n_ticks);


#ifndef HH_SMALL

/* Print, using HH_PRINT, a function call backtrace of the current
   state of execution. */
void hh_backtrace(hh_context_t *ctx);

#endif /* HH_SMALL */

#endif /* !HH_COMPILER */

#endif /* !HH_INTERP */
