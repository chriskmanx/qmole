/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */


#ifndef HH_INCL_CODEGEN
#define HH_INCL_CODEGEN  1


#include "hh_common.h"
#include "hh_ast.h"
#include "hh_interp.h"


/* A doubly-linked list of byte code instructions in a form amenable
   for either peephole optimizations and outputting. */

typedef struct hh_code_t {
  hh_ast_t *ast;
  struct hh_code_t *prev, *next;
  enum { HH_IMM, HH_IMM2, HH_INSN, HH_BRANCH, HH_LABEL, HH_FN } kind;
  hh_signed_word_t position;
  int reachable;
  union {
    hh_insn_t insn;
    struct {
      hh_imm_insn_t insn;
      hh_signed_word_t value;
    } imm;
    struct {
      hh_imm_insn_t insn;
      hh_signed_word_t value1;
      hh_signed_word_t value2;
    } imm2;
    struct {
      hh_imm_insn_t insn;
      struct hh_code_t *target;
    } branch;
    struct {
      unsigned char n_args;
      unsigned char allow_excess_args;
      hh_symbol_t *symbol;
    } fn;
  } u;
} hh_code_t;


extern hh_context_t hh_constant_ctx;

hh_code_t *hh_gen_code(hh_ast_t *list);

void hh_grow_constant_ctx(unsigned long n_words);

void hh_gen_init(int generate_debug_data);


#endif /* !HH_INCL_CODEGEN */
