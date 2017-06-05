/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

/* This file implements some peephole optimizations.
 */

#define HH_COMPILER  1

#include "hh_common.h"
#include "hh_data.h"
#include "hh_codegen.h"


#define HH_PURE  0x00000001


static const int hh_insn_is_pure[] = {
#define INSN(mnemonic, flags, code)      ((flags) & HH_PURE),
#define IMM(mnemonic, flags, code)       /* Nothing */
#define EXT_INSN(mnemonic, flags, code)  /* Nothing */

#include "hh_insn.def"

  0
};


static const int hh_ext_insn_is_pure[] = {
#define INSN(mnemonic, flags, code)      /* Nothing */
#define IMM(mnemonic, flags, code)       /* Nothing */
#define EXT_INSN(mnemonic, flags, code)  ((flags) & HH_PURE),

#include "hh_insn.def"

  0
};


static const int hh_imm_is_pure[] = {
#define INSN(mnemonic, flags, code)      /* Nothing */
#define IMM(mnemonic, flags, code)       ((flags) & HH_PURE),
#define EXT_INSN(mnemonic, flags, code)  /* Nothing */

#include "hh_insn.def"

  0
};


/* Is an instruction "pure", i.e. free of side effects such as IO,
   memory writes, etc.  For example the computing the string length is
   pure, since it inspects the given string and leaves its length into
   `accu'.  It does not pop values from stack, for example.  We do not
   regard side effects of error cases as side effects in this sense.
   Therefore computing string length is pure even though it has a
   side-effecting type check. */

static int hh_code_is_pure(hh_code_t *code)
{
  if (code->kind == HH_INSN)
    if (code->u.insn < HH_START_OF_EXT_INSNS)
      return hh_insn_is_pure[code->u.insn];
    else
      return hh_ext_insn_is_pure[code->u.insn - HH_START_OF_EXT_INSNS - 1];
  else if (code->kind == HH_IMM)
    return hh_imm_is_pure[code->u.imm.insn];
  else
    return 0;
}


static hh_code_t *hh_eliminate_dead_code(hh_code_t *codes)
{
  hh_code_t *code, **codep;
  int changed;
  
  /* Clear reachable marks in insns. */
  for (code = codes; code != NULL; code = code->next)
    code->reachable = 0;

  /* The first insn is reachable, since that's where the execution
     starts. */
  codes->reachable = 1;

  /* Compute reachable instructions. */
  do {
    changed = 0;

    for (code = codes; code != NULL; code = code->next) {
      /* Mark function entry points as reachable. */
      if (code->kind == HH_FN && !code->reachable) {
	code->reachable = 1;
	changed = 1;
      }
      /* Do nothing for this insn if it is not reachable. */
      if (!code->reachable)
	continue;
      /* The instructions after branch, return, and tailcall are not by
	 default reachable, otherwise mark them as reachable. */
      if (code->next != NULL
	  && !code->next->reachable
	  && !(code->kind == HH_BRANCH
	       && code->u.branch.insn == HH_IMM_branch)
	  && !(code->kind == HH_IMM
	       && code->u.imm.insn == HH_IMM_return)
	  && !(code->kind == HH_IMM2
	       && code->u.imm2.insn == HH_IMM_tailcall)) {
	code->next->reachable = 1;
	changed = 1;
      }
      /* The branch targets are reachable. */
      if (code->kind == HH_BRANCH
	  && !code->u.branch.target->reachable) {
	code->u.branch.target->reachable = 1;
	changed = 1;
      }
    }
  } while (changed);

  /* Remove all unreachable insns. */
  for (codep = &codes, code = *codep;
       code != NULL;
       code = *codep)
    if (!code->reachable)
      *codep = code->next;
    else
      codep = &code->next;

  /* hh_dump_codes(codes); */

  return codes;
}


hh_code_t *hh_peephole(hh_code_t *codes)
{
  hh_code_t *code, **codep, *target, *next;
  int dropped, changed;
  
  do {
    changed = 0;

    codes = hh_eliminate_dead_code(codes);

    for (codep = &codes, code = *codep; 
	 code != NULL;
	 codep = dropped ? codep : &code->next, code = *codep) {
      dropped = 0;
      /* Remove a pure instruction followed by an immeadiate load. */
      if (hh_code_is_pure(code)
	  && code->next != NULL
	  && code->next->kind == HH_IMM
	  && (code->next->u.imm.insn == HH_IMM_load
	      || code->next->u.imm.insn == HH_IMM_pick
	      || code->next->u.imm.insn == HH_IMM_get_env)) {
	*codep = code->next;
	dropped = 1;
      }
      /* Merge `drop x' and `return y' into `return x+y',
	 or `drop x' and `drop y' into `drop x+y'. */
      if (code->kind == HH_IMM
	  && code->u.imm.insn == HH_IMM_drop
	  && code->next != NULL
	  && code->next->kind == HH_IMM
	  && (code->next->u.imm.insn == HH_IMM_return
	      || code->next->u.imm.insn == HH_IMM_drop)) {
	*codep = code->next;
	code->next->u.imm.value += code->u.imm.value;
	dropped = 1;
      }
      /* Replace an unconditional branch to a label followed by a
	 return, tailcall or unconditional branch with that insn. */
      if (code->kind == HH_BRANCH
	  && code->u.branch.insn == HH_IMM_branch) {
	target = code->u.branch.target;
	while (target->kind == HH_LABEL)
	  target = target->next;
	if ((target->kind == HH_IMM2 && target->u.imm.insn == HH_IMM_tailcall)
	    || (target->kind == HH_IMM && target->u.imm.insn == HH_IMM_return)
	    || (target->kind == HH_IMM
		&& target->u.imm.insn == HH_IMM_branch)) {
	  next = code->next;
	  *code = *target;
	  code->next = next;
	  changed = 1;
	}
      }
#if 0
      /* XXX This optimization is incorrect - it leaves an un-negated
         value into `accu'. */
      /* Replace `not' and `branch_if_false' with `branch_if_true',
	 and vice versa. */
      if (code->kind == HH_INSN
	  && code->u.insn == HH_INSN_not
	  && code->next != NULL
	  && code->next->kind == HH_BRANCH) {
	if (code->next->u.branch.insn == HH_IMM_branch_if_true) {
	  code->next->u.branch.insn = HH_IMM_branch_if_false;
	  *codep = code->next;
	  dropped = 1;
	} else if (code->next->u.branch.insn == HH_IMM_branch_if_false) {
	  code->next->u.branch.insn = HH_IMM_branch_if_true;
	  *codep = code->next;
	  dropped = 1;
	}
      }
#endif
      /* A `pick -1' after a `push' is redundant. */
      if (code->kind == HH_INSN
	  && code->u.insn == HH_INSN_push
	  && code->next != NULL
	  && code->next->kind == HH_IMM
	  && code->next->u.imm.insn == HH_IMM_pick
	  && code->next->u.imm.value == -1) {
	code->next = code->next->next;
	changed = 1;
      }
      /* Other rewrites to be implemented. */

      if (dropped)
	changed = 1;
    }
  } while (changed);
  
  /* A second peephole optimization creates aggregate instructions,
     such as `push_load'.  Note that there may be no labels in between
     the aggregated instructions. */
  for (codep = &codes, code = *codep;
       code != NULL;
       codep = &code->next, code = *codep) {
    /* Aggregate a push, load, and add/sub to add_imm. */
    if (code->kind == HH_INSN
	&& code->u.insn == HH_INSN_push
	&& code->next != NULL
	&& code->next->kind == HH_IMM
	&& code->next->u.imm.insn == HH_IMM_load
	&& code->next->next != NULL
	&& code->next->next->kind == HH_INSN) {
      if (code->next->next->u.insn == HH_INSN_add) {
	/* Make the load into `add_imm' and remove the `push' in
	   `code' and `add' in `code->next->next'. */
	code->next->u.imm.insn = HH_IMM_add_imm;
	*codep = code->next;
	code->next->next = code->next->next->next;
	continue;
      } else if (code->next->next->u.insn == HH_INSN_sub) {
	/* Make the load into `add_imm' with changed sign, and remove
	   the `push' in `code' and `add' in `code->next->next'. */
	code->next->u.imm.insn = HH_IMM_add_imm;
	hh_grow_constant_ctx(HH_BOX_N_WORDS);
	code->next->u.imm.value =
	  HH_SIGNED_TO_WORD(&hh_constant_ctx,
			    -HH_WORD_TO_SIGNED(&hh_constant_ctx,
					       code->next->u.imm.value));
	*codep = code->next;
	code->next->next = code->next->next->next;
	continue;
      }
    }
    /* Aggregate a push and load into push_load. */
    if (code->kind == HH_INSN
	&& code->u.insn == HH_INSN_push
	&& code->next != NULL
	&& code->next->kind == HH_IMM
	&& code->next->u.imm.insn == HH_IMM_load) {
      code->next->u.imm.insn = HH_IMM_push_load;
      *codep = code->next;
      continue;
    }
    /* Aggregate a `push', `pick y', `push', and `pick x' into into
       `push_pick_push_pick x, y' when x negative (local variable
       reference). */
    if (code->kind == HH_INSN
	&& code->u.insn == HH_INSN_push
	&& code->next != NULL
	&& code->next->kind == HH_IMM
	&& code->next->u.imm.insn == HH_IMM_pick
	&& code->next->u.imm.value < 0
	&& code->next->u.imm.value >= -128
	&& code->next->next != NULL
	&& code->next->next->kind == HH_INSN
	&& code->next->next->u.insn == HH_INSN_push
	&& code->next->next->next != NULL
	&& code->next->next->next->kind == HH_IMM
	&& code->next->next->next->u.imm.insn == HH_IMM_pick) {
      hh_signed_word_t p1 = code->next->u.imm.value;
      hh_signed_word_t p2 = code->next->next->next->u.imm.value;
      code->next->kind = HH_IMM2;
      code->next->u.imm2.insn = HH_IMM_push_pick_push_pick;
      code->next->u.imm2.value1 = p2;
      code->next->u.imm2.value2 = p1;
      *codep = code->next;
      code->next->next = code->next->next->next->next;
      continue;
    }
    /* Aggregate a push and pick into push_pick. */
    if (code->kind == HH_INSN
	&& code->u.insn == HH_INSN_push
	&& code->next != NULL
	&& code->next->kind == HH_IMM
	&& code->next->u.imm.insn == HH_IMM_pick) {
      code->next->u.imm.insn = HH_IMM_push_pick;
      *codep = code->next;
      continue;
    }
  }

  return codes;
}
