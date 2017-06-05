/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

#define HH_COMPILER  1

#include "hh_common.h"
#include "hh_ast.h"
#include "hh_data.h"
#include "hh_interp.h"
#include "hh_codegen.h"
#include "hh_lambda.h"

#include <stdlib.h>


static hh_code_t *hh_code_first, *hh_code_last;
static hh_ast_t *hh_ast_compiler_generated;


static hh_code_t *hh_alloc_code(hh_ast_t *ast)
{
  static hh_code_t *alloc_buf = NULL, *alloc_buf_end;

  if (alloc_buf == NULL || alloc_buf == alloc_buf_end) {
    alloc_buf = malloc(1024 * sizeof(hh_code_t));
    if (alloc_buf == NULL)
      hh_fatal(NULL, "Malloc failed during code generation");
    alloc_buf_end = alloc_buf + 1024;
  }
  /* Put the new code last in the doubly linked list of codes. */
  if (hh_code_last == NULL) {
    hh_code_last = hh_code_first = alloc_buf;
    hh_code_last->prev = NULL;
  } else {
    hh_code_last->next = alloc_buf;
    alloc_buf->prev = hh_code_last;
    hh_code_last = alloc_buf;
  }
  hh_code_last->ast = ast;
  hh_code_last->next = NULL;
  hh_code_last->position = -1;
  return alloc_buf++;
}


/* This pool is used as a fake heap for allocating values that will
   eventually go into the constant pool. */

hh_context_t hh_constant_ctx;

static void hh_init_constant_ctx(void)
{
  hh_word_t n_bytes;

  memset(&hh_constant_ctx, 0, sizeof(hh_constant_ctx));
  hh_constant_ctx.heap_n_words = 1024;
  n_bytes = hh_constant_ctx.heap_n_words * sizeof(hh_word_t);
  hh_constant_ctx.heap_ptr = hh_constant_ctx.heap = malloc(n_bytes);
  if (hh_constant_ctx.heap == NULL)
    hh_fatal(NULL, "Out of memory when allocating constant pool");
  memset(hh_constant_ctx.heap, 0, n_bytes);
  /* Have `hh_constant_ctx's heap and constant be the same so that
     conversion macros work. */
  hh_constant_ctx.constant = hh_constant_ctx.heap;
  /* Store the word 0x01020304 to be the first one in the constant
     pool.  This is used to recognize the need for byte order swap
     after reading the program into the byte code interpreter. */
  *HH_ALLOCATE(&hh_constant_ctx, 1) = 0x01020304L;
}


void hh_grow_constant_ctx(unsigned long n_words)
{
  hh_word_t *new_heap;
  hh_word_t new_heap_n_words, new_heap_n_bytes;

  if (!HH_CAN_ALLOCATE(&hh_constant_ctx, n_words)) {
    /* We don't have a realloc... */
    new_heap_n_words = 3 * hh_constant_ctx.heap_n_words / 2  + n_words + 1024;
    new_heap_n_bytes = new_heap_n_words * sizeof(hh_word_t);
    new_heap = malloc(new_heap_n_bytes);
    if (new_heap == NULL)
      hh_fatal(NULL, "Out of memory when growing constant pool");
    memset(new_heap, 0, new_heap_n_bytes);
    memcpy(new_heap, hh_constant_ctx.heap,
	   (hh_constant_ctx.heap_ptr - hh_constant_ctx.heap)
	     * sizeof(hh_word_t));
    free(hh_constant_ctx.heap);
    hh_constant_ctx.heap_ptr =
      &new_heap[hh_constant_ctx.heap_ptr - hh_constant_ctx.heap];
    hh_constant_ctx.constant = hh_constant_ctx.heap = new_heap;
    hh_constant_ctx.heap_n_words = new_heap_n_words;
  }
}


static int hh_sp = 0;

static struct {
  hh_symbol_t *symbol;
  int sp;
} local_scope[1024], global_scope[1024]; /* XXX Magic constant, static limit */

static int local_scope_ix = 0;
static int global_scope_ix = 0;

static int current_fun_is_lifted = 0;


static hh_word_t hh_gen_quote(hh_ast_t *expr)
{
  hh_word_t cdr, car, *p;
  hh_string_t *string;
  int i;

  if (expr == NULL)
    return HH_NIL;

  switch (expr->arity) {
  case HH_AST_NIL:
    return HH_NIL;
  case HH_AST_SYMBOL:
    if (expr->u.symbol->name_ptr == HH_NIL) {
      /* First create into `string' the string representing the
	 symbol. */
      string = hh_ast_string(expr->u.symbol->name, 
			     strlen(expr->u.symbol->name));
      if (string->string_ptr == HH_NIL) {
	hh_grow_constant_ctx(HH_STRING_N_WORDS(string->n_bytes));
	string->string_ptr =
	  hh_box_string(&hh_constant_ctx, string->bytes, string->n_bytes);
      }
      /* Now create the symbol itself. */
      hh_grow_constant_ctx(HH_SYMBOL_N_WORDS);
      p = HH_ALLOCATE(&hh_constant_ctx, HH_SYMBOL_N_WORDS);
      p[0] = HH_SYMBOL_HDR;
      p[1] = string->string_ptr;
      expr->u.symbol->name_ptr = HH_PTR_TO_WORD(&hh_constant_ctx, p, 0x04);
    }
    return expr->u.symbol->name_ptr;
  case HH_AST_INTEGER:
    hh_grow_constant_ctx(HH_BOX_N_WORDS);
    return HH_SIGNED_TO_WORD(&hh_constant_ctx, expr->u.integer);
  case HH_AST_UNSIGNED_INTEGER:
    hh_grow_constant_ctx(HH_BOX_N_WORDS);
    return HH_UNSIGNED_TO_WORD(&hh_constant_ctx, expr->u.unsigned_integer);
  case HH_AST_STRING:
    if (expr->u.string->string_ptr == HH_NIL) {
      hh_grow_constant_ctx(HH_STRING_N_WORDS(expr->u.string->n_bytes));
      expr->u.string->string_ptr =
	hh_box_string(&hh_constant_ctx, expr->u.string->bytes,
		      expr->u.string->n_bytes);
    }
    return expr->u.string->string_ptr;
  default:
    cdr = HH_NIL;
    for (i = expr->arity - 1; i >= 0; i--) {
      car = hh_gen_quote(expr->u.ast[i]);
      hh_grow_constant_ctx(HH_CONS_N_WORDS);
      HH_CONS(&hh_constant_ctx, p, cdr, car, cdr);
    }
    return cdr;
  }
  HH_NOTREACHED;
}


static void hh_gen_list(hh_ast_t *list, int is_tail, int is_global);


static void hh_gen(hh_ast_t *expr, int is_tail)
{
  hh_insn_t cmp_insn;
  hh_imm_insn_t cmp_branch_insn;
  hh_code_t *code, *code2, *code3, **codes;
  int i, hh_sp_before;
  const char *name;

  switch (expr->arity) {
  case HH_AST_NIL:
    code = hh_alloc_code(expr);
    code->kind = HH_IMM;
    code->u.imm.insn = HH_IMM_load;
    code->u.imm.value = HH_NIL;
    break;
  case HH_AST_INTEGER:
    code = hh_alloc_code(expr);
    code->kind = HH_IMM;
    code->u.imm.insn = HH_IMM_load;
    hh_grow_constant_ctx(HH_BOX_N_WORDS);
    code->u.imm.value = HH_SIGNED_TO_WORD(&hh_constant_ctx, expr->u.integer);
    break;
  case HH_AST_UNSIGNED_INTEGER:
    code = hh_alloc_code(expr);
    code->kind = HH_IMM;
    code->u.imm.insn = HH_IMM_load;
    hh_grow_constant_ctx(HH_BOX_N_WORDS);
    code->u.imm.value = HH_UNSIGNED_TO_WORD(&hh_constant_ctx,
					    expr->u.unsigned_integer);
    break;
  case HH_AST_STRING:
    /* Put the string into the constant pool (unless already there),
       make a load insn to it. */
    if (expr->u.string->string_ptr == HH_NIL) {
      hh_grow_constant_ctx(HH_STRING_N_WORDS(expr->u.string->n_bytes));
      expr->u.string->string_ptr =
	hh_box_string(&hh_constant_ctx, expr->u.string->bytes,
		      expr->u.string->n_bytes);
    }
    code = hh_alloc_code(expr);
    code->kind = HH_IMM;
    code->u.imm.insn = HH_IMM_load;
    code->u.imm.value = expr->u.string->string_ptr;
    break;
  case HH_AST_SYMBOL:
    if (expr->u.symbol == hh_symbol_true) {
      code = hh_alloc_code(expr);
      code->kind = HH_IMM;
      code->u.imm.insn = HH_IMM_load;
      code->u.imm.value = HH_TRUE;
      goto found;
    }
    if (expr->u.symbol == hh_symbol_nil) {
      code = hh_alloc_code(expr);
      code->kind = HH_IMM;
      code->u.imm.insn = HH_IMM_load;
      code->u.imm.value = HH_NIL;
      goto found;
    }
    for (i = local_scope_ix - 1; i >= 0; i--)
      if (expr->u.symbol == local_scope[i].symbol) {
	/* Found the variable in the local scope. */
	code = hh_alloc_code(expr);
	code->kind = HH_IMM;
	code->u.imm.insn = HH_IMM_pick;
	code->u.imm.value = local_scope[i].sp - hh_sp;
	goto found;
      }
    for (i = global_scope_ix - 1; i >= 0; i--)
      if (expr->u.symbol == global_scope[i].symbol) {
	/* Symbol found in global scope.  It is in the bottom of
	   stack.  Note that the semantics of `HH_INSN_pick' check
	   the sign of the immediate value. */
	code = hh_alloc_code(expr);
	code->kind = HH_IMM;
	code->u.imm.insn = HH_IMM_pick;
	code->u.imm.value = global_scope[i].sp;
	goto found;
      }
    if (expr->u.symbol->fn_ptr != HH_NIL) {
      /* The symbol has a definition on the global function-scope. */
      code = hh_alloc_code(expr);
      code->kind = HH_IMM;
      code->u.imm.insn = HH_IMM_load;
      code->u.imm.value = expr->u.symbol->fn_ptr;
      goto found;
    }
    hh_fatal(expr, "Reference to unknown variable `%s'", expr->u.symbol->name);
  found:
    break;
  default:
    if (expr->u.ast[0]->arity == HH_AST_SYMBOL) {

      if (expr->u.ast[0]->u.symbol == hh_symbol_def
	  || expr->u.ast[0]->u.symbol == hh_symbol_fn) {

	/* Assuming that `def' appears only on the top-level, then
	   these have already been handled.  If this is not the
	   top-level, then we can choose to ignore them. */
	return;

#define MODULE(name)  /* Nothing. */
#define MODULE_END    /* Nothing. */
#define BUILTIN(lisp_name, c_name, doc_string, args, code_gen)		\
      } else if (expr->u.ast[0]->u.symbol == hh_symbol_ ## c_name) {	\
        name = (lisp_name);						\
	code_gen							\
        return;

/* Some shortcuts to be used in `hh_builtins.def's code generation
   blocks for the simplest primitives. */

#define GEN_INSN(arg_mnemonic, arg_arity)			\
      {								\
	if (expr->arity != (arg_arity) + 1)			\
	  hh_fatal(expr, "`%s' requires exactly %d argument",	\
		   name, (arg_arity));				\
        for (i = 1; i < (arg_arity); i++) {			\
	  hh_gen(expr->u.ast[i], 0);				\
	  code = hh_alloc_code(expr);				\
	  code->kind = HH_INSN;					\
	  code->u.insn = HH_INSN_push;				\
	  hh_sp++;						\
	}							\
	if ((arg_arity) != 0)					\
	  hh_gen(expr->u.ast[arg_arity], 0);			\
	code = hh_alloc_code(expr);				\
	code->kind = HH_INSN;					\
	code->u.insn = HH_INSN_ ## arg_mnemonic;		\
	if ((arg_arity) >= 2)					\
	  hh_sp -= (arg_arity) - 1;				\
      }
#define GEN_IMM(arg_mnemonic, arg_value, arg_arity)		\
      {								\
	if (expr->arity != (arg_arity) + 1)			\
	  hh_fatal(expr, "`%s' requires exactly %d argument",	\
		   name, (arg_arity));				\
        for (i = 1; i < (arg_arity); i++) {			\
	  hh_gen(expr->u.ast[i], 0);				\
	  code = hh_alloc_code(expr);				\
	  code->kind = HH_INSN;					\
	  code->u.insn = HH_INSN_push;				\
	  hh_sp++;						\
	}							\
	if ((arg_arity) != 0)					\
	  hh_gen(expr->u.ast[arg_arity], 0);			\
	code = hh_alloc_code(expr);				\
	code->kind = HH_IMM;					\
	code->u.imm.insn = HH_IMM_ ## arg_mnemonic;		\
	code->u.imm.value = (arg_value);			\
	if ((arg_arity) >= 2)					\
	  hh_sp -= (arg_arity) - 1;				\
      }

#include "hh_builtins.def"

      }
    }
    /* If we come here, then the head of the expression was not
       recognized to be a built-in primitive.  Evaluate all
       subexpressions and create a generic function application. */
    hh_sp_before = hh_sp;
    if (current_fun_is_lifted && !is_tail) {
      code = hh_alloc_code(expr);
      code->kind = HH_INSN;
      code->u.insn = HH_INSN_push_env;
      hh_sp++;
    }
    for (i = 0; i < expr->arity; i++) {
      if (i != 0) {
	code = hh_alloc_code(expr->u.ast[i]);
	code->kind = HH_INSN;
	code->u.insn = HH_INSN_push;
	hh_sp++;
      }
      hh_gen(expr->u.ast[i], 0);
    }
    code = hh_alloc_code(expr);
    if (is_tail) {
      code->kind = HH_IMM2;
      code->u.imm2.insn = HH_IMM_tailcall;
      /* Compute into `code->u.imm2.value1' the number of values we
	 shall remove from the stack.  These values include the
	 caller's arguments, local variables and intermediate
	 expression values, i.e. `hh_sp' in total before we pushed the
	 callee's arguments to the stack.  However, if there is
	 nothing pushed except the return address, then don't remove
	 anyhing from between. */
      if (expr->arity == 1)
	/* The callee is left in `accu', so don't remove it. */
	code->u.imm2.value1 = hh_sp_before;
      else
	code->u.imm2.value1 = hh_sp_before + 1;
      code->u.imm2.value2 = expr->arity - 1;
    } else {
      code->kind = HH_IMM;
      code->u.imm.insn = HH_IMM_call;
      code->u.imm.value = expr->arity - 1;
      if (current_fun_is_lifted) {
	code = hh_alloc_code(expr);
	code->kind = HH_INSN;
	code->u.insn = HH_INSN_pop_env;
	hh_sp--;
      }
    }
    hh_sp = hh_sp_before;
    break;
  }
}


static void hh_gen_list(hh_ast_t *list, int is_tail, int is_global)
{
  int hh_sp_backup = hh_sp;
  int local_scope_ix_backup = local_scope_ix;
  hh_code_t *code;
  hh_ast_t *next, *expr;

  while (list != NULL) {
    HH_ASSERT(list->arity == 2);
    expr = list->u.ast[0];
    next = list->u.ast[1];

    if (expr->arity != HH_AST_NIL
	&& expr->arity < HH_AST_ATOMS_START
	&& expr->u.ast[0]->arity == HH_AST_SYMBOL
	&& expr->u.ast[0]->u.symbol == hh_symbol_set) {
      /* A local value definition. */
      if (expr->arity != 3)
	hh_fatal(expr, "`set' expects exactly two arguments");
      if (expr->u.ast[1]->arity != HH_AST_SYMBOL)
	hh_fatal(expr, "The second argument of `set' must be a symbol");
      if (expr->u.ast[1]->u.symbol->is_builtin
	  && !HH_NODE_IS_IN_PRELUDE(expr))
	hh_fatal(expr, "Can't `set' builtin symbols except in the prelude");

      hh_gen(expr->u.ast[2], is_tail && next == NULL);
      if (next != NULL) {
	/* Push the value to the stack.  As a slight optimization, we
	   don't need to do this if there's no body left to use the
	   value in.  Therefore the expression `expr->u.ast[2]' above
	   could also be a tail-expression. */
	code = hh_alloc_code(expr);
	code->kind = HH_INSN;
	code->u.insn = HH_INSN_push;
	if (is_global) {
	  global_scope[global_scope_ix].symbol = expr->u.ast[1]->u.symbol;
	  global_scope[global_scope_ix].sp = hh_sp;
	  global_scope_ix++;
	} else {
	  local_scope[local_scope_ix].symbol = expr->u.ast[1]->u.symbol;
	  local_scope[local_scope_ix].sp = hh_sp;
	  local_scope_ix++;
	}
	hh_sp++;
      }

    } else
      /* Otherwise, just generate code for some computational
	 expression. */
      hh_gen(expr, is_tail && next == NULL);

    list = next;
  }

  if (hh_sp != hh_sp_backup) {
    code = hh_alloc_code(hh_ast_compiler_generated);
    code->kind = HH_IMM;
    code->u.imm.insn = HH_IMM_drop;
    code->u.imm.value = hh_sp - hh_sp_backup;
    hh_sp = hh_sp_backup;
  }
  local_scope_ix = local_scope_ix_backup;
}


static void hh_gen_def_cells(hh_ast_t *list)
{
  while (list != NULL) {
    hh_ast_t *expr, *args;
    hh_word_t *p;
    int n_args;

    HH_ASSERT(list->arity == 2);

    expr = list->u.ast[0];
    if (expr->arity != HH_AST_NIL
	&& expr->arity < HH_AST_ATOMS_START
	&& expr->u.ast[0]->arity == HH_AST_SYMBOL
	&& expr->u.ast[0]->u.symbol == hh_symbol_def) {
      /* This should be ensured by the reader. */
      HH_ASSERT(expr->arity == 3);

      /* Check the syntax of the argument list. */
      args = expr->u.ast[1];
      n_args = args->arity;
      if (n_args == HH_AST_SYMBOL)
	hh_fatal(args, "`def's of non-functions not yet implemented");
      if (n_args == HH_AST_NIL
	  || n_args > HH_AST_ATOMS_START
	  || args->u.ast[0]->arity != HH_AST_SYMBOL)
	hh_fatal(args, "Unrecognized form for `def'");
      if (n_args > 127)
	hh_fatal(args, "Too long argument list");

      if (args->u.ast[0]->u.symbol->is_builtin
	  && !HH_NODE_IS_IN_PRELUDE(expr))
	hh_fatal(expr, "Can't `def' builtin symbols except in the prelude");

      if (!args->u.ast[0]->u.symbol->is_used)
	/* The symbol isn't referred anywhere in reachable code,
	   therefore skip it to save memory. */
	goto next;

      if (args->u.ast[0]->u.symbol->fn_ptr != HH_NIL)
	hh_fatal(args, "Function `%s' multiply defined",
		 args->u.ast[0]->u.symbol->name);

      /* Allocate space for the function tuple. */
      hh_grow_constant_ctx(HH_CONS_N_WORDS);
      HH_CONS(&hh_constant_ctx, p, args->u.ast[0]->u.symbol->fn_ptr,
	      HH_NIL, HH_NIL);
    }

  next:
    list = list->u.ast[1];
  }
}


static void hh_gen_defs(hh_ast_t *list)
{
  while (list != NULL) {
    hh_ast_t *expr, *args, *n;
    hh_code_t *code;
    int n_args, i;

    HH_ASSERT(list->arity == 2);

    expr = list->u.ast[0];
    if (expr->arity != HH_AST_NIL
	&& expr->arity < HH_AST_ATOMS_START
	&& expr->u.ast[0]->arity == HH_AST_SYMBOL
	&& expr->u.ast[0]->u.symbol == hh_symbol_set) {

      /* Reveal another global symbol. */
      global_scope_ix++;

    } else if (expr->arity != HH_AST_NIL
	       && expr->arity < HH_AST_ATOMS_START
	       && expr->u.ast[0]->arity == HH_AST_SYMBOL
	       && expr->u.ast[0]->u.symbol == hh_symbol_def) {
      /* This should be ensured by the reader. */
      HH_ASSERT(expr->arity == 3);
   
      /* How many arguments does the function take? */
      args = expr->u.ast[1];
      n_args = args->arity;

      /* Check the syntax of the argument list.
	 XXX These were checked already in `hh_gen_def_cells'.
       */
      if (n_args == HH_AST_SYMBOL)
	hh_fatal(args, "`def's of non-functions not yet implemented");
      if (n_args == HH_AST_NIL
	  || n_args > HH_AST_ATOMS_START
	  || args->u.ast[0]->arity != HH_AST_SYMBOL)
	hh_fatal(args, "Unrecognized form for `def'");
      if (n_args > 127)
	hh_fatal(args, "Too long argument list");
      if (args->u.ast[0]->u.symbol->is_builtin
	  && !HH_NODE_IS_IN_PRELUDE(expr))
	hh_fatal(expr, "Can't `def' builtin symbols except in the prelude");

      if (!args->u.ast[0]->u.symbol->is_used)
	/* The symbol isn't referred anywhere in reachable code,
	   therefore skip it to save memory. */
	goto next;

      current_fun_is_lifted =
	HH_SYMBOL_IS_LIFTED_LAMBDA(args->u.ast[0]->u.symbol);

      /* Decrement out the function from the arg list. */
      n_args--;

      /* Generate a HH_FN header.  It stores information on the arity
	 of the function.  */
      code = hh_alloc_code(expr);
      code->kind = HH_FN;
      code->u.fn.n_args = n_args;
      code->u.fn.allow_excess_args = 0;

      /* Store in the code the pointer to the symbol object.  It is
	 used later, right before outputting the whole thing, to patch
	 the start address of the byte code to the function tuple. */
      code->u.fn.symbol = args->u.ast[0]->u.symbol;

      /* Put the arguments in the scope, check whether the function
	 takes extra arguments. */
      local_scope_ix = 0;
      for (i = 1; i <= n_args; i++) {
	n = args->u.ast[i];
	if (i == n_args && n->arity == 2) {
	  HH_ASSERT(n->u.ast[0]->arity == HH_AST_SYMBOL);
	  HH_ASSERT(n->u.ast[0]->u.symbol == hh_symbol_ellipsis);
	  HH_ASSERT(n->u.ast[1]->arity == HH_AST_SYMBOL);
	  code->u.fn.allow_excess_args = 1;
	  if (n->u.ast[1]->u.symbol->is_builtin)
	    hh_fatal(n, "Binding of a builtin symbol");
	  local_scope[local_scope_ix].symbol = n->u.ast[1]->u.symbol;
	} else {
	  if (n->arity != HH_AST_SYMBOL)
	    hh_fatal(n, "Expected a symbol");
	  if (n->u.symbol->is_builtin)
	    hh_fatal(n, "Binding of a builtin symbol");
	  local_scope[local_scope_ix].symbol = n->u.symbol;
	}
	local_scope[local_scope_ix].sp = i - 1;
	local_scope_ix++;
      }
      
      /* If there is something in `accu', then push it. */
      if (n_args != 0) {
	code = hh_alloc_code(expr);
	code->kind = HH_INSN;
	code->u.insn = HH_INSN_push;
      }
      hh_sp = n_args;

      /* Generate code for the body. */
      hh_gen_list(expr->u.ast[2], 1, 0);

      /* End the function with a return insn. */
      code = hh_alloc_code(expr);
      code->kind = HH_IMM;
      code->u.imm.insn = HH_IMM_return;
      code->u.imm.value = hh_sp + 1;
    }

  next:
    list = list->u.ast[1];
  }
}


hh_code_t *hh_gen_code(hh_ast_t *list)
{
  hh_code_first = hh_code_last = NULL;

  /* Create a dummy AST node to represent the compiler-generated
     insns. */
  hh_ast_compiler_generated = hh_alloc_node(HH_AST_NIL);
  hh_ast_compiler_generated->file = hh_n_files;
  hh_ast_compiler_generated->line = 0xFFFF;
  
  /* Create function tuples of top-level functions. */
  hh_gen_def_cells(list);
  /* Compile the top-level s-expression but not yet the defs. */
  current_fun_is_lifted = 0;
  hh_gen_list(list, 0, 1);
  /* Add an exit to the end of the whole program so that it won't run
     into trying to execute stuff from the constant pool. */
  hh_alloc_code(hh_ast_compiler_generated);
  hh_code_last->kind = HH_INSN;
  hh_code_last->u.insn = HH_INSN_exit;
  /* Compile the bodies of functions. */
  hh_gen_defs(list);
  return hh_code_first;
}


void hh_gen_init(int generate_debug_data)
{
  hh_init_constant_ctx();
  if (generate_debug_data) {
    /* Create debug header. */
    hh_word_t *debug_hdr;

    hh_grow_constant_ctx(3);
    debug_hdr = HH_ALLOCATE(&hh_constant_ctx, 3);
    debug_hdr[0] = HH_DEBUG_INFO_HDR_WORD;
    debug_hdr[1] = debug_hdr[2] = HH_NIL;
  }
}
