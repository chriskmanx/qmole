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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>


extern int hh_lex(void);


hh_symbol_t *hh_symbols = NULL;

static int hh_in_ast_init = 1;

/* Currently the symbol table is a simple linked list.  In future we
   may have to replace it with a hash table, if interning becomes a
   problem. */

hh_symbol_t *hh_ast_symbol(const char *name)
{
  hh_symbol_t *sym;
  static unsigned int number = 0;

  HH_ASSERT(name != NULL);

  /* Look up for `name' in the symbol table. */
  for (sym = hh_symbols; sym != NULL; sym = sym->next)
    if (strcmp(name, sym->name) == 0)
      return sym;
  /* Not found, create one. */
  sym = malloc(sizeof(hh_symbol_t) + strlen(name));
  if (sym == NULL) {
    fprintf(stderr, "Out of memory when interning symbol `%s'\n", name);
    exit(1);
  }
  memset(sym, 0, sizeof(hh_symbol_t));
  sym->name_ptr = sym->fn_ptr = HH_NIL;
  strcpy(sym->name, name);
  sym->number = number++;
  sym->is_builtin = hh_in_ast_init;
  sym->next = hh_symbols;
  hh_symbols = sym;
  return sym;
}

hh_symbol_t 
#define MODULE(name)  /* Nothing. */
#define MODULE_END    /* Nothing. */
#define BUILTIN(lisp_name, c_name, doc_string, args, code_gen) \
  *hh_symbol_ ## c_name,

#include "hh_builtins.def"

  *hh_symbol_out_of_memory, *hh_symbol_def, *hh_symbol_fn, *hh_symbol_dot_do,
  *hh_symbol_macroconcat, *hh_symbol_macroquote,
  *hh_symbol_true, *hh_symbol_nil, *hh_symbol_defs, *hh_symbol_ellipsis;


hh_string_t *hh_strings = NULL;

hh_string_t *hh_ast_string(const char *string, unsigned int n_bytes)
{
  hh_string_t *s;

  HH_ASSERT(string != NULL);
  
  /* Look up for `string' in the already interned strings. */
  for (s = hh_strings; s != NULL; s = s->next)
    if (n_bytes == s->n_bytes && memcmp(string, s->bytes, n_bytes) == 0)
      return s;
  /* Not found, create one. */
  s = malloc(sizeof(hh_string_t) + n_bytes - 1);
  if (s == NULL) {
    fprintf(stderr, "Out of memory when interning string `%s'\n", string);
    exit(1);
  }
  memset(s, 0, sizeof(hh_string_t) - 1);
  memcpy(s->bytes, string, n_bytes);
  s->n_bytes = n_bytes;
  s->next = hh_strings;
  s->string_ptr = HH_NIL;
  hh_strings = s;
  return s;
}


const char *hh_filename[256];
unsigned int hh_n_files = -1;
unsigned int hh_current_line;

void hh_fatal(hh_ast_t *node, const char *fmt, ...)
{
  va_list args;
  
  if (node != NULL)
    fprintf(stderr, "%s:%d: ", hh_filename[node->file], node->line);
  else
    fprintf(stderr, "%s:%d: ", HH_CURRENT_FILENAME, hh_current_line);
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fprintf(stderr, ".\n");
  exit(1);
}


hh_ast_t *hh_alloc_node(unsigned int arity)
{
  static hh_word_t *alloc_buf, *alloc_buf_end;
  hh_word_t *p;
  hh_ast_t *n;
  int n_words;

  /* Check some assumptions on word and pointer size.  Admittably this
     code does not work on 64-bit machines, but that is fixable when
     needed. */
  HH_ASSERT(sizeof(hh_word_t) == 4);
  HH_ASSERT(sizeof(hh_word_t *) == 4);

  n_words = (sizeof(hh_ast_t) + sizeof(hh_word_t) - 1) / sizeof(hh_word_t);
  if (arity == HH_AST_NIL)
    n_words--;
  else if (arity < HH_AST_ATOMS_START)
    n_words += arity - 1;
  if (alloc_buf + n_words <= alloc_buf_end) {
    p = alloc_buf;
    alloc_buf += n_words;
  } else {
    p = malloc(1024 * sizeof(hh_word_t));
    if (p == NULL)
      hh_fatal(NULL, "Malloc failed");
    alloc_buf_end = p + 1024;
    alloc_buf = p + n_words;
  }
  n = (hh_ast_t *) p;
  n->arity = arity;
  n->file = hh_n_files;
  n->line = hh_current_line;
  return n;
}


void hh_ast_copy_location(hh_ast_t *to, hh_ast_t *from)
{
  to->file = from->file;
  to->line = from->line;
}


/* Dump an expression to stderr.  This is used only for debugging. */

void hh_ast_dump(hh_ast_t *n)
{
  if (n == NULL) {
    fprintf(stderr, "NULL");
    return;
  }
  /* fprintf(stderr, "{%s:%d}", hh_filename[n->file], n->line); */
  if (n->arity == HH_AST_NIL)
    fprintf(stderr, "()");
  else if (n->arity == HH_AST_STRING)
    fprintf(stderr, "\"%s\"", n->u.string->bytes);
  else if (n->arity == HH_AST_SYMBOL)
    fprintf(stderr, "%s", n->u.symbol->name);
  else if (n->arity == HH_AST_INTEGER)
    fprintf(stderr, "%ld", n->u.integer);
  else if (n->arity == HH_AST_UNSIGNED_INTEGER)
    fprintf(stderr, "0x%lX", (unsigned long) n->u.unsigned_integer);
  else {
    int i;

    fprintf(stderr, "(");
    for (i = 0; i < n->arity - 1; i++) {
      hh_ast_dump(n->u.ast[i]);
      fprintf(stderr, " ");
    }
    hh_ast_dump(n->u.ast[n->arity - 1]);
    fprintf(stderr, ")");
  }
}


/* Read in an s-expression, return NULL on legal EOF. */

static hh_ast_t *hh_ast_read_expr(int allow_eof);

static hh_ast_t *hh_ast_read_expr_list(void)
{
  hh_ast_t *n, *n_list, *tmp;

  n_list = NULL;
  do {
    n = hh_ast_read_expr(0);
    if (n != NULL) {
      tmp = hh_alloc_node(2);
      tmp->u.ast[0] = n;
      tmp->u.ast[1] = n_list;
      n_list = tmp;
    }
  } while (n != NULL);
  if (n_list == NULL)
    hh_fatal(NULL, "Empty expression list");
  /* Destructively reverse `n_list'. */
  HH_ASSERT(n == NULL);
  while (n_list != NULL) {
    tmp = n_list->u.ast[1];
    n_list->u.ast[1] = n;
    n = n_list;
    n_list = tmp;
  }
  return n;
}

static hh_ast_t *hh_ast_read_expr(int allow_eof)
{
  switch (hh_lex()) {
  case '(':
    {
      hh_ast_t *subexprs[256], *n;
      int n_subexprs = 0, have_seen_ellipsis = 0, lineno = hh_current_line;

      while ((subexprs[n_subexprs] = n = hh_ast_read_expr(0)) != NULL) {
	n_subexprs++;
	/* Handle special forms that expect very long s-expressions,
	   possibly of arity over `HH_AST_ATOMS_START' and turn them
	   into NULL-terminated lists. */
	if (n_subexprs == 1
	    && subexprs[0]->arity == HH_AST_SYMBOL
	    && subexprs[0]->u.symbol == hh_symbol_do) {
	  subexprs[n_subexprs++] = hh_ast_read_expr_list();
	  goto out;
	}
	if (n_subexprs == 2
	    && subexprs[0]->arity == HH_AST_SYMBOL
	    && (subexprs[0]->u.symbol == hh_symbol_def
		|| subexprs[0]->u.symbol == hh_symbol_defs
		|| subexprs[0]->u.symbol == hh_symbol_fn)) {
	  subexprs[n_subexprs++] = hh_ast_read_expr_list();
	  goto out;
	}
	if (n_subexprs >= HH_AST_ATOMS_START)
	  hh_fatal(NULL, "Too long s-expression");
	if (have_seen_ellipsis)
	  hh_fatal(NULL, "Ellipsis `...' must be followed by one symbol "
		   "and then the end of list");
	if (n->arity == 2
	    && n->u.ast[0]->arity == HH_AST_SYMBOL
	    && n->u.ast[0]->u.symbol == hh_symbol_ellipsis)
	  have_seen_ellipsis = 1;
      }
    out:
      n = hh_alloc_node(n_subexprs);
      n->line = lineno;
      memcpy(n->u.ast, subexprs, n_subexprs * sizeof(hh_ast_t *));
      return n;
    }
    break;
  case HH_ELLIPSIS:
    {
      hh_ast_t *n = hh_alloc_node(2);

      if (allow_eof)
	hh_fatal(NULL, "Ellipsis `...' can occur only in lists");
      n->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
      n->u.ast[0]->u.symbol = hh_symbol_ellipsis;
      n->u.ast[1] = hh_ast_read_expr(0);
      if (n->u.ast[1] == NULL || n->u.ast[1]->arity != HH_AST_SYMBOL)
	hh_fatal(NULL, "Ellipsis `...' must be followed by an atom");
      return n;
    }
    break;
  case '\'':
    {
      hh_ast_t *n = hh_alloc_node(2);

      n->u.ast[0] = hh_alloc_node(HH_AST_SYMBOL);
      n->u.ast[0]->u.symbol = hh_symbol_quote;
      n->u.ast[1] = hh_ast_read_expr(0);
      return n;
    }
    break;
  case HH_ATOM:
    return hh_lval;
    break;
  case ')':
    if (allow_eof)
      hh_fatal(NULL, "Stray closing parenthesis");
    else
      return NULL;
    break;
  case EOF:
    if (allow_eof)
      return NULL;
    else
      hh_fatal(NULL, "Missing closing parentheses");
    break;
  default:
    hh_fatal(NULL, "Syntax error");
  }
  HH_NOTREACHED;
  return NULL;
}


/* Read in the given file, and return a list of the s-expressions in
   it. */

hh_ast_t *hh_ast_read_file(const char *filename)
{
  hh_ast_t *n, *n_list, *tmp;
  /* This is defined by flex in `hh_lex.c'. */
  extern FILE *hh_in;

  if (++hh_n_files == 256) {
    fprintf(stderr, "Too many input files.\n");
    exit(1);
  }
  hh_filename[hh_n_files] = filename;
  hh_in = fopen(filename, "r");
  if (hh_in == NULL) {
    fprintf(stderr, "Could not open file \"%s\"\n", filename);
    exit(1);
  }
  hh_current_line = 1;
  /* Read into `n_list' all the definitions in the file. */
  n_list = NULL;
  do {
    n = hh_ast_read_expr(1);
    if (n != NULL) {
      tmp = hh_alloc_node(2);
      tmp->u.ast[0] = n;
      tmp->u.ast[1] = n_list;
      n_list = tmp;
    }
  } while (n != NULL);
  /* Destructively reverse `n_list'. */
  HH_ASSERT(n == NULL);
  while (n_list != NULL) {
    tmp = n_list->u.ast[1];
    n_list->u.ast[1] = n;
    n = n_list;
    n_list = tmp;
  }
  n_list = n;
  /* Close file and return. */
  fclose(hh_in);
  return n_list;
}


int hh_n_catch_tags = 0;


void hh_ast_init(void)
{
  hh_symbol_out_of_memory = hh_ast_symbol("out-of-memory-exception");
  hh_symbol_out_of_memory->catch_tag = ++hh_n_catch_tags;
  HH_ASSERT(hh_symbol_out_of_memory->catch_tag == 1);

  hh_symbol_def = hh_ast_symbol("def");
  hh_symbol_defs = hh_ast_symbol("def-syntax");
  hh_symbol_dot_do = hh_ast_symbol(".do");
  hh_symbol_macroconcat = hh_ast_symbol("##");
  hh_symbol_macroquote = hh_ast_symbol("#'");
  hh_symbol_fn = hh_ast_symbol("fn");
  hh_symbol_true = hh_ast_symbol("t");
  hh_symbol_nil = hh_ast_symbol("nil");
  hh_symbol_ellipsis = hh_ast_symbol("...");

#define MODULE(name)  /* Nothing. */
#define MODULE_END    /* Nothing. */
#define BUILTIN(lisp_name, c_name, doc_string, args, code_gen) \
  hh_symbol_ ## c_name = hh_ast_symbol(lisp_name);

#include "hh_builtins.def"

  hh_in_ast_init = 0;
}
