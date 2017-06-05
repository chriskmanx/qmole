/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

/* Abstract Syntax Tree for the compiler.
 */

#ifndef HH_INCL_AST
#define HH_INCL_AST  1


#include "hh_common.h"


/* Forward declaration. */
typedef struct hh_ast_t hh_ast_t;


/* Symbols. */

typedef struct hh_symbol_t {
  /* Is the symbol quoted anywhere?  If it is, then it might be
     printed, and we have to add its name into the constant pool of
     the program file, and this ptr tells where it is.  It remains
     HH_NIL if the symbol is not quoted. */
  hh_word_t name_ptr;
  /* Is the symbol a name of a function?  If so, then this is the
     pointer to its function tuple in the constant pool. */
  hh_word_t fn_ptr;
  /* If the symbol's value is ever asked in the global scope, then
     `is_used' is set to two.  The value one is used internally by the
     uses-analysis. */
  unsigned char is_used;
  /* Is this a symbol of a builtin, which can only be redefined in the
     prelude? */
  unsigned char is_builtin;
  /* If this symbol is used as a catch tag, then this is its non-zero
     numerical value. */
  unsigned int catch_tag;
  /* A unique integer for this symbol. */
  unsigned int number;
  /* A pointer used internally by the symbol table's data
     structures. */
  struct hh_symbol_t *next;
  /* The symbol's name, written extending this struct.  Null-char
     terminated. */
  char name[1];
} hh_symbol_t;

extern hh_symbol_t *hh_symbols;

/* "Intern" the given string into a symbol, i.e. if the same string
   has not yet been interned then allocate and initialize a
   `hh_symbol_t' for it, otherwise return the already created
   `hh_symbol_t'.  This makes a private copy of `name'. */
hh_symbol_t *hh_ast_symbol(const char *name);


/* Symbols for builtins and special forms. */

extern hh_symbol_t 
#define MODULE(name)  /* Nothing. */
#define MODULE_END    /* Nothing. */
#define BUILTIN(lisp_name, c_name, doc_string, args, code_gen) \
  *hh_symbol_ ## c_name,

#include "hh_builtins.def"

  *hh_symbol_out_of_memory, *hh_symbol_def, *hh_symbol_fn, *hh_symbol_dot_do,
  *hh_symbol_macroconcat, *hh_symbol_macroquote,
  *hh_symbol_true, *hh_symbol_nil, *hh_symbol_defs, *hh_symbol_ellipsis;


/* String constants. */

typedef struct hh_string_t {
  struct hh_string_t *next;
  hh_word_t string_ptr;
  unsigned int n_bytes;
  char bytes[1];		/* Not necessarily null-char-terminated. */
} hh_string_t;

extern hh_string_t *hh_strings;

/* This is similar to interning symbols. */
hh_string_t *hh_ast_string(const char *string, unsigned int n_bytes);


/* And finally we get to the AST definitions. */

typedef enum {
  HH_AST_NIL = 0,
  /* The values from 1 onwards are arities of s-expression. */
  /* The last values represent terminal nodes, such as constants and
     symbols. */
  HH_AST_ATOMS_START = 251,
  HH_AST_STRING = 252,
  HH_AST_SYMBOL = 253,
  HH_AST_INTEGER = 254,
  HH_AST_UNSIGNED_INTEGER = 255
} hh_ast_kind_t;


struct hh_ast_t {
  /* What is the arity of this node?  If the arity is zero, the node
     is a terminal node, i.e. either a constant or symbol. */
  unsigned int arity : 8;
  /* The file from which this AST node was read from.  This is an
     index to the `hh_filename' array in order to save space. */
  unsigned int file  : 8;
  /* What line number did this AST node come from. */
  unsigned int line  : 16;
  /* XXX */
  union {
    hh_signed_word_t integer;
    hh_word_t unsigned_integer;
    hh_symbol_t *symbol;
    hh_string_t *string;
    struct hh_ast_t *ast[1];
  } u;
};

hh_ast_t *hh_alloc_node(unsigned int arity);

/* Does this AST node originate from the prelude file?  Some
   overridings etc. are possible only in the prelude. */
extern int hh_n_preludes;
#define HH_NODE_IS_IN_PRELUDE(ast)  ((ast)->file < hh_n_preludes)

void hh_ast_copy_location(hh_ast_t *to, hh_ast_t *from);

/* Read in the given HogLisp file, and return a list of the
   s-expressions in it.  */
hh_ast_t *hh_ast_read_file(const char *filename);

/* Dump an expression to stderr.  This is used only for debugging. */
void hh_ast_dump(hh_ast_t *n);


/* This must be called before anything else.  It, for example, interns
   the built-in symbols. */
void hh_ast_init(void);


/* Routines and definitions to be used by the lexer: file position and
   error reporting information. */

extern const char *hh_filename[];
extern unsigned int hh_n_files, hh_current_line;
#define HH_CURRENT_FILENAME  hh_filename[hh_n_files]


void hh_fatal(hh_ast_t *node, const char *fmt, ...);


/* Normally these would be defined by the yacc-generated header. */

hh_ast_t *hh_lval;
#define HH_ATOM      257
#define HH_ELLIPSIS  258

void hh_directive_define(const char *name);


extern int hh_n_catch_tags;


#endif /* !HH_INCL_AST */
