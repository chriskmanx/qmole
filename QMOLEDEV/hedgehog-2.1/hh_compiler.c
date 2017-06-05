/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004, 2005 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Authors: Kenneth Oksanen <cessu@iki.fi>
 *          Lars Wirzenius <liw@iki.fi>
 */

/* The main compiler driver file. */

#define HH_COMPILER  1

#include "hh_common.h"
#include "hh_ast.h"
#include "hh_macroexpand.h"
#include "hh_opt.h"
#include "hh_lambda.h"
#include "hh_uses.h"
#include "hh_codegen.h"
#include "hh_peephole.h"
#include "hh_output.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/types.h>
#include <unistd.h>
#include <dirent.h>
#ifndef sun
#include <getopt.h>
#endif


#ifndef DEFAULT_PRELUDE
#define DEFAULT_PRELUDE   "prelude.d"
#endif


void hh_print_compiler(const char *fmt, ...)
{
  va_list args;
  
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
}


static void *safe_malloc(size_t bytes)
{
  void *p;
  
  if (bytes == 0)
    bytes = 1;
  p = malloc(bytes);
  if (p == NULL) {
    fprintf(stderr, "malloc failed allocating %lu bytes\n",
	    (unsigned long) bytes);
    exit(1);
  }
  return p;
}


static char *make_output_name(const char *orig, const char *new_suffix)
{
  char *new, *slash, *dot;
  
  new = safe_malloc(strlen(orig) + strlen(new_suffix) + 1);
  strcpy(new, orig);
  
  /* Remove directories so that only basename remains. */
  for (;;) {
    slash = strrchr(new, '/');
    if (slash == NULL)
      break;
    if (slash[1] == '\0') {
      slash[0] = '\0';
      continue;
    }
    memmove(new, slash + 1, strlen(slash + 1) + 1);
  }
  
  /* Replace last existing suffix or add suffix, if none was there. */
  dot = strrchr(new, '.');
  if (dot == NULL)
    strcat(new, new_suffix);
  else
    strcpy(dot, new_suffix);
  
  return new;
}


static void hh_usage(const char *argv0)
{
  printf("\
This is hhc, the HedgeHog Lisp compiler.\n\
Usage: %s [options] file...\n\
  -h, --help             Display this help and exit\n\
  -D <name>              Define the given conditional compilation name.\n\
  -g                     Add debugging info to the compiled byte code.\n\
  -o <file>              Place the byte code program into the given file.\n\
                         The default is the last source code file name with\n\
                         the suffix replaced with \".hlo\".\n\
  -p, --prelude <file>   Name of the standard prelude file, or '-' if none.\n\
                         Default is \"%s\".\n\
  -x, --hex              Produce a hex dump of the byte code.\n\
                         If no output file is explicitely specified, \n\
                         write the hex dump to stdout.\n\
  -X, --hex-c            Produce a C constant containing the hex dump\n\
                         of the byte code.  If no output file is explicitely\n\
                         specified, write the hex dump to stdout.\n\
  -v, --verbose <level>  Dump various debugging information about the\n\
                         compilation process\n\
This program is distributed under the General Public License.\n\
Report bugs to <cessu@iki.fi>\n",
	 argv0, DEFAULT_PRELUDE);
  exit(0);
}


static int hh_verbose = 0;

#define MAX_N_PRELUDES 256
static char *hh_preludes[MAX_N_PRELUDES];
int hh_n_preludes = 0;

static int ends_with(const char *str, const char *pat)
{
  size_t len, patlen;
  
  len = strlen(str);
  patlen = strlen(pat);
  return len >= patlen && strcmp(str + len - patlen, pat) == 0;
}

static int cmp(const void *a, const void *b)
{
  return strcmp(*(const char **) a, *(const char **) b);
}

static void find_prelude_files(const char *dirname)
{
  DIR *dir;
  struct dirent *ent;
  char *tab[MAX_N_PRELUDES];
  int n, i;
  char *s;
  
  dir = opendir(dirname);
  if (dir == NULL) {
    perror("opendir failed");
    fprintf(stderr, "Can not open default prelude directory '%s'.\n", dirname);
    return;
  }

  n = 0;
  while ((ent = readdir(dir)) != NULL) {
    if (!ends_with(ent->d_name, ".hl"))
        continue;
    if (n >= MAX_N_PRELUDES) {
      fprintf(stderr, "Too many prelude files.\n");
      exit(1);
    }
    s = safe_malloc(strlen(dirname) + strlen(ent->d_name) + 2);
    sprintf(s, "%s/%s", dirname, ent->d_name);
    tab[n++] = s;
  }

  closedir(dir);

  if (hh_n_preludes + n >= MAX_N_PRELUDES) {
    fprintf(stderr, "Too many prelude files.\n");
    exit(1);
  }

  qsort(tab, n, sizeof(tab[0]), cmp);
  for (i = 0; i < n; ++i)
    hh_preludes[hh_n_preludes++] = tab[i];
}


#ifndef sun
static struct option hh_longopts[] = {
  { "prelude", required_argument, NULL, 'p' },
  { "verbose", optional_argument, NULL, 'v' },
  { "debug", no_argument, NULL, 'g' },
  { "hex", no_argument, NULL, 'x' },
  { "hex-c", no_argument, NULL, 'X' },
  { "help", no_argument, NULL, 'h' },
  { NULL, 0, NULL, 0 }
};
#endif


int main(int argc, char **argv)
{
  int c, i, generate_debug_data = 0, default_preludes_only = 1;
  hh_ast_t *module, *prog = NULL, *n;
  hh_code_t *insns;
  char *last_filename = NULL, *output_filename = NULL, *asm_filename;
  FILE *code_fp = NULL, *asm_fp;
  hh_output_type_t output_type = HH_BYTECODE;

  hh_n_preludes = 0;

#ifdef sun
  /* We can't assume getopt_long here. */
  while ((c = getopt(argc, argv, "D:p:o:v:hxXg")) != -1)
#else
  while ((c = getopt_long(argc, argv, "D:p:o:v:hxXg", hh_longopts, &optind))
	 != -1)
#endif
    switch (c) {
    case 'p':
      HH_ASSERT(optarg != NULL);
      if (strcmp(optarg, "-") == 0)
	hh_n_preludes = -1;
      else {
        if (default_preludes_only)
          hh_n_preludes = 0;
	default_preludes_only = 0;
	find_prelude_files(optarg);
      }
      break;
    case 'o':
      HH_ASSERT(optarg != NULL);
      output_filename = optarg;
      break;
    case 'v':
      if (optarg != NULL) {
	char *endptr;
	hh_verbose = strtol(optarg, &endptr, 0);
	if (endptr == optarg || *endptr != '\0') {
	  fprintf(stderr, "%s: option '-v' requires an integer argument\n",
		  argv[0]);
	  exit(1);
	}
      } else
	hh_verbose = 1;
      break;
    case 'g':
      generate_debug_data = 1;
      break;
    case 'x':
      output_type = HH_HEX;
      break;
    case 'X':
      output_type = HH_HEX_C;
      break;
    case 'D':
      HH_ASSERT(optarg != NULL);
      hh_directive_define(optarg);
      break;
    case 'h':
      hh_usage(argv[0]);
      HH_NOTREACHED;
      exit(0);
      break;
    }

  if (optind >= argc) {
    fprintf(stderr, "No input files given.\n");
    exit(1);
  }

  if (hh_n_preludes == 0)
    find_prelude_files(DEFAULT_PRELUDE);
  else if (hh_n_preludes == -1)
    hh_n_preludes = 0;
  
#ifdef HH_LINUX
  hh_directive_define("HH_LINUX");
#endif
#ifdef HH_SUNOS
  hh_directive_define("HH_SUNOS");
#endif
#ifdef HH_BSD
  hh_directive_define("HH_BSD");
#endif
#ifdef HH_UNIX
  hh_directive_define("HH_UNIX");
#endif

  hh_ast_init();
  hh_gen_init(generate_debug_data);

  /* Read prelude files. */
  for (i = 0; i < hh_n_preludes; i++) {
    if (hh_verbose)
      fprintf(stderr, "Reading prelude '%s'.\n", hh_preludes[i]);
    module = hh_ast_read_file(hh_preludes[i]);
    if (hh_verbose) {
      hh_ast_dump(module);
      fprintf(stderr, "\n");
    }
    if (prog == NULL)
      prog = module;
    else if (prog != NULL) {
      /* Append the module to the end of the whole program. */
      for (n = prog; n->u.ast[1] != NULL; n = n->u.ast[1])
	HH_ASSERT(n->arity == 2);
      n->u.ast[1] = module;
    }
  }

  /* Treat the rest of the arguments as input files.  Read them all
     into `prog'. */
  for (i = optind; i < argc; i++) {
    if (hh_verbose)
      fprintf(stderr, "Reading file %s:\n", argv[i]);
    last_filename = argv[i];
    module = hh_ast_read_file(argv[i]);
    if (hh_verbose) {
      hh_ast_dump(module);
      fprintf(stderr, "\n");
    }

    if (prog == NULL)
      prog = module;
    else if (prog != NULL) {
      /* Append the module to the end of the whole program. */
      for (n = prog; n->u.ast[1] != NULL; n = n->u.ast[1])
	HH_ASSERT(n->arity == 2);
      n->u.ast[1] = module;
    }
  }

  /* After lexing and parsing the program comes macro expansion,
     syntax tree level optimizations, uses-analysis which throws away
     unused code, linearization to byte code, and output.
     Some day we may add additional peephole optimization passes. */
  if (hh_verbose)
    fprintf(stderr, "After macro expansion:\n");
  hh_macroexpand(&prog, NULL, 0);
  if (hh_verbose >= 2) {
    hh_ast_dump(prog);
    fprintf(stderr, "\nAfter lambda-lifting:\n");
  }
  prog = hh_lambda(prog);
  if (hh_verbose >= 2) {
    hh_ast_dump(prog);
    fprintf(stderr, "\nAfter algebraic optimizations:\n");
  }
  prog = hh_opt(prog);
  if (hh_verbose) {
    hh_ast_dump(prog);
    fprintf(stderr, "\nFinal byte code as assembler:\n");
  }
  hh_uses(prog);
  insns = hh_gen_code(prog);
  insns = hh_peephole(insns);

  /* Output the compiled byte code in the specified format. */
  if (output_filename == NULL) {
    if (output_type == HH_BYTECODE) {
      if (last_filename == NULL) {
	fprintf(stderr,
		"No output or input file (other than prelude) specified.\n");
	exit(1);
      }
      output_filename = make_output_name(last_filename, ".hlo");
    } else
      /* We're generating the byte code dump in some more readable
	 form.  Throw it into stdout. */
      code_fp = stdout;
  }
  if (code_fp == NULL) {
    HH_ASSERT(output_filename != NULL);
    code_fp = fopen(output_filename, "wb");
    if (code_fp == NULL) {
      fprintf(stderr, "Could not open output file `%s' for writing.\n", 
	      output_filename);
      exit(1);
    }
  }

  asm_filename = make_output_name(last_filename, ".hls");
  asm_fp = fopen(asm_filename, "w");
  if (asm_fp == NULL) {
    fprintf(stderr, "Could not open output file `%s' for writing.\n",
	    asm_filename);
    exit(1);
  }

  hh_output(insns, code_fp, output_type, generate_debug_data, asm_fp);

  fclose(asm_fp);
  fclose(code_fp);

  return 0;
}
