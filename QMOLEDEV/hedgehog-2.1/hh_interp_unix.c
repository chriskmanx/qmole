/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004, 2005  Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Authors: Kenneth Oksanen <cessu@iki.fi>
 *          Lars Wirzenius <liw@iki.fi>
 */

/* This file implements the main program for the byte code interpreter
   for UNIX-like operating systems: command line argument parsing etc. */

#include "hh_common.h"
#include "hh_interp.h"
#include "hh_data.h"
#include "hh_printf.h"

#include <stdio.h>


#ifdef HH_USE_BOOT

/* Include the bootstrap code.
 */
#include "hh_boot.h"

#endif


/* How many steps should we perform before polling system events?
 */
#define STEPS 1000


#define DEFAULT_HEAP_N_WORDS  65536
#define DEFAULT_STACK_N_WORDS  1024

static long hh_heap_n_words = DEFAULT_HEAP_N_WORDS;
static long hh_stack_n_words = DEFAULT_STACK_N_WORDS;


#ifdef HH_TESTING
static int hh_insn_trace = 0;
static int hh_gc_trace = 0;
static int hh_verbose_interp = 0;
#endif
static int hh_profile = 0;
#ifdef HH_USE_BOOT
static int hh_use_bootstrap = 0;
#endif


/* A convenience routine to iterate `hh_interp_step' until the
   program exits, in which case this returns `HH_OK'. */

static hh_error_t hh_interp(hh_context_t *ctx)
{
  hh_error_t error;
  
  do {
    error = hh_interp_step(ctx, STEPS);
#ifdef HH_UNIX
    if (error == HH_OK && ctx->program_wants_to_select)
      ctx->select_retval = select(ctx->select_max_fd + 1,
				  &ctx->select_read_fds,
				  &ctx->select_write_fds,
				  NULL,
				  &ctx->select_timeout);
#endif
  } while (error == HH_OK);
  if (error == HH_ERROR_PROGRAM_EXITED)
    return HH_OK;
  return error;
}


/* The print function used by the interpreter, based on code in hh_printf.c
 */

static int hh_putc(char ch, void *ctx)
{
  return write(STDERR_FILENO, &ch, 1) == 1;
}

int hh_print_interpreter(const char *fmt, ...)
{
  int n;
  va_list args;
  
  va_start(args, fmt);
  n = hh_vprintf(hh_putc, NULL, fmt, args);
  va_end(args);
  return n;
}

int hh_lisp_print_interpreter(hh_context_t *ctx, hh_word_t word, int depth)
{
  hh_lisp_print_ctx_t lpx;

  lpx.ctx = ctx;
  lpx.depth = 0;
  lpx.max_depth = HH_LISP_PRINT_DEPTH_INCR * depth;
  return hh_lisp_print(hh_putc, &lpx, &word);
}


void hh_panic(const char *fmt, ...)
{
  va_list args;
  
  va_start(args, fmt);
  hh_vprintf(hh_putc, NULL, fmt, args);
  exit(1);
  /* Not needed: va_end(args); */
}


/* Read in a byte code file from the filesystem. Return number of bytes,
   or 0 for error. */

static size_t hh_read_from_file(const char *filename, unsigned char **program)
{
  struct stat statbuf;
  size_t n_bytes, n_bytes_read, n;
  int fd;

  /* Read in the byte code program file. */
  if (stat(filename, &statbuf) != 0) {
    HH_PRINT("`stat' failed on `%s'.\n", filename);
    return 0;
  }

  n_bytes = statbuf.st_size;
  *program = HH_MALLOC(n_bytes);
  if (program == NULL) {
    HH_PRINT("Failed to allocate %d bytes for the byte code program.\n",
	     n_bytes);
    return 0;
  }

  fd = open(filename, O_RDONLY);
  if (fd < 0) {
    HH_PRINT("Failed to open `%s'.\n", filename);
    return 0;
  }

  for (n_bytes_read = 0; n_bytes_read < n_bytes; n_bytes_read += n) {
    n = read(fd, *program + n_bytes_read, n_bytes - n_bytes_read);
    if (n < 0) {
      HH_PRINT("Failed to read `%s'.\n", filename);
      close(fd);
      HH_FREE(*program);
      return 0;
    }
  }

  close(fd);

  return n_bytes;
}


#ifdef HH_SMALL

int main(int argc, char **argv)
{
  hh_context_t *ctx;
  hh_error_t error;
  unsigned char *program;
  size_t n_bytes;

  if (argc == 4) {
    hh_heap_n_words = atoi(argv[1]);
    hh_stack_n_words = atoi(argv[2]);
    argv += 2;
  } else if (argc != 2) {
    HH_PRINT("Usage: %s program.hlo\n"
	     "or %s heap_n_words stack_n_words program.hlo\n",
	     argv[0], argv[0]);
    exit(1);
  }

  n_bytes = hh_read_from_file(argv[1], &program);
  if (n_bytes == 0)
    return 1;

  /* Clear the byte code program for execution. */
  if (hh_program_check(program, n_bytes) != HH_OK) {
    HH_PRINT("Program check failed.\n");
    return 1;
  }
    
  ctx = hh_context_allocate(program, hh_heap_n_words,
			    hh_stack_n_words, hh_profile);
  if (ctx == NULL) {
    HH_PRINT("hh_context_allocate failed.\n");
    return 1;
  }
  
  /* Run the byte code program. */
  error = hh_interp(ctx);
  if (error != HH_OK && error != HH_ERROR_PROGRAM_DID_EXEC) {
    hh_error_print(error, ctx);
    HH_BACKTRACE(ctx);
  }
  
  /* Cleanup. */
  hh_context_free(ctx);

  return 0;
}


#else /* Not HH_SMALL */


static void hh_usage(const char *argv0)
{
  HH_PRINT("\
This is hhi, the HedgeHog Lisp byte code interpreter version %d.%d.%d\n\
Usage: %s [options] byte-code-file\n\
  -h, --help             Display this help and exit\n\
  -H, --heap <n_words>   Lisp heap semispace size, in words\n\
                         Default is %u.\n\
  -S, --stack <n_words>  Stack size, in words.  Default is %u.\n\
  -V, --version          Display version and other info and exit.\n"
#ifdef HH_USE_BOOT
"  -b, --bootstrap        Use built-in bootstrap code.\n"
#endif
#ifdef HH_TESTING
"  -g, --gc-trace         Enable garbage collection messages.\n\
  -i, --insn-trace       Enable byte code instruction trace.\n\
  -p, --profile          Enable byte code instruction profiling.\n\
  -v, --verbose          Verbose execution: print useful information,\n\
                         such as highest stack depth encountered.\n"
#endif
"Most of the implementation by Kenneth Oksanen <cessu@iki.fi>\n\
Language design and part of the code due to Lars Wirzenius.\n\
Contact: <support@oliotalo.fi>\n",
	   HEDGEHOG_IMPLEMENTATION_VERSION_MAJOR,
	   HEDGEHOG_IMPLEMENTATION_VERSION_MINOR,
	   HEDGEHOG_IMPLEMENTATION_VERSION_PATCH,
	   argv0, DEFAULT_HEAP_N_WORDS, DEFAULT_STACK_N_WORDS);
  exit(0);
}


static void hh_version(void)
{
  HH_PRINT("\
HedgeHog lisp interpreter v %d.%d.%d\n\
Default heap size = %u words.\n\
Default stack size = %u words.\n\
Number of insns = %d of 64.\n\
Number of immediate insns = %d of 64.\n",
	   HEDGEHOG_IMPLEMENTATION_VERSION_MAJOR,
	   HEDGEHOG_IMPLEMENTATION_VERSION_MINOR,
	   HEDGEHOG_IMPLEMENTATION_VERSION_PATCH,
	   DEFAULT_HEAP_N_WORDS, DEFAULT_STACK_N_WORDS,
	   HH_NUMBER_OF_INSNS, HH_NUMBER_OF_IMMS);
  exit(0);
}


#ifndef HH_SUNOS

#include <getopt.h>

static struct option hh_options[] = {
  { "help", no_argument, NULL, 'h' },
  { "heap", required_argument, NULL, 'H' },
  { "stack", required_argument, NULL, 'S' },
  { "version", no_argument, NULL, 'V' },
#ifdef HH_USE_BOOT
  { "bootstrap", required_argument, NULL, 'b' },
#endif
#ifdef HH_TESTING
  { "gc-trace", no_argument, NULL, 'g' },
  { "insn-trace", no_argument, NULL, 'i' },
  { "profile", no_argument, NULL, 'p' },
  { "verbose", required_argument, NULL, 'v' },
#endif
  { NULL, 0, NULL, 0 }
};

#endif


int main(int argc, char **argv)
{
  hh_context_t *ctx;
  hh_error_t error;
  unsigned char *program;
  size_t n_bytes;
  int c;
  long i, stack_n_words;
  char *endptr;

#ifdef HH_SUNOS
  while ((c = getopt(argc, argv, "hVH:S:bgips:c:vn:")) != -1)
#else
  while ((c = getopt_long(argc, argv, "hVH:S:bgips:c:vn:", hh_options, &optind))
	 != -1)
#endif
    switch (c) {
    case 'h':
      hh_usage(argv[0]);
      HH_NOTREACHED;
      exit(0);
      break;
    case 'V':
      hh_version();
      exit(0);
      break;
    case 'H':
      hh_heap_n_words = strtol(optarg, &endptr, 0);
      if (endptr == optarg || *endptr != '\0') {
	HH_PRINT("%s: option '-H' requires an integer argument\n", argv[0]);
	exit(1);
      }
      if (hh_heap_n_words <= 0) {
	HH_PRINT("%s: option '-H' requires a positive argument\n", argv[0]);
	exit(1);
      }
      break;
    case 'S':
      hh_stack_n_words = strtol(optarg, &endptr, 0);
      if (endptr == optarg || *endptr != '\0') {
	HH_PRINT("%s: option '-S' requires an integer argument\n", argv[0]);
	exit(1);
      }
      if (hh_stack_n_words <= 0) {
	HH_PRINT("%s: option '-S' requires a positive argument\n", argv[0]);
	exit(1);
      }
      break;
#ifdef HH_TESTING
    case 'g':
      hh_gc_trace = 1;
      break;
    case 'i':
      hh_insn_trace = 1;
      break;
    case 'p':
      hh_profile = 1;
      break;
    case 'v':
      hh_verbose_interp = 1;
      break;
#endif
#ifdef HH_USE_BOOT
    case 'b':
      hh_use_bootstrap = 1;
      break;
#endif
    }

  if (optind + 1 < argc) {
    HH_PRINT("Too many arguments.\n");
    exit(1);
  }

  do {
    if (optind < argc)
      n_bytes = hh_read_from_file(argv[optind], &program);
#ifdef HH_USE_BOOT
    else if (hh_use_bootstrap) {
      n_bytes = sizeof(hh_bootstrap.code);
      program = hh_bootstrap.code;
    }
#endif
    else {
      HH_PRINT("No byte code file specified.\n");
      exit(1);
    }
    if (n_bytes == 0)
      return 1;

    /* Clear the byte code program for execution. */
    if (hh_program_check(program, n_bytes) != HH_OK) {
      HH_PRINT("Program check failed.\n");
      return 1;
    }
    
    ctx = hh_context_allocate(program, hh_heap_n_words, 
			      hh_stack_n_words, hh_profile);
    if (ctx == NULL) {
      HH_PRINT("hh_context_allocate failed.\n");
      return 1;
    }
#ifdef HH_TESTING
    ctx->gc_trace_enabled = hh_gc_trace;
    ctx->insn_trace_enabled = hh_insn_trace;
#endif
  
    /* Run the byte code program. */
    error = hh_interp(ctx);
    if (error != HH_OK && error != HH_ERROR_PROGRAM_DID_EXEC) {
      hh_error_print(error, ctx);
      HH_BACKTRACE(ctx);
    }
  
    /* Cleanup. */
    stack_n_words = ctx->stack_n_words;
    i = hh_context_free(ctx);
#ifdef HH_TESTING
    if (hh_verbose_interp) {
      HH_PRINT("Maximum stack depth reached was %lu of %u words\n",
	       i + 1, stack_n_words);
    }
    if (i == stack_n_words - 1)
      HH_PRINT("Stack may have been overwritten.\n");
#endif
  } while (error == HH_ERROR_PROGRAM_DID_EXEC);

  return 0;
}

#endif  /* End of not HH_SMALL */
