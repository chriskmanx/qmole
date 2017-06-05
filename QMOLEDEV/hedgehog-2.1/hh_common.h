/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004, 2005 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Authors: Kenneth Oksanen <cessu@iki.fi>
 *          Lars Wirzenius <liw@iki.fi>
 */

/* Some common routines for both the byte code compiler and the byte
   code interpreter.  The CFLAGS may define platform-specific values
   to macros
     HH_WORD_T:  C type of the 32-bit integer values.
     HH_TESTING: Defined if various run-time checks should be included in the
                 byte code interpreter.  This also increases the executable
		 size and slows down the interpreter significantly.
     HH_SMALL:   May be used to reduce executable size when HH_TESTING 
                 is not defined.
 */


#ifndef HH_INCL_COMMON
#define HH_INCL_COMMON  1


#ifndef HH_COMPILER
/* Add here definitions specific for your platform, e.g. stuff like
     #define HH_MEMMOVE  my_own_memmove_because_the_system_memcpy_is_broken
 */
#endif


/* Types for 32-bit values in the target. */
#ifdef HH_WORD_T
typedef unsigned HH_WORD_T hh_word_t;
typedef signed HH_WORD_T hh_signed_word_t;
#else
typedef unsigned long hh_word_t;
typedef signed long hh_signed_word_t;
#endif


#ifdef HH_LINUX
#define HH_UNIX  1
#endif

#ifdef HH_SUNOS
#define HH_UNIX  1
#endif

#ifdef HH_BSD
#define HH_UNIX  1
#endif


/* Collected include liturgy here. */

#include <stdlib.h>
#include <errno.h>
#include <dirent.h>
#include <sys/select.h>

#ifndef HH_COMPILER

/* Some possibly system-dependent includes for the interpreter. */
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
#include <wait.h>

#ifdef HH_UNIX
#include <sys/stat.h>
#ifdef HH_BSD
#include <sys/param.h>
#endif
#include <sys/mount.h>
#ifdef HH_SUNOS
#include <sys/mntent.h>
#include <stropts.h>
#else
#include <sys/ioctl.h>
#endif
#include <sys/socket.h>
#include <termios.h>
#include <time.h>
#include <netinet/in.h>
#endif

#endif


#ifdef HH_TESTING

#ifndef HH_ASSERT
#ifdef NDEBUG
#define HH_ASSERT(expr)  ((void) 0)
#else
#define HH_ASSERT_WITH_POS(expr, file, line)				     \
  ((void) ((expr)							     \
	   || (hh_panic("%s:%ld:%s\n", file, (long) line, #expr), 0)))
#define HH_ASSERT(expr)  (HH_ASSERT_WITH_POS(expr, __FILE__, __LINE__))
void hh_panic(const char *fmt, ...);  /* Defined in hh_interp_*.c */
#endif
#endif /* HH_ASSERT */

#else

#ifndef HH_ASSERT
#define HH_ASSERT(expr)  ((void) 0)
#endif

#endif

#ifndef HH_NOTREACHED
#define HH_NOTREACHED   HH_ASSERT(0)
#endif


#ifndef HH_BACKTRACE
#ifdef HH_SMALL
#define HH_BACKTRACE(ctx)  /* Does not exist. */
#else
#define HH_BACKTRACE(ctx)  hh_backtrace(ctx)
#endif
#endif


/* Calls to malloc, free, memcmp, memcpy and memmove are all hidden
   behind macros.  This may help to circumvent incorrect
   implementations of them in various platforms, or to define a layer
   in between Hedgehog's memory allocation and the system's default
   allocator. */

#ifndef HH_MALLOC
#define HH_MALLOC(n_bytes)  malloc(n_bytes)
#endif
#ifndef HH_FREE
#define HH_FREE(obj)        free(obj)
#endif

#include <string.h>
#ifndef HH_MEMCMP
/* HH_MEMCMP should compare the bytes in p[0..n_bytes-1] and
   q[0..n_bytes-1] as unsigned chars.  If this is not the case, the
   person porting should define memcmp for example as follows:
     int hh_memcmp(void *p, void *q, unsigned long n)
     {
       unsigned char *up = (unsigned char *) p;
       unsigned char *uq = (unsigned char *) q;
       while (n-- > 0) {
         if (*up < *uq)
           return -1;
         else if (*up++ > *uq++)
           return 1;
       }
       return 0;
     }
*/
#define HH_MEMCMP(p, q, n_bytes)  memcmp(p, q, n_bytes)
#endif
#ifndef HH_MEMMOVE
#define HH_MEMMOVE(to, from, n_bytes)  memmove(to, from, n_bytes)
#endif


#ifdef HH_COMPILER

#include <stdio.h>
#include <stdlib.h>

void hh_print_compiler(const char *fmt, ...); /* Defined in hh_compiler.c */
#define HH_PRINT  hh_print_compiler

#else

int hh_print_interpreter(const char *fmt, ...); /* Defined in hh_interp_*.c */
#define HH_PRINT  hh_print_interpreter

#ifndef HH_LISP_PRINT_DEPTH_INCR
#define HH_LISP_PRINT_DEPTH_INCR  5
#endif
struct hh_context_t;		/* Forward declaration. */
int hh_lisp_print_interpreter(struct hh_context_t *ctx, /* In hh_interp_*.c */
			      hh_word_t word, int depth);

#endif


#if __GNUC__ >= 3
/* Macros that make it possible to give the compiler hints on which
   branch is most likely taken. */
#define HH_LIKELY(cond)    __builtin_expect((cond), 1)
#define HH_UNLIKELY(cond)  __builtin_expect((cond), 0)
#endif

#ifndef HH_LIKELY
#define HH_LIKELY(cond)    (cond)
#define HH_UNLIKELY(cond)  (cond)
#endif


/* The first four bytes of the byte code files should contain the
   values 0x4E, 0xD6, 0xE4 and 0x06, respectively. */
#define HH_COOKIE          0x4ED6E406

/* A one-byte running counter for the version number of the byte code
   file format (the header, constant value and debugging information
   representation).  */
#define HH_BCODE_VERSION   1


/* Version number of the Hedgehog Lisp language. The version number
   consists of two parts: major and minor. The version number defines
   the language implemented: whenever a new builtin function or other
   language feature is added, the minor version number is
   incremented. When a builtin function is removed or its interface
   changes in an incompatible manner, or something else changes that
   breaks existing programs, the major version is incremented. This
   means that when a Hedgehog Lisp program is written for version a.b,
   it will work with a version a.B, as long as b <= B.
  
   Note that this version number is for the LANGUAGE, not the
   implementation.  If the implementation is rewritten completely, but
   in a manner that keeps the language exactly the same, this version
   number won't change even if the implementation's version number
   does. */

#define HEDGEHOG_LISP_VERSION_MAJOR  2
#define HEDGEHOG_LISP_VERSION_MINOR  0

/* The Hedgehog Lisp implementation also has a version number. Its
   major and minor numbers are the same as for the language it
   implements, and its patch level number increases for each
   version. This should be an easy enough system to deal with. If it
   doesn't suffice, we'll change it later. */

#define HEDGEHOG_IMPLEMENTATION_VERSION_MAJOR  HEDGEHOG_LISP_VERSION_MAJOR
#define HEDGEHOG_IMPLEMENTATION_VERSION_MINOR  HEDGEHOG_LISP_VERSION_MINOR
#define HEDGEHOG_IMPLEMENTATION_VERSION_PATCH  1


#endif /* HH_INCL_COMMON */
