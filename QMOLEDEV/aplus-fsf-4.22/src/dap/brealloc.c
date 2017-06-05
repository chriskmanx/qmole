/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#if defined(__NetBSD__) || defined(__FreeBSD) || defined (__APPLE__)
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#include <errno.h>
#include <string.h>
#include <dap/Warn.h>
#include <dap/balloc.h>
#include <dap/ulto.h>

/* internal function definitions */
static void gasp(char *p, int size);

/* external function definitions */
void *
brealloc(char *p, int size)
{
  static char fnc[] = "brealloc";
  char *p0 = p;

  if (p == (char *) (0)) {
    return balloc(size);
  }
  if (size <= 0) {
    bfree(p);
    return (void *) (0);
  }
  if ((p = realloc(p, (unsigned) size)) == (char *) (0)) {
    if (errno == ENOMEM) {
      /* GASP!!!! */
      gasp(p0, size);
      _exit(1);
    }
    Abort("%t %s(): abort: realloc(%u): %m\n", fnc, (unsigned) size);
  }
  return (void *) p;
}

/* internal function definitions */
static void 
gasp(char *p, int size)
{
  static char m0[] = "\nbrealloc(0x";
  static char m1[ULTOHEX_SZ];
  int z1;
  static char m2[] = ", ";
  static char m3[ULTODEC_SZ];
  int z3;
  static char m4[] = "): ";
#ifdef HAVE_STRERROR
  char *m5 = strerror(ENOMEM);
#else
  char *m5 = sys_errlist[ENOMEM];
#endif
  int z5 = strlen(m5);
  static char m6[] = "\n\n";

  z1 = ultohex((unsigned long) p, m1, ULTOHEX_SZ) - 1;
  z3 = ultodec((unsigned long) size, m3, ULTODEC_SZ) - 1;

  (void) write(2, m0, sizeof(m0) - 1);
  (void) write(2, m1, z1);
  (void) write(2, m2, sizeof(m2) - 1);
  (void) write(2, m3, z3);
  (void) write(2, m4, sizeof(m4) - 1);
  (void) write(2, m5, z5);
  (void) write(2, m6, sizeof(m6) - 1);

  return;
}
