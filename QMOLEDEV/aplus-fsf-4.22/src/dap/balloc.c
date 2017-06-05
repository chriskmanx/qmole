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

/* internal function declarations */
static void gasp(int size);

/* external function definitions */
void *
balloc(int size)
{
  static char fnc[] = "balloc";
  void *p;

  if (size <= 0) {
    return (void *) (0);
  }
  if ((p = (void *) malloc((unsigned) size)) == (void *) (0)) {
    if (errno == ENOMEM) {
      /* GASP!!!! */
      gasp(size);
      _exit(1);
    }
    Abort("%t %s(): abort: malloc(%u): %m\n", fnc, (unsigned) size);
  }
  return p;
}

/* internal function definitions */
static void 
gasp(int size)
{
  static char m0[] = "\nballoc(";
  static char m1[ULTODEC_SZ];
  int z1;
  static char m2[] = "): ";
#ifdef HAVE_STRERROR
  char *m3 = strerror(ENOMEM);
#else
  char *m3 = sys_errlist[ENOMEM];
#endif
  int z3 = strlen(m3);
  static char m4[] = "\n\n";

  z1 = ultodec((unsigned long) size, m1, ULTODEC_SZ) - 1;

  (void) write(2, m0, sizeof(m0) - 1);
  (void) write(2, m1, z1);
  (void) write(2, m2, sizeof(m2) - 1);
  (void) write(2, m3, z3);
  (void) write(2, m4, sizeof(m4) - 1);

  return;
}
