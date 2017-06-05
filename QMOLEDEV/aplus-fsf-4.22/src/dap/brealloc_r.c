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
#include <dap/Warn.h>
#include <dap/balloc.h>


/* external function definitions */
void *
brealloc_r(char *p, int size)
{
  static char fnc[] = "brealloc_r";

  if (p == (char *) (0)) {
    return balloc_r(size);
  }
  if (size <= 0) {
    bfree(p);
    errno = EDOM;
    return (void *) (0);
  }
  p = realloc(p, (unsigned) size);
  if ((p == (char *) (0))
      && (errno != ENOMEM)) {
    Abort("%t %s(): abort: realloc(%u): %m\n", fnc, (unsigned) size);
  }
  return (void *) p;
}
