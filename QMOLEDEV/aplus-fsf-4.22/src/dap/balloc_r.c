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
balloc_r(int size)
{
  static char fnc[] = "balloc_r";
  void *p;

  if (size <= 0) {
    errno = EDOM;
    return (void *) (0);
  }
  p = (void *) malloc((unsigned) size);
  if ((p == (void *) (0))
      && (errno != ENOMEM)) {
    Abort("%t %s(): abort: malloc(%u): %m\n", fnc, (unsigned) size);
  }
  return p;
}
