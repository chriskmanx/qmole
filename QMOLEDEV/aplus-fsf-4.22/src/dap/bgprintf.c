/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <stdio.h>
#include <string.h>
#if !defined(__cfront)
#include <stdarg.h>
#endif
#include <dap/Warn.h>
#include <dap/balloc.h>

/* external function definitions */
char *
bgprintf(int size, char *fmt,...)
{
  static char fnc[] = "bgprintf";
  va_list ap;
  char *p = (char *) balloc(size + 1);

  va_start(ap, fmt);
  (void) vsprintf(p, fmt, ap);
  va_end(ap);

  if (strlen((DEV_STRARG) p) > size) {
    Abort("%t %s(): abort: size error: %d > %d\n", fnc, strlen((DEV_STRARG) p), size);
  }
  return p;
}
