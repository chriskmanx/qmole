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
#include <dap/buff.h>

/* external function definitions */
void 
buffgprintf(struct buff * p, int size, char *fmt,...)
{
  static char fnc[] = "buffgprintf";

  if (p != (struct buff *) (0)) {
    va_list ap;
    int len;

    if (p->max - p->put < size + 1) {
      buffroom(p, size + 1);
    }
    va_start(ap, fmt);
    (void) vsprintf(p->put, fmt, ap);
    va_end(ap);

    if ((len = strlen((DEV_STRARG) p->put)) > size) {
      if (len >= p->max - p->put) {
	Abort("%t %s(): abort: size error: %d > %d\n", fnc, len, size);
      }
      Warn("%t %s(): warning: size error: %d > %d\n", fnc, len, size);
    }
    p->put += len;
  }
  return;
}
