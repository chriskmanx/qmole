/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <string.h>
#include <dap/buff.h>

/* external function definitions */
void 
buffputs(struct buff * p, char *s)
{
  if ((p != (struct buff *) (0))
      && (s != (char *) (0))) {
    buffstuff(p, s, strlen((DEV_STRARG) s));
  }
  return;
}
