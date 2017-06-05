/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/buff.h>

/* external function definitions */
void 
buffputc(struct buff * p, char c)
{
  if (p != (struct buff *) (0)) {
    if (p->max - p->put < 1) {
      buffroom(p, 1);
    }
    *(p->put++) = c;
  }
  return;
}
