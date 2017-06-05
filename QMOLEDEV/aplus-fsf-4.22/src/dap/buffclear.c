/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/buff.h>

/* external function definitions */
void 
buffclear(struct buff * p)
{
  if (p != (struct buff *) (0)) {
    bfree(p->min);
    p->min = p->get = p->put = p->max = (char *) (0);
  }
  return;
}
