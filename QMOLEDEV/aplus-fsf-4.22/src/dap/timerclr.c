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
#include <dap/node.h>
#include <dap/timer.h>

/* external function definitions */
void 
timerclr(struct timer * p)
{
  if (p != (struct timer *) (0)) {
    noderemove(p->np);
    nodefree(p->np);
    bfree((char *) p);
  }
  return;
}
