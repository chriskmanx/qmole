/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/node.h>
#include <dap/slpq.h>

/* external function definitions */
void 
slpqwakeup(struct slpq * p, void (*sched) ())
{
  /* if the slpq is null we fake it cause it doesn't matter */
  if (p != (struct slpq *) (0)) {
    struct node *np;

    if ((p->wakes <= 0) && ((np = p->wq->f) != p->wq)) {
      noderemove(np);
      slpqsched(SLPQENTAT(np), sched);
    } else if (p->wakes < p->maxwakes)
      ++(p->wakes);
  }
  return;
}
