/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* This should only be used if the slpqents list is used */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/node.h>
#include <dap/slpq.h>

/* external function definitions */
void 
slpqgiveup(struct slpqent * ep)
{
  if (ep != (struct slpqent *) (0)) {
    struct slpq *p = ep->sp;
    int sched = ep->sched;

    noderemove(ep->np);
    nodefree(ep->np);
    bfree((char *) ep);
    if (sched)
      slpqwakeup(p, (void (*) ()) (0));
  }
  return;
}
