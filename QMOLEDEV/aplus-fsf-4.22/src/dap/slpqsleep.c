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
#include <dap/slpq.h>

/* external function definitions */
struct slpqent *slpqsleep(struct slpq * p, void (*func) (), void *arg, 
			  void (*sched) ())
{
  struct slpqent *ep = (struct slpqent *) (0);

  /* if the slpq is null we fake it cause we will just wait forever */
  if (p != (struct slpq *) (0)) {
    ep = (struct slpqent *) balloc(sizeof(*ep));
    ep->np = nodealloc();
    ep->np->d = (void *) ep;
    ep->sp = p;
    ep->func = func;
    ep->arg = arg;
    ep->sched = 0;
    if (p->wakes > 0) {
      (p->wakes)--;
      slpqsched(ep, sched);
    } else {
      nodeinsert(ep->np, p->wq);
    }
  }
  return ep;
}
