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
void 
slpqsched(struct slpqent * ep, void (*sched) ())
{
  if (ep != (struct slpqent *) (0)) {
    if (sched == (void (*) ()) (0)) {
      ep->sched = 1;
      nodeinsert(ep->np, &slpqents);
    } else {
      void (*func) () = ep->func;
      void *arg = ep->arg;

      nodefree(ep->np);
      bfree((char *) ep);
      (*sched) (func, arg);
    }
  }
  return;
}
