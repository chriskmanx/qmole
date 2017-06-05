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
int 
slpqproc(void)
{
  struct node *np;
  struct slpqent *ep;
  void (*func) ();
  void *arg;
  int didwork = 0;

  while ((np = slpqents.f) != &slpqents) {
    noderemove(np);
    ep = SLPQENTAT(np);
    nodefree(np);
    if (ep != (struct slpqent *) (0)) {
      func = ep->func;
      arg = ep->arg;
      bfree((char *) ep);
      if (func != (void (*) ()) (0))
	(*func) (arg);
    }
    didwork = 1;
  }

  return didwork;
}
