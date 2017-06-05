/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * get rid of a sleep queue -- this will discard any waiting slpqent's, so
 * presumably, any thing that has issued a wait or otherwise knows the
 * address of the sleep queue are already aware of its impending demise.
 */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/node.h>
#include <dap/slpq.h>

/* external function definitions */
void 
slpqfree(struct slpq * p)
{
  /* free always tolarates a null argument */
  if (p != (struct slpq *) (0)) {
    struct node *np;
    struct slpqent *ep;

    /* cause all scheduled slpqents to forget this slpq */
    for (np = slpqents.f; np != &slpqents; np = np->f) {
      if ((ep = SLPQENTAT(np))->sp == p) {
	ep->sp = (struct slpq *) (0);
      }
    }
    /* free all waiting slpqent's */
    while ((np = p->wq->f) != p->wq) {
      noderemove(np);
      bfree((char *) SLPQENTAT(np));
      nodefree(np);
    }
    nodefree(p->wq);		/* free the wait queue head node */
    bfree((char *) p);		/* free the sleep queue struture */
  }
  return;
}
