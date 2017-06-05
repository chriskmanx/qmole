/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* The higher the pri value, the higher the priority */

/* header file inclusions */
#include <dap/node.h>
#include <dap/chan.h>

/* external function definitions */
void 
chansetpri(struct chan * p, int pri)
{
  if (p != (struct chan *) (0)) {
    struct node *np = p->np;
    struct node *hp = &chans;
    struct node *tnp;

    noderemove(np);
    p->pri = pri;
    for (tnp = hp->f; tnp != hp; tnp = tnp->f) {
      if (pri >= CHANAT(tnp)->pri)
	break;
    }
    nodeinsert(np, tnp);
  }
  return;
}
