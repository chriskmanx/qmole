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
#include <dap/fds.h>
#include <dap/chan.h>

/* external function definitions */
int 
chanproc(void)
{
  struct node work;		/* queue marker */
  struct node done;		/* queue marker */
  struct node *hp = &chans;
  struct node *wp = &work;
  struct node *dp = &done;
  struct node *np;
  struct chan *p;
  int pri=0;

  /* initialize local queue markers */
  wp->f = wp->b = wp;
  dp->f = dp->b = dp;

  /* search for a priority with something to do */
  for (np = hp->f; np != hp; np = np->f) {
    p = CHANAT(np);
    if (fdsisset(p->afds, p->fd)) {
      pri = p->pri;
      break;
    }
  }
  if (np == hp)
    return 0;			/* nothing found to do */

  /* place head of work list */
  nodeinsert(wp, np);
  /* search forward for first with different priority */
  while (((np = np->f) != hp) && (CHANAT(np)->pri == pri));
  /* split the work list out of the chan list */
  nodeinsert(wp, np);
  /* place the done marker into the list */
  nodeinsert(dp, wp);

  /* process work list */
  while ((np = wp->f) != dp) {
    nodetoad(wp);		/* leap wp over np */
    p = CHANAT(np);
    if (fdsisset(p->afds, p->fd)) {
      fdsclr(p->afds, p->fd);
      (*(p->func)) (p->ccbp);
    }
  }
  /* take out the work list head */
  noderemove(wp);

  /* find insertion point for done things */
  for (np = hp->f; np != hp; np = np->f) {
    if (pri >= CHANAT(np)->pri)
      break;
  }
  /* splice in the done list at the place found in the chan list */
  nodeinsert(dp, np);
  /* take out the done list head */
  noderemove(dp);

  return 1;
}
