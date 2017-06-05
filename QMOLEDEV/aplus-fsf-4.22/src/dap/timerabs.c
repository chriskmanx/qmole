/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <sys/types.h>
#include <dap/balloc.h>
#include <dap/tv.h>
#include <dap/node.h>
#include <dap/timer.h>

/* external function definitions */
struct timer *timerabs(time_t sec, long int usec, void (*func) (), void *arg) {
  struct timer *p = (struct timer *) balloc(sizeof(*p));
  struct timeval *tvp = &(p->expire);
  struct node *hp = &timers;
  struct node *np;

  (p->np = nodealloc())->d = (void *) (p);
  tvp->tv_sec = (long) sec;
  tvp->tv_usec = usec;
  (void) tvnorm(tvp);
  p->func = func;
  p->arg = arg;
  for (np = hp->b; np != hp; np = np->b) {
    if (tvcmp(tvp, &(TIMERAT(np)->expire)) >= 0)
      break;
  }
  nodeinsert(p->np, np->f);

  return p;
}
