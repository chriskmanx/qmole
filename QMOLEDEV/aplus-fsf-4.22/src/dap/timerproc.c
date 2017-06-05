/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/tv.h>
#include <dap/tod.h>
#include <dap/node.h>
#include <dap/timer.h>

/* external function definitions */
int 
timerproc(void)
{
  struct node *hp = &timers;
  struct timeval *now = tod();
  struct node *np;
  int didwork = 0;

  while ((np = hp->f) != hp) {
    struct timer *tp = TIMERAT(np);
    struct timeval *tvp = &(tp->expire);

    if (tvcmp(now, tvp) >= 0) {
      void (*func) () = tp->func;
      void *arg = tp->arg;

      timerclr(tp);
      if (func != (void (*) ()) (0))
	(*func) (arg);
    } else {
      break;
    }
    didwork = 1;
  }

  return didwork;
}
