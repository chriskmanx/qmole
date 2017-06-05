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
#include <dap/timer.h>

/* external function definitions */
struct timeval *
timernext(void)
{
  struct timer *p;

  if ((p = TIMERAT(timers.f)) == (struct timer *) (0)) {
    return (struct timeval *) (0);
  }
  return &(p->expire);
}
