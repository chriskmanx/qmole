/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/Warn.h>
#include <dap/tv.h>
#include <dap/tod.h>

/* external function definitions */
struct timeval *
tod(void)
{
  static char fnc[] = "tod";
  struct timeval now;

  if (gettimeofday(&now, (struct timezone *) (0)) < 0) {
    Warn("%t %s(): error: gettimeofday(): %m\n", fnc);
  } else {
    dap_tod_last = now;
  }

  return &dap_tod_last;
}
