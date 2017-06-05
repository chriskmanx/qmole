/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/chan.h>
#include <dap/sgnl.h>
#include <dap/slpq.h>
#include <dap/timer.h>
#include <dap/misc.h>

/* external function definitions */
void 
daploop(void)
{
  while (dapbreak == 0) {
    dapselect();
    (void) sgnlproc();
    (void) chanproc();
    (void) timerproc();
    (void) slpqproc();
  }
  return;
}
