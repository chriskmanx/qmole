/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/tod.h>
#include <dap/fds.h>
#include <dap/misc.h>

/* external function definitions */
void 
dapinit(void)
{
  if (dapInitialized)
    return;
  (void) tod();
  (void) hostname();
  fdsinit();
  dapbreak = 0;
  dapZeroTimeout = 0;
  dapInitialized = 1;
  return;
}
