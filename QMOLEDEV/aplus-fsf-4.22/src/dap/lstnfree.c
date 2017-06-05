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
#include <dap/exbo.h>
#include <dap/lstn.h>

/* external function definitions */
void 
lstnfree(struct lstn * p)
{
  if (p != (struct lstn *) (0)) {
    p->retry = LSTN_RETRY_NO;
    lstnclose(p);
    exbofree(p->retry_time);
    bfree(p->name);
    bfree((char *) p);
  }
  return;
}
