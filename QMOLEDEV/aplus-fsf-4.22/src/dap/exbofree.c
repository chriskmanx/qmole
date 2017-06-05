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

/* external function definitions */
void 
exbofree(struct exbo * p)
{
  if (p != (struct exbo *) (0)) {
    bfree((char *) p);
  }
  return;
}
