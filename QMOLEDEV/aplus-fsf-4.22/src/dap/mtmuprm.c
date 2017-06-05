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
#include <dap/mtm.h>

/* external function definitions */
void 
mtmuprm(struct mtm * p)
{
  if (p != (struct mtm *) (0)) {
    noderemove(p->unp);
    p->up = (void *) (0);
  }
  return;
}
