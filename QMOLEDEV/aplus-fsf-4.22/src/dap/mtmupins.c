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
mtmupins(struct mtm * p, struct node * hp, void *up)
{
  if (p != (struct mtm *) (0)) {
    p->up = up;
    nodeinsert(p->unp, hp);
  }
  return;
}
