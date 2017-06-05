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
#include <dap/node.h>
#include <dap/mtm.h>

/* external function definitions */
void 
mtmfree(struct mtm * p)
{
  if (p != (struct mtm *) (0)) {
    mtmdnrm(p);
    mtmuprm(p);
    nodefree(p->dnp);
    nodefree(p->unp);
    bfree((char *) p);
  }
  return;
}
