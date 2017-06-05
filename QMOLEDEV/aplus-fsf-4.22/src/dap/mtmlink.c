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
struct mtm *
mtmlink(struct node * uhp, void *up, struct node * dhp, void *dp, void *d)
{
  struct mtm *p = mtmalloc(d);

  mtmupins(p, uhp, up);
  mtmdnins(p, dhp, dp);

  return p;
}
