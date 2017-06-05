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
struct mtm *
mtmalloc(void *d)
{
  struct mtm *p = (struct mtm *) balloc(sizeof(*p));
  struct node *np;

  p->unp = np = nodealloc();
  np->d = (void *) p;
  p->up = (void *) (0);
  p->dnp = np = nodealloc();
  np->d = (void *) p;
  p->dp = (void *) (0);
  p->d = d;

  return p;
}
