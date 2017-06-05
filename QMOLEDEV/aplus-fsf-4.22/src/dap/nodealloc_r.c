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

/* external function definitions */
struct node *
nodealloc_r(void)
{
  struct node *p = (struct node *) balloc_r(sizeof(*p));

  if (p != (struct node *) (0)) {
    p->f = p->b = p;
    p->d = (void *) (0);
  }
  return p;
}
