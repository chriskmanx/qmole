/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* remove node from its doubly linked list -- idempotent */

/* header file inclusions */
#include <dap/node.h>

/* external function definitions */
void 
noderemove(struct node * p)
{
  if (p != (struct node *) (0)) {
    p->b->f = p->f;
    p->f->b = p->b;
    p->b = p;
    p->f = p;
  }
  return;
}
