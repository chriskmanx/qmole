/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* Take Out node And Deposit one position forward (leap-frog) */

/* header file inclusions */
#include <dap/node.h>

/* external function definitions */
void 
nodetoad(struct node * p)
{
  if (p != (struct node *) (0)) {
    struct node *o = p->b;
    struct node *q = p->f;
    struct node *r;

    o->f = q;
    q->b = o;
    p->f = r = q->f;
    p->b = q;
    q->f = p;
    r->b = p;
  }
  return;
}
