/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * cuts head circular list before h and inserts new circular list, making
 * large circular list.  This operation is semetrical with respect to
 * interchange of h and p.
 */

/* header file inclusions */
#include <dap/node.h>

/* external function definitions */
void 
nodeinsert(struct node * p, struct node * h)
{
  if ((p != (struct node *) (0))
      && (h != (struct node *) (0))) {
    struct node *t = p->b;	/* temporary node */

    p->b->f = h;
    p->b = h->b;
    h->b->f = p;
    h->b = t;
  }
  return;
}
