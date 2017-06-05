/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * avlfind() will search the avl tree for the element with the given key
 * value.  It will return a handle to it.
 */

/* header file inclusions */
#include <dap/avl.h>

/* external function definitions */
void *
avlfind(struct avl * p, void *key)
{
  if ((p != (struct avl *) (0))
      && (key != (void *) (0))) {
    struct avln *ap = p->root;
    int r;

    while (ap != (struct avln *) (0)) {
      r = (*(p->compare)) (key, (*(p->key)) (ap->d));
      if (r < 0)
	ap = ap->l;
      else if (r > 0)
	ap = ap->r;
      else
	return (*(p->value)) (ap->d);
    }
  }
  return (void *) (0);
}
