/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * avlfree() will free all resources in an existing avl tree
 */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/avl.h>

/* internal function definitions */
static void _free(void (*remove) (), struct avln * ap);

/* external function definitions */
void 
avlfree(struct avl * p)
{
  if (p != (struct avl *) (0)) {
    _free(p->remove, p->root);
    bfree((char *) p);
  }
  return;
}

/* internal function definitions */
static void _free(void (*remove) (), struct avln * ap) {
  if (ap != (struct avln *) (0)) {
    _free(remove, ap->l);
    _free(remove, ap->r);
    (*remove) (ap->d);
    bfree((char *) ap);
  }
  return;
}
