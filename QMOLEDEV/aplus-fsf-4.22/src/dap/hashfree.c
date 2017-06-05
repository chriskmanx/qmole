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
#include <dap/hash.h>

/* external function definitions */
void 
hashfree(struct hash * p)
{
  if (p != (struct hash *) (0)) {
    struct node *hnp;
    struct node *end_hnp;
    struct node *np;

    for (end_hnp = (hnp = p->tbl) + p->tblsz; hnp != end_hnp; hnp++) {
      while ((np = hnp->f) != hnp) {
	(*(p->remove)) (np);
      }
    }
    bfree((char *) (p->tbl));
    bfree((char *) p);
  }
  return;
}
