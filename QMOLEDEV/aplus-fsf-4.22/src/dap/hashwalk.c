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
#include <dap/hash.h>

/* external function definitions */
void *hashwalk(struct hash * p, void *(*func) (), void *result) {
  if ((p != (struct hash *) (0))
      && (func != (void *(*) ()) (0))) {
    struct node *hnp;
    struct node *end_hnp;
    struct node *np;
    struct node *next_np;

    for (end_hnp = (hnp = p->tbl) + p->tblsz; hnp != end_hnp; hnp++) {
      for (np = hnp->f; np != hnp; np = next_np) {
	next_np = np->f;
	result = (*func) (np, result);
      }
    }
  }
  return result;
}
