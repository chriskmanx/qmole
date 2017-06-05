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
#include <dap/kvp.h>

/* external function definitions */
struct node *
kvpinsert(struct node * hnp, void *key, void *val)
{
  struct node *np = nodealloc();

  np->d = (void *) kvpalloc(key, val);
  nodeinsert(np, hnp);

  return np;
}
