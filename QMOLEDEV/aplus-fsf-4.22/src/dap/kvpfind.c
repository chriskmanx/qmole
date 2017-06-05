/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <a/development.h>
#include <string.h>
#include <dap/node.h>
#include <dap/kvp.h>

/* external function definitions */
struct node *
kvpfind(struct node * hnp, char *key)
{
  struct node *np;

  for (np = hnp->f; np != hnp; np = np->f) {
    char *tstkey = (char *) (KVPAT(np)->key);
    if (*key != *tstkey)
      continue;
    if (*key == '\0')
      return np;
    if (strcmp((DEV_STRARG) key + 1, (DEV_STRARG) tstkey + 1) == 0)
      return np;
  }
  return (struct node *) (0);
}
