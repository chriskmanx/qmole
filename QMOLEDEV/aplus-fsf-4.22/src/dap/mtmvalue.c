/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * for use when mtm's are linked into hash tables and other search
 * structures.
 */

/* header file inclusions */
#include <dap/node.h>
#include <dap/mtm.h>

/* external function definitions */
void *
mtmvalue(struct node * np)
{
  return MTMAT(np)->d;
}
