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
#include <dap/hpp.h>

/* external function definitions */
void 
hppfree(struct hpp * p)
{
  if (p != (struct hpp *) (0)) {
    bfree(p->host);
    bfree((char *) p);
  }
  return;
}
