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
#include <dap/buff.h>

/* external function definitions */
void 
bufffree(struct buff * p)
{
  if ((p != (struct buff *) (0))
      && (--(p->ref) <= 0)) {
    bfree(p->min);
    bfree((char *) p);
  }
  return;
}
