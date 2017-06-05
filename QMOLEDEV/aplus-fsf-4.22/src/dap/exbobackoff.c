/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/exbo.h>

/* external function definitions */
int 
exbobackoff(struct exbo * p)
{
  if (p->current < p->last) {
    p->current *= 2;
    if (p->current > p->last)
      p->current = p->last;
  } else if (p->current > p->last) {
    p->current /= 2;
    if (p->current < p->last)
      p->current = p->last;
  }
  return (p->negative ? -1 * (int) (p->current) : (int) (p->current));
}
