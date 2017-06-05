/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/buff.h>

/* external function definitions */
struct buff *
buffref(struct buff * p)
{
  if (p != (struct buff *) (0)) {
    (p->ref)++;
  }
  return p;
}
