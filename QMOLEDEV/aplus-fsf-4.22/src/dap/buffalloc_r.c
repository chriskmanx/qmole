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
struct buff *
buffalloc_r(void)
{
  struct buff *p = (struct buff *) balloc_r(sizeof(*p));

  if (p != (struct buff *) (0)) {
    p->ref = 1;
    p->min = p->get = p->put = p->max = (char *) (0);
  }
  return p;
}
