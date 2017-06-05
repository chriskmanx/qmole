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
bufftrim(struct buff * p)
{
  if ((p != (struct buff *) (0))
      && ((p->get != p->min) || (p->put != p->max))) {
    int len = p->put - p->get;
    int off = p->get - p->min;

    if (off != 0) {
      bcopy(p->get, p->min, len);
    }
    /*
     * We use brealloc instead of brealloc_r because this should not fail due
     * to ENOMEM.  After all, we are shrinking the buffer.
     */
    p->get = p->min = (char *) brealloc(p->min, len);
    p->get = p->min;
    p->put = p->max = p->min + len;
  }
  return;
}
