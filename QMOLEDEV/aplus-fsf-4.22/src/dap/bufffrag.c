/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * bufffrag - given a buffer and a fragment length, bufffrag() strips off
 * fragment length bytes from the data area of the buffer places them in a
 * new buffer.  If there is no data after the fragment, then the original
 * buffer is cleared, and the space belonging to it is transferred to the new
 * buffer.  Otherwise, the fragment is stuffed into the new buffer and
 * removed from the old. If fragment length bytes of data are not available,
 * a null pointer is returned.
 */

/* header file inclusions */
#include <dap/buff.h>

/* external function definitions */
struct buff *
bufffrag(struct buff * p, int frag)
{
  struct buff *bp = (struct buff *) (0);
  int len;

  if ((p != (struct buff *) (0))
      && (p->min != (char *) (0))
      && ((len = p->put - p->get) >= frag)) {
    bp = buffalloc();
    if (frag == len) {
      *bp = *p;
      p->min = p->get = p->put = p->max = (char *) (0);
    } else {
      buffstuff(bp, p->get, frag);
      p->get += frag;
    }
  }
  return bp;
}
