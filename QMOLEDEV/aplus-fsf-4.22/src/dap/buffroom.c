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
buffroom(struct buff * p, int add)
{
  int def;			/* space deficiency */

  if ((p != (struct buff *) (0))
      && ((def = add - (p->max - p->put)) > 0)) {
    int len = p->put - p->get;
    int off = p->get - p->min;

    if (def > off) {
      /* deficiency is more than the offset */
      int siz = p->max - p->min;
      int req = siz + def;

      siz += siz / 2;
      if (siz < req)
	siz = req;
      if (siz < sizeof(*p))
	siz = sizeof(*p);
      p->min = (char *) brealloc(p->min, siz);
      p->get = p->min + off;
      p->put = p->get + len;
      p->max = p->min + siz;
    } else {
      bcopy(p->get, p->min, len);
      p->get -= off;
      p->put -= off;
    }
  }
  return;
}
