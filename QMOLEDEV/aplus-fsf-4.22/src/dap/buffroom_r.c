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
int 
buffroom_r(struct buff * p, int add)
{
  int def;			/* space deficiency */

  if ((p != (struct buff *) (0))
      && ((def = add - (p->max - p->put)) > 0)) {
    int len = p->put - p->get;
    int off = p->get - p->min;

    if (def > off) {
      /* deficiency is greater than offset */
      int siz = p->max - p->min;
      int req = siz + def;
      char *newmin;

      siz += siz / 2;
      if (siz < req)
	siz = req;
      if (siz < sizeof(*p))
	siz = sizeof(*p);
      newmin = (char *) brealloc(p->min, siz);
      if (newmin == (char *) (0))
	return -1;
      p->min = newmin;
      p->get = p->min + off;
      p->put = p->get + len;
      p->max = p->min + siz;
    } else {
      bcopy(p->get, p->min, len);
      p->get -= off;
      p->put -= off;
    }
  }
  return 0;
}
