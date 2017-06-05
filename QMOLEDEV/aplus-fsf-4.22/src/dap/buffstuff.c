/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <a/development.h>
#include <dap/buff.h>

/* external function definitions */
void 
buffstuff(struct buff * p, char *value, int size)
{
  if ((p != (struct buff *) (0))
      && (size > 0)) {
    if (p->max - p->put < size) {
      buffroom(p, size);
    }
    bcopy(value, p->put, size);
    p->put += size;
  }
  return;
}
