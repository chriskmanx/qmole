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
int 
buffstuff_r(struct buff * p, char *value, int size)
{
  if ((p != (struct buff *) (0))
      && (size > 0)) {
    if (p->max - p->put < size) {
      if (buffroom_r(p, size)) {
	return -1;
      }
    }
    bcopy(value, p->put, size);
    p->put += size;
  }
  return 0;
}
