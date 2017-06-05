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
buffputc_r(struct buff * p, char c)
{
  if (p != (struct buff *) (0)) {
    if (p->max - p->put < 1) {
      if (buffroom_r(p, 1)) {
	return -1;
      }
    }
    *(p->put++) = c;
  }
  return 0;
}
