/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Chuck Ocheret */

/* header file inclusions */
#include <dap/buff.h>

/* external function definitions */
void 
buffputint(struct buff * p, int d)
{
  buffstuff(p, (char *) (&d), sizeof(d));
  return;
}
