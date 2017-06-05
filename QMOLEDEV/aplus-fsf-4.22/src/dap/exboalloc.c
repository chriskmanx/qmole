/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * exponential back off is controlled using the exbo structure It allows a
 * value such as a timeout to be adjusted for each successive use.  A first
 * and last value is given.  The current value of the exponetially backed off
 * counter is initalized to first value.  It is read using the exbovalue()
 * function.  The value is adjusted using the exbobackoff() function.  This
 * will either double or halve the current value toward the last value If the
 * last value is past, the the current value is set the last value.
 * exboreset() will reset the current value to be equal to the first value.
 * In the future, a factor may be provided to allow the rate of backoff to be
 * controlled.  Also, backoff could be generalized by allowing the user to
 * specify a custom backoff function.  An exponential backoff structure is
 * established using exboalloc().  It is destroyed using exbofree().
 */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/exbo.h>

/* external function definitions */
struct exbo *
exboalloc(int first, int last)
{
  struct exbo *p = (struct exbo *) balloc(sizeof(*p));

  if (first < 0) {
    p->negative = 1;
    first *= -1;
    last *= -1;
  } else {
    p->negative = 0;
  }
  if (last < 0)
    last = 0;

  p->first = (unsigned) first;
  p->current = (unsigned) first;
  p->last = (unsigned) last;

  return p;
}
