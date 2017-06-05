/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * used to place the check sum in the message
 * 
 * Reference: "Fletcher's Error Detection Algorithm: How to implement it
 * efficiently and how to avoid the most common pitfalls," by Anastase
 * Nakassis, COMPUTER COMMUNICATION REVIEW,Vol 18, No 5, October, 1988.
 */

/* header file inclusions */
#include <dap/misc.h>
#include <dap/fletch.h>

/* external function definitions */
void 
fletchdone(struct fletch * p, char *xp, int len, int k)
{
  int c0 = mod255(p->c0);
  int c1 = mod255(p->c1);
  int j, z, x, y;

  /* compute x and y */
  if (len > k + 1) {
    j = mod255(len - (k + 1));
  } else {
    j = 255 - mod255((k + 1) - len);
  }
  z = (j > 128) ? ((255 - j) * (255 - c0)) : (j * c0);
  z = mod255(z);
  if ((x = z + (255 - c1)) >= 255)
    x -= 255;
  if (x == 0)
    x = 255;
  if ((y = (255 - x) + (255 - c0)) >= 255)
    y -= 255;
  if (y == 0)
    y = 255;

  /* install x and y */
  *xp = (char) (x);
  *(xp + 1) = (char) (y);

  /* complete modulus */
  p->c0 = p->c1 = 0;
  p->tilmod = p->modfreq;

  return;
}
