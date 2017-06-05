/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * used to compute intermediate values of c0 and c1
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
fletchsum(struct fletch * p, char *cp, int len)
{
  int c0 = p->c0;
  int c1 = p->c1;
  int tilmod = p->tilmod;
  char *end_cp;

  while (len) {
    if (len < tilmod) {
      end_cp = cp + len;
      while (cp < end_cp) {
	c0 += *((unsigned char *) (cp++));
	c1 += c0;
      }
      tilmod -= len;
      len = 0;
    } else {
      end_cp = cp + tilmod;
      while (cp < end_cp) {
	c0 += *((unsigned char *) (cp++));
	c1 += c0;
      }
      len -= tilmod;
      c0 = mod255(c0);
      c1 = mod255(c1);
      tilmod = p->modfreq;
    }
  }
  p->tilmod = tilmod;
  p->c1 = c1;
  p->c0 = c0;

  return;
}
