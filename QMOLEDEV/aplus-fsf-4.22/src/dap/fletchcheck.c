/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * used to verify check sum on block
 * 
 * Reference: "Fletcher's Error Detection Algorithm: How to implement it
 * efficiently and how to avoid the most common pitfalls," by Anastase
 * Nakassis, COMPUTER COMMUNICATION REVIEW,Vol 18, No 5, October, 1988.
 */

/* header file inclusions */
#include <dap/misc.h>
#include <dap/fletch.h>

/* external function definitions */
int 
fletchcheck(char *buf, int len, int k)
{
  char *xp = buf + k;
  int r;

  if (*xp) {
    if (*(xp + 1)) {
      /* both x and y are not zero, compute check sum */
      struct fletch *p = fletchalloc();

      fletchsum(p, buf, len);
      /* complete modulus */
      if ((mod255(p->c0))
	  || (mod255(p->c1))) {
	/* check sum incorrect */
	r = 1;
      } else {
	/* check sum correct */
	r = 0;
      }
      fletchfree(p);
    } else {
      /* x is not 0 but y is, this is impossible */
      r = 1;
    }
  } else if (*(xp + 1)) {
    /* y is not 0 but x is, this is impossible */
    r = 1;
  } else {
    /* x and y are zero, meaning no check sum, assume this is ok */
    r = 0;
  }

  return r;
}
