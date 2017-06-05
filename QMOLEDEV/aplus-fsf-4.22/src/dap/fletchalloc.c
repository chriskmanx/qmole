/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * Reference: "Fletcher's Error Detection Algorithm: How to implement it
 * efficiently and how to avoid the most common pitfalls," by Anastase
 * Nakassis, COMPUTER COMMUNICATION REVIEW,Vol 18, No 5, October, 1988.
 */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/fletch.h>

/* external function definitions */
struct fletch *
fletchalloc(void)
{
  struct fletch *p;

  p = (struct fletch *) balloc(sizeof(*p));
  p->c0 = p->c1 = 0;
  p->modfreq = sizeof(int) >= 4 ? 4102 : 14;
  p->tilmod = p->modfreq;

  return p;
}
