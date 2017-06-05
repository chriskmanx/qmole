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
void 
fletchfree(struct fletch * p)
{
  if (p != (struct fletch *) (0)) {
    bfree((char *) (p));
  }
  return;
}
