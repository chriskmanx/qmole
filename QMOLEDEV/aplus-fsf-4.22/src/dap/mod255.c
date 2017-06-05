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

/* external function definitions */
int 
mod255(int c)
{
  char *cp = (char *) (&c);
  char *end_cp = cp + sizeof(c);
  int r = 0;

  while (cp < end_cp) {
    r += *(unsigned char *) (cp++);
  }
  while (r >= 255)
    r -= 255;

  return r;
}
