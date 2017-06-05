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

/* internal macro declarations */
#define TILDA		0x01
#define CARAT		0x02
#define BACKSLASH	0x04

/* external function definitions */
int 
PrAscDecode(int c, struct buff * bp, int state)
{
  c &= 0xff;
  if (state & BACKSLASH) {
    /* take this character modified by rest of state */
  } else {
    switch (c) {
    case '~':
      return state |= TILDA;
    case '^':
      return state |= CARAT;
    case '\\':
      return state |= BACKSLASH;
    default:
      /* take this character modified by the tilda and carat */
      break;
    }
  }
  if (state & TILDA)
    c |= 0x80;
  if (state & CARAT)
    c ^= 0x40;
  buffputc(bp, c);
  return 0;
}
