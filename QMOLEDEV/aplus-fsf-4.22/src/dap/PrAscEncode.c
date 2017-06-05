/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

/* header file inclusions */
#include <dap/buff.h>

/* external function definitions */
void 
PrAscEncode(int c, struct buff * bp)
{
  c &= 0xff;
  if (c & 0x80) {
    buffputc(bp, '~');
    c &= 0x7f;
  }
  if (c <= 0x20 || c == 0x7f) {
    buffputc(bp, '^');
    c ^= 0x40;
  }
  if (c == '~' || c == '^' || c == '\\') {
    buffputc(bp, '\\');
  }
  buffputc(bp, c);
  return;
}
