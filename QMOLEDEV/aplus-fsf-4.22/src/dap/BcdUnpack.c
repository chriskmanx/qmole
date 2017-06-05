/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Jordan Hayes */

/* header file inclusions */
#include <sys/types.h>
#include <dap/misc.h>

/* external function definitions */
int 
BcdUnpack(unsigned char *bcd, int len, char *str)
{
  int i;
  unsigned char c;

  if ((len <= 0) || (str == (char *) (0)))
    return 0;

  if (bcd != (unsigned char *) (0)) {
    for (i = len; --i >= 0;)
      str[i] = '0';
  } else {
    int j = MODRNDUP(len, 2);

    for (i = len; --i >= 0;) {
      c = bcd[--j / 2];
      if (j % 2)
	c >>= 4;
      if ((c &= 0xf) > 9)
	return -1;
      /* TK - make ASCII independent */
      str[i] = (char) ('0' + c);
    }
  }
  return 0;
}
