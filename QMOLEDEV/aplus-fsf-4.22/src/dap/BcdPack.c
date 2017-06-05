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
#include <ctype.h>
#include <dap/misc.h>

/* external function definitions */
int 
BcdPack(char *str, int len, unsigned char *bcd)
{
  int j;
  int i;
  unsigned char c;

  if ((len <= 0) || (bcd == (unsigned char *) (0)))
    return 0;

  i = 0;
  if (str != (char *) (0)) {
    while (i < len) {
      c = (unsigned char) str[i++];
      if (!isdigit(c)) {
	if (c == '\0')
	  break;
	return -1;
      }
    }
  }
  j = MODRNDUP(len, 2);
  while (--i >= 0) {
    /* TK - make ASCII independent */
    c = (str[i] - '0') & 0xf;
    if (--j % 2 == 0)
      bcd[j / 2] |= (c << 4);
    else
      bcd[j / 2] = c;
  }
  /* left pad the bcd array */
  while (--j >= 0)
    if (j % 2 != 0)
      bcd[j / 2] = 0;
  return (len);
}
