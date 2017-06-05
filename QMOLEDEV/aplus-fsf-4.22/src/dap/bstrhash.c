/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <string.h>
#include <limits.h>

/* external function definitions */
int 
bstrhash(char *key)
{
  int c;
  int i;
  int r = 0;
  int b[sizeof(int)];

  for (i = 0; i < sizeof(int); b[i++] = 0);

  i = 0;
  while ((c = *key++) != '\0') {
    b[i] ^= c;
    i = (i + 1) % sizeof(int);
  }
  for (i = 0; i < sizeof(int); i++) {
    r = (r << 8) | b[i];
  }
  if (r < 0)
    r -= INT_MIN;		/* force value to be positive */

  return r;
}
