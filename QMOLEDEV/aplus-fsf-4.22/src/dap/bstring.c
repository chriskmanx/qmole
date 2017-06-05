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
#include <dap/balloc.h>

/* external function definitions */
char *
bstring(const char *s)
{
  if (s != (char *) (0)) {
    int size = strlen((DEV_STRARG) s) + 1;
    char *p = (char *) balloc(size);

    bcopy(s, p, size);
    return p;
  }
  return (char *) (0);
}
