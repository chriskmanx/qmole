/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* external function definitions */
int 
trchar(unsigned char *tr, char c)
{
  if (tr != (unsigned char *) (0)) {
    return (int) tr[(unsigned) (c & 0xff)];
  }
  return (int) ((unsigned) (c & 0xff));
}
