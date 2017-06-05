/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <a/development.h>
#include <dap/fds.h>

/* external function definitions */
void 
fdscopy(fd_set * p1, fd_set * p2)
{
  if (p2 != (fd_set *) (0)) {
    if (p1 == (fd_set *) (0))
      bzero((char *) p2, fds_sizeof);
    else
      bcopy((char *) p1, (char *) p2, fds_sizeof);
  }
  return;
}
