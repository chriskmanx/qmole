/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/fds.h>

/* external function definitions */
fd_set *
fdsdup(fd_set * p)
{
  fd_set *p2 = (fd_set *) balloc(fds_sizeof);

  fdscopy(p, p2);

  return p2;
}
