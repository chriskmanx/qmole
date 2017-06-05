/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/fds.h>

/* external function definitions */
void 
fdsclr(fd_set * p, int fd)
{
  if ((p != (fd_set *) (0))
      && (fd >= 0) && (fd < fds_size)) {
    FD_CLR(fd, p);
  }
  return;
}
