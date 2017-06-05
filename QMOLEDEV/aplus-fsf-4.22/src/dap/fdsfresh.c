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
fdsfresh(int fd)
{
  fdsclr(fds_r, fd);
  fdsclr(fds_ra, fd);
  fdsclr(fds_w, fd);
  fdsclr(fds_wa, fd);
  fdsclr(fds_x, fd);
  fdsclr(fds_xa, fd);
  return;
}
