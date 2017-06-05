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
int 
fdsanyset(fd_set * p)
{
  if (p != (fd_set *) (0)) {
    int i;

    for (i = 0; i < fds_howmany; i++) {
      if (__FDS_BITS(p)[i] != (fd_mask) (0)) {
	return 1;
      }
    }
  }
  return 0;
}
