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
fdszero(fd_set * p)
{
  if (p != (fd_set *) (0)) {
    bzero((char *) p, fds_sizeof);
  }
  return;
}
