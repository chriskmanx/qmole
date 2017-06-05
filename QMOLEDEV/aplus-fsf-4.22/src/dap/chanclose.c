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
#include <dap/node.h>
#include <dap/fds.h>
#include <dap/chan.h>

/* external function definitions */
void 
chanclose(struct chan * p)
{
  if (p != (struct chan *) (0)) {
    fdsclr(p->afds, p->fd);
    fdsclr(p->fds, p->fd);
    noderemove(p->np);
    nodefree(p->np);
    bfree(p->name);
    bfree((char *) p);
  }
  return;
}
