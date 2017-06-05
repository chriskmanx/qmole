/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <errno.h>
#include <dap/Warn.h>
#include <dap/tv.h>
#include <dap/fds.h>
#include <dap/misc.h>

/* external function definitions */
int 
doselect(int d, fd_set * r, fd_set * w, fd_set * x, struct timeval * t)
{
  static char fnc[] = "doselect";
  fd_set *rsave = fdsdup(r);
  fd_set *wsave = fdsdup(w);
  fd_set *xsave = fdsdup(x);
  int rc;

  while ((rc = select(d, r, w, x, t)) < 0) {
    if (errno != EINTR) {
      Abort("%t %s(): abort: select(): %m\n", fnc);
    }
    fdscopy(rsave, r);
    fdscopy(wsave, w);
    fdscopy(xsave, x);
  }
  fdsfree(rsave);
  fdsfree(wsave);
  fdsfree(xsave);
  return rc;
}
