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
#include <dap/buff.h>

/* external function definitions */
int 
buffwrite(struct buff * p, int fd, int nby)
{
  static char fnc[] = "buffwrite";
  int wby;

  if ((p == (struct buff *) (0)) || (nby <= 0))
    return 0;
  if ((wby = p->put - p->get) < nby)
    nby = wby;
  if ((wby = write(fd, p->get, nby)) < 0) {
    if ((errno == EINTR) || (errno == EWOULDBLOCK))
      return 0;
    return -1;
  }
  if (wby > nby) {
    Abort("%t %s(): abort: write(%d): too many bytes: %d > %d\n",
	  fnc, fd, wby, nby);
  }
  p->get += wby;

  return wby;
}
