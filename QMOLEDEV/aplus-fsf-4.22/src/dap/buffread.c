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
buffread(struct buff * p, int fd, int nby)
{
  static char fnc[] = "buffread";
  int rby;

  if ((p == (struct buff *) (0)) || (nby <= 0))
    return 0;
  if (p->max - p->put < nby)
    buffroom(p, nby);
  if ((rby = read(fd, p->put, nby)) < 0) {
    if ((errno == EINTR) || (errno == EWOULDBLOCK))
      return 0;
    return -1;
  }
  if (rby == 0) {
    /* end of file */
    errno = EPIPE;
    return -1;
  }
  if (rby > nby) {
    Abort("%t %s(): abort: read(%d): too many bytes: %d > %d\n",
	  fnc, fd, rby, nby);
  }
  p->put += rby;

  return rby;
}
