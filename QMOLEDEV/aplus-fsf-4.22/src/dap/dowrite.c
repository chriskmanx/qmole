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
dowrite(int fd, char *buf, int size)
{
  static char fnc[] = "dowrite";
  int done = 0;
  int rc;

  while (size > 0) {
    if ((rc = write(fd, buf, size)) < 0) {
      if (errno == EINTR) {
	continue;
      } else if (errno == EWOULDBLOCK) {
	fd_set *wfds = fdsalloc();

	fdsset(wfds, fd);
	(void) doselect(fd + 1, (fd_set *) (0), wfds,
			(fd_set *) (0), (struct timeval *) (0));
	fdsclr(wfds, fd);
	fdsfree(wfds);
	continue;
      } else if (errno == EPIPE) {
	return done;
      }
      Abort("%t %s(): abort: write(%d): %m\n", fnc, fd);
    } else if (rc > size) {
      Abort("%t %s: abort: write(%d): too many bytes: %d > %d\n",
	    fnc, fd, rc, size);
    }
    done += rc;
    buf += rc;
    size -= rc;
  }
  return done;
}
