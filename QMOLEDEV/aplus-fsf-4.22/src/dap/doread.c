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
doread(int fd, char *buf, int size)
{
  static char fnc[] = "doread";
  int done = 0;
  int rc;

  while (size > 0) {
    if ((rc = read(fd, buf, size)) < 0) {
      if (errno == EINTR) {
	continue;
      } else if (errno == EWOULDBLOCK) {
	fd_set *rfds = fdsalloc();

	fdsset(rfds, fd);
	(void) doselect(fd + 1, rfds, (fd_set *) (0),
			(fd_set *) (0), (struct timeval *) (0));
	fdsclr(rfds, fd);
	fdsfree(rfds);
	continue;
      }
      Abort("%t %s(): abort: read(%d): %m\n", fnc, fd);
    } else if (rc > size) {
      Abort("%t %s: abort: read(%d): too many bytes: %d > %d\n",
	    fnc, fd, rc, size);
    } else if (rc == 0) {
      return done;
    }
    done += rc;
    buf += rc;
    size -= rc;
  }
  return done;
}
