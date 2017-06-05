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
#include <dap/balloc.h>
#include <dap/tod.h>
#include <dap/fds.h>
#include <dap/chan.h>
#include <dap/timer.h>
#include <dap/exbo.h>
#include <dap/lstn.h>

/* external function definitions */
void 
lstnclose(struct lstn * p)
{
  static char fnc[] = "lstnclose";

  if (p != (struct lstn *) (0)) {
    int fd = p->fd;

    if (fd < 0) {
      timerclr(p->retry_tp);
      p->retry_tp = (struct timer *) (0);
    } else {
      if (p->acptchan != (struct chan *) (0)) {
	chanclose(p->acptchan);
	p->acptchan = (struct chan *) (0);
	p->deaftod = todsec();
	p->lstndtime += (p->deaftod - p->lstntod);
      }
      bfree((char *) (p->l_name));
      p->l_name = (struct sockaddr *) (0);
      p->l_namelen = 0;
      fdsfresh(fd);
      if (close(fd) < 0) {
	Warn("%t %s(%s): warning: close(%d): %m\n",
	     fnc, p->name, fd);
      }
      p->fd = -1;
      p->closetod = todsec();
      (p->closecount)++;
      p->opendtime += (p->closetod - p->opentod);
    }
    if (p->retry == LSTN_RETRY_YES) {
      time_t retry_time = (time_t) exbovalue(p->retry_time);

      Warn("%t %s(%s): note: retrying in %ld seconds\n",
	   fnc, p->name, retry_time);
      p->retry_tp = timer(retry_time, (long) (0),
			  (void (*)(void *))lstnopen, (void *) (p));
      exbobackoff(p->retry_time);
    }
  }
  return;
}
