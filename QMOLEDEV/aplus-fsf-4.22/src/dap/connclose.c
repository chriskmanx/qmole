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
#include <dap/conn.h>

/* external function definitions */
void 
connclose(struct conn * p)
{
  static char fnc[] = "connclose";

  if (p != (struct conn *) (0)) {
    int fd = p->fd;

    if (fd < 0) {
      timerclr(p->retry_tp);
      p->retry_tp = (struct timer *) (0);
    } else {
      if (p->estbd) {
	exboreset(p->retry_time);
	p->estbd = 0;
	p->disctod = todsec();
	(p->disccount)++;
	p->estbdtime += (p->disctod - p->estbtod);
      } else {
	chanclose(p->estbchan);
	p->estbchan = (struct chan *) (0);
      }
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
    bfree((char *) (p->r_name));
    p->r_name = (struct sockaddr *) (0);
    p->r_namelen = 0;
    if (p->retry == CONN_RETRY_YES) {
      time_t retry_time = (time_t) exbovalue(p->retry_time);

      Warn("%t %s(%s): note: retrying in %ld seconds\n",
	   fnc, p->name, retry_time);
      p->retry_tp = timer(retry_time, (long) (0),
			  (void (*)(void *))connopen, (void *) (p));
      exbobackoff(p->retry_time);
    }
  }
  return;
}
