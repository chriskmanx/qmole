/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <sys/types.h>
#ifdef HAVE_SVR4
#include  <sys/filio.h>
#endif
#include <sys/uio.h>
#include <dap/notsunos4.h>
#ifdef PRESUNOS4
#ifndef _SOCKET_
#define _SOCKET_
#include <sys/socket.h>
#endif
#else
#include <sys/socket.h>
#endif
#include <sys/ioctl.h>
#include <errno.h>
#include <dap/Warn.h>
#include <dap/tod.h>
#include <dap/fds.h>
#include <dap/chan.h>
#include <dap/timer.h>
#include <dap/conn.h>

/* internal function declarations */
static void estbd(struct conn * p);

/* external function definitions */
void 
connopen(struct conn * p)
{
  static char fnc[] = "connopen";
  int rc;

  if ((p != (struct conn *) (0))
      && (p->fd < 0)) {
    int fd;
    int toggle;

    p->retry_tp = (struct timer *) (0);

    if ((fd = socket(p->domain, p->type, p->protocol)) < 0) {
      Warn("%t %s(%s): error: socket(): %m\n", fnc, p->name);
      connclose(p);
      return;
    }
    p->opentod = todsec();
    (p->opencount)++;
    fdsfresh(fd);
    p->fd = fd;
    toggle = 1;
    if (ioctl(fd, FIONBIO, &toggle) < 0) {
      Warn("%t %s(%s): warning: ioctl(%d, FIONBIO): %m\n",
	   fnc, p->name, fd);
    }
    if ((p->setupfunc != (int (*) ()) (0))
	&& ((rc = (*(p->setupfunc)) (p->acb, p->fd)) != 0)) {
      if (rc != -1)
	connclose(p);
      return;
    }
    p->r_name = (*(p->r_namefunc)) (p->r_nameinfo, &(p->r_namelen));
    if (p->r_name == (struct sockaddr *) (0)) {
      connclose(p);
      return;
    }
    if (connect(fd, p->r_name, p->r_namelen) < 0) {
      if (errno == EINPROGRESS) {
	p->conntod = todsec();
	(p->conncount)++;
	p->estbchan = chanopen((char *) (0), fd, p->pri, CHAN_W, 
			       (void (*)(void *))estbd, (void *) p);
	chanenbl(p->estbchan);
	return;
      }
      if (errno != EISCONN) {
	Warn("%t %s(%s): error: connect(%d): %m\n",
	     fnc, p->name, fd);
	connclose(p);
	return;
      }
      /* we treat EISCONN as though the connection succeeded */
    }
    p->conntod = todsec();
    (p->conncount)++;
    if ((*(p->estbfunc)) (p->acb, p->fd)) {
      connackestb(p);
    }
  }
  return;
}

/* internal function definitions */
static void 
estbd(struct conn * p)
{
  chanclose(p->estbchan);
  p->estbchan = (struct chan *) (0);
  if ((*(p->estbfunc)) (p->acb, p->fd)) {
    connackestb(p);
  }
  return;
}
