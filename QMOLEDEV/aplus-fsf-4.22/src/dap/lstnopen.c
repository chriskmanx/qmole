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
#if defined(HAVE_SVR4)
#include <sys/filio.h>
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
#include <dap/balloc.h>
#include <dap/tod.h>
#include <dap/fds.h>
#include <dap/chan.h>
#include <dap/timer.h>
#include <dap/exbo.h>
#include <dap/lstn.h>
#include <dap/misc.h>

/* internal function declarations */
static void acpt(struct lstn * p);

/* external function definitions */
void 
lstnopen(struct lstn * p)
{
  static char fnc[] = "lstnopen";

  if ((p != (struct lstn *) (0))
      || (p->fd >= 0)) {
    int fd;
    int toggle;

    p->retry_tp = (struct timer *) (0);

    if ((fd = socket(p->domain, p->type, p->protocol)) < 0) {
      Warn("%t %s(%s): error: socket(): %m\n", fnc, p->name);
      lstnclose(p);
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
    toggle = 1;
    if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR,
		   (char *) (&toggle), sizeof(toggle)) < 0) {
      Warn("%t %s(%s): warning: setsockopt(%d, REUSEADDR): %m\n",
	   fnc, p->name, fd);
    }
    if ((*(p->l_namefunc)) (p) != 0) {
      lstnclose(p);
      return;
    }
    if (p->l_name == (struct sockaddr *) (0)) {
      p->l_name = (struct sockaddr *) balloc(p->l_namelen);
      bzero((char *) (p->l_name), p->l_namelen);
    } else {
      if (bind(fd, p->l_name, p->l_namelen) < 0) {
	Warn("%t %s(%s): error: bind(%d): %m\n",
	     fnc, p->name, fd);
	lstnclose(p);
	return;
      }
    }
    if (listen(fd, 5) < 0) {
      Warn("%t %s(%s): error: listen(%d): %m\n",
	   fnc, p->name, fd);
      lstnclose(p);
      return;
    }
    if (getsockname(fd, p->l_name, &(p->l_namelen)) < 0) {
      Warn("%t %s(%s): error: getsockname(%d): %m\n",
	   fnc, p->name, fd);
      lstnclose(p);
      return;
    }
    if ((*(p->l_regfunc)) (p) != 0) {
      lstnclose(p);
      return;
    }
    p->lstntod = todsec();
    (p->lstncount)++;
    p->acptchan = chanopen(p->name, fd, p->pri, CHAN_R, 
			   (void (*)(void *))acpt, (void *) p);
    chanenbl(p->acptchan);
    exboreset(p->retry_time);
  }
  return;
}

/* internal function definitions */
static void 
acpt(struct lstn * p)
{
  static char fnc[] = "acpt";
  int r_namelen = p->r_namelen;
  struct sockaddr *r_name;
  int fd;
  int l_namelen = p->l_namelen;
  struct sockaddr *l_name;

  r_name = (struct sockaddr *) balloc(r_namelen);
  if ((fd = accept(p->fd, r_name, &r_namelen)) < 0) {
    if ((errno == EWOULDBLOCK)
	|| (errno == EINTR)) {
      /* nothing to report */
    } else if ((errno == EMFILE)
	       || (errno == ENFILE)
	       || (errno == ENXIO)
	       || (errno == EIO)) {

      Warn("%t %s(%s): warn: accept(%d): %m\n",
	   fnc, p->name, p->fd);
    } else {
      Warn("%t %s(%s): error: accept(%d): %m\n",
	   fnc, p->name, p->fd);
      lstnclose(p);
    }
    bfree((char *) r_name);
    return;
  }
  l_name = (struct sockaddr *) balloc(l_namelen);
  if (getsockname(fd, l_name, &l_namelen) < 0) {
    Warn("%t %s(%s): error: getsockname(%d): %m\n",
	 fnc, p->name, fd);
    doclose(fd);
    bfree((char *) l_name);
    bfree((char *) r_name);
    return;
  }
  p->acpttod = todsec();
  (p->acptcount)++;
  r_name = (struct sockaddr *) brealloc((char *) r_name, r_namelen);
  l_name = (struct sockaddr *) brealloc((char *) l_name, l_namelen);
  (*(p->acptfunc)) (p, fd, r_name, r_namelen, l_name, l_namelen);
  return;
}
