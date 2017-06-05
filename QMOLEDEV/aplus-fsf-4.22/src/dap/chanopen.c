/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/Warn.h>
#include <dap/balloc.h>
#include <dap/node.h>
#include <dap/fds.h>
#include <dap/chan.h>

/* internal function declarations */
static void _procnoop(struct chan * p);

/* external function definitions */
struct chan *chanopen(const char *name, int fd, int pri, int type, void (*func) (), void *ccbp) {
  static char fnc[] = "chanopen";
  struct chan *p;

  p = (struct chan *) balloc(sizeof(*p));
  (p->np = nodealloc())->d = (void *) p;
  if (name == (char *) (0))
    name = "<UNKNOWN>";
  p->name = bstring(name);
  if ((fd < 0) || (fd >= fds_size)) {
    Warn("%t %s(%s): warning: bad fd == %d\n", fnc, name, fd);
  }
  p->fd = fd;
  switch (type) {
  case CHAN_R:
    p->fds = fds_r;
    p->afds = fds_ra;
    break;
  case CHAN_W:
    p->fds = fds_w;
    p->afds = fds_wa;
    break;
  case CHAN_X:
  default:
    p->fds = fds_x;
    p->afds = fds_xa;
    break;
  }
  fdsclr(p->fds, fd);
  fdsclr(p->afds, fd);
  if (func == (void (*) ()) (0)) {
    func = _procnoop;
    ccbp = (void *) p;
  }
  p->func = func;
  p->ccbp = ccbp;
  chansetpri(p, pri);

  return p;
}

/* internal function definitions */
static void 
_procnoop(struct chan * p)
{
  fdsclr(p->fds, p->fd);
  fdsclr(p->afds, p->fd);
  return;
}
