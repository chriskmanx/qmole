/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * The conn functions will allow for maintenance of a TCP connection with a
 * given port, across failures of the connection.  The address of the other
 * end of the connection is specified as an opaque object and an
 * interpretation function.  The interpretation function is used translate
 * the opaque object into an address for use in a connection request.
 * Several interpetation functions are provided and others may be supplied
 * for a specific purpose by a user module.  The parameters and control
 * structures for the are initally established using the connalloc()
 * function.  This allocates and initalizes a control block for the
 * connection.  The inputs to connalloc() include certain parameters used in
 * establishment and maintenance of a connection.  The connopen() function is
 * used to initiate connection connection establishment. connopen will
 * intepret the remote address information and the establish a connection.
 * If connection establishment fails, then connopen will reschedule it for
 * sometime in the future.  If it succeeds, then connopen will call theuser
 * provided state change notification function, to indicate that the
 * connection is now active.  If connection establishment is initiated but is
 * not yet complete, connopen sets up a write channel that will notify the
 * user when the connection is established.  Once the connection is
 * established and the user is notified, it is up to the user to set up the
 * appropriate channels for reading and writing information on the
 * connection.  The user may elect to use other dap provided functions which
 * will read and write as necessary. The connection may be closed at any time
 * by calling connclose(). Connclose will reschedule a connopen() call at
 * sometime in the future, unless the connection's retry flag is off.  Thus,
 * exceptions while reading or writing on the connection maybne handled by
 * closing the connection, which automatically schedule reestablishment of
 * the connection.  The user is responsible for ensuring that data transfer
 * buffers are kept in a consistent state between instances of the
 * connection.  When the connection has outlived its usefulness, it is
 * removed using connfree().  This will free all resources allocated to the
 * connection, including closing the file descriptor, clearing the retry
 * timer, etc.
 */

/* header file inclusions */
#include <sys/types.h>
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
#include <dap/balloc.h>
#include <dap/tod.h>
#include <dap/chan.h>
#include <dap/timer.h>
#include <dap/exbo.h>
#include <dap/conn.h>

/* external function definitions */
struct conn *connalloc(char *name, int pri, int retry, int retry_time_first, int retry_time_last, int domain, int type, int protocol, void *r_nameinfo, int (*setupfunc) (), struct sockaddr * (*r_namefunc) (), int (*estbfunc) (), void *acb) {
  struct conn *p = (struct conn *) balloc(sizeof(*p));

  if (name == (char *) (0))
    name = "<UNKNOWN>";
  p->name = bstring(name);
  p->pri = pri;
  if (retry == CONN_RETRY_NO)
    p->retry = CONN_RETRY_NO;
  else
    p->retry = CONN_RETRY_YES;
  p->domain = domain;
  p->type = type;
  p->protocol = protocol;
  p->r_nameinfo = r_nameinfo;
  p->setupfunc = setupfunc;
  p->r_namefunc = r_namefunc;
  p->estbfunc = estbfunc;
  p->acb = acb;
  p->retry_time = exboalloc(retry_time_first, retry_time_last);

  p->retry_tp = (struct timer *) (0);
  p->fd = -1;
  p->r_namelen = 0;
  p->r_name = (struct sockaddr *) (0);
  p->estbchan = (struct chan *) (0);
  p->estbd = 0;

  p->alloctod = todsec();
  p->opentod = (time_t) (0);
  p->conntod = (time_t) (0);
  p->estbtod = (time_t) (0);
  p->disctod = (time_t) (0);
  p->closetod = (time_t) (0);
  p->opendtime = (time_t) (0);
  p->estbdtime = (time_t) (0);
  p->opencount = (unsigned) (0);
  p->conncount = (unsigned) (0);
  p->estbcount = (unsigned) (0);
  p->disccount = (unsigned) (0);
  p->closecount = (unsigned) (0);

  return p;
}
