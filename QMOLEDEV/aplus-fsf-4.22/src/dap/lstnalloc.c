/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * The lstn functions implement a persistent listener.  It waits for
 * connections at the indicated address, accepting connections as they
 * arrive.  When a connection arrives, an application provided accpet
 * notification function is called, so that it may set up the application
 * specific handling for the connection.  The listner continues to listen for
 * furhter connections, unless the application either disables it, closes it
 * or destroys it.  Should the listener encounter an error while either
 * setting itself up or while accepting an incoming connection, it will close
 * and schedule itself to be reopened in the near future.  The computation of
 * the listening address is performed at each open by an application provided
 * l_namefunc.  The computation uses l_nameinfo as its input.  The listner
 * keeps trakc of the number of times it is opened and closed, the time it
 * was created, the time it was last opened, the time it was last closed, the
 * time it last accepted a connection.  The total time it has been listening.
 * No attempt is made to associate accepted connections with the listener
 * after they are accepted, though an application could certainly arrange for
 * this.
 * 
 * l_nameinfo to l_name, l_namelen translation is provided by the l_namefunc,
 * which returns l_name and l_namelen.  The l_name and l_namelen are used to
 * bind a name to the listening socket.  No bind is performed if the l_name
 * is returned as null.  Since l_name can include wild cards which leaves
 * selection of the exact l_name upto the system, a getsockname is performed
 * after the listen, in order to extract the exact listening address.  This
 * exact address it then handed to the l_regfunc function provided by the
 * application.  The l_regfunc function gives the application the opportunity
 * to publish the listening address.  The l_namelen returned by the
 * l_namefunc is used by the lstnopen function when is does its getsockname,
 * so this should be given a reasonable value even if the l_namefunc returns
 * a null l_name.  The l_regfunc function should return 0 if it succeeds and
 * -1 if it fails and wishes for the listening port to be closed.
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
#include <dap/lstn.h>

/* external function definitions */
struct lstn *lstnalloc(char *name, int pri, int retry, int retry_time_first, int retry_time_last, int domain, int type, int protocol, int r_namelen, void *l_nameinfo, int (*l_namefunc) (), int (*l_regfunc) (), void (*acptfunc) (), void *acb) {
  struct lstn *p = (struct lstn *) balloc(sizeof(*p));

  if (name == (char *) (0))
    name = "<UNKNOWN>";
  p->name = bstring(name);
  p->pri = pri;
  if (retry == LSTN_RETRY_NO)
    p->retry = LSTN_RETRY_NO;
  else
    p->retry = LSTN_RETRY_YES;
  p->domain = domain;
  p->type = type;
  p->protocol = protocol;
  p->r_namelen = r_namelen;
  p->l_nameinfo = l_nameinfo;
  p->l_namefunc = l_namefunc;
  p->l_regfunc = l_regfunc;
  p->acptfunc = acptfunc;
  p->acb = acb;
  p->retry_time = exboalloc(retry_time_first, retry_time_last);

  p->retry_tp = (struct timer *) (0);
  p->fd = -1;
  p->l_namelen = 0;
  p->l_name = (struct sockaddr *) (0);
  p->acptchan = (struct chan *) (0);

  p->alloctod = todsec();
  p->opentod = (time_t) (0);
  p->lstntod = (time_t) (0);
  p->acpttod = (time_t) (0);
  p->deaftod = (time_t) (0);
  p->closetod = (time_t) (0);
  p->opendtime = (time_t) (0);
  p->lstndtime = (time_t) (0);
  p->opencount = (unsigned) (0);
  p->lstncount = (unsigned) (0);
  p->acptcount = (unsigned) (0);
  p->deafcount = (unsigned) (0);
  p->closecount = (unsigned) (0);

  return p;
}
