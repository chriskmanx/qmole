/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * translate internet style address (struct sockaddr_in) into hostname and
 * port style address (struct hpp)
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
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <dap/balloc.h>
#include <dap/hpp.h>

/* external function definitions */
struct hpp *
hppmake_in(struct sockaddr_in * name)
{
  struct hpp *p = (struct hpp *) balloc(sizeof(*p));

  if (name != (struct sockaddr_in *) (0)) {
    if (name->sin_addr.s_addr == INADDR_ANY) {
      p->host = (char *) (0);
    } else {
      struct in_addr *a = &(name->sin_addr);
      struct hostent *hp;

      hp = gethostbyaddr((char *) a, sizeof(*a), AF_INET);
      if (hp == (struct hostent *) (0)) {
	p->host = bstring(inet_ntoa(name->sin_addr));
      } else {
	p->host = bstring(hp->h_name);
      }
    }
    p->port = ntohs(name->sin_port);
  } else {
    p->host = (char *) (0);
    p->port = (ushort) (0);
  }

  return p;
}
