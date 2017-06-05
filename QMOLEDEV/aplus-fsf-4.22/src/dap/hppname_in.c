/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * translate hostname and port style address into an internet style address:
 * struct sockaddr_in
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
#include <errno.h>
#include <dap/Warn.h>
#include <dap/balloc.h>
#include <dap/hpp.h>

/* external function definitions */
struct sockaddr_in *
hppname_in(struct hpp * p, int *lenp)
{
  static char fnc[] = "hppname_in";
  struct hostent *hostp;
  struct sockaddr_in *name;

  name = (struct sockaddr_in *) balloc(sizeof(*name));
  name->sin_family = AF_INET;
  if (p->host != (char *) (0)) {
    if ((hostp = gethostbyname(p->host)) == (struct hostent *) (0)) {
      Warn("%t %s(): error: '%s' not found\n", fnc, p->host);
      bfree((char *) name);
      return (struct sockaddr_in *) (0);
    }
    if (hostp->h_addrtype != AF_INET) {
      Warn("%t %s(): error: '%s' not in AF_INET domain\n",
	   fnc, p->host);
      bfree((char *) name);
      return (struct sockaddr_in *) (0);
    }
    if (hostp->h_length != sizeof(name->sin_addr.s_addr)) {
      Warn("%t %s(): error: '%s' address length mismatch\n",
	   fnc, p->host);
      bfree((char *) name);
      return (struct sockaddr_in *) (0);
    }
    bcopy(hostp->h_addr,
	  (char *) (&(name->sin_addr.s_addr)),
	  sizeof(name->sin_addr.s_addr));
  } else {
    name->sin_addr.s_addr = INADDR_ANY;
  }
  name->sin_port = htons(p->port);
  bzero(name->sin_zero, 8);
  *lenp = sizeof(*name);

  return name;
}
