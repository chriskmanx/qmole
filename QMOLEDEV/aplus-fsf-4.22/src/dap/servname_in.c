/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Jordan Hayes */

/*
 * for use with the connalloc and lstnalloc for looking up a service in the
 * remprogs data base and converting it to a AF_INET address.
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
#include <dap/remprog.h>
#include <dap/Warn.h>
#include <dap/balloc.h>

/* external function definitions */
struct sockaddr_in *
servname_in(char *service, int *lenp)
{
  static char fnc[] = "servname_in";
  ServEnt *sp;
  int i;
  struct hostent *hp = NULL;
  struct sockaddr_in *name;

  sp = GetServ(service);

  for (i = 0; i < sp->count; i++) {
    if (NextServ(sp) == -1) {
      i = sp->count;
      break;
    }
    if (sp->host == (char *) (0)) {
      continue;
    }
    if ((hp = gethostbyname(sp->host)) == (struct hostent *) (0)) {
      Warn("%t %s(): error: '%s' not found\n",
	   fnc, sp->host);
      continue;
    }
    if (hp->h_addrtype != AF_INET) {
      Warn("%t %s(): error: '%s' not in AF_INET domain\n",
	   fnc, sp->host);
      continue;
    }
    if (hp->h_length != sizeof(name->sin_addr.s_addr)) {
      Warn("%t %s(): error: '%s' address length mismatch\n",
	   fnc, sp->host);
      continue;
    }
    break;
  }

  if (i >= sp->count) {
    if (sp->count >= 1) {
      Warn("%t %s(): error: can't get a provider for '%s'\n",
	   fnc, service);
    }
    name = (struct sockaddr_in *) (0);
  } else {
    name = (struct sockaddr_in *) balloc(sizeof(*name));
    name->sin_family = AF_INET;
    bcopy(hp->h_addr,
	  (char *) (&(name->sin_addr.s_addr)),
	  sizeof(name->sin_addr.s_addr));
    name->sin_port = htons((unsigned short) (sp->program));
    bzero(name->sin_zero, 8);
    *lenp = sizeof(*name);
  }

  /* TK - ServEntDestroy(sp); */

  return name;
}
