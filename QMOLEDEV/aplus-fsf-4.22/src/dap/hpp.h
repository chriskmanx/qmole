#ifndef included_dap_hpp_h
#define included_dap_hpp_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* header file inclusions */
#include <sys/types.h>
#include <sys/uio.h>
#include <netinet/in.h>

/* external struct, union, typedef and enum declarations */
struct hpp
{
	char *host;				/* hostname */
	unsigned short port;			/* port number */
};

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct hpp *hppalloc(char *, unsigned short);
  extern void hppfree(struct hpp *);
  extern struct hpp *hppmake_in(struct sockaddr_in *);
  extern struct sockaddr_in *hppname_in(struct hpp *,int *);
# ifdef __cplusplus
}
# endif
#else
  extern struct hpp *hppalloc();
  extern void hppfree();
  extern struct hpp *hppmake_in();
  extern struct sockaddr_in *hppname_in();
#endif

#endif

