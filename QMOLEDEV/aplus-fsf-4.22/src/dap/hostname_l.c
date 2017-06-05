/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * hostname_l is the last hostname returned by the operating system.
 */

/* header file inclusions */
#include <sys/param.h>
#if defined(HAVE_SVR4)
#include <netdb.h>
#endif

/* external data definitions */
char hostname_l[MAXHOSTNAMELEN + 1] = "UNKNOWN";
