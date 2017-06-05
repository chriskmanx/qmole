/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <a/development.h>
#include <sys/param.h>
#if defined(HAVE_SVR4)
#include <netdb.h>
#endif
#include <dap/Warn.h>
#include <dap/misc.h>

/* external function definitions */
char *
hostname(void)
{
  static char fnc[] = "hostname";
  static char p[MAXHOSTNAMELEN + 1];

  if (gethostname(p, sizeof(p) - 1) != 0) {
    Warn("%t %s(): error: gethostname(): %m\n", fnc);
  } else {
    p[sizeof(p) - 1] = '\0';
    bcopy(p, hostname_l, sizeof(p));
  }

  return hostname_l;
}
