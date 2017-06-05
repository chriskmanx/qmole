/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Jordan Hayes */

/* header file inclusions */
#include <a/development.h>
#include <netdb.h>
#include <string.h>
#include <dap/Warn.h>
#include <dap/misc.h>

/* external function definitions */
int 
hostcmp(char *host)
{
  static char fnc[] = "hostcmp";
  struct hostent *hp;

  if ((hp = gethostbyname(host)) == (struct hostent *) (0)) {
    Warn("%t %s(): error: gethostbyname(%s): host not found\n",
	 fnc, host);
    return -1;
  }
  if (strcmp((DEV_STRARG) hp->h_name, (DEV_STRARG) hostname())) {
    return 1;
  }
  return 0;
}
