/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <errno.h>
#include <dap/Warn.h>
#include <dap/misc.h>

/* external function definitions */
void 
doclose(int fd)
{
  static char fnc[] = "doclose";

  if (fd >= 0) {
    while (close(fd) != 0) {
      if (errno != EINTR) {
	Warn("%t %s(): error: close(%d): %m\n", fnc, fd);
	break;
      }
    }
  }
  return;
}
