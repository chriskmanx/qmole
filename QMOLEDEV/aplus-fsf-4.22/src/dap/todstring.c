/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <sys/types.h>
#if !defined(sun)
#include <time.h>
#endif
#include <sys/time.h>
#include <dap/balloc.h>
#include <dap/tod.h>

/* external function definitions */
char *
todstring(time_t t)
{
  struct tm *tm = gmtime(&t);

  return bgprintf(16, "%04d%02d%02d.%02d%02d%02d",
		  tm->tm_year + 1900, tm->tm_mon + 1,
		  tm->tm_mday, tm->tm_hour, tm->tm_min,
		  tm->tm_sec);
}
