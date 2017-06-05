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
#include <dap/tv.h>

/* external function definitions */
char *
tvstring(struct timeval * p)
{
  if (p == (struct timeval *) (0)) {
    return bstring("00000000.000000.000000");
  } else {
    struct tm *tm;

    (void) tvnorm(p);
    tm = gmtime((const time_t *)&(p->tv_sec) );
    return bgprintf(23, "%04d%02d%02d.%02d%02d%02d.%06ld",
		    tm->tm_year + 1900, tm->tm_mon + 1,
		    tm->tm_mday, tm->tm_hour, tm->tm_min,
		    tm->tm_sec, p->tv_usec);
  }
}
