/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * dap_tod_last is the last time of day returned by the operating system.
 * Use dap_tod_last.tv_sec to read the seconds and dap_tod_last.tv_usec to
 * read the micro seconds.
 */

/* header file inclusions */
#include <sys/types.h>
#include <sys/time.h>

/* external data definitions */
struct timeval dap_tod_last;
