#ifndef included_dap_tv_h
#define included_dap_tv_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* header file inclusions */
#include <sys/time.h>

/* external macro declarations */
#define MILLION_USECS	((long)(1000000))

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern int tvdiff(struct timeval *,struct timeval *,struct timeval *);
  extern int tvnorm(struct timeval *);
  extern char *tvstring(struct timeval *);
  extern int tvsum(struct timeval *,struct timeval *,struct timeval *);
# ifdef __cplusplus
}
# endif
#else
  extern int tvdiff();
  extern int tvnorm();
  extern char *tvstring();
  extern int tvsum();
#endif
#endif

