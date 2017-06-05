#ifndef included_dap_tod_h
#define included_dap_tod_h

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
#include <sys/time.h>

/* external data declarations */
extern struct timeval dap_tod_last;

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct timeval *tod(void);
  extern time_t todsec(void);
  extern char *todstring(time_t);
# ifdef __cplusplus
}
# endif
#else
  extern struct timeval *tod();
  extern time_t todsec();
  extern char *todstring();
#endif

#endif

