#ifndef included_dap_timer_h
#define included_dap_timer_h

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
#include <dap/node.h>

/* external macro declarations */
#define TIMERAT(np)		((struct timer *)((np)->d))

/* external struct, union, typedef and enum declarations */
struct timer
{
	struct node *np;
	struct timeval expire;
	void (*func)();
	void *arg;
};

/* external data declarations */
extern struct node timers;

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct timer *timer(time_t,long,void (*)(void *),void *);
  extern struct timer *timerabs(time_t,long,void (*)(),void *);
  extern void timerclr(struct timer *);
  extern struct timeval *timernext(void);
  extern int timerproc(void);
# ifdef __cplusplus
}
# endif
#else
  extern struct timer *timer();
  extern struct timer *timerabs();
  extern void timerclr();
  extern struct timeval *timernext();
  extern int timerproc();
#endif
#endif

