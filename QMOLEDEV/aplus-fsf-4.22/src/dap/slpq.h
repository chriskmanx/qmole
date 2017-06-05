#ifndef included_dap_slpq_h
#define included_dap_slpq_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* header file inclusions */
#include <dap/node.h>

/* external macro declarations */
#define SLPQENTAT(np)	((struct slpqent *)((np)->d))

/* external struct, union, typedef and enum declarations */

/* structure for saving sleep queue state information */
struct slpq
{
	struct node *wq;	/* queue of waiting callbacks */
	int wakes;			/* number unmatched signals */
	int maxwakes;		/* maximum unmatched signals allowed */
};

/* structure for saving callback information */
struct slpqent
{
	struct node *np;	/* queueing node */
	struct slpq *sp;	/* pointer to sleep queue */
	void (*func)();		/* callback function */
	void *arg;			/* callback function argument */
	int sched;			/* whether it is already scheduled */
};

/* external data declarations */
extern struct node slpqents;

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct slpq *slpqalloc(int,int);
  extern void slpqfree( struct slpq *);
  extern void slpqgiveup(struct slpqent *);
  extern void slpqimmed(void (*)(),void *);
  extern int slpqproc(void);
  extern void slpqsched(struct slpqent *,void (*)());
  extern struct slpqent *slpqsleep(struct slpq *,void (*)(),
				   void *,void (*)() );
  extern void slpqwakeup( struct slpq *,void (*)());
# ifdef __cplusplus
}
# endif
#else
  extern struct slpq *slpqalloc();
  extern void slpqfree();
  extern void slpqgiveup();
  extern void slpqimmed();
  extern int slpqproc();
  extern void slpqsched();
  extern struct slpqent *slpqsleep();
  extern void slpqwakeup();
#endif
#endif



