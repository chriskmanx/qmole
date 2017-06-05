#ifndef included_dap_chan_h
#define included_dap_chan_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* header file inclusions */
#include <limits.h>
#include <dap/fds.h>
#include <dap/node.h>

/* external macro declarations */
#define CHANAT(np)		((struct chan *)((np)->d))

/* chanopen() channel type codes */
#define CHAN_R	(0)
#define CHAN_W	(1)
#define CHAN_X	(2)

/* chan priority values */
#define CHAN_PRI_HIGH		INT_MAX
#define CHAN_PRI_DFLT		(0)
#define CHAN_PRI_LOW		INT_MIN

/* external struct, union, typedef and enum declarations */
struct chan
{
	char *name;				/* channel name */
	int pri;				/* processing priority of channel */
	struct node *np;		/* scheduling node pointer */
	fd_set *fds;			/* enabled fd set */
	fd_set *afds;			/* available fd set */
	int fd;					/* channel file descriptor */
	void (*func)();			/* processing function */
	void *ccbp;				/* channel control block */
};

/* external data declarations */
extern struct node chans;

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct chan *chanopen( const char *,int,int,int,
				void (*)(void *), void * );
  extern void chanclose( struct chan *  );
  extern void chandsbl( struct chan *  );
  extern void chanenbl( struct chan *  );
  extern int chanisdsbl( struct chan *  );
  extern int chanisenbl( struct chan *  );
  extern int chanproc();
  extern void chansetpri( struct chan *, int);
# ifdef __cplusplus
}
# endif
#else
  extern struct chan *chanopen();
  extern void chanclose();
  extern void chandsbl();
  extern void chanenbl();
  extern int chanisdsbl();
  extern int chanisenbl();
  extern int chanproc();
  extern void chansetpri();
#endif

#endif

