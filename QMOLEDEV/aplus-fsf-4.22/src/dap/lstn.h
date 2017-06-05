#ifndef included_dap_lstn_h
#define included_dap_lstn_h

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
#include <dap/notsunos4.h>
#ifdef PRESUNOS4
#	ifndef _SOCKET_
#		define _SOCKET_
#		include <sys/socket.h>
#	endif
#else
#	include <sys/socket.h>
#endif
#include <dap/exbo.h>
#include <dap/chan.h>
#include <dap/timer.h>

/* external macro declarations */

/* lstn retry values */
#define LSTN_RETRY_NO		(0)
#define LSTN_RETRY_YES		(1)

/* external struct, union, typedef and enum declarations */
struct lstn
{
	/* configuration information */
	char *name;					/* listener's name */
	int pri;					/* connection accept priority */
	int retry;					/* retry listening after failure */
	int domain;					/* socket address domain */
	int type;					/* socket type */
	int protocol;				/* socket protocol */
	int r_namelen;				/* maximum remote name len on accept */
	void *l_nameinfo;			/* local name information */
	int (*l_namefunc)();		/* l_nameinfo to l_name func */
	int (*l_regfunc)();			/* local name registration function */
	void (*acptfunc)();			/* connection accept function */
	void *acb;					/* application's control block */

	/* state information */
	struct exbo *retry_time;	/* retry time limit with back-off */
	struct timer *retry_tp;		/* retry timer */
	int fd;						/* file descriptor */
	int l_namelen;				/* local name length */
	struct sockaddr *l_name;	/* local name */
	struct chan *acptchan;		/* connection accept channel */

	/* administrative information */
	time_t alloctod;			/* time listener was allocated */
	time_t opentod;				/* time listener was opened */
	time_t lstntod;				/* time listener started listening */
	time_t acpttod;				/* time of last accept */
	time_t deaftod;				/* time of listener stopped listening */
	time_t closetod;			/* time of last close */
	time_t opendtime;			/* seconds opened in prev instances */
	time_t lstndtime;			/* seconds listened in prev instnaces */
	unsigned opencount;			/* number of times opened */
	unsigned lstncount;			/* number of times listening */
	unsigned acptcount;			/* number of connections accepted */
	unsigned deafcount;			/* number of times stopped listening */
	unsigned closecount;		/* number of times closed */
};

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct lstn *lstnalloc(
				char *,int,int,int,int,int,int,int,int,
				void *,int (*)(),int (*)(),void (*)(),void *
				);
  extern void lstnclose(struct lstn *);
  extern void lstnfree(struct lstn *);
  extern void lstnopen(struct lstn *);
# ifdef __cplusplus
}
# endif
#else
  extern struct lstn *lstnalloc();
  extern void lstnclose();
  extern void lstnfree();
  extern void lstnopen();
#endif

#endif

