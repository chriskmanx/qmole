#ifndef included_dap_conn_h
#define included_dap_conn_h

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

/* conn retry values */
#define CONN_RETRY_NO		(0)
#define CONN_RETRY_YES		(1)

/* external struct, union, typedef and enum declarations */
struct conn
{
	/* configuration information */
	char *name;					/* connection identification name */
	int pri;					/* connection establishment priority */
	int retry;					/* retry connection after failure */
	int domain;					/* socket address domain */
	int type;					/* socket type */
	int protocol;				/* socket protocol */
	void *r_nameinfo;			/* remote name information */
	int (*setupfunc)();			/* user setup function */
	struct sockaddr *(*r_namefunc)();	/* r_nameinfo to r_name */
	int (*estbfunc)();			/* establishment notification func */
	void *acb;					/* application's control block */

	/* state information */
	struct exbo *retry_time;	/* retry time limit with back-off */
	struct timer *retry_tp;		/* retry timer */
	int fd;						/* file descriptor */
	int r_namelen;				/* remote name length */
	struct sockaddr *r_name;	/* remote name */
	struct chan *estbchan;		/* connection establishment channel */
	int estbd;					/* established */

	/* administrative information */
	time_t alloctod;			/* time connection was allocated */
	time_t opentod;				/* time connection was opened */
	time_t conntod;				/* time connection was attempted */
	time_t estbtod;				/* time connection was established */
	time_t disctod;				/* time connection was disconnected */
	time_t closetod;			/* time connection was closed */
	time_t opendtime;			/* seconds opened in prev instances */
	time_t estbdtime;			/* seconds estbd in prev instances */
	unsigned opencount;			/* number of times opened */
	unsigned conncount;			/* number of times connected */
	unsigned estbcount;			/* number of times established */
	unsigned disccount;			/* number of times disconnected */
	unsigned closecount;		/* number of times closed */
};

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern void connackestb(struct conn *);
  extern struct conn *connalloc(
				char *,int,int,int,int,int,int,int,
				void *,	int (*)(),struct sockaddr *(*)(),
				int (*)(),void *);
  extern void connclose(struct conn *);
  extern void connfree(struct conn *);
  extern void connopen(struct conn *);
# ifdef __cplusplus
}
# endif
#else
  extern void connackestb();
  extern struct conn *connalloc();
  extern void connclose();
  extern void connfree();
  extern void connopen();
#endif

#endif

