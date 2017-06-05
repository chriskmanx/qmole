#ifndef included_dap_mtm_h
#define included_dap_mtm_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* many to many relationships */

/* header file inclusions */
#include <dap/node.h>

/* external macro declarations */
#define MTMAT(np)		((struct mtm *)((np)->d))

/* external struct, union, typedef and enum declarations */
struct mtm
{
	struct node *unp;
	void *up;
	struct node *dnp;
	void *dp;
	void *d;
};

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct mtm *mtmalloc(void *);
  extern void mtmdnins(struct mtm *,struct node *,void *);
  extern void mtmdnrm(struct mtm *);
  extern void mtmfree(struct mtm *);
  extern struct mtm *mtmlink(struct node *,void *,struct node *,
			     void *,void *);
  extern void mtmunlink(struct mtm *);
  extern void mtmupins(struct mtm *,struct node *,void *);
  extern void mtmuprm(struct mtm *);
  extern void *mtmvalue(struct node *);
# ifdef __cplusplus
}
# endif
#else
  extern struct mtm *mtmalloc();
  extern void mtmdnins();
  extern void mtmdnrm();
  extern void mtmfree();
  extern struct mtm *mtmlink();
  extern void mtmunlink();
  extern void mtmupins();
  extern void mtmuprm();
  extern void *mtmvalue();
#endif

#endif

