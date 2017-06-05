#ifndef included_dap_exbo_h
#define included_dap_exbo_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* external struct, union, typedef and enum declarations */
struct exbo
{
	unsigned negative;
	unsigned first;
	unsigned current;
	unsigned last;
};

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
 extern struct exbo *exboalloc(int,int);
 extern int exbobackoff(struct exbo *);
 extern void exbofree(struct exbo *);
 extern int exboreset(struct exbo *);
 extern int exbovalue(struct exbo *);
# ifdef __cplusplus
}
# endif
#else
 extern struct exbo *exboalloc();
 extern int exbobackoff();
 extern void exbofree();
 extern int exboreset();
 extern int exbovalue();
#endif
#endif

