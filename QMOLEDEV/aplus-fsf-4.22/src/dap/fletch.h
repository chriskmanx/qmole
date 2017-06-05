#ifndef included_dap_fletch_h
#define included_dap_fletch_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* external struct, union, typedef and enum declarations */
struct fletch
{
	int c0;				/* current c0 counter value */
	int c1;				/* current c1 counter value */
	int modfreq;		/* frequency of mod operations */
	int tilmod;			/* number of bytes til next mod operation */
};

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct fletch *fletchalloc(void);
  extern void fletchfree(struct fletch *);
  extern void fletchsum(struct fletch *,char *,int);
  extern int fletchcheck(char *,int,int);
  extern void fletchdone(struct fletch *,char *,int,int);
  extern void fletchpatch(char *,int,char *,int,int);
# ifdef __cplusplus
}
# endif
#else
  extern struct fletch *fletchalloc();
  extern void fletchfree();
  extern void fletchsum();
  extern int fletchcheck();
  extern void fletchdone();
  extern void fletchpatch();
#endif

#endif

