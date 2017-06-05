#ifndef included_dap_balloc_h
#define included_dap_balloc_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


#include <a/development.h>

/* extern macro declarations */
#define bnew(t)		(t *)balloc(sizeof(t))
#define bnew_r(t)	(t *)balloc_r(sizeof(t))

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern char *bgprintf(int, char *, ...);
  extern char *bnstring(char *, ...);
  extern char *bnstring_r(char *, ...);
  extern char *bstring(const char *);
  extern char *bstring_r(char *);
  extern int bstrhash(char *);
  extern void *balloc(int);
  extern void *balloc_r(int);
  extern void *bdup(void *, int);
  extern void *bdup_r(void *, int);
  extern void *brealloc(char *, int);
  extern void *brealloc_r(char *, int);
  extern void bfree(char *);
# ifdef __cplusplus
}
# endif
#else
  extern char *bgprintf();
  extern char *bnstring();
  extern char *bnstring_r();
  extern char *bstring();
  extern char *bstring_r();
  extern int bstrhash();
  extern void *balloc();
  extern void *balloc_r();
  extern void *bdup();
  extern void *bdup_r();
  extern void *brealloc();
  extern void *brealloc_r();
  extern void bfree();
#endif
#endif
