#ifndef included_dap_tr_h
#define included_dap_tr_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* external macro declarations */
#define TRCHAR(tr, c)		((int)((tr)[(unsigned)((c)&0xff)))

/* external data declarations */
extern unsigned char tr_atoe[];
extern unsigned char tr_etoa[];
extern unsigned char tr_quadav[];

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern int trchar(unsigned char *, char); /* translate char using table */
# ifdef __cplusplus
}
# endif
#else
  extern int trchar();		            /* translate char using table */
#endif
#endif

