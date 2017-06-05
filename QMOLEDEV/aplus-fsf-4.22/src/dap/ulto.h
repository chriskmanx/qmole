#ifndef included_dap_ulto_h
#define included_dap_ulto_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* external macro declarations */
#define ULTOHEX_SZ	9	/* longest hex string for unsigned long */
#define ULTODEC_SZ	11	/* longest decimal string for unsigned long */
#define ULTOOCT_SZ	12	/* longest octal string for unsigned long */

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern int ultodec(unsigned long,char *,int);
  extern int ultohex(unsigned long,char *,int);
  extern int ultooct(unsigned long,char *,int);
# ifdef __cplusplus
}
# endif
#else
  extern int ultodec();
  extern int ultohex();
  extern int ultooct();
#endif

#endif

