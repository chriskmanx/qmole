#ifndef included_dap_args_h
#define included_dap_args_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

/* args functions assume that argc and argv will not be altered
 * while processing the arguments, thus the check for empty list
 * or null arguments only occurs at the start of processing of a
 * given list or argument.
 */


/* external data declarations */
extern int args_argpos;
extern int args_index;
extern char *args_value;

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
    extern "C" {
# endif
      extern int argsfirst(int, char**);
      extern int argsgetopt(int, char**,char *);
      extern void argsnext(int, char**);
# ifdef __cplusplus
    }
# endif
#else
  extern int argsfirst();
  extern int argsgetopt();
  extern void argsnext();
#endif

#endif

