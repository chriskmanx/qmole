#ifndef included_dap_Warn_h
#define included_dap_Warn_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* header file inclusions */
#include <stdio.h>
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
#include <stdarg.h>
#endif

/* external data declarations */
extern char _ErrBuf[];

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
    extern "C" {
# endif
  extern void vWarn(char *,va_list);
  extern void Warn(char*,...);
  extern void Abort(char*,...);
  extern void Panic(char*,...);
  extern void Exit(int, char*,...);
  extern void SetWarn(char*);
  extern void SetWarnFP(FILE*);
  extern void SetWarnFunc(void (*f)(char *));
# ifdef __cplusplus
   }
# endif
#else
  extern void Warn();
  extern void Abort();
  extern void Panic();
  extern void Exit();
  extern void SetWarn();
  extern void SetWarnFP();
  extern void SetWarnFunc();
#endif


#endif

