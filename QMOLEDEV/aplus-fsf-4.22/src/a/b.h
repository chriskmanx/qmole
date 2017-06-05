#ifndef included_a_b_h
#define included_a_b_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

/* header file inclusions */
#include <sys/types.h>
#include <a/fncdcls.h>

/* external macro declarations */
#if (defined(__sgi) && _MIPS_SZLONG == 64) || defined(__sparcv9)
#define MD      63
#else
#define MD	31
#endif

/* external data declarations */
/* number of words in block of given scale */
extern unsigned long MZ[];

#endif
