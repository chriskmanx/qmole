#ifndef included_a_ik_h
#define included_a_ik_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1996-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

/* This include file contains #defines and externs of general utility
 * within the interpreter.  The difference between this file and k.h
 * is that this file is PURELY INTERNAL TO THE INTERPRETER.  Specifically,
 * this means that the contents of this file are subject to change
 * without notice from one A+ version to the next.
 */

#include <a/k.h>
#include <setjmp.h>

/* Internal global variables */

extern jmptype J;
extern I *Y,*X,*K,sq;
extern PFI g;
extern I Df,Gf,Sf,Tf,Xf,Ef,doErrorStack,oldDepModel;
extern HT CxTable;

/* beam constants */

#define BEAM_RO    0
#define BEAM_RW    1
#define BEAM_LOCAL 2

/* mode-related material */

#define APMODE_ASCII 0
#define APMODE_APL   1
#define APMODE_UNI   2

#define APLpick(mode1,mode0,mode2) (APL?((1==APL)?mode1:mode2):mode0)
#define CC APLpick("\343","//","//")
#define CCtest(s) ((1==APL)?('\343'==*(s)):('/'==(s)[0] && '/'==(s)[1]))

#define SI_CHAR APLpick('&','\376','?')

#endif

