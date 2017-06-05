#ifndef included_a_arthur_h
#define included_a_arthur_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1996-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <a/k.h>

#define R return
#define Z static
#define H printf
#define NL H("\n")
#define CS(n,x) case n:x;break;
#define CSR(n,x) case n:x;
#define DO(n,x) {I i=0,_i=(n);for(;i<_i;++i){x;}}
#define PERR(s,x) {if((I)(x)==-1)R perr(s),0;}
#define W(x) {z=(A)(x);}

#endif
