#ifndef __IPCUtilities_H
#define __IPCUtilities_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#if !defined(__cfront)
#include <stdarg.h>
#endif

extern void ipcWarn(int warnLevel_, const C *fmt_, ...);
extern I longAt(C *c);
extern I shortAt(C *c);
extern struct timeval *atotv(A aobj, struct timeval *tvp);
extern A getAbsoluteTimeout(A aobj_);

// M:Attribute interface

#define ItCHK(aval) if (It!=aval->t||1!=aval->n||0>(ival=*aval->p)) \
                      return MSFalse;

#define RtCHK(aval,range) ItCHK(aval); if(range<ival) return MSFalse;

#define BoolCHK(aval) RtCHK(aval,1);

#define CS(n,x) case n:x;break;
#define CSR(n,x) case n:x;
#define CSBOOL(n,aval,ifT,ifF) CS(n,BoolCHK(aval);if(ival)ifT();else ifF())

#endif
