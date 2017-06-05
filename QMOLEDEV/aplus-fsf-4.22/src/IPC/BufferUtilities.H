#ifndef __BufferUtilities_H
#define __BufferUtilities_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

extern int bufftobuff(MSBuffer *sp,MSBuffer *dp,int n);
extern MSBuffer *createBufferFromAobj(const A &aobj);

#endif
