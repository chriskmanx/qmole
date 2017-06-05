///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSRange.H>

ostream& operator<<(ostream& aStream_,const MSRange& aRange_)
{ return aStream_<<"min: "<<aRange_.min()<<"\tmax: "<<aRange_.max()<<"\tlength: "<<aRange_.length(); }

