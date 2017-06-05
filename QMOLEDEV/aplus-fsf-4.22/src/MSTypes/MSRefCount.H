#ifndef MSRefCountHEADER
#define MSRefCountHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

class MSTypesExport MSRefCount
{
private:
  int _count;

public:
  MSRefCount(void) { _count=0; }
  virtual ~MSRefCount(void);

  int count(void) const { return _count; }
  void increment(void)  { _count++; }
  void decrement(void)  { if (--_count==0) delete this; }
};

#endif
