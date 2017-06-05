#ifndef MSCallbackHEADER
#define MSCallbackHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

class MSTypesExport MSCallback
{
public:
  MSCallback(void);
  virtual ~MSCallback(void);

  virtual void process()=0;
};

#endif






