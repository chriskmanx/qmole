#ifndef MSValidationCallbackHEADER
#define MSValidationCallbackHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSString.H>

class MSGUIExport MSValidationCallback
{
public:
  MSValidationCallback(void);
  virtual ~MSValidationCallback(void);
  virtual MSBoolean validate(MSString&)=0;
};

#endif



