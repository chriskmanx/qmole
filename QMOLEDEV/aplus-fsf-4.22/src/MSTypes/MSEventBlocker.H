#ifndef MSEventBlockerHEADER
#define MSEventBlockerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>

class MSEventSender;
class MSTypesExport MSEventBlocker
{
public:
  MSEventBlocker(MSEventSender *,MSBoolean sendEvent_=MSTrue);
  ~MSEventBlocker(void);

private:
  MSEventSender *_sender;
  MSBoolean      _sendEvent;
};

#endif 




