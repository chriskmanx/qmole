#ifndef MSNullEventHEADER
#define MSNullEventHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEvent.H>

class MSTypesExport MSNullEvent : public MSEvent
{
public:
  MSNullEvent(void);
  MSNullEvent(const MSNullEvent&);
  ~MSNullEvent(void);
  
  static const MSSymbol& symbol(void);

  MSNullEvent& operator=(const MSNullEvent& aEvent_);
};

inline MSNullEvent::MSNullEvent(void) : MSEvent(MSNullEvent::symbol()) {}
inline MSNullEvent::MSNullEvent(const MSNullEvent& aEvent_) : MSEvent(aEvent_.symbol()) {}

inline MSNullEvent& MSNullEvent::operator=(const MSNullEvent& aEvent_)
{ sender(aEvent_.sender()); return *this; }

#endif 



