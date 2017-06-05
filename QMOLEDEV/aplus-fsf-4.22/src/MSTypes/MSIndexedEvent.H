#ifndef MSIndexedEventHEADER
#define MSIndexedEventHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEvent.H>
#include <MSTypes/MSIndexVector.H>

class MSTypesExport MSIndexedEvent : public MSEvent
{
public:
  MSIndexedEvent(void);
  MSIndexedEvent(const MSIndexedEvent&);
  MSIndexedEvent(const MSIndexVector&);
  MSIndexedEvent(unsigned int);
  ~MSIndexedEvent(void);
  
  static const MSSymbol& symbol(void);
  const MSIndexVector& index(void) const;

  MSIndexedEvent& operator=(const MSIndexedEvent& aEvent_);

private:
  MSIndexVector   _index;
};

inline const MSIndexVector& MSIndexedEvent::index(void) const
{ return _index; }
 
inline MSIndexedEvent::MSIndexedEvent(void) : MSEvent(MSIndexedEvent::symbol()) {}

inline MSIndexedEvent::MSIndexedEvent(const MSIndexedEvent& aEvent_) : 
_index(aEvent_.index()),MSEvent(aEvent_.symbol()) {}

inline MSIndexedEvent::MSIndexedEvent(const MSIndexVector& aIntVector_) : 
_index(aIntVector_),MSEvent(MSIndexedEvent::symbol()) {}

inline MSIndexedEvent::MSIndexedEvent(unsigned int index_) :
_index(1,index_),MSEvent(MSIndexedEvent::symbol()) {}

inline MSIndexedEvent& MSIndexedEvent::operator=(const MSIndexedEvent& aEvent_)
{ _index=aEvent_.index(),sender(aEvent_.sender()); return *this; }

#endif 



