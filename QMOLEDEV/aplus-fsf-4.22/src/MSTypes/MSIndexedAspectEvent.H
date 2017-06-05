#ifndef MSIndexedAspectEventHEADER
#define MSIndexedAspectEventHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////



#include <MSTypes/MSEvent.H>
#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSSymbol.H>

class MSTypesExport MSIndexedAspectEvent : public MSEvent
{
public:
  MSIndexedAspectEvent(void);
  MSIndexedAspectEvent(const MSIndexedAspectEvent&);
  MSIndexedAspectEvent(const MSIndexVector&, const MSSymbol& tag=MSSymbol("*"));
  virtual ~MSIndexedAspectEvent(void);
  
  MSIndexedAspectEvent& operator=(const MSIndexedAspectEvent& aEvent_);

  static const MSSymbol& symbol(void);
  const MSIndexVector& index(void) const;
  const MSSymbol& tag(void) const;
  
protected:
  MSSymbol        _tag;
  MSIndexVector   _index;
};

inline const MSSymbol& MSIndexedAspectEvent::tag(void) const
{ return _tag; }
inline const MSIndexVector& MSIndexedAspectEvent::index(void) const
{ return _index; }
  
inline MSIndexedAspectEvent::MSIndexedAspectEvent(void)
    : MSEvent(MSIndexedAspectEvent::symbol()), _tag("*")
{}

inline MSIndexedAspectEvent::MSIndexedAspectEvent(const MSIndexedAspectEvent& aEvent_)
    : MSEvent(aEvent_.symbol()), _index(aEvent_.index()), _tag(aEvent_.tag())
{}

inline MSIndexedAspectEvent::MSIndexedAspectEvent(const MSIndexVector& aIntVector_, const MSSymbol & tag_)
    : 
    MSEvent(MSIndexedAspectEvent::symbol()),_index(aIntVector_),_tag(tag_)
{}

inline MSIndexedAspectEvent& MSIndexedAspectEvent::operator=(const MSIndexedAspectEvent& aEvent_)
{
  if (this!=&aEvent_)
   {
     _index=aEvent_.index(),_tag=aEvent_.tag(),sender(aEvent_.sender());
   }
  return *this;
}

#endif
