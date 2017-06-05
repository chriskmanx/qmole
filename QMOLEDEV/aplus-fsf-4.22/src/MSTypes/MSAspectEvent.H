#ifndef MSAspectEventHEADER
#define MSAspectEventHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////



#include <MSTypes/MSEvent.H>
#include <MSTypes/MSSymbol.H>

class MSTypesExport MSAspectEvent : public MSEvent
{
public:
  MSAspectEvent(void);
  MSAspectEvent(const MSAspectEvent & );
  MSAspectEvent(const MSSymbol & tag_);
  virtual ~MSAspectEvent(void);
  MSAspectEvent & operator=(const MSAspectEvent &);

  static const MSSymbol& symbol(void);
  const MSSymbol & tag (void) const;

protected:
  MSSymbol        _tag;
};

inline const MSSymbol& MSAspectEvent::tag (void) const
{ return _tag; }

inline MSAspectEvent::MSAspectEvent(void)
    : MSEvent(MSAspectEvent::symbol())
{}

inline MSAspectEvent::MSAspectEvent(const MSAspectEvent & ev_)
    : MSEvent(ev_), _tag(ev_._tag)
{}

inline MSAspectEvent::MSAspectEvent(const MSSymbol & tag_)
    : MSEvent(MSAspectEvent::symbol()), _tag(tag_)
{}

inline MSAspectEvent& MSAspectEvent::operator=(const MSAspectEvent & ev_)
{
  if (this==&ev_) return *this;
  _tag=ev_._tag;
  MSEvent::operator=(ev_);
  return *this;
}

#endif
