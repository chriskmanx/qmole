#ifndef MSEventHEADER
#define MSEventHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSSymbol.H>

class MSEventSender;

class MSTypesExport MSEvent
{
public:
  MSEvent(void);
  MSEvent(const MSEvent&);
  MSEvent(const MSSymbol&);
  virtual ~MSEvent(void);
  
  const MSSymbol& type(void) const;
  MSEventSender *sender(void) const;

  MSEvent& operator=(const MSEvent&);

protected:
friend class MSEventSender;
  
  void sender(MSEventSender *);

  MSSymbol       _type;
  MSEventSender *_sender;
};

inline const MSSymbol& MSEvent::type(void) const 
{ return _type; }
inline MSEventSender *MSEvent::sender(void) const 
{ return _sender; }
inline void MSEvent::sender(MSEventSender *pSender_) 
{ _sender=pSender_; }

inline MSEvent::MSEvent(void) : _sender(0) {}
inline MSEvent::MSEvent(const MSEvent& aEvent_) : _type(aEvent_.type()),_sender(aEvent_.sender()) {}
inline MSEvent::MSEvent(const MSSymbol& aSymbol_) : _type(aSymbol_),_sender(0) {} 

inline MSEvent& MSEvent::operator=(const MSEvent& aEvent_)
{ _type=aEvent_.type(),_sender=aEvent_.sender(); return *this; }

#endif 




