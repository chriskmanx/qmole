#ifndef AplusEventHEADER
#define AplusEventHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSEvent.H>
#include <MSTypes/MSIndexedEvent.H>
#include <a/fncdcls.h>

class AplusEvent : public MSEvent
{
public:
  AplusEvent(void);
  AplusEvent(AplusEvent &);
  AplusEvent(A,A,I);
  ~AplusEvent(void);


  A index(void);
  A pick(void);
  I ravel(void);
  static const MSSymbol& symbol(void);

protected:
  A _index;
  A _pick;
  I _ravel;
  
};

class AplusVerifyEvent : public MSEvent
{
public:
  AplusVerifyEvent(void);
  AplusVerifyEvent(AplusVerifyEvent &);
  AplusVerifyEvent(V,A);
  ~AplusVerifyEvent(void);

  A a(void);
  V aplusVar(void);
  MSBoolean result(void);
  void result(MSBoolean r_);
  
  static const MSSymbol& symbol(void);

protected:
  V _aplusVar;
  A _a;
  MSBoolean _result;
};

class AplusUpdateDataEvent : public MSEvent
{
public:
  AplusUpdateDataEvent(void);
  ~AplusUpdateDataEvent(void) {}
  
  static const MSSymbol& symbol(void);
};


class AplusProtectEvent : public MSEvent
{
public:
  AplusProtectEvent(void);
  ~AplusProtectEvent(void);

  static const MSSymbol& symbol(void);
};


class AplusUpdateTitleEvent : public MSEvent
{
public:
  AplusUpdateTitleEvent(void);
  ~AplusUpdateTitleEvent(void);

  static const MSSymbol& symbol(void);
};


inline AplusUpdateDataEvent::AplusUpdateDataEvent(void) : MSEvent(AplusUpdateDataEvent::symbol()) 
{}

inline AplusVerifyEvent::AplusVerifyEvent( AplusVerifyEvent &apve_) :
_aplusVar(apve_.aplusVar()),_a(apve_.a()), _result(apve_.result()), MSEvent(AplusVerifyEvent::symbol())
{}

inline AplusVerifyEvent::AplusVerifyEvent(void) : MSEvent(AplusVerifyEvent::symbol()) {}

inline AplusVerifyEvent::AplusVerifyEvent(V v_, A a_) :
_aplusVar(v_), _a(a_), _result(MSFalse), MSEvent(AplusVerifyEvent::symbol())
{}


inline AplusEvent::AplusEvent( AplusEvent &ape_) :
_index(ape_.index()),_pick(ape_.pick()), _ravel(ape_.ravel()), MSEvent(AplusEvent::symbol())
{}

inline AplusEvent::AplusEvent(void) : MSEvent(AplusEvent::symbol()) {}

inline AplusEvent::AplusEvent(A index_, A pick_, I ravel_) :
_index(index_), _pick(pick_), _ravel(ravel_), MSEvent(AplusEvent::symbol())
{}

#endif // AplusEventHEADER
