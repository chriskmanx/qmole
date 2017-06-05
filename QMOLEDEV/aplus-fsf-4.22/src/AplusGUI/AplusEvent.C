///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusEvent.H>

AplusEvent::~AplusEvent(void) {}

A AplusEvent::index(void) { return _index; }
A AplusEvent::pick(void) { return _pick; }
I AplusEvent::ravel(void) { return _ravel; }

const MSSymbol& AplusEvent::symbol(void)
{
  static MSSymbol sym("AplusEvent");
  return sym;
}


AplusVerifyEvent::~AplusVerifyEvent(void) {}
  
A AplusVerifyEvent::a(void) { return _a; }
V AplusVerifyEvent::aplusVar(void) { return _aplusVar; }
MSBoolean AplusVerifyEvent::result(void) { return _result; }
void AplusVerifyEvent::result(MSBoolean r_) { _result = r_; }

const MSSymbol& AplusVerifyEvent::symbol(void)
{
  static MSSymbol sym("AplusVerifyEvent");
  return sym;
}


const MSSymbol& AplusUpdateDataEvent::symbol(void)
{
  static MSSymbol sym("AplusUpdateDataEvent");
  return sym;
}


AplusProtectEvent::AplusProtectEvent(void) : MSEvent(AplusProtectEvent::symbol()) {}
AplusProtectEvent::~AplusProtectEvent(void) {}

const MSSymbol& AplusProtectEvent::symbol(void)
{
  static MSSymbol sym("AplusProtectEvent");
  return sym;
}


AplusUpdateTitleEvent::AplusUpdateTitleEvent(void) : MSEvent(AplusUpdateTitleEvent::symbol()) {}
AplusUpdateTitleEvent::~AplusUpdateTitleEvent(void) {}

const MSSymbol& AplusUpdateTitleEvent::symbol(void)
{
  static MSSymbol sym("AplusUpdateTitleEvent");
  return sym;
}
