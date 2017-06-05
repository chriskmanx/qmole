///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSIndexedEvent.H>

MSIndexedEvent::~MSIndexedEvent(void) {}

const MSSymbol& MSIndexedEvent::symbol(void) 
{
  static MSSymbol sym ("MSIndexedEvent");
  return sym;
}

