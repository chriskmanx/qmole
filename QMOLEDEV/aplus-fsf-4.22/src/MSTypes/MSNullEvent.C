///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSNullEvent.H>

MSNullEvent::~MSNullEvent(void) {}

const MSSymbol& MSNullEvent::symbol(void) 
{
  static MSSymbol sym ("MSNullEvent");
  return sym;
}


