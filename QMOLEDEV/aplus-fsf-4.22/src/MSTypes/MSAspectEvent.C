///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSAspectEvent.H>



MSAspectEvent::~MSAspectEvent(void)
{}

const MSSymbol& MSAspectEvent::symbol(void) 
{
  static MSSymbol symbol("MSAspectEvent");
  return symbol;
}
