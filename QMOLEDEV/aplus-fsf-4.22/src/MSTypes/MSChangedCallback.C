///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSChangedCallback.H>

MSChangedCallback::MSChangedCallback(void) {}
MSChangedCallback::~MSChangedCallback(void) {}

void MSChangedCallback::receiveEvent(MSEvent& aEvent_)
{ process(aEvent_.sender()); }











