///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSHLayout.H>

MSHLayout::MSHLayout(MSWidget *owner_,const char *title_) : MSLayout(owner_,title_) 
{ init(); }
MSHLayout::MSHLayout(MSWidget *owner_,const MSStringVector& title_) : MSLayout(owner_,title_) 
{ init(); }
MSHLayout::~MSHLayout(void) {}

void MSHLayout::init(void)
{ _orientation=MSLayoutManager::Horizontal; }








