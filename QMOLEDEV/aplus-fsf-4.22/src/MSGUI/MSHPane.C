///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSHPane.H>

static const int MSHPaneDefaultColumnSpacing=8;

MSHPane::MSHPane(MSWidget *owner_,const char *title_) : MSPane(owner_,title_) 
{ init(); }
MSHPane::MSHPane(MSWidget *owner_,const MSStringVector& title_) : MSPane(owner_,title_) 
{ init(); }

void MSHPane::init(void)
{
  _orientation=MSLayoutManager::Horizontal;
  _rowSpacing=0;
  _columnSpacing=MSHPaneDefaultColumnSpacing;
}

MSHPane::~MSHPane(void) {}









