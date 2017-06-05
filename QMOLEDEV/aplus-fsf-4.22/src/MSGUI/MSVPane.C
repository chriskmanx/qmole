///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSVPane.H>

static const int MSVPaneDefaultRowSpacing=8;

MSVPane::MSVPane(MSWidget *owner_,const char *title_) : MSPane(owner_,title_) 
{ init(); }
MSVPane::MSVPane(MSWidget *owner_,const MSStringVector& title_) : MSPane(owner_,title_) 
{ init(); }

void MSVPane::init(void)
{
  _orientation=MSLayoutManager::Vertical;
  _rowSpacing=MSVPaneDefaultRowSpacing;
  _columnSpacing=0;
}

MSVPane::~MSVPane(void) {}








