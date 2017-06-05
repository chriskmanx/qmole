///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSColorCycle.H>

MSColorCycle::MSColorCycle(MSWidget *widget_,int row_,int column_,const MSUnsignedLongVector& colors_,MSCycleColorMode mode_)
:_widget(widget_),_row(row_),_column(column_),_colors(colors_),_mode(mode_),_count(0)
{
  _lastUpdate=*tod();
}

MSColorCycle::~MSColorCycle(void) {}

