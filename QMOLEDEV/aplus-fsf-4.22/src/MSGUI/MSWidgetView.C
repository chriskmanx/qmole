///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSWidgetView.H>

MSWidgetView::MSWidgetView(void) {}
MSWidgetView::MSWidgetView(MSDisplayServer *server_) : MSWidgetOutput(server_) {}
MSWidgetView::MSWidgetView(MSWidget *owner_) : MSWidgetOutput(owner_) {}
MSWidgetView::~MSWidgetView(void) {}

void MSWidgetView::decoupleWidget(void)
{ decouple(); }

MSBoolean MSWidgetView::isProtected(void) const
{
  return (isReadOnlyModel()==MSTrue || 
	  MSWidgetOutput::isProtected()==MSTrue)?MSTrue:MSFalse;
}








