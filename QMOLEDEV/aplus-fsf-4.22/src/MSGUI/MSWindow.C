///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSWindow.H>

MSWindow::MSWindow(const char * windowTitle_) :
MSMenuShell(windowTitle_)
{ init(); }

MSWindow::MSWindow(MSDisplayServer *server_, const char * windowTitle_) :
MSMenuShell( server_, windowTitle_)
{ init(); }

MSWindow::~MSWindow(void)
{
}

void MSWindow::init(void)
{
  MSLayout *layout=new MSLayout(this);
  layout->shadowThickness(0);
}


  
