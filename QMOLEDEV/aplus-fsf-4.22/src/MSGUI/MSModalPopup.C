///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSModalPopup.H>

MSModalPopup::MSModalPopup(const char *windowTitle_) : MSPopup(windowTitle_)
{
   modal(MSTrue);
}

MSModalPopup::MSModalPopup(MSDisplayServer *server_,const char *windowTitle_) : MSPopup(server_,windowTitle_)
{
   modal(MSTrue);
}

MSModalPopup::MSModalPopup(MSShell *leader_,const char *windowTitle_) : MSPopup(leader_,windowTitle_)
{
   modal(MSTrue);
}

MSModalPopup::~MSModalPopup(void)
{}

