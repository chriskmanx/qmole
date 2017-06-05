#ifndef MSModalPopupHEADER
#define MSModalPopupHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSPopup.H>

class MSGUIExport MSModalPopup : public MSPopup
{
public:
  MSModalPopup(const char *windowTitle_=0);
  MSModalPopup(MSDisplayServer *,const char *windowTitle_=0);
  MSModalPopup(MSShell *,const char *windowTitle_=0);
  ~MSModalPopup(void);
};

#endif
