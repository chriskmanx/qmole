#ifndef MSVPaneHEADER
#define MSVPaneHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSPane.H>

class MSGUIExport MSVPane : public MSPane
{
public:
  MSVPane(MSWidget *,const char *title_=0);
  MSVPane(MSWidget *,const MSStringVector&);
  ~MSVPane(void);
protected:
  void init(void);
};

#endif






