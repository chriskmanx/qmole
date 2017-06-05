#ifndef MSHPaneHEADER
#define MSHPaneHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSPane.H>

class MSGUIExport MSHPane : public MSPane
{
public:
  MSHPane(MSWidget *,const char *title_=0);
  MSHPane(MSWidget *,const MSStringVector&);
  ~MSHPane(void);
protected:
  void init(void);
};

#endif






