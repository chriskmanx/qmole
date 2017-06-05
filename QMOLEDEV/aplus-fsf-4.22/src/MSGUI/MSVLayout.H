#ifndef MSVLayoutHEADER
#define MSVLayoutHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSLayout.H>

class MSGUIExport MSVLayout : public MSLayout
{
public:
  MSVLayout(MSWidget *,const char *title_=0);
  MSVLayout(MSWidget *,const MSStringVector&);
  ~MSVLayout(void);
private:
  void init(void);
};

#endif




