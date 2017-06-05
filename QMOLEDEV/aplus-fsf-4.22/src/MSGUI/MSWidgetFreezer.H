#ifndef MSWidgetFreezerHEADER
#define MSWidgetFreezerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSEnum.H>

class MSWidget;

class MSGUIExport MSWidgetFreezer
{
public:
  MSWidgetFreezer(MSWidget *);
  ~MSWidgetFreezer();

private:
  MSWidget  *_widget;
  MSBoolean _wasFrozen;
};

#endif 




