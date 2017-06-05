#ifndef MSWidgetIteratorHEADER
#define MSWidgetIteratorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSGUIEnum.H>

class MSWidget;

typedef MSBoolean (*MSWidgetIteratorFunction)(MSWidget*, void*);

class MSGUIExport MSWidgetIterator
{
public:
  MSWidgetIterator(void);
  virtual ~MSWidgetIterator(void);
  virtual MSBoolean applyTo(MSWidget *)=0;
};

#endif











