#ifndef MSWidgetViewHEADER
#define MSWidgetViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSView.H>
#include <MSGUI/MSWidgetOutput.H>

class MSGUIExport MSWidgetView : public MSWidgetOutput,public MSView
{
public:
  MSWidgetView(void);
  MSWidgetView(MSDisplayServer *);
  MSWidgetView(MSWidget *);
  ~MSWidgetView(void);
protected:
  virtual void decoupleWidget(void);
  virtual MSBoolean isProtected(void) const;
};

#endif



