#ifndef MSCompositeHEADER
#define MSCompositeHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetCommon.H>

class MSGUIExport MSComposite : public MSWidgetCommon
{
public:
  MSComposite(MSWidget *);
  ~MSComposite(void);

  virtual void redraw(void);

protected:
  void init(void);
  virtual void focusIn(void);
  virtual void focusOut(void);

  virtual void computeSize(void);
  virtual void placement(void);
  virtual void configure(void);
};

#endif











