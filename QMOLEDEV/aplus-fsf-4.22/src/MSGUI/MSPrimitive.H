#ifndef MSPrimitiveHEADER
#define MSPrimitiveHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetCommon.H>

class MSGUIExport MSPrimitive : public MSWidgetCommon
{
public:
  MSPrimitive(MSWidget *,const char *title_=0);
  MSPrimitive(MSWidget *,const MSStringVector&);
  ~MSPrimitive(void);

  virtual void redraw(void); 
  virtual void print(const char *file_=0);

protected:
  void init(void);
  void verifyBell(void);

  virtual void focusIn(void);
  virtual void focusOut(void);
};

#endif











