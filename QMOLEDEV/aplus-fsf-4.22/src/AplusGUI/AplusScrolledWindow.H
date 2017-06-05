#ifndef AplusScrolledWindowHEADER
#define AplusScrolledWindowHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSScrolledWindow.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>

class AplusScrolledWindow : public MSScrolledWindow
{
public:
  AplusScrolledWindow(MSWidget *);
  ~AplusScrolledWindow(void);
  virtual void receiveEvent(MSEvent &);
  virtual MSBoolean verifyData(V,A) { return MSTrue; }

  virtual void addSenderNotify(MSEventSender *);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  STANDARD_WIDGET_METHODS_H
};

#endif // AplusScrolledWindowHEADER
