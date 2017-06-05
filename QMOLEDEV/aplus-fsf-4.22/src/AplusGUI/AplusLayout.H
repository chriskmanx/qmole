#ifndef AplusLayoutHEADER
#define AplusLayoutHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSBinaryVector.H>
#include <AplusGUI/Macros.H>
#include <MSGUI/MSLayout.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>

class AplusLayout : public MSLayout
{
public:
  AplusLayout(MSWidget *);
  ~AplusLayout(void);
  virtual void receiveEvent(MSEvent &);
  virtual MSBoolean verifyData(V,A) { return MSTrue; }

  virtual void addSenderNotify(MSEventSender *);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  STANDARD_WIDGET_METHODS_H
};


#endif // AplusLayoutHEADER
