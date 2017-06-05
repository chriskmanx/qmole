#ifndef AplusReferenceHEADER
#define AplusReferenceHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSWidgetCommon.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusEvent.H>

class AplusReference : public MSWidgetCommon
{
public:
  AplusReference(MSWidget *);
  ~AplusReference(void);

  virtual void addSenderNotify(MSEventSender *);
  virtual void receiveEvent(MSEvent &);

  virtual MSBoolean verifyData(V,A) { return MSTrue; }
  virtual void update(V,int,int,int) {}
  virtual void update(const MSIndexVector& v_) {  MSView::update(v_); }
  virtual void update(V,A,A,I) {}

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);
  STANDARD_WIDGET_METHODS_H
};

#endif // AplusReferenceHEADER
