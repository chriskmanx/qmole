#ifndef AplusManagerHEADER
#define AplusManagerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSManager.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusEvent.H>

class AplusManager : public MSManager
{
public:
  AplusManager(MSWidget *);
  ~AplusManager(void);

  virtual void addSenderNotify(MSEventSender *);
  virtual void receiveEvent(MSEvent &);

  virtual MSBoolean verifyData(V,A) { return MSTrue; }
  virtual void update(V,int,int,int) {}
  virtual void update(V,A,A,I) {}
  virtual void update(const MSIndexVector &v_) { MSManager::update(v_); }
  
  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  STANDARD_WIDGET_METHODS_H
};

#endif // AplusManagerHEADER
