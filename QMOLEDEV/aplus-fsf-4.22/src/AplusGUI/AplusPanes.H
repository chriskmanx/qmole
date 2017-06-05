#ifndef AplusPanesHEADER
#define AplusPanesHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSHPane.H>
#include <MSGUI/MSVPane.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>

class AplusPane : public MSPane
{
public:
  AplusPane(MSWidget *);
  ~AplusPane(void);
  virtual void receiveEvent(MSEvent &);
  virtual MSBoolean verifyData(V,A) { return MSTrue; }

  virtual void addSenderNotify(MSEventSender *);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  STANDARD_WIDGET_METHODS_H
};


class AplusHPane : public MSHPane
{
public:
  AplusHPane(MSWidget *);
  ~AplusHPane(void);
  virtual void receiveEvent(MSEvent &);
  virtual MSBoolean verifyData(V,A) { return MSTrue; }

  virtual void addSenderNotify(MSEventSender *);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  STANDARD_WIDGET_METHODS_H
};


class AplusVPane : public MSVPane
{
public:
  AplusVPane(MSWidget *);
  ~AplusVPane(void);
  virtual void receiveEvent(MSEvent &);
  virtual MSBoolean verifyData(V,A) { return MSTrue; }

  virtual void addSenderNotify(MSEventSender *);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  STANDARD_WIDGET_METHODS_H
};

#endif // AplusPanesHEADER
