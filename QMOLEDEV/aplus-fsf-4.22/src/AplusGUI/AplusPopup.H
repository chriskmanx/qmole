#ifndef AplusPopupHEADER
#define AplusPopupHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPopup.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>

class AplusPopup : public MSPopup
{
public:
  AplusPopup(void);
  AplusPopup(MSDisplayServer *);
  ~AplusPopup(void);

  MSWidgetView *getNextFocusAfter(MSWidgetView *);
  void insertFocusAfter(MSWidgetView *, MSWidgetView *);
  virtual void receiveEvent(MSEvent &);
  virtual MSBoolean verifyData(V,A) { return MSTrue; }
  virtual void addSenderNotify(MSEventSender *);
  virtual MSBoolean traverseFocus(MSWidget *);
  virtual void decoupleWidget(void);
  void setWMSaveYourself(void) { MSTopLevel::setWMSaveYourself(); }
  virtual void showAndWaitForMap(void);

  void virtualScreen(A screen_);  
  A virtualScreen(void);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);
};

#endif  //AplusPopupHEADER
