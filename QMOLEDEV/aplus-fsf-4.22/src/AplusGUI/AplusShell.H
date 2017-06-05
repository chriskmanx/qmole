#ifndef AplusShellHEADER
#define AplusShellHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSShell.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>

class AplusShell : public MSShell
{
public:
  AplusShell(void);
  AplusShell(MSDisplayServer *);
  ~AplusShell(void);
  
  MSWidgetView *getNextFocusAfter(MSWidgetView *);
  void insertFocusAfter(MSWidgetView *, MSWidgetView *);
  virtual void receiveEvent(MSEvent &);
  virtual MSBoolean verifyData(V,A) { return MSTrue; }
  virtual void addSenderNotify(MSEventSender *);
  virtual MSBoolean traverseFocus(MSWidget *);
  virtual void decoupleWidget(void);
  void setWMSaveYourself(void) { MSTopLevel::setWMSaveYourself(); }
  virtual void showAndWaitForMap(void);

  static MSBoolean allowNestedTraversal(void) { return _allowNestedTraversal; }
  static void allowNestedTraversal(MSBoolean b_) { _allowNestedTraversal = b_;}

  void virtualScreen(A screen_);  
  A virtualScreen(void);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual void save(void);

private:
  static MSBoolean _allowNestedTraversal;
};

#endif  //AplusShellHEADER
