#ifndef AplusTextHEADER
#define AplusTextHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSText.H>
#include <MSGUI/MSTextField.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>

class AplusText : public MSText
{
public:
  AplusText(MSWidget *);
  ~AplusText(void);

  AplusModel *model(void) { return (AplusModel*)_model; }
  virtual MSBoolean verifyData(V,A);
  virtual void updateData(void);

protected:
  virtual void receiveEvent(MSEvent &);
  virtual void addSenderNotify(MSEventSender *);
  virtual void doClearSelection(void);
  virtual void keyPress(const XEvent *, KeySym, unsigned int, const char *);

  virtual MSBoolean isProtected(void) const;
     
  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  STANDARD_WIDGET_METHODS_H
};

#endif //AplusTextHEADER
