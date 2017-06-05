#ifndef AplusErrorDialogHEADER
#define AplusErrorDialogHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSUtil/MSDialog.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>

class  AplusErrorDialog : public MSDialog
{
public:
  AplusErrorDialog(MSWidget *);
  ~AplusErrorDialog(void);
  virtual MSBoolean verifyData(V,A);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual AplusModel *model(void) const { return (AplusModel *)_model; }
  virtual void updateTitle(void);
  A getTitle(void) const;
  virtual void addSenderNotify(MSEventSender *m_);
  virtual void receiveEvent(MSEvent &);

  STANDARD_WIDGET_METHODS_H
};

#endif // AplusErrorDialogHEADER
