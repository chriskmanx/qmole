#ifndef AplusButtonHEADER
#define AplusButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSButton.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>

class  AplusButton : public MSButton
{
public:
  AplusButton(MSWidget *);
  ~AplusButton(void);
  virtual MSBoolean verifyData(V,A);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual AplusModel *model(void) const { return (AplusModel *)_model; }
  virtual void updateTitle(void);
  virtual void addSenderNotify(MSEventSender *m_);
  virtual void receiveEvent(MSEvent &);
  virtual void activate(void);
  virtual int numRows(void) const;
  virtual int numColumns(void) const;
  virtual int numColumns(int row_) const;
  virtual const char *formatOutput(MSString& str_, int row_);

  A getTitle(void) const;
  
  void callAFunc(A fun_,A cd_);
  virtual MSBoolean isProtected(void) const;

  STANDARD_WIDGET_METHODS_H
};

#endif // AplusButtonHEADER
