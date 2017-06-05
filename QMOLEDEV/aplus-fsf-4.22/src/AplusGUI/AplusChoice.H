#ifndef AplusChoiceHEADER
#define AplusChoiceHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSOptionMenu.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusCommon.H>

class  AplusChoice : public MSOptionMenu
{
public:
  AplusChoice(MSWidget *);
  ~AplusChoice(void);
  virtual MSBoolean verifyData(V,A);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual void addSenderNotify(MSEventSender *);
  virtual void receiveEvent(MSEvent &);

  virtual void updateData(void);
  virtual void update(V,int,int,UpdateType) {}
  virtual void update(V,A,A,I);
  virtual void update(const MSIndexVector &v_) { MSOptionMenu::update(v_); }
     
  virtual void updateTitle(void);
  virtual void updateFont(Font);
  void updateOptions(void);
  Font dataFont(void);
  virtual const char *formatOutput(MSString&);
  virtual const char *itemLabel(unsigned);
  virtual unsigned long itemForeground(unsigned);
  
  virtual void updateForeground(unsigned long);

  virtual void activate(int);
  virtual void activate(void) { MSOptionMenu::activate(); }

  A itemValue(int);
  AplusModel *model(void) { return (AplusModel *)MSView::model(); }
  const AplusModel *model(void) const { return (const AplusModel *)MSView::model(); }

  virtual MSBoolean hasModel(void) const;  // see the comment in AplusChoice.C

  virtual void setWidth(void);

  void updateModelVector(A);
  void setChoice(void);
  virtual MSBoolean isProtected(void) const;
  
  MSStringVector& modelBuffer(void) { return _modelBuffer; }

  MSStringVector _modelBuffer;
  A _itemVal;

  STANDARD_WIDGET_METHODS_H
};

#endif  //AplusGUIHEADER
