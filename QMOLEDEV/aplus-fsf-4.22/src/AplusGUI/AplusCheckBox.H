#ifndef AplusCheckBoxHEADER
#define AplusCheckBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSCheckBox.H>
#include <MSGUI/MSCheckButton.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusButtonBox.H>

class AplusCheckBox : public AplusButtonBox
{
friend class AplusCheckButton;
  
public:
  AplusCheckBox(MSWidget *);
  ~AplusCheckBox(void);

  virtual void update(V,int,int,UpdateType) {}
  virtual void update(V v_, A a_, A p_, I r_) { AplusButtonBox::update(v_,a_,p_,r_); }
  virtual void update(const MSIndexVector &v_) { AplusButtonBox::update(v_); }

  virtual void updateValue(int row_);
  virtual MSBoolean verifyData(V v_,A a_);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual void arm(MSCheckButton *);
  virtual void disarm(MSCheckButton *);

  virtual void updateForeground(unsigned long);
  virtual MSActionButton *newButton(void);
  virtual void setButtonState(MSActionButton *, unsigned long);
};

class AplusCheckButton : public MSCheckButton
{
friend class AplusCheckBox;
  
public:
  AplusCheckButton(MSWidget *);
  ~AplusCheckButton(void);

protected:
  virtual void checkBoxArm(void);
  virtual void checkBoxDisarm(void);
  inline const AplusCheckBox *checkBox(void) const;
  inline AplusCheckBox *checkBox(void);

  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);

  virtual void focusIn(void);

  virtual MSBoolean isProtected(void) const;
};



inline AplusCheckBox *AplusCheckButton::checkBox(void)
{ return (parentWidgetType()==AplusCheckBox::symbol()?(AplusCheckBox *)owner():0); }

inline const AplusCheckBox *AplusCheckButton::checkBox(void) const
{ return (parentWidgetType()==AplusCheckBox::symbol()?(AplusCheckBox *)owner():0); }

#endif  //AplusCheckBoxHEADER
