#ifndef AplusRadioBoxHEADER
#define AplusRadioBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSRadioBox.H>
#include <MSGUI/MSRadioButton.H>
//#include <MSGUI/MSCheckButton.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusButtonBox.H>

class AplusRadioBox : public AplusButtonBox
{
friend class AplusRadioButton;
  
public:
  AplusRadioBox(MSWidget *);
  ~AplusRadioBox(void);

  MSRadioButton *activeButton(void);
  const MSRadioButton *activeButton(void) const;

  virtual void updateForeground(unsigned long);
  virtual void update(V,int,int,UpdateType) {}
  virtual void update(V v_, A a_, A p_, I r_) { AplusButtonBox::update(v_,a_,p_,r_); }
  virtual void update(const MSIndexVector &v_) { AplusButtonBox::update(v_); }
  virtual void updateData(void);
  virtual void updateValue(int row_);
  virtual MSBoolean verifyData(V v_,A a_);
  MSRadioButton *armedButton(void) { return _armedButton; }

protected:
  virtual void firstMapNotify(void);

  virtual void disarm(void);
  virtual void arm(MSRadioButton *);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  virtual MSActionButton *newButton(void);
  virtual void setButtonState(MSActionButton *, unsigned long);

  MSRadioButton *_armedButton;
};

class AplusRadioButton : public MSRadioButton
{
friend class AplusRadioBox;
  
public:
  AplusRadioButton(MSWidget *);
  ~AplusRadioButton(void);

protected:

  virtual void radioBoxArm(void);
  virtual void radioBoxDisarm(void);
  virtual void radioBoxActivate(void);

  inline const AplusRadioBox *radioBox(void) const;
  inline AplusRadioBox *radioBox(void);

  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);
  virtual void focusIn(void);
  virtual MSBoolean isProtected(void) const;
};

inline MSRadioButton *AplusRadioBox::activeButton(void)
{ return (MSRadioButton *)_activeButton; }
inline const MSRadioButton *AplusRadioBox::activeButton(void) const
{ return (const MSRadioButton *)_activeButton; }

inline const AplusRadioBox *AplusRadioButton::radioBox(void) const
{ return (parentWidgetType()==AplusRadioBox::symbol()?(AplusRadioBox *)owner():0); }

inline AplusRadioBox *AplusRadioButton::radioBox(void)
{ return (parentWidgetType()==AplusRadioBox::symbol()?(AplusRadioBox *)owner():0); }

#endif  //AplusRadioBoxHEADER
