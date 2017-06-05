#ifndef MSRadioBoxHEADER
#define MSRadioBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSActionBox.H>
#include <MSGUI/MSRadioButton.H>

class MSGUIExport MSRadioBox : public MSActionBox
{
friend class MSRadioButton;

public:  
  MSRadioBox(MSWidget *,const char *title_=0);
  MSRadioBox(MSWidget *,const MSStringVector&);
  ~MSRadioBox(void);

  MSRadioButton *activeButton(void);
  const MSRadioButton *activeButton(void) const;
  void activeButton(MSRadioButton *radioButton_, MSBoolean callback_=MSFalse);

  virtual const MSSymbol& widgetType(void) const;

protected:
  virtual void firstMapNotify(void);

  virtual void arm(MSRadioButton *);
  virtual void disarm(void);

  static const MSSymbol& symbol(void);
};

inline MSRadioButton *MSRadioBox::activeButton(void)
{ return (MSRadioButton *)_activeButton; }
inline const MSRadioButton *MSRadioBox::activeButton(void) const
{ return (const MSRadioButton *)_activeButton; }

#endif












