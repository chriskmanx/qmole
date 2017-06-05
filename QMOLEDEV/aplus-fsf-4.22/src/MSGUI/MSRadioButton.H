#ifndef MSRadioButtonHEADER
#define MSRadioButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSToggleButtonBase.H>

class MSRadioBox;
class MSGUIExport MSRadioButton : public MSToggleButtonBase
{
friend class MSRadioBox;
public:
  MSRadioButton(MSRadioBox *,const char *label_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSRadioButton(MSRadioBox *,const MSStringVector& label_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSRadioButton(MSRadioBox *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
		const MSPixmap &armedPixmap_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSRadioButton(MSRadioBox *,const char *label_,int integerTag_);
  MSRadioButton(MSRadioBox *,const MSStringVector& label_,int  integerTag_);
  MSRadioButton(MSRadioBox *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
		const MSPixmap &armedPixmap_,int integerTag_);
  ~MSRadioButton(void);

  virtual MSAttrValueList& get(MSAttrValueList&);
  virtual void set(MSAttrValueList&);
  
protected:
  void drawDiamond(void);
  virtual void arm(MSBoolean callback_);
  virtual void arm(void);
  virtual void disarm(void);
  virtual void drawSymbol(void);

  MSRadioBox *radioBox(void) const;

  virtual void radioBoxArm(void);
  virtual void radioBoxDisarm(void);
  virtual void radioBoxActivate(void);

  // Arrow Key Traversal
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);

};

#endif
