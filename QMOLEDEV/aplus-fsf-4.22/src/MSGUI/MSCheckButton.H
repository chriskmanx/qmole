#ifndef MSCheckButtonHEADER
#define MSCheckButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSToggleButtonBase.H>

class MSCheckBox;
class MSGUIExport MSCheckButton : public MSToggleButtonBase
{
friend class MSCheckBox;
public:
  MSCheckButton(MSWidget *,const char *label_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSCheckButton(MSWidget *,const MSStringVector& label_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSCheckButton(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
		const MSPixmap &armedPixmap_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSCheckButton(MSWidget *,const char *label_,int integerTag_);
  MSCheckButton(MSWidget *,const MSStringVector& label_,int  integerTag_);
  MSCheckButton(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
		const MSPixmap &armedPixmap_,int integerTag_);


  ~MSCheckButton(void);

  virtual MSAttrValueList& get(MSAttrValueList&);
  virtual void set(MSAttrValueList&);

protected:
  void drawCheckButton(void);
  virtual void arm(void);
  virtual void disarm(void);
  virtual void drawSymbol(void);

  MSCheckBox *checkBox(void) const;
  virtual void checkBoxArm(void);
  virtual void checkBoxDisarm(void);

  // Arrow Key Traversal
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);

};

#endif




