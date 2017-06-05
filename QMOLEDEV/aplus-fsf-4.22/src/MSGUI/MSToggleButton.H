#ifndef MSToggleButtonHEADER
#define MSToggleButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSToggleButtonBase.H>

class MSToggleButton: public MSToggleButtonBase
{
public:
  
  MSToggleButton(MSWidget *,const char *label_,
		 const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSToggleButton(MSWidget *,const MSStringVector& label_,
		 const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSToggleButton(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
		 const MSPixmap &armedPixmap_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSToggleButton(MSWidget *,const char *label_,int integerTag_);
  MSToggleButton(MSWidget *,const MSStringVector& label_,int  integerTag_);
  MSToggleButton(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
		 const MSPixmap &armedPixmap_,int integerTag_);
  ~MSToggleButton(void);
  
protected:
  void init(void);
  virtual void redraw(void);

  virtual void arm(void);
  virtual void disarm(void);
};


#endif
