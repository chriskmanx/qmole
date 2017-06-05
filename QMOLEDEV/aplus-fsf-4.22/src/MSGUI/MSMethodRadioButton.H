#ifndef MSMethodRadioButtonHEADER
#define MSMethodRadioButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSRadioButton.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSMethodRadioButton.C")
#endif

template <class MethodClass> 
class MSMethodRadioButton : public MSRadioButton
{
typedef void (MethodClass::*Method)(void);
private:
  Method       _armMethod;
  MethodClass *_object;

protected:
  virtual void arm(MSBoolean);
  virtual void arm(void);  

public:
  MSMethodRadioButton(MSRadioBox *,const char *label_,
		      MethodClass *object_,Method armMethod_,
		      const MSSymbol& tag_=MSSymbol::nullSymbol());

  MSMethodRadioButton(MSRadioBox *,const MSStringVector& label_,
		      MethodClass *object_,Method armMethod_,
		      const MSSymbol& tag_=MSSymbol::nullSymbol());

  MSMethodRadioButton(MSRadioBox *,const MSPixmap& pixmap_,
		      const MSPixmap& insensitivePixmap_,const MSPixmap& armedPixmap_,
		      MethodClass *object_,Method armMethod_,
		      const MSSymbol& tag_=MSSymbol::nullSymbol());

  MSMethodRadioButton(MSRadioBox *,const char *label_,
		      MethodClass *object_,Method armMethod_,
		      int integerTag_);

  MSMethodRadioButton(MSRadioBox *,const MSStringVector& label_,
		      MethodClass *object_,Method armMethod_,
		      int integerTag_);

  MSMethodRadioButton(MSRadioBox *,const MSPixmap& pixmap_,
		      const MSPixmap& insensitivePixmap_,const MSPixmap& armedPixmap_,
		      MethodClass *object_,Method armMethod_,
		      int integerTag_);

  ~MSMethodRadioButton(void) {}

  Method armMethod(void) const { return _armMethod; }
  MethodClass *object(void) const { return _object; }
};

#endif








