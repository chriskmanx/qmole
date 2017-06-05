#ifndef MSMethodCheckButtonHEADER
#define MSMethodCheckButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSCheckButton.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSMethodCheckButton.C")
#endif

template <class MethodClass> 
class MSMethodCheckButton : public MSCheckButton
{
typedef void (MethodClass::*Method)(void);
private:
  Method       _armMethod;
  Method       _disarmMethod;
  MethodClass *_object;

protected:
  virtual void arm(void);
  virtual void disarm(void);

public:
  MSMethodCheckButton(MSWidget *,const char *label_,
		      MethodClass *object_,Method armMethod_,Method disarmMethod_,
		      const MSSymbol& tag_=MSSymbol::nullSymbol());

  MSMethodCheckButton(MSWidget *,const MSStringVector& label_,
		      MethodClass *object_,Method armMethod_,Method disarmMethod_,
		      const MSSymbol& tag_=MSSymbol::nullSymbol());

  MSMethodCheckButton(MSWidget *,const MSPixmap& pixmap_,const MSPixmap& insensitivePixmap_,
		      const MSPixmap& armedPixmap_,
		      MethodClass *object_,Method armMethod_,Method disarmMethod_,
		      const MSSymbol& tag_=MSSymbol::nullSymbol());

  MSMethodCheckButton(MSWidget *,const char *label_,
		      MethodClass *object_,Method armMethod_,Method disarmMethod_,
		      int integerTag_);

  MSMethodCheckButton(MSWidget *,const MSStringVector& label_,
		      MethodClass *object_,Method armMethod_,Method disarmMethod_,
		      int integerTag_);

  MSMethodCheckButton(MSWidget *,const MSPixmap& pixmap_,const MSPixmap& insensitivePixmap_,
		      const MSPixmap& armedPixmap_,
		      MethodClass *object_,Method armMethod_,Method disarmMethod_,
		      int integerTag_);

  ~MSMethodCheckButton(void) {}

  Method armMethod(void) const { return _armMethod; }
  Method disarmMethod(void) const { return _disarmMethod; }
  MethodClass *object(void) const { return _object; }
};

#endif








