#ifndef MSMethodButtonHEADER
#define MSMethodButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSButton.H>

template <class MethodClass> 
class MSMethodButton : public MSButton
{
typedef void (MethodClass::*Method)(void);
private:
  Method       _method;
  MethodClass *_object;

public:
  MSMethodButton(MSWidget *owner_,MethodClass *object_,Method method_) :
  _object(object_),_method(method_),
  MSButton(owner_) {}

  MSMethodButton(MSWidget *owner_,const char *label_,MethodClass *object_,Method method_) :
  _object(object_),_method(method_),
  MSButton(owner_,label_) {}

  MSMethodButton(MSWidget *owner_,const MSStringVector& label_,
		 MethodClass *object_,Method method_) :
  _object(object_),_method(method_),
  MSButton(owner_,label_) {}

  MSMethodButton(MSWidget *owner_,const MSPixmap& pixmap_,const MSPixmap& insensitivePixmap_,
		 const MSPixmap& armedPixmap_,MethodClass *object_,Method method_) :
  _object(object_),_method(method_),
  MSButton(owner_,pixmap_,insensitivePixmap_,armedPixmap_) {}

  ~MSMethodButton(void) {}

  Method method(void)       const {return _method;}
  MethodClass *object(void) const {return _object;}

  virtual void activate(void)
   { if (_object!=0&&_method!=0) (_object->*_method)(); }
};

#endif








