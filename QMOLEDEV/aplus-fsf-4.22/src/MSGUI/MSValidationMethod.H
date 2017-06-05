#ifndef MSValidationMethodHEADER
#define MSValidationMethodHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSValidationCallback.H>

template <class MethodClass> 
class MSValidationMethod : public MSValidationCallback
{
  typedef MSBoolean (MethodClass::*Method)(MSString&);
private:
  Method       _method;
  MethodClass *_object;
public:
#ifdef MS_METHOD_POINTER_DEFAULTARG_BUG
   MSValidationMethod(MethodClass *object_=0) :
  _object(object_),_method(0) {}
   MSValidationMethod(MethodClass *object_,Method method_) :
  _object(object_),_method(method_) {}
#else
   MSValidationMethod(MethodClass *object_=0,Method method_=0) :
  _object(object_),_method(method_) {}
#endif
  ~MSValidationMethod(void) {}

  void method(MethodClass *object_,Method method_)
   {_object=object_,_method=method_;}

  Method method(void)       {return _method;}
  MethodClass *object(void) {return _object;}

  virtual MSBoolean validate(MSString& aString_)
   {
     if (_object!=0&&_method!=0) return (_object->*_method)(aString_);
     else return MSFalse;
   }
};

#endif



