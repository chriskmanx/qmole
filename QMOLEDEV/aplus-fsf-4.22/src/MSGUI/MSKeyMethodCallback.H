#ifndef MSKeyMethodCallbackHEADER
#define MSKeyMethodCallbackHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSKeyCallback.H>


template <class MethodClass> 
class MSKeyMethodCallback : public MSKeyCallback
{
  
  typedef MSBoolean (MethodClass::*Method)(MSKeyPress);
private:
  Method       _method;
  MethodClass *_object;
public:
  MSKeyMethodCallback(MethodClass *object_,Method method_) :
    _object(object_),
    _method(method_) {}
  
  ~MSKeyMethodCallback(void) {}
  
  virtual MSBoolean process(MSWidget*,const MSKeyPress& translation_)
    { 
      if (_object!=0&&_method!=0) return (_object->*_method)(translation_);
      return MSFalse;
    }
};

#endif
