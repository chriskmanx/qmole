#ifndef MSKeyVoidMethodCallbackHEADER
#define MSKeyVoidMethodCallbackHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSKeyCallback.H>

template <class MethodClass> 
class MSKeyVoidMethodCallback : public MSKeyCallback
{
  
  typedef void (MethodClass::*Method)();
private:
  Method       _method;
  MethodClass *_object;
public:
  MSKeyVoidMethodCallback(MethodClass *object_,Method method_) :
    _object(object_),
    _method(method_) {}
  
  ~MSKeyVoidMethodCallback(void) {}
  
  virtual MSBoolean process(MSWidget*,const MSKeyPress& translation_)
    { 
      if (_object!=0&&_method!=0) (_object->*_method)();
      return MSFalse;
   }
};

#endif
