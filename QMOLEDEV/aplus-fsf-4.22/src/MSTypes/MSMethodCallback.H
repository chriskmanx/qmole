#ifndef MSMethodCallbackHEADER
#define MSMethodCallbackHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSCallback.H>

template <class MethodClass> 
class MSMethodCallback : public MSCallback
{
//##############################################################################
//  The template class MSMethodCallback is used to provide an
//  MSCallback-type wrapper for particular C++ member methods.  
//
//  The template class argument is the class name MethodClass of the class whose 
//  member method is to be wrappered.
//
//  The constructor for the object requires two things:
//    - An instance of class T.
//    - A pointer to a member method of the class T.  This member method
//      will be applied to the object specified on the constructor when the 
//      callback is invoked.
//
//  Only non-const member methods are supported.  
//
//  Example:
//    // Any arbitrary class with appropriate method...
//    class MyWindow 
//     {
//       void doSomething(void) const;
//     };
//    MyWindow window;
//    MSList list(&window);
//    ...
//    list.callback(MSWidgetCallback::selection,
//        new MSMethodCallback<MyWindow>(&window,&MyWindow::doSomething));
//
//###############################################################################
  typedef void (MethodClass::*Method)(void);
private:
  Method       _method;
  MethodClass *_object;
public:
#ifdef MS_METHOD_POINTER_DEFAULTARG_BUG
  MSMethodCallback(MethodClass *object_,Method method_) :
  _object(object_),_method(method_) {}
  MSMethodCallback(MethodClass *object_=0) :
  _method(0),_object(object_) {}
#else
  MSMethodCallback(MethodClass *object_=0,Method method_=0) :
  _method(method_),_object(object_) {}
#endif
  ~MSMethodCallback(void) {}

  void method(MethodClass *object_,Method method_)
   {_object=object_,_method=method_;}

  Method method(void)       {return _method;}
  MethodClass *object(void) {return _object;}

  virtual void process(void)
   { if (_object!=0&&_method!=0) (_object->*_method)(); }
};

#endif



