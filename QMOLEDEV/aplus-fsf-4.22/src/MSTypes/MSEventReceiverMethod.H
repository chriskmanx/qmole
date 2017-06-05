#ifndef MSEventReceiverMethodHEADER
#define MSEventReceiverMethodHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSDefines.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSEventReceiverMethod.C")
#endif

#ifndef MSEventReceiverHEADER
#include <MSTypes/MSEventReceiver.H>
#endif

template <class MethodClass>
class MSEventReceiverMethod : public MSEventReceiver
{
//##############################################################################
//  The template class MSEventReceiverMethod is used to provide an
//  MSEventReceiver-type wrapper for particular C++ member methods.  
//
//  The template class argument is the class name MethodClass of the class whose 
//  member method is to be wrapped.
//
//  The constructor for the object requires two things:
//    - An instance of class MethodClass.
//    - A pointer to a member method of the class MethodClass.  This member method
//      will be applied to the object specified on the constructor when the 
//      callback is invoked.
//
//    - The method should have the following prototype:
//  
//   void MethodClass::method(MSEvent& aEvent_)
//   {
//   }  
//
//  Only non-const member methods are supported.  
//
//###############################################################################
  typedef void (MethodClass::*Method)(MSEvent&);
private:
  Method       _method;
  MethodClass *_object;

protected:
  virtual MSBoolean addSender(MSEventSender *);
  virtual MSBoolean removeSender(MSEventSender *);
  
public:
#ifdef MS_METHOD_POINTER_DEFAULTARG_BUG
  MSEventReceiverMethod(MethodClass *object_,Method method_) :
  _object(object_),_method(method_) {}
  MSEventReceiverMethod(MethodClass *object_=0) :
  _object(object_),_method(0) {}
#else
  MSEventReceiverMethod(MethodClass *object_=0,Method method_=0) : _object(object_),_method(method_) {}
#endif
  ~MSEventReceiverMethod(void);
  
  void method(MethodClass* object_,Method method_) {_object=object_,_method=method_;}

  Method method(void)        { return _method; }
  MethodClass *object(void)  { return _object; }

  inline virtual void receiveEvent(MSEvent&);
};

template <class MethodClass> 
inline void MSEventReceiverMethod<MethodClass>::receiveEvent(MSEvent& aEvent_)
{ if (_object!=0&&_method!=0) (_object->*_method)(aEvent_); }

#endif
