#ifndef MSChangedCallbackHEADER
#define MSChangedCallbackHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEventReceiver.H>
#include <MSTypes/MSEventSender.H>

class MSTypesExport MSChangedCallback : public MSEventReceiver
{
public:
  MSChangedCallback(void);
  ~MSChangedCallback(void);
	
  virtual void process(MSEventSender *)=0;
protected:
  virtual void receiveEvent(MSEvent&);
};

template <class MethodClass> 
class MSMethodChangedCallback : public MSChangedCallback
{
typedef void (MethodClass::*Method)(MSEventSender *);
private:
  Method       _method;
  MethodClass *_object;

public:
#ifdef MS_METHOD_POINTER_DEFAULTARG_BUG
  MSMethodChangedCallback(MethodClass *object_,Method method_) :
  _object(object_),_method(method_) {}
  MSMethodChangedCallback(MethodClass *object_=0) :
  _object(object_),_method(0) {}
#else
  MSMethodChangedCallback(MethodClass *object_=0,Method method_=0) : 
  _object(object_),_method(method_) {}
#endif
  void method(MethodClass* object_,Method method_) {_object=object_,_method=method_;}

  Method method(void)        { return _method; }
  MethodClass *object(void)  { return _object; }

  virtual void process(MSEventSender *);
};

template <class MethodClass> 
inline void MSMethodChangedCallback<MethodClass>::process(MSEventSender *sender_)
{ if(object()!=0&&method()!=0) (object()->*method())(sender_); }

#endif


