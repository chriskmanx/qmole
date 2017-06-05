#ifndef MSResourceChangeMethodCallbackHEADER
#define MSResourceChangeMethodCallbackHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSResourceChangeCallback.H>

template <class WidgetClass> 
class MSResourceChangeMethodCallback : public MSResourceChangeCallback
{
  typedef void (WidgetClass::*Method)(const char *);
private:
  Method       _method;
public:
  MSResourceChangeMethodCallback(WidgetClass *widget_,Method method_) :
  MSResourceChangeCallback(widget_),_method(method_) {}

  virtual void resourceChanged(const char *newValue_)
   { if (_widget!=0&&_method!=0) (((WidgetClass *)_widget)->*_method)(newValue_); }
};

#endif



