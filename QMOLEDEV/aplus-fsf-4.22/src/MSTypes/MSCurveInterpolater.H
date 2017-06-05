#ifndef MSCurveInterpolaterHEADER
#define MSCurveInterpolaterHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSIndexedModel.H>
#include <MSTypes/MSIndexedEvent.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSCurveInterpolater.C")
#endif

template<class X,class Y> class MSCurve;
template<class Type> class MSTypeVector;

// WARNING - PLEASE READ THIS:
// 
// We have observed a problem when passing any of the
// built-in types by const reference: for example,
// 
// template<class X>
// void Foo::doSomething(const X& x_)
// 
// Foo<int> foo;
// int x=99;
// foo.doSomething(x++);
// 
// It turns out that in foo, the value of x is 100, when it should 
// be 99. This problem can be avoided if the prototype was pass by
// value instead of pass by reference. We believe this to be a compiler
// bug, but one should be cautious and avoid this type of syntax.

template<class X,class Y> 
class MSCurveInterpolater : public MSEventReceiver
{
public:
  MSCurveInterpolater(MSCurve<X,Y> *curve_);
  MSCurveInterpolater(MSCurve<X,Y>& curve_);
  ~MSCurveInterpolater();
		      
  virtual Y interpolate(const X&) const=0;
  virtual MSTypeVector<Y> interpolate(const MSTypeVector<X>&) const=0;

  MSBoolean isValid(void) const;
  MSCurve<X,Y> *curve(void) const;

protected:
  MSCurve<X,Y> *_curve;
  MSBoolean     _isValid;

  virtual void receiveEvent(MSEvent&);
};

template<class X,class Y> 
inline MSBoolean MSCurveInterpolater<X,Y>::isValid(void) const
{ return _isValid; }

template<class X,class Y> 
inline MSCurve<X,Y> *MSCurveInterpolater<X,Y>::curve(void) const
{ return _curve; }

#endif
